
use std::cell::RefCell;
use std::collections::HashMap;


use binding;
use typecheck;

use abi::ABI;
use defs::Def;
use types::Type;
use config::Options;
use session::Session;
use scope::{ Scope, ScopeRef };
use ast::{ NodeID, Pos, Mutability, Visibility, Literal, Ident, Argument, ClassSpec, MatchCase, Pattern, EnumVariant, AST };

use defs::functions::{ FuncDef, ClosureDefRef };
use defs::classes::{ ClassDefRef, StructDefRef, Define, Vtable };

use misc::{ r };
use llvm::llcode::{ LLType, LLLit, LLRef, LLCmpType, LLLink, LLCC, LLExpr, LLGlobal };


static EXCEPTION_POINT_NAME: &str = "__ExceptionPoint__";

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CodeContext {
    Func(ABI, NodeID),
    ClassBody,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Transformer<'sess> {
    pub session: &'sess Session,
    pub globals: RefCell<Vec<LLGlobal>>,
    pub context: RefCell<Vec<CodeContext>>,
    pub expoints: RefCell<Vec<NodeID>>,
    pub types: RefCell<HashMap<NodeID, LLType>>,
}

impl<'sess> Transformer<'sess> {
    pub fn new(session: &'sess Session) -> Self {
        Transformer {
            session: session,
            globals: RefCell::new(vec!()),
            context: RefCell::new(vec!()),
            expoints: RefCell::new(vec!()),
            types: RefCell::new(HashMap::new()),
        }
    }

    pub fn initialize(&self) {
        let global = self.session.map.get_global();

        /*
            "()" => LLType::Void,
            "Nil" => LLType::Ptr(LLType::I8),
            "Bool" => LLType::I1,
            "Byte" => LLType::I8,
            "Int" => LLType::I64,
            "Real" => LLType::F64,
            "String" => LLType::Ptr(LLType::I8),
            "Buffer" => LLType::Ptr(LLType::Ptr(LLType::I8)),
        */

        let expoint_id = NodeID::generate();
        global.define_type(String::from(EXCEPTION_POINT_NAME), Some(expoint_id));
        self.session.set_type(expoint_id, Type::Object(String::from(EXCEPTION_POINT_NAME), expoint_id, vec!()));
    }

    fn set_type(&self, id: NodeID, ltype: LLType) {
        self.types.borrow_mut().insert(id, ltype);
    }

    fn get_type(&self, id: NodeID) -> Option<LLType> {
        self.types.borrow_mut().get(&id).map(|ltype| ltype.clone())
    }

    fn add_global(&self, global: LLGlobal) {
        self.globals.borrow_mut().push(global);
    }

    fn insert_global(&self, index: usize, global: LLGlobal) {
        self.globals.borrow_mut().insert(index, global);
    }

    fn set_context(&self, context: CodeContext) {
        self.context.borrow_mut().push(context);
    }

    fn restore_context(&self) {
        self.context.borrow_mut().pop();
    }

    fn get_context(&self) -> Option<CodeContext> {
        self.context.borrow().last().map(|c| *c)
    }

    /*
    fn get_func_context(&self) -> Option<CodeContext> {
        for i in (self.context.borrow().len() - 1) .. 0 {
            match self.context.borrow()[i] {
                context @ CodeContext::Func(_, _) => return Some(context),
                _ => { },
            }
        }
        return None
    }
    */

    fn with_context<F, R>(&self, context: CodeContext, f: F) -> R where F: FnOnce() -> R {
        self.set_context(context);
        let ret = f();
        self.restore_context();
        ret
    }

    fn with_exception<F, R>(&self, expoint: NodeID, f: F) -> R where F: FnOnce() -> R {
        self.expoints.borrow_mut().push(expoint);
        let ret = f();
        self.expoints.borrow_mut().pop();
        ret
    }

    fn get_exception(&self) -> Option<NodeID> {
        self.expoints.borrow().last().map(|e| *e)
    }


    pub fn transform_code(&self, scope: ScopeRef, code: &Vec<AST>) {
        let run_id = self.build_run_func(scope.clone(), code);

        if !Options::as_ref().is_library {
            self.build_main_func(scope.clone(), run_id);
        }
    }

    pub fn build_run_func(&self, scope: ScopeRef, code: &Vec<AST>) -> NodeID {
        // Define a global that will store whether we've run this function or not
        let module_memo_id = NodeID::generate();
        self.add_global(LLGlobal::DefGlobal(module_memo_id, LLLink::Public, format!("memo.{}", self.session.name), LLType::I1));

        let run_id = NodeID::generate();
        let run_ltype = self.convert_to_mfunc_def_type(LLType::Function(vec!(), r(LLType::I64)));
        self.set_type(run_id, run_ltype.clone());

        let mut fargs = vec!();
        let exp_id = NodeID::generate();
        self.convert_mfunc_def_args(scope.clone(), exp_id, &mut fargs);

        // Set the memo, execute the module's top level scope, and then return 0
        let mut body = vec!();
        body.push(LLExpr::SetGlobal(module_memo_id, r(LLExpr::Literal(LLLit::I1(true)))));
        body.extend(
            self.with_context(CodeContext::Func(ABI::MoltenFunc, run_id), || {
                self.with_exception(exp_id, || {
                    self.transform_vec(scope.clone(), &code)
                })
            })
        );
        body.push(LLExpr::Literal(LLLit::I64(0)));

        // If we've run before, return 0, else execute the body
        let run_body = vec!(LLExpr::Phi(vec!(
            vec!(LLExpr::Cmp(LLCmpType::Equal, r(LLExpr::GetGlobal(module_memo_id)), r(LLExpr::Literal(LLLit::I1(true))))),
            vec!(LLExpr::Literal(LLLit::I1(true)))
        ), vec!(
            vec!(LLExpr::Literal(LLLit::I64(0))),
            body
        )));

        let module_run_name = format!("run_{}", self.session.name.replace(".", "_"));
        self.add_global(LLGlobal::DefCFunc(run_id, LLLink::Public, module_run_name, run_ltype, fargs, run_body, LLCC::FastCC));

        run_id
    }

    pub fn build_main_func(&self, scope: ScopeRef, run_id: NodeID) {
        let main_id = NodeID::generate();
        let main_ltype = LLType::Function(vec!(), r(LLType::I64));
        let mut main_body = vec!();

        // Initialize the garbage collector
        main_body.push(LLExpr::CallC(r(LLExpr::GetNamed("molten_init".to_string())), vec!(), LLCC::CCC));

        let exp_id = NodeID::generate();
        let expoint = self.create_exception_point(&mut main_body, exp_id);

        // Try calling the module's run function
        let try = self.with_exception(exp_id, || {
            self.create_mfunc_invoke(LLExpr::GetValue(run_id), vec!())
        });

        // If an exception occurs, print a message and exit with -1
        let catch = vec!(
            LLExpr::CallC(r(LLExpr::GetNamed("puts".to_string())), vec!(LLExpr::Literal(LLLit::ConstStr(String::from("Uncaught exception.  Terminating")))), LLCC::CCC),
            LLExpr::Literal(LLLit::I64(-1))
        );

        main_body.extend(self.create_exception_block(expoint, try, catch));

        self.add_global(LLGlobal::DefCFunc(main_id, LLLink::Public, String::from("main"), main_ltype, vec!(), main_body, LLCC::CCC));
    }

    pub fn transform_vec(&self, scope: ScopeRef, code: &Vec<AST>) -> Vec<LLExpr> {
        let mut exprs = vec!();
        for expr in code {
            exprs.extend(self.transform_node(scope.clone(), expr));
        }
        exprs
    }

    fn transform_as_result(&self, exprs: &mut Vec<LLExpr>, scope: ScopeRef, node: &AST) -> Option<LLExpr> {
        let mut newexprs = self.transform_node(scope.clone(), node);
        let last = newexprs.pop();
        exprs.extend(newexprs);
        last
    }

    fn transform_as_args(&self, exprs: &mut Vec<LLExpr>, scope: ScopeRef, args: &Vec<AST>) -> Vec<LLExpr> {
        let mut fargs = vec!();
        for arg in args {
            fargs.push(self.transform_as_result(exprs, scope.clone(), arg).unwrap());
        }
        fargs
    }

    fn transform_node(&self, scope: ScopeRef, node: &AST) -> Vec<LLExpr> {
        match &node {
            AST::Literal(_, lit) => vec!(LLExpr::Literal(self.transform_lit(lit))),

            AST::Nil(id) => vec!(LLExpr::Literal(LLLit::Null(self.transform_value_type(&self.session.get_type(*id).unwrap())))),

            AST::Block(_, _, code) => self.transform_vec(scope.clone(), code),

            AST::Import(_, _, ident, decls) => {
                self.transform_import(scope.clone(), &ident.name, decls)
            },

            AST::Try(_, _, code, cases) => {
                self.transform_try(scope.clone(), code, cases)
            },

            AST::Raise(id, _, valexpr) => {
                self.transform_raise(scope.clone(), *id, valexpr)
            },


            AST::Function(id, _, vis, ident, args, _, body, abi) => {
                self.transform_func_def(scope.clone(), *abi, *id, *vis, ident.as_ref().map(|ident| &ident.name), args, body)
            },

            AST::Declare(id, _, vis, ident, _) => {
                let ttype = self.session.get_type(*id).unwrap();
                let abi = ttype.get_abi().unwrap();
                self.transform_func_decl(scope.clone(), abi, *id, *vis, &ident.name, &ttype)
            },

            AST::Invoke(id, _, func, args) => {
                let abi = self.session.get_type(*id).unwrap().get_abi().unwrap();
                self.transform_func_invoke(scope.clone(), abi, *id, func, args)
            },

            AST::Definition(id, _, _, ident, _, value) => {
                self.transform_def_local(scope.clone(), *id, &ident.name, value)
            },

            AST::GetValue(id) => {
                vec!(LLExpr::GetValue(*id))
            },

            AST::Identifier(id, _, ident) => {
                let defid = self.session.get_ref(*id).unwrap();
                self.transform_reference(scope.clone(), defid, &ident.name)
            },


            AST::Ref(id, _, value) => {
                self.transform_alloc_ref(scope.clone(), *id, value)
            },

            AST::Deref(_, _, value) => {
                self.transform_deref_ref(scope.clone(), value)
            },

            AST::Record(id, _, items) => {
                let mut nitems = vec!();
                for item in items {
                    nitems.push(item.1.clone());
                }
                self.transform_tuple_lit(scope.clone(), *id, &nitems)
            },

            AST::Tuple(id, _, items) => {
                self.transform_tuple_lit(scope.clone(), *id, &items)
            },

            AST::RecordUpdate(id, _, record, items) => {
                self.transform_record_update(scope.clone(), *id, &record, &items)
            },

            AST::TypeEnum(id, _, classspec, _) => {
                self.transform_enum_def(scope.clone(), *id, &classspec.ident.name)
            },


            AST::New(id, _, _) => {
                self.transform_new_object(*id)
            },

            AST::Class(id, _, _, _, body) => {
                let mut exprs = self.transform_class_body(scope.clone(), *id, body);
                exprs.push(LLExpr::Literal(self.transform_lit(&Literal::Unit)));
                exprs
            },

            AST::PtrCast(id, _, node) => {
                let mut exprs = vec!();
                let ltype = self.transform_value_type(&self.session.get_type(*id).unwrap());
                let result = self.transform_as_result(&mut exprs, scope.clone(), node).unwrap();
                exprs.push(LLExpr::Cast(ltype, r(result)));
                exprs
            },

            AST::Accessor(id, _, obj, ident, oid) => {
                let otype = self.session.get_type(*oid).unwrap();
                self.transform_accessor(scope.clone(), *id, obj, &ident.name, otype)
            },

            AST::Resolver(id, _, path, field, oid) => {
                let otype = self.session.get_type_from_ref(*oid).unwrap();
                self.transform_resolve(*id, path, &field.name, otype)
            },

            AST::Assignment(id, _, left, right, _) => {
                self.transform_assignment(scope.clone(), *id, left, right)
            },


            AST::If(_, _, cond, texpr, fexpr) => {
                self.transform_if_expr(scope.clone(), cond, texpr, fexpr)
            },

            AST::Match(_, _, cond, cases) => {
                self.transform_match(scope.clone(), cond, cases)
            },

            AST::SideEffect(_, _, ident, args) => {
                self.transform_side_effect(scope.clone(), ident.name.as_str(), args)
            },


            AST::While(_, _, cond, body) => {
                vec!(LLExpr::Loop(self.transform_node(scope.clone(), cond), self.transform_node(scope.clone(), body)))
            },


            AST::TypeAlias(_, _, _, _) => { /* Nothing Needs To Be Done */ vec!() }

            _ => panic!("Not Implemented: {:?}", node),
        }
    }

    fn transform_lit(&self, lit: &Literal) -> LLLit {
        match lit {
            Literal::Unit => LLLit::I32(0),
            Literal::Boolean(num) => LLLit::I1(*num as bool),
            Literal::Integer(num) => LLLit::I64(*num as i64),
            Literal::Real(num) => LLLit::F64(*num),
            // TODO not sure how you'll do strings yet.... maybe it shouldn't even be a literal here
            Literal::String(string) => LLLit::ConstStr(string.clone()),
        }
    }

    fn transform_tuple_lit(&self, scope: ScopeRef, id: NodeID, items: &Vec<AST>) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let mut litems = vec!();
        for item in items {
            litems.push(self.transform_as_result(&mut exprs, scope.clone(), item).unwrap());
        }
        let ltype = self.transform_value_type(&self.session.get_type(id).unwrap());
        exprs.push(LLExpr::DefStruct(id, ltype, litems));
        exprs
    }

    fn transform_record_update(&self, scope: ScopeRef, id: NodeID, record: &AST, items: &Vec<(Ident, AST)>) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let mut litems = vec!();

        let ttype = &self.session.get_type(id).unwrap();
        let valexpr = self.transform_as_result(&mut exprs, scope.clone(), record).unwrap();

        for (ref name, _) in ttype.get_record_types().unwrap() {
            let item = match items.iter().find(|(ident, _)| &ident.name == name) {
                Some((_, expr)) => self.transform_as_result(&mut exprs, scope.clone(), expr).unwrap(),
                None => LLExpr::GetItem(r(valexpr.clone()), litems.len()),
            };
            litems.push(item);
        }
        exprs.push(LLExpr::DefStruct(id, self.transform_value_type(ttype), litems));
        exprs
    }

    fn transform_enum_def(&self, scope: ScopeRef, id: NodeID, name: &String) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let selector = LLType::I8;
        let enumdef = self.session.get_def(id).unwrap().as_enum().unwrap();

        self.add_global(LLGlobal::DefNamedStruct(id, name.clone(), false));
        self.set_type(id, LLType::Alias(id));

        let mut types = vec!();
        for (i, variant) in enumdef.variants.borrow().iter().enumerate() {
            let name = format!("{}_{}", name, variant.ident.name);
            self.transform_enum_variant(variant.id, i as i8, name, selector.clone(), variant.ttype.clone());
            if variant.ttype.is_some() {
                types.push(self.transform_value_type(variant.ttype.as_ref().unwrap()));
            }
        }

        self.add_global(LLGlobal::SetStructBody(id, vec!(selector.clone(), LLType::Largest(types)), false));
        exprs
    }

    fn transform_enum_variant(&self, id: NodeID, variant: i8, name: String, selector: LLType, ttype: Option<Type>) {
        let struct_id = NodeID::generate();
        self.create_enum_struct(struct_id, name.clone(), selector, ttype.clone().map(|t| self.transform_value_type(&t)));
        self.set_type(id, LLType::Alias(struct_id));

        if ttype.is_some() {
            let ftype = self.session.get_type(id).unwrap();
            let (argtypes, rettype, _) = ftype.get_function_types().unwrap();
            let lftype = self.transform_cfunc_def_type(&argtypes.as_vec(), rettype);

            let mut params = vec!();
            let mut tuple_items = vec!();
            for (i, arg) in argtypes.as_vec().iter().enumerate() {
                let arg_id = NodeID::generate();
                tuple_items.push(LLExpr::GetValue(arg_id));
                params.push((arg_id, format!("value{}", i)));
            }

            let tuple_id = NodeID::generate();
            let body = vec!(LLExpr::DefStruct(NodeID::generate(), self.get_type(struct_id).unwrap(), vec!(
                LLExpr::Literal(LLLit::I8(variant as i8)),
                LLExpr::DefStruct(NodeID::generate(), self.transform_value_type(argtypes), tuple_items)
            )));
            self.add_global(LLGlobal::DefCFunc(id, LLLink::Once, name, lftype, params, body, LLCC::CCC));
        }
    }

    fn create_enum_struct(&self, id: NodeID, name: String, selector: LLType, ltype: Option<LLType>) {
        let mut body = vec!(selector);
        match ltype {
            Some(ltype) => body.push(ltype),
            None => { },
        }

        self.add_global(LLGlobal::DefNamedStruct(id, name.clone(), false));
        self.add_global(LLGlobal::SetStructBody(id, body, false));
        self.set_type(id, LLType::Alias(id));
    }

    fn transform_def_local(&self, scope: ScopeRef, id: NodeID, name: &String, value: &AST) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let valexpr = self.transform_as_result(&mut exprs, scope.clone(), value).unwrap();
        let ltype = self.transform_value_type(&self.session.get_type(id).unwrap());
        exprs.push(LLExpr::DefLocal(id, name.clone(), ltype, r(valexpr)));
        exprs
    }

    fn transform_import(&self, scope: ScopeRef, name: &String, decls: &Vec<AST>) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let rid = NodeID::generate();
        let module_run_name = format!("run_{}", name.replace(".", "_"));
        let rftype = LLType::Function(vec!(), r(LLType::I64));
        self.add_global(LLGlobal::DeclCFunc(rid, module_run_name, rftype, LLCC::FastCC));
        exprs.extend(self.create_cfunc_invoke(LLExpr::GetValue(rid), vec!()));
        exprs.extend(self.transform_vec(scope.clone(), decls));
        exprs
    }

    fn transform_try(&self, scope: ScopeRef, code: &AST, cases: &Vec<MatchCase>) -> Vec<LLExpr> {
        let mut exprs = vec!();

        let exp_id = NodeID::generate();
        let expoint = self.create_exception_point(&mut exprs, exp_id);
        let expoint_id = NodeID::generate();
        exprs.push(LLExpr::SetValue(expoint_id, r(expoint)));

        let tryblock = self.with_exception(exp_id, || {
            self.transform_node(scope.clone(), code)
        });


        // TODO either match on the return from setjmp or get the return value from the expoint
        let matchblock = self.transform_match(scope.clone(), &AST::GetValue(expoint_id), cases);
        //let expret_id = NodeID::generate();
        //exprs.push(LLExpr::SetValue(expret_id, r(LLExpr::GetItem(r(LLExpr::GetLocal(exp_id)), 1))));
        //let matchblock = self.transform_match(scope.clone(), &AST::GetValue(expret_id), cases);

        exprs.extend(self.create_exception_block(LLExpr::GetValue(expoint_id), tryblock, matchblock));
        exprs
    }

    fn create_exception_point(&self, exprs: &mut Vec<LLExpr>, exp_id: NodeID) -> LLExpr {
        exprs.push(LLExpr::DefLocal(exp_id, String::from("__exception__"), LLType::ExceptionPoint, r(LLExpr::Literal(LLLit::Null(LLType::ExceptionPoint)))));

        let ret_id = NodeID::generate();
        exprs.push(LLExpr::SetValue(ret_id, r(LLExpr::CallC(r(LLExpr::GetNamed("setjmp".to_string())), vec!(LLExpr::GetValue(exp_id)), LLCC::CCC))));
        LLExpr::GetValue(ret_id)
    }

    fn create_exception_block(&self, expoint: LLExpr, try: Vec<LLExpr>, catch: Vec<LLExpr>) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let iszero = LLExpr::Cmp(LLCmpType::Equal, r(expoint), r(LLExpr::Literal(LLLit::I32(0))));
        exprs.push(LLExpr::Phi(vec!(vec!(iszero), vec!(LLExpr::Literal(LLLit::I1(true)))), vec!(try, catch)));
        exprs
    }

    fn transform_raise(&self, scope: ScopeRef, id: NodeID, valexpr: &AST) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let exp_id = self.get_exception().unwrap();
        let value = self.transform_as_result(&mut exprs, scope.clone(), valexpr).unwrap();

        // TODO either pass the return value to longjmp or set it in the expoint and pass 1 to longjmp...
        exprs.push(LLExpr::CallC(r(LLExpr::GetNamed("longjmp".to_string())), vec!(LLExpr::GetValue(exp_id), value), LLCC::CCC));
        //exprs.push(LLExpr::StoreRef(r(LLExpr::AccessRef(r(LLExpr::GetValue(exp_id)), vec!(LLRef::Field(1)))), r(LLExpr::Cast(LLType::Var, r(value)))));
        //exprs.push(LLExpr::CallC(r(LLExpr::GetNamed("longjmp".to_string())), vec!(LLExpr::GetValue(exp_id), LLExpr::Literal(LLLit::I32(1))), LLCC::CCC));

        exprs.push(LLExpr::Literal(LLLit::I32(0)));
        exprs
    }





    fn transform_func_name(&self, scope: ScopeRef, name: Option<&String>, id: NodeID) -> String {
        let ftype = self.session.get_type(id).unwrap();
        scope.get_full_name(name.map(|name| ftype.get_abi().unwrap_or(ABI::Molten).mangle_name(name, ftype.get_argtypes().unwrap(), 2)), id)
    }

    pub fn transform_func_def_type(&self, abi: ABI, args: &Vec<Type>, ret: &Type) -> LLType {
        match abi {
            ABI::C | ABI::MoltenFunc => self.transform_cfunc_def_type(args, ret),
            ABI::Molten | ABI::Unknown => self.transform_closure_def_type(args, ret),
            _ => panic!("Not Implemented: {:?}", abi),
        }
    }

    fn transform_func_decl(&self, scope: ScopeRef, abi: ABI, id: NodeID, vis: Visibility, name: &String, ttype: &Type) -> Vec<LLExpr> {
        match abi {
            ABI::C | ABI::MoltenFunc => self.transform_cfunc_decl(scope.clone(), id, vis, name, ttype),
            ABI::Molten | ABI::Unknown => self.transform_closure_decl(scope.clone(), id, vis, name, ttype),
            _ => panic!("Not Implemented: {:?}", abi),
        }
    }

    pub fn transform_func_def(&self, scope: ScopeRef, abi: ABI, id: NodeID, vis: Visibility, name: Option<&String>, args: &Vec<Argument>, body: &AST) -> Vec<LLExpr> {
        match abi {
            ABI::C | ABI::MoltenFunc => self.transform_cfunc_def(scope.clone(), id, vis, name, args, body),
            ABI::Molten | ABI::Unknown => self.transform_closure_def(scope.clone(), id, vis, name, args, body),
            _ => panic!("Not Implemented: {:?}", abi),
        }
    }

    fn transform_func_invoke(&self, scope: ScopeRef, abi: ABI, id: NodeID, func: &AST, args: &Vec<AST>) -> Vec<LLExpr> {
        match abi {
            ABI::C | ABI::MoltenFunc => self.transform_cfunc_invoke(scope.clone(), id, func, args),
            ABI::Molten | ABI::Unknown => self.transform_closure_invoke(scope.clone(), id, func, args),
            _ => panic!("Not Implemented: {:?}", abi),
        }
    }

    fn create_func_invoke(&self, abi: ABI, func: LLExpr, fargs: Vec<LLExpr>) -> Vec<LLExpr> {
        match abi {
            ABI::C | ABI::MoltenFunc => self.create_cfunc_invoke(func, fargs),
            ABI::Molten | ABI::Unknown => self.create_closure_invoke(func, fargs),
            _ => panic!("Not Implemented: {:?}", abi),
        }
    }

    fn transform_func_as_result(&self, exprs: &mut Vec<LLExpr>, scope: ScopeRef, func: &AST, fargs: &mut Vec<LLExpr>) -> LLExpr {
        match func {
            AST::Accessor(id, _, _, ident, oid) => {
                let otype = self.session.get_type(*oid).unwrap();
                match otype {
                    Type::Object(_, _, _) => {
                        // Convert method-style calls
                        let defid = self.session.get_ref(*id).unwrap();
                        exprs.push(LLExpr::SetValue(*oid, r(fargs[0].clone())));
                        fargs[0] = LLExpr::GetValue(*oid);
                        exprs.extend(self.convert_accessor(defid, LLExpr::GetValue(*oid), &ident.name, otype));
                        exprs.pop().unwrap()
                    },
                    _ => self.transform_as_result(exprs, scope.clone(), func).unwrap(),
                }
            },
            _ => self.transform_as_result(exprs, scope.clone(), func).unwrap(),
        }
    }

    fn transform_vis(&self, vis: Visibility) -> LLLink {
        match vis {
            Visibility::Private => LLLink::Private,
            Visibility::Public => LLLink::Public,
        }
    }






    pub fn transform_cfunc_def_type(&self, args: &Vec<Type>, ret: &Type) -> LLType {
        let mut argtypes = vec!();
        for arg in args {
            argtypes.push(self.transform_value_type(arg));
        }

        // TODO this is going to have to be special for returns, because sometimes a return value needs to be converted to a memory address argument
        let rettype = self.transform_value_type(ret);

        LLType::Function(argtypes, r(rettype))
    }

    fn transform_cfunc_decl(&self, scope: ScopeRef, id: NodeID, vis: Visibility, name: &String, ttype: &Type) -> Vec<LLExpr> {
        let fname = self.transform_func_name(scope.clone(), Some(name), id);
        let ftype = self.session.get_type(id).unwrap();
        let (argtypes, rettype, _) = ftype.get_function_types().unwrap();
        let lftype = self.transform_cfunc_def_type(&argtypes.as_vec(), rettype);
        self.add_global(LLGlobal::DeclCFunc(id, fname, lftype, LLCC::CCC));
        vec!(LLExpr::GetValue(id))
    }

    fn transform_cfunc_def_args(&self, args: &Vec<Argument>) -> Vec<(NodeID, String)> {
        args.iter().map(|arg| (arg.id, arg.ident.name.clone())).collect()
    }

    fn transform_cfunc_def(&self, scope: ScopeRef, id: NodeID, vis: Visibility, name: Option<&String>, args: &Vec<Argument>, body: &AST) -> Vec<LLExpr> {
        let fscope = self.session.map.get(&id);
        let fname = self.transform_func_name(scope.clone(), name, id);

        let ftype = self.session.get_type(id).unwrap();
        let (argtypes, rettype, _) = ftype.get_function_types().unwrap();
        let lftype = self.transform_cfunc_def_type(&argtypes.as_vec(), rettype);
        self.set_type(id, lftype.clone());

        let fargs = self.transform_cfunc_def_args(args);

        self.with_context(CodeContext::Func(ABI::C, id), || {
            self.add_global(LLGlobal::DefCFunc(id, self.transform_vis(vis), fname, lftype, fargs, self.transform_node(fscope.clone(), body), LLCC::CCC));
        });
        vec!(LLExpr::GetValue(id))
    }

    fn transform_cfunc_invoke(&self, scope: ScopeRef, id: NodeID, func: &AST, args: &Vec<AST>) -> Vec<LLExpr> {
        let mut exprs = vec!();

        let mut fargs = self.transform_as_args(&mut exprs, scope.clone(), args);

        let funcresult = self.transform_func_as_result(&mut exprs, scope.clone(), func, &mut fargs);

        exprs.extend(self.create_cfunc_invoke(funcresult, fargs));
        exprs
    }

    fn create_cfunc_invoke(&self, func: LLExpr, fargs: Vec<LLExpr>) -> Vec<LLExpr> {
        vec!(LLExpr::CallC(r(func), fargs, LLCC::CCC))
    }





    fn transform_mfunc_def_type(&self, args: &Vec<Type>, ret: &Type) -> LLType {
        let ltype = self.transform_cfunc_def_type(args, ret);
        self.convert_to_mfunc_def_type(ltype)
    }

    fn convert_to_mfunc_def_type(&self, ltype: LLType) -> LLType {
        match ltype {
            LLType::Function(mut args, ret) => {
                args.push(LLType::Ptr(r(LLType::ExceptionPoint)));
                //args.push(LLType::Ptr(r(LLType::I8)));
                LLType::Function(args, ret)
            },
            _ => ltype
        }
    }

    fn convert_mfunc_molten_type(&self, scope: ScopeRef, ttype: Type) -> Type {
        match ttype {
            Type::Function(argtypes, rettype, _) => {
                let mut argtypes = argtypes.as_vec();
                argtypes.push(scope.find_type(self.session, &String::from(EXCEPTION_POINT_NAME)).unwrap());
                Type::Function(r(Type::Tuple(argtypes)), rettype, ABI::C)
            },
            ttype @ _ => ttype,
        }
    }

    fn convert_mfunc_def_args(&self, fscope: ScopeRef, exp_id: NodeID, fargs: &mut Vec<(NodeID, String)>) {
        fargs.push((exp_id, String::from("__exception__")));
    }

    fn transform_mfunc_def(&self, scope: ScopeRef, id: NodeID, vis: Visibility, name: Option<&String>, args: &Vec<Argument>, body: &AST) -> Vec<LLExpr> {
        let fscope = self.session.map.get(&id);
        let fname = self.transform_func_name(scope.clone(), name, id);

        let ftype = self.session.get_type(id).unwrap();
        let (argtypes, rettype, _) = ftype.get_function_types().unwrap();
        let lftype = self.transform_mfunc_def_type(&argtypes.as_vec(), rettype);
        self.set_type(id, lftype.clone());

        let exp_id = NodeID::generate();
        let mut fargs = self.transform_cfunc_def_args(args);
        self.convert_mfunc_def_args(scope.clone(), exp_id, &mut fargs);

        self.with_context(CodeContext::Func(ABI::MoltenFunc, id), || {
            self.with_exception(exp_id, || {
                self.add_global(LLGlobal::DefCFunc(id, self.transform_vis(vis), fname, lftype, fargs, self.transform_node(fscope.clone(), body), LLCC::FastCC));
            });
        });
        vec!(LLExpr::GetValue(id))
    }

    fn transform_mfunc_invoke(&self, scope: ScopeRef, id: NodeID, func: &AST, args: &Vec<AST>) -> Vec<LLExpr> {
        let mut exprs = vec!();

        let mut fargs = self.transform_as_args(&mut exprs, scope.clone(), args);

        let funcresult = self.transform_func_as_result(&mut exprs, scope.clone(), func, &mut fargs);

        exprs.extend(self.create_mfunc_invoke(funcresult, fargs));
        exprs
    }

    fn create_mfunc_invoke(&self, func: LLExpr, mut fargs: Vec<LLExpr>) -> Vec<LLExpr> {
        fargs.push(LLExpr::GetValue(self.get_exception().unwrap()));
        vec!(LLExpr::CallC(r(func), fargs, LLCC::FastCC))
        //self.create_cfunc_invoke(func, fargs)
    }





    fn transform_closure_def_type(&self, args: &Vec<Type>, ret: &Type) -> LLType {
        let ltype = self.transform_cfunc_def_type(args, ret);
        self.convert_to_closure_def_type(ltype)
    }

    fn convert_to_closure_def_type(&self, ltype: LLType) -> LLType {
        match ltype {
            LLType::Function(mut args, ret) => {
                args.push(LLType::Ptr(r(LLType::I8)));
                self.convert_to_mfunc_def_type(LLType::Function(args, ret))
            },
            _ => ltype
        }
    }

    fn transform_closure_value_type(&self, args: &Vec<Type>, ret: &Type) -> LLType {
        let ltype = self.transform_closure_def_type(args, ret);
        self.convert_to_closure_value_type(ltype)
    }

    fn convert_to_closure_value_type(&self, ltype: LLType) -> LLType {
        LLType::Ptr(r(LLType::Struct(vec!(LLType::Ptr(r(ltype))))))
    }

    fn convert_closure_molten_type(&self, scope: ScopeRef, ttype: Type) -> Type {
        match ttype {
            Type::Function(mut argtypes, rettype, _) => {
                let mut argtypes = argtypes.as_vec();
                argtypes.push(scope.find_type(self.session, &String::from("String")).unwrap());
                self.convert_mfunc_molten_type(scope.clone(), Type::Function(r(Type::Tuple(argtypes)), rettype, ABI::C))
            },
            ttype @ _ => ttype,
        }
    }

    fn transform_closure_raw_func_data(&self, scope: ScopeRef, id: NodeID, fname: &String) -> (NodeID, String, LLType) {
        let ftype = self.session.get_type(id).unwrap();
        let (argtypes, rettype, _) = ftype.get_function_types().unwrap();
        let cftype = self.transform_closure_def_type(&argtypes.as_vec(), rettype);
        let cfid = NodeID::generate();
        let cfname = format!("{}_func", fname);
        self.set_type(cfid, cftype.clone());
        (cfid, cfname, cftype)
    }

    fn transform_closure_decl(&self, scope: ScopeRef, id: NodeID, vis: Visibility, name: &String, ttype: &Type) -> Vec<LLExpr> {
        let fname = self.transform_func_name(scope.clone(), Some(name), id);

        let did = NodeID::generate();
        self.add_global(LLGlobal::DefGlobal(did, LLLink::Once, fname.clone(), self.transform_value_type(ttype)));
        vec!(LLExpr::SetValue(id, r(LLExpr::GetLocal(did))))
    }

    fn convert_closure_def_args(&self, fscope: ScopeRef, cl: ClosureDefRef, exp_id: NodeID, fargs: &mut Vec<(NodeID, String)>) {
        fargs.push((cl.context_arg_id, String::from("__context__")));
        self.convert_mfunc_def_args(fscope.clone(), exp_id, fargs);
    }

    fn transform_closure_def(&self, scope: ScopeRef, id: NodeID, vis: Visibility, name: Option<&String>, args: &Vec<Argument>, body: &AST) -> Vec<LLExpr> {
        let fscope = self.session.map.get(&id);
        let fname = self.transform_func_name(scope.clone(), name, id);
        let (cfid, cfname, cftype) = self.transform_closure_raw_func_data(scope.clone(), id, &fname);

        let cl = self.session.get_def(id).unwrap().as_closure().unwrap();

        // Add context argument to transformed arguments list
        let exp_id = NodeID::generate();
        let mut fargs = self.transform_cfunc_def_args(args);
        self.convert_closure_def_args(fscope.clone(), cl.clone(), exp_id, &mut fargs);

        let ptype = self.convert_closure_molten_type(scope.clone(), self.session.get_type(id).unwrap());
        cl.add_field(self.session, cfid, "__func__", ptype.clone(), Define::Never);

        // Transforms body and create C function definition
        let index = self.globals.borrow().len();
        let body = self.with_context(CodeContext::Func(ABI::Molten, id), || {
            self.with_exception(exp_id, || {
                self.transform_node(fscope.clone(), body)
            })
        });
        self.insert_global(index, LLGlobal::DefCFunc(cfid, self.transform_vis(vis), cfname.clone(), cftype, fargs, body, LLCC::FastCC));

        let structtype = LLType::Ptr(r(self.transform_struct_def(&cl.context_struct)));
        self.insert_global(index, LLGlobal::DefType(cl.context_type_id, format!("__context_{}__", cl.context_type_id), structtype.clone()));


        FuncDef::define(self.session, scope.clone(), cfid, cl.vis, &Some(cfname.clone()), Some(ptype)).unwrap();
        let mut fields = vec!();
        cl.context_struct.foreach_field(|defid, field, _| {
            let rid = NodeID::generate();
            self.session.set_ref(rid, defid);
            if field.as_str() == "__func__" {
                fields.push((Ident::from_str("__func__"), AST::Identifier(rid, Pos::empty(), Ident::new(cfname.clone()))));
            } else {
                fields.push((Ident::from_str(field.as_str()), AST::Identifier(rid, Pos::empty(), Ident::new(field.clone()))));
            }
        });

        let mut code = vec!();
        let did = NodeID::generate();
        code.push(AST::Definition(did, Pos::empty(), Mutability::Mutable, Ident::new(fname.clone()), None, r(AST::make_ref(Pos::empty(), AST::make_record(Pos::empty(), fields)))));
        // TODO I'm going back on my decision to use a tuple pair to represent the function and context reference because it can't be converted to i8* (the generics type)
        //      Once I have generics that can operate on different sized data instead of only references, I can switch back
        //code.push(AST::Tuple(NodeID::generate(), Pos::empty(), vec!(AST::make_ident_from_str(Pos::empty(), real_fname.as_str()), AST::make_ident(Pos::empty(), Ident::new(cname.clone())))));

        binding::bind_names(self.session, scope.clone(), &code);
        typecheck::TypeChecker::check(self.session, scope.clone(), &code);
        let mut exprs = self.transform_vec(scope.clone(), &code);
        exprs.push(LLExpr::SetValue(id, r(LLExpr::GetLocal(did))));
        exprs.push(LLExpr::GetValue(id));

        if vis == Visibility::Public {
            let gid = NodeID::generate();
            self.add_global(LLGlobal::DefGlobal(gid, LLLink::Once, fname.clone(), structtype));
            exprs.push(LLExpr::SetGlobal(gid, r(LLExpr::GetLocal(did))));
        }

        exprs
    }

    fn transform_closure_invoke(&self, scope: ScopeRef, id: NodeID, func: &AST, args: &Vec<AST>) -> Vec<LLExpr> {
        let mut exprs = vec!();

        let mut fargs = self.transform_as_args(&mut exprs, scope.clone(), args);

        let fid = NodeID::generate();
        let funcresult = self.transform_func_as_result(&mut exprs, scope.clone(), func, &mut fargs);
        exprs.push(LLExpr::SetValue(fid, r(funcresult)));

        exprs.extend(self.create_closure_invoke(LLExpr::GetValue(fid), fargs));
        exprs
    }

    fn create_closure_invoke(&self, func: LLExpr, mut fargs: Vec<LLExpr>) -> Vec<LLExpr> {
        let mut exprs = vec!();

        fargs.push(LLExpr::Cast(LLType::Ptr(r(LLType::I8)), r(func.clone())));

        let function = LLExpr::LoadRef(r(LLExpr::AccessRef(r(func), vec!(LLRef::Field(0)))));
        exprs.extend(self.create_mfunc_invoke(function, fargs));
        exprs
    }





    fn create_reference(&self, defid: NodeID) -> Vec<LLExpr> {
        match self.session.get_def(defid) {
            Ok(Def::Var(_)) => vec!(LLExpr::GetLocal(defid)),
            Ok(_) => vec!(LLExpr::GetValue(defid)),
            Err(_) => panic!("TransformError: attempting to reference a non-existent value"),
        }
    }

    fn transform_reference(&self, scope: ScopeRef, defid: NodeID, name: &String) -> Vec<LLExpr> {
        if
            !scope.contains_context(name)
            && !Scope::global(scope.clone()).contains(name)
            && !self.session.get_def(defid).unwrap().is_globally_accessible()
        {
            match self.get_context() {
                Some(CodeContext::Func(ABI::Molten, ref cid)) => {
                    let cl = self.session.get_def(*cid).unwrap().as_closure().unwrap();
                    if *cid == defid {
                        vec!(LLExpr::Cast(LLType::Alias(cl.context_type_id), r(LLExpr::GetValue(cl.context_arg_id))))
                    } else {
                        let index = cl.find_or_add_field(self.session, defid, name.as_str(), self.session.get_type(defid).unwrap());
                        let context = LLExpr::Cast(LLType::Alias(cl.context_type_id), r(LLExpr::GetValue(cl.context_arg_id)));
                        vec!(LLExpr::LoadRef(r(LLExpr::AccessRef(r(context), vec!(LLRef::Field(index))))))
                    }
                },
                _ => panic!("Cannot access variable outside of scope: {:?}", name),
            }
        } else {
            self.create_reference(defid)
        }
    }



    fn transform_alloc_ref(&self, scope: ScopeRef, id: NodeID, value: &AST) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let valexpr = self.transform_as_result(&mut exprs, scope.clone(), value).unwrap();
        let ltype = self.transform_value_type(&self.session.get_type(id).unwrap());
        exprs.push(LLExpr::AllocRef(id, ltype, Some(r(valexpr))));
        exprs
    }

    fn transform_deref_ref(&self, scope: ScopeRef, value: &AST) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let valexpr = self.transform_as_result(&mut exprs, scope.clone(), value).unwrap();
        exprs.push(LLExpr::LoadRef(r(valexpr)));
        exprs
    }

    fn transform_new_object(&self, id: NodeID) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let defid = self.session.get_ref(id).unwrap();
        let ltype = self.transform_value_type(&self.session.get_type(defid).unwrap());

        exprs.push(LLExpr::AllocRef(id, ltype, None));
        if let Def::Class(classdef) = self.session.get_def(defid).unwrap() {
            if let Some(index) = classdef.get_struct_vtable_index() {
                exprs.push(LLExpr::StoreRef(r(LLExpr::AccessRef(r(LLExpr::GetValue(id)), vec!(LLRef::Field(index)))), r(LLExpr::GetLocal(classdef.vtable.id))));
                exprs.push(LLExpr::GetValue(id));
            }
        }
        exprs
    }

    fn transform_class_type_data(&self, scope: ScopeRef, classdef: ClassDefRef, body: &Vec<AST>) {
        classdef.build_vtable(self.session, scope.clone(), body);
        classdef.build_structdef(self.session, scope.clone(), body);

        self.add_global(LLGlobal::DefNamedStruct(classdef.id, classdef.classname.clone(), true));
        self.set_type(classdef.id, LLType::Alias(classdef.id));
        self.add_global(LLGlobal::DefNamedStruct(classdef.vtable.id, format!("{}_vtable", classdef.classname.clone()), true));
        self.set_type(classdef.vtable.id, LLType::Alias(classdef.vtable.id));

        let stype = self.transform_struct_def(&classdef.structdef);
        self.add_global(LLGlobal::SetStructBody(classdef.id, stype.get_items(), true));

        let vtype = self.transform_vtable_def(&classdef.vtable);
        self.add_global(LLGlobal::SetStructBody(classdef.vtable.id, vtype.get_items(), true));
    }

    fn transform_vtable_def(&self, vtable: &Vtable) -> LLType {
        let mut items = vec!();
        vtable.foreach_entry(|_, _, ttype| {
            items.push(self.transform_value_type(ttype));
        });
        LLType::Struct(items)
    }

    fn transform_vtable_init(&self, classdef: ClassDefRef) -> Vec<LLExpr> {
        if !classdef.has_vtable() {
            return vec!();
        }

        let mut exprs = vec!();
        let tscope = self.session.map.get(&classdef.id);

        self.add_global(LLGlobal::DefGlobal(classdef.vtable.id, LLLink::Once, format!("__{}_vtable", tscope.get_basename()), self.get_type(classdef.vtable.id).unwrap()));
        // TODO should vtables be dynamically allocated, or should we add a LLType::ElementOf() type or something to GetElement an aliased type
        exprs.push(LLExpr::SetGlobal(classdef.vtable.id, r(LLExpr::AllocRef(NodeID::generate(), self.get_type(classdef.vtable.id).unwrap(), None))));
        classdef.vtable.foreach_enumerated(|i, id, _, ttype| {
            let ltype = self.transform_value_type(ttype);
            let field = LLExpr::AccessRef(r(LLExpr::GetGlobal(classdef.vtable.id)), vec!(LLRef::Field(i)));
            exprs.push(LLExpr::StoreRef(r(field), r(LLExpr::Cast(ltype, r(LLExpr::GetValue(id))))));
            //exprs.push(LLExpr::SetItem(r(LLExpr::GetLocal(classdef.vtable.id)), i, r(LLExpr::Cast(ltype, r(LLExpr::GetValue(id))))));
        });

        exprs
    }

    fn transform_class_body(&self, scope: ScopeRef, id: NodeID, body: &Vec<AST>) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let tscope = self.session.map.get(&id);
        let classdef = self.session.get_def(id).unwrap().as_class().unwrap();

        self.transform_class_type_data(scope.clone(), classdef.clone(), body);

        for node in body {
            match node {
                AST::Function(id, _, vis, ident, args, _, body, abi) => {
                    // TODO i switched to using scope here instead of tscope because it was causing problems with references inside closures
                    exprs.extend(self.transform_func_def(tscope.clone(), *abi, *id, *vis, ident.as_ref().map(|ident| &ident.name), args, body));
                },
                AST::Declare(id, _, vis, ident, _) => {
                    let ttype = self.session.get_type(*id).unwrap();
                    exprs.extend(self.transform_func_decl(tscope.clone(), ttype.get_abi().unwrap(), *id, *vis, &ident.name, &ttype));
                },
                AST::Definition(_, _, _, _, _, _) => { },
                _ => panic!("Not Implemented: {:?}", node),
            }
        }

        exprs.extend(self.transform_vtable_init(classdef));
        exprs
    }

    fn transform_accessor(&self, scope: ScopeRef, id: NodeID, obj: &AST, field: &String, otype: Type) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let defid = self.session.get_ref(id).unwrap();
        let objval = self.transform_as_result(&mut exprs, scope.clone(), obj).unwrap();
        exprs.extend(self.convert_accessor(defid, objval, field, otype));
        exprs
    }

    fn convert_accessor(&self, defid: NodeID, objval: LLExpr, field: &String, otype: Type) -> Vec<LLExpr> {
        let mut exprs = vec!();
        match otype {
            Type::Object(_, objid, _) => {
                let objdef = self.session.get_def(objid).unwrap();
                match self.session.get_def(defid) {
                    Ok(Def::Method(_)) => {
                        let classdef = objdef.as_class().unwrap();
                        let vindex = classdef.get_struct_vtable_index().unwrap();
                        let index = classdef.vtable.get_index_by_id(defid).unwrap();
                        let vtable = LLExpr::LoadRef(r(LLExpr::AccessRef(r(objval), vec!(LLRef::Field(vindex)))));
                        exprs.push(LLExpr::LoadRef(r(LLExpr::AccessRef(r(vtable), vec!(LLRef::Field(index))))));
                    },
                    Ok(Def::Field(_)) => {
                        let (index, _) = objdef.as_struct().unwrap().find_field_by_id(defid).unwrap();
                        exprs.push(LLExpr::LoadRef(r(LLExpr::AccessRef(r(objval), vec!(LLRef::Field(index))))));
                    },
                    Err(_) => {
                        return vec!(LLExpr::GetValue(defid));
                    },
                    Ok(def) => panic!("Not Implemented: {:?}", def),
                }
            },
            Type::Record(items) => {
                let index = items.iter().position(|(name, _)| name == field).unwrap();
                exprs.push(LLExpr::GetItem(r(objval), index));
            },
            Type::Tuple(_) => {
                let index = field.parse::<usize>().unwrap();
                exprs.push(LLExpr::GetItem(r(objval), index));
            },
            _ => panic!("Not Implemented: {:?}", otype),
        }
        exprs
    }

    fn transform_resolve(&self, id: NodeID, path: &AST, field: &String, otype: Type) -> Vec<LLExpr> {
        let defid = self.session.get_ref(id).unwrap();
        match self.session.get_def(otype.get_id().unwrap()).unwrap() {
            Def::Class(classdef) => {
                let index = classdef.vtable.get_index_by_id(defid).unwrap();
                vec!(LLExpr::LoadRef(r(LLExpr::AccessRef(r(LLExpr::GetGlobal(classdef.vtable.id)), vec!(LLRef::Field(index))))))
            },
            Def::Enum(enumdef) => {
                match enumdef.get_variant_type_by_id(defid) {
                    Some(_) => vec!(LLExpr::GetValue(defid)),
                    None => {
                        let variant = enumdef.get_variant_by_id(defid).unwrap();
                        vec!(LLExpr::DefStruct(id, self.get_type(defid).unwrap(), vec!(LLExpr::Literal(LLLit::I8(variant as i8)))))
                    },
                }
            },
            def @ _ => panic!("DefError: expected class or enum but found {:?}", def),
        }
    }

    fn transform_assignment(&self, scope: ScopeRef, id: NodeID, left: &AST, right: &AST) -> Vec<LLExpr> {
        let mut exprs = vec!();

        let value = self.transform_as_result(&mut exprs, scope.clone(), right).unwrap();
        match left {
            AST::Accessor(aid, _, obj, _, oid) => {
                let objval = self.transform_as_result(&mut exprs, scope.clone(), obj).unwrap();
                let fieldid = self.session.get_ref(*aid).unwrap();
                let objdef = self.session.get_def(self.session.get_type(*oid).unwrap().get_id().unwrap()).unwrap();
                let (index, _) = objdef.as_struct().unwrap().find_field_by_id(fieldid).unwrap();
                exprs.push(LLExpr::StoreRef(r(LLExpr::AccessRef(r(objval), vec!(LLRef::Field(index)))), r(value)));
            },
            AST::Identifier(aid, _, _) => {
                let defid = self.session.get_ref(*aid).unwrap();
                exprs.push(LLExpr::StoreRef(r(LLExpr::GetValue(defid)), r(value)));
            },
            AST::Deref(_, _, node) => {
                let result = self.transform_as_result(&mut exprs, scope.clone(), node).unwrap();
                exprs.push(LLExpr::StoreRef(r(result), r(value)));
            },
            _ => panic!("InternalError: attempting to assign to an invalid pattern, {:?}", left),
        }
        exprs
    }



    fn transform_if_expr(&self, scope: ScopeRef, cond: &AST, texpr: &AST, fexpr: &AST) -> Vec<LLExpr> {
        let mut conds = vec!();
        conds.push(self.transform_node(scope.clone(), cond));
        conds.push(vec!(LLExpr::Literal(LLLit::I1(true))));

        let mut blocks = vec!();
        blocks.push(self.transform_node(scope.clone(), texpr));
        blocks.push(self.transform_node(scope.clone(), fexpr));

        vec!(LLExpr::Phi(conds, blocks))
    }

    fn transform_match(&self, scope: ScopeRef, cond: &AST, cases: &Vec<MatchCase>) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let mut conds = vec!();
        let mut blocks = vec!();

        let condid = NodeID::generate();
        let condval = self.transform_as_result(&mut exprs, scope.clone(), cond).unwrap();
        exprs.push(LLExpr::SetValue(condid, r(condval)));

        for case in cases {
            let lscope = self.session.map.get(&case.id);
            conds.push(self.transform_pattern(lscope.clone(), &case.pat, condid));
            blocks.push(self.transform_node(lscope.clone(), &case.body));
        }

        exprs.push(LLExpr::Phi(conds, blocks));
        exprs
    }

    fn transform_pattern(&self, scope: ScopeRef, pat: &Pattern, value_id: NodeID) -> Vec<LLExpr> {
        let mut exprs = vec!();

        match pat {
            Pattern::Wild => exprs.push(LLExpr::Literal(LLLit::I1(true))),
            Pattern::Literal(id, lit) => {
                let compfunc = self.transform_as_result(&mut exprs, scope.clone(), &AST::Identifier(*id, Pos::empty(), Ident::from_str("=="))).unwrap();
                let compabi = self.session.get_type(*id).unwrap().get_abi().unwrap();
                let result = self.transform_as_result(&mut exprs, scope.clone(), lit).unwrap();
                exprs.extend(self.create_func_invoke(compabi, compfunc, vec!(LLExpr::GetValue(value_id), result)));
            },
            Pattern::Binding(id, ident) => {
                exprs.extend(self.transform_def_local(scope.clone(), *id, &ident.name, &AST::GetValue(value_id)));
                exprs.push(LLExpr::Literal(LLLit::I1(true)));
            },
            Pattern::Annotation(id, _, pat) => {
                let ttype = self.session.get_type(*id).unwrap();
                exprs.push(LLExpr::SetValue(*id, r(LLExpr::Cast(self.transform_value_type(&ttype), r(LLExpr::GetValue(value_id))))));
                exprs.extend(self.transform_pattern(scope.clone(), pat, *id));
            },
            Pattern::Resolve(id, left, field, oid) => {
                let defid = self.session.get_ref(*id).unwrap();
                let enumdef = self.session.get_def_from_ref(*oid).unwrap().as_enum().unwrap();
                let variant = enumdef.get_variant_by_id(defid).unwrap();
                exprs.push(LLExpr::Cmp(LLCmpType::Equal, r(LLExpr::GetItem(r(LLExpr::GetValue(value_id)), 0)), r(LLExpr::Literal(LLLit::I8(variant as i8)))));
            },
            Pattern::EnumArgs(id, left, args) => {
                let variant_id = self.session.get_ref(*id).unwrap();
                let item_id = NodeID::generate();
                exprs.push(LLExpr::SetValue(item_id, r(LLExpr::GetItem(r(LLExpr::Cast(self.get_type(variant_id).unwrap(), r(LLExpr::GetValue(value_id)))), 1))));
                for (i, arg) in args.iter().enumerate() {
                    let arg_id = NodeID::generate();
                    exprs.push(LLExpr::SetValue(arg_id, r(LLExpr::GetItem(r(LLExpr::GetValue(item_id)), i))));
                    exprs.extend(self.transform_pattern(scope.clone(), &arg, arg_id));
                }
                //let result = exprs.pop();
                exprs.extend(self.transform_pattern(scope.clone(), left, value_id));
            },
            _ => panic!("Not Implemented: {:?}", pat),
        }
        exprs
    }

    /*
    fn find_func(&self, scope: ScopeRef, name: &str, argtypes: &Vec<Type>) -> NodeID {
        let compid = scope.get_var_def(&String::from(name)).unwrap();
        match self.session.get_def(compid) {
            Ok(Def::Overload(ol)) => ol.find_variant(self.session, scope.clone(), Type::Tuple(argtypes.clone())).unwrap().0,
            _ => compid,
        }
    }
    */

    fn transform_side_effect(&self, scope: ScopeRef, op: &str, args: &Vec<AST>) -> Vec<LLExpr> {
        let mut conds = vec!();
        let mut blocks = vec!();

        // TODO this doesn't work with non-boolean values
        match op {
            "and" => {
                conds.push(self.transform_node(scope.clone(), &args[0]));
                blocks.push(self.transform_node(scope.clone(), &args[1]));
                vec!(LLExpr::Phi(conds, blocks))
            },
            "or" => {
                conds.push(self.transform_node(scope.clone(), &args[0]));
                blocks.push(vec!(LLExpr::Literal(LLLit::I1(true))));
                conds.push(vec!(LLExpr::Literal(LLLit::I1(true))));
                blocks.push(self.transform_node(scope.clone(), &args[1]));
                vec!(LLExpr::Phi(conds, blocks))
            },
            _ => panic!("Not Implemented: {:?}", op),
        }
    }



    pub fn transform_value_type(&self, ttype: &Type) -> LLType {
        match &ttype {
            Type::Object(name, id, _) => match name.as_str() {
                // TODO void is not entirely correct, and it was causing issues with return values
                //"()" => LLType::Void,
                "()" => LLType::I32,
                "Nil" => LLType::Ptr(r(LLType::I8)),
                "Bool" => LLType::I1,
                "Byte" => LLType::I8,
                "Int" => LLType::I64,
                "Real" => LLType::F64,
                "String" => LLType::Ptr(r(LLType::I8)),
                "Buffer" => LLType::Ptr(r(LLType::Ptr(r(LLType::I8)))),
                t if t == EXCEPTION_POINT_NAME => LLType::Ptr(r(LLType::ExceptionPoint)),
                _ => self.types.borrow().get(id).unwrap().clone(),
            },
            Type::Ref(ttype) => LLType::Ptr(r(self.transform_value_type(ttype))),
            Type::Tuple(items) => LLType::Struct(items.iter().map(|item| self.transform_value_type(&item)).collect()),
            Type::Record(items) => LLType::Struct(items.iter().map(|item| self.transform_value_type(&item.1)).collect()),
            // TODO how will you do generics
            //Type::Variable(name, id, _) => { panic!("") },
            //Type::Variable(name, id, _) => LLType::Ptr(r(LLType::I8)),
            Type::Variable(_, _, _) => LLType::Var,
            Type::Function(args, ret, abi) => {
                match abi {
                    ABI::C | ABI::MoltenFunc => LLType::Ptr(r(self.transform_cfunc_def_type(&args.as_vec(), &*ret))),
                    ABI::Molten | ABI::Unknown => self.transform_closure_value_type(&args.as_vec(), &*ret),
                    _ => panic!("Not Implemented: {:?}", abi),
                }
            },
            _ => panic!("Not Implemented: {:?}", ttype),
        }
    }

    fn transform_struct_def(&self, structdef: &StructDefRef) -> LLType {
        let mut items = vec!();
        structdef.foreach_field(|_, _, ttype| {
            items.push(self.transform_value_type(ttype));
        });
        LLType::Struct(items)
    }
}


