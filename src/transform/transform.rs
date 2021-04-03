
use std::collections::HashMap;


use binding;
use typecheck;

use abi::ABI;
use defs::Def;
use types::Type;
use config::Options;
use session::Session;
use scope::{ Scope, ScopeRef };
use ast::{ Pos };
use hir::{ NodeID, Visibility, Mutability, Literal, Ident, Argument, MatchCase, Pattern, PatKind, Expr, ExprKind };

use defs::functions::{ FuncDef, ClosureDefRef };
use defs::classes::{ ClassDefRef, StructDefRef, Define, Vtable };

use misc::{ r };
use transform::llcode::{ LLType, LLLit, LLRef, LLCmpType, LLLink, LLCC, LLExpr, LLGlobal };
use transform::functions::{ CFuncTransform, MFuncTransform, ClosureTransform };

use visitor::{ Visitor, ScopeStack };


pub static EXCEPTION_POINT_NAME: &str = "__ExceptionPoint__";

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CodeContext {
    Func(ABI, NodeID),
    ClassBody,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Transformer<'sess> {
    pub session: &'sess Session,
    pub stack: ScopeStack,
    pub context: Vec<CodeContext>,
    pub globals: Vec<LLGlobal>,
    pub expoints: Vec<NodeID>,
    pub types: HashMap<NodeID, LLType>,
}

impl<'sess> Transformer<'sess> {
    pub fn new(session: &'sess Session) -> Self {
        Transformer {
            session: session,
            stack: ScopeStack::new(),
            context: vec!(),
            globals: vec!(),
            expoints: vec!(),
            types: HashMap::new(),
        }
    }

    pub fn initialize(&mut self) {
        let global = self.session.map.get_global();

        self.set_type(global.get_type_def(&"()".to_string()).unwrap(), LLType::I32);
        self.set_type(global.get_type_def(&"Nil".to_string()).unwrap(), LLType::Ptr(r(LLType::I8)));
        self.set_type(global.get_type_def(&"Bool".to_string()).unwrap(), LLType::I1);
        self.set_type(global.get_type_def(&"Byte".to_string()).unwrap(), LLType::I8);
        self.set_type(global.get_type_def(&"Char".to_string()).unwrap(), LLType::I32);
        self.set_type(global.get_type_def(&"Int".to_string()).unwrap(), LLType::I64);
        self.set_type(global.get_type_def(&"Real".to_string()).unwrap(), LLType::F64);
        self.set_type(global.get_type_def(&"String".to_string()).unwrap(), LLType::Ptr(r(LLType::I8)));
        self.set_type(global.get_type_def(&"Buffer".to_string()).unwrap(), LLType::Ptr(r(LLType::Ptr(r(LLType::I8)))));

        let expoint_id = NodeID::generate();
        global.define_type(String::from(EXCEPTION_POINT_NAME), Some(expoint_id)).unwrap();
        self.session.set_type(expoint_id, Type::Object(String::from(EXCEPTION_POINT_NAME), expoint_id, vec!()));
        self.set_type(expoint_id, LLType::Ptr(r(LLType::ExceptionPoint)));
    }

    pub fn set_type(&mut self, id: NodeID, ltype: LLType) {
        self.types.insert(id, ltype);
    }

    pub fn get_type(&self, id: NodeID) -> Option<LLType> {
        self.types.get(&id).map(|ltype| ltype.clone())
    }

    pub fn add_global(&mut self, global: LLGlobal) {
        self.globals.push(global);
    }

    pub fn insert_global(&mut self, index: usize, global: LLGlobal) {
        self.globals.insert(index, global);
    }

    pub fn set_context(&mut self, context: CodeContext) {
        self.context.push(context);
    }

    pub fn restore_context(&mut self) {
        self.context.pop();
    }

    pub fn get_context(&self) -> Option<CodeContext> {
        self.context.last().map(|c| *c)
    }

    /*
    fn get_func_context(&self) -> Option<CodeContext> {
        for i in (self.context.len() - 1) .. 0 {
            match self.context[i] {
                context @ CodeContext::Func(_, _) => return Some(context),
                _ => { },
            }
        }
        return None
    }
    */

    pub fn with_context<F, R>(&mut self, context: CodeContext, f: F) -> R where F: FnOnce(&mut Self) -> R {
        self.set_context(context);
        let ret = f(self);
        self.restore_context();
        ret
    }

    pub fn with_exception<F, R>(&mut self, expoint: NodeID, f: F) -> R where F: FnOnce(&mut Self) -> R {
        self.expoints.push(expoint);
        let ret = f(self);
        self.expoints.pop();
        ret
    }

    pub fn with_scope<F, R>(&mut self, scope: ScopeRef, f: F) -> R where F: FnOnce(&mut Self) -> R {
        self.stack.push_scope(scope);
        let ret = f(self);
        self.stack.pop_scope();
        ret
    }

    pub fn get_exception(&self) -> Option<NodeID> {
        self.expoints.last().map(|e| *e)
    }


    pub fn transform_code(&mut self, scope: ScopeRef, code: &Vec<Expr>) {
        self.stack.push_scope(scope.clone());
        let run_id = self.build_run_func(code);

        if !Options::as_ref().is_library {
            self.build_main_func(run_id);
        }
    }
}




impl<'sess> Transformer<'sess> {
    pub fn build_run_func(&mut self, code: &Vec<Expr>) -> NodeID {
        // Define a global that will store whether we've run this function or not
        let module_memo_id = NodeID::generate();
        self.add_global(LLGlobal::DefGlobal(module_memo_id, LLLink::Public, format!("memo.{}", self.session.name), LLType::I1));

        let run_id = NodeID::generate();
        let run_ltype = MFuncTransform::convert_to_def_type(self, LLType::Function(vec!(), r(LLType::I64)));
        self.set_type(run_id, run_ltype.clone());

        let mut fargs = vec!();
        let exp_id = NodeID::generate();
        MFuncTransform::convert_def_args(self, exp_id, &mut fargs);

        // Set the memo, execute the module's top level scope, and then return 0
        let mut body = vec!();
        body.push(LLExpr::SetGlobal(module_memo_id, r(LLExpr::Literal(LLLit::I1(true)))));
        body.extend(
            self.with_context(CodeContext::Func(ABI::MoltenFunc, run_id), |transform| {
                transform.with_exception(exp_id, |transform| {
                    transform.transform_vec(&code)
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

    pub fn build_main_func(&mut self, run_id: NodeID) {
        let main_id = NodeID::generate();
        let main_ltype = LLType::Function(vec!(), r(LLType::I64));
        let mut main_body = vec!();

        // Initialize the garbage collector
        main_body.push(LLExpr::CallC(r(LLExpr::GetNamed("molten_init".to_string())), vec!(), LLCC::CCC));

        let exp_id = NodeID::generate();
        let expoint = self.create_exception_point(&mut main_body, exp_id);

        // Try calling the module's run function
        let try = self.with_exception(exp_id, |transform| {
            MFuncTransform::create_invoke(transform, LLExpr::GetValue(run_id), vec!())
        });

        // If an exception occurs, print a message and exit with -1
        let catch = vec!(
            LLExpr::CallC(r(LLExpr::GetNamed("puts".to_string())), vec!(LLExpr::Literal(LLLit::ConstStr(String::from("Uncaught exception.  Terminating")))), LLCC::CCC),
            LLExpr::Literal(LLLit::I64(-1))
        );

        main_body.extend(self.create_exception_block(expoint, try, catch));

        self.add_global(LLGlobal::DefCFunc(main_id, LLLink::Public, String::from("main"), main_ltype, vec!(), main_body, LLCC::CCC));
    }

    pub fn transform_vec(&mut self, code: &Vec<Expr>) -> Vec<LLExpr> {
        let mut exprs = vec!();
        for expr in code {
            exprs.extend(self.transform_node(expr));
        }
        exprs
    }

    pub fn transform_as_result(&mut self, exprs: &mut Vec<LLExpr>, node: &Expr) -> Option<LLExpr> {
        let mut newexprs = self.transform_node(node);
        let last = newexprs.pop();
        exprs.extend(newexprs);
        last
    }

    pub fn transform_as_args(&mut self, exprs: &mut Vec<LLExpr>, args: &Vec<Expr>) -> Vec<LLExpr> {
        let mut fargs = vec!();
        for arg in args {
            fargs.push(self.transform_as_result(exprs, arg).unwrap());
        }
        fargs
    }

    pub fn transform_node(&mut self, node: &Expr) -> Vec<LLExpr> {
        match &node.kind {
            ExprKind::Literal(lit) => vec!(LLExpr::Literal(self.transform_lit(lit))),

            ExprKind::Nil => vec!(LLExpr::Literal(LLLit::Null(self.transform_value_type(&self.session.get_type(node.id).unwrap())))),

            ExprKind::Block(code) => self.transform_vec(code),

            ExprKind::Import(ident, decls) => {
                self.transform_import(&ident.name, decls)
            },

            ExprKind::Try(code, cases) => {
                self.transform_try(code, cases)
            },

            ExprKind::Raise(valexpr) => {
                self.transform_raise(node.id, valexpr)
            },


            ExprKind::Function(vis, ident, args, _, body, abi) => {
                self.transform_func_def(*abi, node.id, *vis, ident.as_ref().map(|ident| &ident.name), args, body)
            },

            ExprKind::Declare(vis, ident, _) => {
                let ttype = self.session.get_type_from_ref(node.id).unwrap();
                let abi = ttype.get_abi().unwrap();
                self.transform_func_decl(abi, node.id, *vis, &ident.name, &ttype)
            },

            ExprKind::Invoke(func, args, fid) => {
                let abi = self.session.get_type(*fid).unwrap().get_abi().unwrap();
                self.transform_func_invoke(abi, node.id, func, args)
            },

            ExprKind::Definition(_, ident, _, value) => {
                self.transform_def_local(node.id, &ident.name, value)
            },

            ExprKind::Identifier(ident) => {
                let defid = self.session.get_ref(node.id).unwrap();
                self.transform_reference(defid, &ident.name)
            },


            ExprKind::Ref(value) => {
                self.transform_alloc_ref(node.id, value)
            },

            ExprKind::Deref(value) => {
                self.transform_deref_ref(value)
            },

            ExprKind::Record(items) => {
                let mut nitems = vec!();
                for item in items {
                    nitems.push(item.1.clone());
                }
                self.transform_tuple_lit(node.id, &nitems)
            },

            ExprKind::Tuple(items) => {
                self.transform_tuple_lit(node.id, &items)
            },

            ExprKind::RecordUpdate(record, items) => {
                self.transform_record_update(node.id, &record, &items)
            },

            ExprKind::Enum(classspec, _) => {
                self.transform_enum_def(node.id, &classspec.ident.name)
            },


            ExprKind::New(_) => {
                self.transform_new_object(node.id)
            },

            ExprKind::Class(_, _, body) => {
                let mut exprs = self.transform_class_body(node.id, body);
                exprs.push(LLExpr::Literal(self.transform_lit(&Literal::Unit)));
                exprs
            },

            ExprKind::PtrCast(_, subnode) => {
                let mut exprs = vec!();
                let ltype = self.transform_value_type(&self.session.get_type(node.id).unwrap());
                let result = self.transform_as_result(&mut exprs, subnode).unwrap();
                exprs.push(LLExpr::Cast(ltype, r(result)));
                exprs
            },

            ExprKind::Accessor(obj, ident, oid) => {
                let otype = self.session.get_type(*oid).unwrap();
                self.transform_accessor(node.id, obj, &ident.name, otype)
            },

            ExprKind::Resolver(path, field, oid) => {
                let otype = self.session.get_type_from_ref(*oid).unwrap();
                self.transform_resolve(node.id, path, &field.name, otype)
            },

            ExprKind::Assignment(left, right, _) => {
                self.transform_assignment(node.id, left, right)
            },


            ExprKind::If(cond, texpr, fexpr) => {
                self.transform_if_expr(cond, texpr, fexpr)
            },

            ExprKind::Match(cond, cases) => {
                self.transform_match(cond, cases)
            },

            ExprKind::SideEffect(ident, args) => {
                self.transform_side_effect(ident.name.as_str(), args)
            },


            ExprKind::While(cond, body) => {
                vec!(LLExpr::Loop(self.transform_node(cond), self.transform_node(body)))
            },


            ExprKind::TypeAlias(_, _) => { /* Nothing Needs To Be Done */ vec!() }
        }
    }

    pub fn transform_lit(&mut self, lit: &Literal) -> LLLit {
        match lit {
            Literal::Unit => LLLit::I32(0),
            Literal::Boolean(num) => LLLit::I1(*num as bool),
            Literal::Character(num) => LLLit::I32(*num as i32),
            Literal::Integer(num) => LLLit::I64(*num as i64),
            Literal::Real(num) => LLLit::F64(*num),
            // TODO not sure how you'll do strings yet.... maybe it shouldn't even be a literal here
            Literal::String(string) => LLLit::ConstStr(string.clone()),
        }
    }

    pub fn transform_tuple_lit(&mut self, id: NodeID, items: &Vec<Expr>) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let mut litems = vec!();
        for item in items {
            litems.push(self.transform_as_result(&mut exprs, item).unwrap());
        }
        let ltype = self.transform_value_type(&self.session.get_type(id).unwrap());
        exprs.push(LLExpr::DefStruct(id, ltype, litems));
        exprs
    }

    pub fn transform_record_update(&mut self, id: NodeID, record: &Expr, items: &Vec<(Ident, Expr)>) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let mut litems = vec!();

        let ttype = &self.session.get_type(id).unwrap();
        let valexpr = self.transform_as_result(&mut exprs, record).unwrap();

        for (ref name, _) in ttype.get_record_types().unwrap() {
            let item = match items.iter().find(|(ident, _)| &ident.name == name) {
                Some((_, expr)) => self.transform_as_result(&mut exprs, expr).unwrap(),
                None => LLExpr::GetItem(r(valexpr.clone()), litems.len()),
            };
            litems.push(item);
        }
        exprs.push(LLExpr::DefStruct(id, self.transform_value_type(ttype), litems));
        exprs
    }

    pub fn transform_enum_def(&mut self, id: NodeID, name: &String) -> Vec<LLExpr> {
        let defid = self.session.get_ref(id).unwrap();
        let selector = LLType::I8;
        let enumdef = self.session.get_def(defid).unwrap().as_enum().unwrap();

        self.add_global(LLGlobal::DefNamedStruct(defid, name.clone(), false));
        self.set_type(defid, LLType::Alias(defid));

        let mut types = vec!();
        for (i, variant) in enumdef.variants.borrow().iter().enumerate() {
            let name = format!("{}_{}", name, variant.ident.name);
            self.transform_enum_variant(variant.id, i as i8, name, selector.clone(), variant.ttype.clone());
            if variant.ttype.is_some() {
                types.push(self.transform_value_type(variant.ttype.as_ref().unwrap()));
            }
        }

        self.add_global(LLGlobal::SetStructBody(defid, vec!(selector.clone(), LLType::Largest(types)), false));
        vec!()
    }

    pub fn transform_enum_variant(&mut self, id: NodeID, variant: i8, name: String, selector: LLType, ttype: Option<Type>) {
        let struct_id = NodeID::generate();
        let etype = ttype.clone().map(|t| self.transform_value_type(&t));
        self.create_enum_struct(struct_id, name.clone(), selector, etype);
        self.set_type(id, LLType::Alias(struct_id));

        if ttype.is_some() {
            let ftype = self.session.get_type(id).unwrap();
            let (argtypes, rettype, _) = ftype.get_function_types().unwrap();
            let lftype = CFuncTransform::transform_def_type(self, &argtypes.as_vec(), rettype);

            let mut params = vec!();
            let mut tuple_items = vec!();
            for (i, _) in argtypes.as_vec().iter().enumerate() {
                let arg_id = NodeID::generate();
                tuple_items.push(LLExpr::GetValue(arg_id));
                params.push((arg_id, format!("value{}", i)));
            }

            let body = vec!(LLExpr::DefStruct(NodeID::generate(), self.get_type(struct_id).unwrap(), vec!(
                LLExpr::Literal(LLLit::I8(variant as i8)),
                LLExpr::DefStruct(NodeID::generate(), self.transform_value_type(argtypes), tuple_items)
            )));
            self.add_global(LLGlobal::DefCFunc(id, LLLink::Once, name, lftype, params, body, LLCC::CCC));
        }
    }

    pub fn create_enum_struct(&mut self, id: NodeID, name: String, selector: LLType, ltype: Option<LLType>) {
        let mut body = vec!(selector);
        match ltype {
            Some(ltype) => body.push(ltype),
            None => { },
        }

        self.add_global(LLGlobal::DefNamedStruct(id, name.clone(), false));
        self.add_global(LLGlobal::SetStructBody(id, body, false));
        self.set_type(id, LLType::Alias(id));
    }

    pub fn create_def_local(&mut self, id: NodeID, name: &String, valexpr: LLExpr) -> Vec<LLExpr> {
        let ltype = self.transform_value_type(&self.session.get_type(id).unwrap());
        vec!(LLExpr::DefLocal(id, name.clone(), ltype, r(valexpr)))
    }

    pub fn transform_def_local(&mut self, id: NodeID, name: &String, value: &Expr) -> Vec<LLExpr> {
        let defid = self.session.get_ref(id).unwrap();
        let mut exprs = vec!();
        let valexpr = self.transform_as_result(&mut exprs, value).unwrap();
        exprs.extend(self.create_def_local(defid, name, valexpr));
        exprs
    }

    pub fn transform_import(&mut self, name: &String, decls: &Vec<Expr>) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let rid = NodeID::generate();
        let module_run_name = format!("run_{}", name.replace(".", "_"));
        let rftype = LLType::Function(vec!(), r(LLType::I64));
        self.add_global(LLGlobal::DeclCFunc(rid, module_run_name, rftype, LLCC::FastCC));
        exprs.extend(CFuncTransform::create_invoke(self, LLExpr::GetValue(rid), vec!()));
        exprs.extend(self.transform_vec(decls));
        exprs
    }

    pub fn transform_try(&mut self, code: &Expr, cases: &Vec<MatchCase>) -> Vec<LLExpr> {
        let mut exprs = vec!();

        let exp_id = NodeID::generate();
        let expoint = self.create_exception_point(&mut exprs, exp_id);
        let expoint_id = NodeID::generate();
        exprs.push(LLExpr::SetValue(expoint_id, r(expoint)));

        let tryblock = self.with_exception(exp_id, |transform| {
            transform.transform_node(code)
        });


        let exret_id = NodeID::generate();
        exprs.push(LLExpr::SetValue(exret_id, r(LLExpr::GetItem(r(LLExpr::GetLocal(exp_id)), 1))));
        let matchblock = self.create_match(LLExpr::GetValue(exret_id), cases);

        exprs.extend(self.create_exception_block(LLExpr::GetValue(expoint_id), tryblock, matchblock));
        exprs
    }

    pub fn create_exception_point(&mut self, exprs: &mut Vec<LLExpr>, exp_id: NodeID) -> LLExpr {
        exprs.push(LLExpr::DefLocal(exp_id, String::from("__exception__"), LLType::ExceptionPoint, r(LLExpr::Literal(LLLit::Null(LLType::ExceptionPoint)))));

        let ret_id = NodeID::generate();
        exprs.push(LLExpr::SetValue(ret_id, r(LLExpr::CallC(r(LLExpr::GetNamed("setjmp".to_string())), vec!(LLExpr::GetValue(exp_id)), LLCC::CCC))));
        LLExpr::GetValue(ret_id)
    }

    pub fn create_exception_block(&mut self, expoint: LLExpr, try: Vec<LLExpr>, catch: Vec<LLExpr>) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let iszero = LLExpr::Cmp(LLCmpType::Equal, r(expoint), r(LLExpr::Literal(LLLit::I32(0))));
        exprs.push(LLExpr::Phi(vec!(vec!(iszero), vec!(LLExpr::Literal(LLLit::I1(true)))), vec!(try, catch)));
        exprs
    }

    pub fn transform_raise(&mut self, _id: NodeID, valexpr: &Expr) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let exp_id = self.get_exception().unwrap();
        let value = self.transform_as_result(&mut exprs, valexpr).unwrap();

        exprs.push(LLExpr::StoreRef(r(LLExpr::AccessRef(r(LLExpr::GetValue(exp_id)), vec!(LLRef::Field(1)))), r(LLExpr::Cast(LLType::Var, r(value)))));
        exprs.push(LLExpr::CallC(r(LLExpr::GetNamed("longjmp".to_string())), vec!(LLExpr::GetValue(exp_id), LLExpr::Literal(LLLit::I32(1))), LLCC::CCC));

        exprs.push(LLExpr::Literal(LLLit::I32(0)));
        exprs
    }



    pub fn create_reference(&mut self, defid: NodeID) -> Vec<LLExpr> {
        match self.session.get_def(defid) {
            Ok(Def::Var(_)) => vec!(LLExpr::GetLocal(defid)),
            Ok(_) => vec!(LLExpr::GetValue(defid)),
            Err(_) => panic!("TransformError: attempting to reference a non-existent value"),
        }
    }

    pub fn transform_reference(&mut self, defid: NodeID, name: &String) -> Vec<LLExpr> {
        let scope = self.stack.get_scope();
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



    pub fn transform_alloc_ref(&mut self, id: NodeID, value: &Expr) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let valexpr = self.transform_as_result(&mut exprs, value).unwrap();
        let ltype = self.transform_value_type(&self.session.get_type(id).unwrap());
        exprs.push(LLExpr::AllocRef(id, ltype, Some(r(valexpr))));
        exprs
    }

    pub fn transform_deref_ref(&mut self, value: &Expr) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let valexpr = self.transform_as_result(&mut exprs, value).unwrap();
        exprs.push(LLExpr::LoadRef(r(valexpr)));
        exprs
    }

    pub fn transform_new_object(&mut self, id: NodeID) -> Vec<LLExpr> {
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

    pub fn transform_class_type_data(&mut self, classdef: ClassDefRef, body: &Vec<Expr>) {
        classdef.build_vtable(self.session, body);
        classdef.build_structdef(self.session, body);

        self.add_global(LLGlobal::DefNamedStruct(classdef.id, classdef.classname.clone(), true));
        self.set_type(classdef.id, LLType::Alias(classdef.id));
        self.add_global(LLGlobal::DefNamedStruct(classdef.vtable.id, format!("{}_vtable", classdef.classname.clone()), true));
        self.set_type(classdef.vtable.id, LLType::Alias(classdef.vtable.id));

        let stype = self.transform_struct_def(&classdef.structdef);
        self.add_global(LLGlobal::SetStructBody(classdef.id, stype.get_items(), true));

        let vtype = self.transform_vtable_def(&classdef.vtable);
        self.add_global(LLGlobal::SetStructBody(classdef.vtable.id, vtype.get_items(), true));
    }

    pub fn transform_vtable_def(&mut self, vtable: &Vtable) -> LLType {
        let mut items = vec!();
        vtable.foreach_entry(|_, _, ttype| {
            items.push(self.transform_value_type(ttype));
        });
        LLType::Struct(items)
    }

    pub fn transform_class_vtable_init(&mut self, classdef: ClassDefRef) -> Vec<LLExpr> {
        if !classdef.has_vtable() {
            return vec!();
        }

        let tscope = self.session.map.get(&classdef.id);
        self.transform_vtable_init(format!("__{}_vtable", tscope.get_basename()), &classdef.vtable)
    }

    pub fn transform_vtable_init(&mut self, name: String, vtable: &Vtable) -> Vec<LLExpr> {
        let mut exprs = vec!();

        self.add_global(LLGlobal::DefGlobal(vtable.id, LLLink::Once, name, self.get_type(vtable.id).unwrap()));
        // TODO should vtables be dynamically allocated, or should we add a LLType::ElementOf() type or something to GetElement an aliased type
        exprs.push(LLExpr::SetGlobal(vtable.id, r(LLExpr::AllocRef(NodeID::generate(), self.get_type(vtable.id).unwrap(), None))));
        vtable.foreach_enumerated(|i, id, _, ttype| {
            let ltype = self.transform_value_type(ttype);
            let field = LLExpr::AccessRef(r(LLExpr::GetGlobal(vtable.id)), vec!(LLRef::Field(i)));
            exprs.push(LLExpr::StoreRef(r(field), r(LLExpr::Cast(ltype, r(LLExpr::GetValue(id))))));
            //exprs.push(LLExpr::SetItem(r(LLExpr::GetLocal(vtable.id)), i, r(LLExpr::Cast(ltype, r(LLExpr::GetValue(id))))));
        });

        exprs
    }

    pub fn transform_class_body(&mut self, id: NodeID, body: &Vec<Expr>) -> Vec<LLExpr> {
        let defid = self.session.get_ref(id).unwrap();
        let mut exprs = vec!();
        let tscope = self.session.map.get(&defid);
        let classdef = self.session.get_def(defid).unwrap().as_class().unwrap();

        self.transform_class_type_data(classdef.clone(), body);

        self.with_scope(tscope, |transform| {
            for node in body {
                match &node.kind {
                    ExprKind::Function(vis, ident, args, _, body, abi) => {
                        // TODO i switched to using scope here instead of tscope because it was causing problems with references inside closures
                        exprs.extend(transform.transform_func_def(*abi, node.id, *vis, ident.as_ref().map(|ident| &ident.name), args, body));
                    },
                    ExprKind::Declare(vis, ident, _) => {
                        let ttype = transform.session.get_type_from_ref(node.id).unwrap();
                        exprs.extend(transform.transform_func_decl(ttype.get_abi().unwrap(), node.id, *vis, &ident.name, &ttype));
                    },
                    ExprKind::Definition(_, _, _, _) => { },
                    _ => panic!("Not Implemented: {:?}", node),
                }
            }
        });

        exprs.extend(self.transform_class_vtable_init(classdef));
        exprs
    }

    pub fn transform_accessor(&mut self, id: NodeID, obj: &Expr, field: &String, otype: Type) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let defid = self.session.get_ref(id).unwrap();
        let objval = self.transform_as_result(&mut exprs, obj).unwrap();
        exprs.extend(self.convert_accessor(defid, objval, field, otype));
        exprs
    }

    pub fn convert_accessor(&mut self, defid: NodeID, objval: LLExpr, field: &String, otype: Type) -> Vec<LLExpr> {
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

    pub fn transform_resolve(&mut self, id: NodeID, _path: &Expr, _field: &String, otype: Type) -> Vec<LLExpr> {
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

    pub fn transform_assignment(&mut self, _id: NodeID, left: &Expr, right: &Expr) -> Vec<LLExpr> {
        let mut exprs = vec!();

        let value = self.transform_as_result(&mut exprs, right).unwrap();
        match &left.kind {
            ExprKind::Accessor(obj, _, oid) => {
                let objval = self.transform_as_result(&mut exprs, obj).unwrap();
                let fieldid = self.session.get_ref(left.id).unwrap();
                let objdef = self.session.get_def(self.session.get_type(*oid).unwrap().get_id().unwrap()).unwrap();
                let (index, _) = objdef.as_struct().unwrap().find_field_by_id(fieldid).unwrap();
                exprs.push(LLExpr::StoreRef(r(LLExpr::AccessRef(r(objval), vec!(LLRef::Field(index)))), r(value)));
            },
            ExprKind::Identifier(_) => {
                let defid = self.session.get_ref(left.id).unwrap();
                exprs.push(LLExpr::StoreRef(r(LLExpr::GetValue(defid)), r(value)));
            },
            ExprKind::Deref(node) => {
                let result = self.transform_as_result(&mut exprs, node).unwrap();
                exprs.push(LLExpr::StoreRef(r(result), r(value)));
            },
            _ => panic!("InternalError: attempting to assign to an invalid pattern, {:?}", left),
        }
        exprs
    }



    pub fn transform_if_expr(&mut self, cond: &Expr, texpr: &Expr, fexpr: &Expr) -> Vec<LLExpr> {
        let mut conds = vec!();
        conds.push(self.transform_node(cond));
        conds.push(vec!(LLExpr::Literal(LLLit::I1(true))));

        let mut blocks = vec!();
        blocks.push(self.transform_node(texpr));
        blocks.push(self.transform_node(fexpr));

        vec!(LLExpr::Phi(conds, blocks))
    }

    pub fn create_match(&mut self, condval: LLExpr, cases: &Vec<MatchCase>) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let mut conds = vec!();
        let mut blocks = vec!();

        let condid = NodeID::generate();
        exprs.push(LLExpr::SetValue(condid, r(condval)));

        for case in cases {
            let lscope = self.session.map.get(&case.id);
            self.with_scope(lscope, |transform| {
                conds.push(transform.transform_pattern(&case.pat, condid));
                blocks.push(transform.transform_node(&case.body));
            });
        }

        exprs.push(LLExpr::Phi(conds, blocks));
        exprs
    }

    pub fn transform_match(&mut self, cond: &Expr, cases: &Vec<MatchCase>) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let condval = self.transform_as_result(&mut exprs, cond).unwrap();
        exprs.extend(self.create_match(condval, cases));
        exprs
    }

    pub fn transform_pattern(&mut self, pat: &Pattern, value_id: NodeID) -> Vec<LLExpr> {
        let mut exprs = vec!();

        match &pat.kind {
            PatKind::Wild => exprs.push(LLExpr::Literal(LLLit::I1(true))),
            PatKind::Literal(lit) => {
                let compfunc = self.transform_as_result(&mut exprs, &Expr::new_with_id(pat.id, Pos::empty(), ExprKind::Identifier(Ident::from_str("==")))).unwrap();
                let compabi = self.session.get_type(pat.id).unwrap().get_abi().unwrap();
                let result = LLExpr::Literal(self.transform_lit(lit));
                exprs.extend(self.create_func_invoke(compabi, compfunc, vec!(LLExpr::GetValue(value_id), result)));
            },
            PatKind::Binding(ident) => {
                let defid = self.session.get_ref(pat.id).unwrap();
                exprs.extend(self.create_def_local(defid, &ident.name, LLExpr::GetValue(value_id)));
                exprs.push(LLExpr::Literal(LLLit::I1(true)));
            },
            PatKind::Annotation(_, subpat) => {
                let ttype = self.session.get_type(pat.id).unwrap();
                exprs.push(LLExpr::SetValue(pat.id, r(LLExpr::Cast(self.transform_value_type(&ttype), r(LLExpr::GetValue(value_id))))));
                exprs.extend(self.transform_pattern(subpat, pat.id));
            },
            PatKind::Resolve(_left, _field, oid) => {
                let defid = self.session.get_ref(pat.id).unwrap();
                let enumdef = self.session.get_def_from_ref(*oid).unwrap().as_enum().unwrap();
                let variant = enumdef.get_variant_by_id(defid).unwrap();
                exprs.push(LLExpr::Cmp(LLCmpType::Equal, r(LLExpr::GetItem(r(LLExpr::GetValue(value_id)), 0)), r(LLExpr::Literal(LLLit::I8(variant as i8)))));
            },
            PatKind::EnumArgs(left, args) => {
                let variant_id = self.session.get_ref(pat.id).unwrap();
                let item_id = NodeID::generate();
                exprs.push(LLExpr::SetValue(item_id, r(LLExpr::GetItem(r(LLExpr::Cast(self.get_type(variant_id).unwrap(), r(LLExpr::GetValue(value_id)))), 1))));
                for (i, arg) in args.iter().enumerate() {
                    let arg_id = NodeID::generate();
                    exprs.push(LLExpr::SetValue(arg_id, r(LLExpr::GetItem(r(LLExpr::GetValue(item_id)), i))));
                    exprs.extend(self.transform_pattern(&arg, arg_id));
                }
                //let result = exprs.pop();
                exprs.extend(self.transform_pattern(left, value_id));
            },
            _ => panic!("Not Implemented: {:?}", pat),
        }
        exprs
    }

    /*
    fn find_func(&mut self, name: &str, argtypes: &Vec<Type>) -> NodeID {
        let compid = scope.get_var_def(&String::from(name)).unwrap();
        match self.session.get_def(compid) {
            Ok(Def::Overload(ol)) => ol.find_variant(self.session, Type::Tuple(argtypes.clone())).unwrap().0,
            _ => compid,
        }
    }
    */

    pub fn transform_side_effect(&mut self, op: &str, args: &Vec<Expr>) -> Vec<LLExpr> {
        let mut conds = vec!();
        let mut blocks = vec!();

        // TODO this doesn't work with non-boolean values
        match op {
            "and" => {
                conds.push(self.transform_node(&args[0]));
                blocks.push(self.transform_node(&args[1]));
                vec!(LLExpr::Phi(conds, blocks))
            },
            "or" => {
                conds.push(self.transform_node(&args[0]));
                blocks.push(vec!(LLExpr::Literal(LLLit::I1(true))));
                conds.push(vec!(LLExpr::Literal(LLLit::I1(true))));
                blocks.push(self.transform_node(&args[1]));
                vec!(LLExpr::Phi(conds, blocks))
            },
            _ => panic!("Not Implemented: {:?}", op),
        }
    }



    pub fn transform_value_type(&mut self, ttype: &Type) -> LLType {
        match &ttype {
            Type::Object(_, id, _) => self.get_type(*id).unwrap(),
            Type::Ref(ttype) => LLType::Ptr(r(self.transform_value_type(ttype))),
            Type::Tuple(items) => LLType::Struct(items.iter().map(|item| self.transform_value_type(&item)).collect()),
            Type::Record(items) => LLType::Struct(items.iter().map(|item| self.transform_value_type(&item.1)).collect()),
            // TODO how will you do generics
            Type::Variable(_, _, _) => LLType::Var,
            Type::Function(args, ret, abi) => {
                match abi {
                    ABI::C | ABI::MoltenFunc => LLType::Ptr(r(CFuncTransform::transform_def_type(self, &args.as_vec(), &*ret))),
                    ABI::Molten | ABI::Unknown => ClosureTransform::transform_value_type(self, &args.as_vec(), &*ret),
                    _ => panic!("Not Implemented: {:?}", abi),
                }
            },
            _ => panic!("Not Implemented: {:?}", ttype),
        }
    }

    pub fn transform_struct_def(&mut self, structdef: &StructDefRef) -> LLType {
        let mut items = vec!();
        structdef.foreach_field(|_, _, ttype| {
            items.push(self.transform_value_type(ttype));
        });
        LLType::Struct(items)
    }
}


