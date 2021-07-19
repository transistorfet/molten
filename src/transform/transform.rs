
use std::collections::HashMap;

use abi::ABI;
use defs::Def;
use types::Type;
use config::Options;
use scope::{ Scope, ScopeRef };
use session::{ Session, Error };
use ast::{ Pos };
use hir::{ NodeID, Visibility, Mutability, AssignType, Literal, Ident, Argument, ClassSpec, MatchCase, EnumVariant, Pattern, PatKind, Expr, ExprKind };

use misc::{ r };
use transform::llcode::{ LLType, LLLit, LLRef, LLCmpType, LLLink, LLCC, LLExpr, LLGlobal };
use transform::functions::{ ClosureTransform };

use visitor::{ Visitor, ScopeStack };


#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CodeContext {
    Func(ABI, NodeID),
    Import,
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

        self.initialize_exception_type();
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

    /*
    pub fn with_scope<F, R>(&mut self, scope: ScopeRef, f: F) -> R where F: FnOnce(&mut Self) -> R {
        self.stack.push_scope(scope);
        let ret = f(self);
        self.stack.pop_scope();
        ret
    }
    */

    pub fn get_exception(&self) -> Option<NodeID> {
        self.expoints.last().map(|e| *e)
    }

    pub fn get_global_exception(&self) -> Option<NodeID> {
        self.expoints.first().map(|e| *e)
    }

    pub fn transform_code(&mut self, scope: ScopeRef, code: &Vec<Expr>) {
        self.stack.push_scope(scope.clone());

        let exp_id = NodeID::generate();
        self.declare_global_exception_point(exp_id);
        self.expoints.push(exp_id);

        let init_code = self.visit_vec(code).unwrap();
        if !Options::as_ref().is_library {
            self.build_main_func(init_code);
        }
    }
}



impl<'sess> Visitor for Transformer<'sess> {
    type Return = Vec<LLExpr>;

    fn default_return(&self) -> Self::Return {
        vec!()
    }

    fn get_scope_stack<'a>(&'a self) -> &'a ScopeStack {
        &self.stack
    }

    fn get_scope_by_id(&self, id: NodeID) -> ScopeRef {
        self.session.map.get(&id)
    }

    fn handle_error(&mut self, node: &Expr, err: Error) -> Result<Self::Return, Error> {
        self.session.print_error(&err.add_pos(&node.get_pos()));
        Ok(vec!())
    }


    // TODO this function is necessary to combine the outputs from each node visit, but if we instead push exprs directly onto the top body, we wont need this
    fn visit_vec(&mut self, code: &Vec<Expr>) -> Result<Self::Return, Error> {
        let mut exprs = vec!();
        for expr in code {
            exprs.extend(self.visit_node(expr).unwrap());
        }
        Ok(exprs)
    }


    fn visit_literal(&mut self, _id: NodeID, lit: &Literal) -> Result<Self::Return, Error> {
        Ok(vec!(LLExpr::Literal(self.transform_lit(lit))))
    }

    fn visit_nil(&mut self, id: NodeID) -> Result<Self::Return, Error> {
        Ok(vec!(LLExpr::Literal(LLLit::Null(self.transform_value_type(&self.session.get_type(id).unwrap())))))
    }

    fn visit_ptr_cast(&mut self, id: NodeID, _ttype: &Type, code: &Expr) -> Result<Self::Return, Error> {
        let mut exprs = vec!();
        let ltype = self.transform_value_type(&self.session.get_type(id).unwrap());
        let result = self.transform_as_result(&mut exprs, code).unwrap();
        exprs.push(LLExpr::Cast(ltype, r(result)));
        Ok(exprs)
    }

    fn visit_ref(&mut self, id: NodeID, expr: &Expr) -> Result<Self::Return, Error> {
        Ok(self.transform_alloc_ref(id, expr))
    }

    fn visit_deref(&mut self, _id: NodeID, expr: &Expr) -> Result<Self::Return, Error> {
        Ok(self.transform_deref_ref(expr))
    }

    fn visit_tuple(&mut self, id: NodeID, items: &Vec<Expr>) -> Result<Self::Return, Error> {
        Ok(self.transform_tuple_lit(id, &items))
    }

    fn visit_record(&mut self, id: NodeID, items: &Vec<(Ident, Expr)>) -> Result<Self::Return, Error> {
        let mut nitems = vec!();
        for item in items {
            nitems.push(item.1.clone());
        }
        Ok(self.transform_tuple_lit(id, &nitems))
    }

    fn visit_record_update(&mut self, id: NodeID, record: &Expr, items: &Vec<(Ident, Expr)>) -> Result<Self::Return, Error> {
        Ok(self.transform_record_update(id, &record, &items))
    }

    fn visit_identifier(&mut self, id: NodeID, ident: &Ident) -> Result<Self::Return, Error> {
        let defid = self.session.get_ref(id).unwrap();
        Ok(self.transform_reference(defid, &ident.name))
    }

    fn visit_resolver(&mut self, node: &Expr, left: &Expr, right: &Ident, oid: NodeID) -> Result<Self::Return, Error> {
        let object_id = self.session.get_type_from_ref(oid).unwrap().get_id().unwrap();
        Ok(self.transform_resolve(node.id, left, &right.name, object_id))
    }

    fn visit_accessor(&mut self, node: &Expr, left: &Expr, right: &Ident, oid: NodeID) -> Result<Self::Return, Error> {
        let otype = self.session.get_type(oid).unwrap();
        Ok(self.transform_accessor(node.id, left, &right.name, otype))
    }


    fn visit_invoke(&mut self, id: NodeID, func: &Expr, args: &Vec<Expr>, fid: NodeID) -> Result<Self::Return, Error> {
        let abi = self.session.get_type(fid).unwrap().get_abi().unwrap();
        Ok(self.transform_func_invoke(abi, id, func, args))
    }


    fn visit_side_effect(&mut self, _id: NodeID, op: &Ident, args: &Vec<Expr>) -> Result<Self::Return, Error> {
        Ok(self.transform_side_effect(op.name.as_str(), args))
    }


    fn visit_if(&mut self, _id: NodeID, cond: &Expr, texpr: &Expr, fexpr: &Expr) -> Result<Self::Return, Error> {
        Ok(self.transform_if_expr(cond, texpr, fexpr))
    }

    fn visit_raise(&mut self, id: NodeID, expr: &Expr) -> Result<Self::Return, Error> {
        Ok(self.transform_raise(id, expr))
    }

    fn visit_try(&mut self, _id: NodeID, code: &Expr, cases: &Vec<MatchCase>) -> Result<Self::Return, Error> {
        Ok(self.transform_try(code, cases))
    }

    fn visit_match(&mut self, _id: NodeID, cond: &Expr, cases: &Vec<MatchCase>) -> Result<Self::Return, Error> {
        Ok(self.transform_match(cond, cases))
    }

    fn visit_while(&mut self, _id: NodeID, cond: &Expr, body: &Expr) -> Result<Self::Return, Error> {
        Ok(vec!(LLExpr::Loop(self.visit_node(cond)?, self.visit_node(body)?)))
    }


    fn visit_declare(&mut self, id: NodeID, vis: Visibility, ident: &Ident, _ttype: &Type) -> Result<Self::Return, Error> {
        let ttype = self.session.get_type_from_ref(id).unwrap();
        let abi = ttype.get_abi().unwrap();
        Ok(self.transform_func_decl(abi, id, vis, &ident.name, &ttype))
    }

    fn visit_function(&mut self, id: NodeID, vis: Visibility, ident: &Option<Ident>, args: &Vec<Argument>, _rettype: &Option<Type>, body: &Expr, abi: ABI) -> Result<Self::Return, Error> {
        Ok(self.transform_func_def(abi, id, vis, ident.as_ref().map(|ident| ident.name.as_str()), args, body))
    }

    fn visit_new(&mut self, id: NodeID, _classspec: &ClassSpec) -> Result<Self::Return, Error> {
        Ok(self.transform_new_object(id))
    }

    fn visit_class(&mut self, id: NodeID, _classspec: &ClassSpec, _parentspec: &Option<ClassSpec>, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        let mut exprs = self.transform_class_body(id, body);
        exprs.push(LLExpr::Literal(self.transform_lit(&Literal::Unit)));
        Ok(exprs)
    }

    fn visit_type_alias(&mut self, _id: NodeID, _classspec: &ClassSpec, _ttype: &Type) -> Result<Self::Return, Error> {
        /* Nothing Needs To Be Done */
        Ok(vec!())
        //Ok(self.default_return())
    }

    fn visit_enum(&mut self, id: NodeID, classspec: &ClassSpec, _variants: &Vec<EnumVariant>) -> Result<Self::Return, Error> {
        Ok(self.transform_enum_def(id, &classspec.ident.name))
    }


    fn visit_import(&mut self, _id: NodeID, ident: &Ident, decls: &Vec<Expr>) -> Result<Self::Return, Error> {
        Ok(self.transform_import(&ident.name, decls))
    }

    fn visit_definition(&mut self, id: NodeID, _mutable: Mutability, ident: &Ident, _ttype: &Option<Type>, expr: &Expr) -> Result<Self::Return, Error> {
        Ok(self.transform_def_local(id, &ident.name, expr))
    }

    fn visit_assignment(&mut self, id: NodeID, left: &Expr, right: &Expr, _ty: AssignType) -> Result<Self::Return, Error> {
        Ok(self.transform_assignment(id, left, right))
    }

    fn visit_module(&mut self, id: NodeID, name: &str, code: &Expr, memo_id: NodeID) -> Result<Self::Return, Error> {
        Ok(self.transform_module(id, name, code, memo_id))
    }

    /*
    fn visit_pattern_binding(&mut self, _id: NodeID, _ident: &Ident) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_pattern_annotation(&mut self, _id: NodeID, _ttype: &Type, subpat: &Pattern) -> Result<Self::Return, Error> {
        self.visit_pattern(subpat)
    }

    fn visit_pattern_resolve(&mut self, _id: NodeID, left: &Pattern, _field: &Ident, _oid: NodeID) -> Result<Self::Return, Error> {
        self.visit_pattern(left)
    }

    fn visit_pattern_enum_args(&mut self, id: NodeID, left: &Pattern, args: &Vec<Pattern>) -> Result<Self::Return, Error> {
        walk_pattern_enum_args(self, id, left, args)
    }

    fn visit_pattern_tuple(&mut self, id: NodeID, items: &Vec<Pattern>) -> Result<Self::Return, Error> {
        walk_pattern_tuple(self, id, items)
    }

    fn visit_pattern_record(&mut self, id: NodeID, items: &Vec<(Ident, Pattern)>) -> Result<Self::Return, Error> {
        walk_pattern_record(self, id, items)
    }

    fn visit_pattern_wild(&mut self, _id: NodeID) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_pattern_literal(&mut self, id: NodeID, lit: &Literal) -> Result<Self::Return, Error> {
        self.visit_literal(id, lit)
    }

    fn visit_pattern_identifier(&mut self, _id: NodeID, _ident: &Ident) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }
    */

    fn visit_pattern(&mut self, _pat: &Pattern) -> Result<Self::Return, Error> {
        //Ok(self.transform_pattern(pat))
        panic!("TODO: Shouldn't be using visit_pattern yet\n");
    }
}



impl<'sess> Transformer<'sess> {

    pub fn transform_module(&mut self, id: NodeID, name: &str, code: &Expr, memo_id: NodeID) -> Vec<LLExpr> {
        let defid = self.session.get_ref(id).unwrap();

        // Define a global that will store whether we've run this function or not
        let memo_name = format!("memo.{}", name);
        self.add_global(LLGlobal::DefGlobal(memo_id, LLLink::Once, memo_name, LLType::I1, true));

        let module_run_name = format!("run_{}", name);
        let exprs = ClosureTransform::transform_def(self, defid, Visibility::Public, Some(&module_run_name), &vec!(), code);

        exprs
    }

    pub fn build_main_func(&mut self, init_code: Vec<LLExpr>) {
        let main_id = NodeID::generate();
        let main_ltype = LLType::Function(vec!(), r(LLType::I64));
        let mut main_body = vec!();

        // Initialize the garbage collector
        main_body.push(LLExpr::CallC(r(LLExpr::GetNamed("molten_init".to_string())), vec!(), LLCC::CCC));

        let exp_id = self.get_global_exception().unwrap();
        let expoint = self.init_exception_point(&mut main_body, exp_id);

        let scope = self.stack.get_scope();
        // TODO there should be a better way of getting this, even if it's just a method on Session
        let module_run_name = format!("run_{}", self.session.name.replace(".", "_"));
        let run_id = scope.get_var_def(&module_run_name).unwrap();

        main_body.extend(init_code);

        // Try calling the module's run function
        let mut try = self.with_exception(exp_id, |transform| {
            ClosureTransform::create_invoke(transform, LLExpr::GetLocal(run_id), vec!())
        });
        try.push(LLExpr::Literal(LLLit::I64(0)));

        // If an exception occurs, print a message and exit with -1
        let catch = vec!(
            LLExpr::CallC(r(LLExpr::GetNamed("puts".to_string())), vec!(LLExpr::Literal(LLLit::ConstStr(String::from("Uncaught exception.  Terminating")))), LLCC::CCC),
            LLExpr::Literal(LLLit::I64(-1))
        );

        main_body.extend(self.create_exception_block(expoint, try, catch));

        self.add_global(LLGlobal::DefCFunc(main_id, LLLink::Public, String::from("main"), main_ltype, vec!(), main_body, LLCC::CCC));
    }

    pub fn transform_as_result(&mut self, exprs: &mut Vec<LLExpr>, node: &Expr) -> Option<LLExpr> {
        let mut newexprs = self.visit_node(node).unwrap();
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

    pub fn transform_value_type(&mut self, ttype: &Type) -> LLType {
        match &ttype {
            Type::Object(_, id, _) => self.get_type(*id).unwrap(),
            Type::Ref(ttype) => LLType::Ptr(r(self.transform_value_type(ttype))),
            Type::Tuple(items) => LLType::Struct(items.iter().map(|item| self.transform_value_type(&item)).collect()),
            Type::Record(items) => LLType::Struct(items.iter().map(|item| self.transform_value_type(&item.1)).collect()),
            Type::Variable(id) => panic!("Unexpected placeholder type variable during transform: {}", id),
            // TODO how will you do generics
            Type::Universal(_, _) => LLType::Var,
            Type::Function(args, ret, abi) => self.transform_func_value_type(*abi, &args.as_vec(), &*ret),
        }
    }

    pub fn transform_lit(&mut self, lit: &Literal) -> LLLit {
        match lit {
            Literal::Unit => LLLit::I32(0),
            Literal::Boolean(num) => LLLit::I1(*num as bool),
            Literal::Character(num) => LLLit::I32(*num as i32),
            Literal::Integer(num) => LLLit::I64(*num as i64),
            Literal::Real(num) => LLLit::F64(*num),
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

    pub fn create_def_local(&mut self, id: NodeID, name: &str, valexpr: LLExpr) -> Vec<LLExpr> {
        let ltype = self.transform_value_type(&self.session.get_type(id).unwrap());
        vec!(LLExpr::DefLocal(id, name.to_string(), ltype, r(valexpr)))
    }

    pub fn transform_def_local(&mut self, id: NodeID, name: &str, value: &Expr) -> Vec<LLExpr> {
        let defid = self.session.get_ref(id).unwrap();
        let mut exprs = vec!();
        let valexpr = self.transform_as_result(&mut exprs, value).unwrap();
        exprs.extend(self.create_def_local(defid, name, valexpr));
        exprs
    }

    pub fn transform_import(&mut self, name: &str, decls: &Vec<Expr>) -> Vec<LLExpr> {
        let mut exprs = vec!();

        let defid = NodeID::generate();
        let compiled_func_id = NodeID::generate();
        let scope = self.stack.get_scope();
        let ttype = Type::Function(r(Type::Tuple(vec!())), r(scope.make_obj(self.session, "Bool", vec!()).unwrap()), ABI::Molten);
        let lltype = ClosureTransform::convert_to_def_type(LLType::Function(vec!(), r(LLType::I1)));
        self.session.set_type(defid, ttype.clone());
        self.set_type(compiled_func_id, lltype.clone());

        let module_run_name = format!("run_{}", name.replace(".", "_"));
        let module_run_name_global = self.transform_func_name(Some(&module_run_name), defid);
        let module_run_name_func = format!("{}_func", module_run_name_global);
        self.add_global(LLGlobal::DeclCFunc(compiled_func_id, module_run_name_func, lltype, LLCC::FastCC));

        let ftype = self.transform_value_type(&ttype);
        self.add_global(LLGlobal::DefGlobal(defid, LLLink::Once, module_run_name_global, ftype, true));
        //exprs.extend(ClosureTransform::transform_decl(self, defid, Visibility::Public, &module_run_name, &ttype));

        exprs.push(LLExpr::SetGlobal(defid, r(ClosureTransform::make_closure_value(self, NodeID::generate(), compiled_func_id, LLExpr::Literal(LLLit::Null(LLType::Ptr(r(LLType::I8))))))));
        exprs.extend(ClosureTransform::create_invoke(self, LLExpr::GetLocal(defid), vec!()));

        self.with_context(CodeContext::Import, |transform| {
            exprs.extend(transform.visit_vec(decls).unwrap());
        });
        exprs
    }


    pub fn create_reference(&mut self, defid: NodeID) -> Vec<LLExpr> {
        match self.session.get_def(defid) {
            Ok(Def::Var(_)) => vec!(LLExpr::GetLocal(defid)),
            Ok(Def::Closure(_)) => vec!(LLExpr::GetLocal(defid)),
            Ok(_) => vec!(LLExpr::GetValue(defid)),
            Err(_) => panic!("TransformError: attempting to reference a non-existent value"),
        }
    }

    pub fn transform_reference(&mut self, defid: NodeID, name: &str) -> Vec<LLExpr> {
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
                        //vec!(LLExpr::Cast(LLType::Alias(cl.context_type_id), r(LLExpr::GetValue(cl.context_arg_id))))
                        vec!(ClosureTransform::make_closure_value(self, NodeID::generate(), cl.compiled_func_id, LLExpr::GetValue(cl.context_arg_id)))
                    } else {
                        let index = cl.find_or_add_field(self.session, defid, name, self.session.get_type(defid).unwrap());
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

    pub fn transform_accessor(&mut self, id: NodeID, obj: &Expr, field: &str, otype: Type) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let defid = self.session.get_ref(id).unwrap();
        let objval = self.transform_as_result(&mut exprs, obj).unwrap();
        exprs.extend(self.convert_accessor(defid, objval, field, otype));
        exprs
    }

    pub fn convert_accessor(&mut self, defid: NodeID, objval: LLExpr, field: &str, otype: Type) -> Vec<LLExpr> {
        let mut exprs = vec!();
        match otype {
            Type::Object(_, objid, _) => {
                let objdef = self.session.get_def(objid).unwrap();
                match self.session.get_def(defid) {
                    Ok(Def::Method(_)) => {
                        let classdef = objdef.as_class().unwrap();
                        exprs.extend(self.transform_access_method(classdef, objval, defid));
                    },
                    Ok(Def::Field(_)) => {
                        let structdef = objdef.as_struct().unwrap();
                        exprs.extend(self.transform_access_field(structdef, objval, defid));
                    },
                    result @ _ => panic!("Not Implemented: {:?}", result),
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

    pub fn transform_resolve(&mut self, id: NodeID, _path: &Expr, _field: &str, object_id: NodeID) -> Vec<LLExpr> {
        let defid = self.session.get_ref(id).unwrap();
        match self.session.get_def(object_id).unwrap() {
            Def::Class(classdef) => self.transform_resolve_method(classdef, defid),
            Def::Enum(enumdef) => self.transform_resolve_enum(id, enumdef, defid),
            def @ _ => panic!("DefError: expected class or enum but found {:?}", def),
        }
    }

    pub fn transform_assignment(&mut self, _id: NodeID, left: &Expr, right: &Expr) -> Vec<LLExpr> {
        let mut exprs = vec!();

        let value = self.transform_as_result(&mut exprs, right).unwrap();
        match &left.kind {
            ExprKind::Accessor(obj, _, oid) => {
                let objval = self.transform_as_result(&mut exprs, obj).unwrap();
                let field_id = self.session.get_ref(left.id).unwrap();
                let objdef = self.session.get_def(self.session.get_type(*oid).unwrap().get_id().unwrap()).unwrap();
                let (index, _) = objdef.as_struct().unwrap().find_field_by_id(field_id).unwrap();
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
        conds.push(self.visit_node(cond).unwrap());
        conds.push(vec!(LLExpr::Literal(LLLit::I1(true))));

        let mut blocks = vec!();
        blocks.push(self.visit_node(texpr).unwrap());
        blocks.push(self.visit_node(fexpr).unwrap());

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
                blocks.push(transform.visit_node(&case.body).unwrap());
                Ok(vec!())
            }).unwrap();
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

    // TODO this can't be moved into the visitor yet because of the value_id (which represents the value to compare against)
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
                //exprs.push(LLExpr::SetValue(item_id, r(LLExpr::GetItem(r(LLExpr::Cast(self.get_type(variant_id).unwrap(), r(LLExpr::GetValue(value_id)))), 1))));

                // TODO this indirection is a fix for an infinite loop during optimization when directly fetching the second argument in an enum, the raw array
                //      The problem with the previous approach is that the Cast llexpr is very complicated and will not necessarily do the right conversion when
                //      using a struct instead of a pointer to a struct.  Creating a temporary variable means that the direct variable reference is a pointer, and
                //      can thus be indexed as one.  This might change if enums are made into references always
                let tmp_id = NodeID::generate();
                let ltype = self.get_type(variant_id).unwrap();
                exprs.push(LLExpr::DefLocal(tmp_id, tmp_id.to_string(), ltype.clone(), r(LLExpr::GetValue(value_id))));

                exprs.push(LLExpr::SetValue(item_id, r(
                    LLExpr::LoadRef(r(
                        LLExpr::AccessRef(r(
                            LLExpr::Cast(LLType::Ptr(r(ltype)), r(LLExpr::GetValue(tmp_id)))
                        ), vec!(LLRef::Field(1)))
                    ))
                )));

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

    pub fn transform_side_effect(&mut self, op: &str, args: &Vec<Expr>) -> Vec<LLExpr> {
        let mut conds = vec!();
        let mut blocks = vec!();

        // TODO this doesn't work with non-boolean values
        match op {
            "and" => {
                conds.push(self.visit_node(&args[0]).unwrap());
                blocks.push(self.visit_node(&args[1]).unwrap());
                vec!(LLExpr::Phi(conds, blocks))
            },
            "or" => {
                conds.push(self.visit_node(&args[0]).unwrap());
                blocks.push(vec!(LLExpr::Literal(LLLit::I1(true))));
                conds.push(vec!(LLExpr::Literal(LLLit::I1(true))));
                blocks.push(self.visit_node(&args[1]).unwrap());
                vec!(LLExpr::Phi(conds, blocks))
            },
            _ => panic!("Not Implemented: {:?}", op),
        }
    }
}


