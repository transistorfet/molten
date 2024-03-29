
use std::collections::HashMap;

use crate::abi::ABI;
use crate::defs::Def;
use crate::types::Type;
use crate::misc::UniqueID;
use crate::config::Options;
use crate::scope::ScopeRef;
use crate::session::{ Session, Error };
use crate::parsing::ast::Pos;
use crate::analysis::hir::{ Visibility, Mutability, AssignType, Literal, MatchCase, EnumVariant, WhereClause, Function, Pattern, PatKind, Expr, ExprKind };
use crate::defs::modules::ModuleDef;

use crate::misc::{ r };
use crate::transform::functions::ClosureTransform;
use crate::llvm::llcode::{ LLType, LLLit, LLRef, LLCmpType, LLLink, LLCC, LLExpr, LLGlobal };

use crate::analysis::visitor::{ Visitor, ScopeStack };


#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CodeContext {
    Func(ABI, UniqueID),
    Import,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Transformer<'sess> {
    pub session: &'sess Session,
    pub stack: ScopeStack,
    pub context: Vec<CodeContext>,
    pub globals: Vec<LLGlobal>,
    pub expoints: Vec<UniqueID>,
    pub types: HashMap<UniqueID, LLType>,
}

impl<'sess> Transformer<'sess> {
    pub fn new(session: &'sess Session) -> Self {
        Transformer {
            session,
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
        self.set_type(global.get_type_def(&"Bool".to_string()).unwrap(), LLType::I1);
        self.set_type(global.get_type_def(&"Byte".to_string()).unwrap(), LLType::I8);
        self.set_type(global.get_type_def(&"Char".to_string()).unwrap(), LLType::I32);
        self.set_type(global.get_type_def(&"Int".to_string()).unwrap(), LLType::I64);
        self.set_type(global.get_type_def(&"Real".to_string()).unwrap(), LLType::F64);
        self.set_type(global.get_type_def(&"String".to_string()).unwrap(), LLType::Ptr(r(LLType::I8)));
        self.set_type(global.get_type_def(&"UniversalArray".to_string()).unwrap(), LLType::Ptr(r(LLType::Var)));

        self.initialize_exception_type();
    }

    pub fn set_type(&mut self, id: UniqueID, ltype: LLType) {
        self.types.insert(id, ltype);
    }

    pub fn get_type(&self, id: UniqueID) -> Result<LLType, Error> {
        self.types.get(&id).map(|ltype| ltype.clone()).ok_or(Error::new(format!("Transform Type Unset: {}", id)))
    }

    pub fn add_global(&mut self, global: LLGlobal) {
        self.globals.push(global);
    }

    pub fn insert_global(&mut self, index: usize, global: LLGlobal) {
        self.globals.insert(index, global);
    }

    pub fn get_context(&self) -> Option<CodeContext> {
        self.context.last().map(|c| *c)
    }

    pub fn with_context<F, R>(&mut self, context: CodeContext, f: F) -> R where F: FnOnce(&mut Self) -> R {
        self.context.push(context);
        let ret = f(self);
        self.context.pop();
        ret
    }

    pub fn with_exception<F, R>(&mut self, expoint: UniqueID, f: F) -> R where F: FnOnce(&mut Self) -> R {
        self.expoints.push(expoint);
        let ret = f(self);
        self.expoints.pop();
        ret
    }

    pub fn get_exception(&self) -> Option<UniqueID> {
        self.expoints.last().map(|e| *e)
    }

    pub fn get_global_exception(&self) -> Option<UniqueID> {
        self.expoints.first().map(|e| *e)
    }

    pub fn transform_code(&mut self, scope: ScopeRef, code: &Vec<Expr>) {
        self.stack.push_scope(scope);

        let exp_id = UniqueID::generate();
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

    fn get_scope_stack(&self) -> &ScopeStack {
        &self.stack
    }

    fn get_session(&self) -> &Session {
        self.session
    }

    fn handle_error(&mut self, node: &Expr, err: Error) -> Result<Self::Return, Error> {
        self.session.print_error(&err.add_pos(node.get_pos()));
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


    fn visit_literal(&mut self, _refid: UniqueID, lit: &Literal) -> Result<Self::Return, Error> {
        Ok(vec!(LLExpr::Literal(self.transform_lit(lit))))
    }

    fn visit_annotation(&mut self, refid: UniqueID, _ttype: &Type, code: &Expr) -> Result<Self::Return, Error> {
        let mut exprs = vec!();
        let ltype = self.transform_value_type(&self.session.get_type(refid).unwrap());
        let result = self.transform_as_result(&mut exprs, code);
        //let result = self.check_convert_to_trait(&mut exprs, self.session.get_type(refid).unwrap(), code);
        exprs.push(LLExpr::Cast(ltype, r(result)));
        Ok(exprs)
    }

    fn visit_pack_universal(&mut self, _refid: UniqueID, ttype: &Type, code: &Expr) -> Result<Self::Return, Error> {
        let mut exprs = vec!();
        let src_type = self.session.get_type(code.id).unwrap();
        let result = self.transform_as_result(&mut exprs, code);
        let result = self.convert_to_trait_universal(&mut exprs, &ttype, &src_type, result);
        exprs.push(result);
        Ok(exprs)
    }

    fn visit_unpack_universal(&mut self, _refid: UniqueID, ttype: &Type, code: &Expr) -> Result<Self::Return, Error> {
        let mut exprs = vec!();
        let result = self.transform_as_result(&mut exprs, code);
        let result = self.convert_from_trait_universal(&mut exprs, &ttype, result);
        exprs.push(result);
        Ok(exprs)
    }

    fn visit_ref(&mut self, refid: UniqueID, expr: &Expr) -> Result<Self::Return, Error> {
        Ok(self.transform_alloc_ref(refid, expr))
    }

    fn visit_deref(&mut self, _refid: UniqueID, expr: &Expr) -> Result<Self::Return, Error> {
        Ok(self.transform_deref_ref(expr))
    }

    fn visit_tuple(&mut self, refid: UniqueID, items: &Vec<Expr>) -> Result<Self::Return, Error> {
        Ok(self.transform_tuple_lit(refid, items))
    }

    fn visit_record(&mut self, refid: UniqueID, items: &Vec<(String, Expr)>) -> Result<Self::Return, Error> {
        let mut nitems = vec!();
        for item in items {
            nitems.push(item.1.clone());
        }
        Ok(self.transform_tuple_lit(refid, &nitems))
    }

    fn visit_record_update(&mut self, refid: UniqueID, record: &Expr, items: &Vec<(String, Expr)>) -> Result<Self::Return, Error> {
        Ok(self.transform_record_update(refid, record, items))
    }

    fn visit_identifier(&mut self, refid: UniqueID, name: &str) -> Result<Self::Return, Error> {
        let defid = self.session.get_ref(refid).unwrap();
        Ok(self.transform_reference(defid, name))
    }

    fn visit_resolver(&mut self, refid: UniqueID, left: &Expr, right: &str, oid: UniqueID) -> Result<Self::Return, Error> {
        let object_id = self.session.get_type_from_ref(oid).unwrap().get_id().unwrap();
        Ok(self.transform_resolve(refid, left, right, object_id))
    }

    fn visit_accessor(&mut self, refid: UniqueID, left: &Expr, right: &str, oid: UniqueID) -> Result<Self::Return, Error> {
        Ok(self.transform_accessor(refid, left, right, oid))
    }


    fn visit_invoke(&mut self, refid: UniqueID, func: &Expr, args: &Vec<Expr>, fid: UniqueID) -> Result<Self::Return, Error> {
        let deftype = self.session.get_type(fid).unwrap();
        let abi = deftype.get_abi().unwrap();
        Ok(self.transform_func_invoke(abi, refid, func, args))
    }


    fn visit_side_effect(&mut self, _refid: UniqueID, op: &str, args: &Vec<Expr>) -> Result<Self::Return, Error> {
        Ok(self.transform_side_effect(op, args))
    }


    fn visit_if(&mut self, _refid: UniqueID, cond: &Expr, texpr: &Expr, fexpr: &Expr) -> Result<Self::Return, Error> {
        Ok(self.transform_if_expr(cond, texpr, fexpr))
    }

    fn visit_raise(&mut self, refid: UniqueID, expr: &Expr) -> Result<Self::Return, Error> {
        Ok(self.transform_raise(refid, expr))
    }

    fn visit_try(&mut self, _refid: UniqueID, code: &Expr, cases: &Vec<MatchCase>) -> Result<Self::Return, Error> {
        Ok(self.transform_try(code, cases))
    }

    fn visit_match(&mut self, _refid: UniqueID, cond: &Expr, cases: &Vec<MatchCase>) -> Result<Self::Return, Error> {
        Ok(self.transform_match(cond, cases))
    }

    fn visit_while(&mut self, _refid: UniqueID, cond: &Expr, body: &Expr) -> Result<Self::Return, Error> {
        Ok(vec!(LLExpr::Loop(self.visit_node(cond)?, self.visit_node(body)?)))
    }


    fn visit_declare(&mut self, refid: UniqueID, vis: Visibility, name: &str, _ttype: &Type, _whereclause: &WhereClause) -> Result<Self::Return, Error> {
        let ttype = self.session.get_type_from_ref(refid).unwrap();
        let abi = ttype.get_abi().unwrap();
        Ok(self.transform_func_decl(abi, refid, vis, name, &ttype))
    }

    fn visit_function(&mut self, refid: UniqueID, func: &Function) -> Result<Self::Return, Error> {
        Ok(self.transform_func_def(refid, func))
    }

    fn visit_alloc_object(&mut self, refid: UniqueID, _ttype: &Type) -> Result<Self::Return, Error> {
        Ok(self.transform_alloc_object(refid))
    }

    fn visit_class(&mut self, refid: UniqueID, _classtype: &Type, _parenttype: &Option<Type>, _whereclause: &WhereClause, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        let mut exprs = self.transform_class_body(refid, body);
        exprs.push(LLExpr::Literal(self.transform_lit(&Literal::Unit)));
        Ok(exprs)
    }

    fn visit_type_alias(&mut self, _refid: UniqueID, _deftype: &Type, _ttype: &Type) -> Result<Self::Return, Error> {
        /* Nothing Needs To Be Done */
        Ok(vec!())
        //Ok(self.default_return())
    }

    fn visit_enum(&mut self, refid: UniqueID, enumtype: &Type, _whereclause: &WhereClause, _variants: &Vec<EnumVariant>) -> Result<Self::Return, Error> {
        Ok(self.transform_enum_def(refid, enumtype.get_name()?))
    }

    fn visit_trait_def(&mut self, refid: UniqueID, traitname: &str, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        let defid = self.session.get_ref(refid).unwrap();
        Ok(self.transform_trait_def(defid, traitname, body))
    }

    fn visit_trait_impl(&mut self, refid: UniqueID, _traitname: &str, _impltype: &Type, _whereclause: &WhereClause, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        Ok(self.transform_trait_impl(refid, body))
    }

    fn visit_import(&mut self, _refid: UniqueID, name: &str, decls: &Vec<Expr>) -> Result<Self::Return, Error> {
        Ok(self.transform_import(name, decls))
    }

    fn visit_definition(&mut self, refid: UniqueID, _mutable: Mutability, name: &str, _ttype: &Option<Type>, expr: &Expr) -> Result<Self::Return, Error> {
        Ok(self.transform_def_local(refid, name, expr))
    }

    fn visit_field(&mut self, _refid: UniqueID, _mutable: Mutability, _name: &str, _ttype: &Option<Type>) -> Result<Self::Return, Error> {
        // Do nothing because a field only appears inside a structdef
        Ok(vec!())
    }

    fn visit_assignment(&mut self, refid: UniqueID, left: &Expr, right: &Expr, _ty: AssignType) -> Result<Self::Return, Error> {
        Ok(self.transform_assignment(refid, left, right))
    }

    fn visit_module(&mut self, refid: UniqueID, name: &str, code: &Expr, memo_id: UniqueID) -> Result<Self::Return, Error> {
        Ok(self.transform_module(refid, name, code, memo_id))
    }

    fn visit_module_decl(&mut self, _refid: UniqueID, name: &str) -> Result<Self::Return, Error> {
        Ok(self.transform_module_decl(name))
    }

    /*
    fn visit_pattern_binding(&mut self, _refid: UniqueID, _name: &str) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_pattern_annotation(&mut self, _refid: UniqueID, _ttype: &Type, subpat: &Pattern) -> Result<Self::Return, Error> {
        self.visit_pattern(subpat)
    }

    fn visit_pattern_resolve(&mut self, _refid: UniqueID, left: &Pattern, _field: &str, _oid: UniqueID) -> Result<Self::Return, Error> {
        self.visit_pattern(left)
    }

    fn visit_pattern_enum_args(&mut self, refid: UniqueID, left: &Pattern, args: &Vec<Pattern>) -> Result<Self::Return, Error> {
        walk_pattern_enum_args(self, refid, left, args)
    }

    fn visit_pattern_tuple(&mut self, refid: UniqueID, items: &Vec<Pattern>) -> Result<Self::Return, Error> {
        walk_pattern_tuple(self, refid, items)
    }

    fn visit_pattern_record(&mut self, refid: UniqueID, items: &Vec<(String, Pattern)>) -> Result<Self::Return, Error> {
        walk_pattern_record(self, refid, items)
    }

    fn visit_pattern_wild(&mut self, _refid: UniqueID) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_pattern_literal(&mut self, refid: UniqueID, lit: &Literal) -> Result<Self::Return, Error> {
        self.visit_literal(refid, lit)
    }

    fn visit_pattern_identifier(&mut self, _refid: UniqueID, _name: &str) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }
    */

    fn visit_pattern(&mut self, _pat: &Pattern) -> Result<Self::Return, Error> {
        //Ok(self.transform_pattern(pat))
        panic!("TODO: Shouldn't be using visit_pattern yet\n");
    }
}



impl<'sess> Transformer<'sess> {

    pub fn transform_module(&mut self, _id: UniqueID, name: &str, code: &Expr, memo_id: UniqueID) -> Vec<LLExpr> {
        // Define a global that will store whether we've run this function or not
        let memo_name = ModuleDef::get_memo_name(&ModuleDef::get_module_name(name));
        self.add_global(LLGlobal::DefGlobal(memo_id, LLLink::Once, memo_name, LLType::I1, true));

        self.visit_node(code).unwrap()
    }

    pub fn build_main_func(&mut self, init_code: Vec<LLExpr>) {
        let main_id = UniqueID::generate();
        let main_ltype = LLType::Function(vec!(), r(LLType::I64));
        let mut main_body = vec!();

        // Initialize the garbage collector
        main_body.push(LLExpr::CallC(r(LLExpr::GetNamed("molten_init".to_string())), vec!(), LLCC::CCC));

        let exp_id = self.get_global_exception().unwrap();
        let expoint = self.init_exception_point(&mut main_body, exp_id);

        let scope = self.stack.get_scope();
        let module_run_name = ModuleDef::get_run_name(&ModuleDef::get_module_name(&self.session.name));
        let run_id = scope.get_var_def(&module_run_name).unwrap();

        main_body.extend(init_code);

        // Try calling the module's run function
        let mut tryblock = self.with_exception(exp_id, |transform| {
            ClosureTransform::create_invoke(transform, LLExpr::GetLocal(run_id), vec!())
        });
        tryblock.push(LLExpr::Literal(LLLit::I64(0)));

        // If an exception occurs, print a message and exit with -1
        let catch = vec!(
            LLExpr::CallC(r(LLExpr::GetNamed("puts".to_string())), vec!(LLExpr::Literal(LLLit::ConstStr(String::from("Uncaught exception.  Terminating")))), LLCC::CCC),
            LLExpr::Literal(LLLit::I64(-1))
        );

        main_body.extend(self.create_exception_block(expoint, tryblock, catch));

        self.add_global(LLGlobal::DefCFunc(main_id, LLLink::Public, String::from("main"), main_ltype, vec!(), main_body, LLCC::CCC));
    }

    pub fn transform_as_result(&mut self, exprs: &mut Vec<LLExpr>, node: &Expr) -> LLExpr {
        let mut newexprs = self.visit_node(node).unwrap();
        let last = newexprs.pop().unwrap();
        exprs.extend(newexprs);
        last
    }

    pub fn transform_as_args(&mut self, exprs: &mut Vec<LLExpr>, args: &Vec<Expr>) -> Vec<LLExpr> {
        let mut fargs = vec!();
        for arg in args.iter() {
            fargs.push(self.transform_as_result(exprs, arg));
        }
        fargs
    }

    pub fn transform_value_type(&mut self, ttype: &Type) -> LLType {
        match &ttype {
            Type::Object(_, id, _) => self.get_type(*id).unwrap(),
            Type::Ref(ttype) => LLType::Ptr(r(self.transform_value_type(ttype))),
            Type::Tuple(items) => LLType::Struct(items.iter().map(|item| self.transform_value_type(item)).collect()),
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

    pub fn transform_tuple_lit(&mut self, refid: UniqueID, items: &Vec<Expr>) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let mut litems = vec!();
        for item in items {
            litems.push(self.transform_as_result(&mut exprs, item));
        }
        let ltype = self.transform_value_type(&self.session.get_type(refid).unwrap());
        exprs.push(LLExpr::DefStruct(refid, ltype, litems));
        exprs
    }

    pub fn transform_record_update(&mut self, refid: UniqueID, record: &Expr, items: &Vec<(String, Expr)>) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let mut litems = vec!();

        let ttype = &self.session.get_type(refid).unwrap();
        let valexpr = self.transform_as_result(&mut exprs, record);

        for (name, _) in ttype.get_record_types().unwrap() {
            let item = match items.iter().find(|(ident, _)| ident == name) {
                Some((_, expr)) => self.transform_as_result(&mut exprs, expr),
                None => LLExpr::GetItem(r(valexpr.clone()), litems.len()),
            };
            litems.push(item);
        }
        exprs.push(LLExpr::DefStruct(refid, self.transform_value_type(ttype), litems));
        exprs
    }

    pub fn create_def_local(&mut self, defid: UniqueID, name: &str, valexpr: LLExpr) -> Vec<LLExpr> {
        let ltype = self.transform_value_type(&self.session.get_type(defid).unwrap());
        vec!(LLExpr::DefLocal(defid, name.to_string(), ltype, r(valexpr)))
    }

    pub fn transform_def_local(&mut self, refid: UniqueID, name: &str, value: &Expr) -> Vec<LLExpr> {
        let defid = self.session.get_ref(refid).unwrap();
        let mut exprs = vec!();
        let valexpr = self.transform_as_result(&mut exprs, value);
        //let ttype = self.session.get_type(defid).unwrap();
        //let valexpr = self.check_transform_to_trait(&mut exprs, &ttype, value);
        exprs.extend(self.create_def_local(defid, name, valexpr));
        exprs
    }

    pub fn transform_import(&mut self, _name: &str, decls: &Vec<Expr>) -> Vec<LLExpr> {
        self.with_context(CodeContext::Import, |transform| {
            transform.visit_vec(decls)
        }).unwrap()
    }

    pub fn transform_module_decl(&mut self, name: &str) -> Vec<LLExpr> {
        let mut exprs = vec!();

        let defid = UniqueID::generate();
        let compiled_func_id = UniqueID::generate();
        let scope = self.stack.get_scope();
        let ttype = Type::Function(r(Type::Tuple(vec!())), r(scope.find_type(self.session, "Bool").unwrap()), ABI::Molten);
        let lltype = ClosureTransform::convert_to_def_type(LLType::Function(vec!(), r(LLType::I1)));
        self.session.set_type(defid, ttype.clone());
        self.set_type(compiled_func_id, lltype.clone());

        let module_run_name = ModuleDef::get_run_name(&ModuleDef::get_module_name(name));
        let module_run_name_global = self.transform_func_name(&module_run_name, defid);
        let module_run_name_func = format!("{}_func", module_run_name_global);
        self.add_global(LLGlobal::DeclCFunc(compiled_func_id, module_run_name_func, lltype, LLCC::FastCC));

        let ftype = self.transform_value_type(&ttype);
        self.add_global(LLGlobal::DefGlobal(defid, LLLink::Once, module_run_name_global, ftype, true));
        //exprs.extend(ClosureTransform::transform_decl(self, defid, Visibility::Public, &module_run_name, &ttype));

        exprs.push(LLExpr::SetGlobal(defid, r(ClosureTransform::make_closure_value(self, UniqueID::generate(), compiled_func_id, LLExpr::Literal(LLLit::Null(LLType::Ptr(r(LLType::I8))))))));
        exprs.extend(ClosureTransform::create_invoke(self, LLExpr::GetLocal(defid), vec!()));
        exprs
    }


    pub fn create_reference(&mut self, defid: UniqueID) -> Vec<LLExpr> {
        match self.session.get_def(defid) {
            Ok(Def::Var(_)) => vec!(LLExpr::GetLocal(defid)),
            Ok(Def::Closure(_)) => vec!(LLExpr::GetLocal(defid)),
            Ok(_) => vec!(LLExpr::GetValue(defid)),
            Err(_) => panic!("TransformError: attempting to reference a non-existent value with id {:?}", defid),
        }
    }

    pub fn transform_reference(&mut self, defid: UniqueID, name: &str) -> Vec<LLExpr> {
        match ClosureTransform::check_convert_closure_reference(self, defid, name) {
            Some(exprs) => exprs,
            None => self.create_reference(defid),
        }
    }


    pub fn transform_alloc_ref(&mut self, refid: UniqueID, value: &Expr) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let valexpr = self.transform_as_result(&mut exprs, value);
        let ltype = self.transform_value_type(&self.session.get_type(refid).unwrap());
        exprs.push(LLExpr::AllocRef(refid, ltype, Some(r(valexpr))));
        exprs
    }

    pub fn transform_deref_ref(&mut self, value: &Expr) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let valexpr = self.transform_as_result(&mut exprs, value);
        exprs.push(LLExpr::LoadRef(r(valexpr)));
        exprs
    }

    pub fn transform_alloc_object(&mut self, refid: UniqueID) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let defid = self.session.get_ref(refid).unwrap();
        let ltype = self.transform_value_type(&self.session.get_type(defid).unwrap());

        exprs.push(LLExpr::AllocRef(refid, ltype, None));
        if let Def::Class(classdef) = self.session.get_def(defid).unwrap() {
            if let Some(index) = classdef.get_struct_vtable_index() {
                exprs.push(LLExpr::StoreRef(r(LLExpr::AccessRef(r(LLExpr::GetValue(refid)), vec!(LLRef::Field(index)))), r(LLExpr::GetLocal(classdef.vtable.id))));
                exprs.push(LLExpr::GetValue(refid));
            }
        }
        exprs
    }

    pub fn transform_accessor(&mut self, refid: UniqueID, obj: &Expr, field: &str, oid: UniqueID) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let defid = self.session.get_ref(refid).unwrap();
        let objval = self.transform_as_result(&mut exprs, obj);
        exprs.extend(self.convert_accessor(defid, objval, field, oid));
        exprs
    }

    pub fn convert_accessor(&mut self, defid: UniqueID, objval: LLExpr, field: &str, oid: UniqueID) -> Vec<LLExpr> {
        let mut exprs = vec!();

        let otype = self.session.get_type(oid).unwrap();
        match otype {
            Type::Object(_, objid, _) => {
                let objdef = self.session.get_def(objid).unwrap();
                match self.session.get_def(defid) {
                    Ok(Def::Method(_)) => {
                        match objdef {
                            Def::Class(classdef) =>
                                exprs.extend(self.transform_class_access_method(classdef, objval, defid)),
                            Def::Enum(_) =>
                                exprs.extend(self.transform_enum_access_method(defid)),
                            result => panic!("Not Implemented: {:?}", result),
                        }
                    },
                    Ok(Def::TraitFunc(_)) => {
                        let traitdef = objdef.as_trait_def().unwrap();
                        exprs.extend(self.transform_trait_access_method(traitdef, objval, defid));
                    },
                    Ok(Def::Field(_)) => {
                        let structdef = objdef.as_struct().unwrap();
                        exprs.extend(self.transform_class_access_field(structdef, objval, defid));
                    },
                    result => panic!("Not Implemented: {:?}", result),
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
            Type::Universal(_, _) => {
                let trait_id = self.session.get_ref(oid).unwrap();
                let traitdef = self.session.get_def(trait_id).unwrap().as_trait_def().unwrap();
                exprs.extend(self.transform_trait_access_method(traitdef, objval, defid))
            },
            _ => panic!("Not Implemented: {:?}", otype),
        }
        exprs
    }

    pub fn transform_resolve(&mut self, refid: UniqueID, _path: &Expr, _field: &str, object_id: UniqueID) -> Vec<LLExpr> {
        let field_id = self.session.get_ref(refid).unwrap();
        match self.session.get_def(object_id).unwrap() {
            Def::Class(classdef) => self.transform_class_resolve_method(classdef, field_id),
            Def::Enum(enumdef) => self.transform_enum_resolve_constructor(enumdef, field_id),
            def => panic!("DefError: expected class or enum but found {:?}", def),
        }
    }

    pub fn transform_assignment(&mut self, _refid: UniqueID, left: &Expr, right: &Expr) -> Vec<LLExpr> {
        let mut exprs = vec!();

        let value = self.transform_as_result(&mut exprs, right);
        match &left.kind {
            ExprKind::Accessor(obj, _, oid) => {
                let objval = self.transform_as_result(&mut exprs, obj);
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
                let result = self.transform_as_result(&mut exprs, node);
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

        let condid = UniqueID::generate();
        exprs.push(LLExpr::SetValue(condid, r(condval)));

        for case in cases {
            let lscope = self.session.map.get(case.id).unwrap();
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
        let condval = self.transform_as_result(&mut exprs, cond);
        exprs.extend(self.create_match(condval, cases));
        exprs
    }

    pub fn transform_pattern_as_result(&mut self, exprs: &mut Vec<LLExpr>, pat: &Pattern, value_id: UniqueID) -> LLExpr {
        let mut newexprs = self.transform_pattern(pat, value_id);
        let last = newexprs.pop().unwrap();
        exprs.extend(newexprs);
        last
    }

    pub fn transform_pattern_conjunction(&mut self, exprs: &mut Vec<LLExpr>, prev: Option<LLExpr>, pat: &Pattern, value_id: UniqueID) -> LLExpr {
        let result = self.transform_pattern_as_result(exprs, pat, value_id);
        match prev {
            //Some(expr) => LLExpr::Cmp(LLCmpType::Equal, r(expr), r(result)),
            Some(expr) => LLExpr::And(r(LLExpr::Cmp(LLCmpType::Equal, r(expr), r(LLExpr::Literal(LLLit::I1(true))))), r(LLExpr::Cmp(LLCmpType::Equal, r(result), r(LLExpr::Literal(LLLit::I1(true)))))),
            None => result
        }
    }

    // TODO this can't be moved into the visitor yet because of the value_id (which represents the value to compare against)
    pub fn transform_pattern(&mut self, pat: &Pattern, value_id: UniqueID) -> Vec<LLExpr> {
        let mut exprs = vec!();

        match &pat.kind {
            PatKind::Wild => exprs.push(LLExpr::Literal(LLLit::I1(true))),

            PatKind::Literal(lit, fid) => {
                let compfunc = self.transform_as_result(&mut exprs, &Expr::new_with_id(*fid, Pos::empty(), ExprKind::Identifier("==".to_string())));
                let compabi = self.session.get_type(*fid).unwrap().get_abi().unwrap();
                let result = LLExpr::Literal(self.transform_lit(lit));
                exprs.extend(self.create_func_invoke(compabi, compfunc, vec!(LLExpr::GetValue(value_id), result)));
            },

            PatKind::Binding(name) => {
                let defid = self.session.get_ref(pat.id).unwrap();
                exprs.extend(self.create_def_local(defid, name, LLExpr::GetValue(value_id)));
                exprs.push(LLExpr::Literal(LLLit::I1(true)));
            },

            PatKind::Annotation(_, subpat) => {
                let ttype = self.session.get_type(pat.id).unwrap();
                exprs.push(LLExpr::SetValue(pat.id, r(LLExpr::Cast(self.transform_value_type(&ttype), r(LLExpr::GetValue(value_id))))));
                exprs.extend(self.transform_pattern(subpat, pat.id));
            },

            PatKind::PackUniversal(ttype, subpat) => {
                let unpacked_type = self.session.get_type(pat.id).unwrap();
                let value = self.convert_to_trait_universal(&mut exprs, &unpacked_type, ttype, LLExpr::GetValue(value_id));
                exprs.push(LLExpr::SetValue(pat.id, r(value)));
                exprs.extend(self.transform_pattern(subpat, pat.id));
            },

            PatKind::UnpackUniversal(ttype, subpat) => {
                let value = self.convert_from_trait_universal(&mut exprs, ttype, LLExpr::GetValue(value_id));
                exprs.push(LLExpr::SetValue(pat.id, r(value)));
                exprs.extend(self.transform_pattern(subpat, pat.id));
            },

            PatKind::Ref(subpat) => {
                exprs.push(LLExpr::SetValue(pat.id, r(LLExpr::LoadRef(r(LLExpr::GetValue(value_id))))));
                exprs.extend(self.transform_pattern(subpat, pat.id));
            },

            PatKind::Tuple(items) => {
                let mut prev = None;

                for (i, item) in items.iter().enumerate() {
                    let value = LLExpr::GetItem(r(LLExpr::GetValue(value_id)), i);
                    exprs.push(LLExpr::SetValue(item.id, r(value)));
                    prev = Some(self.transform_pattern_conjunction(&mut exprs, prev, item, item.id));
                }
                exprs.push(prev.unwrap());
            },

            PatKind::Record(items) => {
                let mut prev = None;

                for (i, (_, item)) in items.iter().enumerate() {
                    //let index = items.iter().position(|(name, _)| name == field).unwrap();
                    let value = LLExpr::GetItem(r(LLExpr::GetValue(value_id)), i);
                    exprs.push(LLExpr::SetValue(item.id, r(value)));
                    prev = Some(self.transform_pattern_conjunction(&mut exprs, prev, item, item.id));
                }
                exprs.push(prev.unwrap());
            },

            PatKind::EnumVariant(_, args, eid) => {
                let enum_id = self.session.get_ref(pat.id).unwrap();
                let enumdef = self.session.get_def(enum_id).unwrap().as_enum().unwrap();
                let variant_id = self.session.get_ref(*eid).unwrap();
                let variant = enumdef.get_variant_by_id(variant_id).unwrap();

                if args.len() > 0 {
                    // TODO this indirection is a fix for an infinite loop during optimization when directly fetching the second argument in an enum, the raw array
                    //      The problem with the previous approach is that the Cast llexpr is very complicated and will not necessarily do the right conversion when
                    //      using a struct instead of a pointer to a struct.  Creating a temporary variable means that the direct variable reference is a pointer, and
                    //      can thus be indexed as one.  This might change if enums are made into references always
                    let value_ref_id = UniqueID::generate();
                    let variant_type = self.get_type(variant_id).unwrap();
                    exprs.push(LLExpr::DefLocal(value_ref_id, value_ref_id.to_string(), variant_type.clone(), r(LLExpr::Cast(variant_type.clone(), r(LLExpr::GetValue(value_id))))));

                    // Extract the data struct out of the tagged struct
                    let item_id = UniqueID::generate();
                    exprs.push(LLExpr::SetValue(item_id, r(
                        LLExpr::LoadRef(r(
                            LLExpr::AccessRef(r(
                                LLExpr::Cast(LLType::Ptr(r(variant_type)), r(LLExpr::GetValue(value_ref_id)))
                            ), vec!(LLRef::Field(1)))
                        ))
                    )));

                    // For each argument, get the corresponding value from the enum data struct and match it against the associated pattern
                    let mut prev = None;
                    for (i, arg) in args.iter().enumerate() {
                        exprs.push(LLExpr::SetValue(arg.id, r(LLExpr::GetItem(r(LLExpr::GetValue(item_id)), i))));
                        prev = Some(self.transform_pattern_conjunction(&mut exprs, prev, arg, arg.id));
                    }
                    exprs.push(prev.unwrap());
                } else {
                    exprs.push(LLExpr::Literal(LLLit::I1(true)));
                }

                let variant_result = LLExpr::Cmp(LLCmpType::Equal, r(LLExpr::GetItem(r(LLExpr::Cast(LLType::Alias(enumdef.id), r(LLExpr::GetValue(value_id)))), 0)), r(LLExpr::Literal(LLLit::I8(variant as i8))));
                exprs = vec!(LLExpr::Phi(vec!(vec!(variant_result)), vec!(exprs)));
            },
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


