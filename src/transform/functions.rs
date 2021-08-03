
use crate::binding;
use crate::typecheck;

use crate::abi::ABI;
use crate::types::Type;
use crate::ast::{ Pos };
use crate::hir::{ NodeID, Visibility, Mutability, Argument, Function, Expr, ExprKind };
use crate::defs::Def;
use crate::visitor::{ Visitor };

use crate::misc::{ r };
use crate::transform::transform::{ Transformer, CodeContext };
use crate::transform::classes::{ StructTransform };
use crate::llvm::llcode::{ LLType, LLLink, LLCC, LLExpr, LLGlobal };


impl<'sess> Transformer<'sess> {
    pub fn transform_vis(&mut self, vis: Visibility) -> LLLink {
        match vis {
            Visibility::Private | Visibility::Anonymous => LLLink::Private,
            Visibility::Public | Visibility::Global => LLLink::Public,
        }
    }

    pub fn transform_func_name(&mut self, name: &str, id: NodeID) -> String {
        let scope = self.stack.get_scope();
        let ftype = self.session.get_type(id).unwrap();
        scope.get_full_name(ftype.get_abi().unwrap_or(ABI::Molten).mangle_name(name, ftype.get_argtypes().unwrap()))
    }

    pub fn transform_func_def_type(&mut self, abi: ABI, args: &Vec<Type>, ret: &Type) -> LLType {
        match abi {
            ABI::C => CFuncTransform::transform_def_type(self, args, ret),
            ABI::Molten | ABI::Unknown => ClosureTransform::transform_def_type(self, args, ret),
            _ => panic!("Not Implemented: {:?}", abi),
        }
    }

    pub fn transform_func_value_type(&mut self, abi: ABI, args: &Vec<Type>, ret: &Type) -> LLType {
        match abi {
            ABI::C => LLType::Ptr(r(CFuncTransform::transform_def_type(self, args, ret))),
            ABI::Molten | ABI::Unknown => ClosureTransform::transform_value_type(self, args, ret),
            _ => panic!("Not Implemented: {:?}", abi),
        }
    }

    pub fn transform_func_decl(&mut self, abi: ABI, id: NodeID, vis: Visibility, name: &str, ttype: &Type) -> Vec<LLExpr> {
        let defid = self.session.get_ref(id).unwrap();
        match abi {
            ABI::C => CFuncTransform::transform_decl(self, defid, vis, name, ttype),
            ABI::Molten | ABI::Unknown => ClosureTransform::transform_decl(self, defid, vis, name, ttype),
            _ => panic!("Not Implemented: {:?}", abi),
        }
    }

    pub fn transform_func_def(&mut self, id: NodeID, func: &Function) -> Vec<LLExpr> {
        let defid = self.session.get_ref(id).unwrap();
        match func.abi {
            ABI::C => CFuncTransform::transform_def(self, defid, func.vis, &func.name, &func.args, &func.body),
            ABI::Molten | ABI::Unknown => ClosureTransform::transform_def(self, defid, func.vis, &func.name, &func.args, &func.body),
            _ => panic!("Not Implemented: {:?}", func.abi),
        }
    }

    pub fn transform_func_invoke(&mut self, abi: ABI, _id: NodeID, func: &Expr, args: &Vec<Expr>, ftype: &Type, rettype: &Type) -> Vec<LLExpr> {
        let mut exprs = vec!();

        let argtypes = ftype.get_argtypes().unwrap().as_vec();
        let src_type = ftype.get_rettype().unwrap();
        let fargs = self.transform_as_typed_args(&mut exprs, argtypes, args);
        let funcresult = self.transform_as_result(&mut exprs, func);
        exprs.extend(self.create_func_invoke(abi, funcresult, fargs));
        let last = exprs.pop().unwrap();
        let result = self.check_convert_to_trait(&mut exprs, &rettype, src_type, last);
        exprs.push(result);
        exprs
    }

    pub fn create_func_invoke(&mut self, abi: ABI, func: LLExpr, fargs: Vec<LLExpr>) -> Vec<LLExpr> {
        match abi {
            ABI::C => CFuncTransform::create_invoke(self, func, fargs),
            ABI::Molten | ABI::Unknown => ClosureTransform::create_invoke(self, func, fargs),
            _ => panic!("Not Implemented: {:?}", abi),
        }
    }

}



pub struct CFuncTransform;

impl CFuncTransform {
    pub fn transform_def_type(transform: &mut Transformer, args: &Vec<Type>, ret: &Type) -> LLType {
        let mut argtypes = vec!();
        for arg in args {
            argtypes.push(transform.transform_value_type(arg));
        }

        // TODO this is going to have to be special for returns, because sometimes a return value needs to be converted to a memory address argument
        let rettype = transform.transform_value_type(ret);

        LLType::Function(argtypes, r(rettype))
    }

    pub fn transform_decl(transform: &mut Transformer, defid: NodeID, _vis: Visibility, name: &str, _ttype: &Type) -> Vec<LLExpr> {
        let fname = transform.transform_func_name(name, defid);
        let ftype = transform.session.get_type(defid).unwrap();
        let (argtypes, rettype, _) = ftype.get_function_types().unwrap();
        let lftype = CFuncTransform::transform_def_type(transform, &argtypes.as_vec(), rettype);
        transform.add_global(LLGlobal::DeclCFunc(defid, fname, lftype, LLCC::CCC));
        vec!(LLExpr::GetValue(defid))
    }

    pub fn transform_def_args(transform: &mut Transformer, args: &Vec<Argument>) -> Vec<(NodeID, String)> {
        args.iter().map(|arg| (transform.session.get_ref(arg.id).unwrap(), arg.name.clone())).collect()
    }

    pub fn transform_def(transform: &mut Transformer, defid: NodeID, vis: Visibility, name: &str, args: &Vec<Argument>, body: &Vec<Expr>) -> Vec<LLExpr> {
        let fscope = transform.session.map.get(defid).unwrap();
        let fname = transform.transform_func_name(name, defid);

        let ftype = transform.session.get_type(defid).unwrap();
        let (argtypes, rettype, _) = ftype.get_function_types().unwrap();
        let lftype = CFuncTransform::transform_def_type(transform, &argtypes.as_vec(), rettype);
        transform.set_type(defid, lftype.clone());

        let fargs = CFuncTransform::transform_def_args(transform, args);

        transform.stack.push_scope(fscope);
        transform.with_context(CodeContext::Func(ABI::C, defid), |transform| {
            let llvis = transform.transform_vis(vis);
            let llbody = transform.visit_vec(body).unwrap();
            transform.add_global(LLGlobal::DefCFunc(defid, llvis, fname, lftype, fargs, llbody, LLCC::CCC));
        });
        transform.stack.pop_scope();
        vec!(LLExpr::GetValue(defid))
    }

    pub fn create_invoke(_transform: &mut Transformer, func: LLExpr, fargs: Vec<LLExpr>) -> Vec<LLExpr> {
        vec!(LLExpr::CallC(r(func), fargs, LLCC::CCC))
    }

}

pub struct ClosureTransform;

impl ClosureTransform {
    pub fn transform_def_type(transform: &mut Transformer, args: &Vec<Type>, ret: &Type) -> LLType {
        let ltype = CFuncTransform::transform_def_type(transform, args, ret);
        ClosureTransform::convert_to_def_type(ltype)
    }

    pub fn convert_to_def_type(ltype: LLType) -> LLType {
        match ltype {
            LLType::Function(mut args, ret) => {
                args.push(LLType::Ptr(r(LLType::I8)));
                args.push(LLType::Ptr(r(LLType::ExceptionPoint)));
                LLType::Function(args, ret)
            },
            _ => ltype
        }
    }

    pub fn transform_value_type(transform: &mut Transformer, args: &Vec<Type>, ret: &Type) -> LLType {
        let ltype = ClosureTransform::transform_def_type(transform, args, ret);
        ClosureTransform::convert_to_value_type(ltype)
    }

    pub fn convert_to_value_type(ltype: LLType) -> LLType {
        LLType::Struct(vec!(LLType::Ptr(r(ltype)), LLType::Ptr(r(LLType::I8))))
    }

    pub fn transform_raw_func_data(transform: &mut Transformer, id: NodeID, fname: &str, cfid: NodeID) -> (String, LLType) {
        let ftype = transform.session.get_type(id).unwrap();
        let (argtypes, rettype, _) = ftype.get_function_types().unwrap();
        let cftype = ClosureTransform::transform_def_type(transform, &argtypes.as_vec(), rettype);
        let cfname = format!("{}_func", fname);
        transform.set_type(cfid, cftype.clone());
        (cfname, cftype)
    }

    pub fn transform_decl(transform: &mut Transformer, defid: NodeID, vis: Visibility, name: &str, ttype: &Type) -> Vec<LLExpr> {
        let fname = transform.transform_func_name(name, defid);
        let ftype = transform.transform_value_type(ttype);

        ClosureTransform::make_definition(transform, defid, vis, fname, ftype, None)
    }

    pub fn convert_def_args(context_arg_id: NodeID, exp_id: NodeID, fargs: &mut Vec<(NodeID, String)>) {
        fargs.push((context_arg_id, String::from("__context__")));
        fargs.push((exp_id, String::from("__exception__")));
    }

    pub fn transform_def(transform: &mut Transformer, defid: NodeID, vis: Visibility, name: &str, args: &Vec<Argument>, body: &Vec<Expr>) -> Vec<LLExpr> {
        let scope = transform.stack.get_scope();
        let fscope = transform.session.map.get(defid).unwrap();
        let fname = transform.transform_func_name(name, defid);
        let cl = transform.session.get_def(defid).unwrap().as_closure().unwrap();

        // Add context argument to transformed arguments list
        let exp_id = NodeID::generate();
        let mut fargs = CFuncTransform::transform_def_args(transform, args);
        transform.with_scope(fscope.clone(), |_| {
            ClosureTransform::convert_def_args(cl.context_arg_id, exp_id, &mut fargs);
            Ok(vec!())
        }).unwrap();

        // We need the function type during the body transformation to convert closure context references
        transform.set_type(cl.compiled_func_id, LLType::Alias(cl.compiled_func_id));

        // Transforms body and create C function definition
        let index = transform.globals.len();
        let mut body = transform.with_context(CodeContext::Func(ABI::Molten, defid), |transform| {
            transform.with_exception(exp_id, |transform| {
                transform.with_scope(fscope, |transform| {
                    transform.visit_vec(body)
                }).unwrap()
            })
        });

        if let Ok(Def::TraitFunc(_)) = transform.session.get_def(defid) {
            transform.convert_impl_func_args(defid, &mut fargs, &mut body);
        }

        let (compiled_func_name, compiled_func_type) = ClosureTransform::transform_raw_func_data(transform, defid, &fname, cl.compiled_func_id);
        transform.insert_global(index, LLGlobal::DefType(cl.compiled_func_id, fname.clone(), compiled_func_type.clone()));

        let llvis = transform.transform_vis(vis);
        transform.insert_global(index, LLGlobal::DefCFunc(cl.compiled_func_id, llvis, compiled_func_name.clone(), compiled_func_type.clone(), fargs, body, LLCC::FastCC));

        let structtype = LLType::Ptr(r(StructTransform::get_type(transform, &cl.context_struct)));
        transform.insert_global(index, LLGlobal::DefType(cl.context_type_id, format!("__context_{}__", cl.context_type_id), structtype.clone()));


        let mut fields = vec!();
        cl.context_struct.foreach_field(|defid, field, _| {
            let rid = NodeID::generate();
            transform.session.set_ref(rid, defid);
            fields.push((field.to_string(), Expr::new_with_id(rid, Pos::empty(), ExprKind::Identifier(field.to_string()))));
        });

        let mut code = vec!();
        let did_context = NodeID::generate();
        let context_name = format!("{}_context", fname);
        // TODO if you could make this create and set the struct with the fptr and allocated context set before it then populates the context, then
        //      you wouldn't need the special recursive case in transform_reference.  It should be represented as an object, since the closure has a
        //      struct already, but it will take some refactoring first
        code.push(Expr::new_with_id(did_context, Pos::empty(), ExprKind::Definition(Mutability::Mutable, context_name.clone(), None, r(
            Expr::make_ref(Pos::empty(), Expr::make_record(Pos::empty(), fields))
        ))));

        binding::NameBinder::bind_names(transform.session, scope.clone(), &code);
        typecheck::TypeChecker::check(transform.session, scope.clone(), &code);
        let mut exprs = transform.visit_vec(&code).unwrap();
        let did_context_defid = transform.session.get_ref(did_context).unwrap();

        let did = NodeID::generate();
        exprs.push(ClosureTransform::make_closure_value(transform, did, cl.compiled_func_id, LLExpr::GetLocal(did_context_defid)));

        let lltype = ClosureTransform::convert_to_value_type(transform.get_type(cl.compiled_func_id).unwrap());
        exprs.extend(ClosureTransform::make_definition(transform, defid, vis, fname, lltype, Some(LLExpr::GetValue(did))));

        exprs
    }

    fn make_definition(transform: &mut Transformer, id: NodeID, vis: Visibility, name: String, ltype: LLType, value: Option<LLExpr>) -> Vec<LLExpr> {
        let mut exprs = vec!();

        if let Some(CodeContext::Import) = transform.get_context() {
            transform.add_global(LLGlobal::DefGlobal(id, LLLink::Public, name, ltype, false));
        } else if vis == Visibility::Public {
            transform.add_global(LLGlobal::DefGlobal(id, LLLink::Once, name, ltype, true));
            exprs.push(LLExpr::SetGlobal(id, r(value.unwrap())));
        } else {
            exprs.push(LLExpr::DefLocal(id, name, ltype, r(value.unwrap())));
        }

        exprs.push(LLExpr::GetLocal(id));
        exprs
    }

    pub fn make_closure_value(transform: &mut Transformer, id: NodeID, compiled_func_id: NodeID, context: LLExpr) -> LLExpr {
        let lltype = ClosureTransform::convert_to_value_type(transform.get_type(compiled_func_id).unwrap());
        LLExpr::DefStruct(id, lltype, vec!(
            LLExpr::GetValue(compiled_func_id), LLExpr::Cast(LLType::Ptr(r(LLType::I8)), r(context))
        ))
    }

    pub fn create_invoke(transform: &mut Transformer, func: LLExpr, mut fargs: Vec<LLExpr>) -> Vec<LLExpr> {
        let mut exprs = vec!();

        // Fetch the current exception value
        let exp_id = match transform.get_context() {
            Some(CodeContext::Func(ABI::C, _)) => transform.get_global_exception().unwrap(),
            _ => transform.get_exception().unwrap(),
        };

        // Evaluate the function expression and save it, so that we don't execute it twice when indexing
        let fobj_id = NodeID::generate();
        exprs.push(LLExpr::SetValue(fobj_id, r(func)));

        // Add the extra context and exception arguments
        fargs.push(LLExpr::Cast(LLType::Ptr(r(LLType::I8)), r(LLExpr::GetItem(r(LLExpr::GetValue(fobj_id)), 1))));
        fargs.push(LLExpr::GetValue(exp_id));

        exprs.push(LLExpr::CallC(r(LLExpr::GetItem(r(LLExpr::GetValue(fobj_id)), 0)), fargs, LLCC::FastCC));

        exprs
    }
}


