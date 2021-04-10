
use binding;
use typecheck;

use abi::ABI;
use types::Type;
use ast::{ Pos };
use hir::{ NodeID, Visibility, Mutability, Ident, Argument, Expr, ExprKind };
use visitor::{ Visitor };

use defs::functions::{ FuncDef };

use misc::{ r };
use transform::transform::{ Transformer, CodeContext, EXCEPTION_POINT_NAME };
use transform::classes::{ StructTransform };
use transform::llcode::{ LLType, LLLink, LLCC, LLExpr, LLGlobal };



impl<'sess> Transformer<'sess> {
    pub fn transform_vis(&mut self, vis: Visibility) -> LLLink {
        match vis {
            Visibility::Private => LLLink::Private,
            Visibility::Public | Visibility::Global => LLLink::Public,
        }
    }

    pub fn transform_func_name(&mut self, name: Option<&String>, id: NodeID) -> String {
        let scope = self.stack.get_scope();
        let ftype = self.session.get_type(id).unwrap();
        scope.get_full_name(name.map(|name| ftype.get_abi().unwrap_or(ABI::Molten).mangle_name(name, ftype.get_argtypes().unwrap(), 2)), id)
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

    pub fn transform_func_decl(&mut self, abi: ABI, id: NodeID, vis: Visibility, name: &String, ttype: &Type) -> Vec<LLExpr> {
        let defid = self.session.get_ref(id).unwrap();
        match abi {
            ABI::C => CFuncTransform::transform_decl(self, defid, vis, name, ttype),
            ABI::Molten | ABI::Unknown => ClosureTransform::transform_decl(self, defid, vis, name, ttype),
            _ => panic!("Not Implemented: {:?}", abi),
        }
    }

    pub fn transform_func_def(&mut self, abi: ABI, id: NodeID, vis: Visibility, name: Option<&String>, args: &Vec<Argument>, body: &Expr) -> Vec<LLExpr> {
        let defid = self.session.get_ref(id).unwrap();
        match abi {
            ABI::C => CFuncTransform::transform_def(self, defid, vis, name, args, body),
            ABI::Molten | ABI::Unknown => ClosureTransform::transform_def(self, defid, vis, name, args, body),
            _ => panic!("Not Implemented: {:?}", abi),
        }
    }

    pub fn transform_func_invoke(&mut self, abi: ABI, id: NodeID, func: &Expr, args: &Vec<Expr>) -> Vec<LLExpr> {
        match abi {
            ABI::C => CFuncTransform::transform_invoke(self, id, func, args),
            ABI::Molten | ABI::Unknown => ClosureTransform::transform_invoke(self, id, func, args),
            _ => panic!("Not Implemented: {:?}", abi),
        }
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

    pub fn transform_decl(transform: &mut Transformer, defid: NodeID, _vis: Visibility, name: &String, _ttype: &Type) -> Vec<LLExpr> {
        let fname = transform.transform_func_name(Some(name), defid);
        let ftype = transform.session.get_type(defid).unwrap();
        let (argtypes, rettype, _) = ftype.get_function_types().unwrap();
        let lftype = CFuncTransform::transform_def_type(transform, &argtypes.as_vec(), rettype);
        transform.add_global(LLGlobal::DeclCFunc(defid, fname, lftype, LLCC::CCC));
        vec!(LLExpr::GetValue(defid))
    }

    pub fn transform_def_args(transform: &mut Transformer, args: &Vec<Argument>) -> Vec<(NodeID, String)> {
        args.iter().map(|arg| (transform.session.get_ref(arg.id).unwrap(), arg.ident.name.clone())).collect()
    }

    pub fn transform_def(transform: &mut Transformer, defid: NodeID, vis: Visibility, name: Option<&String>, args: &Vec<Argument>, body: &Expr) -> Vec<LLExpr> {
        let fscope = transform.session.map.get(&defid);
        let fname = transform.transform_func_name(name, defid);

        let ftype = transform.session.get_type(defid).unwrap();
        let (argtypes, rettype, _) = ftype.get_function_types().unwrap();
        let lftype = CFuncTransform::transform_def_type(transform, &argtypes.as_vec(), rettype);
        transform.set_type(defid, lftype.clone());

        let fargs = CFuncTransform::transform_def_args(transform, args);

        transform.stack.push_scope(fscope.clone());
        transform.with_context(CodeContext::Func(ABI::C, defid), |transform| {
            let llvis = transform.transform_vis(vis);
            let llbody = transform.visit_node(body).unwrap();
            transform.add_global(LLGlobal::DefCFunc(defid, llvis, fname, lftype, fargs, llbody, LLCC::CCC));
        });
        transform.stack.pop_scope();
        vec!(LLExpr::GetValue(defid))
    }

    pub fn transform_invoke(transform: &mut Transformer, _id: NodeID, func: &Expr, args: &Vec<Expr>) -> Vec<LLExpr> {
        let mut exprs = vec!();

        let fargs = transform.transform_as_args(&mut exprs, args);

        let funcresult = transform.transform_as_result(&mut exprs, func).unwrap();

        exprs.extend(CFuncTransform::create_invoke(transform, funcresult, fargs));
        exprs
    }

    pub fn create_invoke(transform: &mut Transformer, func: LLExpr, fargs: Vec<LLExpr>) -> Vec<LLExpr> {
        vec!(LLExpr::CallC(r(func), fargs, LLCC::CCC))
    }

}

pub struct ClosureTransform;

impl ClosureTransform {
    pub fn transform_def_type(transform: &mut Transformer, args: &Vec<Type>, ret: &Type) -> LLType {
        let ltype = CFuncTransform::transform_def_type(transform, args, ret);
        ClosureTransform::convert_to_def_type(transform, ltype)
    }

    pub fn convert_to_def_type(transform: &mut Transformer, ltype: LLType) -> LLType {
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
        ClosureTransform::convert_to_value_type(transform, ltype)
    }

    pub fn convert_to_value_type(transform: &mut Transformer, ltype: LLType) -> LLType {
        LLType::Struct(vec!(LLType::Ptr(r(ltype)), LLType::Ptr(r(LLType::I8))))
    }

    pub fn convert_molten_type(transform: &mut Transformer, ttype: Type) -> Type {
        let scope = transform.stack.get_scope();
        match ttype {
            Type::Function(argtypes, rettype, _) => {
                let mut argtypes = argtypes.as_vec();
                argtypes.push(scope.find_type(transform.session, &String::from("String")).unwrap());
                argtypes.push(scope.find_type(transform.session, &String::from(EXCEPTION_POINT_NAME)).unwrap());
                Type::Function(r(Type::Tuple(argtypes)), rettype, ABI::C)
            },
            ttype @ _ => ttype,
        }
    }

    pub fn transform_raw_func_data(transform: &mut Transformer, id: NodeID, fname: &String, cfid: NodeID) -> (String, LLType) {
        let ftype = transform.session.get_type(id).unwrap();
        let (argtypes, rettype, _) = ftype.get_function_types().unwrap();
        let cftype = ClosureTransform::transform_def_type(transform, &argtypes.as_vec(), rettype);
        let cfname = format!("{}_func", fname);
        transform.set_type(cfid, cftype.clone());
        (cfname, cftype)
    }

    pub fn transform_decl(transform: &mut Transformer, defid: NodeID, _vis: Visibility, name: &String, ttype: &Type) -> Vec<LLExpr> {
        let fname = transform.transform_func_name(Some(name), defid);
        let ftype = transform.transform_value_type(ttype);

        let did = NodeID::generate();
        transform.add_global(LLGlobal::DefGlobal(did, LLLink::Once, fname, ftype));
        vec!(LLExpr::SetValue(defid, r(LLExpr::GetLocal(did))))
    }

    pub fn convert_def_args(transform: &mut Transformer, context_arg_id: NodeID, exp_id: NodeID, fargs: &mut Vec<(NodeID, String)>) {
        fargs.push((context_arg_id, String::from("__context__")));
        fargs.push((exp_id, String::from("__exception__")));
    }

    pub fn transform_def(transform: &mut Transformer, defid: NodeID, vis: Visibility, name: Option<&String>, args: &Vec<Argument>, body: &Expr) -> Vec<LLExpr> {
        let scope = transform.stack.get_scope();
        let fscope = transform.session.map.get(&defid);
        let fname = transform.transform_func_name(name, defid);
        let cl = transform.session.get_def(defid).unwrap().as_closure().unwrap();
        let (compiled_func_name, compiled_func_type) = ClosureTransform::transform_raw_func_data(transform, defid, &fname, cl.compiled_func_id);


        // Add context argument to transformed arguments list
        let exp_id = NodeID::generate();
        let mut fargs = CFuncTransform::transform_def_args(transform, args);
        transform.with_scope(fscope.clone(), |transform| {
            ClosureTransform::convert_def_args(transform, cl.context_arg_id, exp_id, &mut fargs);
            Ok(vec!())
        }).unwrap();

        let ptype = ClosureTransform::convert_molten_type(transform, transform.session.get_type(defid).unwrap());

        // Transforms body and create C function definition
        let index = transform.globals.len();
        let body = transform.with_context(CodeContext::Func(ABI::Molten, defid), |transform| {
            transform.with_exception(exp_id, |transform| {
                transform.with_scope(fscope, |transform| {
                    transform.visit_node(body)
                }).unwrap()
            })
        });
        let llvis = transform.transform_vis(vis);
        transform.insert_global(index, LLGlobal::DefCFunc(cl.compiled_func_id, llvis, compiled_func_name.clone(), compiled_func_type, fargs, body, LLCC::FastCC));

        let structtype = LLType::Ptr(r(StructTransform::get_type(transform, &cl.context_struct)));
        transform.insert_global(index, LLGlobal::DefType(cl.context_type_id, format!("__context_{}__", cl.context_type_id), structtype.clone()));


        FuncDef::define(transform.session, scope.clone(), cl.compiled_func_id, cl.vis, &Some(compiled_func_name.clone()), Some(ptype)).unwrap();
        let mut fields = vec!();
        cl.context_struct.foreach_field(|defid, field, _| {
            let rid = NodeID::generate();
            transform.session.set_ref(rid, defid);
            fields.push((Ident::from_str(field.as_str()), Expr::new_with_id(rid, Pos::empty(), ExprKind::Identifier(Ident::new(field.clone())))));
        });

        let mut code = vec!();
        let did_context = NodeID::generate();
        let context_name = format!("{}_context", fname);
        // TODO if you could make this create and set the struct with the fptr and allocated context set before it then populates the context, then
        //      you wouldn't need the special recursive case in transform_reference.  It should be represented as an object, since the closure has a
        //      struct already, but it will take some refactoring first
        code.push(Expr::new_with_id(did_context, Pos::empty(), ExprKind::Definition(Mutability::Mutable, Ident::new(context_name.clone()), None, r(
            if fields.len() > 0 {
                Expr::make_ref(Pos::empty(), Expr::make_record(Pos::empty(), fields))
            } else {
                Expr::make_nil()
            }
        ))));

        binding::NameBinder::bind_names(transform.session, scope.clone(), &code);
        typecheck::TypeChecker::check(transform.session, scope.clone(), &code);
        let mut exprs = transform.visit_vec(&code).unwrap();
        let did_context_defid = transform.session.get_ref(did_context).unwrap();

        exprs.push(ClosureTransform::make_closure_value(transform, defid, cl.compiled_func_id, LLExpr::GetLocal(did_context_defid)));
        exprs.push(LLExpr::GetValue(defid));

        if vis == Visibility::Public {
            let gid = NodeID::generate();
            transform.add_global(LLGlobal::DefGlobal(gid, LLLink::Once, fname.clone(), structtype));
            exprs.push(LLExpr::SetGlobal(gid, r(LLExpr::GetValue(defid))));
        }

        exprs
    }

    pub fn make_closure_value(transform: &mut Transformer, id: NodeID, compiled_func_id: NodeID, context: LLExpr) -> LLExpr {
        LLExpr::DefStruct(id, LLType::Struct(vec!(LLType::Ptr(r(transform.get_type(compiled_func_id).unwrap())), LLType::Ptr(r(LLType::I8)))), vec!(
            LLExpr::GetValue(compiled_func_id), context
        ))
    }

    pub fn transform_invoke(transform: &mut Transformer, _id: NodeID, func: &Expr, args: &Vec<Expr>) -> Vec<LLExpr> {
        let mut exprs = vec!();

        let fargs = transform.transform_as_args(&mut exprs, args);

        let fid = NodeID::generate();
        let funcresult = transform.transform_as_result(&mut exprs, func).unwrap();
        exprs.push(LLExpr::SetValue(fid, r(funcresult)));

        exprs.extend(ClosureTransform::create_invoke(transform, LLExpr::GetValue(fid), fargs));
        exprs
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


