
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
use defs::classes::{ Define };

use misc::{ r };
use transform::transform::{ Transformer, CodeContext, EXCEPTION_POINT_NAME };
use transform::llcode::{ LLType, LLLit, LLRef, LLCmpType, LLLink, LLCC, LLExpr, LLGlobal };



impl<'a> Transformer<'a> {
    pub fn transform_vis(&mut self, vis: Visibility) -> LLLink {
        match vis {
            Visibility::Private => LLLink::Private,
            Visibility::Public => LLLink::Public,
        }
    }

    pub fn transform_func_name(&mut self, name: Option<&String>, id: NodeID) -> String {
        let scope = self.stack.get_scope();
        let ftype = self.session.get_type(id).unwrap();
        scope.get_full_name(name.map(|name| ftype.get_abi().unwrap_or(ABI::Molten).mangle_name(name, ftype.get_argtypes().unwrap(), 2)), id)
    }

    pub fn transform_func_def_type(&mut self, abi: ABI, args: &Vec<Type>, ret: &Type) -> LLType {
        match abi {
            ABI::C | ABI::MoltenFunc => CFuncTransform::transform_def_type(self, args, ret),
            ABI::Molten | ABI::Unknown => ClosureTransform::transform_def_type(self, args, ret),
            _ => panic!("Not Implemented: {:?}", abi),
        }
    }

    pub fn transform_func_value_type(&mut self, abi: ABI, args: &Vec<Type>, ret: &Type) -> LLType {
        match abi {
            ABI::C | ABI::MoltenFunc => LLType::Ptr(r(CFuncTransform::transform_def_type(self, args, ret))),
            ABI::Molten | ABI::Unknown => ClosureTransform::transform_value_type(self, args, ret),
            _ => panic!("Not Implemented: {:?}", abi),
        }
    }

    pub fn transform_func_decl(&mut self, abi: ABI, id: NodeID, vis: Visibility, name: &String, ttype: &Type) -> Vec<LLExpr> {
        let defid = self.session.get_ref(id).unwrap();
        match abi {
            ABI::C | ABI::MoltenFunc => CFuncTransform::transform_decl(self, defid, vis, name, ttype),
            ABI::Molten | ABI::Unknown => ClosureTransform::transform_decl(self, defid, vis, name, ttype),
            _ => panic!("Not Implemented: {:?}", abi),
        }
    }

    pub fn transform_func_def(&mut self, abi: ABI, id: NodeID, vis: Visibility, name: Option<&String>, args: &Vec<Argument>, body: &Expr) -> Vec<LLExpr> {
        let defid = self.session.get_ref(id).unwrap();
        match abi {
            ABI::C | ABI::MoltenFunc => CFuncTransform::transform_def(self, defid, vis, name, args, body),
            ABI::Molten | ABI::Unknown => ClosureTransform::transform_def(self, defid, vis, name, args, body),
            _ => panic!("Not Implemented: {:?}", abi),
        }
    }

    pub fn transform_func_invoke(&mut self, abi: ABI, id: NodeID, func: &Expr, args: &Vec<Expr>) -> Vec<LLExpr> {
        match abi {
            ABI::C | ABI::MoltenFunc => CFuncTransform::transform_invoke(self, id, func, args),
            ABI::Molten | ABI::Unknown => ClosureTransform::transform_invoke(self, id, func, args),
            _ => panic!("Not Implemented: {:?}", abi),
        }
    }

    pub fn create_func_invoke(&mut self, abi: ABI, func: LLExpr, fargs: Vec<LLExpr>) -> Vec<LLExpr> {
        match abi {
            ABI::C | ABI::MoltenFunc => CFuncTransform::create_invoke(self, func, fargs),
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
            let llbody = transform.transform_node(body);
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


pub struct MFuncTransform;

impl MFuncTransform {
    pub fn transform_def_type(transform: &mut Transformer, args: &Vec<Type>, ret: &Type) -> LLType {
        let ltype = CFuncTransform::transform_def_type(transform, args, ret);
        MFuncTransform::convert_to_def_type(transform, ltype)
    }

    pub fn convert_to_def_type(transform: &mut Transformer, ltype: LLType) -> LLType {
        match ltype {
            LLType::Function(mut args, ret) => {
                args.push(LLType::Ptr(r(LLType::ExceptionPoint)));
                //args.push(LLType::Ptr(r(LLType::I8)));
                LLType::Function(args, ret)
            },
            _ => ltype
        }
    }

    pub fn convert_molten_type(transform: &mut Transformer, ttype: Type) -> Type {
        let scope = transform.stack.get_scope();
        match ttype {
            Type::Function(argtypes, rettype, _) => {
                let mut argtypes = argtypes.as_vec();
                argtypes.push(scope.find_type(transform.session, &String::from(EXCEPTION_POINT_NAME)).unwrap());
                Type::Function(r(Type::Tuple(argtypes)), rettype, ABI::C)
            },
            ttype @ _ => ttype,
        }
    }

    pub fn convert_def_args(transform: &mut Transformer, exp_id: NodeID, fargs: &mut Vec<(NodeID, String)>) {
        fargs.push((exp_id, String::from("__exception__")));
    }

    pub fn transform_def(transform: &mut Transformer, defid: NodeID, vis: Visibility, name: Option<&String>, args: &Vec<Argument>, body: &Expr) -> Vec<LLExpr> {
        let fscope = transform.session.map.get(&defid);
        let fname = transform.transform_func_name(name, defid);

        let ftype = transform.session.get_type(defid).unwrap();
        let (argtypes, rettype, _) = ftype.get_function_types().unwrap();
        let lftype = MFuncTransform::transform_def_type(transform, &argtypes.as_vec(), rettype);
        transform.set_type(defid, lftype.clone());

        let exp_id = NodeID::generate();
        let mut fargs = CFuncTransform::transform_def_args(transform, args);
        MFuncTransform::convert_def_args(transform, exp_id, &mut fargs);

        transform.with_context(CodeContext::Func(ABI::MoltenFunc, defid), |transform| {
            transform.with_exception(exp_id, |transform| {
                transform.with_scope(fscope, |transform| {
                    let llvis = transform.transform_vis(vis);
                    let llbody = transform.transform_node(body);
                    transform.add_global(LLGlobal::DefCFunc(defid, llvis, fname, lftype, fargs, llbody, LLCC::FastCC));
                });
            });
        });
        vec!(LLExpr::GetValue(defid))
    }

    pub fn transform_invoke(transform: &mut Transformer, _id: NodeID, func: &Expr, args: &Vec<Expr>) -> Vec<LLExpr> {
        let mut exprs = vec!();

        let fargs = transform.transform_as_args(&mut exprs, args);

        let funcresult = transform.transform_as_result(&mut exprs, func).unwrap();

        exprs.extend(MFuncTransform::create_invoke(transform, funcresult, fargs));
        exprs
    }

    pub fn create_invoke(transform: &mut Transformer, func: LLExpr, mut fargs: Vec<LLExpr>) -> Vec<LLExpr> {
        fargs.push(LLExpr::GetValue(transform.get_exception().unwrap()));
        vec!(LLExpr::CallC(r(func), fargs, LLCC::FastCC))
        //CFuncTransform::create_invoke(transform, func, fargs)
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
                MFuncTransform::convert_to_def_type(transform, LLType::Function(args, ret))
            },
            _ => ltype
        }
    }

    pub fn transform_value_type(transform: &mut Transformer, args: &Vec<Type>, ret: &Type) -> LLType {
        let ltype = ClosureTransform::transform_def_type(transform, args, ret);
        ClosureTransform::convert_to_value_type(transform, ltype)
    }

    pub fn convert_to_value_type(transform: &mut Transformer, ltype: LLType) -> LLType {
        LLType::Ptr(r(LLType::Struct(vec!(LLType::Ptr(r(ltype))))))
    }

    pub fn convert_molten_type(transform: &mut Transformer, ttype: Type) -> Type {
        let scope = transform.stack.get_scope();
        match ttype {
            Type::Function(argtypes, rettype, _) => {
                let mut argtypes = argtypes.as_vec();
                argtypes.push(scope.find_type(transform.session, &String::from("String")).unwrap());
                MFuncTransform::convert_molten_type(transform, Type::Function(r(Type::Tuple(argtypes)), rettype, ABI::C))
            },
            ttype @ _ => ttype,
        }
    }

    pub fn transform_raw_func_data(transform: &mut Transformer, id: NodeID, fname: &String) -> (NodeID, String, LLType) {
        let ftype = transform.session.get_type(id).unwrap();
        let (argtypes, rettype, _) = ftype.get_function_types().unwrap();
        let cftype = ClosureTransform::transform_def_type(transform, &argtypes.as_vec(), rettype);
        let cfid = NodeID::generate();
        let cfname = format!("{}_func", fname);
        transform.set_type(cfid, cftype.clone());
        (cfid, cfname, cftype)
    }

    pub fn transform_decl(transform: &mut Transformer, defid: NodeID, _vis: Visibility, name: &String, ttype: &Type) -> Vec<LLExpr> {
        let fname = transform.transform_func_name(Some(name), defid);
        let ftype = transform.transform_value_type(ttype);

        let did = NodeID::generate();
        transform.add_global(LLGlobal::DefGlobal(did, LLLink::Once, fname, ftype));
        vec!(LLExpr::SetValue(defid, r(LLExpr::GetLocal(did))))
    }

    pub fn convert_def_args(transform: &mut Transformer, cl: ClosureDefRef, exp_id: NodeID, fargs: &mut Vec<(NodeID, String)>) {
        fargs.push((cl.context_arg_id, String::from("__context__")));
        MFuncTransform::convert_def_args(transform, exp_id, fargs);
    }

    pub fn transform_def(transform: &mut Transformer, defid: NodeID, vis: Visibility, name: Option<&String>, args: &Vec<Argument>, body: &Expr) -> Vec<LLExpr> {
        let scope = transform.stack.get_scope();
        let fscope = transform.session.map.get(&defid);
        let fname = transform.transform_func_name(name, defid);
        let (cfid, cfname, cftype) = ClosureTransform::transform_raw_func_data(transform, defid, &fname);

        let cl = transform.session.get_def(defid).unwrap().as_closure().unwrap();

        // Add context argument to transformed arguments list
        let exp_id = NodeID::generate();
        let mut fargs = CFuncTransform::transform_def_args(transform, args);
        transform.with_scope(fscope.clone(), |transform| {
            ClosureTransform::convert_def_args(transform, cl.clone(), exp_id, &mut fargs);
        });

        let ptype = ClosureTransform::convert_molten_type(transform, transform.session.get_type(defid).unwrap());
        cl.add_field(transform.session, cfid, "__func__", ptype.clone(), Define::Never);

        // Transforms body and create C function definition
        let index = transform.globals.len();
        let body = transform.with_context(CodeContext::Func(ABI::Molten, defid), |transform| {
            transform.with_exception(exp_id, |transform| {
                transform.with_scope(fscope, |transform| {
                    transform.transform_node(body)
                })
            })
        });
        let llvis = transform.transform_vis(vis);
        transform.insert_global(index, LLGlobal::DefCFunc(cfid, llvis, cfname.clone(), cftype, fargs, body, LLCC::FastCC));

        let structtype = LLType::Ptr(r(transform.transform_struct_def(&cl.context_struct)));
        transform.insert_global(index, LLGlobal::DefType(cl.context_type_id, format!("__context_{}__", cl.context_type_id), structtype.clone()));


        FuncDef::define(transform.session, scope.clone(), cfid, cl.vis, &Some(cfname.clone()), Some(ptype)).unwrap();
        let mut fields = vec!();
        cl.context_struct.foreach_field(|defid, field, _| {
            let rid = NodeID::generate();
            transform.session.set_ref(rid, defid);
            if field.as_str() == "__func__" {
                fields.push((Ident::from_str("__func__"), Expr::new_with_id(rid, Pos::empty(), ExprKind::Identifier(Ident::new(cfname.clone())))));
            } else {
                fields.push((Ident::from_str(field.as_str()), Expr::new_with_id(rid, Pos::empty(), ExprKind::Identifier(Ident::new(field.clone())))));
            }
        });

        let mut code = vec!();
        let did = NodeID::generate();
        code.push(Expr::new_with_id(did, Pos::empty(), ExprKind::Definition(Mutability::Mutable, Ident::new(fname.clone()), None, r(Expr::make_ref(Pos::empty(), Expr::make_record(Pos::empty(), fields))))));
        // TODO I'm going back on my decision to use a tuple pair to represent the function and context reference because it can't be converted to i8* (the generics type)
        //      Once I have generics that can operate on different sized data instead of only references, I can switch back
        //code.push(Expr::new_with_id(NodeID::generate(), Pos::empty(), ExprKind::Tuple(vec!(Expr::make_ident_from_str(Pos::empty(), real_fname.as_str()), Expr::make_ident(Pos::empty(), Ident::new(cname.clone()))))));

        binding::NameBinder::bind_names(transform.session, scope.clone(), &code);
        typecheck::TypeChecker::check(transform.session, scope.clone(), &code);
        let mut exprs = transform.transform_vec(&code);
        let did_defid = transform.session.get_ref(did).unwrap();

        exprs.push(LLExpr::SetValue(defid, r(LLExpr::GetLocal(did_defid))));
        exprs.push(LLExpr::GetValue(defid));

        if vis == Visibility::Public {
            let gid = NodeID::generate();
            transform.add_global(LLGlobal::DefGlobal(gid, LLLink::Once, fname.clone(), structtype));
            exprs.push(LLExpr::SetGlobal(gid, r(LLExpr::GetLocal(did_defid))));
        }

        exprs
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

        fargs.push(LLExpr::Cast(LLType::Ptr(r(LLType::I8)), r(func.clone())));

        let function = LLExpr::LoadRef(r(LLExpr::AccessRef(r(func), vec!(LLRef::Field(0)))));
        exprs.extend(MFuncTransform::create_invoke(transform, function, fargs));
        exprs
    }
}


