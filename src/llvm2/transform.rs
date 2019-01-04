
use std::cell::RefCell;
use std::collections::HashMap;


use binding;
use typecheck;

use abi::ABI;
use defs::Def;
use types::Type;
use session::Session;
use scope::{ Scope, ScopeRef };
use ast::{ NodeID, Pos, Literal, Ident, ClassSpec, Argument, Pattern, AST };

use defs::functions::FuncDef;
use defs::classes::{ ClassDefRef, StructDefRef, Define, Vtable };

use llvm2::llcode::{ r, LLABI, LLType, LLLit, LLRef, LLExpr, LLGlobal };
//use llvm2::lib::{ BuiltinDef, initialize_builtins };



#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CodeContext {
    CFunc,
    ClassBody,
    Closure(NodeID),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Transformer<'sess> {
    pub session: &'sess Session,
    pub globals: RefCell<Vec<LLGlobal>>,
    pub context: RefCell<Vec<CodeContext>>,
    pub types: RefCell<HashMap<NodeID, LLType>>,
}

impl<'sess> Transformer<'sess> {
    pub fn new(session: &'sess Session) -> Self {
        Transformer {
            session: session,
            globals: RefCell::new(vec!()),
            context: RefCell::new(vec!()),
            types: RefCell::new(HashMap::new()),
        }
    }

    pub fn initialize(&self) {
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
        self.context.borrow_mut().last().map(|c| *c)
    }

    pub fn transform_code(&self, scope: ScopeRef, code: &Vec<AST>) {
        let mut toplevel = self.transform_vec(scope.clone(), &code);

        toplevel.push(LLExpr::Literal(LLLit::I64(0)));

        let mainid = NodeID::generate();
        let ftype = LLType::Function(vec!(), r(LLType::I64), LLABI::C);
        self.set_type(mainid, ftype.clone());

        self.add_global(LLGlobal::DefCFunc(mainid, String::from("main"), ftype, vec!(), toplevel));
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
            AST::Import(_, _, _, decls) => self.transform_vec(scope.clone(), decls),


            AST::Function(id, _, ident, args, _, body, abi) => {
                self.transform_func_def(scope.clone(), *abi, *id, ident.as_ref().map(|ident| &ident.name), args, body)
            },

            AST::Declare(id, _, ident, ttype) => {
                let abi = self.session.get_type(*id).unwrap().get_abi().unwrap();
                self.transform_func_decl(scope.clone(), abi, *id, &ident.name, ttype)
            },

            AST::Invoke(id, _, func, args) => {
                let abi = self.session.get_type(*id).unwrap().get_abi().unwrap();
                self.transform_func_invoke(scope.clone(), abi, func, args)
            },

            AST::Definition(id, _, mutable, ident, _, value) => {
                self.transform_def_local(scope.clone(), *id, &ident.name, value)
            },

            AST::Identifier(id, _, ident) => {
                let defid = self.session.get_ref(*id).unwrap();
                self.transform_reference(scope.clone(), defid, &ident.name)
            },


            AST::Ref(id, _, value) => {
                self.transform_alloc_ref(scope.clone(), *id, value)
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


            AST::New(id, _, _) => {
                self.transform_new_object(scope.clone(), *id)
            },

            AST::Class(id, _, _, _, body) => {
                let mut exprs = self.transform_class_body(scope.clone(), *id, body);
                exprs.push(LLExpr::Literal(self.transform_lit(&Literal::Unit)));
                exprs
            },

            AST::Resolver(id, _, _, _) => {
                // TODO this is temporary and due to the fact that we're accessing a local def containing the closure
                vec!(LLExpr::LoadRef(r(LLExpr::GetValue(self.session.get_ref(*id).unwrap()))))
                //self.transform_reference(scope.clone(), self.session.get_ref(*id).unwrap(), &String::from(""))
            },

            AST::PtrCast(ttype, node) => {
                let mut exprs = vec!();
                let ltype = self.transform_value_type(ttype);
                let result = self.transform_as_result(&mut exprs, scope.clone(), node).unwrap();
                exprs.push(LLExpr::Cast(ltype, r(result)));
                exprs
            },

            AST::Accessor(id, _, obj, ident, oid) => {
                let otype = self.session.get_type(*oid).unwrap();
                self.transform_accessor(scope.clone(), *id, obj, &ident.name, otype)
            },

            AST::Assignment(id, _, left, right) => {
                self.transform_assignment(scope.clone(), *id, left, right)
            },


            AST::If(_, _, cond, texpr, fexpr) => {
                self.transform_if_expr(scope.clone(), cond, texpr, fexpr)
            },

            AST::Match(_, _, cond, cases, compid) => {
                self.transform_match(scope.clone(), cond, cases, *compid)
            },

            AST::SideEffect(_, _, ident, args) => {
                self.transform_side_effect(scope.clone(), ident.name.as_str(), args)
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
            _ => panic!("Not Implemented: {:?}", lit),
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

    fn transform_def_local(&self, scope: ScopeRef, id: NodeID, name: &String, value: &AST) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let valexpr = self.transform_as_result(&mut exprs, scope.clone(), value).unwrap();
        let ltype = self.transform_value_type(&self.session.get_type(id).unwrap());
        exprs.push(LLExpr::DefLocal(id, name.clone(), ltype, r(valexpr)));
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

    fn transform_func_decl(&self, scope: ScopeRef, abi: ABI, id: NodeID, name: &String, ttype: &Type) -> Vec<LLExpr> {
        match abi {
            ABI::C | ABI::MoltenFunc => self.transform_cfunc_decl(scope.clone(), id, name, ttype),
            ABI::Molten | ABI::Unknown => self.transform_closure_decl(scope.clone(), id, name, ttype),
            _ => panic!("Not Implemented: {:?}", abi),
        }
    }

    pub fn transform_func_def(&self, scope: ScopeRef, abi: ABI, id: NodeID, name: Option<&String>, args: &Vec<Argument>, body: &AST) -> Vec<LLExpr> {
        match abi {
            ABI::C | ABI::MoltenFunc => self.transform_cfunc_def(scope.clone(), id, name, args, body),
            ABI::Molten | ABI::Unknown => self.transform_closure_def(scope.clone(), id, name, args, body),
            _ => panic!("Not Implemented: {:?}", abi),
        }
    }

    fn transform_func_invoke(&self, scope: ScopeRef, abi: ABI, func: &AST, args: &Vec<AST>) -> Vec<LLExpr> {
        match abi {
            ABI::C | ABI::MoltenFunc => self.transform_cfunc_invoke(scope.clone(), func, args),
            ABI::Molten | ABI::Unknown => self.transform_closure_invoke(scope.clone(), func, args),
            _ => panic!("Not Implemented: {:?}", abi),
        }
    }

    fn create_func_invoke(&self, abi: ABI, func: LLExpr, mut fargs: Vec<LLExpr>) -> Vec<LLExpr> {
        match abi {
            ABI::C | ABI::MoltenFunc => self.create_cfunc_invoke(func, fargs),
            ABI::Molten | ABI::Unknown => self.create_closure_invoke(func, fargs),
            _ => panic!("Not Implemented: {:?}", abi),
        }
    }

    fn transform_func_as_result(&self, exprs: &mut Vec<LLExpr>, scope: ScopeRef, func: &AST, fargs: &mut Vec<LLExpr>) -> LLExpr {
        match func {
            AST::Accessor(id, _, obj, ident, oid) => {
                let otype = self.session.get_type(*oid).unwrap();
                match otype {
                    Type::Object(_, _, _) => {
                        // Convert method-style calls into 
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



    pub fn transform_cfunc_def_type(&self, args: &Vec<Type>, ret: &Type) -> LLType {
        let mut argtypes = vec!();
        for arg in args {
            argtypes.push(self.transform_value_type(arg));
        }

        // TODO this is going to have to be special for returns, because sometimes a return value needs to be converted to a memory address argument
        let rettype = self.transform_value_type(ret);

        LLType::Function(argtypes, r(rettype), LLABI::C)
    }

    fn transform_cfunc_decl(&self, scope: ScopeRef, id: NodeID, name: &String, ttype: &Type) -> Vec<LLExpr> {
        let fname = self.transform_func_name(scope.clone(), Some(name), id);
        let ftype = self.session.get_type(id).unwrap();
        let (argtypes, rettype, abi) = ftype.get_function_types().unwrap();
        let lftype = self.transform_cfunc_def_type(&argtypes.as_vec(), rettype);
        self.add_global(LLGlobal::DeclCFunc(id, fname, lftype));
        vec!(LLExpr::GetValue(id))
    }

    fn transform_cfunc_def(&self, scope: ScopeRef, id: NodeID, name: Option<&String>, args: &Vec<Argument>, body: &AST) -> Vec<LLExpr> {
        let fscope = self.session.map.get(&id);
        let fname = self.transform_func_name(scope.clone(), name, id);
        let fargs = args.iter().map(|arg| (arg.id, arg.ident.name.clone())).collect();
        let ftype = self.session.get_type(id).unwrap();
        let (argtypes, rettype, abi) = ftype.get_function_types().unwrap();
        let lftype = self.transform_cfunc_def_type(&argtypes.as_vec(), rettype);
        self.set_type(id, lftype.clone());

        self.set_context(CodeContext::CFunc);
        self.add_global(LLGlobal::DefCFunc(id, fname, lftype, fargs, self.transform_node(fscope.clone(), body)));
        self.restore_context();
        vec!(LLExpr::GetValue(id))
    }

    fn transform_cfunc_invoke(&self, scope: ScopeRef, func: &AST, args: &Vec<AST>) -> Vec<LLExpr> {
        let mut exprs = vec!();

        let mut fargs = self.transform_as_args(&mut exprs, scope.clone(), args);

        let funcresult = self.transform_func_as_result(&mut exprs, scope.clone(), func, &mut fargs);

        exprs.extend(self.create_cfunc_invoke(funcresult, fargs));
        exprs
    }

    fn create_cfunc_invoke(&self, func: LLExpr, mut fargs: Vec<LLExpr>) -> Vec<LLExpr> {
        vec!(LLExpr::CallC(r(func), fargs))
    }



    fn transform_closure_def_type(&self, args: &Vec<Type>, ret: &Type) -> LLType {
        let ltype = self.transform_cfunc_def_type(args, ret);
        self.convert_to_closure_def_type(ltype)
    }

    fn convert_to_closure_def_type(&self, ltype: LLType) -> LLType {
        match ltype {
            LLType::Function(mut args, ret, abi) => {
                args.push(LLType::Ptr(r(LLType::I8)));
                LLType::Function(args, ret, abi)
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

    fn create_closure_type(&self, scope: ScopeRef, ttype: Type) -> Type {
        match ttype {
            Type::Function(mut argtypes, rettype, abi) => {
                let mut argtypes = argtypes.as_vec();
                argtypes.push(scope.find_type(self.session, &String::from("String")).unwrap());
                Type::Function(Box::new(Type::Tuple(argtypes)), rettype, ABI::C)
            },
            ttype @ _ => ttype,
        }
    }

    fn transform_closure_decl(&self, scope: ScopeRef, id: NodeID, name: &String, ttype: &Type) -> Vec<LLExpr> {
        let fname = self.transform_func_name(scope.clone(), Some(name), id);
        let ftype = self.session.get_type(id).unwrap();
        let (argtypes, rettype, abi) = ftype.get_function_types().unwrap();
        let cftype = self.transform_closure_def_type(&argtypes.as_vec(), rettype);
        let cfid = NodeID::generate();
        let cfname = format!("{}_func", fname);
        self.set_type(cfid, cftype.clone());
        self.add_global(LLGlobal::DeclCFunc(cfid, cfname, cftype.clone()));

        let atype = self.convert_to_closure_value_type(cftype);
        let stype = atype.get_element();
        let aexpr = LLExpr::AllocRef(NodeID::generate(), atype.clone(), Some(r(LLExpr::DefStruct(id, stype, vec!(LLExpr::GetValue(cfid))))));
        vec!(LLExpr::DefLocal(id, fname.clone(), atype, r(aexpr)))
    }

    fn transform_closure_def(&self, scope: ScopeRef, id: NodeID, name: Option<&String>, args: &Vec<Argument>, body: &AST) -> Vec<LLExpr> {
        let fscope = self.session.map.get(&id);
        let fname = self.transform_func_name(scope.clone(), name, id);
        let cl = self.session.get_def(id).unwrap().as_closure().unwrap();
        let cfid = NodeID::generate();
        let cfname = format!("{}_func", fname);

        // Add context argument to transformed arguments list
        let mut fargs: Vec<(NodeID, String)> = args.iter().map(|arg| (arg.id, arg.ident.name.clone())).collect();
        fargs.push((cl.context_arg_id, String::from("__context__")));

        // Transform function type and add context argument
        let ftype = self.session.get_type(id).unwrap();
        let (argtypes, rettype, abi) = ftype.get_function_types().unwrap();
        let lftype = self.transform_closure_def_type(&argtypes.as_vec(), rettype);
        self.set_type(cfid, lftype.clone());

        let ptype = self.create_closure_type(scope.clone(), self.session.get_type(id).unwrap());
        cl.add_field(self.session, cfid, "__func__", ptype.clone(), Define::Never);

        // Transforms body and create C function definition
        let index = self.globals.borrow().len();
        self.set_context(CodeContext::Closure(id));
        let body = self.transform_node(fscope.clone(), body);
        self.restore_context();
        self.insert_global(index, LLGlobal::DefCFunc(cfid, cfname.clone(), lftype, fargs, body));

        let structtype = LLType::Ptr(r(self.transform_struct_def(&cl.context_struct)));
        self.insert_global(index, LLGlobal::DefType(cl.context_type_id, format!("__context_{}__", cl.context_type_id), structtype));


        FuncDef::define(self.session, scope.clone(), cfid, &Some(cfname.clone()), Some(ptype)).unwrap();
        let mut fields = vec!();
        cl.context_struct.foreach_field(|_, field, _| {
            if field.as_str() == "__func__" {
                fields.push((Ident::from_str("__func__"), AST::make_ident_from_str(Pos::empty(), cfname.as_str())));
            } else {
                fields.push((Ident::from_str(field.as_str()), AST::make_ident_from_str(Pos::empty(), field.as_str())));
            }
        });

        let mut code = vec!();
        code.push(AST::Definition(NodeID::generate(), Pos::empty(), true, Ident::new(fname.clone()), None, Box::new(AST::make_ref(Pos::empty(), AST::make_record(Pos::empty(), fields)))));
        // TODO I'm going back on my decision to use a tuple pair to represent the function and context reference because it can't be converted to i8* (the generics type)
        //      Once I have generics that can operate on different sized data instead of only references, I can switch back
        //code.push(AST::Tuple(NodeID::generate(), Pos::empty(), vec!(AST::make_ident_from_str(Pos::empty(), real_fname.as_str()), AST::make_ident(Pos::empty(), Ident::new(cname.clone())))));
        code.push(AST::make_ident(Pos::empty(), Ident::new(fname.clone())));
        println!("CLOSURE CREATION CODE {:?}: {:#?}", id, code);

        binding::bind_names(self.session, scope.clone(), &mut code);
        typecheck::check_types(self.session, scope.clone(), &code);
        let mut exprs = self.transform_vec(scope.clone(), &code);
        let last = exprs.pop().unwrap();
        exprs.push(LLExpr::SetValue(id, r(last)));
        exprs.push(LLExpr::GetValue(id));

        exprs
    }

    fn transform_closure_invoke(&self, scope: ScopeRef, func: &AST, args: &Vec<AST>) -> Vec<LLExpr> {
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
        exprs.push(LLExpr::CallC(r(function), fargs));
        exprs
    }



    fn transform_reference(&self, scope: ScopeRef, defid: NodeID, name: &String) -> Vec<LLExpr> {
        let dscope = Scope::target(self.session, scope.clone());
        if !dscope.contains_local(name) && !Scope::global(dscope.clone()).contains(name) {
            match self.get_context() {
                Some(CodeContext::Closure(ref cid)) => {
                    let cl = self.session.get_def(*cid).unwrap().as_closure().unwrap();
                    let index = cl.find_or_add_field(self.session, defid, name.as_str(), self.session.get_type(defid).unwrap());
                    let context = LLExpr::Cast(LLType::Alias(cl.context_type_id), r(LLExpr::GetValue(cl.context_arg_id)));
                    vec!(LLExpr::LoadRef(r(LLExpr::AccessRef(r(context), vec!(LLRef::Field(index))))))
                },
                _ => panic!("Cannot access variable outside of scope: {:?}", name),
            }
        } else {
            match self.session.get_def(defid) {
                Ok(Def::Var(_)) => vec!(LLExpr::GetLocal(defid)),
                Ok(_) => vec!(LLExpr::GetValue(defid)),
                Err(_) => panic!("TransformError: attempting to reference a non-existent value"),
            }
        }
    }



    fn transform_alloc_ref(&self, scope: ScopeRef, id: NodeID, value: &AST) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let valexpr = self.transform_as_result(&mut exprs, scope.clone(), value).unwrap();
        let ltype = self.transform_value_type(&self.session.get_type(id).unwrap());
        exprs.push(LLExpr::AllocRef(id, ltype, Some(r(valexpr))));
        exprs
    }

    fn transform_new_object(&self, scope: ScopeRef, id: NodeID) -> Vec<LLExpr> {
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

        self.add_global(LLGlobal::DefNamedStruct(classdef.id, classdef.classname.clone()));
        self.set_type(classdef.id, LLType::Alias(classdef.id));
        self.add_global(LLGlobal::DefNamedStruct(classdef.vtable.id, format!("{}_vtable", classdef.classname.clone())));
        self.set_type(classdef.vtable.id, LLType::Alias(classdef.vtable.id));

        let stype = self.transform_struct_def(&classdef.structdef);
        self.add_global(LLGlobal::SetStructBody(classdef.id, stype.get_items()));

        let vtype = self.transform_vtable_def(&classdef.vtable);
        self.add_global(LLGlobal::SetStructBody(classdef.vtable.id, vtype.get_items()));
    }

    fn transform_vtable_def(&self, vtable: &Vtable) -> LLType {
        let mut items = vec!();
        vtable.foreach_entry(|id, name, ttype| {
            items.push(self.transform_value_type(ttype));
        });
        LLType::Struct(items)
    }

    fn transform_vtable_init(&self, scope: ScopeRef, classdef: ClassDefRef) -> Vec<LLExpr> {
        if !classdef.has_vtable() {
            return vec!();
        }

        let mut exprs = vec!();
        let tscope = self.session.map.get(&classdef.id);

        self.add_global(LLGlobal::DefGlobal(classdef.vtable.id, format!("__{}_vtable", tscope.get_basename()), self.get_type(classdef.vtable.id).unwrap()));
        // TODO should vtables be dynamically allocated, or should we add a LLType::ElementOf() type or something to GetElement an aliased type
        exprs.push(LLExpr::SetGlobal(classdef.vtable.id, r(LLExpr::AllocRef(NodeID::generate(), self.get_type(classdef.vtable.id).unwrap(), None))));
        classdef.vtable.foreach_enumerated(|i, id, name, ttype| {
            let ltype = self.transform_value_type(ttype);
            let field = LLExpr::AccessRef(r(LLExpr::GetGlobal(classdef.vtable.id)), vec!(LLRef::Field(i)));
            exprs.push(LLExpr::StoreRef(r(field), r(LLExpr::Cast(ltype, r(LLExpr::GetLocal(id))))));
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
                AST::Function(id, _, ident, args, _, body, abi) => {
                    exprs.extend(self.transform_func_def(tscope.clone(), *abi, *id, ident.as_ref().map(|ident| &ident.name), args, body));
                },
                AST::Declare(id, _, ident, ttype) => {
                    exprs.extend(self.transform_func_decl(tscope.clone(), ttype.get_abi().unwrap(), *id, &ident.name, ttype));
                },
                AST::Definition(_, _, _, _, _, _) => { },
                _ => panic!("Not Implemented: {:?}", node),
            }
        }

        exprs.extend(self.transform_vtable_init(scope.clone(), classdef));
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
                    Ok(Def::Method(meth)) => {
                        let classdef = objdef.as_class().unwrap();
                        let vindex = classdef.get_struct_vtable_index().unwrap();
                        let index = classdef.vtable.get_index_by_id(defid).unwrap();
                        let vtable = LLExpr::LoadRef(r(LLExpr::AccessRef(r(objval), vec!(LLRef::Field(vindex)))));
                        exprs.push(LLExpr::LoadRef(r(LLExpr::AccessRef(r(vtable), vec!(LLRef::Field(index))))));
                    },
                    Ok(Def::Field(field)) => {
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
            Type::Tuple(items) => {
                let index = field.parse::<usize>().unwrap();
                exprs.push(LLExpr::GetItem(r(objval), index));
            },
            _ => panic!("Not Implemented: {:?}", otype),
        }
        exprs
    }

    fn transform_assignment(&self, scope: ScopeRef, id: NodeID, left: &AST, right: &AST) -> Vec<LLExpr> {
        let mut exprs = vec!();

        let value = self.transform_as_result(&mut exprs, scope.clone(), right).unwrap();
        match left {
            AST::Accessor(aid, _, obj, field, oid) => {
                let objval = self.transform_as_result(&mut exprs, scope.clone(), obj).unwrap();
                let fieldid = self.session.get_ref(*aid).unwrap();
                let objdef = self.session.get_def(self.session.get_type(*oid).unwrap().get_id().unwrap()).unwrap();
                let (index, _) = objdef.as_struct().unwrap().find_field_by_id(fieldid).unwrap();
                exprs.push(LLExpr::StoreRef(r(LLExpr::AccessRef(r(objval), vec!(LLRef::Field(index)))), r(value)));
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

    fn transform_match(&self, scope: ScopeRef, cond: &AST, cases: &Vec<(Pattern, AST)>, compid: NodeID) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let mut conds = vec!();
        let mut blocks = vec!();

        let ftype = self.session.get_type(compid).unwrap();

        let condid = NodeID::generate();
        let condval = self.transform_as_result(&mut exprs, scope.clone(), cond).unwrap();
        exprs.push(LLExpr::SetValue(condid, r(condval)));

        let cid = NodeID::generate();
        let funcresult = self.transform_as_result(&mut exprs, scope.clone(), &AST::Identifier(compid, Pos::empty(), Ident::from_str("=="))).unwrap();
        exprs.push(LLExpr::SetValue(cid, r(funcresult)));

        for (pat, block) in cases {
            conds.push(match pat {
                Pattern::Underscore => vec!(LLExpr::Literal(LLLit::I1(true))),
                Pattern::Literal(lit) => {
                    let result = self.transform_as_result(&mut exprs, scope.clone(), lit).unwrap();
                    self.create_func_invoke(ftype.get_abi().unwrap(), LLExpr::GetValue(cid), vec!(LLExpr::GetValue(condid), result))
                },
                _ => panic!("Not Implemented: {:?}", pat),
            });
            blocks.push(self.transform_node(scope.clone(), block));
        }

        exprs.push(LLExpr::Phi(conds, blocks));
        exprs
    }

    fn transform_pattern(&self, scope: ScopeRef, pat: Pattern) -> Vec<LLExpr> {
        panic!("Not Implemented");
    }

    fn find_comparison_func(&self, scope: ScopeRef, ctype: &Type) -> NodeID {
        let compid = scope.get_var_def(&String::from("==")).unwrap();
        match self.session.get_def(compid) {
            Ok(Def::Overload(ol)) => ol.find_variant(self.session, scope.clone(), Type::Tuple(vec!(ctype.clone(), ctype.clone()))).unwrap().0,
            _ => compid,
        }
    }

    fn transform_side_effect(&self, scope: ScopeRef, op: &str, args: &Vec<AST>) -> Vec<LLExpr> {
        let mut conds = vec!();
        let mut blocks = vec!();

        // TODO this doesn't work with non-boolean values
        conds.push(self.transform_node(scope.clone(), &args[0]));
        blocks.push(self.transform_node(scope.clone(), &args[1]));
        vec!(LLExpr::Phi(conds, blocks))
    }



    pub fn transform_value_type(&self, ttype: &Type) -> LLType {
        match &ttype {
            Type::Object(name, id, params) => match name.as_str() {
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
                _ => self.types.borrow().get(id).unwrap().clone(),
            },
            Type::Ref(ttype) => LLType::Ptr(r(self.transform_value_type(ttype))),
            Type::Tuple(items) => LLType::Struct(items.iter().map(|item| self.transform_value_type(&item)).collect()),
            Type::Record(items) => LLType::Struct(items.iter().map(|item| self.transform_value_type(&item.1)).collect()),
            // TODO how will you do generics
            //Type::Variable(name, id) => { panic!("") },
            Type::Variable(name, id) => LLType::Ptr(r(LLType::I8)),
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
        structdef.foreach_field(|id, name, ttype| {
            items.push(self.transform_value_type(ttype));
        });
        LLType::Struct(items)
    }
}


