
use std::cell::RefCell;
use std::collections::HashMap;


use binding;
use typecheck;

use abi::ABI;
use defs::Def;
use types::Type;
use session::Session;
use scope::{ Scope, ScopeRef };
use ast::{ NodeID, Pos, Literal, Ident, ClassSpec, Argument, AST };

use defs::functions::FuncDef;
use defs::classes::{ Define, StructDefRef };

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

/*
//((((
let local = NodeID::generate();
//toplevel.push(LLExpr::CallC(r(LLExpr::GetValue(mainid)), vec!()));
toplevel.push(LLExpr::DefLocal(local, String::from("test"), LLType::I64, r(LLExpr::Literal(LLLit::I64(0)))));

let tuple = NodeID::generate();
toplevel.push(LLExpr::DefStruct(tuple, LLType::Struct(vec!(LLType::I64, LLType::I64)), vec!(LLExpr::Literal(LLLit::I64(421)), LLExpr::Literal(LLLit::I64(764)))));

let refcell = NodeID::generate();
toplevel.push(LLExpr::AllocRef(refcell, LLType::Ptr(r(LLType::I64)), r(LLExpr::Literal(LLLit::I64(421)))));
toplevel.push(LLExpr::LoadRef(r(LLExpr::GetValue(refcell))));


toplevel.push(LLExpr::GetLocal(local));
//))))
*/

        self.add_global(LLGlobal::DefCFunc(mainid, String::from("main"), ftype, vec!(), toplevel));
    }

    pub fn transform_vec(&self, scope: ScopeRef, code: &Vec<AST>) -> Vec<LLExpr> {
        let mut exprs = vec!();
        for expr in code {
            exprs.extend(self.transform_node(scope.clone(), expr));
        }
        exprs
    }

    fn transform_node(&self, scope: ScopeRef, node: &AST) -> Vec<LLExpr> {
        match &node {
            AST::Literal(_, lit) => vec!(LLExpr::Literal(self.transform_lit(lit))),

            AST::Block(_, _, code) => self.transform_vec(scope.clone(), code),

            AST::Function(id, _, ident, args, _, body, abi) => {
                self.transform_func_def(scope.clone(), *abi, *id, ident.as_ref().map(|ident| &ident.name), args, body)
            },

            AST::Declare(id, _, ident, ttype) => {
                let abi = self.session.get_type(*id).unwrap().get_abi().unwrap();
                self.transform_func_decl(scope.clone(), abi, *id, &ident.name, ttype)
            },

            AST::Invoke(id, _, func, args) => {
                let abi = self.session.get_type(*id).unwrap().get_abi().unwrap();
                self.transform_func_invoke(scope.clone(), abi, *id, func, args)
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

            _ => panic!("Not Implemented: {:?}", node),
        }
    }

    fn transform_lit(&self, lit: &Literal) -> LLLit {
        match lit {
            Literal::Unit => LLLit::I64(0),
            Literal::Boolean(num) => LLLit::I1(*num as bool),
            Literal::Integer(num) => LLLit::I64(*num as i64),
            Literal::Real(num) => LLLit::F64(*num),
            // TODO not sure how you'll do strings yet
            //Literal::String(string) => ,
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

    fn transform_alloc_ref(&self, scope: ScopeRef, id: NodeID, value: &AST) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let valexpr = self.transform_as_result(&mut exprs, scope.clone(), value).unwrap();
        let ltype = self.transform_value_type(&self.session.get_type(id).unwrap());
        exprs.push(LLExpr::AllocRef(id, ltype, r(valexpr)));
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

    fn transform_func_invoke(&self, scope: ScopeRef, abi: ABI, id: NodeID, func: &AST, args: &Vec<AST>) -> Vec<LLExpr> {
        match abi {
            ABI::C | ABI::MoltenFunc => self.transform_cfunc_invoke(scope.clone(), id, func, args),
            ABI::Molten | ABI::Unknown => self.transform_closure_invoke(scope.clone(), id, func, args),
            _ => panic!("Not Implemented: {:?}", abi),
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

    fn transform_cfunc_invoke(&self, scope: ScopeRef, id: NodeID, func: &AST, args: &Vec<AST>) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let mut fargs = vec!();

        let funcresult = self.transform_as_result(&mut exprs, scope.clone(), func).unwrap();

        for arg in args {
            fargs.push(self.transform_as_result(&mut exprs, scope.clone(), arg).unwrap());
        }

        exprs.push(LLExpr::CallC(r(funcresult), fargs));
        exprs
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

    fn make_closure_type(&self, scope: ScopeRef, ttype: Type) -> Type {
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
        let aexpr = LLExpr::AllocRef(NodeID::generate(), atype.clone(), r(LLExpr::DefStruct(id, stype, vec!(LLExpr::GetValue(cfid)))));
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
        // TODO this needs to be added, doesn't it?
        cl.add_field(self.session, cfid, "__func__", ftype.clone(), Define::Never);

        // Transforms body and create C function definition
        let index = self.globals.borrow().len();
        self.set_context(CodeContext::Closure(id));
        let body = self.transform_node(fscope.clone(), body);
        self.restore_context();
        self.insert_global(index, LLGlobal::DefCFunc(cfid, cfname.clone(), lftype, fargs, body));

        let structtype = LLType::Ptr(r(self.transform_struct_def(&cl.context_struct)));
        self.insert_global(index, LLGlobal::DefType(cl.context_type_id, format!("__context_{}__", cl.context_type_id), structtype));

        let ptype = self.make_closure_type(scope.clone(), self.session.get_type(id).unwrap());
        FuncDef::define(self.session, scope.clone(), cfid, &Some(cfname.clone()), Some(ptype)).unwrap();
        let mut fields = vec!();
        cl.context_struct.foreach_field(|field, _| {
            if field.as_str() == "__func__" {
                fields.push((Ident::from_str("__func__"), AST::make_ident_from_str(Pos::empty(), cfname.as_str())));
            } else {
                fields.push((Ident::from_str(field.as_str()), AST::make_ident_from_str(Pos::empty(), field.as_str())));
            }
        });

        let mut code = vec!();
        // TODO this must be changed back to "id"
        code.push(AST::Definition(/*id*/ NodeID::generate(), Pos::empty(), true, Ident::new(fname.clone()), None, Box::new(AST::make_ref(Pos::empty(), AST::make_record(Pos::empty(), fields)))));
        // TODO I'm going back on my decision to use a tuple pair to represent the function and context reference because it can't be converted to i8* (the generics type)
        //      Once I have generics that can operate on different sized data instead of only references, I can switch back
        //code.push(AST::Tuple(NodeID::generate(), Pos::empty(), vec!(AST::make_ident_from_str(Pos::empty(), real_fname.as_str()), AST::make_ident(Pos::empty(), Ident::new(cname.clone())))));
        code.push(AST::make_ident(Pos::empty(), Ident::new(fname.clone())));

        binding::bind_names(self.session, scope.clone(), &mut code);
        typecheck::check_types(self.session, scope.clone(), &code);
        let mut make_context = self.transform_vec(scope.clone(), &code);
        let last = make_context.pop().unwrap();
        make_context.push(LLExpr::SetValue(id, Box::new(last)));
        make_context.push(LLExpr::GetValue(id));

        make_context
    }

    fn transform_closure_invoke(&self, scope: ScopeRef, id: NodeID, func: &AST, args: &Vec<AST>) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let mut fargs = vec!();

        let fid = NodeID::generate();
        let funcresult = self.transform_as_result(&mut exprs, scope.clone(), func).unwrap();
        exprs.push(LLExpr::SetValue(fid, r(funcresult)));

        for arg in args {
            fargs.push(self.transform_as_result(&mut exprs, scope.clone(), arg).unwrap());
        }
        fargs.push(LLExpr::Cast(LLType::Ptr(r(LLType::I8)), r(LLExpr::GetValue(fid))));

        let function = LLExpr::LoadRef(r(LLExpr::AccessRef(r(LLExpr::GetValue(fid)), vec!(LLRef::Field(0)))));
        exprs.push(LLExpr::CallC(r(function), fargs));
        exprs
    }



    fn transform_reference(&self, scope: ScopeRef, defid: NodeID, name: &String) -> Vec<LLExpr> {
        // TODO temporarily disabling global variables
        if !scope.contains_local(name) && !Scope::global(scope.clone()).contains(name) {
        //if !scope.contains_local(name) {
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



    fn transform_as_result(&self, exprs: &mut Vec<LLExpr>, scope: ScopeRef, node: &AST) -> Option<LLExpr> {
        let mut newexprs = self.transform_node(scope.clone(), node);
        let last = newexprs.pop();
        exprs.extend(newexprs);
        last
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
        structdef.foreach_field(|name, ttype| {
            items.push(self.transform_value_type(ttype));
        });
        LLType::Struct(items)
    }
}


