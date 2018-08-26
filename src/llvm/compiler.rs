
use std::ptr;
use std::fmt;
use std::ffi::CString;
use std::cell::RefCell;
use std::collections::HashMap;


extern crate llvm_sys;
use self::llvm_sys::*;
use self::llvm_sys::prelude::*;
use self::llvm_sys::core::*;
use self::llvm_sys::target::*;
use self::llvm_sys::target_machine::*;
use self::llvm_sys::transforms::pass_manager_builder::*;

use export;
use abi::ABI;
use types::{ self, Type };
use utils::UniqueID;
use config::Options;
use session::Session;
use defs::Def;
use scope::{ Scope, ScopeRef, ScopeMapRef };
use ast::{ NodeID, Pos, Literal, Ident, ClassSpec, AST };

use defs::functions::FuncDef;
use defs::classes::{ ClassDefRef, StructDef };

use llvm::lib::{ BuiltinDef, initialize_builtins };


pub type Value = Box<Compilable>;

impl Clone for Box<Compilable> {
    fn clone(&self) -> Box<Compilable> {
        self.box_clone()
    }
}

impl fmt::Debug for Box<Compilable> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.get_ref())
    }
}



pub fn from_abi(abi: &ABI, value: LLVMValueRef) -> Box<Compilable> {
    match *abi {
        ABI::Unknown | ABI::Molten => Box::new(Function(value)),
        ABI::C => Box::new(CFunction(value)),
        _ => panic!("Unsupported ABI for LLVM Compiling: {:?}", abi)
    }
}

pub fn from_type(ttype: &Type, value: LLVMValueRef) -> Box<Compilable> {
    match *ttype {
        Type::Function(_, _, ref abi) => from_abi(abi, value),
        _ => Box::new(Data(value))
    }
}


pub trait Compilable {
    fn box_clone(&self) -> Box<Compilable>;
    fn get_ref(&self) -> LLVMValueRef;

    //unsafe fn resolve(&self, data: &LLVM) -> Box<Compilable>;

    unsafe fn invoke(&self, data: &LLVM, unwind: Unwind, mut largs: Vec<LLVMValueRef>) -> LLVMValueRef {
        //LLVMBuildCall(data.builder, self.0, largs.as_mut_ptr(), largs.len() as u32, label("tmp"))
        match unwind {
            None => LLVMBuildCall(data.builder, self.get_ref(), largs.as_mut_ptr(), largs.len() as u32, label("thingies")),
            Some((then, catch)) => {
                LLVMBuildInvoke(data.builder, self.get_ref(), largs.as_mut_ptr(), largs.len() as u32, then, catch, label("tmp"))
                //LLVMSetUnwindDest(invoke, unwind.unwrap());
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Data(pub LLVMValueRef);

impl Compilable for Data {
    fn box_clone(&self) -> Box<Compilable> {
        Box::new((*self).clone())
    }

    fn get_ref(&self) -> LLVMValueRef {
        self.0
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function(pub LLVMValueRef);

impl Compilable for Function {
    fn box_clone(&self) -> Box<Compilable> {
        Box::new((*self).clone())
    }

    fn get_ref(&self) -> LLVMValueRef {
        self.0
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CFunction(pub LLVMValueRef);

impl Compilable for CFunction {
    fn box_clone(&self) -> Box<Compilable> {
        Box::new((*self).clone())
    }

    fn get_ref(&self) -> LLVMValueRef {
        self.0
    }

    unsafe fn invoke(&self, data: &LLVM, _unwind: Unwind, mut largs: Vec<LLVMValueRef>) -> LLVMValueRef {
        LLVMBuildCall(data.builder, self.0, largs.as_mut_ptr(), largs.len() as u32, label("tmp"))
    }
}


#[derive(Clone, Debug)]
pub struct Builtin(pub BuiltinFunction, pub Type);

impl Compilable for Builtin {
    fn box_clone(&self) -> Box<Compilable> {
        Box::new((*self).clone())
    }

    fn get_ref(&self) -> LLVMValueRef {
        ptr::null_mut()
    }

    unsafe fn invoke(&self, data: &LLVM, _unwind: Unwind, largs: Vec<LLVMValueRef>) -> LLVMValueRef {
        (self.0).0(data, largs)
    }
}

#[derive(Clone)]
pub struct BuiltinFunction(pub unsafe fn(&LLVM, Vec<LLVMValueRef>) -> LLVMValueRef);

impl fmt::Debug for BuiltinFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "??")
    }
}




#[derive(Clone, Debug, PartialEq)]
pub struct Var(pub LLVMValueRef);

impl Compilable for Var {
    fn box_clone(&self) -> Box<Compilable> {
        Box::new((*self).clone())
    }

    fn get_ref(&self) -> LLVMValueRef {
        self.0
    }

}

#[derive(Clone, Debug, PartialEq)]
pub struct Global(pub LLVMValueRef);

impl Compilable for Global {
    fn box_clone(&self) -> Box<Compilable> {
        Box::new((*self).clone())
    }

    fn get_ref(&self) -> LLVMValueRef {
        self.0
    }
}



#[derive(Clone, Debug, PartialEq)]
pub struct TypeValue {
    pub value: LLVMTypeRef,
    pub vttype: Option<LLVMTypeRef>,
}


pub struct LLVM<'sess> {
    pub session: &'sess Session,
    pub map: &'sess ScopeMapRef,
    //pub builtins: BuiltinMap<'sess>,
    pub functions: Vec<&'sess AST>,
    pub classes: Vec<&'sess AST>,
    pub context: LLVMContextRef,
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,
    //pub funcpass: LLVMPassManagerRef,
    pub values: RefCell<HashMap<UniqueID, Value>>,
    pub types: RefCell<HashMap<UniqueID, TypeValue>>,
}

impl<'sess> LLVM<'sess> {
    pub fn set_value(&self, id: UniqueID, value: Value) {
debug!(">>>>>>>>> SET VALUE: {:?} = {:?} (existing value {:?})", id, value, self.values.borrow().get(&id));
        self.values.borrow_mut().insert(id, value);
    }

    pub fn get_value(&self, id: UniqueID) -> Option<Value> {
        self.values.borrow().get(&id).cloned()
    }

    pub fn set_type(&self, id: UniqueID, value: TypeValue) {
        self.types.borrow_mut().insert(id, value);
    }

    pub fn get_type(&self, id: UniqueID) -> Option<TypeValue> {
        self.types.borrow().get(&id).cloned()
    }
}


type Unwind = Option<(LLVMBasicBlockRef, LLVMBasicBlockRef)>;


pub fn compile<'sess>(builtins: &Vec<BuiltinDef<'sess>>, session: &'sess Session, module_name: &str, code: &'sess Vec<AST>) {
    unsafe {
        compile_module(builtins, session, session.map.get_global(), module_name, code)
    }
}

unsafe fn compile_module<'sess>(builtins: &Vec<BuiltinDef<'sess>>, session: &'sess Session, scope: ScopeRef, module_name: &str, code: &'sess Vec<AST>) {
    let context = LLVMContextCreate();
    let module = LLVMModuleCreateWithName(label(module_name));
    let builder = LLVMCreateBuilderInContext(context);
    let data = &mut LLVM {
        session: session,
        map: &session.map,
        //builtins: BuiltinMap::new(),
        functions: Vec::new(),
        classes: Vec::new(),
        context: context,
        module: module,
        builder: builder,
        values: RefCell::new(HashMap::new()),
        types: RefCell::new(HashMap::new()),
    };
    //LLVMSetDataLayout(data.module, label("e-m:e-i64:64-f80:128-n8:16:32:64-S128"));

    initialize_builtins(data, scope.clone(), builtins);
    collect_functions_vec(data, scope.clone(), code);
    declare_globals(data, scope.clone());
    for func in &data.functions {
        build_function_body(data, func);
    }

    let module_init_name = format!("init.{}", module_name.replace("/", "."));


    let ftype = Type::Function(Box::new(Type::Tuple(vec!())), Box::new(scope.make_obj(data.session, String::from("Bool"), vec!()).unwrap()), ABI::Molten);
    let lftype = LLVMFunctionType(bool_type(data), ptr::null_mut(), 0, 0);
    let function = LLVMAddFunction(module, label(module_init_name.as_str()), lftype);
    //LLVMSetPersonalityFn(function, LLVMGetNamedFunction(data.module, label("__gxx_personality_v0")));
    LLVMPositionBuilderAtEnd(builder, LLVMAppendBasicBlockInContext(context, function, label("entry")));
    compile_vec(data, function, None, scope.clone(), code);
    LLVMBuildRet(builder, LLVMConstInt(bool_type(data), 1, 0));
    let id = NodeID::generate();
    FuncDef::define_func(session, scope.clone(), id, &Some(module_init_name.clone()), ABI::C, Some(ftype)).unwrap();
    data.set_value(id, Box::new(Function(function)));


    if !Options::as_ref().is_library {
        let pos = Pos::empty();
        let function_type = LLVMFunctionType(int_type(data), ptr::null_mut(), 0, 0);
        let function = LLVMAddFunction(module, b"main\0".as_ptr() as *const _, function_type);
        //LLVMSetPersonalityFn(function, LLVMGetNamedFunction(data.module, label("__gxx_personality_v0")));
        LLVMPositionBuilderAtEnd(builder, LLVMAppendBasicBlockInContext(context, function, label("entry")));
        //compile_node(data, function, None, scope.clone(), &AST::make_invoke(pos.clone(), Box::new(AST::make_ident(pos.clone(), Ident::new(pos.clone(), String::from(module_init_name)))), vec!(), Some(Type::Function(Box::new(Type::Tuple(vec!())), Box::new(Type::Object(String::from("Bool"), vec!())), ABI::Molten))));
        build_c_call(data, module_init_name.as_str(), &mut vec!());
        LLVMBuildRet(builder, int_value(data, 0));
    }

    //optimize_ir(data, 3);

    // Output to a file, and also a string for debugging
    //format!("{}.ll", module_name)
    LLVMPrintModuleToFile(module, label(session.target.as_str()), ptr::null_mut());

    if Options::as_ref().debug {
        println!("{}\n", CString::from_raw(LLVMPrintModuleToString(module)).into_string().unwrap());
    }

    let name = session.target.rsplitn(2, '.').collect::<Vec<&str>>()[1];
    export::write_exports(session, session.map.get_global(), format!("{}.dec", name).as_str(), code);
    //write_module(data, format!("{}.o", name).as_str());

    LLVMDisposeBuilder(builder);
    LLVMDisposeModule(module);
    LLVMContextDispose(context);
}

unsafe fn optimize_ir(data: &LLVM, llvm_opt: u32) {
    let builder = LLVMPassManagerBuilderCreate();
    // -O optimization level
    LLVMPassManagerBuilderSetOptLevel(builder, llvm_opt);

    let pass_manager = LLVMCreatePassManager();
    LLVMPassManagerBuilderPopulateModulePassManager(builder, pass_manager);
    LLVMPassManagerBuilderDispose(builder);

    LLVMRunPassManager(pass_manager, data.module);
    //LLVMRunPassManager(pass_manager, data.module);

    LLVMDisposePassManager(pass_manager);
}

unsafe fn write_module(data: &LLVM, filename: &str) {

    LLVM_InitializeAllTargetInfos();
    LLVM_InitializeAllTargets();
    LLVM_InitializeAllTargetMCs();
    LLVM_InitializeAllAsmParsers();
    LLVM_InitializeAllAsmPrinters();

    let target_triple = LLVMGetDefaultTargetTriple();
    LLVMSetTarget(data.module, target_triple);

    let target: *mut LLVMTargetRef = Box::into_raw(Box::new(ptr::null_mut()));
    let err_msg: *mut *mut i8 = Box::into_raw(Box::new(ptr::null_mut()));
    if LLVMGetTargetFromTriple(target_triple, target, err_msg) != 0 {
        let err = CString::from_raw(*err_msg.as_ref().unwrap());
        eprintln!("Get LLVM target failed");
        eprintln!("{}", err.to_str().unwrap());
        panic!();
    }
    LLVMDisposeMessage(*err_msg.as_ref().unwrap());

    let target_machine = LLVMCreateTargetMachine(*target.as_ref().unwrap(), target_triple, label("generic"), label(""), LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault, LLVMRelocMode::LLVMRelocDefault, LLVMCodeModel::LLVMCodeModelDefault);
    LLVMTargetMachineEmitToFile(target_machine, data.module, label(filename), LLVMCodeGenFileType::LLVMObjectFile, ptr::null_mut());
/*
    let mut target = ptr::null_mut();
    let mut err_msg = ptr::null_mut();
    LLVMGetTargetFromTriple(target_triple, &mut target, &mut err_msg);
    if target.is_null() {
        //let err = CString::from_raw(*err_msg.as_ref().unwrap());
        eprintln!("Get LLVM target failed");
        //eprintln!("{}", err.to_str().unwrap());
        panic!();
    }
    LLVMDisposeMessage(err_msg);

    let target_machine = LLVMCreateTargetMachine(target, target_triple, label("generic"), label(""), LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault, LLVMRelocMode::LLVMRelocDefault, LLVMCodeModel::LLVMCodeModelDefault);
    LLVMTargetMachineEmitToFile(target_machine, data.module, label(filename), LLVMCodeGenFileType::LLVMObjectFile, ptr::null_mut());
*/

}

unsafe fn declare_globals(data: &LLVM, scope: ScopeRef) {
    let mut globals = vec!();
    for (name, sym) in scope.names.borrow().iter() {
        let did = sym.defid.unwrap();
        let def = data.session.get_def(did).unwrap();
        if def.num_variants(data.session) == 0 {
            let ltype = get_type(data, scope.clone(), data.session.get_type(did).unwrap(), true);
            let global = LLVMAddGlobal(data.module, ltype, label(name.as_str()));
            LLVMSetInitializer(global, null_value(ltype));
            globals.push((did, global));
        }
    }

    for (did, global) in globals {
        data.set_value(did, Box::new(Global(global)));
    }

    for node in &data.classes {
        if let AST::Class(ref id, _, ClassSpec { ident: ref cident, .. }, _, _) = **node {
            let tscope = data.map.get(id);
            let classdef = tscope.find_type_def(data.session, &cident.name).unwrap().as_class().unwrap();
            let value = data.get_type(tscope.get_type_def(&cident.name).unwrap()).unwrap();

            if classdef.has_vtable() {
                let mut methods = vec!();
                for (index, &(ref id, ref name, ref ttype)) in classdef.vtable.borrow().table.iter().enumerate() {
                    let dtype = data.session.get_type(*id).unwrap();
                    debug!("VTABLE INIT: {:?} {:?} {:?}", cident.name, id, index);
                    methods.push(build_generic_cast(data, data.get_value(*id).unwrap().get_ref(), get_type(data, tscope.clone(), ttype.clone(), true)));
                }

                let vtype = value.vttype.unwrap();
                let vname = format!("__{}_vtable", cident.name);
                let global = LLVMAddGlobal(data.module, LLVMGetElementType(vtype), label(vname.as_str()));
                LLVMSetInitializer(global, LLVMConstNamedStruct(vtype, methods.as_mut_ptr(), methods.len() as u32));
                LLVMSetLinkage(global, LLVMLinkage::LLVMLinkOnceAnyLinkage);
                let id = scope.define(vname.clone(), None).unwrap();
                data.session.set_type(id, Type::Object(format!("{}_vtable", cident.name), classdef.vtable.borrow().id, vec!()));
                data.set_value(id, Box::new(Global(global)));
            }
        }
    }
}

unsafe fn compile_vec(data: &LLVM, func: LLVMValueRef, unwind: Unwind, scope: ScopeRef, code: &Vec<AST>) -> Value {
    let mut last: Value = Box::new(Data(zero_int(data)));
    for node in code {
        last = compile_node(data, func, unwind, scope.clone(), node);
    }
    last
}

unsafe fn compile_node(data: &LLVM, func: LLVMValueRef, unwind: Unwind, scope: ScopeRef, node: &AST) -> Value {
    debug!("COMPILE: {:?}", node);
    match *node {
        AST::Nil(ref id) => Box::new(Data(null_value(get_type(data, scope.clone(), data.session.get_type(*id).unwrap(), true)))),
        AST::Literal(ref id, ref lit) => {
            match lit {
                Literal::Boolean(ref num) => Box::new(Data(LLVMConstInt(bool_type(data), *num as u64, 0))),
                Literal::Integer(ref num) => Box::new(Data(LLVMConstInt(int_type(data), *num as u64, 0))),
                Literal::Real(ref num) => Box::new(Data(LLVMConstReal(real_type(data), *num))),
                Literal::String(ref string) => Box::new(Data(LLVMBuildGlobalStringPtr(data.builder, label(string.as_str()), label("strc")))),
            }
        },

        AST::Block(ref id, _, ref body) => { compile_vec(data, func, unwind, scope.clone(), body) },

        AST::Invoke(ref id, _, ref fexpr, ref args) => {
            let ftype = data.session.get_type(*id).unwrap();
            let (atypes, rtype, abi) = match ftype.clone() {
                Type::Function(atypes, rtype, abi) => (atypes, *rtype, abi),
                ftype @ _ => panic!("TypeError: expected function type: {:?}", ftype),
            };

            let mut largs = vec!();
            // TODO this forces the function arg type to be a tuple
            for (ttype, arg) in atypes.get_types().unwrap().iter().zip(args.iter()) {
                let mut larg = compile_node(data, func, unwind, scope.clone(), arg).get_ref();
                let ltype = get_type(data, scope.clone(), ttype.clone(), true);
                if ltype != LLVMTypeOf(larg) {
                    // TODO this seems to cast to int as well as pointers, so maybe it's doing too much, at least without checking that it's supposed to
                    larg = LLVMBuildPointerCast(data.builder, larg, ltype, label("ptr"));
                }
                largs.push(larg);
            }

            //let function = compile_node(data, func, unwind, scope.clone(), fexpr);
            let function = match **fexpr {
                // Compile the first argument of a method access only once for the method lookup and the argument evalution
                AST::Accessor(ref id, ref pos, ref left, ref field, ref otype) => {
                    let oid = left.get_id();
                    //let oid = NodeID::generate();
                    //let classvars = scope.find_type_def(data.session, &otype.clone().unwrap().get_name().unwrap()).unwrap().as_class().unwrap().classvars.clone();
                    //let defid = classvars.get_var_def(&field.name).unwrap();
                    //data.session.set_ref(oid, defid);

// TODO replace use of otype
                    data.set_value(oid, from_type(otype.as_ref().unwrap(), largs[0]));
                    compile_node(data, func, unwind, scope.clone(), &AST::Accessor(*id, pos.clone(), Box::new(AST::Recall(oid, pos.clone())), field.clone(), otype.clone()))
                },
                _ => compile_node(data, func, unwind, scope.clone(), fexpr)
            };
            //let function = Function(build_generic_cast(data, function.get_ref(), get_type(data, scope.clone(), ftype.clone(), true)));
            if !function.get_ref().is_null() { LLVMDumpValue(function.get_ref()); }

            if !function.get_ref().is_null() {
                // Cast values to the function's declared type; this is a hack for typevar/generic arguments
                let mut lftype = LLVMTypeOf(function.get_ref());
                if LLVMGetTypeKind(lftype) == LLVMTypeKind::LLVMPointerTypeKind {
                    lftype = LLVMGetElementType(lftype);
                }
                let mut ltypes = Vec::with_capacity(LLVMCountParamTypes(lftype) as usize);
                ltypes.set_len(LLVMCountParamTypes(lftype) as usize);
                LLVMGetParamTypes(lftype, ltypes.as_mut_ptr());
                for i in 0 .. ltypes.len() {
                    largs[i] = build_generic_cast(data, largs[i], ltypes[i]);
                }
            }

            //LLVMBuildCall(data.builder, function, largs.as_mut_ptr(), largs.len() as u32, label("tmp").as_ptr())
            //let mut value = match unwind {
            //    None => LLVMBuildCall(data.builder, function.get_ref(), largs.as_mut_ptr(), largs.len() as u32, label("tmp")),
            //    Some((then, catch)) => {
            //        LLVMBuildInvoke(data.builder, function.get_ref(), largs.as_mut_ptr(), largs.len() as u32, then, catch, label("tmp"))
            //        //LLVMSetUnwindDest(invoke, unwind.unwrap());
            //    }
            //};
            let mut value = function.invoke(data, unwind, largs);

            // Cast return value to the function's declared return type
            if !function.get_ref().is_null() {
                let lrtype = get_type(data, scope.clone(), rtype.clone(), true);
                value = build_generic_cast(data, value, lrtype);
            }

            from_type(&rtype, value)
        },

        AST::Function(ref id, _, ref ident, _, _, _, ref abi) => {
            match *ident {
                Some(ref ident) => data.get_value(*id).unwrap(),
                _ => from_abi(abi, LLVMGetNamedFunction(data.module, label(scope.get_full_name(ident, *id).as_str()))),
            }
        },

        AST::Recall(ref id, ref pos) => {
            match data.get_value(*id) {
                Some(x) => x,
                None => panic!("UnsetError:{:?}: use before assignment", pos),
            }
        },

        AST::Identifier(ref id, ref pos, ref ident) => {
            let defid = data.session.get_ref(*id).unwrap();
            let pointer = match data.get_value(defid) {
                Some(x) => x,
                None => panic!("UnsetError:{:?}: use before assignment {:?}", pos, &ident.name),
            };
            let ttype = data.session.get_type(defid).unwrap();
            if !pointer.get_ref().is_null() { debug!("IDENT: {:?} {:?} {:?}", LLVMGetValueKind(pointer.get_ref()), LLVMGetTypeKind(LLVMTypeOf(pointer.get_ref())), ttype); }
            //if !pointer.get_ref().is_null() { LLVMDumpValue(pointer.get_ref()); }
            //if LLVMGetTypeKind(LLVMTypeOf(pointer)) == LLVMTypeKind::LLVMPointerTypeKind {
            if !pointer.get_ref().is_null() {
                match LLVMGetValueKind(pointer.get_ref()) {
                    //LLVMValueKind::LLVMArgumentValueKind |
                    //LLVMValueKind::LLVMFunctionValueKind => pointer,
                    LLVMValueKind::LLVMInstructionValueKind |
                    LLVMValueKind::LLVMGlobalVariableValueKind => from_type(&ttype, LLVMBuildLoad(data.builder, pointer.get_ref(), label("tmp"))),
                    _ => pointer,
                }
            } else {
                pointer
            }

            //pointer
        },

        AST::Definition(ref id, _, ref ident, _, ref value) => {
            let ttype = data.session.get_type(*id).unwrap();
            let ltype = get_type(data, scope.clone(), ttype, true);
            let pointer: Value = if scope.is_global() {
                //LLVMAddGlobal(data.module, ltype, label(ident.name.as_str()))
                //let global = LLVMGetNamedGlobal(data.module, label(ident.name.as_str()));
                data.get_value(*id).unwrap()
            } else {
                Box::new(Var(LLVMBuildAlloca(data.builder, ltype, label(ident.name.as_str()))))
            };
            data.set_value(*id, pointer.clone());
            let value = compile_node(data, func, unwind, scope.clone(), value);
            LLVMBuildStore(data.builder, build_generic_cast(data, value.get_ref(), ltype), pointer.get_ref());
            value
        },

        // TODO you might need to create a new function to hold the body, so that it can be resumed from, and there also
        // might be an issue when you call a function one level down, and pass it the same unwind locations... it might effect all
        // functions, all the way down
        AST::Try(ref id, _, ref body, ref cases, ref condtype) => {
// TODO replace use of condtype
            /*
            // TODO need to add multiple cases, and also it doesn't work
            let try_block = LLVMAppendBasicBlockInContext(data.context, func, label("try"));
            let catch_block = LLVMAppendBasicBlockInContext(data.context, func, label("catch"));
            let finally_block = LLVMAppendBasicBlockInContext(data.context, func, label("finally"));

            LLVMBuildBr(data.builder, try_block);
            LLVMPositionBuilderAtEnd(data.builder, try_block);
            let body_value = compile_node(data, func, Some((finally_block, catch_block)), scope.clone(), body);
            LLVMBuildBr(data.builder, finally_block);

            LLVMPositionBuilderAtEnd(data.builder, catch_block);
            let pad = LLVMBuildLandingPad(data.builder, LLVMTypeOf(body_value), ptr::null_mut(), 0, label("pad"));
            LLVMSetCleanup(pad, 1);
            //LLVMAddClause(pad, );
            LLVMBuildBr(data.builder, finally_block);

            LLVMPositionBuilderAtEnd(data.builder, finally_block);
            //let phi = LLVMBuildPhi(data.builder, LLVMTypeOf(body_value), label("matchphi"));

            //LLVMAddIncoming(phi, values.as_mut_ptr(), do_blocks.as_mut_ptr(), values.len() as u32);
            //phi
            */
            Box::new(Data(null_value(str_type(data))))
        },

        AST::Raise(ref id, _, ref expr) => {
            //LLVMBuildResume(data.builder, compile_node(data, func, unwind, scope.clone(), expr))
            Box::new(Data(null_value(str_type(data))))
        },

        AST::SideEffect(ref id, _, ref op, ref args) => {
            // TODO This only handles two arguments, which is all that will be parsed, but you can do better... 
            let lexpr_block = LLVMAppendBasicBlockInContext(data.context, func, label(op.name.as_str()));
            let rexpr_block = LLVMAppendBasicBlockInContext(data.context, func, label(op.name.as_str()));
            let merge_block = LLVMAppendBasicBlockInContext(data.context, func, label(op.name.as_str()));

            LLVMBuildBr(data.builder, lexpr_block);

            LLVMPositionBuilderAtEnd(data.builder, lexpr_block);
            let lexpr_value = compile_node(data, func, unwind, scope.clone(), &args[0]).get_ref();
            let test_type = match op.as_str() {
                "or" => LLVMIntPredicate::LLVMIntNE,
                "and" => LLVMIntPredicate::LLVMIntEQ,
                _ => panic!("NotImplementedError: attempted to compile invalid side effect operation: {}", op.name.as_str())
            };
            let is_enough = LLVMBuildICmp(data.builder, test_type, lexpr_value, null_value(LLVMTypeOf(lexpr_value)), label("is_enough"));
            LLVMBuildCondBr(data.builder, is_enough, merge_block, rexpr_block);

            LLVMPositionBuilderAtEnd(data.builder, rexpr_block);
            let rexpr_value = compile_node(data, func, unwind, scope.clone(), &args[1]).get_ref();
            LLVMBuildBr(data.builder, merge_block);

            LLVMPositionBuilderAtEnd(data.builder, merge_block);
            let phi = LLVMBuildPhi(data.builder, LLVMTypeOf(lexpr_value), label(op.name.as_str()));

            let mut values = vec![lexpr_value, rexpr_value];
            let mut blocks = vec![lexpr_block, rexpr_block];

            LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);
            Box::new(Data(phi))
        },

        AST::If(ref id, _, ref cond, ref texpr, ref fexpr) => {
            let cond_value = compile_node(data, func, unwind, scope.clone(), cond).get_ref();
            let cond_zero = LLVMConstInt(LLVMTypeOf(cond_value), 0, 0);
            let is_nonzero = LLVMBuildICmp(data.builder, LLVMIntPredicate::LLVMIntNE, cond_value, cond_zero, label("is_nonzero"));

            let texpr_block = LLVMAppendBasicBlockInContext(data.context, func, label("ifthen"));
            let fexpr_block = LLVMAppendBasicBlockInContext(data.context, func, label("ifelse"));
            let merge_block = LLVMAppendBasicBlockInContext(data.context, func, label("ifend"));

            LLVMBuildCondBr(data.builder, is_nonzero, texpr_block, fexpr_block);


            LLVMPositionBuilderAtEnd(data.builder, texpr_block);
            let texpr_value = compile_node(data, func, unwind, scope.clone(), texpr).get_ref();
            LLVMBuildBr(data.builder, merge_block);
            let texpr_block = LLVMGetInsertBlock(data.builder);

            LLVMPositionBuilderAtEnd(data.builder, fexpr_block);
            let fexpr_value = compile_node(data, func, unwind, scope.clone(), fexpr).get_ref();
            LLVMBuildBr(data.builder, merge_block);
            let fexpr_block = LLVMGetInsertBlock(data.builder);

            LLVMPositionBuilderAtEnd(data.builder, merge_block);
            let phi = LLVMBuildPhi(data.builder, LLVMTypeOf(texpr_value), label("iftmp"));

            let mut values = vec![texpr_value, fexpr_value];
            let mut blocks = vec![texpr_block, fexpr_block];

            LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);
            Box::new(Data(phi))
        },

        AST::Match(ref id, _, ref cond, ref cases, ref condtype) => {
// TODO replace use of condtype
            let mut cond_blocks = vec!();
            let mut do_blocks = vec!();
            for _ in 0 .. cases.len() {
                cond_blocks.push(LLVMAppendBasicBlockInContext(data.context, func, label("matchcond")));
                do_blocks.push(LLVMAppendBasicBlockInContext(data.context, func, label("matchdo")));
            }
            let merge_block = LLVMAppendBasicBlockInContext(data.context, func, label("matchend"));
            cond_blocks.push(merge_block);

            let cond_value = compile_node(data, func, unwind, scope.clone(), cond).get_ref();
            LLVMBuildBr(data.builder, cond_blocks[0]);

            let ctype = condtype.as_ref().unwrap();
            let mut values = vec!();
            for (i, &(ref case, ref expr)) in cases.iter().enumerate() {
                LLVMPositionBuilderAtEnd(data.builder, cond_blocks[i]);
                match *case {
                    AST::Underscore => { LLVMBuildBr(data.builder, do_blocks[i]); },
                    _ => {
                        let case_value = compile_node(data, func, unwind, scope.clone(), case).get_ref();
                        //let is_true = LLVMBuildICmp(data.builder, LLVMIntPredicate::LLVMIntEQ, cond_value, case_value, label("is_true"));
                        let eqfunc = get_method(data, scope.clone(), "", "==", vec!(ctype.clone(), ctype.clone()));
                        let is_true = eqfunc.invoke(data, unwind, vec!(cond_value, case_value));
                        LLVMBuildCondBr(data.builder, is_true, do_blocks[i], cond_blocks[i + 1]);
                    }
                }

                LLVMPositionBuilderAtEnd(data.builder, do_blocks[i]);
                values.push(compile_node(data, func, unwind, scope.clone(), expr).get_ref());
                LLVMBuildBr(data.builder, merge_block);
                do_blocks[i] = LLVMGetInsertBlock(data.builder);
            }

            LLVMPositionBuilderAtEnd(data.builder, merge_block);
            let phi = LLVMBuildPhi(data.builder, LLVMTypeOf(values[0]), label("matchphi"));

            LLVMAddIncoming(phi, values.as_mut_ptr(), do_blocks.as_mut_ptr(), values.len() as u32);
            Box::new(Data(phi))
        },

        AST::While(ref id, _, ref cond, ref body) => {
            let before_block = LLVMAppendBasicBlockInContext(data.context, func, label("while"));
            let body_block = LLVMAppendBasicBlockInContext(data.context, func, label("whilebody"));
            let after_block = LLVMAppendBasicBlockInContext(data.context, func, label("whileend"));

            LLVMBuildBr(data.builder, before_block);
            LLVMPositionBuilderAtEnd(data.builder, before_block);
            let cond_value = compile_node(data, func, unwind, scope.clone(), cond).get_ref();
            let cond_zero = LLVMConstInt(LLVMTypeOf(cond_value), 0, 0);
            let is_nonzero = LLVMBuildICmp(data.builder, LLVMIntPredicate::LLVMIntNE, cond_value, cond_zero, label("is_nonzero"));
            LLVMBuildCondBr(data.builder, is_nonzero, body_block, after_block);

            LLVMPositionBuilderAtEnd(data.builder, body_block);
            //let body_value = compile_node(data, func, unwind, scope.clone(), body).get_ref();
            compile_node(data, func, unwind, scope.clone(), body).get_ref();
            LLVMBuildBr(data.builder, before_block);

            LLVMPositionBuilderAtEnd(data.builder, after_block);
            //let phi = LLVMBuildPhi(data.builder, LLVMTypeOf(body_value), label("whileend"));

            //let mut values = vec![body_value];
            //let mut blocks = vec![before_block];

            //LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), 1);
            //phi
            //body_value
            Box::new(Data(null_value(str_type(data))))
        },

        AST::For(ref id, _, ref ident, ref list, ref body) => {
            let lscope = data.map.get(id);
            let listdef = scope.find_type_def(data.session, &String::from("List")).unwrap().as_class().unwrap();

            let list_value = compile_node(data, func, unwind, scope.clone(), list).get_ref();
            let inc = LLVMBuildAlloca(data.builder, int_type(data), label("inc"));
            LLVMBuildStore(data.builder, zero_int(data), inc);

            let itype = get_type(data, lscope.clone(), lscope.get_variable_type(data.session, &ident.name).unwrap(), true);
            let item = LLVMBuildAlloca(data.builder, itype, label("item"));
            data.set_value(*id, Box::new(Var(item)));

            let before_block = LLVMAppendBasicBlockInContext(data.context, func, label("for"));
            let body_block = LLVMAppendBasicBlockInContext(data.context, func, label("forbody"));
            let after_block = LLVMAppendBasicBlockInContext(data.context, func, label("forend"));

            LLVMBuildBr(data.builder, before_block);
            LLVMPositionBuilderAtEnd(data.builder, before_block);
            //let cond_value = compile_node(data, func, unwind, scope.clone(), cond).get_ref();

            let cond_value = LLVMBuildLoad(data.builder, inc, label("tmp"));
            let cond_length = data.get_value(listdef.classvars.get_var_def(&String::from("len")).unwrap()).unwrap().invoke(data, unwind, vec!(list_value));
            let is_end = LLVMBuildICmp(data.builder, LLVMIntPredicate::LLVMIntSGE, cond_value, cond_length, label("is_end"));
            LLVMBuildCondBr(data.builder, is_end, after_block, body_block);

            LLVMPositionBuilderAtEnd(data.builder, body_block);
            let nextitem = build_cast_from_vartype(data, data.get_value(listdef.classvars.get_var_def(&String::from("get")).unwrap()).unwrap().invoke(data, unwind, vec!(list_value, cond_value)), itype);
            LLVMBuildStore(data.builder, nextitem, item);

            let body_value = compile_node(data, func, unwind, lscope.clone(), body).get_ref();

            let next = LLVMBuildLoad(data.builder, inc, label("tmp"));
            let next = LLVMBuildAdd(data.builder, next, int_value(data, 1), label("tmp"));
            LLVMBuildStore(data.builder, next, inc);
            LLVMBuildBr(data.builder, before_block);

            LLVMPositionBuilderAtEnd(data.builder, after_block);
            //let phi = LLVMBuildPhi(data.builder, LLVMInt64TypeInContext(data.context), label("forend"));

            //let mut values = vec![texpr_value, fexpr_value];
            //let mut blocks = vec![texpr_block, fexpr_block];

            //LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);
            //phi
            Box::new(Data(body_value))
        },

        AST::List(ref id, ref pos, ref items) => {
            // TODO this was desugared out eariler
            /*
            let itype = stype.as_ref().unwrap();
            let ltype = Type::Object(format!("List"), vec!(itype.clone()));
            let newfunc = get_method(data, scope.clone(), "List", "new", vec!(ltype.clone()));
            let pushfunc = get_method(data, scope.clone(), "List", "push", vec!(ltype.clone(), itype.clone()));
            //let listdef = scope.find_type_def(data.session, &String::from("List")).unwrap().as_class().unwrap();
            // TODO this is kinda wrong, since you're passing it without type params
            //let list = data.get_value(listdef.get_var_def(&String::from("new")).unwrap()).unwrap().invoke(data, unwind, vec!(compile_node(data, func, unwind, scope.clone(), &AST::make_new(pos.clone(), (String::from("List"), vec!()))).get_ref()));
            let list = newfunc.invoke(data, unwind, vec!(compile_node(data, func, unwind, scope.clone(), &AST::make_new(pos.clone(), ClassSpec::new(pos.clone(), Ident::new(pos.clone(), String::from("List")), vec!(itype.clone())))).get_ref()));
            for item in items {
                let value = build_cast_to_vartype(data, compile_node(data, func, unwind, scope.clone(), item).get_ref());
                //data.get_value(listdef.get_var_def(&String::from("push")).unwrap()).unwrap().invoke(data, unwind, vec!(list, value));
                pushfunc.invoke(data, unwind, vec!(list, value));
            }
            Box::new(Data(list))
            */
            Box::new(Data(null_value(str_type(data))))
        },

        AST::Tuple(ref id, ref pos, ref items) => {
            let ttype = data.session.get_type(*id).unwrap();
            let ltype = get_type(data, scope.clone(), ttype, true);
            let tuple = LLVMBuildAlloca(data.builder, ltype, label("tuple"));
            for (index, item) in items.iter().enumerate() {
                let value = compile_node(data, func, unwind, scope.clone(), item).get_ref();
                let mut indices = vec!(i32_value(data, 0), i32_value(data, index));
                let pointer = LLVMBuildGEP(data.builder, tuple, indices.as_mut_ptr(), indices.len() as u32, label("tmp"));
                LLVMBuildStore(data.builder, value, pointer);
            }
            Box::new(Var(tuple))
        },

        AST::PtrCast(ref ttype, ref code) => {
            let mut value = compile_node(data, func, unwind, scope.clone(), code).get_ref();
            let ltype = get_type(data, scope.clone(), ttype.clone(), true);
            value = build_generic_cast(data, value, ltype);
            from_type(ttype, value)
        },

        AST::New(ref id, _, ClassSpec { ref ident, .. }) => {
            let classdef = scope.find_type_def(data.session, &ident.name).unwrap().as_class().unwrap();
            let value = classdef.classvars.get_var_def(&String::from("__alloc__")).map(|id| data.get_value(id).unwrap());
            let object = if let Some(function) = value {
                let mut largs = vec!();
                //LLVMBuildCall(data.builder, function, largs.as_mut_ptr(), largs.len() as u32, label("tmp"))
                function.invoke(data, unwind, largs)
            } else {
                //panic!("InternalError: no __alloc__ method for {}", &ident.name);
                let value = data.get_type(data.session.get_ref(*id).unwrap()).unwrap();
                let mem = LLVMBuildMalloc(data.builder, LLVMGetElementType(value.value), label("ptr"));
                let object = LLVMBuildPointerCast(data.builder, mem, value.value, label("ptr"));
                if let Some(index) = classdef.get_struct_vtable_index() {
                    //let vtable = LLVMGetNamedGlobal(data.module, label(format!("__{}_vtable", &ident.name).as_str()));
                    let vtable = data.get_value(scope.variable_id(&format!("__{}_vtable", &ident.name)).unwrap()).unwrap().get_ref();
                    let mut indices = vec!(i32_value(data, 0), i32_value(data, index));
                    let pointer = LLVMBuildGEP(data.builder, object, indices.as_mut_ptr(), indices.len() as u32, label("tmp"));
                    LLVMBuildStore(data.builder, vtable, pointer);
                }
                object
            };

            //let init = classdef.search(&String::from("__init__"), |sym| sym.value.clone());
            //let mut largs = vec!(object);
            //LLVMBuildCall(data.builder, init, largs.as_mut_ptr(), largs.len() as u32, label("tmp"));
            Box::new(Data(object))
        },

        AST::Class(ref id, _, ref classspec, _, ref body) => {
            let tscope = data.map.get(id);
            //let classdef = scope.find_type_def(data.session, &classpec.ident.name).unwrap().as_class().unwrap();

            // TODO you still need to compile the body of the func, if you're going to allow that... like an init function
            //compile_vec(data, func, unwind, tscope.clone(), body);

            Box::new(Data(null_value(str_type(data))))
        },

        AST::Resolver(ref id, ref pos, ref left, ref right) => {
            match **left {
                AST::Identifier(_, _, ref ident) => {
                    //let classdef = scope.find_type_def(data.session, &ident.name).unwrap().as_class().unwrap();
                    //let classid = classdef.classvars.variable_id(&right.name).unwrap();
                    data.get_value(data.session.get_ref(*id).unwrap()).unwrap()
                },
                _ => panic!("SyntaxError:{:?}: left-hand side of scope resolver must be identifier", pos)
            }
        },

        AST::Accessor(ref id, _, ref left, ref right, ref otype) => {
            let object = compile_node(data, func, unwind, scope.clone(), left).get_ref();
// TODO replace use of otype
            //let otype = data.session.get_type_from_ref(left.get_id());
            //debug!("####################: {:?} {:?}", otype, data.session.get_type_from_ref(left.get_id()));

            let ttype = data.session.get_type_from_ref(*id).unwrap();
            let name = otype.clone().unwrap().get_name().unwrap();
            let classdef = scope.find_type_def(data.session, &name).unwrap().as_class().unwrap();
            debug!("*ACCESS: {:?} {:?} {:?}", *id, right, classdef);

            if let Some(structindex) = classdef.get_struct_index(right.as_str()) {
                debug!(">>ACCESS STRUCT: {:?} {:?}", structindex, classdef.get_struct_type(structindex));
                let pointer = build_struct_access(data, classdef.clone(), &right.name, object);
                from_type(&classdef.get_struct_type(structindex), LLVMBuildLoad(data.builder, pointer, label("tmp")))
            } else if let Some(vtableindex) = classdef.get_vtable_index(data.session, scope.clone(), right.as_str(), &ttype) {
                debug!(">>ACCESS VTABLE: {:?} {:?}", vtableindex, classdef.get_vtable_type(vtableindex));
                //debug!("TABLE: {:#?}", classdef.vtable);
                let vindex = classdef.get_struct_vtable_index().unwrap();
                let mut indices = vec!(i32_value(data, 0), i32_value(data, vindex));
                let pointer = LLVMBuildGEP(data.builder, object, indices.as_mut_ptr(), indices.len() as u32, label("tmp"));
                let vtable = LLVMBuildLoad(data.builder, pointer, label("tmp"));

                let mut indices = vec!(i32_value(data, 0), i32_value(data, vtableindex));
                let pointer = LLVMBuildGEP(data.builder, vtable, indices.as_mut_ptr(), indices.len() as u32, label("tmp"));
                from_type(&classdef.get_vtable_type(vtableindex), LLVMBuildLoad(data.builder, pointer, label("tmp")))
            } else {
                debug!("ACCESS DEREF: {:?} -> {:?}", id, data.session.get_ref(*id).unwrap());
                data.get_value(data.session.get_ref(*id).unwrap()).unwrap()
            }
        },

        AST::Assignment(ref id, _, ref left, ref right) => {
            let value = compile_node(data, func, unwind, scope.clone(), right);
            match **left {
                AST::Accessor(ref id, _, ref left, ref right, ref otype) => {
// TODO replace use of otype
                    let name = otype.clone().unwrap().get_name().unwrap();
                    let object = compile_node(data, func, unwind, scope.clone(), left).get_ref();
                    let classdef = scope.find_type_def(data.session, &name).unwrap().as_class().unwrap();
                    let pointer = build_struct_access(data, classdef, &right.name, object);
                    LLVMBuildStore(data.builder, value.get_ref(), pointer)
                },
                _ => panic!("???"),
            };
            value
        },

        AST::Import(ref id, ref pos, ref ident, _) => {
            let module_init_name = format!("init.{}", &ident.name);
            if LLVMGetNamedFunction(data.module, label(module_init_name.as_str())).is_null() {
                let lftype = LLVMFunctionType(bool_type(data), &mut [].as_mut_ptr(), 0, false as i32);
                let function = LLVMAddFunction(data.module, label(module_init_name.as_str()), lftype);

                //let id = scope.define(module_init_name.clone(), Some(ftype.clone()), None).unwrap();
                let id = NodeID::generate();
                let ftype = Type::Function(Box::new(Type::Tuple(vec!())), Box::new(scope.make_obj(data.session, String::from("Bool"), vec!()).unwrap()), ABI::Molten);
                FuncDef::define_func(data.session, scope.clone(), id, &Some(module_init_name.clone()), ABI::C, Some(ftype.clone())).unwrap();
                data.set_value(id, Box::new(Function(function)));
            }
            //compile_node(data, func, None, scope.clone(), &AST::make_invoke(pos.clone(), Box::new(AST::make_ident(pos.clone(), Ident::new(pos.clone(), String::from(module_init_name)))), vec!(), Some(ftype)));
            //compile_node(data, func, None, scope.clone(), &AST::make_invoke(pos.clone(), Box::new(AST::Recall(*id, pos.clone())), vec!(), Some(ftype)));
            build_c_call(data, module_init_name.as_str(), &mut vec!());
            Box::new(Data(ptr::null_mut()))
        },

        AST::Declare(ref id, _, _, _) => { Box::new(Data(zero_int(data))) },

        AST::TypeDef(ref id, _, _, _) => panic!("NotImplementedError: not yet supported, {:?}", node),

        AST::Underscore |
        AST::Index(_, _, _, _, _) => panic!("InternalError: ast element shouldn't appear at this late phase: {:?}", node),
    }
}


unsafe fn collect_functions_vec<'sess>(data: &mut LLVM<'sess>, scope: ScopeRef, items: &'sess Vec<AST>) -> Option<Value> {
    let mut last = None;
    for item in items {
        last = collect_functions_node(data, scope.clone(), item);
    }
    last
}

unsafe fn collect_functions_node<'sess>(data: &mut LLVM<'sess>, scope: ScopeRef, node: &'sess AST) -> Option<Value> {
    match *node {
        AST::Function(ref id, _, ref ident, ref args, _, ref body, ref abi) => {
            let fscope = data.map.get(id);
            let fname = scope.get_full_name(ident, *id);

            let lftype = get_type(data, scope.clone(), data.session.get_type(*id).unwrap(), false);
            let function = build_function_start(data, scope.clone(), *id, ident, lftype, args.len(), *abi);
            //LLVMSetGC(function, label("shadow-stack"));
            //LLVMSetPersonalityFn(function, LLVMGetNamedFunction(data.module, label("__gxx_personality_v0")));

            data.set_value(*id, from_abi(abi, function));

            for (i, ref arg) in args.iter().enumerate() {
                let llarg = LLVMGetParam(function, i as u32);
                LLVMSetValueName(llarg, label(arg.ident.name.as_str()));
                data.set_value(arg.id, from_type(&data.session.get_type(arg.id).unwrap(), llarg));
            }

            collect_functions_node(data, fscope.clone(), body);
            data.functions.push(node);
            return Some(from_abi(abi, function));
        },

        AST::Tuple(ref id, _, ref items) => { collect_functions_vec(data, scope.clone(), items); },
        AST::List(ref id, _, ref items) => { collect_functions_vec(data, scope.clone(), items); },

        AST::Invoke(ref id, _, ref fexpr, ref args) => {
            collect_functions_node(data, scope.clone(), fexpr);
            collect_functions_vec(data, scope.clone(), args);
        },

        AST::SideEffect(ref id, _, _, ref args) => { collect_functions_vec(data, scope.clone(), args); },

        AST::Definition(ref id, _, ref ident, _, ref value) => {
            //collect_functions_node(data, scope.clone(), value);
            if let Some(function) = collect_functions_node(data, scope.clone(), value) {
                data.set_value(*id, function);
            }
        },

        AST::Declare(ref id, _, ref ident, _) => {
            // TODO what to do about abi??
            let ttype = data.session.get_type(*id).unwrap();
            if let Type::Function(ref argtypes, _, _) = ttype {
                let argcount = match **argtypes {
                    Type::Tuple(ref args) => args.len(),
                    _ => 1,
                };
                let lftype = get_type(data, scope.clone(), ttype.clone(), false);
                build_function_start(data, scope.clone(), *id, &Some(ident.clone()), lftype, argcount, ttype.get_abi().unwrap());
            }
        },

        AST::Block(ref id, _, ref body) => { collect_functions_vec(data, scope, body); },

        AST::If(ref id, _, ref cond, ref texpr, ref fexpr) => {
            collect_functions_node(data, scope.clone(), cond);
            collect_functions_node(data, scope.clone(), texpr);
            collect_functions_node(data, scope, fexpr);
        },

        AST::Raise(ref id, _, ref expr) => { collect_functions_node(data, scope, expr); },

        AST::Try(ref id, _, ref cond, ref cases, _) |
        AST::Match(ref id, _, ref cond, ref cases, _) => {
            collect_functions_node(data, scope.clone(), cond);
            for case in cases {
                collect_functions_node(data, scope.clone(), &case.0);
                collect_functions_node(data, scope.clone(), &case.1);
            }
        },

        AST::For(ref id, _, _, ref cond, ref body) => {
            let lscope = data.map.get(id);
            collect_functions_node(data, lscope.clone(), cond);
            collect_functions_node(data, lscope.clone(), body);
        },

        AST::While(ref id, _, ref cond, ref body) => {
            collect_functions_node(data, scope.clone(), cond);
            collect_functions_node(data, scope.clone(), body);
        },

        AST::PtrCast(_, ref code) => {
            collect_functions_node(data, scope.clone(), code);
        },

        AST::New(ref id, _, _) => { },

        AST::Class(ref id, _, ClassSpec { ref ident, .. }, _, ref body) => {
            let tscope = data.map.get(id);
            let classdef = data.session.get_def(*id).unwrap().as_class().unwrap();

            // TODO remove this hardcoded check before constructing
            if ident.name.as_str() != "String" && !ident.name.as_str().contains("closure") {
                classdef.build_vtable(data.session, scope.clone(), body);
            }
            classdef.build_structdef(data.session, scope.clone(), body);

            let lltype = build_class_type(data, scope.clone(), *id, &ident.name, classdef);

            //let alloc = String::from("__alloc__");
            //let classdef = scope.find_type_def(data.session, name).unwrap().as_class().unwrap();
            //if !classdef.contains_local(&alloc) {
            //    debug!("******* CREATING ALLOC: {}", name);
            //    let cname = scope.get_full_name(&Some(name.clone()), id);
            //    let id = classdef.define(alloc.clone(), Some(Type::Function(Box::new(Type::Tuple(vec!())), Box::new(Type::Object(name.clone(), types.clone())))));
            //    data.set_value(id, build_allocator(data, tscope.clone(), &name, format!("{}_{}", cname, alloc).as_str(), lltype));
            //}
            collect_functions_vec(data, tscope, body);
            data.classes.push(node);
        },

        AST::Index(ref id, _, ref left, ref right, _) => {
            collect_functions_node(data, scope.clone(), left);
            collect_functions_node(data, scope, right);
        },

        AST::Resolver(ref id, _, ref left, _) => {
            collect_functions_node(data, scope, left);
        },

        AST::Accessor(ref id, _, ref left, _, _) => {
            collect_functions_node(data, scope, left);
        },

        AST::Assignment(ref id, _, _, ref right) => {
            collect_functions_node(data, scope, right);
        },

        AST::Import(ref id, _, _, ref decls) => {
            collect_functions_vec(data, scope, decls);
        },

        AST::TypeDef(ref id, _, _, _) => { },

        AST::Recall(_, _) |
        AST::Identifier(_, _, _) |
        AST::Underscore | AST::Nil(_) |
        AST::Literal(_, _) => { }
    };
    None
}


pub fn label(string: &str) -> *mut i8 {
    CString::new(string).unwrap().into_raw()
}

pub unsafe fn bool_type(data: &LLVM) -> LLVMTypeRef {
    LLVMInt1TypeInContext(data.context)
}

pub unsafe fn i32_type(data: &LLVM) -> LLVMTypeRef {
    LLVMInt32TypeInContext(data.context)
}

pub unsafe fn int_type(data: &LLVM) -> LLVMTypeRef {
    LLVMInt64TypeInContext(data.context)
}

pub unsafe fn real_type(data: &LLVM) -> LLVMTypeRef {
    LLVMDoubleTypeInContext(data.context)
}

pub unsafe fn str_type(data: &LLVM) -> LLVMTypeRef {
    LLVMPointerType(LLVMInt8TypeInContext(data.context), 0)
}

pub unsafe fn ptr_type(data: &LLVM) -> LLVMTypeRef {
    LLVMPointerType(LLVMPointerType(LLVMInt8TypeInContext(data.context), 0), 0)
}


pub unsafe fn null_value(ttype: LLVMTypeRef) -> LLVMValueRef {
    LLVMConstNull(ttype)
}

pub unsafe fn zero_int(data: &LLVM) -> LLVMValueRef {
    LLVMConstInt(int_type(data), 0, 0)
}

pub unsafe fn i32_value(data: &LLVM, num: usize) -> LLVMValueRef {
    LLVMConstInt(i32_type(data), num as u64, 0)
}

pub unsafe fn int_value(data: &LLVM, num: usize) -> LLVMValueRef {
    LLVMConstInt(int_type(data), num as u64, 0)
}

//pub unsafe fn build_str_const(data: &LLVM, string: &str) -> LLVMValueRef {
//    LLVMBuildGlobalStringPtr(data.builder, label(string), label("str"))
//}

pub unsafe fn build_generic_cast(data: &LLVM, value: LLVMValueRef, ltype: LLVMTypeRef) -> LLVMValueRef {
    if ltype != LLVMTypeOf(value) {
        debug!("{:?} -> {:?}", LLVMGetTypeKind(LLVMTypeOf(value)), LLVMGetTypeKind(ltype));
        if LLVMGetTypeKind(LLVMTypeOf(value)) == LLVMTypeKind::LLVMPointerTypeKind {
            if LLVMGetTypeKind(ltype) == LLVMTypeKind::LLVMPointerTypeKind {
                LLVMBuildPointerCast(data.builder, value, ltype, label("ptr"))
            } else {
                LLVMBuildPtrToInt(data.builder, value, ltype, label("ptr"))
            }
        } else {
            if LLVMGetTypeKind(ltype) == LLVMTypeKind::LLVMPointerTypeKind {
                LLVMBuildIntToPtr(data.builder, value, ltype, label("ptr"))
            } else {
                panic!("I HAVEN'T DONE THIS");
            }
        }
    } else {
        value
    }
}

pub unsafe fn build_cast_to_vartype(data: &LLVM, value: LLVMValueRef) -> LLVMValueRef {
    if LLVMGetTypeKind(LLVMTypeOf(value)) == LLVMTypeKind::LLVMPointerTypeKind {
        LLVMBuildPointerCast(data.builder, value, str_type(data), label("ptr"))
    } else {
        LLVMBuildIntToPtr(data.builder, value, str_type(data), label("ptr"))
    }
}

pub unsafe fn build_cast_from_vartype(data: &LLVM, value: LLVMValueRef, ltype: LLVMTypeRef) -> LLVMValueRef {
    if LLVMGetTypeKind(ltype) == LLVMTypeKind::LLVMPointerTypeKind {
        LLVMBuildPointerCast(data.builder, value, ltype, label("ptr"))
    } else {
        LLVMBuildPtrToInt(data.builder, value, ltype, label("ptr"))
    }
}


//pub unsafe fn get_attribute(data: &LLVM, name: &str) -> LLVMAttributeRef {
//    let kind = LLVMGetEnumAttributeKindForName(label(name), name.len());
//    LLVMCreateEnumAttribute(data.context, kind, 0)
//}

pub unsafe fn build_function_start_lib(data: &LLVM, scope: ScopeRef, id: NodeID, name: &str, mut largs: Vec<LLVMTypeRef>, return_type: LLVMTypeRef) -> LLVMValueRef {
    let lftype = LLVMFunctionType(return_type, largs.as_mut_ptr(), largs.len() as u32, false as i32);
    let function = build_function_start(data, scope, id, &Some(Ident::from_str(name)), lftype, largs.len(), ABI::Molten);

    // TODO maybe these shouldn't be here, but it causes problems for library functions without it
    let bb = LLVMAppendBasicBlockInContext(data.context, function, label("entry"));
    LLVMPositionBuilderAtEnd(data.builder, bb);

    function
}

pub unsafe fn build_function_start(data: &LLVM, scope: ScopeRef, id: NodeID, ident: &Option<Ident>, lftype: LLVMTypeRef, numargs: usize, abi: ABI) -> LLVMValueRef {
    let fname = scope.get_full_name(&ident.clone().map(|ident| Ident::from_str(get_mangled_name(data.session, scope.clone(), &ident.name, id).as_str())), id);
    let function = LLVMAddFunction(data.module, label(fname.as_str()), lftype);

    let nargs = LLVMCountParams(function) as usize;
    if nargs != 0 && nargs != numargs {
        panic!("ArgsError: argument counts don't match");
    }

    data.set_value(id, from_abi(&abi, function));
    function
}

unsafe fn build_function_body(data: &LLVM, node: &AST) {
    if let AST::Function(ref id, _, ref ident, _, _, ref body, _) = *node {
        // TODO do you need to take into account abi?
        let fscope = data.map.get(id);
        let function = data.get_value(*id).unwrap().get_ref();

        let bb = LLVMAppendBasicBlockInContext(data.context, function, label("entry"));
        LLVMPositionBuilderAtEnd(data.builder, bb);
        let ret = compile_node(data, function, None, fscope.clone(), body);
        LLVMBuildRet(data.builder, ret.get_ref());

        //if llvm::analysis::LLVMVerifyFunction(function, llvm::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction) != 0 {
        //    panic!("VerifyError: verification failed");
        //}
        //LLVMRunFunctionPassManager(data.funcpass, function);
    }
}

pub fn get_method(data: &LLVM, scope: ScopeRef, class: &str, method: &str, argtypes: Vec<Type>) -> Box<Compilable> {
    use types::{ find_variant, Check };
    let classdef = if class == "" {
        scope.clone()
    } else {
        scope.find_type_def(data.session, &String::from(class)).unwrap().as_class().unwrap().classvars.clone()
    };

    let name = String::from(method);
    //let ftype = find_variant(data.session, scope.clone(), classdef.get_variable_type(data.session, &name).unwrap(), Type::Tuple(argtypes), Check::Def).unwrap();
    let defid = classdef.get_var_def(&name).unwrap();
    let (defid, ftype) = match data.session.get_def(defid) {
        Ok(Def::Overload(ol)) => ol.find_variant(data.session, scope.clone(), Type::Tuple(argtypes)).unwrap(),
        _ => (defid, data.session.get_type(defid).unwrap()),
    };

    //let funcdefs = classdef.num_funcdefs(data.session, &name);
    //let mname = ABI::Molten.mangle_name(&name, ftype.get_argtypes().unwrap(), funcdefs);
    let function = data.get_value(defid).unwrap();
    function
}

/*
pub unsafe fn build_invoke(data: &LLVM, scope: ScopeRef, unwind: Unwind, classdef: ScopeRef, name: &str, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    let mut dtype = classdef.get_variable_type(data.session, &name).unwrap();
    if dtype.is_overloaded() {
        dtype = types::find_variant(scope.clone(), dtype, ttype.get_argtypes().unwrap().clone(), types::Check::List).unwrap();
    }
    let name = ttype.get_abi().unwrap().mangle_name(name, dtype.get_argtypes().unwrap(), classdef.num_funcdefs(data.session, &name));
    let function = data.get_value(classdef.get_var_def(&name).unwrap()).unwrap();
    debug!("INVOKE: {:?} {:?} {:?}", cname, name, index);

    //let (ttype, funcdefs, function) = scope.search(name, |sym| (sym.ttype.clone(), sym.funcdefs, sym.value.clone()));
    //let name = ABI::Molten.mangle_name(name, argtypes, funcdefs);
    build_cast_from_vartype(data, function.invoke(data, unwind, vec!(list_value, cond_value)), itype);
}
*/

pub unsafe fn build_c_call(data: &LLVM, name: &str, largs: &mut Vec<LLVMValueRef>) -> LLVMValueRef {
    let function = LLVMGetNamedFunction(data.module, label(name));
    LLVMBuildCall(data.builder, function, largs.as_mut_ptr(), largs.len() as u32, label("tmp"))
}

/*
pub unsafe fn build_allocator(data: &LLVM, scope: ScopeRef, cname: &String, fname: &str, lltype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start(data, fname, vec!(), lltype);
    //let obj = build_malloc(data, LLVMSizeOf(LLVMGetElementType(lltype)));
    let mem = LLVMBuildMalloc(data.builder, LLVMGetElementType(lltype), label("ptr"));
    let object = LLVMBuildPointerCast(data.builder, mem, lltype, label("ptr"));

    let structdef = data.get_type(scope.type_id(&cname).unwrap()).unwrap().structdef;
    //for &(name, ttype, expr) in &structdef {
    //    let value = compile_node(data, function, None, scope.clone(), &expr);
    //    let pointer = build_struct_access(data, scope, object, cname, &name);
    //    LLVMBuildStore(data.builder, pointer, value);
    //}

    LLVMBuildRet(data.builder, object);
    function
}
*/

pub unsafe fn build_struct_access(data: &LLVM, classdef: ClassDefRef, field: &String, object: LLVMValueRef) -> LLVMValueRef {
    let index = classdef.get_struct_index(field).unwrap();
    let mut indices = vec!(i32_value(data, 0), i32_value(data, index));
    LLVMBuildGEP(data.builder, object, indices.as_mut_ptr(), indices.len() as u32, label("tmp"))
}

pub unsafe fn build_class_type(data: &LLVM, scope: ScopeRef, id: NodeID, name: &String, classdef: ClassDefRef) -> LLVMTypeRef {
    let (vttype, pvttype) = if classdef.has_vtable() {
        let vtname = format!("{}_vtable", name);
        let vttype = LLVMStructCreateNamed(data.context, label(vtname.as_str()));
        let pvttype = LLVMPointerType(vttype, 0);
        let vtid = classdef.vtable.borrow().id;
        let structdef = StructDef::define_struct(data.session, scope.clone(), vtid, Type::Object(vtname.clone(), vtid, vec!()), None).unwrap();
        data.set_type(vtid, TypeValue { value: pvttype, vttype: None });
        (Some(vttype), Some(pvttype))
    } else {
        (None, None)
    };
    let lltype = LLVMStructCreateNamed(data.context, label(name));
    let pltype = LLVMPointerType(lltype, 0);
    data.set_type(id, TypeValue { value: pltype, vttype: pvttype });

    let mut types = vec!();
    for &(_, ref ttype) in classdef.structdef.borrow().iter() {
        types.push(get_type(data, scope.clone(), ttype.clone(), true))
    }
    LLVMStructSetBody(lltype, types.as_mut_ptr(), types.len() as u32, false as i32);

    if let Some(vttype) = vttype {
        let mut types = vec!();
        for &(_, _, ref ttype) in classdef.vtable.borrow().table.iter() {
            types.push(get_type(data, scope.clone(), ttype.clone(), true))
        }
        LLVMStructSetBody(vttype, types.as_mut_ptr(), types.len() as u32, false as i32);
    }

    pltype
}

pub unsafe fn get_type(data: &LLVM, scope: ScopeRef, ttype: Type, use_fptrs: bool) -> LLVMTypeRef {
    match ttype {
        Type::Object(ref tname, ref id, ref _ptypes) => match tname.as_str() {
            "Nil" => str_type(data),
            "Bool" => bool_type(data),
            "Byte" => LLVMInt8TypeInContext(data.context),
            "Int" => int_type(data),
            "Real" => real_type(data),
            "String" => str_type(data),
            "Buffer" => ptr_type(data),

            //_ => match data.get_type(scope.get_type_def(tname).unwrap()) {
            _ => match data.get_type(*id) {
                Some(typedata) => typedata.value,
                // TODO this should panic...  but Nil doesn't have a value (because it needs to know the type of null pointer it should be)
                //None => LLVMInt64TypeInContext(data.context),
                None => panic!("CompileError: unassigned type value, {:?}", tname),
            }
        },
        Type::Tuple(ref types) => {
            let mut ltypes = vec!();
            for ttype in types {
                ltypes.push(get_type(data, scope.clone(), ttype.clone(), true));
            }
            LLVMStructType(ltypes.as_mut_ptr(), ltypes.len() as u32, false as i32)
        },
        Type::Function(ref args, ref ret, _) => {
            // TODO should you incorporate abi??
            let mut atypes = vec!();
            for ttype in args.get_types().unwrap() {
                atypes.push(get_type(data, scope.clone(), ttype.clone(), true));
            }
            let rtype = get_type(data, scope.clone(), *ret.clone(), true);
            let ftype = LLVMFunctionType(rtype, atypes.as_mut_ptr(), atypes.len() as u32, false as i32);
            if use_fptrs {
                LLVMPointerType(ftype, 0)
            } else {
                ftype
            }
        },
        // TODO this is not the correct way to deal with type variables... there should be overloaded functions generated
        //Type::Variable(_, _) => LLVMInt64TypeInContext(data.context),
        Type::Variable(_, _) => str_type(data), //ptr_type(data),
        _ => panic!("InvalidType: cannot convert to llvm, {:?}", ttype),
    }
}

fn get_mangled_name(session: &Session, scope: ScopeRef, name: &String, id: NodeID) -> String {
    //let dscope = Scope::target(session, scope);
    //let defid = dscope.get_var_def(&name).unwrap();
    //let def = session.get_def(defid).unwrap();

    let ftype = session.get_type(id).unwrap();
    //ftype.get_abi().unwrap_or(ABI::Molten).mangle_name(&name, ftype.get_argtypes().unwrap(), def.num_variants(session))
    ftype.get_abi().unwrap_or(ABI::Molten).mangle_name(&name, ftype.get_argtypes().unwrap(), 2)
}


