
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

use abi::ABI;
use types::{ self, Type };
use utils::UniqueID;
use config::Options;
use session::Session;
use defs::Def;
use scope::{ ScopeRef, ScopeMapRef };
use ast::{ NodeID, Pos, Literal, Ident, ClassSpec, AST };

use defs::classes::{ ClassDefRef, StructDef, StructDefRef };

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
pub struct Closure(pub LLVMValueRef);

impl Compilable for Closure {
    fn box_clone(&self) -> Box<Compilable> {
        Box::new((*self).clone())
    }

    fn get_ref(&self) -> LLVMValueRef {
        self.0
    }

    unsafe fn invoke(&self, data: &LLVM, unwind: Unwind, mut largs: Vec<LLVMValueRef>) -> LLVMValueRef {
LLVMDumpValue(self.0);
        //let func = build_struct_access(data, 0, self.0);
        //LLVMBuildCall(data.builder, func, largs.as_mut_ptr(), largs.len() as u32, label("tmp"))
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


pub fn generate<'sess>(builtins: &Vec<BuiltinDef<'sess>>, session: &'sess Session, transform: &'sess Vec<TopLevel>) {
    unsafe {
        generate_module(builtins, session, session.map.get_global(), transform)
    }
}

unsafe fn generate_module<'sess>(builtins: &Vec<BuiltinDef<'sess>>, session: &'sess Session, scope: ScopeRef, transform: &'sess Vec<TopLevel>) {
    let context = LLVMContextCreate();
    let module = LLVMModuleCreateWithName(label(session.name.as_str()));
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
    generate_declarations(data, scope.clone(), transform);
    generate_vtables(data, scope.clone(), transform);
    generate_definitions(data, scope.clone(), transform);


    // Create a main() function, if this is not a library
    if !Options::as_ref().is_library {
        let module_init_name = format!("init.{}", session.name);
        debug!("CREATING MAIN: {:?}", module_init_name);
        let pos = Pos::empty();
        let function_type = LLVMFunctionType(int_type(data), ptr::null_mut(), 0, 0);
        let function = LLVMAddFunction(module, b"main\0".as_ptr() as *const _, function_type);
        //LLVMSetPersonalityFn(function, LLVMGetNamedFunction(data.module, label("__gxx_personality_v0")));
        LLVMPositionBuilderAtEnd(builder, LLVMAppendBasicBlockInContext(context, function, label("entry")));
        build_c_call(data, module_init_name.as_str(), &mut vec!());
        LLVMBuildRet(builder, int_value(data, 0));
    }

    //optimize_ir(data, 3);

    // Output to a file, and also a string for debugging
    //format!("{}.ll", module_name)
    LLVMPrintModuleToFile(module, label(format!("{}.ll", session.target).as_str()), ptr::null_mut());

    if Options::as_ref().debug {
        println!("{}\n", CString::from_raw(LLVMPrintModuleToString(module)).into_string().unwrap());
    }

    //write_module(data, format!("{}.o", session.target).as_str());

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

pub unsafe fn build_function_start_lib(data: &LLVM, id: NodeID, name: &str, mut largs: Vec<LLVMTypeRef>, return_type: LLVMTypeRef) -> LLVMValueRef {
    let lftype = LLVMFunctionType(return_type, largs.as_mut_ptr(), largs.len() as u32, false as i32);
    let function = build_function_start(data, id, String::from(name), lftype, largs.len(), ABI::Molten);

    // TODO maybe these shouldn't be here, but it causes problems for library functions without it
    let bb = LLVMAppendBasicBlockInContext(data.context, function, label("entry"));
    LLVMPositionBuilderAtEnd(data.builder, bb);

    function
}

pub unsafe fn build_function_start(data: &LLVM, id: NodeID, name: String, lftype: LLVMTypeRef, numargs: usize, abi: ABI) -> LLVMValueRef {
    debug!("BUILD FUNC START: {:?} {:?}", id, name);
    let function = LLVMAddFunction(data.module, label(name.as_str()), lftype);

    let nargs = LLVMCountParams(function) as usize;
    if nargs != 0 && nargs != numargs {
        panic!("ArgsError: argument counts don't match: expected {} but found {}", numargs, nargs);
    }

    data.set_value(id, from_abi(&abi, function));
    function
}

unsafe fn build_function_body(data: &LLVM, id: NodeID, body: &Expr) {
    debug!("BUILD FUNC BODY: {:?}", id);
    // TODO do you need to take into account abi?
    let fscope = data.map.get(&id);
    let function = data.get_value(id).unwrap().get_ref();

    let bb = LLVMAppendBasicBlockInContext(data.context, function, label("entry"));
    LLVMPositionBuilderAtEnd(data.builder, bb);
    let ret = generate_expr(data, function, None, fscope.clone(), body);
    LLVMBuildRet(data.builder, ret.get_ref());

    //if llvm::analysis::LLVMVerifyFunction(function, llvm::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction) != 0 {
    //    panic!("VerifyError: verification failed");
    //}
    //LLVMRunFunctionPassManager(data.funcpass, function);
}

// TODO can we get rid of or refactor this
pub fn get_method(data: &LLVM, scope: ScopeRef, class: &str, method: &str, argtypes: Vec<Type>) -> Box<Compilable> {
    let classdef = if class == "" {
        scope.clone()
    } else {
        scope.find_type_def(data.session, &String::from(class)).unwrap().get_vars().unwrap()
    };

    let name = String::from(method);
    let defid = classdef.get_var_def(&name).unwrap();
    let (defid, ftype) = match data.session.get_def(defid) {
        Ok(Def::Overload(ol)) => ol.find_variant(data.session, scope.clone(), Type::Tuple(argtypes)).unwrap(),
        _ => (defid, data.session.get_type(defid).unwrap()),
    };

    data.get_value(defid).unwrap()
}

pub unsafe fn generate_func_start(data: &LLVM, scope: ScopeRef, id: NodeID, name: String, args: &Vec<(NodeID, String)>) -> Value {
    let ttype = data.session.get_type(id).unwrap();
    let lftype = get_ltype(data, ttype.clone(), false);
    let abi = ttype.get_abi().unwrap();
    let function = build_function_start(data, id, name, lftype, args.len(), abi);
    //LLVMSetGC(function, label("shadow-stack"));
    //LLVMSetPersonalityFn(function, LLVMGetNamedFunction(data.module, label("__gxx_personality_v0")));

    for (i, ref arg) in args.iter().enumerate() {
        let llarg = LLVMGetParam(function, i as u32);
        LLVMSetValueName(llarg, label(arg.1.as_str()));
        data.set_value(arg.0, from_type(&data.session.get_type(arg.0).unwrap(), llarg));
    }

    from_abi(&abi, function)
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

pub unsafe fn build_struct_access(data: &LLVM, index: usize, object: LLVMValueRef) -> LLVMValueRef {
    let mut indices = vec!(i32_value(data, 0), i32_value(data, index));
    LLVMBuildGEP(data.builder, object, indices.as_mut_ptr(), indices.len() as u32, label("tmp"))
}

pub unsafe fn build_class_type(data: &LLVM, scope: ScopeRef, id: NodeID, name: &String, classdef: ClassDefRef) -> LLVMTypeRef {
    let (vttype, pvttype) = if classdef.has_vtable() {
        let vtname = format!("{}_vtable", name);
        let vttype = LLVMStructCreateNamed(data.context, label(vtname.as_str()));
        let pvttype = LLVMPointerType(vttype, 0);
        let vtid = classdef.vtable.id;
        // TODO move this define to class def...
        //let structdef = StructDef::define(data.session, scope.clone(), vtid, Type::Object(vtname.clone(), vtid, vec!())).unwrap();
        data.set_type(vtid, TypeValue { value: pvttype, vttype: None });
        (Some(vttype), Some(pvttype))
    } else {
        (None, None)
    };
    let lltype = LLVMStructCreateNamed(data.context, label(name));
    let pltype = LLVMPointerType(lltype, 0);
    data.set_type(id, TypeValue { value: pltype, vttype: pvttype });

    let mut types = vec!();
    for &(_, ref ttype) in classdef.structdef.fields.borrow().iter() {
        types.push(get_ltype(data, ttype.clone(), true))
    }
    LLVMStructSetBody(lltype, types.as_mut_ptr(), types.len() as u32, false as i32);

    if let Some(vttype) = vttype {
        let mut types = vec!();
        for &(_, _, ref ttype) in classdef.vtable.table.borrow().iter() {
            types.push(get_ltype(data, ttype.clone(), true))
        }
        LLVMStructSetBody(vttype, types.as_mut_ptr(), types.len() as u32, false as i32);
    }

    pltype
}

pub unsafe fn build_struct_type(data: &LLVM, id: NodeID, name: &String, structdef: StructDefRef) -> LLVMTypeRef {
    let lltype = LLVMStructCreateNamed(data.context, label(name));
    let pltype = LLVMPointerType(lltype, 0);
    data.set_type(id, TypeValue { value: pltype, vttype: None });

    let mut types = vec!();
    for &(_, ref ttype) in structdef.fields.borrow().iter() {
        types.push(get_ltype(data, ttype.clone(), true))
    }
    LLVMStructSetBody(lltype, types.as_mut_ptr(), types.len() as u32, false as i32);

    pltype
}

/*
pub unsafe fn build_vtable_type(data: &LLVM, id: NodeID, name: &String, vtable: &Vtable) -> LLVMTypeRef {
    let vtname = format!("{}_vtable", name);
    let vttype = LLVMStructCreateNamed(data.context, label(vtname.as_str()));
    let pvttype = LLVMPointerType(vttype, 0);
    // TODO move this define to class def...
    //let structdef = StructDef::define(data.session, scope.clone(), vtid, Type::Object(vtname.clone(), vtable.id, vec!())).unwrap();
    data.set_type(vtable.id, TypeValue { value: pvttype, vttype: None });

    let mut types = vec!();
    for &(_, _, ref ttype) in vtable.table.borrow().iter() {
        types.push(get_ltype(data, ttype.clone(), true))
    }
    LLVMStructSetBody(vttype, types.as_mut_ptr(), types.len() as u32, false as i32);

    pvttype
}
*/


pub unsafe fn get_ltype(data: &LLVM, ttype: Type, use_fptrs: bool) -> LLVMTypeRef {
    match ttype {
        Type::Object(ref tname, ref id, ref _ptypes) => match tname.as_str() {
            "Nil" => str_type(data),
            "Bool" => bool_type(data),
            "Byte" => LLVMInt8TypeInContext(data.context),
            "Int" => int_type(data),
            "Real" => real_type(data),
            "String" => str_type(data),
            "Buffer" => ptr_type(data),

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
                ltypes.push(get_ltype(data, ttype.clone(), true));
            }
            LLVMStructType(ltypes.as_mut_ptr(), ltypes.len() as u32, false as i32)
        },
        Type::Function(ref args, ref ret, _) => {
            // TODO should you incorporate abi??
            let mut atypes = vec!();
            for ttype in args.get_types().unwrap() {
                atypes.push(get_ltype(data, ttype.clone(), true));
            }
            let rtype = get_ltype(data, *ret.clone(), true);
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





use llvm::transform::{ TopLevel, TopKind, Expr, ExprKind, InvokeKind };

pub unsafe fn generate_declarations(data: &LLVM, scope: ScopeRef, transform: &Vec<TopLevel>) {
    for element in transform {
        match element.kind {
            TopKind::Global(ref name) => {
                let ltype = get_ltype(data, data.session.get_type(element.id).unwrap(), true);
                let global = LLVMAddGlobal(data.module, ltype, label(name.as_str()));
                LLVMSetInitializer(global, null_value(ltype));
                data.set_value(element.id, Box::new(Global(global)));
            },
            TopKind::Func(ref name, ref args, _) |
            TopKind::Method(ref name, ref args, _) => {
                generate_func_start(data, scope.clone(), element.id, name.clone(), args);
            },
            TopKind::Closure(ref fid, ref fname, ref name, ref args, _) => {
                let cl = data.session.get_def(element.id).unwrap().as_closure().unwrap();
                build_struct_type(data, cl.contextid, name, cl.context.clone());
                generate_func_start(data, scope.clone(), *fid, fname.clone(), args);
            },
            TopKind::Declare(ref name) => {
                let ttype = data.session.get_type(element.id).unwrap();
                if let Type::Function(ref argtypes, _, _) = ttype {
                    let argcount = match **argtypes {
                        Type::Tuple(ref args) => args.len(),
                        _ => 1,
                    };
                    let lftype = get_ltype(data, ttype.clone(), false);
                    build_function_start(data, element.id, name.clone(), lftype, argcount, ttype.get_abi().unwrap());
                }
            },
            TopKind::ClassDef(ref name, ref body) => {
                let classdef = data.session.get_def(element.id).unwrap().as_class().unwrap();

                let lltype = build_class_type(data, scope.clone(), element.id, name, classdef.clone());

                //let alloc = String::from("__alloc__");
                //let classdef = scope.find_type_def(data.session, name).unwrap().as_class().unwrap();
                //if !classdef.contains_local(&alloc) {
                //    debug!("******* CREATING ALLOC: {}", name);
                //    let cname = scope.get_full_name(&Some(name.clone()), id);
                //    let id = classdef.define(alloc.clone(), Some(Type::Function(Box::new(Type::Tuple(vec!())), Box::new(Type::Object(name.clone(), types.clone())))));
                //    data.set_value(id, build_allocator(data, tscope.clone(), &name, format!("{}_{}", cname, alloc).as_str(), lltype));
                //}

            },
        }
    }
}

pub unsafe fn generate_vtables(data: &LLVM, scope: ScopeRef, transform: &Vec<TopLevel>) {
    for element in transform {
        match element.kind {
            TopKind::ClassDef(ref name, ref body) => {
                let tscope = data.session.map.get(&element.id);
                let classdef = data.session.get_def(element.id).unwrap().as_class().unwrap();

                if classdef.has_vtable() {
                    let mut methods = vec!();
                    for (index, &(ref id, ref name, ref ttype)) in classdef.vtable.table.borrow().iter().enumerate() {
                        let dtype = data.session.get_type(*id).unwrap();
                        debug!("VTABLE INIT: {:?} {:?} {:?}", name, id, index);
                        methods.push(build_generic_cast(data, data.get_value(*id).unwrap().get_ref(), get_ltype(data, ttype.clone(), true)));
                    }

                    let value = data.get_type(element.id).unwrap();
                    let vtype = value.vttype.unwrap();
                    let global = LLVMAddGlobal(data.module, LLVMGetElementType(vtype), label(format!("__{}_vtable", name).as_str()));
                    LLVMSetInitializer(global, LLVMConstNamedStruct(vtype, methods.as_mut_ptr(), methods.len() as u32));
                    LLVMSetLinkage(global, LLVMLinkage::LLVMLinkOnceAnyLinkage);
                    data.set_value(classdef.vtable.id, Box::new(Global(global)));
                }
            },
            _ => { },
        }
    }
}

pub unsafe fn generate_definitions(data: &LLVM, scope: ScopeRef, transform: &Vec<TopLevel>) {
    for element in transform {
        match element.kind {
            TopKind::Func(_, _, ref body) => {
                build_function_body(data, element.id, body);
            },
            TopKind::Method(_, _, ref body) => {
                build_function_body(data, element.id, body);
            },
            _ => { },
        }
    }
}

pub unsafe fn generate_expr_vec(data: &LLVM, func: LLVMValueRef, unwind: Unwind, scope: ScopeRef, exprs: &Vec<Expr>) -> Value {
    let mut last: Value = Box::new(Data(zero_int(data)));
    for expr in exprs {
        last = generate_expr(data, func, unwind, scope.clone(), expr);
    }
    last
}

pub unsafe fn generate_expr(data: &LLVM, func: LLVMValueRef, unwind: Unwind, scope: ScopeRef, expr: &Expr) -> Value {
    debug!("CODEGEN: {:?}", expr);
    match expr.kind {
        ExprKind::Nil => Box::new(Data(null_value(get_ltype(data, data.session.get_type(expr.id).unwrap(), true)))),
        ExprKind::Literal(ref lit) => {
            match lit {
                Literal::Boolean(ref num) => Box::new(Data(LLVMConstInt(bool_type(data), *num as u64, 0))),
                Literal::Integer(ref num) => Box::new(Data(LLVMConstInt(int_type(data), *num as u64, 0))),
                Literal::Real(ref num) => Box::new(Data(LLVMConstReal(real_type(data), *num))),
                Literal::String(ref string) => Box::new(Data(LLVMBuildGlobalStringPtr(data.builder, label(string.as_str()), label("__string")))),
            }
        },

        ExprKind::Block(ref body) => { generate_expr_vec(data, func, unwind, scope.clone(), body) },

        ExprKind::Invoke(ref invoke, ref args) => {
            let (atypes, rtype, abi) = get_function_types(data, expr.id);
            let mut largs = generate_invoke_args(data, func, unwind, scope.clone(), &atypes, args);
            let function = match invoke {
                InvokeKind::Method(ref objexpr, ref field, ref oid) => {
                    let object = largs[0];
                    let otype = data.session.get_type(*oid).unwrap();
                    data.set_value(*oid, from_type(&otype, object));
                    //let object = generate_expr(data, func, unwind, scope.clone(), objexpr).get_ref();
                    //largs.insert(0, object);
                    generate_access_field(data, func, unwind, scope.clone(), expr.id, object, field, *oid)
                },
                InvokeKind::Closure(ref fexpr) => {
                    let closure = generate_expr(data, func, unwind, scope.clone(), fexpr).get_ref();
                    largs.insert(0, closure);
                    Box::new(Closure(closure))
                },
                InvokeKind::CFunc(ref fexpr) |
                InvokeKind::Func(ref fexpr) => generate_expr(data, func, unwind, scope.clone(), fexpr),
                InvokeKind::CByName(ref name) => Box::new(CFunction(LLVMGetNamedFunction(data.module, label(name.as_str())))),
            };
            //if !function.get_ref().is_null() { LLVMDumpValue(function.get_ref()); println!(""); }

            generate_cast_args(data, function.get_ref(), &mut largs);
            let value = function.invoke(data, unwind, largs);
            // TODO this is perhaps an alternate way of doing calls?
            //let value = match invoke {
            //    InvokeKind::Func(_) |
            //    InvokeKind::Method(_, _, _) => invoke_molten_function(data, function.get_ref(), unwind, largs),
            //
            //    InvokeKind::CFunc(_) |
            //    InvokeKind::CByName(_) => invoke_c_function(data, function.get_ref(), unwind, largs),
            //};
            let value = generate_cast(data, rtype.clone(), value);

            from_type(&rtype, value)
        },


        /////// Variables ///////

        ExprKind::DefVar(ref name, ref value) => {
            let ltype = get_ltype(data, data.session.get_type(expr.id).unwrap(), true);
            let pointer = Box::new(Var(LLVMBuildAlloca(data.builder, ltype, label(name.as_str()))));
            data.set_value(expr.id, pointer.clone());

            let value = generate_expr(data, func, unwind, scope.clone(), value);
            LLVMBuildStore(data.builder, build_generic_cast(data, value.get_ref(), ltype), pointer.get_ref());
            value
        },

        ExprKind::DefGlobal(ref name, ref value) => {
            let pointer = data.get_value(expr.id).unwrap();

            let ltype = get_ltype(data, data.session.get_type(expr.id).unwrap(), true);
            let value = generate_expr(data, func, unwind, scope.clone(), value);
            LLVMBuildStore(data.builder, build_generic_cast(data, value.get_ref(), ltype), pointer.get_ref());
            value
        },

        ExprKind::AccessVar(ref name) => {
            let defid = data.session.get_ref(expr.id).unwrap();
            let pointer = match data.get_value(defid) {
                Some(x) => x,
                None => panic!("UnsetError:{:?}: use before assignment {:?}", expr.pos, name),
            };
            let ttype = data.session.get_type(defid).unwrap();
            from_type(&ttype, LLVMBuildLoad(data.builder, pointer.get_ref(), label(name.as_str())))
        },

        ExprKind::AccessValue(_) => {
            debug!("ACCESSING: {:?}", expr.id);
            match data.get_value(data.session.get_ref(expr.id).unwrap()) {
                Some(lvalue) => lvalue,
                None => panic!("UnsetError:{:?}: use before assignment", expr.pos),
            }
        },

        ExprKind::SetClosure(ref value) => {
            let cl = data.session.get_def(expr.id).unwrap().as_closure().unwrap();
            //let value = data.get_value(cl.varid).unwrap();
            let value = generate_expr(data, func, unwind, scope.clone(), value);
LLVMDumpValue(value.get_ref());
            data.set_value(expr.id, value.clone());
            value
        },


        /////// Objects ///////

        ExprKind::AccessField(ref objexpr, ref field, ref oid) => {
            let object = generate_expr(data, func, unwind, scope.clone(), objexpr).get_ref();
            //let structdef = data.session.get_def_from_ref(objexpr.id).unwrap().as_struct().unwrap();
            generate_access_field(data, func, unwind, scope.clone(), expr.id, object, field, *oid)
        },

        ExprKind::AllocObject(ref name) => {
            let def = data.session.get_def_from_ref(expr.id).unwrap();
            let value = def.get_vars().unwrap().get_var_def(&String::from("__alloc__")).map(|id| data.get_value(id).unwrap());
            let object = if let Some(function) = value {
                let mut largs = vec!();
                //LLVMBuildCall(data.builder, function, largs.as_mut_ptr(), largs.len() as u32, label("tmp"))
                function.invoke(data, unwind, largs)
            } else {
                let value = data.get_type(data.session.get_ref(expr.id).unwrap()).unwrap();
                let mem = LLVMBuildMalloc(data.builder, LLVMGetElementType(value.value), label("ptr"));
                let object = LLVMBuildPointerCast(data.builder, mem, value.value, label("ptr"));
                if let Def::Class(classdef) = def {
                    if let Some(index) = classdef.get_struct_vtable_index() {
                        let vtable = data.get_value(classdef.vtable.id).unwrap().get_ref();
                        let mut indices = vec!(i32_value(data, 0), i32_value(data, index));
                        let pointer = LLVMBuildGEP(data.builder, object, indices.as_mut_ptr(), indices.len() as u32, label("tmp"));
                        LLVMBuildStore(data.builder, vtable, pointer);
                    }
                }
                object
            };

            //let init = classdef.search(&String::from("__init__"), |sym| sym.value.clone());
            //let mut largs = vec!(object);
            //LLVMBuildCall(data.builder, init, largs.as_mut_ptr(), largs.len() as u32, label("tmp"));
            Box::new(Data(object))
        },

        ExprKind::AccessMethod => {
            data.get_value(data.session.get_ref(expr.id).unwrap()).unwrap()
        },

        ExprKind::AssignField(ref objexpr, ref field, ref valexpr, ref oid) => {
            let value = generate_expr(data, func, unwind, scope.clone(), valexpr);

            let otype = data.session.get_type(*oid).unwrap();
            let object = generate_expr(data, func, unwind, scope.clone(), objexpr).get_ref();
// TODO this needs to be changed... you need to change build_struct_access probabl
            let structdef = data.session.get_def(otype.get_id().unwrap()).unwrap().as_struct().unwrap();
            let pointer = build_struct_access(data, structdef.get_index(field.as_str()).unwrap(), object);
            LLVMBuildStore(data.builder, value.get_ref(), pointer);

            value
        },


        /////// Flow Control ///////

        ExprKind::If(ref cond, ref texpr, ref fexpr) => {
            let cond_value = generate_expr(data, func, unwind, scope.clone(), cond).get_ref();
            let cond_zero = LLVMConstInt(LLVMTypeOf(cond_value), 0, 0);
            let is_nonzero = LLVMBuildICmp(data.builder, LLVMIntPredicate::LLVMIntNE, cond_value, cond_zero, label("is_nonzero"));

            let texpr_block = LLVMAppendBasicBlockInContext(data.context, func, label("ifthen"));
            let fexpr_block = LLVMAppendBasicBlockInContext(data.context, func, label("ifelse"));
            let merge_block = LLVMAppendBasicBlockInContext(data.context, func, label("ifend"));

            LLVMBuildCondBr(data.builder, is_nonzero, texpr_block, fexpr_block);


            LLVMPositionBuilderAtEnd(data.builder, texpr_block);
            let texpr_value = generate_expr(data, func, unwind, scope.clone(), texpr).get_ref();
            LLVMBuildBr(data.builder, merge_block);
            let texpr_block = LLVMGetInsertBlock(data.builder);

            LLVMPositionBuilderAtEnd(data.builder, fexpr_block);
            let fexpr_value = generate_expr(data, func, unwind, scope.clone(), fexpr).get_ref();
            LLVMBuildBr(data.builder, merge_block);
            let fexpr_block = LLVMGetInsertBlock(data.builder);

            LLVMPositionBuilderAtEnd(data.builder, merge_block);
            let phi = LLVMBuildPhi(data.builder, LLVMTypeOf(texpr_value), label("iftmp"));

            let mut values = vec![texpr_value, fexpr_value];
            let mut blocks = vec![texpr_block, fexpr_block];

            LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);
            Box::new(Data(phi))
        },

        ExprKind::SideEffect(ref op, ref args) => {
            // TODO This only handles two arguments, which is all that will be parsed, but you can do better... 
            let lexpr_block = LLVMAppendBasicBlockInContext(data.context, func, label(op.as_str()));
            let rexpr_block = LLVMAppendBasicBlockInContext(data.context, func, label(op.as_str()));
            let merge_block = LLVMAppendBasicBlockInContext(data.context, func, label(op.as_str()));

            LLVMBuildBr(data.builder, lexpr_block);

            LLVMPositionBuilderAtEnd(data.builder, lexpr_block);
            let lexpr_value = generate_expr(data, func, unwind, scope.clone(), &args[0]).get_ref();
            let test_type = match op.as_str() {
                "or" => LLVMIntPredicate::LLVMIntNE,
                "and" => LLVMIntPredicate::LLVMIntEQ,
                _ => panic!("NotImplementedError: attempted to compile invalid side effect operation: {}", op.as_str())
            };
            let is_enough = LLVMBuildICmp(data.builder, test_type, lexpr_value, null_value(LLVMTypeOf(lexpr_value)), label("is_enough"));
            LLVMBuildCondBr(data.builder, is_enough, merge_block, rexpr_block);

            LLVMPositionBuilderAtEnd(data.builder, rexpr_block);
            let rexpr_value = generate_expr(data, func, unwind, scope.clone(), &args[1]).get_ref();
            LLVMBuildBr(data.builder, merge_block);

            LLVMPositionBuilderAtEnd(data.builder, merge_block);
            let phi = LLVMBuildPhi(data.builder, LLVMTypeOf(lexpr_value), label(op.as_str()));

            let mut values = vec![lexpr_value, rexpr_value];
            let mut blocks = vec![lexpr_block, rexpr_block];

            LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);
            Box::new(Data(phi))
        },


        ExprKind::Match(ref cond, ref cases, ref cid) => {
            let mut cond_blocks = vec!();
            let mut do_blocks = vec!();
            for _ in 0 .. cases.len() {
                cond_blocks.push(LLVMAppendBasicBlockInContext(data.context, func, label("matchcond")));
                do_blocks.push(LLVMAppendBasicBlockInContext(data.context, func, label("matchdo")));
            }
            let merge_block = LLVMAppendBasicBlockInContext(data.context, func, label("matchend"));
            cond_blocks.push(merge_block);

            let cond_value = generate_expr(data, func, unwind, scope.clone(), cond).get_ref();
            LLVMBuildBr(data.builder, cond_blocks[0]);

            let ctype = data.session.get_type(*cid).unwrap();
            let mut values = vec!();
            for (i, &(ref case, ref expr)) in cases.iter().enumerate() {
                LLVMPositionBuilderAtEnd(data.builder, cond_blocks[i]);
                match case.kind {
                    ExprKind::Underscore => { LLVMBuildBr(data.builder, do_blocks[i]); },
                    _ => {
                        let case_value = generate_expr(data, func, unwind, scope.clone(), case).get_ref();
                        //let is_true = LLVMBuildICmp(data.builder, LLVMIntPredicate::LLVMIntEQ, cond_value, case_value, label("is_true"));
                        let eqfunc = get_method(data, scope.clone(), "", "==", vec!(ctype.clone(), ctype.clone()));
                        let is_true = eqfunc.invoke(data, unwind, vec!(cond_value, case_value));
                        LLVMBuildCondBr(data.builder, is_true, do_blocks[i], cond_blocks[i + 1]);
                    }
                }

                LLVMPositionBuilderAtEnd(data.builder, do_blocks[i]);
                values.push(generate_expr(data, func, unwind, scope.clone(), expr).get_ref());
                LLVMBuildBr(data.builder, merge_block);
                do_blocks[i] = LLVMGetInsertBlock(data.builder);
            }

            LLVMPositionBuilderAtEnd(data.builder, merge_block);
            let phi = LLVMBuildPhi(data.builder, LLVMTypeOf(values[0]), label("matchphi"));

            LLVMAddIncoming(phi, values.as_mut_ptr(), do_blocks.as_mut_ptr(), values.len() as u32);
            Box::new(Data(phi))
        },



        ExprKind::While(ref cond, ref body) => {
            let before_block = LLVMAppendBasicBlockInContext(data.context, func, label("while"));
            let body_block = LLVMAppendBasicBlockInContext(data.context, func, label("whilebody"));
            let after_block = LLVMAppendBasicBlockInContext(data.context, func, label("whileend"));

            LLVMBuildBr(data.builder, before_block);
            LLVMPositionBuilderAtEnd(data.builder, before_block);
            let cond_value = generate_expr(data, func, unwind, scope.clone(), cond).get_ref();
            let cond_zero = LLVMConstInt(LLVMTypeOf(cond_value), 0, 0);
            let is_nonzero = LLVMBuildICmp(data.builder, LLVMIntPredicate::LLVMIntNE, cond_value, cond_zero, label("is_nonzero"));
            LLVMBuildCondBr(data.builder, is_nonzero, body_block, after_block);

            LLVMPositionBuilderAtEnd(data.builder, body_block);
            //let body_value = compile_node(data, func, unwind, scope.clone(), body).get_ref();
            generate_expr(data, func, unwind, scope.clone(), body).get_ref();
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

        ExprKind::For(ref name, ref list, ref body) => {
            let lscope = data.map.get(&expr.id);
            let listdef = scope.find_type_def(data.session, &String::from("List")).unwrap().as_class().unwrap();

            let list_value = generate_expr(data, func, unwind, scope.clone(), list).get_ref();
            let inc = LLVMBuildAlloca(data.builder, int_type(data), label("inc"));
            LLVMBuildStore(data.builder, zero_int(data), inc);

            let itype = get_ltype(data, lscope.get_variable_type(data.session, &name).unwrap(), true);
            let item = LLVMBuildAlloca(data.builder, itype, label("item"));
            data.set_value(expr.id, Box::new(Var(item)));

            let before_block = LLVMAppendBasicBlockInContext(data.context, func, label("for"));
            let body_block = LLVMAppendBasicBlockInContext(data.context, func, label("forbody"));
            let after_block = LLVMAppendBasicBlockInContext(data.context, func, label("forend"));

            LLVMBuildBr(data.builder, before_block);
            LLVMPositionBuilderAtEnd(data.builder, before_block);
            //let cond_value = generate_expr(data, func, unwind, scope.clone(), cond).get_ref();

            let cond_value = LLVMBuildLoad(data.builder, inc, label("tmp"));
            let cond_length = data.get_value(listdef.structdef.vars.get_var_def(&String::from("len")).unwrap()).unwrap().invoke(data, unwind, vec!(list_value));
            let is_end = LLVMBuildICmp(data.builder, LLVMIntPredicate::LLVMIntSGE, cond_value, cond_length, label("is_end"));
            LLVMBuildCondBr(data.builder, is_end, after_block, body_block);

            LLVMPositionBuilderAtEnd(data.builder, body_block);
            let nextitem = build_cast_from_vartype(data, data.get_value(listdef.structdef.vars.get_var_def(&String::from("get")).unwrap()).unwrap().invoke(data, unwind, vec!(list_value, cond_value)), itype);
            LLVMBuildStore(data.builder, nextitem, item);

            let body_value = generate_expr(data, func, unwind, lscope.clone(), body).get_ref();

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


        /////// Exceptions ///////

        // TODO you might need to create a new function to hold the body, so that it can be resumed from, and there also
        // might be an issue when you call a function one level down, and pass it the same unwind locations... it might effect all
        // functions, all the way down
        ExprKind::Try(ref body, ref cases, ref cid) => {
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

        ExprKind::Raise(ref expr) => {
            //LLVMBuildResume(data.builder, compile_node(data, func, unwind, scope.clone(), expr))
            Box::new(Data(null_value(str_type(data))))
        },


        /////// Miscellaneous ///////

        ExprKind::Tuple(ref items) => {
            let ttype = data.session.get_type(expr.id).unwrap();
            let ltype = get_ltype(data, ttype, true);
            let tuple = LLVMBuildAlloca(data.builder, ltype, label("tuple"));
            for (index, item) in items.iter().enumerate() {
                let value = generate_expr(data, func, unwind, scope.clone(), item).get_ref();
                let mut indices = vec!(i32_value(data, 0), i32_value(data, index));
                let pointer = LLVMBuildGEP(data.builder, tuple, indices.as_mut_ptr(), indices.len() as u32, label("tmp"));
                LLVMBuildStore(data.builder, value, pointer);
            }
            Box::new(Var(tuple))
        },

        ExprKind::PtrCast(ref ttype, ref code) => {
            let mut value = generate_expr(data, func, unwind, scope.clone(), code).get_ref();
            let ltype = get_ltype(data, ttype.clone(), true);
            value = build_generic_cast(data, value, ltype);
            from_type(ttype, value)
        },

        ExprKind::Underscore => panic!("FIX THIS BY MAKING A PATTERN TREE"),
    }
}


pub unsafe fn generate_access_field(data: &LLVM, func: LLVMValueRef, unwind: Unwind, scope: ScopeRef, id: NodeID, object: LLVMValueRef, name: &String, oid: NodeID) -> Value {
// TODO can we remove oid from everything???
    let otype = data.session.get_type(oid).unwrap();
    //let otype = data.session.get_type_from_ref(objexpr.get_id()).unwrap();

    let ttype = data.session.get_type_from_ref(id).unwrap();
    let classdef = data.session.get_def(otype.get_id().unwrap()).unwrap().as_class().unwrap();
    debug!("*ACCESS: {:?} {:?} {:?}", id, name, classdef);

    if let Some(structindex) = classdef.get_struct_index(name.as_str()) {
        debug!(">>ACCESS STRUCT: {:?} {:?}", structindex, classdef.get_struct_type(structindex));
        let pointer = build_struct_access(data, classdef.get_struct_index(name.as_str()).unwrap(), object);
        from_type(&classdef.get_struct_type(structindex), LLVMBuildLoad(data.builder, pointer, label("tmp")))
    } else if let Some(vtableindex) = classdef.get_vtable_index(data.session, scope.clone(), name.as_str(), &ttype) {
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
        debug!("ACCESS DEREF: {:?} -> {:?}", id, data.session.get_ref(id).unwrap());
        data.get_value(data.session.get_ref(id).unwrap()).unwrap()
    }
}


pub fn get_function_types(data: &LLVM, id: NodeID) -> (Type, Type, ABI) {
    let ftype = data.session.get_type(id).unwrap();
    match ftype.clone() {
        Type::Function(atypes, rtype, abi) => (*atypes, *rtype, abi),
        ftype @ _ => panic!("TypeError: expected function type: {:?}", ftype),
    }
}

pub unsafe fn generate_invoke_args(data: &LLVM, func: LLVMValueRef, unwind: Unwind, scope: ScopeRef, atypes: &Type, args: &Vec<Expr>) -> Vec<LLVMValueRef> {
    let mut largs = vec!();
    // TODO this forces the function arg type to be a tuple
    for (ttype, arg) in atypes.get_types().unwrap().iter().zip(args.iter()) {
        let mut larg = generate_expr(data, func, unwind, scope.clone(), arg).get_ref();
        let ltype = get_ltype(data, ttype.clone(), true);
        if ltype != LLVMTypeOf(larg) {
            // TODO this seems to cast to int as well as pointers, so maybe it's doing too much, at least without checking that it's supposed to
            larg = LLVMBuildPointerCast(data.builder, larg, ltype, label("ptr"));
        }
        largs.push(larg);
    }
    largs
}

pub unsafe fn generate_cast_args(data: &LLVM, function: LLVMValueRef, largs: &mut Vec<LLVMValueRef>) {
    if !function.is_null() {
        // Cast values to the function's declared type; this is a hack for typevar/generic arguments
        let mut lftype = LLVMTypeOf(function);
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
}

pub unsafe fn generate_cast(data: &LLVM, rtype: Type, value: LLVMValueRef) -> LLVMValueRef {
    let lrtype = get_ltype(data, rtype, true);
    build_generic_cast(data, value, lrtype)
}

pub unsafe fn invoke_molten_function(data: &LLVM, func: LLVMValueRef, unwind: Unwind, mut largs: Vec<LLVMValueRef>) -> LLVMValueRef {
    //LLVMBuildCall(data.builder, self.0, largs.as_mut_ptr(), largs.len() as u32, label("tmp"))
    match unwind {
        None => LLVMBuildCall(data.builder, func, largs.as_mut_ptr(), largs.len() as u32, label("thingies")),
        Some((then, catch)) => {
            LLVMBuildInvoke(data.builder, func, largs.as_mut_ptr(), largs.len() as u32, then, catch, label("tmp"))
            //LLVMSetUnwindDest(invoke, unwind.unwrap());
        }
    }
}

pub unsafe fn invoke_c_function(data: &LLVM, func: LLVMValueRef, unwind: Unwind, mut largs: Vec<LLVMValueRef>) -> LLVMValueRef {
    LLVMBuildCall(data.builder, func, largs.as_mut_ptr(), largs.len() as u32, label("tmp"))
}

pub unsafe fn build_c_call(data: &LLVM, name: &str, largs: &mut Vec<LLVMValueRef>) -> LLVMValueRef {
    let function = LLVMGetNamedFunction(data.module, label(name));
    LLVMBuildCall(data.builder, function, largs.as_mut_ptr(), largs.len() as u32, label("tmp"))
}

