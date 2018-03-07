
use std::ptr;
use std::fmt::Debug;
use std::collections::HashMap;
use std::collections::hash_map::Entry;


extern crate llvm_sys as llvm;
use self::llvm::prelude::*;
use self::llvm::core::*;

use parser::{ parse_type };
use scope::{ self, Scope, ScopeRef, ScopeMapRef, Context };
use binding::{ declare_typevars };
use types::{ Type, check_type, Check };
use compiler_llvm::*;
use utils::UniqueID;


pub type RuntimeFunction = unsafe fn(&LLVM, &str, LLVMTypeRef) -> LLVMValueRef;
pub type ComptimeFunction = unsafe fn(&LLVM, Vec<LLVMValueRef>) -> LLVMValueRef;

#[derive(Clone)]
pub enum Func {
    Undefined,
    External,
    Runtime(RuntimeFunction),
    Comptime(ComptimeFunction)
}

#[derive(Clone)]
pub enum Builtin<'a> {
    Type(&'a str, Type),
    Func(&'a str, &'a str, Func),
    Class(&'a str, Vec<(String, Type)>, Vec<Builtin<'a>>),
}

#[derive(Clone)]
pub struct BuiltinMap<'a> (HashMap<&'a str, Vec<(ComptimeFunction, Type)>>);

impl<'a> BuiltinMap<'a> {
    pub fn new() -> BuiltinMap<'a> {
        BuiltinMap(HashMap::new())
    }

    pub fn add(&mut self, name: &'a str, func: ComptimeFunction, ftype: Type) {
        match self.0.entry(name) {
            Entry::Vacant(entry) => { entry.insert(vec!((func, ftype))); },
            Entry::Occupied(mut entry) => { entry.get_mut().push((func, ftype)); },
        }
    }

    pub unsafe fn compile_builtin(data: &LLVM, scope: ScopeRef<Value, TypeValue>, name: &String, largs: &Vec<LLVMValueRef>, stype: Type) -> Option<LLVMValueRef> {
        let name = if let Some(uname) = scope::unmangle_name(name) {
            uname
        } else {
            name.clone()
        };

        match data.builtins.0.get(name.as_str()) {
            Some(ref list) => {
                for ref entry in list.iter() {
                    match check_type(scope.clone(), Some(entry.1.clone()), Some(stype.clone()), Check::Def, false) {
                        Ok(_) => return Some(entry.0(data, largs.clone())),
                        Err(_) => { },
                    };
                }
                None
            },
            None => None,
        }
    }
}

pub fn make_global<'a, V, T>(map: ScopeMapRef<V, T>, builtins: &Vec<Builtin<'a>>) where V: Clone + Debug, T: Clone + Debug {
    let primatives = map.add(ScopeMapRef::<V, T>::PRIMATIVE, None);
    primatives.borrow_mut().set_context(Context::Primative);

    register_builtins_vec(primatives.clone(), primatives.clone(), builtins);

    let global = map.add(ScopeMapRef::<V, T>::GLOBAL, Some(primatives));
    global.borrow_mut().set_context(Context::Global);
}
 
pub fn register_builtins_vec<'a, V, T>(scope: ScopeRef<V, T>, tscope: ScopeRef<V, T>, entries: &Vec<Builtin<'a>>) where V: Clone + Debug, T: Clone + Debug {
    for node in entries {
        register_builtins_node(scope.clone(), tscope.clone(), node);
    }
}

pub fn register_builtins_node<'a, V, T>(scope: ScopeRef<V, T>, tscope: ScopeRef<V, T>, node: &Builtin<'a>) where V: Clone + Debug, T: Clone + Debug {
    match *node {
        Builtin::Type(ref name, ref ttype) => {
            let mut ttype = ttype.clone();
            declare_typevars(tscope.clone(), Some(&mut ttype), true).unwrap();
            scope.borrow_mut().define_type(String::from(*name), ttype.clone()).unwrap();
        },
        Builtin::Func(ref name, ref ftype, _) => {
            let mut ftype = parse_type(ftype);
            declare_typevars(tscope.clone(), ftype.as_mut(), false).unwrap();
            Scope::define_func_variant(scope.clone(), String::from(*name), tscope.clone(), ftype.clone().unwrap()).unwrap();
        },
        Builtin::Class(ref name, _, ref entries) => {
            let name = String::from(*name);
            let classdef = Scope::new_ref(None);
            classdef.borrow_mut().set_basename(name.clone());
            scope.borrow_mut().set_class_def(&name, None, classdef.clone());

            let tscope = Scope::new_ref(Some(scope.clone()));
            register_builtins_vec(classdef.clone(), tscope.clone(), entries);
        },
    }
}

pub unsafe fn initialize_builtins<'a>(data: &mut LLVM<'a>, scope: ScopeRef<Value, TypeValue>, entries: &Vec<Builtin<'a>>) {
    let pscope = scope.borrow().get_parent().unwrap().clone();
    declare_builtins_vec(data, ptr::null_mut(), pscope.clone(), scope.clone(), entries);
    declare_irregular_functions(data, pscope.clone());
}

pub unsafe fn declare_builtins_vec<'a>(data: &mut LLVM<'a>, objtype: LLVMTypeRef, scope: ScopeRef<Value, TypeValue>, tscope: ScopeRef<Value, TypeValue>, entries: &Vec<Builtin<'a>>) {
    for node in entries {
        declare_builtins_node(data, objtype, scope.clone(), tscope.clone(), node);
    }
}

pub unsafe fn declare_builtins_node<'a>(data: &mut LLVM<'a>, objtype: LLVMTypeRef, scope: ScopeRef<Value, TypeValue>, tscope: ScopeRef<Value, TypeValue>, node: &Builtin<'a>) {
    match *node {
        Builtin::Type(ref name, ref ttype) => { },
        Builtin::Func(ref sname, ref types, ref func) => {
            let mut name = String::from(*sname);
            let ftype = parse_type(types).unwrap();
            match *func {
                Func::External => {
                    let func = LLVMAddFunction(data.module, label(name.as_str()), get_type(data, scope.clone(), ftype, false));
                    if scope.borrow().contains(&name) {
                        scope.borrow_mut().assign(&name, func);
                    }
                },
                Func::Runtime(func) => {
                    if scope.borrow().get_variable_type(&name).unwrap().is_overloaded() {
                        name = scope::mangle_name(&name, ftype.get_argtypes().unwrap());
                        scope.borrow_mut().define(name.clone(), Some(ftype)).unwrap();
                    }
                    let fname = scope.borrow().get_full_name(&Some(name.clone()), UniqueID(0));
                    scope.borrow_mut().assign(&name, func(data, fname.as_str(), objtype));
                },
                Func::Comptime(func) => {
                    data.builtins.add(sname, func, ftype);
                },
                _ => { },
            }
        },
        Builtin::Class(ref name, ref structdef, ref entries) => {
            let name = String::from(*name);
            let classdef = scope.borrow().get_class_def(&name);

            //let tscope = Scope::new_ref(Some(scope.clone()));
            let lltype = if structdef.len() > 0 {
                build_class_type(data, scope.clone(), &name, structdef.clone(), vec!())
            } else {
                let lltype = get_type(data, scope.clone(), scope.borrow().find_type(&name).unwrap(), true);
                scope.borrow_mut().set_type_value(&name, TypeValue { structdef: vec!(), value: lltype, vtable: vec!(), vttype: None });
                lltype
            };

            declare_builtins_vec(data, lltype, classdef.clone(), tscope.clone(), entries);
        },
    }
}


pub unsafe fn declare_function(module: LLVMModuleRef, scope: ScopeRef<Value, TypeValue>, name: &str, args: &mut [LLVMTypeRef], ret_type: LLVMTypeRef, vargs: bool) {
    let ftype = LLVMFunctionType(ret_type, args.as_mut_ptr(), args.len() as u32, vargs as i32);
    let func = LLVMAddFunction(module, label(name), ftype);
    let name = &String::from(name);
    if scope.borrow().contains(name) {
        scope.borrow_mut().assign(name, func);
    }
}

unsafe fn declare_irregular_functions(data: &LLVM, scope: ScopeRef<Value, TypeValue>) {
    let bytestr_type = LLVMPointerType(LLVMInt8Type(), 0);
    //let cint_type = LLVMInt32TypeInContext(data.context);
    let cint_type = int_type(data);

    //declare_function(data.module, scope.clone(), "malloc", &mut [cint_type], bytestr_type, false);
    //declare_function(data.module, scope.clone(), "realloc", &mut [bytestr_type, cint_type], bytestr_type, false);
    //declare_function(data.module, scope.clone(), "free", &mut [bytestr_type], LLVMVoidType(), false);

    //declare_function(data.module, scope.clone(), "strlen", &mut [bytestr_type], cint_type, false);
    //declare_function(data.module, scope.clone(), "memcpy", &mut [bytestr_type, bytestr_type, cint_type], bytestr_type, false);

    //declare_function(data.module, scope.clone(), "puts", &mut [bytestr_type], cint_type, false);
    declare_function(data.module, scope.clone(), "sprintf", &mut [bytestr_type, bytestr_type], cint_type, true);

    declare_function(data.module, scope.clone(), "llvm.pow.f64", &mut [real_type(data), real_type(data)], real_type(data), false);

    //declare_function(data.module, scope.clone(), "__gxx_personality_v0", &mut [bytestr_type, bytestr_type], cint_type, true);
}


pub fn get_builtins<'a>() -> Vec<Builtin<'a>> {
    vec!(
        Builtin::Func("malloc",     "(Int) -> 'ptr",                Func::External),
        Builtin::Func("realloc",    "('ptr, Int) -> 'ptr",          Func::External),
        Builtin::Func("free",       "('ptr) -> Nil",                Func::External),
        Builtin::Func("memcpy",     "('ptr, 'ptr, Int) -> 'ptr",    Func::External),
        Builtin::Func("strcmp",     "(String, String) -> Int",      Func::External),
        Builtin::Func("puts",       "(String) -> Nil",              Func::External),
        Builtin::Func("gets",       "(String) -> String",           Func::External),
        Builtin::Func("strlen",     "(String) -> Int",              Func::External),
        Builtin::Func("sprintf",    "'tmp",                         Func::Undefined),

        Builtin::Func("println",    "(String) -> Nil",              Func::Runtime(build_lib_println)),
        Builtin::Func("readline",   "() -> String",                 Func::Runtime(build_lib_readline)),


        Builtin::Type("Nil",    Type::Object(String::from("Nil"), vec!())),
        Builtin::Type("Bool",   Type::Object(String::from("Bool"), vec!())),
        Builtin::Type("Byte",   Type::Object(String::from("Byte"), vec!())),
        Builtin::Type("Int",    Type::Object(String::from("Int"), vec!())),
        Builtin::Type("Real",   Type::Object(String::from("Real"), vec!())),
        Builtin::Type("String", Type::Object(String::from("String"), vec!())),
        //Builtin::Type("Class",  Type::Object(String::from("Class"), vec!())),
        Builtin::Type("Buffer", Type::Object(String::from("Buffer"), vec!(Type::Variable(String::from("item"), UniqueID(0))))),
        Builtin::Type("List",   Type::Object(String::from("List"), vec!(Type::Variable(String::from("item"), UniqueID(0))))),

        Builtin::Class("Int", vec!(), vec!(
            Builtin::Func("+",   "(Int, Int) -> Int", Func::Comptime(add_int)),
            Builtin::Func("add", "(Int, Int) -> Int", Func::Runtime(build_lib_add)),
        )),

        /*
        Builtin::Class("String", vec!(), vec!(
            //Builtin::Func("push", "(String, String) -> String", Func::Comptime(add_int)),
            Builtin::Func("[]",   "(String, Int) -> Int",       Func::Runtime(build_string_get)),
        )),
        */
        Builtin::Func("getindex",   "(String, Int) -> Int",       Func::Runtime(build_string_get)),

        Builtin::Class("Buffer", vec!(), vec!(
            Builtin::Func("__alloc__",  "() -> Buffer<'item>",                      Func::Runtime(build_buffer_allocator)),
            Builtin::Func("new",        "(Buffer<'item>, Int) -> Buffer<'item>",    Func::Runtime(build_buffer_constructor)),
            Builtin::Func("resize",     "(Buffer<'item>, Int) -> Buffer<'item>",    Func::Runtime(build_buffer_resize)),
            Builtin::Func("[]",         "(Buffer<'item>, Int) -> 'item",            Func::Runtime(build_buffer_get)),
            Builtin::Func("[]",         "(Buffer<'item>, Int, 'item) -> 'item",     Func::Runtime(build_buffer_set)),
        )),

        /*
        Builtin::Class("List", vec!(
            (String::from("items"), parse_type("'item").unwrap()),
            (String::from("size"), parse_type("Int").unwrap()),
            (String::from("capacity"), parse_type("Int").unwrap()),
        ), vec!(
            Builtin::Func("new",  "(List<'item>) -> List<'item>",       Func::Runtime(build_list_constructor)),
            Builtin::Func("push", "(List<'item>, 'item) -> Int",        Func::Runtime(build_list_push)),
            Builtin::Func("[]",   "(List<'item>, Int) -> 'item",        Func::Runtime(build_list_get)),
            Builtin::Func("[]",   "(List<'item>, Int, 'item) -> 'item", Func::Runtime(build_list_set)),
        )),
        */


        Builtin::Func("+",   "(Int, Int) -> Int",  Func::Comptime(add_int)),
        Builtin::Func("-",   "(Int, Int) -> Int",  Func::Comptime(sub_int)),
        Builtin::Func("*",   "(Int, Int) -> Int",  Func::Comptime(mul_int)),
        Builtin::Func("/",   "(Int, Int) -> Int",  Func::Comptime(div_int)),
        Builtin::Func("%",   "(Int, Int) -> Int",  Func::Comptime(mod_int)),
        //Builtin::Func("^",   "(Int, Int) -> Int",  Func::Comptime(pow_int)),
        //Builtin::Func("<<",  "(Int, Int) -> Int",  Func::Comptime(shl_int)),
        //Builtin::Func(">>",  "(Int, Int) -> Int",  Func::Comptime(shr_int)),
        Builtin::Func("&",   "(Int, Int) -> Int",  Func::Comptime(and_int)),
        Builtin::Func("|",   "(Int, Int) -> Int",  Func::Comptime(or_int)),
        Builtin::Func("<",   "(Int, Int) -> Bool", Func::Comptime(lt_int)),
        Builtin::Func(">",   "(Int, Int) -> Bool", Func::Comptime(gt_int)),
        Builtin::Func("<=",  "(Int, Int) -> Bool", Func::Comptime(lte_int)),
        Builtin::Func(">=",  "(Int, Int) -> Bool", Func::Comptime(gte_int)),
        Builtin::Func("==",  "(Int, Int) -> Bool", Func::Comptime(eq_int)),
        Builtin::Func("!=",  "(Int, Int) -> Bool", Func::Comptime(ne_int)),
        Builtin::Func("~",   "(Int) -> Int",       Func::Comptime(com_int)),
        Builtin::Func("not", "(Int) -> Bool",      Func::Comptime(not_int)),


        Builtin::Func("+",   "(Real, Real) -> Real",  Func::Comptime(add_real)),
        Builtin::Func("-",   "(Real, Real) -> Real",  Func::Comptime(sub_real)),
        Builtin::Func("*",   "(Real, Real) -> Real",  Func::Comptime(mul_real)),
        Builtin::Func("/",   "(Real, Real) -> Real",  Func::Comptime(div_real)),
        Builtin::Func("%",   "(Real, Real) -> Real",  Func::Comptime(mod_real)),
        Builtin::Func("^",   "(Real, Real) -> Real",  Func::Comptime(pow_real)),
        Builtin::Func("<",   "(Real, Real) -> Bool",  Func::Comptime(lt_real)),
        Builtin::Func(">",   "(Real, Real) -> Bool",  Func::Comptime(gt_real)),
        Builtin::Func("<=",  "(Real, Real) -> Bool",  Func::Comptime(lte_real)),
        Builtin::Func(">=",  "(Real, Real) -> Bool",  Func::Comptime(gte_real)),
        Builtin::Func("==",  "(Real, Real) -> Bool",  Func::Comptime(eq_real)),
        Builtin::Func("!=",  "(Real, Real) -> Bool",  Func::Comptime(ne_real)),
        Builtin::Func("not", "(Real) -> Bool",        Func::Comptime(not_real)),


        Builtin::Func("==",  "(Bool, Bool) -> Bool",  Func::Comptime(eq_bool)),
        Builtin::Func("!=",  "(Bool, Bool) -> Bool",  Func::Comptime(ne_bool)),
        Builtin::Func("not", "(Bool) -> Bool",        Func::Comptime(not_bool)),
    )
}

fn add_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildAdd(data.builder, args[0], args[1], label("tmp")) } }
fn sub_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildSub(data.builder, args[0], args[1], label("tmp")) } }
fn mul_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildMul(data.builder, args[0], args[1], label("tmp")) } }
fn div_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildSDiv(data.builder, args[0], args[1], label("tmp")) } }
fn mod_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildSRem(data.builder, args[0], args[1], label("tmp")) } }
fn and_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildAnd(data.builder, args[0], args[1], label("tmp")) } }
fn or_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildOr(data.builder, args[0], args[1], label("tmp")) } }
fn eq_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntEQ, args[0], args[1], label("tmp")) } }
fn ne_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntNE, args[0], args[1], label("tmp")) } }
fn lt_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntSLT, args[0], args[1], label("tmp")) } }
fn gt_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntSGT, args[0], args[1], label("tmp")) } }
fn lte_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntSLE, args[0], args[1], label("tmp")) } }
fn gte_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntSGE, args[0], args[1], label("tmp")) } }
fn com_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildXor(data.builder, args[0], int_value(data, 0xFFFFFFFFFFFFFFFF), label("tmp")) } }
fn not_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildNot(data.builder, args[0], label("tmp")) } }

fn add_real(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFAdd(data.builder, args[0], args[1], label("tmp")) } }
fn sub_real(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFSub(data.builder, args[0], args[1], label("tmp")) } }
fn mul_real(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFMul(data.builder, args[0], args[1], label("tmp")) } }
fn div_real(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFDiv(data.builder, args[0], args[1], label("tmp")) } }
fn mod_real(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFRem(data.builder, args[0], args[1], label("tmp")) } }
fn pow_real(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { build_call(data, "llvm.pow.f64", &mut vec!(args[0], args[1])) } }
fn eq_real(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOEQ, args[0], args[1], label("tmp")) } }
fn ne_real(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealONE, args[0], args[1], label("tmp")) } }
fn lt_real(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOLT, args[0], args[1], label("tmp")) } }
fn gt_real(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOGT, args[0], args[1], label("tmp")) } }
fn lte_real(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOLE, args[0], args[1], label("tmp")) } }
fn gte_real(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOGE, args[0], args[1], label("tmp")) } }
fn not_real(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildNot(data.builder, args[0], label("tmp")) } }

fn eq_bool(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntEQ, args[0], args[1], label("tmp")) } }
fn ne_bool(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntNE, args[0], args[1], label("tmp")) } }
fn not_bool(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildNot(data.builder, args[0], label("tmp")) } }


unsafe fn build_buffer_allocator(data: &LLVM, name: &str, objtype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start(data, name, vec!(), objtype);
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);
    LLVMBuildRet(data.builder, null_value(objtype));
    function
}

unsafe fn build_buffer_constructor(data: &LLVM, name: &str, objtype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start(data, name, vec!(objtype, int_type(data)), objtype);
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);

    let ptr = LLVMBuildArrayMalloc(data.builder, LLVMInt64TypeInContext(data.context), LLVMGetParam(function, 1), label("tmp"));
    let castptr = LLVMBuildPointerCast(data.builder, ptr, objtype, label("ptr"));
    LLVMBuildRet(data.builder, castptr);
    function
}

unsafe fn build_buffer_resize(data: &LLVM, name: &str, objtype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start(data, name, vec!(objtype, int_type(data)), objtype);
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);

    let buffer = LLVMBuildPointerCast(data.builder, LLVMGetParam(function, 0), str_type(data), label("tmp"));
    let size = LLVMBuildMul(data.builder, LLVMGetParam(function, 1), LLVMSizeOf(LLVMInt64TypeInContext(data.context)), label("tmp"));
    let newptr = build_call(data, "realloc", &mut vec!(buffer, size));
    let castptr = LLVMBuildPointerCast(data.builder, newptr, objtype, label("ptr"));
    LLVMBuildRet(data.builder, castptr);
    function
}

unsafe fn build_buffer_get(data: &LLVM, name: &str, objtype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start(data, name, vec!(objtype, int_type(data)), str_type(data));
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);

    let buffer = LLVMGetParam(function, 0);
    let index = LLVMBuildCast(data.builder, llvm::LLVMOpcode::LLVMTrunc, LLVMGetParam(function, 1), i32_type(data), label("tmp"));
    let mut indices = vec!(index);
    let pointer = LLVMBuildGEP(data.builder, buffer, indices.as_mut_ptr(), indices.len() as u32, label("tmp"));
    let value = LLVMBuildLoad(data.builder, pointer, label("tmp"));

    LLVMBuildRet(data.builder, value);
    //LLVMBuildRet(data.builder, null_value(str_type(data)));
    function
}

unsafe fn build_buffer_set(data: &LLVM, name: &str, objtype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start(data, name, vec!(objtype, int_type(data), str_type(data)), str_type(data));
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);

    let buffer = LLVMGetParam(function, 0);
    let index = LLVMBuildCast(data.builder, llvm::LLVMOpcode::LLVMTrunc, LLVMGetParam(function, 1), i32_type(data), label("tmp"));
    let mut indices = vec!(index);
    let pointer = LLVMBuildGEP(data.builder, buffer, indices.as_mut_ptr(), indices.len() as u32, label("tmp"));
    let value = build_cast_to_vartype(data, LLVMGetParam(function, 2));
    LLVMBuildStore(data.builder, value, pointer);

    //LLVMBuildRet(data.builder, value);
    LLVMBuildRet(data.builder, null_value(str_type(data)));
    function
}

/*
unsafe fn build_list_constructor(data: &LLVM, name: &str, objtype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start(data, name, vec!(objtype), objtype);
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);

    //let raw_ptr = build_malloc(data, LLVMSizeOf(LLVMGetElementType(objtype)));
    //let ptr = LLVMBuildPointerCast(data.builder, raw_ptr, objtype, label("ptr"));
    LLVMBuildRet(data.builder, LLVMGetParam(function, 0));
    function
}

unsafe fn build_list_push(data: &LLVM, name: &str, objtype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start(data, name, vec!(objtype, str_type(data)), int_type(data));
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);

    //let raw_ptr = build_malloc(data, LLVMSizeOf(LLVMGetElementType(objtype)));
    //let ptr = LLVMBuildPointerCast(data.builder, raw_ptr, objtype, label("ptr"));
    LLVMBuildRet(data.builder, int_value(data, 0));
    function
}

unsafe fn build_list_get(data: &LLVM, name: &str, objtype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start(data, name, vec!(objtype, int_type(data)), str_type(data));
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);

    let list = LLVMGetParam(function, 0);
    let mut indices = vec!(i32_value(data, 0), i32_value(data, 0));
    let pointer = LLVMBuildGEP(data.builder, list, indices.as_mut_ptr(), indices.len() as u32, label("tmp"));
    let array = LLVMBuildLoad(data.builder, pointer, label("tmp"));

    let array = LLVMBuildPointerCast(data.builder, array, ptr_type(data), label("ptr"));
    let index = LLVMBuildCast(data.builder, llvm::LLVMOpcode::LLVMTrunc, LLVMGetParam(function, 1), i32_type(data), label("tmp"));
    let mut indices = vec!(index);
    let pointer = LLVMBuildGEP(data.builder, array, indices.as_mut_ptr(), indices.len() as u32, label("tmp"));
    let value = LLVMBuildLoad(data.builder, pointer, label("tmp"));

    LLVMBuildRet(data.builder, value);
    //LLVMBuildRet(data.builder, null_value(str_type(data)));
    function
}

unsafe fn build_list_set(data: &LLVM, name: &str, objtype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start(data, name, vec!(objtype, int_type(data), str_type(data)), str_type(data));
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);

    let list = LLVMGetParam(function, 0);
    let mut indices = vec!(i32_value(data, 0), i32_value(data, 0));
    let pointer = LLVMBuildGEP(data.builder, list, indices.as_mut_ptr(), indices.len() as u32, label("tmp"));
    let array = LLVMBuildLoad(data.builder, pointer, label("tmp"));

    let array = LLVMBuildPointerCast(data.builder, array, ptr_type(data), label("ptr"));
    let index = LLVMBuildCast(data.builder, llvm::LLVMOpcode::LLVMTrunc, LLVMGetParam(function, 1), i32_type(data), label("tmp"));
    let mut indices = vec!(index);
    let pointer = LLVMBuildGEP(data.builder, array, indices.as_mut_ptr(), indices.len() as u32, label("tmp"));
    let value = build_cast_to_vartype(data, LLVMGetParam(function, 2));
    LLVMBuildStore(data.builder, value, pointer);

    //LLVMBuildRet(data.builder, value);
    LLVMBuildRet(data.builder, null_value(str_type(data)));
    function
}
*/

unsafe fn build_string_get(data: &LLVM, name: &str, objtype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start(data, name, vec!(str_type(data), int_type(data)), int_type(data));
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);

    let string = LLVMGetParam(function, 0);
    let mut indices = vec!(LLVMGetParam(function, 1));
    let pointer = LLVMBuildGEP(data.builder, string, indices.as_mut_ptr(), indices.len() as u32, label("tmp"));
    let value = LLVMBuildLoad(data.builder, pointer, label("tmp"));
    let value = LLVMBuildCast(data.builder, llvm::LLVMOpcode::LLVMZExt, value, int_type(data), label("tmp"));
    
    LLVMBuildRet(data.builder, value);
    function
}

unsafe fn build_lib_add(data: &LLVM, name: &str, objtype: LLVMTypeRef) -> LLVMValueRef {
    let dtype = int_type(data);
    let mut atypes = vec!(dtype, dtype);
    let function = LLVMAddFunction(data.module, label("builtin.add"), LLVMFunctionType(dtype, atypes.as_mut_ptr(), atypes.len() as u32, false as i32));
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);
    //LLVMAddAttributeAtIndex(function, 0, get_attribute(data, "inlinealways"));
    LLVMPositionBuilderAtEnd(data.builder, LLVMAppendBasicBlockInContext(data.context, function, label("entry")));
    let value = LLVMBuildAdd(data.builder, LLVMGetParam(function, 0), LLVMGetParam(function, 1), label("tmp"));
    LLVMBuildRet(data.builder, value);
    function
}



unsafe fn build_lib_println(data: &LLVM, name: &str, objtype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start(data, name, vec!(str_type(data)), str_type(data));
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);

    let value = build_call(data, "puts", &mut vec!(LLVMGetParam(function, 0)));
    LLVMBuildRet(data.builder, value);
    function
}

unsafe fn build_lib_readline(data: &LLVM, name: &str, objtype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start(data, name, vec!(), str_type(data));
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);

    let buffer = build_call(data, "malloc", &mut vec!(int_value(data, 2048)));
    build_call(data, "gets", &mut vec!(buffer));
    let len = build_call(data, "strlen", &mut vec!(buffer));
    let value = build_call(data, "realloc", &mut vec!(buffer, len));
    LLVMBuildRet(data.builder, value);
    function
}


//    let mut types = vec!(cint_type, bool_type(data));
//    let rtype = LLVMStructTypeInContext(data.context, types.as_mut_ptr(), types.len() as u32, false as i32);

//    declare_function(data.module, pscope.clone(), "llvm.sadd.with.overflow.i64", &mut [cint_type, cint_type], rtype, false);


