
use std::ptr;
use std::rc::Rc;
use std::fmt::Debug;
//use std::collections::HashMap;
//use std::collections::hash_map::Entry;


extern crate llvm_sys as llvm;
use self::llvm::prelude::*;
use self::llvm::core::*;

use abi::ABI;
use types::Type;
use classes::ClassDef;
use ast::{ Pos, Ident, ClassSpec };
use parser::{ parse_type };
use scope::{ Scope, ScopeRef, ScopeMapRef, Context };
use binding::{ declare_typevars };
use utils::UniqueID;

use llvm::compiler::*;


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
pub enum BuiltinDef<'sess> {
    Type(&'sess str, Type),
    Func(&'sess str, &'sess str, Func),
    Class(&'sess str, Vec<Type>, Vec<(String, Type)>, Vec<BuiltinDef<'sess>>),
}


pub fn make_global<'sess>(map: &'sess ScopeMapRef, builtins: &Vec<BuiltinDef<'sess>>) {
    let primatives = map.add(ScopeMapRef::PRIMATIVE, None);
    primatives.set_context(Context::Primative);

    register_builtins_vec(primatives.clone(), primatives.clone(), builtins);

    let global = map.add(ScopeMapRef::GLOBAL, Some(primatives));
    global.set_context(Context::Global);
}
 
pub fn register_builtins_vec<'sess>(scope: ScopeRef, tscope: ScopeRef, entries: &Vec<BuiltinDef<'sess>>) {
    for node in entries {
        register_builtins_node(scope.clone(), tscope.clone(), node);
    }
}

pub fn register_builtins_node<'sess>(scope: ScopeRef, tscope: ScopeRef, node: &BuiltinDef<'sess>) {
    match *node {
        BuiltinDef::Type(ref name, ref ttype) => {
            let mut ttype = ttype.clone();
            declare_typevars(tscope.clone(), Some(&mut ttype), true).unwrap();
            scope.define_type(String::from(*name), ttype.clone()).unwrap();
            let classdef = ClassDef::new_ref(ClassSpec::new(Pos::empty(), Ident::from_str(ttype.get_name().unwrap().as_str()), ttype.get_params().unwrap()), None, Scope::new_ref(None));
            scope.set_class_def(&String::from(*name), classdef);
        },
        BuiltinDef::Func(ref name, ref ftype, _) => {
            let mut ftype = parse_type(ftype);
            declare_typevars(tscope.clone(), ftype.as_mut(), false).unwrap();
            Scope::define_func_variant(scope, String::from(*name), tscope.clone(), ftype.clone().unwrap()).unwrap();
        },
        BuiltinDef::Class(ref name, ref params, _, ref entries) => {
            //let cname = String::from(*name);
            //let classdef = Scope::new_ref(None);
            //classdef.set_basename(cname.clone());
            //scope.set_class_def(&cname, None, classdef.clone());
            let classdef = ClassDef::define_class(scope.clone(), &ClassSpec::new(Pos::empty(), Ident::from_str(name), params.clone()), None).unwrap();

            let tscope = Scope::new_ref(Some(scope.clone()));
            register_builtins_vec(classdef.classvars.clone(), tscope.clone(), entries);
        },
    }
}

pub unsafe fn initialize_builtins<'sess>(data: &mut LLVM<'sess>, scope: ScopeRef, entries: &Vec<BuiltinDef<'sess>>) {
    let pscope = scope.get_parent().unwrap();
    declare_builtins_vec(data, ptr::null_mut(), pscope.clone(), scope.clone(), entries);
    declare_irregular_functions(data, pscope.clone());
}

pub unsafe fn declare_builtins_vec<'sess>(data: &mut LLVM<'sess>, objtype: LLVMTypeRef, scope: ScopeRef, tscope: ScopeRef, entries: &Vec<BuiltinDef<'sess>>) {
    for node in entries {
        declare_builtins_node(data, objtype, scope.clone(), tscope.clone(), node);
    }
}

pub unsafe fn declare_builtins_node<'sess>(data: &mut LLVM<'sess>, objtype: LLVMTypeRef, scope: ScopeRef, tscope: ScopeRef, node: &BuiltinDef<'sess>) {
    match *node {
        BuiltinDef::Type(ref name, ref ttype) => { },
        BuiltinDef::Func(ref sname, ref types, ref func) => {
            let mut name = String::from(*sname);
            let ftype = parse_type(types).unwrap();
            match *func {
                Func::External => {
                    let func = LLVMAddFunction(data.module, label(name.as_str()), get_type(data, scope.clone(), ftype.clone(), false));
                    if scope.contains(&name) {
                        data.set_value(scope.variable_id(&name).unwrap(), from_abi(&ftype.get_abi().unwrap(), func));
                    }
                },
                Func::Runtime(func) => {
                    let sftype = scope.get_variable_type(&name).unwrap();
                    name = ftype.get_abi().unwrap_or(ABI::Molten).mangle_name(&name, ftype.get_argtypes().unwrap(), sftype.num_funcdefs());
                    if !scope.contains(&name) {
                        scope.define(name.clone(), Some(ftype.clone())).unwrap();
                    }
                    let fname = scope.get_full_name(&Some(Ident::new(Pos::empty(), name.clone())), UniqueID(0));
                    data.set_value(scope.variable_id(&name).unwrap(), from_type(&ftype, func(data, fname.as_str(), objtype)));
                },
                Func::Comptime(func) => {
                    //data.builtins.add(sname, func, ftype.clone());
                    let sftype = scope.get_variable_type(&name).unwrap();
                    name = ftype.get_abi().unwrap_or(ABI::Molten).mangle_name(&name, ftype.get_argtypes().unwrap(), sftype.num_funcdefs());
                    if !scope.contains(&name) {
                        scope.define(name.clone(), Some(ftype.clone())).unwrap();
                    }
                    data.set_value(scope.variable_id(&name).unwrap(), Box::new(Builtin(BuiltinFunction(func), ftype)));
                },
                _ => { },
            }
        },
        BuiltinDef::Class(ref name, _, ref structdef, ref entries) => {
            let cname = String::from(*name);
            let classdef = scope.get_class_def(&cname);

            let lltype = if structdef.len() > 0 {
                *classdef.structdef.borrow_mut() = structdef.clone();
                build_class_type(data, scope.clone(), &cname, classdef.clone())
            } else {
                let lltype = get_type(data, scope.clone(), scope.find_type(&cname).unwrap(), true);
                data.set_type(scope.type_id(&cname).unwrap(), TypeValue { value: lltype, vttype: None });
                lltype
            };

            declare_builtins_vec(data, lltype, classdef.classvars.clone(), tscope.clone(), entries);
        },
    }
}


pub unsafe fn declare_c_function(data: &LLVM, scope: ScopeRef, name: &str, args: &mut [LLVMTypeRef], ret_type: LLVMTypeRef, vargs: bool) {
    let ftype = LLVMFunctionType(ret_type, args.as_mut_ptr(), args.len() as u32, vargs as i32);
    let func = LLVMAddFunction(data.module, label(name), ftype);
    let name = &String::from(name);
    if scope.contains(name) {
        data.set_value(scope.variable_id(name).unwrap(), Box::new(CFunction(func)));
    }
}

unsafe fn declare_irregular_functions(data: &LLVM, scope: ScopeRef) {
    let bytestr_type = LLVMPointerType(LLVMInt8Type(), 0);
    //let cint_type = LLVMInt32TypeInContext(data.context);
    let cint_type = int_type(data);

    //declare_function(data, scope.clone(), "malloc", &mut [cint_type], bytestr_type, false);
    //declare_function(data, scope.clone(), "realloc", &mut [bytestr_type, cint_type], bytestr_type, false);
    //declare_function(data, scope.clone(), "free", &mut [bytestr_type], LLVMVoidType(), false);

    //declare_function(data, scope.clone(), "strlen", &mut [bytestr_type], cint_type, false);
    //declare_function(data, scope.clone(), "memcpy", &mut [bytestr_type, bytestr_type, cint_type], bytestr_type, false);

    //declare_function(data, scope.clone(), "puts", &mut [bytestr_type], cint_type, false);
    declare_c_function(data, scope.clone(), "sprintf", &mut [bytestr_type, bytestr_type], cint_type, true);

    declare_c_function(data, scope.clone(), "llvm.pow.f64", &mut [real_type(data), real_type(data)], real_type(data), false);

    //declare_function(data, scope.clone(), "__gxx_personality_v0", &mut [bytestr_type, bytestr_type], cint_type, true);
}


pub fn get_builtins<'sess>() -> Vec<BuiltinDef<'sess>> {
    vec!(
        BuiltinDef::Func("malloc",     "(Int) -> 'ptr / C",             Func::External),
        BuiltinDef::Func("realloc",    "('ptr, Int) -> 'ptr / C",       Func::External),
        BuiltinDef::Func("free",       "('ptr) -> Nil / C",             Func::External),
        BuiltinDef::Func("memcpy",     "('ptr, 'ptr, Int) -> 'ptr / C", Func::External),
        BuiltinDef::Func("strcmp",     "(String, String) -> Int / C",   Func::External),
        BuiltinDef::Func("puts",       "(String) -> Nil / C",           Func::External),
        BuiltinDef::Func("gets",       "(String) -> String / C",        Func::External),
        BuiltinDef::Func("strlen",     "(String) -> Int / C",           Func::External),
        //BuiltinDef::Func("sprintf",    "'tmp",                          Func::Undefined),
        BuiltinDef::Func("sprintf",    "(String, String, 'sess1, 'sess2) -> Nil / C", Func::Undefined),

        BuiltinDef::Func("println",    "(String) -> Nil / C",           Func::Runtime(build_lib_println)),
        BuiltinDef::Func("readline",   "() -> String / C",              Func::Runtime(build_lib_readline)),


        BuiltinDef::Type("Nil",    Type::Object(String::from("Nil"), vec!())),
        BuiltinDef::Type("Bool",   Type::Object(String::from("Bool"), vec!())),
        BuiltinDef::Type("Byte",   Type::Object(String::from("Byte"), vec!())),
        BuiltinDef::Type("Real",   Type::Object(String::from("Real"), vec!())),
        BuiltinDef::Type("String", Type::Object(String::from("String"), vec!())),
        BuiltinDef::Type("List",   Type::Object(String::from("List"), vec!(Type::Variable(String::from("item"), UniqueID(0))))),
        //BuiltinDef::Type("Class",  Type::Object(String::from("Class"), vec!())),

        BuiltinDef::Class("Int", vec!(), vec!(), vec!(
            BuiltinDef::Func("+",   "(Int, Int) -> Int", Func::Comptime(add_int)),
            BuiltinDef::Func("add", "(Int, Int) -> Int", Func::Runtime(build_lib_add)),
        )),

        /*
        BuiltinDef::Class("String", vec!(), vec!(
            //BuiltinDef::Func("push", "(String, String) -> String", Func::Comptime(add_int)),
            BuiltinDef::Func("[]",   "(String, Int) -> Int",       Func::Runtime(build_string_get)),
        )),
        */
        BuiltinDef::Func("getindex",   "(String, Int) -> Int",       Func::Runtime(build_string_get)),

        BuiltinDef::Class("Buffer", vec!(Type::Variable(String::from("item"), UniqueID(0))), vec!(), vec!(
            BuiltinDef::Func("__alloc__",  "() -> Buffer<'item>",                      Func::Runtime(build_buffer_allocator)),
            BuiltinDef::Func("new",        "(Buffer<'item>, Int) -> Buffer<'item>",    Func::Runtime(build_buffer_constructor)),
            BuiltinDef::Func("resize",     "(Buffer<'item>, Int) -> Buffer<'item>",    Func::Runtime(build_buffer_resize)),
            BuiltinDef::Func("[]",         "(Buffer<'item>, Int) -> 'item",            Func::Runtime(build_buffer_get)),
            BuiltinDef::Func("[]",         "(Buffer<'item>, Int, 'item) -> 'item",     Func::Runtime(build_buffer_set)),
        )),


        BuiltinDef::Func("+",   "(Int, Int) -> Int",    Func::Comptime(add_int)),
        BuiltinDef::Func("-",   "(Int, Int) -> Int",    Func::Comptime(sub_int)),
        BuiltinDef::Func("*",   "(Int, Int) -> Int",    Func::Comptime(mul_int)),
        BuiltinDef::Func("/",   "(Int, Int) -> Int",    Func::Comptime(div_int)),
        BuiltinDef::Func("%",   "(Int, Int) -> Int",    Func::Comptime(mod_int)),
        //BuiltinDef::Func("^",   "(Int, Int) -> Int",    Func::Comptime(pow_int)),
        //BuiltinDef::Func("<<",  "(Int, Int) -> Int",    Func::Comptime(shl_int)),
        //BuiltinDef::Func(">>",  "(Int, Int) -> Int",    Func::Comptime(shr_int)),
        BuiltinDef::Func("&",   "(Int, Int) -> Int",    Func::Comptime(and_int)),
        BuiltinDef::Func("|",   "(Int, Int) -> Int",    Func::Comptime(or_int)),
        BuiltinDef::Func("<",   "(Int, Int) -> Bool",   Func::Comptime(lt_int)),
        BuiltinDef::Func(">",   "(Int, Int) -> Bool",   Func::Comptime(gt_int)),
        BuiltinDef::Func("<=",  "(Int, Int) -> Bool",   Func::Comptime(lte_int)),
        BuiltinDef::Func(">=",  "(Int, Int) -> Bool",   Func::Comptime(gte_int)),
        BuiltinDef::Func("==",  "(Int, Int) -> Bool",   Func::Comptime(eq_int)),
        BuiltinDef::Func("!=",  "(Int, Int) -> Bool",   Func::Comptime(ne_int)),
        BuiltinDef::Func("~",   "(Int) -> Int",         Func::Comptime(com_int)),
        BuiltinDef::Func("not", "(Int) -> Bool",        Func::Comptime(not_int)),


        BuiltinDef::Func("+",   "(Real, Real) -> Real", Func::Comptime(add_real)),
        BuiltinDef::Func("-",   "(Real, Real) -> Real", Func::Comptime(sub_real)),
        BuiltinDef::Func("*",   "(Real, Real) -> Real", Func::Comptime(mul_real)),
        BuiltinDef::Func("/",   "(Real, Real) -> Real", Func::Comptime(div_real)),
        BuiltinDef::Func("%",   "(Real, Real) -> Real", Func::Comptime(mod_real)),
        BuiltinDef::Func("^",   "(Real, Real) -> Real", Func::Comptime(pow_real)),
        BuiltinDef::Func("<",   "(Real, Real) -> Bool", Func::Comptime(lt_real)),
        BuiltinDef::Func(">",   "(Real, Real) -> Bool", Func::Comptime(gt_real)),
        BuiltinDef::Func("<=",  "(Real, Real) -> Bool", Func::Comptime(lte_real)),
        BuiltinDef::Func(">=",  "(Real, Real) -> Bool", Func::Comptime(gte_real)),
        BuiltinDef::Func("==",  "(Real, Real) -> Bool", Func::Comptime(eq_real)),
        BuiltinDef::Func("!=",  "(Real, Real) -> Bool", Func::Comptime(ne_real)),
        BuiltinDef::Func("not", "(Real) -> Bool",       Func::Comptime(not_real)),


        BuiltinDef::Func("==",  "(Bool, Bool) -> Bool", Func::Comptime(eq_bool)),
        BuiltinDef::Func("!=",  "(Bool, Bool) -> Bool", Func::Comptime(ne_bool)),
        BuiltinDef::Func("not", "(Bool) -> Bool",       Func::Comptime(not_bool)),
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
fn pow_real(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { build_c_call(data, "llvm.pow.f64", &mut vec!(args[0], args[1])) } }
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
    let newptr = build_c_call(data, "realloc", &mut vec!(buffer, size));
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

    let value = build_c_call(data, "puts", &mut vec!(LLVMGetParam(function, 0)));
    LLVMBuildRet(data.builder, value);
    function
}

unsafe fn build_lib_readline(data: &LLVM, name: &str, objtype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start(data, name, vec!(), str_type(data));
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);

    let buffer = build_c_call(data, "malloc", &mut vec!(int_value(data, 2048)));
    build_c_call(data, "gets", &mut vec!(buffer));
    let len = build_c_call(data, "strlen", &mut vec!(buffer));
    let value = build_c_call(data, "realloc", &mut vec!(buffer, len));
    LLVMBuildRet(data.builder, value);
    function
}


//    let mut types = vec!(cint_type, bool_type(data));
//    let rtype = LLVMStructTypeInContext(data.context, types.as_mut_ptr(), types.len() as u32, false as i32);

//    declare_function(data.module, pscope, "llvm.sadd.with.overflow.i64", &mut [cint_type, cint_type], rtype, false);


