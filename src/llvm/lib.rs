
use std::ptr;
//use std::collections::HashMap;
//use std::collections::hash_map::Entry;


extern crate llvm_sys as llvm;
use self::llvm::prelude::*;
use self::llvm::core::*;

use abi::ABI;
use types::Type;
use defs::Def;
use session::Session;
use ast::{ NodeID, Pos, Ident };
use parser::{ parse_type };
use scope::{ Scope, ScopeRef, ScopeMapRef, Context };
use binding::{ declare_typevars };
use utils::UniqueID;

use defs::classes::ClassDef;
use defs::functions::FuncDef;

use llvm::codegen::*;


pub type RuntimeFunction = unsafe fn(&LLVM, NodeID, &str, LLVMTypeRef) -> LLVMValueRef;
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
    Func(NodeID, &'sess str, &'sess str, Func),
    Class(NodeID, &'sess str, Vec<Type>, Vec<(String, Type)>, Vec<BuiltinDef<'sess>>),
}


pub fn make_global<'sess>(session: &Session, builtins: &Vec<BuiltinDef<'sess>>) {
    let primatives = session.map.add(ScopeMapRef::PRIMATIVE, None);
    primatives.set_context(Context::Primative);

    declare_builtins_vec(session, primatives.clone(), builtins);

    let global = session.map.add(ScopeMapRef::GLOBAL, Some(primatives));
    global.set_context(Context::Global);
}
 
pub fn declare_builtins_vec<'sess>(session: &Session, scope: ScopeRef, entries: &Vec<BuiltinDef<'sess>>) {
    for node in entries {
        declare_builtins_node(session, scope.clone(), node);
    }
}

pub fn declare_builtins_node<'sess>(session: &Session, scope: ScopeRef, node: &BuiltinDef<'sess>) {
    match *node {
        BuiltinDef::Func(ref id, ref name, ref ftype, ref func) => {
            let mut ftype = parse_type(ftype);
            declare_typevars(session, scope.clone(), ftype.as_mut(), false).unwrap();
            let abi = match *func {
                Func::External => ABI::C,
                _ => ABI::Molten,
            };
            FuncDef::define_func(session, scope.clone(), *id, &Some(String::from(*name)), abi, ftype.clone()).unwrap();
        },
        BuiltinDef::Class(ref id, ref name, ref params, _, ref entries) => {
            let tscope = ClassDef::create_class_scope(session, scope.clone(), *id);
            let mut ttype = Type::Object(String::from(*name), *id, params.clone());
            declare_typevars(session, tscope.clone(), Some(&mut ttype), true).unwrap();
            let classdef = ClassDef::define_class(session, scope.clone(), *id, ttype, None).unwrap();

            declare_builtins_vec(session, tscope.clone(), entries);
        },
    }
}

pub unsafe fn initialize_builtins<'sess>(data: &mut LLVM<'sess>, scope: ScopeRef, entries: &Vec<BuiltinDef<'sess>>) {
    define_builtins_vec(data, ptr::null_mut(), scope.clone(), entries);
    let pscope = scope.get_parent().unwrap();
    declare_irregular_functions(data, pscope.clone());
}

pub unsafe fn define_builtins_vec<'sess>(data: &mut LLVM<'sess>, objtype: LLVMTypeRef, scope: ScopeRef, entries: &Vec<BuiltinDef<'sess>>) {
    for node in entries {
        define_builtins_node(data, objtype, scope.clone(), node);
    }
}

pub unsafe fn define_builtins_node<'sess>(data: &mut LLVM<'sess>, objtype: LLVMTypeRef, scope: ScopeRef, node: &BuiltinDef<'sess>) {
    match *node {
        BuiltinDef::Func(ref id, ref sname, ref types, ref func) => {
            let mut name = String::from(*sname);
            let ftype = parse_type(types).unwrap();
            match *func {
                Func::External => {
                    let func = LLVMAddFunction(data.module, label(name.as_str()), get_ltype(data, ftype.clone(), false));
                    data.set_value(*id, from_abi(&ftype.get_abi().unwrap(), func));
                },
                Func::Runtime(func) => {
                    data.set_value(*id, from_type(&ftype, func(data, *id, name.as_str(), objtype)));
                },
                Func::Comptime(func) => {
                    data.set_value(*id, Box::new(Builtin(BuiltinFunction(func), ftype)));
                },
                Func::Undefined => { },
            }
        },
        BuiltinDef::Class(ref id, ref name, _, ref structdef, ref entries) => {
            let tscope = data.session.map.get(id);
            let cname = String::from(*name);
            let classdef = scope.find_type_def(data.session, &String::from(*name)).unwrap().as_class().unwrap();

            let lltype = if structdef.len() > 0 {
                for (ref field, ref ttype) in structdef {
                    classdef.structdef.add_field(data.session, field, ttype.clone());
                }
                build_class_type(data, scope.clone(), *id, &cname, classdef.clone())
            } else {
                let lltype = get_ltype(data, scope.find_type(data.session, &cname).unwrap(), true);
                data.set_type(*id, TypeValue { value: lltype, vttype: None });
                lltype
            };

            define_builtins_vec(data, lltype, tscope.clone(), entries);
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


fn id() -> NodeID {
    NodeID::generate()
}

pub fn get_builtins<'sess>() -> Vec<BuiltinDef<'sess>> {
    vec!(
        /*
        BuiltinDef::Type(id(), "Nil",    Type::Object(String::from("Nil"), vec!())),
        BuiltinDef::Type(id(), "Bool",   Type::Object(String::from("Bool"), vec!())),
        BuiltinDef::Type(id(), "Byte",   Type::Object(String::from("Byte"), vec!())),
        BuiltinDef::Type(id(), "Real",   Type::Object(String::from("Real"), vec!())),
        BuiltinDef::Type(id(), "String", Type::Object(String::from("String"), vec!())),
        BuiltinDef::Type(id(), "List",   Type::Object(String::from("List"), vec!(Type::Variable(String::from("item"), UniqueID(0))))),
        //BuiltinDef::Type(id(), "Class",  Type::Object(String::from("Class"), vec!())),
        */

        BuiltinDef::Class(id(), "Nil",    vec!(), vec!(), vec!()),
        BuiltinDef::Class(id(), "Bool",   vec!(), vec!(), vec!()),
        BuiltinDef::Class(id(), "Byte",   vec!(), vec!(), vec!()),
        BuiltinDef::Class(id(), "Real",   vec!(), vec!(), vec!()),
        BuiltinDef::Class(id(), "String", vec!(), vec!(), vec!()),
        //BuiltinDef::Class(id(), "List",   vec!(Type::Variable(String::from("item"), UniqueID(0))), vec!(), vec!()),
        //BuiltinDef::Class(id(), "Class",  Type::Object(String::from("Class"), vec!())),

        BuiltinDef::Class(id(), "Int", vec!(), vec!(), vec!(
            BuiltinDef::Func(id(), "+",   "(Int, Int) -> Int", Func::Comptime(add_int)),
            BuiltinDef::Func(id(), "add", "(Int, Int) -> Int", Func::Runtime(build_lib_add)),
        )),

        /*
        BuiltinDef::Class(id(), "String", vec!(), vec!(
            //BuiltinDef::Func(id(), "push", "(String, String) -> String", Func::Comptime(add_int)),
            BuiltinDef::Func(id(), "[]",   "(String, Int) -> Int",       Func::Runtime(build_string_get)),
        )),
        */
        BuiltinDef::Func(id(), "getindex",   "(String, Int) -> Int",       Func::Runtime(build_string_get)),


        BuiltinDef::Func(id(), "malloc",     "(Int) -> 'ptr / C",             Func::External),
        BuiltinDef::Func(id(), "realloc",    "('ptr, Int) -> 'ptr / C",       Func::External),
        BuiltinDef::Func(id(), "free",       "('ptr) -> Nil / C",             Func::External),
        BuiltinDef::Func(id(), "memcpy",     "('ptr, 'ptr, Int) -> 'ptr / C", Func::External),
        BuiltinDef::Func(id(), "strcmp",     "(String, String) -> Int / C",   Func::External),
        BuiltinDef::Func(id(), "puts",       "(String) -> Nil / C",           Func::External),
        BuiltinDef::Func(id(), "gets",       "(String) -> String / C",        Func::External),
        BuiltinDef::Func(id(), "strlen",     "(String) -> Int / C",           Func::External),
        //BuiltinDef::Func(id(), "sprintf",    "'tmp",                          Func::Undefined),
        //BuiltinDef::Func(id(), "sprintf",    "(String, String, 'sess1, 'sess2) -> Nil / C", Func::Undefined),
        BuiltinDef::Func(id(), "sprintf",    "(String, String, 'sess1, 'sess2) -> Nil / C", Func::Comptime(sprintf)),

        BuiltinDef::Func(id(), "println",    "(String) -> Nil / C",           Func::Runtime(build_lib_println)),
        BuiltinDef::Func(id(), "readline",   "() -> String / C",              Func::Runtime(build_lib_readline)),


        BuiltinDef::Class(id(), "Buffer", vec!(Type::Variable(String::from("item"), UniqueID(0))), vec!(), vec!(
            BuiltinDef::Func(id(), "__alloc__",  "() -> Buffer<'item>",                      Func::Runtime(build_buffer_allocator)),
            BuiltinDef::Func(id(), "new",        "(Buffer<'item>, Int) -> Buffer<'item>",    Func::Runtime(build_buffer_constructor)),
            BuiltinDef::Func(id(), "resize",     "(Buffer<'item>, Int) -> Buffer<'item>",    Func::Runtime(build_buffer_resize)),
            BuiltinDef::Func(id(), "[]",         "(Buffer<'item>, Int) -> 'item",            Func::Runtime(build_buffer_get)),
            BuiltinDef::Func(id(), "[]",         "(Buffer<'item>, Int, 'item) -> 'item",     Func::Runtime(build_buffer_set)),
        )),


        BuiltinDef::Func(id(), "+",   "(Int, Int) -> Int",    Func::Comptime(add_int)),
        BuiltinDef::Func(id(), "-",   "(Int, Int) -> Int",    Func::Comptime(sub_int)),
        BuiltinDef::Func(id(), "*",   "(Int, Int) -> Int",    Func::Comptime(mul_int)),
        BuiltinDef::Func(id(), "/",   "(Int, Int) -> Int",    Func::Comptime(div_int)),
        BuiltinDef::Func(id(), "%",   "(Int, Int) -> Int",    Func::Comptime(mod_int)),
        //BuiltinDef::Func(id(), "^",   "(Int, Int) -> Int",    Func::Comptime(pow_int)),
        //BuiltinDef::Func(id(), "<<",  "(Int, Int) -> Int",    Func::Comptime(shl_int)),
        //BuiltinDef::Func(id(), ">>",  "(Int, Int) -> Int",    Func::Comptime(shr_int)),
        BuiltinDef::Func(id(), "&",   "(Int, Int) -> Int",    Func::Comptime(and_int)),
        BuiltinDef::Func(id(), "|",   "(Int, Int) -> Int",    Func::Comptime(or_int)),
        BuiltinDef::Func(id(), "<",   "(Int, Int) -> Bool",   Func::Comptime(lt_int)),
        BuiltinDef::Func(id(), ">",   "(Int, Int) -> Bool",   Func::Comptime(gt_int)),
        BuiltinDef::Func(id(), "<=",  "(Int, Int) -> Bool",   Func::Comptime(lte_int)),
        BuiltinDef::Func(id(), ">=",  "(Int, Int) -> Bool",   Func::Comptime(gte_int)),
        BuiltinDef::Func(id(), "==",  "(Int, Int) -> Bool",   Func::Comptime(eq_int)),
        BuiltinDef::Func(id(), "!=",  "(Int, Int) -> Bool",   Func::Comptime(ne_int)),
        BuiltinDef::Func(id(), "~",   "(Int) -> Int",         Func::Comptime(com_int)),
        BuiltinDef::Func(id(), "not", "(Int) -> Bool",        Func::Comptime(not_int)),


        BuiltinDef::Func(id(), "+",   "(Real, Real) -> Real", Func::Comptime(add_real)),
        BuiltinDef::Func(id(), "-",   "(Real, Real) -> Real", Func::Comptime(sub_real)),
        BuiltinDef::Func(id(), "*",   "(Real, Real) -> Real", Func::Comptime(mul_real)),
        BuiltinDef::Func(id(), "/",   "(Real, Real) -> Real", Func::Comptime(div_real)),
        BuiltinDef::Func(id(), "%",   "(Real, Real) -> Real", Func::Comptime(mod_real)),
        BuiltinDef::Func(id(), "^",   "(Real, Real) -> Real", Func::Comptime(pow_real)),
        BuiltinDef::Func(id(), "<",   "(Real, Real) -> Bool", Func::Comptime(lt_real)),
        BuiltinDef::Func(id(), ">",   "(Real, Real) -> Bool", Func::Comptime(gt_real)),
        BuiltinDef::Func(id(), "<=",  "(Real, Real) -> Bool", Func::Comptime(lte_real)),
        BuiltinDef::Func(id(), ">=",  "(Real, Real) -> Bool", Func::Comptime(gte_real)),
        BuiltinDef::Func(id(), "==",  "(Real, Real) -> Bool", Func::Comptime(eq_real)),
        BuiltinDef::Func(id(), "!=",  "(Real, Real) -> Bool", Func::Comptime(ne_real)),
        BuiltinDef::Func(id(), "not", "(Real) -> Bool",       Func::Comptime(not_real)),


        BuiltinDef::Func(id(), "==",  "(Bool, Bool) -> Bool", Func::Comptime(eq_bool)),
        BuiltinDef::Func(id(), "!=",  "(Bool, Bool) -> Bool", Func::Comptime(ne_bool)),
        BuiltinDef::Func(id(), "not", "(Bool) -> Bool",       Func::Comptime(not_bool)),
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

fn sprintf(data: &LLVM, mut args: Vec<LLVMValueRef>) -> LLVMValueRef {
    unsafe {
        build_c_call(data, "sprintf", &mut args)
    }
}


unsafe fn build_buffer_allocator(data: &LLVM, id: NodeID, name: &str, objtype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start_lib(data, id, name, vec!(), objtype);
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);
    LLVMBuildRet(data.builder, null_value(objtype));
    function
}

unsafe fn build_buffer_constructor(data: &LLVM, id: NodeID, name: &str, objtype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start_lib(data, id, name, vec!(objtype, int_type(data)), objtype);
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);

    let ptr = LLVMBuildArrayMalloc(data.builder, LLVMInt64TypeInContext(data.context), LLVMGetParam(function, 1), label("tmp"));
    let castptr = LLVMBuildPointerCast(data.builder, ptr, objtype, label("ptr"));
    LLVMBuildRet(data.builder, castptr);
    function
}

unsafe fn build_buffer_resize(data: &LLVM, id: NodeID, name: &str, objtype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start_lib(data, id, name, vec!(objtype, int_type(data)), objtype);
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);

    let buffer = LLVMBuildPointerCast(data.builder, LLVMGetParam(function, 0), str_type(data), label("tmp"));
    let size = LLVMBuildMul(data.builder, LLVMGetParam(function, 1), LLVMSizeOf(LLVMInt64TypeInContext(data.context)), label("tmp"));
    let newptr = build_c_call(data, "realloc", &mut vec!(buffer, size));
    let castptr = LLVMBuildPointerCast(data.builder, newptr, objtype, label("ptr"));
    LLVMBuildRet(data.builder, castptr);
    function
}

unsafe fn build_buffer_get(data: &LLVM, id: NodeID, name: &str, objtype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start_lib(data, id, name, vec!(objtype, int_type(data)), str_type(data));
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

unsafe fn build_buffer_set(data: &LLVM, id: NodeID, name: &str, objtype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start_lib(data, id, name, vec!(objtype, int_type(data), str_type(data)), str_type(data));
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


unsafe fn build_string_get(data: &LLVM, id: NodeID, name: &str, _objtype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start_lib(data, id, name, vec!(str_type(data), int_type(data)), int_type(data));
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);

    let string = LLVMGetParam(function, 0);
    let mut indices = vec!(LLVMGetParam(function, 1));
    let pointer = LLVMBuildGEP(data.builder, string, indices.as_mut_ptr(), indices.len() as u32, label("tmp"));
    let value = LLVMBuildLoad(data.builder, pointer, label("tmp"));
    let value = LLVMBuildCast(data.builder, llvm::LLVMOpcode::LLVMZExt, value, int_type(data), label("tmp"));
    
    LLVMBuildRet(data.builder, value);
    function
}

unsafe fn build_lib_add(data: &LLVM, id: NodeID, name: &str, _objtype: LLVMTypeRef) -> LLVMValueRef {
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



unsafe fn build_lib_println(data: &LLVM, id: NodeID, name: &str, _objtype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start_lib(data, id, name, vec!(str_type(data)), str_type(data));
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);

    let value = build_c_call(data, "puts", &mut vec!(LLVMGetParam(function, 0)));
    LLVMBuildRet(data.builder, value);
    function
}

unsafe fn build_lib_readline(data: &LLVM, id: NodeID, name: &str, _objtype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start_lib(data, id, name, vec!(), str_type(data));
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


