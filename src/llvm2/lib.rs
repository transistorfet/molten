
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

use defs::classes::{ ClassDef, Define };
use defs::functions::{ AnyFunc, CFuncDef, FuncDef, BuiltinFuncDef };

use llvm2::llcode::*;
use llvm2::transform::*;
use llvm2::codegen::*;

pub type ObjectFunction = unsafe fn(&LLVM, LLVMTypeRef, Vec<LLVMValueRef>) -> LLVMValueRef;
pub type PlainFunction = unsafe fn(&LLVM, Vec<LLVMValueRef>) -> LLVMValueRef;

#[derive(Clone)]
pub enum FuncKind {
    FromNamed,
    External,
    Method(ObjectFunction),
    Function(PlainFunction)
}

#[derive(Clone)]
pub enum BuiltinDef<'sess> {
    Func(NodeID, &'sess str, &'sess str, FuncKind),
    Class(NodeID, &'sess str, Vec<Type>, Vec<(String, Type)>, Vec<BuiltinDef<'sess>>),
}


pub fn make_global<'sess>(session: &Session, builtins: &Vec<BuiltinDef<'sess>>) {
    let primatives = session.map.add(ScopeMapRef::PRIMATIVE, None);
    primatives.set_context(Context::Primative);

    declare_builtins_vec(session, primatives.clone(), builtins);

    let global = session.map.add(ScopeMapRef::GLOBAL, Some(primatives));
    // TODO disabling this allows the identifier closure convertor to convert references to variables inside the main function
    //global.set_context(Context::Global);
}
 
pub fn declare_builtins_vec<'sess>(session: &Session, scope: ScopeRef, entries: &Vec<BuiltinDef<'sess>>) {
    for node in entries {
        declare_builtins_node(session, scope.clone(), node);
    }
}

pub fn declare_builtins_node<'sess>(session: &Session, scope: ScopeRef, node: &BuiltinDef<'sess>) {
    match *node {
        BuiltinDef::Func(ref id, ref name, ref ftype, ref func) => {
            let tscope = if scope.is_primative() {
                Scope::new_ref(Some(scope.clone()))
            } else {
                scope.clone()
            };

            let mut ftype = parse_type(ftype);
            declare_typevars(session, tscope.clone(), ftype.as_mut(), true).unwrap();
            debug!("BUILTIN TYPE: {:?}", ftype);
            let abi = ftype.as_ref().map(|t| t.get_abi().unwrap()).unwrap_or(ABI::Molten);
            match *func {
                FuncKind::Function(_) => {
                    FuncDef::define(session, scope.clone(), *id, &Some(String::from(*name)), ftype.clone()).unwrap();
                },
                _ => {
                    AnyFunc::define(session, scope.clone(), *id, &Some(String::from(*name)), abi, ftype.clone()).unwrap();
                },
            };
        },
        BuiltinDef::Class(ref id, ref name, ref params, _, ref entries) => {
            let tscope = ClassDef::create_class_scope(session, scope.clone(), *id);
            let mut ttype = Type::Object(String::from(*name), *id, params.clone());
            declare_typevars(session, tscope.clone(), Some(&mut ttype), true).unwrap();
            let classdef = ClassDef::define(session, scope.clone(), *id, ttype, None).unwrap();

            declare_builtins_vec(session, tscope.clone(), entries);
        },
    }
}


pub unsafe fn initialize_builtins<'sess>(llvm: &LLVM<'sess>, transformer: &Transformer, scope: ScopeRef, entries: &Vec<BuiltinDef<'sess>>) {
    let pscope = scope.get_parent().unwrap();
    declare_irregular_functions(llvm);
    define_builtins_vec(llvm, transformer, ptr::null_mut(), scope.clone(), entries);
}

pub unsafe fn define_builtins_vec<'sess>(llvm: &LLVM<'sess>, transformer: &Transformer, objtype: LLVMTypeRef, scope: ScopeRef, entries: &Vec<BuiltinDef<'sess>>) {
    for node in entries {
        define_builtins_node(llvm, transformer, objtype, scope.clone(), node);
    }
}

pub unsafe fn define_builtins_node<'sess>(llvm: &LLVM<'sess>, transformer: &Transformer, objtype: LLVMTypeRef, scope: ScopeRef, node: &BuiltinDef<'sess>) {
    match *node {
        BuiltinDef::Func(ref id, ref sname, ref types, ref func) => {
            let ftype = parse_type(types).unwrap();
            let (argtypes, rettype, abi) = ftype.get_function_types().unwrap();
            let mut name = abi.mangle_name(sname, argtypes, 2);
            let ltype = transformer.transform_func_def_type(abi, &argtypes.as_vec(), rettype);
            match *func {
                FuncKind::External => {
                    let func = LLVMAddFunction(llvm.module, cstring(&name), llvm.build_type(&ltype));
                    llvm.set_value(*id, func);
                },
                FuncKind::Method(func) => {
                    let function = build_lib_method(llvm, *id, name.as_str(), objtype, &ltype, func);
                    llvm.set_value(*id, function);
                },
                FuncKind::Function(func) => {
                    let function = build_lib_function(llvm, *id, name.as_str(), &ltype, func);
                    llvm.set_value(*id, function);
                },
                FuncKind::FromNamed => {
                    llvm.set_value(*id, LLVMGetNamedFunction(llvm.module, cstring(&name)));
                },
            }
        },
        BuiltinDef::Class(ref id, ref name, _, ref structdef, ref entries) => {
            let tscope = llvm.session.map.get(id);
            let cname = String::from(*name);
            let classdef = llvm.session.get_def(*id).unwrap().as_class().unwrap();
            let ltype = transformer.transform_value_type(&llvm.session.get_type(*id).unwrap());

            let lltype = if structdef.len() > 0 {
                for (ref field, ref ttype) in structdef {
                    classdef.structdef.add_field(llvm.session, NodeID::generate(), true, field, ttype.clone(), Define::IfNotExists);
                }
                //build_class_type(llvm, scope.clone(), *id, &cname, classdef.clone())

                //self.transform_class_type_data(scope.clone(), classdef.clone(), body);
                //exprs.extend(self.transform_vtable_init(scope.clone(), classdef));

                llvm.ptr_type()
            } else {
                let lltype = llvm.build_type(&ltype);
                llvm.set_type(*id, lltype);
                lltype
            };

            define_builtins_vec(llvm, transformer, lltype, tscope.clone(), entries);
        },
    }
}


pub unsafe fn declare_c_function(llvm: &LLVM, name: &str, args: &mut [LLVMTypeRef], ret_type: LLVMTypeRef, vargs: bool) -> LLVMValueRef {
    let ftype = LLVMFunctionType(ret_type, args.as_mut_ptr(), args.len() as u32, vargs as i32);
    let function = LLVMAddFunction(llvm.module, cstr(name), ftype);
    function
}

unsafe fn declare_irregular_functions(llvm: &LLVM) {
    let bytestr_type = LLVMPointerType(LLVMInt8Type(), 0);
    //let cint_type = LLVMInt32TypeInContext(llvm.context);
    let cint_type = llvm.i64_type();

    //declare_function(llvm, "malloc", &mut [cint_type], bytestr_type, false);
    //declare_function(llvm, "realloc", &mut [bytestr_type, cint_type], bytestr_type, false);
    //declare_function(llvm, "free", &mut [bytestr_type], LLVMVoidType(), false);

    //declare_function(llvm, "strlen", &mut [bytestr_type], cint_type, false);
    //declare_function(llvm, "memcpy", &mut [bytestr_type, bytestr_type, cint_type], bytestr_type, false);

    //declare_function(llvm, "puts", &mut [bytestr_type], cint_type, false);
    declare_c_function(llvm, "sprintf", &mut [bytestr_type, bytestr_type], cint_type, true);

    declare_c_function(llvm, "llvm.pow.f64", &mut [llvm.f64_type(), llvm.f64_type()], llvm.f64_type(), false);

    //declare_function(llvm, "__gxx_personality_v0", &mut [bytestr_type, bytestr_type], cint_type, true);
}

unsafe fn build_lib_function(llvm: &LLVM, id: NodeID, name: &str, ltype: &LLType, func: PlainFunction) -> LLVMValueRef {
    let function = LLVMAddFunction(llvm.module, cstr(name), llvm.build_type(&ltype));
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceODRLinkage);

    //let name = "alwaysinline";
    //let kind = LLVMGetEnumAttributeKindForName(cstr(name), name.len());
    //let attribute = LLVMCreateEnumAttribute(llvm.context, kind, 0);
    //LLVMAddAttributeAtIndex(function, 0, attribute);

    let bb = LLVMAppendBasicBlockInContext(llvm.context, function, cstr("entry"));
    LLVMPositionBuilderAtEnd(llvm.builder, bb);

    let args = (0..ltype.argcount()).map(|i| LLVMGetParam(function, i as u32)).collect();
    let ret = func(llvm, args);
    LLVMBuildRet(llvm.builder, llvm.build_cast(llvm.build_type(&ltype.get_rettype()), ret));
    //LLVMBuildRet(llvm.builder, ret);

    function
}

unsafe fn build_lib_method(llvm: &LLVM, id: NodeID, name: &str, objtype: LLVMTypeRef, ltype: &LLType, func: ObjectFunction) -> LLVMValueRef {
    let function = LLVMAddFunction(llvm.module, cstr(name), llvm.build_type(&ltype));
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceODRLinkage);

    //let name = "alwaysinline";
    //let kind = LLVMGetEnumAttributeKindForName(cstr(name), name.len());
    //let attribute = LLVMCreateEnumAttribute(llvm.context, kind, 0);
    //LLVMAddAttributeAtIndex(function, 0, attribute);

    let bb = LLVMAppendBasicBlockInContext(llvm.context, function, cstr("entry"));
    LLVMPositionBuilderAtEnd(llvm.builder, bb);

    let args = (0..ltype.argcount()).map(|i| LLVMGetParam(function, i as u32)).collect();
    let ret = func(llvm, objtype, args);
    LLVMBuildRet(llvm.builder, llvm.build_cast(llvm.build_type(&ltype.get_rettype()), ret));
    //LLVMBuildRet(llvm.builder, ret);

    function
}


fn id() -> NodeID {
    NodeID::generate()
}

pub fn get_builtins<'sess>() -> Vec<BuiltinDef<'sess>> {
    vec!(
        BuiltinDef::Class(id(), "()",     vec!(), vec!(), vec!()),
        BuiltinDef::Class(id(), "Nil",    vec!(), vec!(), vec!()),
        BuiltinDef::Class(id(), "Bool",   vec!(), vec!(), vec!()),
        BuiltinDef::Class(id(), "Byte",   vec!(), vec!(), vec!()),
        BuiltinDef::Class(id(), "Int",    vec!(), vec!(), vec!()),
        BuiltinDef::Class(id(), "Real",   vec!(), vec!(), vec!()),
        BuiltinDef::Class(id(), "String", vec!(), vec!(), vec!()),
        //    BuiltinDef::Func(id(), "[]",   "(String, Int) -> Int",            FuncKind::Runtime(build_string_get)),
        //BuiltinDef::Class(id(), "List",   vec!(Type::Variable(String::from("item"), UniqueID(0))), vec!(), vec!()),
        //BuiltinDef::Class(id(), "Class",  Type::Object(String::from("Class"), vec!())),


        BuiltinDef::Func(id(), "malloc",     "(Int) -> 'ptr / C",               FuncKind::External),
        BuiltinDef::Func(id(), "realloc",    "('ptr, Int) -> 'ptr / C",         FuncKind::External),
        BuiltinDef::Func(id(), "free",       "('ptr) -> () / C",                FuncKind::External),
        BuiltinDef::Func(id(), "memcpy",     "('ptr, 'ptr, Int) -> 'ptr / C",   FuncKind::External),
        BuiltinDef::Func(id(), "strcmp",     "(String, String) -> Int / C",     FuncKind::External),
        BuiltinDef::Func(id(), "puts",       "(String) -> () / C",              FuncKind::External),
        BuiltinDef::Func(id(), "gets",       "(String) -> String / C",          FuncKind::External),
        BuiltinDef::Func(id(), "strlen",     "(String) -> Int / C",             FuncKind::External),
        //BuiltinDef::Func(id(), "sprintf",    "'tmp",                          FuncKind::FromNamed),
        //BuiltinDef::Func(id(), "sprintf2",    "(String, String, '__a1, '__a2) -> () / C", FuncKind::Function(sprintf)),
        BuiltinDef::Func(id(), "sprintf",    "(String, String, '__a1, '__a2) -> () / C", FuncKind::FromNamed),

        BuiltinDef::Func(id(), "println",    "(String) -> () / C",              FuncKind::Function(println)),
        //BuiltinDef::Func(id(), "readline",   "() -> String / C",                FuncKind::Function(readline)),

        BuiltinDef::Func(id(), "sizeof",    "('ptr) -> Int",                    FuncKind::Function(sizeof_value)),


        BuiltinDef::Func(id(), "getindex",   "(String, Int, Int) -> Int / C",           FuncKind::Function(string_get)),
        //BuiltinDef::Func(id(), "bufalloc",   "(Int, Int) -> String / C",                FuncKind::Function(buffer_alloc)),
        //BuiltinDef::Func(id(), "getindex",   "(String, Int, Int) -> Int / C",           FuncKind::Function(buffer_get)),
        //BuiltinDef::Func(id(), "setindex",   "(String, Int, Int, 'item) -> Int / C",    FuncKind::Function(buffer_set)),


        BuiltinDef::Class(id(), "Buffer", vec!(Type::Variable(String::from("item"), UniqueID(0))), vec!(), vec!(
            BuiltinDef::Func(id(), "__alloc__",  "() -> Buffer<'item>",                      FuncKind::Method(buffer_allocator)),
            BuiltinDef::Func(id(), "new",        "(Buffer<'item>, Int) -> Buffer<'item>",    FuncKind::Method(buffer_constructor)),
            BuiltinDef::Func(id(), "resize",     "(Buffer<'item>, Int) -> Buffer<'item>",    FuncKind::Method(buffer_resize)),
            BuiltinDef::Func(id(), "[]",         "(Buffer<'item>, Int) -> 'item",            FuncKind::Method(buffer_get_method)),
            BuiltinDef::Func(id(), "[]",         "(Buffer<'item>, Int, 'item) -> 'item",     FuncKind::Method(buffer_set_method)),
        )),

        //// Integer Builtins ////
        BuiltinDef::Func(id(), "+",   "(Int, Int) -> Int / MF",      FuncKind::Function(add_int)),
        BuiltinDef::Func(id(), "-",   "(Int, Int) -> Int / MF",      FuncKind::Function(sub_int)),
        BuiltinDef::Func(id(), "*",   "(Int, Int) -> Int / MF",      FuncKind::Function(mul_int)),
        BuiltinDef::Func(id(), "/",   "(Int, Int) -> Int / MF",      FuncKind::Function(div_int)),
        BuiltinDef::Func(id(), "%",   "(Int, Int) -> Int / MF",      FuncKind::Function(mod_int)),
        //BuiltinDef::Func(id(), "^",   "(Int, Int) -> Int / MF",    FuncKind::Function(pow_int)),
        //BuiltinDef::Func(id(), "<<",  "(Int, Int) -> Int / MF",    FuncKind::Function(shl_int)),
        //BuiltinDef::Func(id(), ">>",  "(Int, Int) -> Int / MF",    FuncKind::Function(shr_int)),
        BuiltinDef::Func(id(), "&",   "(Int, Int) -> Int / MF",      FuncKind::Function(and_int)),
        BuiltinDef::Func(id(), "|",   "(Int, Int) -> Int / MF",      FuncKind::Function(or_int)),
        BuiltinDef::Func(id(), "<",   "(Int, Int) -> Bool / MF",     FuncKind::Function(lt_int)),
        BuiltinDef::Func(id(), ">",   "(Int, Int) -> Bool / MF",     FuncKind::Function(gt_int)),
        BuiltinDef::Func(id(), "<=",  "(Int, Int) -> Bool / MF",     FuncKind::Function(lte_int)),
        BuiltinDef::Func(id(), ">=",  "(Int, Int) -> Bool / MF",     FuncKind::Function(gte_int)),
        BuiltinDef::Func(id(), "==",  "(Int, Int) -> Bool / MF",     FuncKind::Function(eq_int)),
        BuiltinDef::Func(id(), "!=",  "(Int, Int) -> Bool / MF",     FuncKind::Function(ne_int)),
        BuiltinDef::Func(id(), "~",   "(Int) -> Int / MF",           FuncKind::Function(com_int)),
        BuiltinDef::Func(id(), "not", "(Int) -> Bool / MF",          FuncKind::Function(not_int)),


        //// Real Builtins ////
        BuiltinDef::Func(id(), "+",   "(Real, Real) -> Real / MF",   FuncKind::Function(add_real)),
        BuiltinDef::Func(id(), "-",   "(Real, Real) -> Real / MF",   FuncKind::Function(sub_real)),
        BuiltinDef::Func(id(), "*",   "(Real, Real) -> Real / MF",   FuncKind::Function(mul_real)),
        BuiltinDef::Func(id(), "/",   "(Real, Real) -> Real / MF",   FuncKind::Function(div_real)),
        BuiltinDef::Func(id(), "%",   "(Real, Real) -> Real / MF",   FuncKind::Function(mod_real)),
        BuiltinDef::Func(id(), "^",   "(Real, Real) -> Real / MF",   FuncKind::Function(pow_real)),
        BuiltinDef::Func(id(), "<",   "(Real, Real) -> Bool / MF",   FuncKind::Function(lt_real)),
        BuiltinDef::Func(id(), ">",   "(Real, Real) -> Bool / MF",   FuncKind::Function(gt_real)),
        BuiltinDef::Func(id(), "<=",  "(Real, Real) -> Bool / MF",   FuncKind::Function(lte_real)),
        BuiltinDef::Func(id(), ">=",  "(Real, Real) -> Bool / MF",   FuncKind::Function(gte_real)),
        BuiltinDef::Func(id(), "==",  "(Real, Real) -> Bool / MF",   FuncKind::Function(eq_real)),
        BuiltinDef::Func(id(), "!=",  "(Real, Real) -> Bool / MF",   FuncKind::Function(ne_real)),


        //// Boolean Builtins ////
        BuiltinDef::Func(id(), "==",  "(Bool, Bool) -> Bool / MF",   FuncKind::Function(eq_bool)),
        BuiltinDef::Func(id(), "!=",  "(Bool, Bool) -> Bool / MF",   FuncKind::Function(ne_bool)),
        BuiltinDef::Func(id(), "not", "(Bool) -> Bool / MF",         FuncKind::Function(not_bool)),


        //// Ref Builtins ////
        BuiltinDef::Func(id(), "!",   "(ref 'ptr) -> 'ptr / MF",     FuncKind::Function(deref)),
    )
}


fn add_int(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildAdd(llvm.builder, args[0], args[1], cstr("")) } }
fn sub_int(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildSub(llvm.builder, args[0], args[1], cstr("")) } }
fn mul_int(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildMul(llvm.builder, args[0], args[1], cstr("")) } }
fn div_int(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildSDiv(llvm.builder, args[0], args[1], cstr("")) } }
fn mod_int(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildSRem(llvm.builder, args[0], args[1], cstr("")) } }
fn and_int(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildAnd(llvm.builder, args[0], args[1], cstr("")) } }
fn or_int(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildOr(llvm.builder, args[0], args[1], cstr("")) } }
fn eq_int(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(llvm.builder, llvm::LLVMIntPredicate::LLVMIntEQ, args[0], args[1], cstr("")) } }
fn ne_int(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(llvm.builder, llvm::LLVMIntPredicate::LLVMIntNE, args[0], args[1], cstr("")) } }
fn lt_int(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(llvm.builder, llvm::LLVMIntPredicate::LLVMIntSLT, args[0], args[1], cstr("")) } }
fn gt_int(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(llvm.builder, llvm::LLVMIntPredicate::LLVMIntSGT, args[0], args[1], cstr("")) } }
fn lte_int(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(llvm.builder, llvm::LLVMIntPredicate::LLVMIntSLE, args[0], args[1], cstr("")) } }
fn gte_int(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(llvm.builder, llvm::LLVMIntPredicate::LLVMIntSGE, args[0], args[1], cstr("")) } }
fn com_int(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildXor(llvm.builder, args[0], llvm.i64_const(0x7FFFFFFFFFFFFFFF), cstr("")) } }
fn not_int(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildNot(llvm.builder, args[0], cstr("")) } }

fn add_real(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFAdd(llvm.builder, args[0], args[1], cstr("")) } }
fn sub_real(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFSub(llvm.builder, args[0], args[1], cstr("")) } }
fn mul_real(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFMul(llvm.builder, args[0], args[1], cstr("")) } }
fn div_real(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFDiv(llvm.builder, args[0], args[1], cstr("")) } }
fn mod_real(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFRem(llvm.builder, args[0], args[1], cstr("")) } }
fn pow_real(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { llvm.build_call_by_name("llvm.pow.f64", &mut vec!(args[0], args[1])) } }
fn eq_real(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(llvm.builder, llvm::LLVMRealPredicate::LLVMRealOEQ, args[0], args[1], cstr("")) } }
fn ne_real(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(llvm.builder, llvm::LLVMRealPredicate::LLVMRealONE, args[0], args[1], cstr("")) } }
fn lt_real(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(llvm.builder, llvm::LLVMRealPredicate::LLVMRealOLT, args[0], args[1], cstr("")) } }
fn gt_real(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(llvm.builder, llvm::LLVMRealPredicate::LLVMRealOGT, args[0], args[1], cstr("")) } }
fn lte_real(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(llvm.builder, llvm::LLVMRealPredicate::LLVMRealOLE, args[0], args[1], cstr("")) } }
fn gte_real(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(llvm.builder, llvm::LLVMRealPredicate::LLVMRealOGE, args[0], args[1], cstr("")) } }

fn eq_bool(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(llvm.builder, llvm::LLVMIntPredicate::LLVMIntEQ, args[0], args[1], cstr("")) } }
fn ne_bool(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(llvm.builder, llvm::LLVMIntPredicate::LLVMIntNE, args[0], args[1], cstr("")) } }
fn not_bool(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildNot(llvm.builder, args[0], cstr("")) } }

fn deref(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildLoad(llvm.builder, args[0], cstr("")) } }

fn sprintf(llvm: &LLVM, mut args: Vec<LLVMValueRef>) -> LLVMValueRef {
    unsafe {
        llvm.build_call_by_name("sprintf", &mut args)
    }
}

unsafe fn sizeof_value(llvm: &LLVM, mut args: Vec<LLVMValueRef>) -> LLVMValueRef {
    let ltype = LLVMPointerType(LLVMTypeOf(args[0]), 0);
    let mut indices = vec!(llvm.i32_const(1));
    let pointer = LLVMBuildGEP(llvm.builder, llvm.null_const(ltype), indices.as_mut_ptr(), indices.len() as u32, cstr(""));
    LLVMBuildPtrToInt(llvm.builder, pointer, llvm.i64_type(), cstr(""))
}


/*
unsafe fn buffer_get(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    let size = LLVMBuildMul(llvm.builder, args[1], args[2], cstr(""));
    let mut indices = vec!(size);
    let pointer = LLVMBuildGEP(llvm.builder, args[0], indices.as_mut_ptr(), indices.len() as u32, cstr(""));
    let value = LLVMBuildLoad(llvm.builder, pointer, cstr(""));
    LLVMBuildCast(llvm.builder, llvm::LLVMOpcode::LLVMZExt, value, llvm.i64_type(), cstr("tmp"))
}
*/


unsafe fn buffer_allocator(llvm: &LLVM, objtype: LLVMTypeRef, mut args: Vec<LLVMValueRef>) -> LLVMValueRef {
    llvm.null_const(objtype)
}

unsafe fn buffer_constructor(llvm: &LLVM, objtype: LLVMTypeRef, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    let ptr = LLVMBuildArrayMalloc(llvm.builder, LLVMInt64TypeInContext(llvm.context), args[1], cstr("tmp"));
    LLVMBuildPointerCast(llvm.builder, ptr, objtype, cstr("ptr"))
}

unsafe fn buffer_resize(llvm: &LLVM, objtype: LLVMTypeRef, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    let buffer = LLVMBuildPointerCast(llvm.builder, args[0], llvm.str_type(), cstr("tmp"));
    let size = LLVMBuildMul(llvm.builder, args[1], LLVMSizeOf(LLVMInt64TypeInContext(llvm.context)), cstr("tmp"));
    let newptr = llvm.build_call_by_name("realloc", &mut vec!(buffer, size));
    LLVMBuildPointerCast(llvm.builder, newptr, objtype, cstr("ptr"))
}

unsafe fn buffer_get_method(llvm: &LLVM, objtype: LLVMTypeRef, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    // TODO generate this in place of the custom function
    //Return(
    //  AccessVar(
    //    AccessOffset(
    //        AccessValue(arg 0),
    //        Cast(AccessValue(arg 1), i32)
    //    )
    //  )
    //)
    // AccessOffset has one index, AccessField has a (0, fieldnum) to deref it first

    let index = LLVMBuildCast(llvm.builder, llvm::LLVMOpcode::LLVMTrunc, args[1], llvm.i32_type(), cstr("tmp"));
    let mut indices = vec!(index);
    let pointer = LLVMBuildGEP(llvm.builder, args[0], indices.as_mut_ptr(), indices.len() as u32, cstr("tmp"));
    LLVMBuildLoad(llvm.builder, pointer, cstr("tmp"))
}

unsafe fn buffer_set_method(llvm: &LLVM, objtype: LLVMTypeRef, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    let index = LLVMBuildCast(llvm.builder, llvm::LLVMOpcode::LLVMTrunc, args[1], llvm.i32_type(), cstr("tmp"));
    let mut indices = vec!(index);
    let pointer = LLVMBuildGEP(llvm.builder, args[0], indices.as_mut_ptr(), indices.len() as u32, cstr("tmp"));
    let value = llvm.build_cast(llvm.str_type(), args[2]);
    LLVMBuildStore(llvm.builder, value, pointer);

    llvm.null_const(llvm.str_type())
}


unsafe fn string_get(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    let mut indices = vec!(args[1]);
    let pointer = LLVMBuildGEP(llvm.builder, args[0], indices.as_mut_ptr(), indices.len() as u32, cstr("tmp"));
    let value = LLVMBuildLoad(llvm.builder, pointer, cstr("tmp"));
    LLVMBuildCast(llvm.builder, llvm::LLVMOpcode::LLVMZExt, value, llvm.i64_type(), cstr("tmp"))
}


unsafe fn println(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    llvm.build_call_by_name("puts", &mut vec!(args[0]));
    //llvm.i64_const(0)
    llvm.null_const(llvm.str_type())
}

//unsafe fn readline(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef {
//    let buffer = llvm.build_call_by_name("malloc", &mut vec!(llvm.i32_const(2048)));
//    llvm.build_call_by_name("gets", &mut vec!(buffer));
//    let len = llvm.build_call_by_name("strlen", &mut vec!(buffer));
//    llvm.build_call_by_name("realloc", &mut vec!(buffer, len))
//}


