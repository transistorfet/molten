
use std::ptr;
//use std::collections::HashMap;
//use std::collections::hash_map::Entry;


extern crate llvm_sys as llvm;
use self::llvm::prelude::*;
use self::llvm::core::*;

use abi::ABI;
use types::Type;
use session::Session;
use parser::{ parse_type };
use hir::{ NodeID, Mutability, Visibility };
use scope::{ Scope, ScopeRef, ScopeMapRef, Context };
use binding::{ bind_type_names };
use config::Options;
use misc::{ r, UniqueID };

use defs::classes::{ ClassDef, Define };
use defs::functions::{ AnyFunc };

use transform::llcode::{ LLType, LLLink };
use transform::transform::Transformer;

use llvm::codegen::{ LLVM, cstr, cstring };

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

    let _global = session.map.add(ScopeMapRef::GLOBAL, Some(primatives));
    // NOTE disabling this allows the identifier closure convertor to convert references to variables inside the main function
    //global.set_context(Context::Global);
}

pub fn declare_builtins_vec<'sess>(session: &Session, scope: ScopeRef, entries: &Vec<BuiltinDef<'sess>>) {
    for node in entries {
        declare_builtins_node(session, scope.clone(), node);
    }
}

pub fn declare_builtins_node<'sess>(session: &Session, scope: ScopeRef, node: &BuiltinDef<'sess>) {
    match node {
        BuiltinDef::Func(id, name, ftype, func) => {
            let tscope = if scope.is_primative() {
                Scope::new_ref(Some(scope.clone()))
            } else {
                scope.clone()
            };

            let mut ftype = parse_type(ftype);
            bind_type_names(session, tscope.clone(), ftype.as_mut(), true).unwrap();
            debug!("BUILTIN TYPE: {:?}", ftype);
            let abi = ftype.as_ref().map(|t| t.get_abi().unwrap()).unwrap_or(ABI::Molten);
            AnyFunc::define(session, scope.clone(), *id, Visibility::Global, Some(name), abi, ftype.clone()).unwrap();
        },
        BuiltinDef::Class(id, name, params, _, entries) => {
            let tscope = session.map.get_or_add(*id, Some(scope.clone()));
            let mut ttype = Type::Object(String::from(*name), *id, params.clone());
            bind_type_names(session, tscope.clone(), Some(&mut ttype), true).unwrap();
            ClassDef::define(session, scope.clone(), *id, ttype, None).unwrap();

            declare_builtins_vec(session, tscope.clone(), entries);
        },
    }
}


pub fn initialize_builtins<'sess>(llvm: &LLVM<'sess>, transformer: &mut Transformer, scope: ScopeRef, entries: &Vec<BuiltinDef<'sess>>) {
    unsafe {
        declare_irregular_functions(llvm);
        define_builtins_vec(llvm, transformer, ptr::null_mut(), scope.clone(), entries);
    }
}

pub unsafe fn define_builtins_vec<'sess>(llvm: &LLVM<'sess>, transformer: &mut Transformer, objtype: LLVMTypeRef, scope: ScopeRef, entries: &Vec<BuiltinDef<'sess>>) {
    for node in entries {
        define_builtins_node(llvm, transformer, objtype, scope.clone(), node);
    }
}

pub unsafe fn define_builtins_node<'sess>(llvm: &LLVM<'sess>, transformer: &mut Transformer, objtype: LLVMTypeRef, _scope: ScopeRef, node: &BuiltinDef<'sess>) {
    match node {
        BuiltinDef::Func(id, sname, _, func) => {
            let ftype = llvm.session.get_type(*id).unwrap();
            let (argtypes, rettype, abi) = ftype.get_function_types().unwrap();
            let name = abi.mangle_name(sname, argtypes, 2);
            let ltype = transformer.transform_func_def_type(abi, &argtypes.as_vec(), rettype);
            match *func {
                FuncKind::FromNamed => {
                    llvm.set_value(*id, LLVMGetNamedFunction(llvm.module, cstring(&name)));
                },
                FuncKind::External => {
                    let func = LLVMAddFunction(llvm.module, cstring(&name), llvm.build_type(&ltype));
                    llvm.set_value(*id, func);
                },
                FuncKind::Method(func) => {
                    let function = build_lib_method(llvm, name.as_str(), objtype, &ltype, func);
                    llvm.set_value(*id, function);
                },
                FuncKind::Function(func) => {
                    if abi == ABI::Molten {
                        let func_id = NodeID::generate();
                        let function = build_lib_function(llvm, format!("{}_func", name).as_str(), &ltype, func);
                        llvm.set_value(func_id, function);

                        let closure = llvm.struct_const(vec!(function, llvm.null_const(llvm.str_type())));
                        let rtype = LLVMTypeOf(closure);
                        let global = llvm.build_def_global(name.as_str(), LLLink::Once, rtype, Some(closure));
                        llvm.set_value(*id, global);
                    } else {
                        let function = build_lib_function(llvm, name.as_str(), &ltype, func);
                        llvm.set_value(*id, function);
                    }
                },
            }
        },
        BuiltinDef::Class(id, _name, _, structdef, entries) => {
            let tscope = llvm.session.map.get(id);
            //let cname = String::from(*name);
            let classdef = llvm.session.get_def(*id).unwrap().as_class().unwrap();
            if entries.len() <= 0 {
                classdef.set_primative();
            }
            let ltype = transformer.transform_value_type(&llvm.session.get_type(*id).unwrap());

            let lltype = if structdef.len() > 0 {
                for (ref field, ref ttype) in structdef {
                    classdef.structdef.add_field(llvm.session, NodeID::generate(), Mutability::Mutable, field, ttype.clone(), Define::IfNotExists);
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
    /*
     * Memory Functions
     */

    if Options::as_ref().no_gc {
        declare_c_function(llvm, "malloc", &mut [llvm.i64_type()], llvm.str_type(), false);
        declare_c_function(llvm, "realloc", &mut [llvm.str_type(), llvm.i64_type()], llvm.str_type(), false);
        declare_c_function(llvm, "free", &mut [llvm.str_type()], llvm.void_type(), false);
    } else {
        declare_c_function(llvm, "GC_init", &mut [], llvm.void_type(), false);
        declare_c_function(llvm, "GC_malloc", &mut [llvm.i64_type()], llvm.str_type(), false);
        declare_c_function(llvm, "GC_realloc", &mut [llvm.str_type(), llvm.i64_type()], llvm.str_type(), false);
        declare_c_function(llvm, "GC_free", &mut [llvm.str_type()], llvm.void_type(), false);
    }

    build_lib_function(llvm, "molten_init", &LLType::Function(vec!(), r(LLType::I32)), molten_init);
    build_lib_function(llvm, "molten_malloc", &LLType::Function(vec!(LLType::I64), r(LLType::Ptr(r(LLType::I8)))), molten_malloc);
    build_lib_function(llvm, "molten_realloc", &LLType::Function(vec!(LLType::Ptr(r(LLType::I8)), LLType::I64), r(LLType::Ptr(r(LLType::I8)))), molten_realloc);
    build_lib_function(llvm, "molten_free", &LLType::Function(vec!(LLType::Ptr(r(LLType::I8))), r(LLType::I32)), molten_free);

    /*
     * Exception Functions
     */

    declare_c_function(llvm, "llvm.pow.f64", &mut [llvm.f64_type(), llvm.f64_type()], llvm.f64_type(), false);

    declare_c_function(llvm, "setjmp", &mut [llvm.str_type()], llvm.i32_type(), false);
    declare_c_function(llvm, "longjmp", &mut [llvm.str_type(), llvm.i32_type()], llvm.void_type(), false);
    //declare_c_function(llvm, "llvm.eh.sjlj.setjmp", &mut [llvm.str_type()], llvm.i32_type(), false);
    //declare_c_function(llvm, "llvm.eh.sjlj.longjmp", &mut [llvm.str_type()], llvm.void_type(), false);
    //declare_c_function(llvm, "llvm.stacksave", &mut [], llvm.str_type(), false);

    //declare_function(llvm, "__gxx_personality_v0", &mut [llvm.str_type(), llvm.str_type()], llvm.i64_type(), true);


    /*
     * libc Functions
     */

    let filetype = LLVMStructCreateNamed(llvm.context, cstr("struct._IO_FILE"));
    let filetypeptr = LLVMPointerType(filetype, 0);

    let stdout = LLVMAddGlobal(llvm.module, filetypeptr, cstr("stdout"));
    LLVMSetLinkage(stdout, llvm::LLVMLinkage::LLVMExternalLinkage);
    LLVMSetAlignment(stdout, 8);

    let stdin = LLVMAddGlobal(llvm.module, filetypeptr, cstr("stdin"));
    LLVMSetLinkage(stdin, llvm::LLVMLinkage::LLVMExternalLinkage);
    LLVMSetAlignment(stdin, 8);

    declare_c_function(llvm, "fgetc", &mut [filetypeptr], llvm.i32_type(), false);
    declare_c_function(llvm, "fgets", &mut [llvm.str_type(), llvm.i64_type(), filetypeptr], llvm.i64_type(), false);
    declare_c_function(llvm, "fputs", &mut [llvm.str_type(), filetypeptr], llvm.i64_type(), false);

    declare_c_function(llvm, "puts", &mut [llvm.str_type()], llvm.i64_type(), false);
    //declare_c_function(llvm, "printf", &mut [llvm.str_type()], llvm.i64_type(), true);
    declare_c_function(llvm, "sprintf", &mut [llvm.str_type(), llvm.str_type()], llvm.i64_type(), true);


    //declare_c_function(llvm, "strlen", &mut [llvm.str_type()], llvm.i64_type(), false);
    declare_c_function(llvm, "memcpy", &mut [llvm.str_type(), llvm.str_type(), llvm.i64_type()], llvm.str_type(), false);
}

unsafe fn build_lib_function(llvm: &LLVM, name: &str, ltype: &LLType, func: PlainFunction) -> LLVMValueRef {
    let function = LLVMAddFunction(llvm.module, cstr(name), llvm.build_type(&ltype));
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceODRLinkage);

    //let name = "alwaysinline";
    //let kind = LLVMGetEnumAttributeKindForName(cstr(name), name.len());
    //let attribute = LLVMCreateEnumAttribute(llvm.context, kind, 0);
    //LLVMAddAttributeAtIndex(function, 0, attribute);

    let bb = LLVMAppendBasicBlockInContext(llvm.context, function, cstr("entry"));
    LLVMPositionBuilderAtEnd(llvm.builder, bb);
    *llvm.curfunc.borrow_mut() = function;

    let args = (0..ltype.argcount()).map(|i| LLVMGetParam(function, i as u32)).collect();
    let ret = func(llvm, args);
    LLVMBuildRet(llvm.builder, llvm.cast_typevars(LLVMGetReturnType(LLVMGetElementType(LLVMTypeOf(function))), ret));

    function
}

unsafe fn build_lib_method(llvm: &LLVM, name: &str, objtype: LLVMTypeRef, ltype: &LLType, func: ObjectFunction) -> LLVMValueRef {
    let function = LLVMAddFunction(llvm.module, cstr(name), llvm.build_type(&ltype));
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceODRLinkage);

    //let name = "alwaysinline";
    //let kind = LLVMGetEnumAttributeKindForName(cstr(name), name.len());
    //let attribute = LLVMCreateEnumAttribute(llvm.context, kind, 0);
    //LLVMAddAttributeAtIndex(function, 0, attribute);

    let bb = LLVMAppendBasicBlockInContext(llvm.context, function, cstr("entry"));
    LLVMPositionBuilderAtEnd(llvm.builder, bb);
    *llvm.curfunc.borrow_mut() = function;

    let args = (0..ltype.argcount()).map(|i| LLVMGetParam(function, i as u32)).collect();
    let ret = func(llvm, objtype, args);
    LLVMBuildRet(llvm.builder, llvm.cast_typevars(LLVMGetReturnType(LLVMGetElementType(LLVMTypeOf(function))), ret));

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
        BuiltinDef::Class(id(), "Char",   vec!(), vec!(), vec!()),
        BuiltinDef::Class(id(), "Int",    vec!(), vec!(), vec!()),
        BuiltinDef::Class(id(), "Real",   vec!(), vec!(), vec!()),
        BuiltinDef::Class(id(), "String", vec!(), vec!(), vec!()),

        //    BuiltinDef::Func(id(), "[]",   "(String, Int) -> Int",            FuncKind::Runtime(build_string_get)),
        BuiltinDef::Func(id(), "getindex",  "(String, Int) -> Char / C",                    FuncKind::Function(string_get)),


        BuiltinDef::Class(id(), "Buffer",   vec!(Type::Variable(String::from("item"), UniqueID(0), true)), vec!(), vec!()),
        BuiltinDef::Func(id(), "bufalloc",  "(Int) -> Buffer<'item> / C",                   FuncKind::Function(buffer_alloc)),
        BuiltinDef::Func(id(), "bufresize", "(Buffer<'item>, Int) -> Buffer<'item> / C",    FuncKind::Function(buffer_resize)),
        BuiltinDef::Func(id(), "bufget",    "(Buffer<'item>, Int) -> 'item / C",            FuncKind::Function(buffer_get)),
        BuiltinDef::Func(id(), "bufset",    "(Buffer<'item>, Int, 'item) -> () / C",        FuncKind::Function(buffer_set)),

        /*
        BuiltinDef::Class(id(), "Buffer", vec!(Type::Variable(String::from("item"), UniqueID(0), true)), vec!(), vec!(
            BuiltinDef::Func(id(), "__alloc__",  "() -> Buffer<'item>",                      FuncKind::Method(buffer_allocator)),
            BuiltinDef::Func(id(), "new",        "(Buffer<'item>, Int) -> Buffer<'item>",    FuncKind::Method(buffer_constructor)),
            BuiltinDef::Func(id(), "resize",     "(Buffer<'item>, Int) -> Buffer<'item>",    FuncKind::Method(buffer_resize)),
            BuiltinDef::Func(id(), "[]",         "(Buffer<'item>, Int) -> 'item",            FuncKind::Method(buffer_get_method)),
            BuiltinDef::Func(id(), "[]",         "(Buffer<'item>, Int, 'item) -> 'item",     FuncKind::Method(buffer_set_method)),
        )),
        */

        //BuiltinDef::Class(id(), "List",   vec!(Type::Variable(String::from("item"), UniqueID(0))), vec!(), vec!()),
        //BuiltinDef::Class(id(), "Class",  Type::Object(String::from("Class"), vec!())),

        BuiltinDef::Func(id(), "strlen",     "(String) -> Int / C",             FuncKind::External),
        BuiltinDef::Func(id(), "strcmp",     "(String, String) -> Int / C",     FuncKind::External),
        /*
        BuiltinDef::Func(id(), "memcpy",     "('ptr, 'ptr, Int) -> 'ptr / C",   FuncKind::External),
        BuiltinDef::Func(id(), "strcmp",     "(String, String) -> Int / C",     FuncKind::External),
        BuiltinDef::Func(id(), "puts",       "(String) -> () / C",              FuncKind::External),
        BuiltinDef::Func(id(), "gets",       "(String) -> String / C",          FuncKind::External),
        BuiltinDef::Func(id(), "strlen",     "(String) -> Int / C",             FuncKind::External),
        //BuiltinDef::Func(id(), "sprintf",    "'tmp",                          FuncKind::FromNamed),
        //BuiltinDef::Func(id(), "sprintf2",    "(String, String, '__a1, '__a2) -> () / C", FuncKind::Function(sprintf)),
        BuiltinDef::Func(id(), "sprintf",    "(String, String, '__a1, '__a2) -> () / C", FuncKind::FromNamed),
        */

        BuiltinDef::Func(id(), "print",      "(String) -> () / C",              FuncKind::Function(print)),
        BuiltinDef::Func(id(), "println",    "(String) -> () / C",              FuncKind::Function(println)),
        BuiltinDef::Func(id(), "readline",   "() -> String / C",                FuncKind::Function(readline)),


        //// Unit Builtins ////
        BuiltinDef::Func(id(), "==",  "((), ()) -> Bool",   FuncKind::Function(always_true)),
        BuiltinDef::Func(id(), "!=",  "((), ()) -> Bool",   FuncKind::Function(always_false)),


        //// Boolean Builtins ////
        BuiltinDef::Func(id(), "==",  "(Bool, Bool) -> Bool",   FuncKind::Function(eq_bool)),
        BuiltinDef::Func(id(), "!=",  "(Bool, Bool) -> Bool",   FuncKind::Function(ne_bool)),
        BuiltinDef::Func(id(), "not", "(Bool) -> Bool",         FuncKind::Function(not_bool)),


        //// Integer Builtins ////
        BuiltinDef::Func(id(), "+",   "(Int, Int) -> Int",      FuncKind::Function(add_int)),
        BuiltinDef::Func(id(), "-",   "(Int, Int) -> Int",      FuncKind::Function(sub_int)),
        BuiltinDef::Func(id(), "*",   "(Int, Int) -> Int",      FuncKind::Function(mul_int)),
        BuiltinDef::Func(id(), "/",   "(Int, Int) -> Int",      FuncKind::Function(div_int)),
        BuiltinDef::Func(id(), "%",   "(Int, Int) -> Int",      FuncKind::Function(mod_int)),
        //BuiltinDef::Func(id(), "^",   "(Int, Int) -> Int",    FuncKind::Function(pow_int)),
        //BuiltinDef::Func(id(), "<<",  "(Int, Int) -> Int",    FuncKind::Function(shl_int)),
        //BuiltinDef::Func(id(), ">>",  "(Int, Int) -> Int",    FuncKind::Function(shr_int)),
        BuiltinDef::Func(id(), "&",   "(Int, Int) -> Int",      FuncKind::Function(and_int)),
        BuiltinDef::Func(id(), "|",   "(Int, Int) -> Int",      FuncKind::Function(or_int)),
        BuiltinDef::Func(id(), "<",   "(Int, Int) -> Bool",     FuncKind::Function(lt_int)),
        BuiltinDef::Func(id(), ">",   "(Int, Int) -> Bool",     FuncKind::Function(gt_int)),
        BuiltinDef::Func(id(), "<=",  "(Int, Int) -> Bool",     FuncKind::Function(lte_int)),
        BuiltinDef::Func(id(), ">=",  "(Int, Int) -> Bool",     FuncKind::Function(gte_int)),
        BuiltinDef::Func(id(), "==",  "(Int, Int) -> Bool",     FuncKind::Function(eq_int)),
        BuiltinDef::Func(id(), "!=",  "(Int, Int) -> Bool",     FuncKind::Function(ne_int)),
        BuiltinDef::Func(id(), "~",   "(Int) -> Int",           FuncKind::Function(com_int)),
        BuiltinDef::Func(id(), "not", "(Int) -> Bool",          FuncKind::Function(not_int)),


        //// Character Builtins ////
        BuiltinDef::Func(id(), "<",   "(Char, Char) -> Bool",   FuncKind::Function(lt_char)),
        BuiltinDef::Func(id(), ">",   "(Char, Char) -> Bool",   FuncKind::Function(gt_char)),
        BuiltinDef::Func(id(), "<=",  "(Char, Char) -> Bool",   FuncKind::Function(lte_char)),
        BuiltinDef::Func(id(), ">=",  "(Char, Char) -> Bool",   FuncKind::Function(gte_char)),
        BuiltinDef::Func(id(), "==",  "(Char, Char) -> Bool",   FuncKind::Function(eq_char)),
        BuiltinDef::Func(id(), "!=",  "(Char, Char) -> Bool",   FuncKind::Function(ne_char)),


        //// Real Builtins ////
        BuiltinDef::Func(id(), "+",   "(Real, Real) -> Real",   FuncKind::Function(add_real)),
        BuiltinDef::Func(id(), "-",   "(Real, Real) -> Real",   FuncKind::Function(sub_real)),
        BuiltinDef::Func(id(), "*",   "(Real, Real) -> Real",   FuncKind::Function(mul_real)),
        BuiltinDef::Func(id(), "/",   "(Real, Real) -> Real",   FuncKind::Function(div_real)),
        BuiltinDef::Func(id(), "%",   "(Real, Real) -> Real",   FuncKind::Function(mod_real)),
        BuiltinDef::Func(id(), "^",   "(Real, Real) -> Real",   FuncKind::Function(pow_real)),
        BuiltinDef::Func(id(), "<",   "(Real, Real) -> Bool",   FuncKind::Function(lt_real)),
        BuiltinDef::Func(id(), ">",   "(Real, Real) -> Bool",   FuncKind::Function(gt_real)),
        BuiltinDef::Func(id(), "<=",  "(Real, Real) -> Bool",   FuncKind::Function(lte_real)),
        BuiltinDef::Func(id(), ">=",  "(Real, Real) -> Bool",   FuncKind::Function(gte_real)),
        BuiltinDef::Func(id(), "==",  "(Real, Real) -> Bool",   FuncKind::Function(eq_real)),
        BuiltinDef::Func(id(), "!=",  "(Real, Real) -> Bool",   FuncKind::Function(ne_real)),

        BuiltinDef::Func(id(), "char", "(Int) -> Char",         FuncKind::Function(int_to_char)),
        BuiltinDef::Func(id(), "int", "(Char) -> Int",          FuncKind::Function(char_to_int)),
        BuiltinDef::Func(id(), "int", "(Real) -> Int",          FuncKind::Function(real_to_int)),
        BuiltinDef::Func(id(), "real", "(Int) -> Real",         FuncKind::Function(int_to_real)),

        BuiltinDef::Func(id(), "str", "(()) -> String",         FuncKind::Function(unit_to_str)),
        BuiltinDef::Func(id(), "str", "(Bool) -> String",       FuncKind::Function(bool_to_str)),
        BuiltinDef::Func(id(), "str", "(Char) -> String",       FuncKind::Function(char_to_str)),
        BuiltinDef::Func(id(), "str", "(Int) -> String",        FuncKind::Function(int_to_str)),
        BuiltinDef::Func(id(), "hex", "(Int) -> String",        FuncKind::Function(int_to_hex)),
        BuiltinDef::Func(id(), "str", "(Real) -> String",       FuncKind::Function(real_to_str)),

        BuiltinDef::Func(id(), "+", "(String, String) -> String",  FuncKind::Function(add_str)),
        BuiltinDef::Func(id(), "==", "(String, String) -> Bool",  FuncKind::Function(eq_str)),
        BuiltinDef::Func(id(), "!=", "(String, String) -> Bool",  FuncKind::Function(ne_str)),
    )
}


fn always_true(llvm: &LLVM, _args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { llvm.i1_const(true) } }
fn always_false(llvm: &LLVM, _args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { llvm.i1_const(false) } }

fn eq_bool(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(llvm.builder, llvm::LLVMIntPredicate::LLVMIntEQ, args[0], args[1], cstr("")) } }
fn ne_bool(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(llvm.builder, llvm::LLVMIntPredicate::LLVMIntNE, args[0], args[1], cstr("")) } }
fn not_bool(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildNot(llvm.builder, args[0], cstr("")) } }

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
fn com_int(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildXor(llvm.builder, args[0], llvm.u64_const(0xFFFFFFFFFFFFFFFF), cstr("")) } }
fn not_int(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { llvm.build_cast(llvm.i1_type(), LLVMBuildNot(llvm.builder, args[0], cstr(""))) } }

fn eq_char(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(llvm.builder, llvm::LLVMIntPredicate::LLVMIntEQ, args[0], args[1], cstr("")) } }
fn ne_char(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(llvm.builder, llvm::LLVMIntPredicate::LLVMIntNE, args[0], args[1], cstr("")) } }
fn lt_char(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(llvm.builder, llvm::LLVMIntPredicate::LLVMIntSLT, args[0], args[1], cstr("")) } }
fn gt_char(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(llvm.builder, llvm::LLVMIntPredicate::LLVMIntSGT, args[0], args[1], cstr("")) } }
fn lte_char(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(llvm.builder, llvm::LLVMIntPredicate::LLVMIntSLE, args[0], args[1], cstr("")) } }
fn gte_char(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(llvm.builder, llvm::LLVMIntPredicate::LLVMIntSGE, args[0], args[1], cstr("")) } }

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


fn char_to_int(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildCast(llvm.builder, llvm::LLVMOpcode::LLVMZExt, args[0], llvm.i64_type(), cstr("")) } }
fn int_to_char(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildCast(llvm.builder, llvm::LLVMOpcode::LLVMTrunc, args[0], llvm.i32_type(), cstr("")) } }
fn real_to_int(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFPToSI(llvm.builder, args[0], llvm.i64_type(), cstr("")) } }
fn int_to_real(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildSIToFP(llvm.builder, args[0], llvm.f64_type(), cstr("")) } }

fn unit_to_str(llvm: &LLVM, _args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { llvm.str_const("()") } }



unsafe fn molten_init(llvm: &LLVM, _args: Vec<LLVMValueRef>) -> LLVMValueRef {
    if !Options::as_ref().no_gc {
        llvm.build_call_by_name("GC_init", &mut vec!());
    }
    llvm.i32_const(0)
}

unsafe fn molten_malloc(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    //llvm.build_call_by_name("puts", &mut vec!(LLVMBuildGlobalStringPtr(llvm.builder, cstr("MALLOC"), cstr("__string"))));
    let name = if Options::as_ref().no_gc { "malloc" } else { "GC_malloc" };

    llvm.build_call_by_name(name, &mut vec!(args[0]))
}

unsafe fn molten_realloc(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    let name = if Options::as_ref().no_gc { "realloc" } else { "GC_realloc" };

    let buffer = LLVMBuildPointerCast(llvm.builder, args[0], llvm.str_type(), cstr(""));
    llvm.build_call_by_name(name, &mut vec!(buffer, args[1]))
}

unsafe fn molten_free(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    let name = if Options::as_ref().no_gc { "free" } else { "GC_free" };

    llvm.build_call_by_name(name, &mut vec!(args[0]));
    llvm.i32_const(0)
}



unsafe fn buffer_alloc(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    let size = LLVMBuildMul(llvm.builder, args[0], LLVMSizeOf(llvm.i64_type()), cstr(""));
    let ptr = llvm.build_call_by_name("molten_malloc", &mut vec!(size));
    LLVMBuildPointerCast(llvm.builder, ptr, llvm.ptr_type(), cstr("ptr"))
}

unsafe fn buffer_resize(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    let buffer = LLVMBuildPointerCast(llvm.builder, args[0], llvm.str_type(), cstr(""));
    let size = LLVMBuildMul(llvm.builder, args[1], LLVMSizeOf(llvm.i64_type()), cstr(""));
    let newptr = llvm.build_call_by_name("molten_realloc", &mut vec!(buffer, size));
    LLVMBuildPointerCast(llvm.builder, newptr, llvm.ptr_type(), cstr("ptr"))
}

unsafe fn buffer_get(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef {
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

unsafe fn buffer_set(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    let index = LLVMBuildCast(llvm.builder, llvm::LLVMOpcode::LLVMTrunc, args[1], llvm.i32_type(), cstr("tmp"));
    let mut indices = vec!(index);
    let pointer = LLVMBuildGEP(llvm.builder, args[0], indices.as_mut_ptr(), indices.len() as u32, cstr("tmp"));
    let value = llvm.build_cast(llvm.str_type(), args[2]);
    LLVMBuildStore(llvm.builder, value, pointer);

    llvm.i32_const(0)
}


unsafe fn string_get(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    let mut indices = vec!(args[1]);
    let pointer = LLVMBuildGEP(llvm.builder, args[0], indices.as_mut_ptr(), indices.len() as u32, cstr(""));
    let value = LLVMBuildLoad(llvm.builder, pointer, cstr(""));
    LLVMBuildCast(llvm.builder, llvm::LLVMOpcode::LLVMZExt, value, llvm.i32_type(), cstr(""))
}


unsafe fn print(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    // This would access the char* inside a wide pointer/slice where the second element is the size
    //let str = LLExpr::LoadRef(r(LLExpr::AccessRef(r(args[0]), vec!(LLRef::Field(0)))))
    //LLExpr::CallC(r(LLExpr::GetNamed("fputs")), vec!(str), LLCC::CCC)
    //LLExpr::Literal(LLLit::I32(0))
    llvm.build_call_by_name("fputs", &mut vec!(args[0], llvm.build_load(LLVMGetNamedGlobal(llvm.module, cstr("stdout")))));
    llvm.i32_const(0)
}

unsafe fn println(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    llvm.build_call_by_name("puts", &mut vec!(args[0]));
    llvm.i32_const(0)
}


unsafe fn readline(llvm: &LLVM, _args: Vec<LLVMValueRef>) -> LLVMValueRef {
    let buffer = llvm.build_cast(llvm.str_type(), llvm.build_call_by_name("molten_malloc", &mut vec!(llvm.i64_const(2048))));

    let ret = llvm.build_call_by_name("fgets", &mut vec!(buffer, llvm.i64_const(2048), llvm.build_load(LLVMGetNamedGlobal(llvm.module, cstr("stdin")))));
    // TODO we ignore ret which could cause the buffer to not be null terminated
    let len = llvm.build_call_by_name("strlen", &mut vec!(buffer));
    llvm.build_call_by_name("molten_realloc", &mut vec!(buffer, len))
}


unsafe fn bool_to_str(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    LLVMBuildSelect(llvm.builder, args[0], llvm.str_const("true"), llvm.str_const("false"), cstr(""))
}

unsafe fn char_to_str(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    let buffer = llvm.build_cast(llvm.str_type(), llvm.build_call_by_name("molten_malloc", &mut vec!(llvm.i64_const(22))));

    llvm.build_call_by_name("sprintf", &mut vec!(buffer, llvm.str_const("%c"), args[0]));
    buffer
}

unsafe fn int_to_str(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    let buffer = llvm.build_cast(llvm.str_type(), llvm.build_call_by_name("molten_malloc", &mut vec!(llvm.i64_const(22))));

    llvm.build_call_by_name("sprintf", &mut vec!(buffer, llvm.str_const("%d"), args[0]));
    buffer
}

unsafe fn int_to_hex(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    let buffer = llvm.build_cast(llvm.str_type(), llvm.build_call_by_name("molten_malloc", &mut vec!(llvm.i64_const(22))));

    llvm.build_call_by_name("sprintf", &mut vec!(buffer, llvm.str_const("0x%lX"), args[0]));
    buffer
}

unsafe fn real_to_str(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    let buffer = llvm.build_cast(llvm.str_type(), llvm.build_call_by_name("molten_malloc", &mut vec!(llvm.i64_const(22))));

    llvm.build_call_by_name("sprintf", &mut vec!(buffer, llvm.str_const("%f"), args[0]));
    buffer
}


unsafe fn add_str(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    let len1 = llvm.build_call_by_name("strlen", &mut vec!(args[0]));
    let len2 = llvm.build_call_by_name("strlen", &mut vec!(args[1]));
    let len3 = LLVMBuildAdd(llvm.builder, LLVMBuildAdd(llvm.builder, len1, len2, cstr("")), llvm.i64_const(1), cstr(""));

    let buffer = llvm.build_cast(llvm.str_type(), llvm.build_call_by_name("molten_malloc", &mut vec!(len3)));
    llvm.build_call_by_name("sprintf", &mut vec!(buffer, llvm.str_const("%s%s"), args[0], args[1]));

    buffer
}

unsafe fn eq_str(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    let result = llvm.build_call_by_name("strcmp", &mut vec!(args[0], args[1]));
    LLVMBuildICmp(llvm.builder, llvm_sys::LLVMIntPredicate::LLVMIntEQ, result, llvm.i64_const(0), cstr(""))
}

unsafe fn ne_str(llvm: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef {
    let result = llvm.build_call_by_name("strcmp", &mut vec!(args[0], args[1]));
    LLVMBuildICmp(llvm.builder, llvm_sys::LLVMIntPredicate::LLVMIntNE, result, llvm.i64_const(0), cstr(""))
}

