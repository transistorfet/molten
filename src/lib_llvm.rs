
use std::ptr;
use std::fmt::Debug;
use std::collections::HashMap;
use std::collections::hash_map::Entry;


extern crate llvm_sys as llvm;
use self::llvm::prelude::*;
use self::llvm::core::*;

use types::{ Type, check_type, Check };
use utils::UniqueID;
use parser::{ parse_type };
use scope::{ Scope, ScopeRef, ScopeMapRef, check_for_typevars, mangle_name, unmangle_name };
use compiler_llvm::*;


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
        let name = if let Some(uname) = unmangle_name(name) {
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

pub fn make_global<'a, V, T>(builtins: &Vec<Builtin<'a>>) -> ScopeMapRef<V, T> where V: Clone + Debug, T: Clone + Debug {
    let map = ScopeMapRef::new();
    let primatives = map.add(ScopeMapRef::<V, T>::PRIMATIVE, None);

    register_builtins_vec(primatives.clone(), primatives.clone(), builtins);

    let global = map.add(ScopeMapRef::<V, T>::GLOBAL, Some(primatives));
    global.borrow_mut().set_global(true);

    return map;
}
 
pub fn register_builtins_vec<'a, V, T>(scope: ScopeRef<V, T>, tscope: ScopeRef<V, T>, entries: &Vec<Builtin<'a>>) where V: Clone + Debug, T: Clone + Debug {
    for node in entries {
        register_builtins_node(scope.clone(), tscope.clone(), node);
    }
}

pub fn register_builtins_node<'a, V, T>(scope: ScopeRef<V, T>, tscope: ScopeRef<V, T>, node: &Builtin<'a>) where V: Clone + Debug, T: Clone + Debug {
    match *node {
        Builtin::Type(ref name, ref ttype) => scope.borrow_mut().define_type(String::from(*name), ttype.clone()),
        Builtin::Func(ref name, ref ftype, _) => {
            let ftype = parse_type(ftype);
            check_for_typevars(tscope.clone(), &ftype);
            Scope::define_func_variant(scope.clone(), String::from(*name), tscope.clone(), ftype.clone().unwrap());
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
                    if scope.borrow().find(&name).is_some() {
                        scope.borrow_mut().assign(&name, func);
                    }
                },
                Func::Runtime(func) => {
                    if scope.borrow().get_variable_type(&name).unwrap().is_overloaded() {
                        name = mangle_name(&name, ftype.get_argtypes());
                        scope.borrow_mut().define(name.clone(), Some(ftype));
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
                build_class_type(data, scope.clone(), &name, structdef.clone())
            } else {
                let lltype = get_type(data, scope.clone(), scope.borrow().find_type(&name).unwrap(), true);
                scope.borrow_mut().set_type_value(&name, TypeValue { structdef: structdef.clone(), value: lltype });
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
    if scope.borrow().find(name).is_some() {
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

        Builtin::Func("readline",   "() -> String",                 Func::Runtime(build_lib_readline)),


        Builtin::Type("Nil",    Type::Object(String::from("Nil"), vec!())),
        Builtin::Type("Bool",   Type::Object(String::from("Bool"), vec!())),
        Builtin::Type("Byte",   Type::Object(String::from("Byte"), vec!())),
        Builtin::Type("Int",    Type::Object(String::from("Int"), vec!())),
        Builtin::Type("Real",   Type::Object(String::from("Real"), vec!())),
        Builtin::Type("String", Type::Object(String::from("String"), vec!())),
        //Builtin::Type("Class",  Type::Object(String::from("Class"), vec!())),
        Builtin::Type("Buffer", Type::Object(String::from("Buffer"), vec!(Type::Variable(String::from("item"))))),
        Builtin::Type("List",   Type::Object(String::from("List"), vec!(Type::Variable(String::from("item"))))),

        Builtin::Class("Int", vec!(), vec!(
            Builtin::Func("+",   "(Int, Int) -> Int", Func::Comptime(add_int)),
            Builtin::Func("add", "(Int, Int) -> Int", Func::Runtime(build_lib_add)),
        )),

        Builtin::Class("String", vec!(), vec!(
            //Builtin::Func("push", "(String, String) -> String", Func::Comptime(add_int)),
            Builtin::Func("[]",   "(String, Int) -> Int",       Func::Runtime(build_string_get)),
        )),

        Builtin::Class("Buffer", vec!(), vec!(
            Builtin::Func("__alloc__",  "() -> Buffer['item]",                      Func::Runtime(build_buffer_allocator)),
            Builtin::Func("new",        "(Buffer['item], Int) -> Buffer['item]",    Func::Runtime(build_buffer_constructor)),
            Builtin::Func("resize",     "(Buffer['item], Int) -> Buffer['item]",    Func::Runtime(build_buffer_resize)),
            Builtin::Func("[]",         "(Buffer['item], Int) -> 'item",            Func::Runtime(build_buffer_get)),
            Builtin::Func("[]",         "(Buffer['item], Int, 'item) -> 'item",     Func::Runtime(build_buffer_set)),
        )),

/*
        Builtin::Class("List", vec!(
            (String::from("items"), parse_type("'item").unwrap()),
            (String::from("size"), parse_type("Int").unwrap()),
            (String::from("capacity"), parse_type("Int").unwrap()),
        ), vec!(
            Builtin::Func("new",  "(List['item]) -> List['item]",       Func::Runtime(build_list_constructor)),
            Builtin::Func("push", "(List['item], 'item) -> Int",        Func::Runtime(build_list_push)),
            Builtin::Func("[]",   "(List['item], Int) -> 'item",        Func::Runtime(build_list_get)),
            Builtin::Func("[]",   "(List['item], Int, 'item) -> 'item", Func::Runtime(build_list_set)),
        )),
*/


        Builtin::Func("*",   "(Int, Int) -> Int",  Func::Comptime(mul_int)),
        Builtin::Func("/",   "(Int, Int) -> Int",  Func::Comptime(div_int)),
        //Builtin::Func("^",   "(Int, Int) -> Int",  Func::Comptime(exp_int)),
        //Builtin::Func("%",   "(Int, Int) -> Int",  Func::Comptime(mod_int)),
        Builtin::Func("+",   "(Int, Int) -> Int",  Func::Comptime(add_int)),
        Builtin::Func("-",   "(Int, Int) -> Int",  Func::Comptime(sub_int)),
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
        //Builtin::Func("~",   "(Int) -> Int",       Func::Comptime(com_int)),
        Builtin::Func("not", "(Int) -> Bool",      Func::Comptime(not_int)),


        Builtin::Func("*",   "(Real, Real) -> Real",  Func::Comptime(mul_real)),
        Builtin::Func("/",   "(Real, Real) -> Real",  Func::Comptime(div_real)),
        //Builtin::Func("^",   "(Real, Real) -> Real",  Func::Comptime(exp_real)),
        Builtin::Func("+",   "(Real, Real) -> Real",  Func::Comptime(add_real)),
        Builtin::Func("-",   "(Real, Real) -> Real",  Func::Comptime(sub_real)),
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
fn and_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildAnd(data.builder, args[0], args[1], label("tmp")) } }
fn or_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildOr(data.builder, args[0], args[1], label("tmp")) } }
fn eq_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntEQ, args[0], args[1], label("tmp")) } }
fn ne_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntNE, args[0], args[1], label("tmp")) } }
fn lt_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntSLT, args[0], args[1], label("tmp")) } }
fn gt_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntSGT, args[0], args[1], label("tmp")) } }
fn lte_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntSLE, args[0], args[1], label("tmp")) } }
fn gte_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntSGE, args[0], args[1], label("tmp")) } }
fn not_int(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildNot(data.builder, args[0], label("tmp")) } }

fn add_real(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFAdd(data.builder, args[0], args[1], label("tmp")) } }
fn sub_real(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFSub(data.builder, args[0], args[1], label("tmp")) } }
fn mul_real(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFMul(data.builder, args[0], args[1], label("tmp")) } }
fn div_real(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFDiv(data.builder, args[0], args[1], label("tmp")) } }
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

/*
pub unsafe fn build_malloc(data: &LLVM, size: LLVMValueRef) -> LLVMValueRef {
    build_call(data, "malloc", &mut vec!(size))
}

pub unsafe fn build_malloc_const(data: &LLVM, size: usize) -> LLVMValueRef {
    build_call(data, "malloc", &mut vec!(LLVMConstInt(int_type(data), size as u64, 0)))
}

pub unsafe fn build_realloc(data: &LLVM, ptr: LLVMValueRef, size: LLVMValueRef) -> LLVMValueRef {
    build_call(data, "realloc", &mut vec!(ptr, size))
}
*/

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
    let function = build_function_start(data, name, vec!(objtype, int_type(data)), int_type(data));
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





/*
pub fn register_string_type<V, T>(scope: ScopeRef<V, T>, name: String) where V: Clone + Debug, T: Clone + Debug {
    let classdef = Scope::new_ref(None);
    classdef.borrow_mut().set_basename(name.clone());

    let bintype = parse_type("(Int, Int) -> Int");
    let booltype = parse_type("(Int, Int) -> Bool");
    classdef.borrow_mut().define(String::from("push"), parse_type("(String, String) -> String"));


    scope.borrow_mut().set_class_def(&name, None, classdef);
}

pub fn register_list_type<V, T>(scope: ScopeRef<V, T>, name: String) where V: Clone + Debug, T: Clone + Debug {
    let classdef = Scope::new_ref(None);
    classdef.borrow_mut().set_basename(name.clone());
    scope.borrow_mut().set_class_def(&name, None, classdef.clone());

    let tscope = Scope::new_ref(Some(scope.clone()));
    tscope.borrow_mut().define_type(String::from("item"), Type::Variable(String::from("item")));

    //let bintype = parse_type("(Int, Int) -> Int");
    //let booltype = parse_type("(Int, Int) -> Bool");
    //classdef.borrow_mut().define(String::from("push"), parse_type("(String, String) -> String"));
    classdef.borrow_mut().define(String::from("new"), parse_type("() -> List['a]"));
    classdef.borrow_mut().define(String::from("push"), parse_type("(List['a], 'a) -> Int"));
    //classdef.borrow_mut().define(String::from("[]"), parse_type("(List['a], Int) -> 'a"));
    Scope::define_func_variant(classdef.clone(), String::from("[]"), tscope.clone(), parse_type("(List['item], Int) -> 'item").unwrap());
    Scope::define_func_variant(classdef.clone(), String::from("[]"), tscope.clone(), parse_type("(List['item], Int, 'item) -> 'item").unwrap());

    //use compiler_llvm;
    //scope.borrow_mut().set_type_value(&name, compiler_llvm::TypeValue{ structdef, compiler_llvm::build_class_type() });
}
*/






/*
unsafe fn compile_builtin(data: &LLVM, func: LLVMValueRef, scope: ScopeRef<Value, TypeValue>, name: &String, largs: &Vec<LLVMValueRef>, stype: Type) -> Option<LLVMValueRef> {
    if largs.len() <= 0 {
        return None
    }

    println!("**BUILTINS: {:?}", stype);
    match stype {
        Type::Function(ref sargs, _) => {
            match sargs[0] {
                Type::Object(ref tname, ref ptypes) => {
                    match data.builtins.get(tname) {
                        Some(ref map) => match map.get(name) {
                            Some(builtin) => {
                                Some(builtin(data, largs.clone()))
                            },
                            None => None
                        },
                        None => None
                    }
                },
                _ => None
            }
        },
        _ => None
    }
}

use std::collections::HashMap;

pub type Function = fn(&LLVM, Vec<LLVMValueRef>) -> LLVMValueRef;
pub type FunctionMap = HashMap<String, Function>;
pub type TypeFunctionMap = HashMap<String, FunctionMap>;

unsafe fn initialize_builtins(data: &mut LLVM, scope: ScopeRef<Value, TypeValue>) {
    initialize_type_int(data);
    initialize_type_real(data);
    initialize_type_string(data);
    initialize_type_list(data, scope.clone());
}

unsafe fn initialize_type_int(data: &mut LLVM) {
    let mut builtins: FunctionMap = HashMap::new();

    build_lib_add(data);
    let global = data.map.get_global();
    let intdef = global.borrow().get_class_def(&String::from("Int"));
    intdef.borrow_mut().assign(&String::from("add"), LLVMGetNamedFunction(data.module, label("builtin.add")));

    let ftype = Type::Function(vec!(Type::Object(String::from("Int"), vec!()), Type::Object(String::from("Int"), vec!())), Box::new(Type::Object(String::from("Int"), vec!())));
    global.borrow_mut().define_assign(mangle_name(&String::from("+"), &vec!(Type::Object(String::from("Int"), vec!()), Type::Object(String::from("Int"), vec!()))), Some(ftype), LLVMGetNamedFunction(data.module, label("builtin.add")));


    fn add(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildAdd(data.builder, args[0], args[1], label("tmp")) } }
    fn sub(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildSub(data.builder, args[0], args[1], label("tmp")) } }
    fn mul(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildMul(data.builder, args[0], args[1], label("tmp")) } }
    fn div(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildSDiv(data.builder, args[0], args[1], label("tmp")) } }
    fn eq(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntEQ, args[0], args[1], label("tmp")) } }
    fn ne(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntNE, args[0], args[1], label("tmp")) } }
    fn lt(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntSLT, args[0], args[1], label("tmp")) } }
    fn gt(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntSGT, args[0], args[1], label("tmp")) } }
    fn lte(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntSLE, args[0], args[1], label("tmp")) } }
    fn gte(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntSGE, args[0], args[1], label("tmp")) } }
    fn not(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildNot(data.builder, args[0], label("tmp")) } }

    builtins.insert(String::from("+"), add);
    builtins.insert(String::from("-"), sub);
    builtins.insert(String::from("*"), mul);
    builtins.insert(String::from("/"), div);
    builtins.insert(String::from("=="), eq);
    builtins.insert(String::from("!="), ne);
    builtins.insert(String::from("<"), lt);
    builtins.insert(String::from(">"), gt);
    builtins.insert(String::from("<="), lte);
    builtins.insert(String::from(">="), gte);
    builtins.insert(String::from("not"), not);

    //scope.borrow_mut().update_builtins(&String::from("Int"), builtins);
    data.builtins.insert(String::from("Bool"), builtins.clone());
    data.builtins.insert(String::from("Int"), builtins);
}

unsafe fn initialize_type_real(data: &mut LLVM) {
    let mut builtins: FunctionMap = HashMap::new();

    fn add(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFAdd(data.builder, args[0], args[1], label("tmp")) } }
    fn sub(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFSub(data.builder, args[0], args[1], label("tmp")) } }
    fn mul(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFMul(data.builder, args[0], args[1], label("tmp")) } }
    fn div(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFDiv(data.builder, args[0], args[1], label("tmp")) } }
    fn eq(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOEQ, args[0], args[1], label("tmp")) } }
    fn ne(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealONE, args[0], args[1], label("tmp")) } }
    fn lt(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOLT, args[0], args[1], label("tmp")) } }
    fn gt(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOGT, args[0], args[1], label("tmp")) } }
    fn lte(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOLE, args[0], args[1], label("tmp")) } }
    fn gte(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOGE, args[0], args[1], label("tmp")) } }
    fn not(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildNot(data.builder, args[0], label("tmp")) } }

    builtins.insert(String::from("+"), add);
    builtins.insert(String::from("-"), sub);
    builtins.insert(String::from("*"), mul);
    builtins.insert(String::from("/"), div);
    builtins.insert(String::from("=="), eq);
    builtins.insert(String::from("!="), ne);
    builtins.insert(String::from("<"), lt);
    builtins.insert(String::from(">"), gt);
    builtins.insert(String::from("<="), lte);
    builtins.insert(String::from(">="), gte);
    builtins.insert(String::from("not"), not);

    //scope.borrow_mut().update_builtins(&String::from("Int"), builtins);
    data.builtins.insert(String::from("Real"), builtins);
}

unsafe fn initialize_type_string(data: &mut LLVM) {
    let mut builtins: FunctionMap = HashMap::new();

    //build_lib_str(data);
    //build_lib_strcat(data);

    fn add(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFAdd(data.builder, args[0], args[1], label("tmp")) } }
    //fn sub(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFSub(data.builder, args[0], args[1], label("tmp")) } }
    //fn mul(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFMul(data.builder, args[0], args[1], label("tmp")) } }
    //fn div(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFDiv(data.builder, args[0], args[1], label("tmp")) } }
    fn eq(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOEQ, args[0], args[1], label("tmp")) } }
    fn ne(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealONE, args[0], args[1], label("tmp")) } }
    //fn lt(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOLT, args[0], args[1], label("tmp")) } }
    //fn gt(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOGT, args[0], args[1], label("tmp")) } }
    //fn lte(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOLE, args[0], args[1], label("tmp")) } }
    //fn gte(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOGE, args[0], args[1], label("tmp")) } }
    //fn not(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildNot(data.builder, args[0], label("tmp")) } }

    builtins.insert(String::from("+"), add);
    //builtins.insert(String::from("-"), sub);
    //builtins.insert(String::from("*"), mul);
    //builtins.insert(String::from("/"), div);
    builtins.insert(String::from("=="), eq);
    builtins.insert(String::from("!="), ne);
    //builtins.insert(String::from("<"), lt);
    //builtins.insert(String::from(">"), gt);
    //builtins.insert(String::from("<="), lte);
    //builtins.insert(String::from(">="), gte);
    //builtins.insert(String::from("not"), not);

    //scope.borrow_mut().update_builtins(&String::from("Int"), builtins);
    data.builtins.insert(String::from("String"), builtins);
}

unsafe fn initialize_type_list(data: &mut LLVM, scope: ScopeRef<Value, TypeValue>) {
    let name = String::from("List");
    let classdef = scope.borrow().get_class_def(&name);

    use parser::parse_type;
    let structdef = vec!(
        (String::from("size"), parse_type("Int").unwrap()),
        (String::from("capacity"), parse_type("Int").unwrap()),
        (String::from("items"), parse_type("'item").unwrap())
    );
    let lltype = build_class_type(data, scope.clone(), &name, structdef.clone());
    scope.borrow_mut().set_type_value(&name, TypeValue { structdef: structdef, value: lltype });

    let cons = build_list_constructor(data, lltype);
    classdef.borrow_mut().assign(&String::from("new"), cons);

    let push = build_list_push(data, lltype);
    classdef.borrow_mut().assign(&String::from("push"), push);

    let get = build_list_get(data, lltype);
    classdef.borrow_mut().assign(&String::from("[]"), get);
}
*/



/*
unsafe fn build_lib_str(data: &LLVM) {
    let function = build_function_start(data, "str", vec!(int_type(data)), str_type(data));
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);

    let pointer = LLVMBuildAlloca(data.builder, str_type(data), label("buffer"));
    LLVMBuildStore(data.builder, build_malloc_const(data, 22), pointer);

    let value = LLVMBuildLoad(data.builder, pointer, label("tmp"));
    let num = LLVMGetParam(function, 0);
    build_call(data, "sprintf", &mut vec!(value, build_str_const(data, "%d"), num));

    LLVMBuildRet(data.builder, value);
}

unsafe fn build_lib_strcat(data: &LLVM) {
    let function = build_function_start(data, "strcat", vec!(str_type(data), str_type(data)), str_type(data));
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);

    let str1 = LLVMGetParam(function, 0);
    let str2 = LLVMGetParam(function, 1);
    let str1len = build_call(data, "strlen", &mut vec!(str1));
    let str2len = build_call(data, "strlen", &mut vec!(str2));
    let sum = LLVMBuildAdd(data.builder, str1len, str2len, label("tmp"));

    let buffer = build_malloc(data, sum);

    build_call(data, "memcpy", &mut vec!(buffer, str1, str1len));
    //build_call(data, "memcpy", &mut vec!(buffer, str2, str1len));

    LLVMBuildRet(data.builder, buffer);
}
*/


