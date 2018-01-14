
use std::ptr;
use std::fs::File;
use std::io::prelude::*;
use std::ffi::CString;

extern crate llvm_sys as llvm;
use self::llvm::prelude::*;
use self::llvm::core::*;

use import;
use parser::AST;
use scope::{ Scope, ScopeRef, ScopeMapRef, mangle_name };
use types::Type;
use utils::UniqueID;
//use lib_llvm::{ TypeFunctionMap, initialize_builtins };


pub fn compile(map: ScopeMapRef<Value, TypeValue>, module_name: &str, code: &Vec<AST>) {
    unsafe {
        compile_module(map.clone(), map.get_global(), module_name, code)
    }
}


pub type Value = LLVMValueRef;
//pub type TypeValue = LLVMTypeRef;

//#[derive(Clone, Debug, PartialEq)]
//pub enum Value {
//    LLVM(LLVMValueRef),
//    Builtin(UniqueID),
//}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeValue {
    structdef: Vec<(String, Type)>,
    value: LLVMTypeRef,
}


pub struct LLVM<'a> {
    map: ScopeMapRef<Value, TypeValue>,
    builtins: TypeFunctionMap,
    functions: Vec<&'a AST>,
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    //funcpass: LLVMPassManagerRef,
}

type Unwind = Option<(LLVMBasicBlockRef, LLVMBasicBlockRef)>;


unsafe fn compile_module(map: ScopeMapRef<Value, TypeValue>, scope: ScopeRef<Value, TypeValue>, module_name: &str, code: &Vec<AST>) {
    let context = LLVMContextCreate();
    //let module = LLVMModuleCreateWithName(b"main_module\0".as_ptr() as *const _);
    let module = LLVMModuleCreateWithName(label(module_name).as_ptr());
    let builder = LLVMCreateBuilderInContext(context);
    //let funcpass = LLVMCreateFunctionPassManagerForModule(module);
    //LLVMInitializeFunctionPassManager(funcpass);
    //let data = &mut LLVM { map: map, builtins: HashMap::new(), functions: Vec::new(), context: context, module: module, builder: builder, funcpass: funcpass };
    let data = &mut LLVM { map: map, builtins: HashMap::new(), functions: Vec::new(), context: context, module: module, builder: builder };

    declare_functions(data, scope.clone());
    initialize_builtins(data);
    collect_functions_vec(data, scope.clone(), code);
    declare_globals(data, scope.clone());
    for func in &data.functions {
        build_function_body(data, func);
    }

    let int_type = LLVMInt64TypeInContext(context);
    let function_type = LLVMFunctionType(int_type, ptr::null_mut(), 0, 0);
    let function = LLVMAddFunction(module, b"main\0".as_ptr() as *const _, function_type);

    let entry_name = label("entry");
    let bb = LLVMAppendBasicBlockInContext(context, function, entry_name.as_ptr());
    LLVMPositionBuilderAtEnd(builder, bb);

    let ret = compile_vec(data, function, None, scope, code);
    LLVMBuildRet(builder, ret);

    // Output to a file, and also a string for debugging
    //LLVMFinalizeFunctionPassManager(funcpass);
    let file_parts: Vec<&str> = module_name.rsplitn(2, '.').collect();
    LLVMPrintModuleToFile(module, label(format!("{}.ll", file_parts[1]).as_str()).as_ptr(), ptr::null_mut());
    let compiled = CString::from_raw(LLVMPrintModuleToString(module));

    println!("{}\n", compiled.into_string().unwrap());

    import::store_index(data.map.get_global(), format!("{}.idx", file_parts[1]).as_str(), code);

    LLVMDisposeBuilder(builder);
    LLVMDisposeModule(module);
    LLVMContextDispose(context);
}

unsafe fn declare_functions(data: &LLVM, scope: ScopeRef<Value, TypeValue>) {
    let pscope = scope.borrow().get_parent().unwrap().clone();
    let bytestr_type = LLVMPointerType(LLVMInt8Type(), 0);
    //let cint_type = LLVMInt32TypeInContext(data.context);
    let cint_type = int_type(data);

    declare_function(data.module, pscope.clone(), "malloc", &mut [cint_type], bytestr_type, false);
    declare_function(data.module, pscope.clone(), "realloc", &mut [bytestr_type, cint_type], bytestr_type, false);
    declare_function(data.module, pscope.clone(), "free", &mut [bytestr_type], LLVMVoidType(), false);

    declare_function(data.module, pscope.clone(), "strlen", &mut [bytestr_type], cint_type, false);
    declare_function(data.module, pscope.clone(), "memcpy", &mut [bytestr_type, bytestr_type, cint_type], bytestr_type, false);

    declare_function(data.module, pscope.clone(), "puts", &mut [bytestr_type], cint_type, false);
    declare_function(data.module, pscope.clone(), "sprintf", &mut [bytestr_type, bytestr_type], cint_type, true);

    // TODO this is probably wrong because we're only declaring extern the imported functions in the global scope, but not functions imported into a local scope, which still need to be declared globally as funcs
    for (name, sym) in &scope.borrow().names {
        if sym.external && !sym.ttype.as_ref().unwrap().is_overloaded() {
            LLVMAddFunction(data.module, label(name.as_str()).as_ptr(), get_type(data, scope.clone(), sym.ttype.clone().unwrap(), false));
        }
    }
}

unsafe fn declare_globals(data: &LLVM, scope: ScopeRef<Value, TypeValue>) {
    for (name, sym) in &scope.borrow().names {
        if LLVMGetNamedFunction(data.module, label(name.as_str()).as_ptr()).is_null() && !sym.ttype.as_ref().unwrap().is_overloaded() {
            let ltype = get_type(data, scope.clone(), sym.ttype.clone().unwrap(), true);
            LLVMAddGlobal(data.module, ltype, label(name.as_str()).as_ptr());
        }
    }
}

unsafe fn compile_vec(data: &LLVM, func: LLVMValueRef, unwind: Unwind, scope: ScopeRef<Value, TypeValue>, code: &Vec<AST>) -> LLVMValueRef {
    let mut last = zero_int(data);
    for node in code {
        last = compile_node(data, func, unwind, scope.clone(), node);
    }
    last
}

unsafe fn compile_node(data: &LLVM, func: LLVMValueRef, unwind: Unwind, scope: ScopeRef<Value, TypeValue>, node: &AST) -> LLVMValueRef {
    println!("COMPILE: {:?}", node);
    match *node {
        AST::Noop |
        AST::Nil => zero_int(data), // null_value() but what type should it be?let invoke 
        AST::Boolean(ref num) => LLVMConstInt(bool_type(data), *num as u64, 0),
        AST::Integer(ref num) => LLVMConstInt(int_type(data), *num as u64, 0),
        AST::Real(ref num) => LLVMConstReal(real_type(data), *num),
        AST::String(ref string) => LLVMBuildGlobalStringPtr(data.builder, label(string.as_str()).as_ptr(), label("strc").as_ptr()),

        AST::Block(ref body) => { compile_vec(data, func, unwind, scope, body) },

        AST::Invoke(ref fexpr, ref args, ref stype) => {
            let atypes = match stype.clone().unwrap() {
                Type::Function(atypes, _) => atypes,
                stype @ _ => panic!("TypeError: expected function type: {:?}", stype),
            };

            let mut largs = vec!();
            for (ttype, arg) in atypes.iter().zip(args.iter()) {
                let mut larg = compile_node(data, func, unwind, scope.clone(), arg);
                let ltype = get_type(data, scope.clone(), ttype.clone(), true);
                if ltype != LLVMTypeOf(larg) {
                    larg = LLVMBuildPointerCast(data.builder, larg, ltype, label("ptr").as_ptr());
                }
                largs.push(larg);
            }

            let name = match **fexpr {
                AST::Identifier(ref name) => name.clone(),
                _ => String::from("")
            };

            if let Some(result) = compile_builtin(data, func, scope.clone(), &name, &largs, stype.clone().unwrap()) {
                println!("BUILTIN: {:?}", result);
                result
            } else {
                let mut function = LLVMGetNamedFunction(data.module, label(name.as_str()).as_ptr());
                if function.is_null() {
                    //function = compile_node(data, func, unwind, scope.clone(), fexpr);
                    function = match **fexpr {
                        AST::Accessor(_, ref name, ref stype) => compile_node(data, func, unwind, scope.clone(), &AST::Resolver(Box::new(AST::Identifier(stype.clone().unwrap().get_name().unwrap())), name.clone())),
                        _ => compile_node(data, func, unwind, scope.clone(), fexpr)
                    };
                }
                LLVMDumpValue(function);
                //LLVMBuildCall(data.builder, function, largs.as_mut_ptr(), largs.len() as u32, label("tmp").as_ptr())
                match unwind {
                    None => LLVMBuildCall(data.builder, function, largs.as_mut_ptr(), largs.len() as u32, label("tmp").as_ptr()),
                    Some((then, catch)) => {
                        LLVMBuildInvoke(data.builder, function, largs.as_mut_ptr(), largs.len() as u32, then, catch, label("tmp").as_ptr())
                        //LLVMSetUnwindDest(invoke, unwind.unwrap());
                    }
                }
            }
        },

        AST::Function(ref name, ref args, ref rtype, ref body, ref id) => {
            let fname = scope.borrow().get_full_name(name, id.clone());
            LLVMGetNamedFunction(data.module, label(fname.as_str()).as_ptr())
        },

        AST::Identifier(ref name) => {
            let pointer = match scope.borrow().get_variable_value(name) {
                Some(x) => x,
                None => {
                    let pointer = LLVMGetNamedGlobal(data.module, label(name.as_str()).as_ptr());
                    if pointer.is_null() {
                        panic!("UnsetError: use before assignment {:?}", name);
                    }
                    pointer
                }
            };
            println!("IDENT: {:?} {:?}", LLVMGetValueKind(pointer), LLVMGetTypeKind(LLVMTypeOf(pointer)));
            //if LLVMGetTypeKind(LLVMTypeOf(pointer)) == llvm::LLVMTypeKind::LLVMPointerTypeKind {
            match LLVMGetValueKind(pointer) {
                llvm::LLVMValueKind::LLVMArgumentValueKind |
                llvm::LLVMValueKind::LLVMFunctionValueKind => pointer,
                _ => LLVMBuildLoad(data.builder, pointer, label("tmp").as_ptr())
            }
        },

        AST::Definition((ref name, ref ttype), ref body) => {
            let ltype = get_type(data, scope.clone(), ttype.clone().unwrap(), true);
            let pointer = if scope.borrow().is_global() {
                //LLVMAddGlobal(data.module, ltype, label(name.as_str()).unwrap().as_ptr())
                let global = LLVMGetNamedGlobal(data.module, label(name.as_str()).as_ptr());
                LLVMSetInitializer(global, null_value(ltype));
                global
            } else {
                LLVMBuildAlloca(data.builder, ltype, label(name.as_str()).as_ptr())
            };
            scope.borrow_mut().assign(name, pointer);
            let value = compile_node(data, func, unwind, scope, body);
            LLVMBuildStore(data.builder, value, pointer);
            value
        },

        /*
        // TODO you might need to create a new function to hold the body, so that it can be resumed from, and there also
        // might be an issue when you call a function one level down, and pass it the same unwind locations... it might effect all
        // functions, all the way down
        AST::Try(ref body, ref cases) => {
            // TODO need to add multiple cases, and also it doesn't work
            let try_block = LLVMAppendBasicBlockInContext(data.context, func, label("try").as_ptr());
            let catch_block = LLVMAppendBasicBlockInContext(data.context, func, label("catch").as_ptr());
            let finally_block = LLVMAppendBasicBlockInContext(data.context, func, label("finally").as_ptr());

            LLVMBuildBr(data.builder, try_block);
            LLVMPositionBuilderAtEnd(data.builder, try_block);
            let body_value = compile_node(data, func, Some((finally_block, catch_block)), scope.clone(), body);
            LLVMBuildBr(data.builder, finally_block);

            LLVMPositionBuilderAtEnd(data.builder, catch_block);
            let pad = LLVMBuildLandingPad(data.builder, LLVMTypeOf(body_value), ptr::null_mut(), 0, label("pad").as_ptr());
            LLVMBuildBr(data.builder, finally_block);

            LLVMPositionBuilderAtEnd(data.builder, finally_block);
            //let phi = LLVMBuildPhi(data.builder, LLVMTypeOf(body_value), label("matchphi").as_ptr());

            //LLVMAddIncoming(phi, values.as_mut_ptr(), do_blocks.as_mut_ptr(), values.len() as u32);
            //phi
            zero_int(data)
        },
        */

/*
        AST::Raise(ref expr) => {

        },
*/

        AST::SideEffect(ref op, ref args) => {
            // TODO This only handles two arguments, which is all that will be parsed, but you can do better... 
            let lexpr_block = LLVMAppendBasicBlockInContext(data.context, func, label(op).as_ptr());
            let rexpr_block = LLVMAppendBasicBlockInContext(data.context, func, label(op).as_ptr());
            let merge_block = LLVMAppendBasicBlockInContext(data.context, func, label(op).as_ptr());

            LLVMBuildBr(data.builder, lexpr_block);

            LLVMPositionBuilderAtEnd(data.builder, lexpr_block);
            let lexpr_value = compile_node(data, func, unwind, scope.clone(), &args[0]);
            let test_type = match op.as_str() {
                "or" => llvm::LLVMIntPredicate::LLVMIntNE,
                "and" => llvm::LLVMIntPredicate::LLVMIntEQ,
                _ => panic!("NotImplementedError: attempted to compile invalid side effect operation: {}", op)
            };
            let is_enough = LLVMBuildICmp(data.builder, test_type, lexpr_value, null_value(LLVMTypeOf(lexpr_value)), label("is_enough").as_ptr());
            LLVMBuildCondBr(data.builder, is_enough, merge_block, rexpr_block);

            LLVMPositionBuilderAtEnd(data.builder, rexpr_block);
            let rexpr_value = compile_node(data, func, unwind, scope.clone(), &args[1]);
            LLVMBuildBr(data.builder, merge_block);

            LLVMPositionBuilderAtEnd(data.builder, merge_block);
            let phi = LLVMBuildPhi(data.builder, LLVMTypeOf(lexpr_value), label(op).as_ptr());

            let mut values = vec![lexpr_value, rexpr_value];
            let mut blocks = vec![lexpr_block, rexpr_block];

            LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);
            phi
        },

        AST::If(ref cond, ref texpr, ref fexpr) => {
            let cond_value = compile_node(data, func, unwind, scope.clone(), cond);
            let cond_zero = LLVMConstInt(LLVMTypeOf(cond_value), 0, 0);
            let is_nonzero = LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntNE, cond_value, cond_zero, label("is_nonzero").as_ptr());

            let entry_name = label("if");
            let texpr_block = LLVMAppendBasicBlockInContext(data.context, func, entry_name.as_ptr());
            let fexpr_block = LLVMAppendBasicBlockInContext(data.context, func, entry_name.as_ptr());
            let merge_block = LLVMAppendBasicBlockInContext(data.context, func, entry_name.as_ptr());

            LLVMBuildCondBr(data.builder, is_nonzero, texpr_block, fexpr_block);


            LLVMPositionBuilderAtEnd(data.builder, texpr_block);
            let texpr_value = compile_node(data, func, unwind, scope.clone(), texpr);
            LLVMBuildBr(data.builder, merge_block);
            //let texpr_block = LLVMGetInsertBlock(data.builder);

            LLVMPositionBuilderAtEnd(data.builder, fexpr_block);
            let fexpr_value = compile_node(data, func, unwind, scope.clone(), fexpr);
            LLVMBuildBr(data.builder, merge_block);
            //let fexpr_block = LLVMGetInsertBlock(data.builder);

            LLVMPositionBuilderAtEnd(data.builder, merge_block);
            let phi = LLVMBuildPhi(data.builder, LLVMTypeOf(texpr_value), label("iftmp").as_ptr());

            let mut values = vec![texpr_value, fexpr_value];
            let mut blocks = vec![texpr_block, fexpr_block];

            LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);
            phi
        },

        AST::Match(ref cond, ref cases) => {
            let mut cond_blocks = vec!();
            let mut do_blocks = vec!();
            for _ in 0 .. cases.len() {
                cond_blocks.push(LLVMAppendBasicBlockInContext(data.context, func, label("matchcond").as_ptr()));
                do_blocks.push(LLVMAppendBasicBlockInContext(data.context, func, label("matchdo").as_ptr()));
            }
            let merge_block = LLVMAppendBasicBlockInContext(data.context, func, label("matchend").as_ptr());
            cond_blocks.push(merge_block);

            let cond_value = compile_node(data, func, unwind, scope.clone(), cond);
            LLVMBuildBr(data.builder, cond_blocks[0]);

            let mut values = vec!();
            for (i, &(ref case, ref expr)) in cases.iter().enumerate() {
                LLVMPositionBuilderAtEnd(data.builder, cond_blocks[i]);
                match *case {
                    AST::Underscore => { LLVMBuildBr(data.builder, do_blocks[i]); },
                    _ => {
                        let case_value = compile_node(data, func, unwind, scope.clone(), case);
                        let is_true = LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntEQ, cond_value, case_value, label("is_true").as_ptr());
                        LLVMBuildCondBr(data.builder, is_true, do_blocks[i], cond_blocks[i + 1]);
                    }
                }

                LLVMPositionBuilderAtEnd(data.builder, do_blocks[i]);
                values.push(compile_node(data, func, unwind, scope.clone(), expr));
                LLVMBuildBr(data.builder, merge_block);
            }

            LLVMPositionBuilderAtEnd(data.builder, merge_block);
            let phi = LLVMBuildPhi(data.builder, LLVMTypeOf(values[0]), label("matchphi").as_ptr());

            LLVMAddIncoming(phi, values.as_mut_ptr(), do_blocks.as_mut_ptr(), values.len() as u32);
            phi
        },

        AST::While(ref cond, ref body) => {
            let before_block = LLVMAppendBasicBlockInContext(data.context, func, label("while").as_ptr());
            let body_block = LLVMAppendBasicBlockInContext(data.context, func, label("while").as_ptr());
            let after_block = LLVMAppendBasicBlockInContext(data.context, func, label("whileend").as_ptr());

            LLVMBuildBr(data.builder, before_block);
            LLVMPositionBuilderAtEnd(data.builder, before_block);
            let cond_value = compile_node(data, func, unwind, scope.clone(), cond);
            let cond_zero = LLVMConstInt(LLVMTypeOf(cond_value), 0, 0);
            let is_nonzero = LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntNE, cond_value, cond_zero, label("is_nonzero").as_ptr());
            LLVMBuildCondBr(data.builder, is_nonzero, body_block, after_block);

            LLVMPositionBuilderAtEnd(data.builder, body_block);
            let body_value = compile_node(data, func, unwind, scope.clone(), body);
            LLVMBuildBr(data.builder, before_block);

            LLVMPositionBuilderAtEnd(data.builder, after_block);
            //let phi = LLVMBuildPhi(data.builder, LLVMInt64TypeInContext(data.context), label("whileend").as_ptr());

            //let mut values = vec![texpr_value, fexpr_value];
            //let mut blocks = vec![texpr_block, fexpr_block];

            //LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);
            //phi
            body_value
        },

/*
        AST::For(ref name, ref cond, ref body, ref id) => {
            let lscope = map.get(id);

        },

        AST::List(ref items) => {

        },

        AST::Index(ref base, ref index) => {

        },
*/

        AST::Class(ref name, ref parent, ref body, ref id) => {
            let tscope = data.map.get(id);
            let classdef = scope.borrow().get_class_def(name).unwrap();

            // TODO you still need to compile the body of the func, if you're going to allow that... like an init function
            //compile_vec(data, func, unwind, tscope.clone(), body);

            zero_int(data)
        },

        AST::Resolver(ref left, ref right) => {
            match **left {
                AST::Identifier(ref name) => {
                    let classdef = scope.borrow().get_class_def(name).unwrap();
                    let value = classdef.borrow().get_variable_value(right);
                    value.unwrap()
                },
                _ => panic!("SyntaxError: left-hand side of scope resolver must be identifier")
            }
        },

        AST::Accessor(ref left, ref right, ref ltype) => {
            let object = compile_node(data, func, unwind, scope.clone(), left);

            let name = ltype.clone().unwrap().get_name().unwrap();
            let classdef = scope.borrow().get_class_def(&name).unwrap();
            let sym = classdef.borrow().find(right).unwrap();
            println!("*ACCESS: {:?} {:?}", right, classdef);

            if let Type::Function(_, _) = sym.ttype.unwrap() {
                sym.value.unwrap()
            } else {
                let cint_type = i32_type(data);

                //let structdef = scope.borrow().get_struct_def(&name).unwrap();
                let structdef = scope.borrow().get_type_value(&name).unwrap().structdef;
                let index = structdef.iter().position(|ref r| r.0 == *right).unwrap();
                let mut indices = vec!(LLVMConstInt(cint_type, 0, 0), LLVMConstInt(cint_type, index as u64, 0));
                let pointer = LLVMBuildGEP(data.builder, object, indices.as_mut_ptr(), indices.len() as u32, label("tmp").as_ptr());
                LLVMBuildLoad(data.builder, pointer, label("tmp").as_ptr())
            }
        },

        AST::Assignment(ref left, ref right) => {
            let value = compile_node(data, func, unwind, scope.clone(), right);
            match **left {
                AST::Accessor(ref left, ref right, ref ltype) => {
                    let name = ltype.clone().unwrap().get_name().unwrap();
                    let object = compile_node(data, func, unwind, scope.clone(), left);
                    let cint_type = i32_type(data);

                    let structdef = scope.borrow().get_type_value(&name).unwrap().structdef;
                    let index = structdef.iter().position(|ref r| r.0 == *right).unwrap();
                    let mut indices = vec!(LLVMConstInt(cint_type, 0, 0), LLVMConstInt(cint_type, index as u64, 0));
                    let pointer = LLVMBuildGEP(data.builder, object, indices.as_mut_ptr(), indices.len() as u32, label("tmp").as_ptr());
                    LLVMBuildStore(data.builder, value, pointer)
                },
                _ => panic!("???"),
            };
            value
        },

        AST::Import(_) => { ptr::null_mut() },  //// Nothing to do

        AST::Type(_, _) => panic!("NotImplementedError: not yet supported, {:?}", node),

        _ => {
            let int_type = LLVMInt64TypeInContext(data.context);
            let num: isize = -12;
            LLVMConstInt(int_type, num as u64, 0)
        },
    }
}

//unsafe fn compile_assignable(data: &LLVM, scope: ScopeRef<Value, TypeValue>, items: &AST) -> LLVMValueRef {

//}

unsafe fn build_function_body(data: &LLVM, node: &AST) {
    if let AST::Function(ref name, ref args, ref rtype, ref body, ref id) = *node {
        let fscope = data.map.get(id);
        //let fname = if name.is_some() { name.clone().unwrap() } else { format!("anon{}", id) };
        let scope = fscope.borrow().parent.clone().unwrap();
        let fname = scope.borrow().get_full_name(name, id.clone());
        let function = LLVMGetNamedFunction(data.module, label(fname.as_str()).as_ptr());

        let bb = LLVMAppendBasicBlockInContext(data.context, function, label("entry").as_ptr());
        LLVMPositionBuilderAtEnd(data.builder, bb);
        let ret = compile_node(data, function, None, fscope.clone(), body);
        LLVMBuildRet(data.builder, ret);

        //if llvm::analysis::LLVMVerifyFunction(function, llvm::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction) != 0 {
        //    panic!("VerifyError: verification failed");
        //}
        //LLVMRunFunctionPassManager(data.funcpass, function);
    }
}

unsafe fn collect_functions_vec<'a>(data: &mut LLVM<'a>, scope: ScopeRef<Value, TypeValue>, items: &'a Vec<AST>) -> Option<LLVMValueRef> {
    let mut last = None;
    for item in items {
        last = collect_functions_node(data, scope.clone(), item);
    }
    last
}

unsafe fn collect_functions_node<'a>(data: &mut LLVM<'a>, scope: ScopeRef<Value, TypeValue>, node: &'a AST) -> Option<LLVMValueRef> {
    match *node {

        AST::Function(ref name, ref args, ref rtype, ref body, ref id) => {
            let fscope = data.map.get(id);
            let ftype = get_type(data, scope.clone(), Type::Function(args.iter().map(|t| t.1.clone().unwrap()).collect(), Box::new(rtype.clone().unwrap())), false);
            let fname = scope.borrow().get_full_name(name, id.clone());
            let function = LLVMAddFunction(data.module, label(fname.as_str()).as_ptr(), ftype);
            //LLVMSetGC(function, label("shadow-stack").as_ptr());

            let dscope = Scope::target(scope.clone());
            match *name {
                Some(ref name) => dscope.borrow_mut().assign(name, function),
                _ => { },
            }

            let nargs = LLVMCountParams(function) as usize;
            if nargs != 0 && nargs != args.len() {
                panic!("ArgsError: argument counts don't match");
            }

            for (i, &(ref name, _, _)) in args.iter().enumerate() {
                let llarg = LLVMGetParam(function, i as u32);
                LLVMSetValueName(llarg, label(name.as_str()).into_raw());
                fscope.borrow_mut().assign(name, llarg);
            }

            collect_functions_node(data, fscope, body);
            data.functions.push(node);
            return Some(function);
        },

        AST::List(ref items) => { collect_functions_vec(data, scope, items); },

        AST::Invoke(ref name, ref args, _) => { collect_functions_vec(data, scope, args); },

        AST::SideEffect(_, ref args) => { collect_functions_vec(data, scope, args); },

        AST::Definition((ref name, ref ttype), ref body) => {
            //collect_functions_node(data, scope, body);
            if let Some(function) = collect_functions_node(data, scope.clone(), body) {
                let dscope = Scope::target(scope.clone());
                dscope.borrow_mut().assign(name, function);
            }
        },

        AST::Block(ref body) => { collect_functions_vec(data, scope, body); },

        AST::If(ref cond, ref texpr, ref fexpr) => {
            collect_functions_node(data, scope.clone(), cond);
            collect_functions_node(data, scope.clone(), texpr);
            collect_functions_node(data, scope, fexpr);
        },

        AST::Raise(ref expr) => { collect_functions_node(data, scope, expr); },

        AST::Try(ref cond, ref cases) |
        AST::Match(ref cond, ref cases) => {
            collect_functions_node(data, scope.clone(), cond);
            for case in cases {
                collect_functions_node(data, scope.clone(), &case.0);
                collect_functions_node(data, scope.clone(), &case.1);
            }
        },

        AST::For(ref name, ref cond, ref body, ref id) => {
            let lscope = data.map.get(id);
            collect_functions_node(data, lscope.clone(), cond);
            collect_functions_node(data, lscope.clone(), body);
        },

        AST::While(ref cond, ref body) => {
            collect_functions_node(data, scope.clone(), cond);
            collect_functions_node(data, scope.clone(), body);
        },

        AST::Class(ref name, ref parent, ref body, ref id) => {
            let tscope = data.map.get(id);

            let mut structdef = if let Some(ref name) = *parent {
                scope.borrow().get_type_value(name).unwrap().structdef
            } else {
                vec!()
            };
            for ref node in body.iter() {
                match **node {
                    AST::Definition((ref name, ref ttype), ref body) => {
                        match **body {
                            AST::Function(_, _, _, _, _) => { },
                            _ => structdef.push((name.clone(), ttype.clone().unwrap())),
                        }
                    },
                    _ => { }
                }
            }

            let mut types = vec!();
            for &(_, ref ttype) in &structdef {
                types.push(get_type(data, tscope.clone(), ttype.clone(), true))
            }
            let lltype = LLVMStructCreateNamed(data.context, label(name).as_ptr());
            LLVMStructSetBody(lltype, types.as_mut_ptr(), types.len() as u32, false as i32);
            scope.borrow_mut().set_type_value(name, TypeValue { structdef: structdef, value: lltype });


            // TODO you need to add the 'new' function definition here, but the value should be stored in cscope, not tscope...
            let classdef = scope.borrow().get_class_def(name).unwrap();
            classdef.borrow_mut().assign(&String::from("new"), build_constructor(data, format!("{}_new", name).as_str(), lltype));
            /*
            for node in body {
                match *node {
                    AST::Definition((ref name, ref ttype), ref expr) => {
                        if let Some(function) = collect_functions_node(data, tscope.clone(), expr) {
                            classdef.borrow_mut().assign(name, function);
                        }
                    },
                    AST::Function(ref name, _, _, _, _) => {
                        if let Some(ref name) = *name {
                            if let Some(function) = collect_functions_node(data, tscope.clone(), node) {
                                classdef.borrow_mut().assign(name, function);
                            }
                        }
                    },
                    _ => { collect_functions_node(data, tscope.clone(), node); },
                }
            }
            */

            collect_functions_vec(data, tscope.clone(), body);
            //for (name, sym) in &tscope.borrow().names {
            //    if let Some(value) = sym.value {
            //        classdef.borrow_mut().assign(name, value);
            //    }
            //}
        },

        AST::Index(ref left, ref right) => {
            collect_functions_node(data, scope.clone(), left);
            collect_functions_node(data, scope.clone(), right);
        },

        AST::Resolver(ref left, ref right) => {
            collect_functions_node(data, scope.clone(), left);
        },

        AST::Accessor(ref left, ref right, _) => {
            collect_functions_node(data, scope.clone(), left);
        },

        AST::Assignment(ref left, ref right) => {
            collect_functions_node(data, scope.clone(), right);
        },

        AST::Import(_) => {
            // TODO what do you do here?  you need to declare imported functions, but that requires the index, which has already been loaded and discarded by this point
        },

        AST::Type(_, _) => panic!("NotImplementedError: not yet supported, {:?}", node),

        AST::Identifier(_) |
        AST::Noop | AST::Underscore | AST::Nil |
        AST::Boolean(_) | AST::Integer(_) | AST::Real(_) | AST::String(_) => { }
    };
    None
}



fn label(string: &str) -> CString {
    CString::new(string).unwrap()
}

unsafe fn bool_type(data: &LLVM) -> LLVMTypeRef {
    LLVMInt1TypeInContext(data.context)
}

unsafe fn i32_type(data: &LLVM) -> LLVMTypeRef {
    LLVMInt32TypeInContext(data.context)
}

unsafe fn int_type(data: &LLVM) -> LLVMTypeRef {
    LLVMInt64TypeInContext(data.context)
}

unsafe fn real_type(data: &LLVM) -> LLVMTypeRef {
    LLVMDoubleTypeInContext(data.context)
}

unsafe fn str_type(data: &LLVM) -> LLVMTypeRef {
    LLVMPointerType(LLVMInt8Type(), 0)
}


unsafe fn null_value(ttype: LLVMTypeRef) -> LLVMValueRef {
    LLVMConstNull(ttype)
}

unsafe fn zero_int(data: &LLVM) -> LLVMValueRef {
    LLVMConstInt(int_type(data), 0, 0)
}

unsafe fn int_value(data: &LLVM, num: usize) -> LLVMValueRef {
    LLVMConstInt(int_type(data), num as u64, 0)
}

unsafe fn build_str_const(data: &LLVM, string: &str) -> LLVMValueRef {
    LLVMBuildGlobalStringPtr(data.builder, label(string).as_ptr(), label("str").as_ptr())
}

unsafe fn build_malloc(data: &LLVM, size: LLVMValueRef) -> LLVMValueRef {
    build_call(data, "malloc", &mut vec!(size))
}

unsafe fn build_malloc_const(data: &LLVM, size: usize) -> LLVMValueRef {
    build_call(data, "malloc", &mut vec!(LLVMConstInt(int_type(data), size as u64, 0)))
}

unsafe fn build_realloc(data: &LLVM, ptr: LLVMValueRef, size: LLVMValueRef) -> LLVMValueRef {
    build_call(data, "realloc", &mut vec!(ptr, size))
}


unsafe fn declare_function(module: LLVMModuleRef, scope: ScopeRef<Value, TypeValue>, name: &str, args: &mut [LLVMTypeRef], ret_type: LLVMTypeRef, vargs: bool) {
    let ftype = LLVMFunctionType(ret_type, args.as_mut_ptr(), args.len() as u32, vargs as i32);
    let func = LLVMAddFunction(module, label(name).as_ptr(), ftype);
    let name = &String::from(name);
    if scope.borrow().find(name).is_some() {
        scope.borrow_mut().assign(name, func);
    }
}

unsafe fn build_function_start(data: &LLVM, name: &str, args: Vec<(String, LLVMTypeRef)>, return_type: LLVMTypeRef) -> LLVMValueRef {
    let mut atypes: Vec<LLVMTypeRef> = args.iter().map(|t| t.1.clone()).collect();
    let ftype = LLVMFunctionType(return_type, atypes.as_mut_ptr(), atypes.len() as u32, false as i32);
    let function = LLVMAddFunction(data.module, label(name).as_ptr(), ftype);

    let nargs = LLVMCountParams(function) as usize;
    if nargs != 0 && nargs != args.len() {
        panic!("ArgsError: argument counts don't match");
    }

    for (i, &(ref name, _)) in args.iter().enumerate() {
        let llarg = LLVMGetParam(function, i as u32);
        LLVMSetValueName(llarg, label(name.as_str()).into_raw());
        //scope.borrow_mut().assign(name, llarg);
    }

    let bb = LLVMAppendBasicBlockInContext(data.context, function, label("entry").as_ptr());
    LLVMPositionBuilderAtEnd(data.builder, bb);

    function
}

unsafe fn build_call(data: &LLVM, name: &str, largs: &mut Vec<LLVMValueRef>) -> LLVMValueRef {
    let function = LLVMGetNamedFunction(data.module, label(name).as_ptr());
    LLVMBuildCall(data.builder, function, largs.as_mut_ptr(), largs.len() as u32, label("tmp").as_ptr())
}

unsafe fn build_constructor(data: &LLVM, name: &str, rtype: LLVMTypeRef) -> LLVMValueRef {
    let ptr_type = LLVMPointerType(rtype, 0);
    let function = build_function_start(data, name, vec!(), ptr_type);
    
    let raw_ptr = build_malloc(data, LLVMSizeOf(rtype));
    let ptr = LLVMBuildPointerCast(data.builder, raw_ptr, ptr_type, label("ptr").as_ptr());
    LLVMBuildRet(data.builder, ptr);
    function
}


unsafe fn get_type(data: &LLVM, scope: ScopeRef<Value, TypeValue>, ttype: Type, use_fptrs: bool) -> LLVMTypeRef {
    match ttype {
        Type::Object(ref tname) => match tname.as_str() {
            "Bool" => LLVMInt1TypeInContext(data.context),
            "Int" => LLVMInt64TypeInContext(data.context),
            "Real" => LLVMDoubleTypeInContext(data.context),
            "String" => LLVMPointerType(LLVMInt8Type(), 0),

            _ => match scope.borrow().get_type_value(tname) {
                Some(typedata) => LLVMPointerType(typedata.value, 0),
                // TODO this should panic...
                None => LLVMInt64TypeInContext(data.context),
            }
        },
        Type::Function(ref args, ref ret) => {
            let mut atypes = vec!();
            for ttype in args {
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
        Type::Variable(ref tname) => LLVMInt64TypeInContext(data.context),
        _ => panic!("InvalidType: cannot convert to llvm, {:?}", ttype),
    }
}


unsafe fn compile_builtin(data: &LLVM, func: LLVMValueRef, scope: ScopeRef<Value, TypeValue>, name: &String, largs: &Vec<LLVMValueRef>, stype: Type) -> Option<LLVMValueRef> {
    if largs.len() <= 0 {
        return None
    }

    println!("**BUILTINS: {:?}", stype);
    match stype {
        Type::Function(ref sargs, _) => {
            match sargs[0] {
                Type::Object(ref tname) => {
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

unsafe fn initialize_builtins(data: &mut LLVM) {
    initialize_type_int(data);
    initialize_type_real(data);
    initialize_type_string(data);
}

unsafe fn initialize_type_int(data: &mut LLVM) {
    let mut builtins: FunctionMap = HashMap::new();

    build_lib_add(data);
    let global = data.map.get_global();
    let intdef = global.borrow().get_class_def(&String::from("Int")).unwrap();
    intdef.borrow_mut().assign(&String::from("add"), LLVMGetNamedFunction(data.module, label("builtin.add").as_ptr()));

    let ftype = Type::Function(vec!(Type::Object(String::from("Int")), Type::Object(String::from("Int"))), Box::new(Type::Object(String::from("Int"))));
    global.borrow_mut().define_assign(mangle_name(&String::from("+"), &vec!(Type::Object(String::from("Int")), Type::Object(String::from("Int")))), Some(ftype), LLVMGetNamedFunction(data.module, label("builtin.add").as_ptr()));


    fn add(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildAdd(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    fn sub(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildSub(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    fn mul(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildMul(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    fn div(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildSDiv(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    fn eq(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntEQ, args[0], args[1], label("tmp").as_ptr()) } }
    fn ne(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntNE, args[0], args[1], label("tmp").as_ptr()) } }
    fn lt(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntSLT, args[0], args[1], label("tmp").as_ptr()) } }
    fn gt(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntSGT, args[0], args[1], label("tmp").as_ptr()) } }
    fn lte(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntSLE, args[0], args[1], label("tmp").as_ptr()) } }
    fn gte(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntSGE, args[0], args[1], label("tmp").as_ptr()) } }
    fn not(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildNot(data.builder, args[0], label("tmp").as_ptr()) } }

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

    fn add(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFAdd(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    fn sub(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFSub(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    fn mul(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFMul(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    fn div(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFDiv(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    fn eq(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOEQ, args[0], args[1], label("tmp").as_ptr()) } }
    fn ne(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealONE, args[0], args[1], label("tmp").as_ptr()) } }
    fn lt(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOLT, args[0], args[1], label("tmp").as_ptr()) } }
    fn gt(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOGT, args[0], args[1], label("tmp").as_ptr()) } }
    fn lte(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOLE, args[0], args[1], label("tmp").as_ptr()) } }
    fn gte(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOGE, args[0], args[1], label("tmp").as_ptr()) } }
    fn not(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildNot(data.builder, args[0], label("tmp").as_ptr()) } }

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
    build_lib_strcat(data);

    fn add(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFAdd(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    //fn sub(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFSub(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    //fn mul(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFMul(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    //fn div(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFDiv(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    fn eq(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOEQ, args[0], args[1], label("tmp").as_ptr()) } }
    fn ne(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealONE, args[0], args[1], label("tmp").as_ptr()) } }
    //fn lt(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOLT, args[0], args[1], label("tmp").as_ptr()) } }
    //fn gt(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOGT, args[0], args[1], label("tmp").as_ptr()) } }
    //fn lte(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOLE, args[0], args[1], label("tmp").as_ptr()) } }
    //fn gte(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOGE, args[0], args[1], label("tmp").as_ptr()) } }
    //fn not(data: &LLVM, args: Vec<LLVMValueRef>) -> LLVMValueRef { unsafe { LLVMBuildNot(data.builder, args[0], label("tmp").as_ptr()) } }

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

unsafe fn build_lib_str(data: &LLVM) {
    let args = vec!((String::from("num"), int_type(data)));
    let function = build_function_start(data, "str", args, str_type(data));
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);

    let pointer = LLVMBuildAlloca(data.builder, str_type(data), label("buffer").as_ptr());
    LLVMBuildStore(data.builder, build_malloc_const(data, 22), pointer);

    let value = LLVMBuildLoad(data.builder, pointer, label("tmp").as_ptr());
    let num = LLVMGetParam(function, 0);
    build_call(data, "sprintf", &mut vec!(value, build_str_const(data, "%d"), num));

    LLVMBuildRet(data.builder, value);
}

unsafe fn build_lib_strcat(data: &LLVM) {
    let args = vec!((String::from("str1"), str_type(data)), (String::from("str2"), str_type(data)));
    let function = build_function_start(data, "strcat", args, str_type(data));
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);

    let str1 = LLVMGetParam(function, 0);
    let str2 = LLVMGetParam(function, 1);
    let str1len = build_call(data, "strlen", &mut vec!(str1));
    let str2len = build_call(data, "strlen", &mut vec!(str2));
    let sum = LLVMBuildAdd(data.builder, str1len, str2len, label("tmp").as_ptr());

    let buffer = build_malloc(data, sum);

    build_call(data, "memcpy", &mut vec!(buffer, str1, str1len));
    //build_call(data, "memcpy", &mut vec!(buffer, str2, str1len));

    LLVMBuildRet(data.builder, buffer);
}

unsafe fn build_lib_add(data: &LLVM) {
    let dtype = int_type(data);
    let mut atypes = vec!(dtype, dtype);
    let function = LLVMAddFunction(data.module, label("builtin.add").as_ptr(), LLVMFunctionType(dtype, atypes.as_mut_ptr(), atypes.len() as u32, false as i32));
    LLVMSetLinkage(function, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);
    //LLVMAddAttributeAtIndex(function, 0, "inlinealways"
    LLVMPositionBuilderAtEnd(data.builder, LLVMAppendBasicBlockInContext(data.context, function, label("entry").as_ptr()));
    let value = LLVMBuildAdd(data.builder, LLVMGetParam(function, 0), LLVMGetParam(function, 1), label("tmp").as_ptr());
    LLVMBuildRet(data.builder, value);
}

//    let mut types = vec!(cint_type, bool_type(data));
//    let rtype = LLVMStructTypeInContext(data.context, types.as_mut_ptr(), types.len() as u32, false as i32);

//    declare_function(data.module, pscope.clone(), "llvm.sadd.with.overflow.i64", &mut [cint_type, cint_type], rtype, false);

