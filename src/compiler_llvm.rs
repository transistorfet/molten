
extern crate llvm_sys as llvm;
use self::llvm::prelude::*;

use std::ptr;
use std::ffi::CString;

use parser::AST;
use scope::{ ScopeRef, ScopeMapRef };
use types::Type;

pub type Value = LLVMValueRef;

pub fn compile(map: ScopeMapRef<Value>, code: &Vec<AST>) -> String {
    unsafe {
        compile_module(map.clone(), map.get_global(), code)
    }
}


pub struct LLVM {
    map: ScopeMapRef<Value>,
    builtins: TypeFunctionMap,
    types: HashMap<String, LLVMTypeRef>,
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    funcpass: LLVMPassManagerRef,
}


unsafe fn compile_module(map: ScopeMapRef<Value>, scope: ScopeRef<Value>, code: &Vec<AST>) -> String {
    let context = llvm::core::LLVMContextCreate();
    let module = llvm::core::LLVMModuleCreateWithName(b"main_module\0".as_ptr() as *const _);
    let builder = llvm::core::LLVMCreateBuilderInContext(context);
    let funcpass = llvm::core::LLVMCreateFunctionPassManagerForModule(module);
    llvm::core::LLVMInitializeFunctionPassManager(funcpass);
    let data = &mut LLVM { map: map, builtins: HashMap::new(), types: HashMap::new(), context: context, module: module, builder: builder, funcpass: funcpass };

    declare_functions(data, scope.clone());
    declare_globals(data, scope.clone());
    initialize_builtins(data);
    compile_functions_vec(data, scope.clone(), code);

    let int_type = llvm::core::LLVMInt64TypeInContext(context);
    let function_type = llvm::core::LLVMFunctionType(int_type, ptr::null_mut(), 0, 0);
    let function = llvm::core::LLVMAddFunction(module, b"main\0".as_ptr() as *const _, function_type);

    let entry_name = label("entry");
    let bb = llvm::core::LLVMAppendBasicBlockInContext(context, function, entry_name.as_ptr());
    llvm::core::LLVMPositionBuilderAtEnd(builder, bb);

    //let mut names = HashMap::new();
    //insert_allocations(context, builder, &mut names, &input);

    let ret = compile_vec(data, function, scope, code);
    llvm::core::LLVMBuildRet(builder, ret);

    // Output to a file, and also a string for debugging
    llvm::core::LLVMFinalizeFunctionPassManager(funcpass);
    let out_file = CString::new("out.ll").unwrap();
    llvm::core::LLVMPrintModuleToFile(module, out_file.as_ptr(), ptr::null_mut());
    let compiled = CString::from_raw(llvm::core::LLVMPrintModuleToString(module));

    llvm::core::LLVMDisposeBuilder(builder);
    llvm::core::LLVMDisposeModule(module);
    llvm::core::LLVMContextDispose(context);

    compiled.into_string().unwrap()
}

unsafe fn declare_functions(data: &LLVM, scope: ScopeRef<Value>) {
    let pscope = scope.borrow().get_parent().unwrap().clone();
    let bytestr_type = llvm::core::LLVMPointerType(llvm::core::LLVMInt8Type(), 0);
    let cint_type = llvm::core::LLVMInt32TypeInContext(data.context);

    declare_function(data.module, pscope.clone(), "malloc", &mut [cint_type], bytestr_type, false);
    declare_function(data.module, pscope.clone(), "realloc", &mut [bytestr_type, cint_type], bytestr_type, false);
    declare_function(data.module, pscope.clone(), "free", &mut [bytestr_type], llvm::core::LLVMVoidType(), false);

    declare_function(data.module, pscope.clone(), "strlen", &mut [bytestr_type], cint_type, false);
    declare_function(data.module, pscope.clone(), "memcpy", &mut [bytestr_type, bytestr_type, cint_type], bytestr_type, false);

    declare_function(data.module, pscope.clone(), "puts", &mut [bytestr_type], cint_type, false);
    declare_function(data.module, pscope.clone(), "sprintf", &mut [bytestr_type, bytestr_type], cint_type, true);
}

unsafe fn declare_globals(data: &LLVM, scope: ScopeRef<Value>) {
    for (name, sym) in &scope.borrow().names {
        let ltype = get_type(data, scope.clone(), sym.ttype.clone(), true);
        llvm::core::LLVMAddGlobal(data.module, ltype, label(name.as_str()).as_ptr());
    }
}

unsafe fn compile_vec(data: &LLVM, func: LLVMValueRef, scope: ScopeRef<Value>, code: &Vec<AST>) -> LLVMValueRef {
    let int_type = llvm::core::LLVMInt64TypeInContext(data.context);
    let zero = llvm::core::LLVMConstInt(int_type, 0, 0);

    let mut last = zero;
    for node in code {
        last = compile_node(data, func, scope.clone(), node);
    }
    last
}

unsafe fn compile_node(data: &LLVM, func: LLVMValueRef, scope: ScopeRef<Value>, node: &AST) -> LLVMValueRef {
    println!("COMPILE: {:?}", node);
    match *node {
        //AST::Nil => String::from("NULL"),
        AST::Boolean(ref num) => {
            let ltype = get_type(data, scope.clone(), Type::Concrete(String::from("Bool")), true);
            llvm::core::LLVMConstInt(ltype, *num as u64, 0)
        },
        AST::Integer(ref num) => {
            let ltype = get_type(data, scope.clone(), Type::Concrete(String::from("Int")), true);
            llvm::core::LLVMConstInt(ltype, *num as u64, 0)
        },
        AST::Real(ref num) => {
            let ltype = get_type(data, scope.clone(), Type::Concrete(String::from("Real")), true);
            llvm::core::LLVMConstReal(ltype, *num)
        },
        AST::String(ref string) => {
            //let ltype = get_type(data, scope.clone(), Type::Concrete(String::from("String")), true);
            //llvm::core::LLVMConstString(cstring, string.len() as u32, false as i32)
            llvm::core::LLVMBuildGlobalStringPtr(data.builder, label(string.as_str()).as_ptr(), label("strc").as_ptr())
        },

        //AST::List(ref items) => {

        //},

        AST::Invoke(ref fexpr, ref args, ref stype) => {
            let name = match **fexpr {
                AST::Identifier(ref name) => name.clone(),
                _ => String::from("something-invalid")
            };

            if let Some(result) = compile_builtin(data, func, scope.clone(), &name, args, stype.clone().unwrap()) {
                println!("BUILTIN: {:?}", result);
                result
            }
            else {
                let mut largs = vec!();
                for arg in args {
                    largs.push(compile_node(data, func, scope.clone(), arg));
                }

                //let mut function = llvm::core::LLVMGetNamedFunction(data.module, label(name.as_str()).as_ptr());
                //let mut function = scope.borrow().find(name).unwrap().address.clone().expect(format!("UnsetError: use before assignment {:?}", name).as_str());
                /*
                let mut function = llvm::core::LLVMGetNamedGlobal(data.module, label(name.as_str()).as_ptr());
                if function.is_null() {
                    function = scope.borrow().find(name).unwrap().value.clone().expect(format!("UnsetError: use before assignment {:?}", name).as_str());
                }
                if llvm::core::LLVMGetTypeKind(llvm::core::LLVMTypeOf(function)) == llvm::LLVMTypeKind::LLVMPointerTypeKind {
                    function = llvm::core::LLVMBuildLoad(data.builder, function, label("tmp").as_ptr())
                }
                */
                let mut function = llvm::core::LLVMGetNamedFunction(data.module, label(name.as_str()).as_ptr());
                if function.is_null() {
                    function = compile_node(data, func, scope.clone(), fexpr);
                }
                llvm::core::LLVMDumpValue(function);
                llvm::core::LLVMBuildCall(data.builder, function, largs.as_mut_ptr(), largs.len() as u32, label("tmp").as_ptr())
            }
        },

        AST::Function(ref args, ref body, ref id, ref ttype) => {
            llvm::core::LLVMGetNamedFunction(data.module, label(format!("anon{}", id).as_str()).as_ptr())
            //llvm::core::LLVMBuildPointerCast(data.builder, function, llvm::core::LLVMPointerType(llvm::core::LLVMTypeOf(function), 0), label("fptr").as_ptr())
        },

        AST::Identifier(ref name) => {
            let pointer = match scope.borrow().find(name).unwrap().value.clone() {
                Some(x) => x,
                None => {
                    let pointer = llvm::core::LLVMGetNamedGlobal(data.module, label(name.as_str()).as_ptr());
                    if pointer.is_null() {
                        panic!("UnsetError: use before assignment {:?}", name);
                    }
                    pointer
                }
            };
            println!("IDENT: {:?} {:?}", llvm::core::LLVMGetValueKind(pointer), llvm::core::LLVMGetTypeKind(llvm::core::LLVMTypeOf(pointer)));
            //if llvm::core::LLVMGetTypeKind(llvm::core::LLVMTypeOf(pointer)) == llvm::LLVMTypeKind::LLVMPointerTypeKind {
            if llvm::core::LLVMGetValueKind(pointer) == llvm::LLVMValueKind::LLVMArgumentValueKind {
                pointer
            }
            else {
                llvm::core::LLVMBuildLoad(data.builder, pointer, label(name.as_str()).as_ptr())
            }
        },

        AST::Definition((ref name, ref ttype), ref body) => {
            let ltype = get_type(data, scope.clone(), ttype.clone().unwrap(), true);
            let pointer = if scope.borrow().is_global() {
                //llvm::core::LLVMAddGlobal(data.module, ltype, label(name.as_str()).unwrap().as_ptr())
                let global = llvm::core::LLVMGetNamedGlobal(data.module, label(name.as_str()).as_ptr());
                llvm::core::LLVMSetInitializer(global, null_value(ltype));
                global
            }
            else {
                llvm::core::LLVMBuildAlloca(data.builder, ltype, label(name.as_str()).as_ptr())
            };
            scope.borrow_mut().assign(name, pointer);
            let value = compile_node(data, func, scope, body);
            llvm::core::LLVMBuildStore(data.builder, value, pointer);
            value
        },

        AST::Block(ref body) => { compile_vec(data, func, scope, body) },

        AST::If(ref cond, ref texpr, ref fexpr) => {
            let cond_value = compile_node(data, func, scope.clone(), cond);
            let ctype = llvm::core::LLVMTypeOf(cond_value);
            let zero = llvm::core::LLVMConstInt(ctype, 0, 0);
            let is_nonzero = llvm::core::LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntNE, cond_value, zero, label("is_nonzero").as_ptr());

            let entry_name = label("if");
            let texpr_block = llvm::core::LLVMAppendBasicBlockInContext(data.context, func, entry_name.as_ptr());
            let fexpr_block = llvm::core::LLVMAppendBasicBlockInContext(data.context, func, entry_name.as_ptr());
            let merge_block = llvm::core::LLVMAppendBasicBlockInContext(data.context, func, entry_name.as_ptr());

            llvm::core::LLVMBuildCondBr(data.builder, is_nonzero, texpr_block, fexpr_block);


            llvm::core::LLVMPositionBuilderAtEnd(data.builder, texpr_block);
            let texpr_value = compile_node(data, func, scope.clone(), texpr);
            llvm::core::LLVMBuildBr(data.builder, merge_block);
            let texpr_block = llvm::core::LLVMGetInsertBlock(data.builder);

            llvm::core::LLVMPositionBuilderAtEnd(data.builder, fexpr_block);
            let fexpr_value = compile_node(data, func, scope.clone(), fexpr);
            llvm::core::LLVMBuildBr(data.builder, merge_block);
            let fexpr_block = llvm::core::LLVMGetInsertBlock(data.builder);

            llvm::core::LLVMPositionBuilderAtEnd(data.builder, merge_block);
            let phi = llvm::core::LLVMBuildPhi(data.builder, llvm::core::LLVMInt64TypeInContext(data.context), label("iftmp").as_ptr());

            let mut values = vec![texpr_value, fexpr_value];
            let mut blocks = vec![texpr_block, fexpr_block];

            llvm::core::LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);
            phi
        },

/*
        AST::Raise(ref expr) => {

        },

        AST::Try(ref cond, ref cases) => {

        },

        AST::For(ref name, ref cond, ref body, ref id) => {
            let lscope = map.get(id);

        },

        AST::Match(ref cond, ref cases) => {
            let old = self.indent.clone();
            self.indent = old.clone() + &"    ";

            // TODO should you implement this as an if statement instead?
            let mut compiled_cases = vec!();
            for &(ref case, ref expr) in cases {
                compiled_cases.push(format!("{space}  case {}:\n{indent}{}\n{indent}break;", self.compile_node(scope.clone(), case, false), self.compile_node(scope.clone(), expr, is_last), space=old, indent=self.indent));
            }
            let compiled = format!("select ({}) {{\n{}\n{space}}}", self.compile_node(scope.clone(), cond, false), compiled_cases.join("\n"), space=old);

            self.indent = old;
            compiled
        },

        AST::While(ref cond, ref body) => {
            let old = self.indent.clone();
            self.indent = old.clone() + &"    ";
            let compiled = format!("while ({}) {{\n{indent}{}\n{space}}}", self.compile_node(scope.clone(), cond, false), add_terminator(self.compile_node(scope.clone(), body, false)), space=old, indent=self.indent);
            self.indent = old;
            compiled
        },
*/

        AST::Class(ref name, ref parent, ref body, ref id) => {
            let tscope = data.map.get(id);
            let cscope = scope.borrow().find_class_def(name).unwrap();
            
            //compile_vec(data, func, tscope.clone(), body);

            let mut last = zero_value();
            for node in body {
                last = match *node {
                    AST::Definition((ref name, ref ttype), ref expr) => {
                        let value = compile_node(data, func, tscope.clone(), expr);
                        //let ltype = get_type(data, scope.clone(), ttype.clone().unwrap(), true);
                        if let AST::Function(_, _, _, _) = **expr {
                            cscope.borrow_mut().assign(name, value);
                        }
                        value
                    },
                    _ => { compile_node(data, func, tscope.clone(), node) }
                }
            }
            // TODO this might not be right
            last
        },

/*
        AST::Index(ref base, ref index) => {

        },

        AST::Resolver(ref left, ref right) => {

        },

*/
        AST::Accessor(ref left, ref right, ref ltype) => {
            // TODO This is obviously not filled in
            let object = compile_node(data, func, scope.clone(), left);

            match ltype.clone().unwrap() {
                Type::Class(ref name) |
                Type::Concrete(ref name) => {
                    let classdef = scope.borrow().find_class_def(name).unwrap();
                    let sym = classdef.borrow().find(right).unwrap();
                    if let Type::Function(_, _) = sym.ttype {
                        sym.value.unwrap()
                    }
                    else {
                        let structdef = scope.borrow().find_struct_def(name).unwrap();
                        let index = structdef.iter().position(|ref r| r.0 == *right).unwrap();
                        llvm::core::LLVMBuildExtractElement(data.builder, object, llvm::core::LLVMConstInt(llvm::core::LLVMInt32TypeInContext(data.context), index as u64, 0), label("tmp").as_ptr())
                    }
                },
                _ => panic!("Invalid type {:?}", ltype),
            }
        },

/*
        AST::Import(_) => { },

        AST::Type(_) => { },
*/

        _ => {
            let int_type = llvm::core::LLVMInt64TypeInContext(data.context);
            let num: isize = -12;
            llvm::core::LLVMConstInt(int_type, num as u64, 0)
        },
    }
}

unsafe fn compile_functions_vec(data: &mut LLVM, scope: ScopeRef<Value>, items: &Vec<AST>) {
    for item in items {
        compile_functions_node(data, scope.clone(), item);
    }
}

unsafe fn compile_functions_node(data: &mut LLVM, scope: ScopeRef<Value>, node: &AST) {
    match *node {

        AST::Function(ref args, ref body, ref id, ref ttype) => {
            let fscope = data.map.get(id);
            let ftype = get_type(data, fscope.clone(), ttype.clone().unwrap(), false);
            let function = llvm::core::LLVMAddFunction(data.module, label(format!("anon{}", id).as_str()).as_ptr(), ftype);

            let nargs = llvm::core::LLVMCountParams(function) as usize;
            if nargs != 0 && nargs != args.len() {
                panic!("ArgsError: argument counts don't match");
            }

            for (i, &(ref name, _, _)) in args.iter().enumerate() {
                let llarg = llvm::core::LLVMGetParam(function, i as u32);
                llvm::core::LLVMSetValueName(llarg, label(name.as_str()).into_raw());
                fscope.borrow_mut().assign(name, llarg);
            }

            let bb = llvm::core::LLVMAppendBasicBlockInContext(data.context, function, label("entry").as_ptr());
            llvm::core::LLVMPositionBuilderAtEnd(data.builder, bb);
            let ret = compile_node(data, function, fscope.clone(), body);
            llvm::core::LLVMBuildRet(data.builder, ret);

            //if llvm::analysis::LLVMVerifyFunction(function, llvm::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction) != 0 {
            //    panic!("VerifyError: verification failed");
            //}
            llvm::core::LLVMRunFunctionPassManager(data.funcpass, function);
        },

        AST::List(ref items) => { compile_functions_vec(data, scope, items) },

        AST::Invoke(ref name, ref args, _) => { compile_functions_vec(data, scope, args) },

        AST::Definition((ref name, ref ttype), ref body) => {
            compile_functions_node(data, scope, body)
        },

        AST::Block(ref body) => { compile_functions_vec(data, scope, body) },

        AST::If(ref cond, ref texpr, ref fexpr) => {
            compile_functions_node(data, scope.clone(), cond);
            compile_functions_node(data, scope.clone(), texpr);
            compile_functions_node(data, scope, fexpr)
        },

        AST::Raise(ref expr) => { compile_functions_node(data, scope, expr) },

        AST::Try(ref cond, ref cases) |
        AST::Match(ref cond, ref cases) => {
            compile_functions_node(data, scope.clone(), cond);
            for case in cases {
                compile_functions_node(data, scope.clone(), &case.0);
                compile_functions_node(data, scope.clone(), &case.1);
            }
        },

        AST::For(ref name, ref cond, ref body, ref id) => {
            let lscope = data.map.get(id);
            compile_functions_node(data, lscope.clone(), cond);
            compile_functions_node(data, lscope.clone(), body);
        },

        AST::While(ref cond, ref body) => {
            compile_functions_node(data, scope.clone(), cond);
            compile_functions_node(data, scope.clone(), body);
        },

        AST::Class(ref name, ref parent, ref body, ref id) => {
            let tscope = data.map.get(id);

            let structdef = scope.borrow().find_struct_def(name).unwrap();
            let mut types = vec!();
            for (_, ttype) in structdef {
                types.push(get_type(data, tscope.clone(), ttype.clone(), true))
            }
            let lltype = llvm::core::LLVMStructCreateNamed(data.context, label(name).as_ptr());
            llvm::core::LLVMStructSetBody(lltype, types.as_mut_ptr(), types.len() as u32, false as i32);
            data.types.insert(name.clone(), lltype);

            // TODO you need to add the 'new' function definition here, but the value should be stored in cscope, not tscope...
            compile_functions_vec(data, tscope.clone(), body);
            // TODO the values stored to tscope must be transfered (or only stored in) cscope
        },

        AST::Index(ref left, ref right) => {
            compile_functions_node(data, scope.clone(), left);
            compile_functions_node(data, scope.clone(), right);
        },

        AST::Resolver(ref left, ref right) => {
            compile_functions_node(data, scope.clone(), left);
        },

        AST::Accessor(ref left, ref right, _) => {
            compile_functions_node(data, scope.clone(), left);
        },

        //AST::Type(_) => { },

        AST::Import(_) => { },      // TODO nothing to do
        _ => { },
    }
}



fn label(string: &str) -> CString {
    CString::new(string).unwrap()
}

unsafe fn null_value(ttype: LLVMTypeRef) -> LLVMValueRef {
    llvm::core::LLVMConstNull(ttype)
}

unsafe fn i32_type(data: &LLVM) -> LLVMTypeRef {
    llvm::core::LLVMInt32TypeInContext(data.context)
}

unsafe fn int_type(data: &LLVM) -> LLVMTypeRef {
    llvm::core::LLVMInt64TypeInContext(data.context)
}
unsafe fn zero_value() -> LLVMValueRef {
    let int_type = llvm::core::LLVMInt64Type();
    llvm::core::LLVMConstInt(int_type, 0, 0)
}

unsafe fn str_type(data: &LLVM) -> LLVMTypeRef {
    llvm::core::LLVMPointerType(llvm::core::LLVMInt8Type(), 0)
}

unsafe fn build_str_const(data: &LLVM, string: &str) -> LLVMValueRef {
    llvm::core::LLVMBuildGlobalStringPtr(data.builder, label(string).as_ptr(), label("str").as_ptr())
}

unsafe fn build_malloc(data: &LLVM, size: LLVMValueRef) -> LLVMValueRef {
    build_call(data, "malloc", &mut vec!(size))
}

unsafe fn build_malloc_const(data: &LLVM, size: usize) -> LLVMValueRef {
    let cint_type = llvm::core::LLVMInt32TypeInContext(data.context);
    build_call(data, "malloc", &mut vec!(llvm::core::LLVMConstInt(cint_type, size as u64, 0)))
}

unsafe fn build_realloc(data: &LLVM, ptr: LLVMValueRef, size: LLVMValueRef) -> LLVMValueRef {
    build_call(data, "realloc", &mut vec!(ptr, size))
}


unsafe fn declare_function(module: LLVMModuleRef, scope: ScopeRef<Value>, name: &str, args: &mut [LLVMTypeRef], ret_type: LLVMTypeRef, vargs: bool) {
    let ftype = llvm::core::LLVMFunctionType(ret_type, args.as_mut_ptr(), args.len() as u32, vargs as i32);
    let func = llvm::core::LLVMAddFunction(module, label(name).as_ptr(), ftype);
    let name = &String::from(name);
    if scope.borrow().find(name).is_some() {
        scope.borrow_mut().assign(name, func);
    }
}

unsafe fn build_function_start(data: &LLVM, name: &str, args: Vec<(String, LLVMTypeRef)>, return_type: LLVMTypeRef) -> LLVMValueRef {
    let mut atypes: Vec<LLVMTypeRef> = args.iter().map(|t| t.1.clone()).collect();
    let ftype = llvm::core::LLVMFunctionType(return_type, atypes.as_mut_ptr(), atypes.len() as u32, false as i32);
    let function = llvm::core::LLVMAddFunction(data.module, label(name).as_ptr(), ftype);

    let nargs = llvm::core::LLVMCountParams(function) as usize;
    if nargs != 0 && nargs != args.len() {
        panic!("ArgsError: argument counts don't match");
    }

    for (i, &(ref name, _)) in args.iter().enumerate() {
        let llarg = llvm::core::LLVMGetParam(function, i as u32);
        llvm::core::LLVMSetValueName(llarg, label(name.as_str()).into_raw());
        //scope.borrow_mut().assign(name, llarg);
    }

    let bb = llvm::core::LLVMAppendBasicBlockInContext(data.context, function, label("entry").as_ptr());
    llvm::core::LLVMPositionBuilderAtEnd(data.builder, bb);

    function
}

unsafe fn build_call(data: &LLVM, name: &str, largs: &mut Vec<LLVMValueRef>) -> LLVMValueRef {
    let function = llvm::core::LLVMGetNamedFunction(data.module, label(name).as_ptr());
    llvm::core::LLVMBuildCall(data.builder, function, largs.as_mut_ptr(), largs.len() as u32, label("tmp").as_ptr())
}




unsafe fn get_type(data: &LLVM, scope: ScopeRef<Value>, ttype: Type, use_fptrs: bool) -> LLVMTypeRef {
    match ttype {
        Type::Concrete(ref tname) => match tname.as_str() {
            "Bool" => llvm::core::LLVMInt1TypeInContext(data.context),
            "Int" => llvm::core::LLVMInt64TypeInContext(data.context),
            "Real" => llvm::core::LLVMDoubleTypeInContext(data.context),
            "String" => llvm::core::LLVMPointerType(llvm::core::LLVMInt8Type(), 0),

            // TODO this should panic...
            _ => llvm::core::LLVMInt64TypeInContext(data.context),
        },
        Type::Function(ref args, ref ret) => {
            let mut atypes = vec!();
            for ttype in args {
                atypes.push(get_type(data, scope.clone(), ttype.clone(), true));
            }
            let rtype = get_type(data, scope.clone(), *ret.clone(), true);
            let ftype = llvm::core::LLVMFunctionType(rtype, atypes.as_mut_ptr(), atypes.len() as u32, false as i32);
            if use_fptrs {
                llvm::core::LLVMPointerType(ftype, 0)
            }
            else {
                ftype
            }
        },
        Type::Class(ref name) => {
            /*
            println!("GETTING CLASS STRUCTDEF: {:?}", name);
            let structdef = scope.borrow().find_struct_def(name).unwrap();
            let mut types = vec!();
            for (name, ttype) in structdef {
                match ttype {
                    Type::Function(_, _) => { },
                    _ => types.push(get_type(data, scope.clone(), ttype.clone(), true)),
                }
            }
            llvm::core::LLVMPointerType(llvm::core::LLVMStructTypeInContext(data.context, types.as_mut_ptr(), types.len() as u32, false as i32), 0)
            */
            llvm::core::LLVMPointerType(*data.types.get(name).unwrap(), 0)
        },
        // TODO this is not the correct way to deal with type variables... there should be overloaded functions generated
        Type::Variable(ref tname) => llvm::core::LLVMInt64TypeInContext(data.context),
        _ => panic!("InvalidType: cannot convert to llvm, {:?}", ttype),
    }
}


unsafe fn compile_builtin(data: &LLVM, func: LLVMValueRef, scope: ScopeRef<Value>, name: &String, args: &Vec<AST>, stype: Type) -> Option<LLVMValueRef> {
    if args.len() <= 0 {
        return None
    }

    if vec!("and", "or").contains(&name.as_str()) {
        // TODO fill this in
        Some(zero_value())
    }
    else {
        let mut largs = vec!();
        for arg in args {
            largs.push(compile_node(data, func, scope.clone(), arg));
        }

        println!("**BUILTINS: {:?}", stype);
        match stype {
            Type::Function(ref sargs, _) => {
                match sargs[0] {
                    Type::Concrete(ref tname) => {
                        match data.builtins.get(tname) {
                            Some(ref map) => match map.get(name) {
                                Some(func) => Some(func(data, largs)),
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
}


use std::collections::HashMap;

pub type Function = fn(&LLVM, Vec<Value>) -> Value;
pub type FunctionMap = HashMap<String, Function>;
pub type TypeFunctionMap = HashMap<String, FunctionMap>;

unsafe fn initialize_builtins(data: &mut LLVM) {
    initialize_type_int(data);
    initialize_type_real(data);
    initialize_type_string(data);
}

unsafe fn initialize_type_int(data: &mut LLVM) {
    let mut builtins: FunctionMap = HashMap::new();

    fn add(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildAdd(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    fn sub(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildSub(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    fn mul(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildMul(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    fn div(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildSDiv(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    fn eq(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntEQ, args[0], args[1], label("tmp").as_ptr()) } }
    fn ne(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntNE, args[0], args[1], label("tmp").as_ptr()) } }
    fn lt(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntSLT, args[0], args[1], label("tmp").as_ptr()) } }
    fn gt(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntSGT, args[0], args[1], label("tmp").as_ptr()) } }
    fn lte(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntSLE, args[0], args[1], label("tmp").as_ptr()) } }
    fn gte(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntSGE, args[0], args[1], label("tmp").as_ptr()) } }
    fn not(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildNot(data.builder, args[0], label("tmp").as_ptr()) } }

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

    fn add(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildFAdd(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    fn sub(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildFSub(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    fn mul(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildFMul(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    fn div(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildFDiv(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    fn eq(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOEQ, args[0], args[1], label("tmp").as_ptr()) } }
    fn ne(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealONE, args[0], args[1], label("tmp").as_ptr()) } }
    fn lt(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOLT, args[0], args[1], label("tmp").as_ptr()) } }
    fn gt(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOGT, args[0], args[1], label("tmp").as_ptr()) } }
    fn lte(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOLE, args[0], args[1], label("tmp").as_ptr()) } }
    fn gte(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOGE, args[0], args[1], label("tmp").as_ptr()) } }
    fn not(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildNot(data.builder, args[0], label("tmp").as_ptr()) } }

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

    build_lib_str(data);
    build_lib_strcat(data);

    fn add(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildFAdd(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    //fn sub(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildFSub(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    //fn mul(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildFMul(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    //fn div(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildFDiv(data.builder, args[0], args[1], label("tmp").as_ptr()) } }
    fn eq(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOEQ, args[0], args[1], label("tmp").as_ptr()) } }
    fn ne(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealONE, args[0], args[1], label("tmp").as_ptr()) } }
    //fn lt(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOLT, args[0], args[1], label("tmp").as_ptr()) } }
    //fn gt(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOGT, args[0], args[1], label("tmp").as_ptr()) } }
    //fn lte(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOLE, args[0], args[1], label("tmp").as_ptr()) } }
    //fn gte(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildFCmp(data.builder, llvm::LLVMRealPredicate::LLVMRealOGE, args[0], args[1], label("tmp").as_ptr()) } }
    //fn not(data: &LLVM, args: Vec<Value>) -> Value { unsafe { llvm::core::LLVMBuildNot(data.builder, args[0], label("tmp").as_ptr()) } }

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

    let pointer = llvm::core::LLVMBuildAlloca(data.builder, str_type(data), label("buffer").as_ptr());
    llvm::core::LLVMBuildStore(data.builder, build_malloc_const(data, 22), pointer);

    let value = llvm::core::LLVMBuildLoad(data.builder, pointer, label("tmp").as_ptr());
    let num = llvm::core::LLVMGetParam(function, 0);
    build_call(data, "sprintf", &mut vec!(value, build_str_const(data, "%d"), num));

    llvm::core::LLVMBuildRet(data.builder, value);
}

unsafe fn build_lib_strcat(data: &LLVM) {
    let args = vec!((String::from("str1"), str_type(data)), (String::from("str2"), str_type(data)));
    let function = build_function_start(data, "strcat", args, str_type(data));

    let str1 = llvm::core::LLVMGetParam(function, 0);
    let str2 = llvm::core::LLVMGetParam(function, 1);
    let str1len = build_call(data, "strlen", &mut vec!(str1));
    let str2len = build_call(data, "strlen", &mut vec!(str2));
    let sum = llvm::core::LLVMBuildAdd(data.builder, str1len, str2len, label("tmp").as_ptr());

    let buffer = build_malloc(data, sum);

    build_call(data, "memcpy", &mut vec!(buffer, str1, str1len));
    //build_call(data, "memcpy", &mut vec!(buffer, str2, str1len));

    llvm::core::LLVMBuildRet(data.builder, buffer);
}

