
use std::ptr;
use std::ffi::CString;

extern crate llvm_sys as llvm;
use self::llvm::prelude::*;
use self::llvm::core::*;

use export;
use types::Type;
use utils::UniqueID;
use config::Options;
use session::Session;
use ast::{ AST, Pos };
use scope::{ Scope, ScopeRef, ScopeMapRef };
use lib_llvm::{ Builtin, BuiltinMap, initialize_builtins };



pub type Value = LLVMValueRef;

/*
#[derive(Clone)]
pub enum Value {
    Ref(LLVMValueRef),
    Builtin(fn(&LLVM, Vec<LLVMValueRef>) -> LLVMValueRef),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Value::Ref(val) => write!(f, "{:?}", val),
            &Value::Builtin(func) => write!(f, "Builtin")
        }
    }
}
*/


#[derive(Clone, Debug, PartialEq)]
pub struct TypeValue {
    pub structdef: Vec<(String, Type)>,
    pub value: LLVMTypeRef,
    pub vtable: Vec<(String, Type)>,
    pub vttype: Option<LLVMTypeRef>,
}


pub struct LLVM<'a> {
    pub session: &'a Session<Value, TypeValue>,
    pub map: ScopeMapRef<Value, TypeValue>,
    //pub builtins: TypeFunctionMap,
    pub builtins: BuiltinMap<'a>,
    pub functions: Vec<&'a AST>,
    pub classes: Vec<&'a AST>,
    pub context: LLVMContextRef,
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,
    //pub funcpass: LLVMPassManagerRef,
}

type Unwind = Option<(LLVMBasicBlockRef, LLVMBasicBlockRef)>;


pub fn compile(builtins: &Vec<Builtin>, session: &Session<Value, TypeValue>, module_name: &str, code: &Vec<AST>) {
    unsafe {
        compile_module(builtins, session, session.map.get_global(), module_name, code)
    }
}

unsafe fn compile_module(builtins: &Vec<Builtin>, session: &Session<Value, TypeValue>, scope: ScopeRef<Value, TypeValue>, module_name: &str, code: &Vec<AST>) {
    let context = LLVMContextCreate();
    let module = LLVMModuleCreateWithName(label(module_name));
    let builder = LLVMCreateBuilderInContext(context);
    let data = &mut LLVM {
        session: session,
        map: session.map.clone(),
        builtins: BuiltinMap::new(),
        functions: Vec::new(),
        classes: Vec::new(),
        context: context,
        module: module,
        builder: builder
    };
    //LLVMSetDataLayout(data.module, label("e-m:e-i64:64-f80:128-n8:16:32:64-S128"));

    initialize_builtins(data, scope.clone(), builtins);
    collect_functions_vec(data, scope.clone(), code);
    declare_globals(data, scope.clone());
    for func in &data.functions {
        build_function_body(data, func);
    }

    let module_init_name = format!("init.{}", module_name.replace("/", "."));

    let function_type = LLVMFunctionType(bool_type(data), ptr::null_mut(), 0, 0);
    let function = LLVMAddFunction(module, label(module_init_name.as_str()), function_type);
    //LLVMSetPersonalityFn(function, LLVMGetNamedFunction(data.module, label("__gxx_personality_v0")));
    LLVMPositionBuilderAtEnd(builder, LLVMAppendBasicBlockInContext(context, function, label("entry")));
    compile_vec(data, function, None, scope.clone(), code);
    LLVMBuildRet(builder, LLVMConstInt(bool_type(data), 1, 0));


    if !Options::as_ref().is_library {
        let pos = Pos::empty();
        let function_type = LLVMFunctionType(int_type(data), ptr::null_mut(), 0, 0);
        let function = LLVMAddFunction(module, b"main\0".as_ptr() as *const _, function_type);
        //LLVMSetPersonalityFn(function, LLVMGetNamedFunction(data.module, label("__gxx_personality_v0")));
        LLVMPositionBuilderAtEnd(builder, LLVMAppendBasicBlockInContext(context, function, label("entry")));
        compile_node(data, function, None, scope.clone(), &AST::Invoke(pos.clone(), Box::new(AST::Identifier(pos.clone(), String::from(module_init_name))), vec!(), Some(Type::Function(vec!(), Box::new(Type::Object(String::from("Bool"), vec!()))))));
        LLVMBuildRet(builder, int_value(data, 0));
    }

    // Output to a file, and also a string for debugging
    LLVMPrintModuleToFile(module, label(format!("{}.ll", module_name).as_str()), ptr::null_mut());

    if Options::as_ref().debug {
        println!("{}\n", CString::from_raw(LLVMPrintModuleToString(module)).into_string().unwrap());
    }

    export::write_exports(data.map.clone(), data.map.get_global(), format!("{}.dec", module_name).as_str(), code);

    LLVMDisposeBuilder(builder);
    LLVMDisposeModule(module);
    LLVMContextDispose(context);
}

unsafe fn declare_globals(data: &LLVM, scope: ScopeRef<Value, TypeValue>) {
    for node in &data.classes {
        if let AST::Class(_, (ref name, _), _, _, ref id) = **node {
            let tscope = data.map.get(id);
            let classdef = scope.borrow().get_class_def(name);
            let value = scope.borrow().get_type_value(name).unwrap();

            if value.vtable.len() > 0 {
                let mut methods = vec!();
                for (index, &(ref name, _)) in value.vtable.iter().enumerate() {
                    methods.push(classdef.borrow().get_variable_value(&name).unwrap());
                }

                let vtype = value.vttype.unwrap();
                let global = LLVMAddGlobal(data.module, LLVMGetElementType(vtype), label(format!("__{}_vtable", name).as_str()));
                LLVMSetInitializer(global, LLVMConstNamedStruct(vtype, methods.as_mut_ptr(), methods.len() as u32));
                LLVMSetLinkage(global, llvm::LLVMLinkage::LLVMLinkOnceAnyLinkage);
            }
        }
    }

    for (name, sym) in &scope.borrow().names {
        if LLVMGetNamedFunction(data.module, label(name.as_str())).is_null() && !sym.ttype.as_ref().unwrap().is_overloaded() {
            let ltype = get_type(data, scope.clone(), sym.ttype.clone().unwrap(), true);
            LLVMAddGlobal(data.module, ltype, label(name.as_str()));
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
    debug!("COMPILE: {:?}", node);
    match *node {
        AST::Noop => ptr::null_mut(),
        AST::Nil(ref ttype) => null_value(get_type(data, scope.clone(), ttype.clone().unwrap(), true)),
        AST::Boolean(ref num) => LLVMConstInt(bool_type(data), *num as u64, 0),
        AST::Integer(ref num) => LLVMConstInt(int_type(data), *num as u64, 0),
        AST::Real(ref num) => LLVMConstReal(real_type(data), *num),
        AST::String(ref string) => LLVMBuildGlobalStringPtr(data.builder, label(string.as_str()), label("strc")),

        AST::Block(_, ref body) => { compile_vec(data, func, unwind, scope, body) },

        AST::Invoke(ref pos, ref fexpr, ref args, ref stype) => {
            let (atypes, rtype) = match stype.clone().unwrap() {
                Type::Function(atypes, rtype) => (atypes, *rtype),
                stype @ _ => panic!("TypeError: expected function type: {:?}", stype),
            };

            let mut largs = vec!();
            for (ttype, arg) in atypes.iter().zip(args.iter()) {
                let mut larg = compile_node(data, func, unwind, scope.clone(), arg);
                let ltype = get_type(data, scope.clone(), ttype.clone(), true);
                if ltype != LLVMTypeOf(larg) {
                    // TODO this seems to cast to int as well as pointers, so maybe it's doing too much, at least without checking that it's supposed to
                    larg = LLVMBuildPointerCast(data.builder, larg, ltype, label("ptr"));
                }
                largs.push(larg);
            }

            let name = match **fexpr {
                AST::Identifier(_, ref name) => name.clone(),
                _ => String::from("")
            };

            if let Some(result) = BuiltinMap::compile_builtin(data, scope.clone(), &name, &largs, stype.clone().unwrap()) {
                debug!("BUILTIN: {:?}", result);
                result
            } else {
                let mut function = LLVMGetNamedFunction(data.module, label(name.as_str()));
                if function.is_null() {
                    //function = compile_node(data, func, unwind, scope.clone(), fexpr);
                    function = match **fexpr {
                        // TODO this is possibly very bad, but hopefully it works temporarily, for vtable
                        //AST::Accessor(ref pos, _, ref name, ref stype) => {
                        //    compile_node(data, func, unwind, scope.clone(), &AST::Resolver(pos.clone(), Box::new(AST::Identifier(pos.clone(), stype.clone().unwrap().get_name().unwrap())), name.clone()))
                        //},
                        _ => compile_node(data, func, unwind, scope.clone(), fexpr)
                    };
                }
                //LLVMDumpValue(function);

                // Cast values to the function's declared type; this is a hack for typevar/generic arguments
                let mut lftype = LLVMTypeOf(function);
                if LLVMGetTypeKind(lftype) == llvm::LLVMTypeKind::LLVMPointerTypeKind {
                    lftype = LLVMGetElementType(lftype);
                }
                let mut ltypes = Vec::with_capacity(LLVMCountParamTypes(lftype) as usize);
                ltypes.set_len(LLVMCountParamTypes(lftype) as usize);
                LLVMGetParamTypes(lftype, ltypes.as_mut_ptr());
                for i in 0 .. ltypes.len() {
                    if ltypes[i] != LLVMTypeOf(largs[i]) {
                        debug!("{:?} -> {:?}", LLVMGetTypeKind(LLVMTypeOf(largs[i])), LLVMGetTypeKind(ltypes[i]));
                        largs[i] = build_generic_cast(data, largs[i], ltypes[i]);
                    }
                }

                //LLVMBuildCall(data.builder, function, largs.as_mut_ptr(), largs.len() as u32, label("tmp").as_ptr())
                let mut value = match unwind {
                    None => LLVMBuildCall(data.builder, function, largs.as_mut_ptr(), largs.len() as u32, label("tmp")),
                    Some((then, catch)) => {
                        LLVMBuildInvoke(data.builder, function, largs.as_mut_ptr(), largs.len() as u32, then, catch, label("tmp"))
                        //LLVMSetUnwindDest(invoke, unwind.unwrap());
                    }
                };

                let lrtype = get_type(data, scope.clone(), rtype, true);
                if lrtype != LLVMTypeOf(value) {
                    debug!("RETURN {:?} -> {:?}", LLVMGetTypeKind(LLVMTypeOf(value)), LLVMGetTypeKind(lrtype));
                    value = build_generic_cast(data, value, lrtype);
                }
                value
            }
        },

        AST::Function(_, ref name, _, _, _, ref id) => {
            let fname = scope.borrow().get_full_name(name, id.clone());
            LLVMGetNamedFunction(data.module, label(fname.as_str()))
        },

        AST::Identifier(ref pos, ref name) => {
            let pointer = match scope.borrow().get_variable_value(name) {
                Ok(x) => x,
                Err(_) => {
                    let pointer = LLVMGetNamedGlobal(data.module, label(name.as_str()));
                    if pointer.is_null() {
                        panic!("UnsetError:{:?}: use before assignment {:?}", pos, name);
                    }
                    pointer
                }
            };
            debug!("IDENT: {:?} {:?}", LLVMGetValueKind(pointer), LLVMGetTypeKind(LLVMTypeOf(pointer)));
            //if LLVMGetTypeKind(LLVMTypeOf(pointer)) == llvm::LLVMTypeKind::LLVMPointerTypeKind {
            match LLVMGetValueKind(pointer) {
                llvm::LLVMValueKind::LLVMArgumentValueKind |
                llvm::LLVMValueKind::LLVMFunctionValueKind => pointer,
                _ => LLVMBuildLoad(data.builder, pointer, label("tmp"))
            }
        },

        AST::Definition(_, (ref name, ref ttype), ref value) => {
            let ltype = get_type(data, scope.clone(), ttype.clone().unwrap(), true);
            let pointer = if scope.borrow().is_global() {
                //LLVMAddGlobal(data.module, ltype, label(name.as_str()))
                let global = LLVMGetNamedGlobal(data.module, label(name.as_str()));
                LLVMSetInitializer(global, null_value(ltype));
                global
            } else {
                LLVMBuildAlloca(data.builder, ltype, label(name.as_str()))
            };
            scope.borrow_mut().assign(name, pointer);
            let value = compile_node(data, func, unwind, scope, value);
            LLVMBuildStore(data.builder, value, pointer);
            value
        },

        // TODO you might need to create a new function to hold the body, so that it can be resumed from, and there also
        // might be an issue when you call a function one level down, and pass it the same unwind locations... it might effect all
        // functions, all the way down
        AST::Try(_, ref body, ref cases) => {
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
            //zero_int(data)
            null_value(str_type(data))
        },

        AST::Raise(_, ref expr) => {
            //LLVMBuildResume(data.builder, compile_node(data, func, unwind, scope.clone(), expr))
            //zero_int(data)
            null_value(str_type(data))
        },

        AST::SideEffect(_, ref op, ref args) => {
            // TODO This only handles two arguments, which is all that will be parsed, but you can do better... 
            let lexpr_block = LLVMAppendBasicBlockInContext(data.context, func, label(op));
            let rexpr_block = LLVMAppendBasicBlockInContext(data.context, func, label(op));
            let merge_block = LLVMAppendBasicBlockInContext(data.context, func, label(op));

            LLVMBuildBr(data.builder, lexpr_block);

            LLVMPositionBuilderAtEnd(data.builder, lexpr_block);
            let lexpr_value = compile_node(data, func, unwind, scope.clone(), &args[0]);
            let test_type = match op.as_str() {
                "or" => llvm::LLVMIntPredicate::LLVMIntNE,
                "and" => llvm::LLVMIntPredicate::LLVMIntEQ,
                _ => panic!("NotImplementedError: attempted to compile invalid side effect operation: {}", op)
            };
            let is_enough = LLVMBuildICmp(data.builder, test_type, lexpr_value, null_value(LLVMTypeOf(lexpr_value)), label("is_enough"));
            LLVMBuildCondBr(data.builder, is_enough, merge_block, rexpr_block);

            LLVMPositionBuilderAtEnd(data.builder, rexpr_block);
            let rexpr_value = compile_node(data, func, unwind, scope.clone(), &args[1]);
            LLVMBuildBr(data.builder, merge_block);

            LLVMPositionBuilderAtEnd(data.builder, merge_block);
            let phi = LLVMBuildPhi(data.builder, LLVMTypeOf(lexpr_value), label(op));

            let mut values = vec![lexpr_value, rexpr_value];
            let mut blocks = vec![lexpr_block, rexpr_block];

            LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);
            phi
        },

        AST::If(_, ref cond, ref texpr, ref fexpr) => {
            let cond_value = compile_node(data, func, unwind, scope.clone(), cond);
            let cond_zero = LLVMConstInt(LLVMTypeOf(cond_value), 0, 0);
            let is_nonzero = LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntNE, cond_value, cond_zero, label("is_nonzero"));

            let texpr_block = LLVMAppendBasicBlockInContext(data.context, func, label("ifthen"));
            let fexpr_block = LLVMAppendBasicBlockInContext(data.context, func, label("ifelse"));
            let merge_block = LLVMAppendBasicBlockInContext(data.context, func, label("ifend"));

            LLVMBuildCondBr(data.builder, is_nonzero, texpr_block, fexpr_block);


            LLVMPositionBuilderAtEnd(data.builder, texpr_block);
            let texpr_value = compile_node(data, func, unwind, scope.clone(), texpr);
            LLVMBuildBr(data.builder, merge_block);
            let texpr_block = LLVMGetInsertBlock(data.builder);

            LLVMPositionBuilderAtEnd(data.builder, fexpr_block);
            let fexpr_value = compile_node(data, func, unwind, scope.clone(), fexpr);
            LLVMBuildBr(data.builder, merge_block);
            let fexpr_block = LLVMGetInsertBlock(data.builder);

            LLVMPositionBuilderAtEnd(data.builder, merge_block);
            let phi = LLVMBuildPhi(data.builder, LLVMTypeOf(texpr_value), label("iftmp"));

            let mut values = vec![texpr_value, fexpr_value];
            let mut blocks = vec![texpr_block, fexpr_block];

            LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);
            phi
        },

        AST::Match(_, ref cond, ref cases) => {
            let mut cond_blocks = vec!();
            let mut do_blocks = vec!();
            for _ in 0 .. cases.len() {
                cond_blocks.push(LLVMAppendBasicBlockInContext(data.context, func, label("matchcond")));
                do_blocks.push(LLVMAppendBasicBlockInContext(data.context, func, label("matchdo")));
            }
            let merge_block = LLVMAppendBasicBlockInContext(data.context, func, label("matchend"));
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
                        let is_true = LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntEQ, cond_value, case_value, label("is_true"));
                        LLVMBuildCondBr(data.builder, is_true, do_blocks[i], cond_blocks[i + 1]);
                    }
                }

                LLVMPositionBuilderAtEnd(data.builder, do_blocks[i]);
                values.push(compile_node(data, func, unwind, scope.clone(), expr));
                LLVMBuildBr(data.builder, merge_block);
            }

            LLVMPositionBuilderAtEnd(data.builder, merge_block);
            let phi = LLVMBuildPhi(data.builder, LLVMTypeOf(values[0]), label("matchphi"));

            LLVMAddIncoming(phi, values.as_mut_ptr(), do_blocks.as_mut_ptr(), values.len() as u32);
            phi
        },

        AST::While(_, ref cond, ref body) => {
            let before_block = LLVMAppendBasicBlockInContext(data.context, func, label("while"));
            let body_block = LLVMAppendBasicBlockInContext(data.context, func, label("whilebody"));
            let after_block = LLVMAppendBasicBlockInContext(data.context, func, label("whileend"));

            LLVMBuildBr(data.builder, before_block);
            LLVMPositionBuilderAtEnd(data.builder, before_block);
            let cond_value = compile_node(data, func, unwind, scope.clone(), cond);
            let cond_zero = LLVMConstInt(LLVMTypeOf(cond_value), 0, 0);
            let is_nonzero = LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntNE, cond_value, cond_zero, label("is_nonzero"));
            LLVMBuildCondBr(data.builder, is_nonzero, body_block, after_block);

            LLVMPositionBuilderAtEnd(data.builder, body_block);
            //let body_value = compile_node(data, func, unwind, scope.clone(), body);
            compile_node(data, func, unwind, scope.clone(), body);
            LLVMBuildBr(data.builder, before_block);

            LLVMPositionBuilderAtEnd(data.builder, after_block);
            //let phi = LLVMBuildPhi(data.builder, LLVMTypeOf(body_value), label("whileend"));

            //let mut values = vec![body_value];
            //let mut blocks = vec![before_block];

            //LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), 1);
            //phi
            //body_value
            //zero_int(data)
            null_value(str_type(data))
        },

        AST::For(_, ref name, ref list, ref body, ref id) => {
            let lscope = data.map.get(id);

            let list_value = compile_node(data, func, unwind, scope.clone(), list);
            let inc = LLVMBuildAlloca(data.builder, int_type(data), label("inc"));
            LLVMBuildStore(data.builder, zero_int(data), inc);

            let itype = get_type(data, lscope.clone(), lscope.borrow().get_variable_type(name).unwrap(), true);
            let item = LLVMBuildAlloca(data.builder, itype, label("item"));
            lscope.borrow_mut().assign(name, item);

            let before_block = LLVMAppendBasicBlockInContext(data.context, func, label("for"));
            let body_block = LLVMAppendBasicBlockInContext(data.context, func, label("forbody"));
            let after_block = LLVMAppendBasicBlockInContext(data.context, func, label("forend"));

            LLVMBuildBr(data.builder, before_block);
            LLVMPositionBuilderAtEnd(data.builder, before_block);
            //let cond_value = compile_node(data, func, unwind, scope.clone(), cond);

            let cond_value = LLVMBuildLoad(data.builder, inc, label("tmp"));
            let cond_length = build_call(data, "List_len", &mut vec!(list_value));
            let is_end = LLVMBuildICmp(data.builder, llvm::LLVMIntPredicate::LLVMIntSGE, cond_value, cond_length, label("is_end"));
            LLVMBuildCondBr(data.builder, is_end, after_block, body_block);

            LLVMPositionBuilderAtEnd(data.builder, body_block);
            let nextitem = build_cast_from_vartype(data, build_call(data, "List_get", &mut vec!(list_value, cond_value)), itype);
            LLVMBuildStore(data.builder, nextitem, item);

            let body_value = compile_node(data, func, unwind, lscope.clone(), body);

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
            body_value
        },

        AST::List(ref pos, ref items) => {
            // TODO this is kinda wrong, since you're passing it without type params
            let list = build_call(data, "List_new", &mut vec!(compile_node(data, func, unwind, scope.clone(), &AST::New(pos.clone(), (String::from("List"), vec!())))));
            for item in items {
                let value = build_cast_to_vartype(data, compile_node(data, func, unwind, scope.clone(), item));
                build_call(data, "List_push", &mut vec!(list, value));
            }
            list
        },


        AST::New(_, (ref name, ref types)) => {
            let classdef = scope.borrow().get_class_def(name);
            let value = classdef.borrow().search(&String::from("__alloc__"), |sym| sym.value.clone());
            let object = if let Some(function) = value {
                let mut largs = vec!();
                LLVMBuildCall(data.builder, function, largs.as_mut_ptr(), largs.len() as u32, label("tmp"))
            } else {
                //panic!("InternalError: no __alloc__ method for {}", name);
                let value = scope.borrow().get_type_value(name).unwrap();
                let mem = LLVMBuildMalloc(data.builder, LLVMGetElementType(value.value), label("ptr"));
                let object = LLVMBuildPointerCast(data.builder, mem, value.value, label("ptr"));
                if let Some(index) = value.structdef.iter().position(|ref r| r.0.as_str() == "__vtable__") {
                    let vtable = LLVMGetNamedGlobal(data.module, label(format!("__{}_vtable", name).as_str()));
                    let mut indices = vec!(i32_value(data, 0), i32_value(data, index));
                    let pointer = LLVMBuildGEP(data.builder, object, indices.as_mut_ptr(), indices.len() as u32, label("tmp"));
                    LLVMBuildStore(data.builder, vtable, pointer);
                }
                object
            };

            //let init = classdef.borrow().search(&String::from("__init__"), |sym| sym.value.clone());
            //let mut largs = vec!(object);
            //LLVMBuildCall(data.builder, init, largs.as_mut_ptr(), largs.len() as u32, label("tmp"));
            object
        },

        AST::Class(_, (ref name, ref types), ref parent, ref body, ref id) => {
            //let tscope = data.map.get(id);
            //let classdef = scope.borrow().get_class_def(name);

            // TODO you still need to compile the body of the func, if you're going to allow that... like an init function
            //compile_vec(data, func, unwind, tscope.clone(), body);

            //zero_int(data)
            null_value(str_type(data))
        },

        AST::Resolver(ref pos, ref left, ref right) => {
            match **left {
                AST::Identifier(_, ref name) => {
                    let classdef = scope.borrow().get_class_def(name);
                    let value = classdef.borrow().get_variable_value(right);
                    value.unwrap()
                },
                _ => panic!("SyntaxError:{:?}: left-hand side of scope resolver must be identifier", pos)
            }
        },

        AST::Accessor(_, ref left, ref right, ref ltype) => {
            let object = compile_node(data, func, unwind, scope.clone(), left);

            let name = ltype.clone().unwrap().get_name().unwrap();
            let classdef = scope.borrow().get_class_def(&name);
            //let sym = classdef.borrow().search(right, |sym| Some(sym.clone())).unwrap();
            let value = classdef.borrow().get_variable_value(right);
            debug!("*ACCESS: {:?} {:?}", right, classdef);
            // TODO add a check for vtables
            let typeval = scope.borrow().get_type_value(&name).unwrap();

            if struct_has_member(scope.clone(), &name, right) {
                let pointer = build_struct_access(data, scope.clone(), object, &name, right);
                LLVMBuildLoad(data.builder, pointer, label("tmp"))
            } else if let Some(index) = typeval.vtable.iter().position(|ref r| r.0 == *right) {
                let vindex = typeval.structdef.iter().position(|ref r| r.0.as_str() == "__vtable__").unwrap();
                let mut indices = vec!(i32_value(data, 0), i32_value(data, vindex));
                let pointer = LLVMBuildGEP(data.builder, object, indices.as_mut_ptr(), indices.len() as u32, label("tmp"));
                let vtable = LLVMBuildLoad(data.builder, pointer, label("tmp"));

                let mut indices = vec!(i32_value(data, 0), i32_value(data, index));
                let pointer = LLVMBuildGEP(data.builder, vtable, indices.as_mut_ptr(), indices.len() as u32, label("tmp"));
                LLVMBuildLoad(data.builder, pointer, label("tmp"))
            } else {
                value.unwrap()
            }
        },

        AST::Assignment(_, ref left, ref right) => {
            let value = compile_node(data, func, unwind, scope.clone(), right);
            match **left {
                AST::Accessor(_, ref left, ref right, ref ltype) => {
                    let name = ltype.clone().unwrap().get_name().unwrap();
                    let object = compile_node(data, func, unwind, scope.clone(), left);
                    let pointer = build_struct_access(data, scope.clone(), object, &name, right);
                    LLVMBuildStore(data.builder, value, pointer)
                },
                _ => panic!("???"),
            };
            value
        },

        AST::Import(ref pos, ref name, _) => {
            let module_init_name = format!("init.{}", name);
            if LLVMGetNamedFunction(data.module, label(module_init_name.as_str())).is_null() {
                let ftype = LLVMFunctionType(bool_type(data), &mut [].as_mut_ptr(), 0, false as i32);
                LLVMAddFunction(data.module, label(module_init_name.as_str()), ftype);
            }
            compile_node(data, func, None, scope.clone(), &AST::Invoke(pos.clone(), Box::new(AST::Identifier(pos.clone(), String::from(module_init_name))), vec!(), Some(Type::Function(vec!(), Box::new(Type::Object(String::from("Bool"), vec!()))))));
            ptr::null_mut()
        },

        AST::Declare(_, _, _, _) => { zero_int(data) },

        AST::Type(_, _, _) => panic!("NotImplementedError: not yet supported, {:?}", node),

        AST::Underscore |
        AST::Index(_, _, _, _) => panic!("InternalError: ast element shouldn't appear at this late phase: {:?}", node),
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
        AST::Function(ref pos, ref name, ref args, ref rtype, ref body, ref id) => {
            let fscope = data.map.get(id);
            let fname = scope.borrow().get_full_name(name, id.clone());

            let ftype = get_type(data, scope.clone(), Type::Function(args.iter().map(|t| t.1.clone().unwrap()).collect(), Box::new(rtype.clone().unwrap())), false);
            let function = LLVMAddFunction(data.module, label(fname.as_str()), ftype);
            //LLVMSetGC(function, label("shadow-stack"));
            //LLVMSetPersonalityFn(function, LLVMGetNamedFunction(data.module, label("__gxx_personality_v0")));

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
                LLVMSetValueName(llarg, label(name.as_str()));
                fscope.borrow_mut().assign(name, llarg);
            }

            collect_functions_node(data, fscope, body);
            data.functions.push(node);
            return Some(function);
        },

        AST::List(_, ref items) => { collect_functions_vec(data, scope, items); },

        AST::Invoke(_, _, ref args, _) => { collect_functions_vec(data, scope, args); },

        AST::SideEffect(_, _, ref args) => { collect_functions_vec(data, scope, args); },

        AST::Definition(_, (ref name, _), ref value) => {
            //collect_functions_node(data, scope, value);
            if let Some(function) = collect_functions_node(data, scope.clone(), value) {
                let dscope = Scope::target(scope.clone());
                dscope.borrow_mut().assign(name, function);
            }
        },

        AST::Declare(_, ref name, ref ttype, _) => {
            if let &Type::Function(_, _) = ttype {
                let fname = scope.borrow().get_full_name(&Some(name.clone()), UniqueID(0));
                let function = LLVMAddFunction(data.module, label(fname.as_str()), get_type(data, scope.clone(), ttype.clone(), false));

                let dscope = Scope::target(scope.clone());
                dscope.borrow_mut().assign(name, function);
            }
        },

        AST::Block(_, ref body) => { collect_functions_vec(data, scope, body); },

        AST::If(_, ref cond, ref texpr, ref fexpr) => {
            collect_functions_node(data, scope.clone(), cond);
            collect_functions_node(data, scope.clone(), texpr);
            collect_functions_node(data, scope, fexpr);
        },

        AST::Raise(_, ref expr) => { collect_functions_node(data, scope, expr); },

        AST::Try(_, ref cond, ref cases) |
        AST::Match(_, ref cond, ref cases) => {
            collect_functions_node(data, scope.clone(), cond);
            for case in cases {
                collect_functions_node(data, scope.clone(), &case.0);
                collect_functions_node(data, scope.clone(), &case.1);
            }
        },

        AST::For(_, _, ref cond, ref body, ref id) => {
            let lscope = data.map.get(id);
            collect_functions_node(data, lscope.clone(), cond);
            collect_functions_node(data, lscope.clone(), body);
        },

        AST::While(_, ref cond, ref body) => {
            collect_functions_node(data, scope.clone(), cond);
            collect_functions_node(data, scope.clone(), body);
        },

        AST::New(_, _) => { },

        AST::Class(ref pos, (ref name, ref types), ref parent, ref body, ref id) => {
            let tscope = data.map.get(id);

            let (mut structdef, mut vtable) = if let Some((ref name, ref types)) = *parent {
                let value = scope.borrow().get_type_value(name).unwrap();
                (value.structdef, value.vtable)
            } else {
                (vec!(), vec!())
            };

            // Build vtable for class
            let classdef = scope.borrow().get_class_def(name);
            let parent = classdef.borrow().get_parent().unwrap_or(Scope::new_ref(None));
if name.as_str() != "String" {
            for ref node in body.iter() {
                match **node {
                    AST::Function(_, ref fname, ref args, ref rtype, _, _) => {
if fname.clone().unwrap().as_str() != "new" {
debug!("***************: {:?}:{:?}", name, fname);
                        if fname.is_some() && parent.borrow().contains(fname.as_ref().unwrap()) {
                            vtable.push((fname.clone().unwrap(), Type::Function(args.iter().map(|t| t.1.clone().unwrap()).collect(), Box::new(rtype.clone().unwrap()))));
                        }
}
                    },
                    AST::Declare(_, ref fname, ref ttype, _) => {
                        match *ttype {
                            Type::Function(_, _) => {
if fname.as_str() != "new" {
debug!("+++++++++++++++: {:?}:{:?}", name, fname);
                                if parent.borrow().contains(fname) {
                                    vtable.push((fname.clone(), ttype.clone()))
                                }
}
                            },
                            _ => { },
                        }
                    },
                    _ => { }
                }
            }
}

            // Build struct definition for class
            if vtable.len() > 0 {
                if let Some(index) = structdef.iter().position(|ref r| r.0.as_str() == "__vtable__") {
                    structdef[index].1 = Type::Object(format!("{}_vtable", name), vec!());
                } else {
                    structdef.push((String::from("__vtable__"), Type::Object(format!("{}_vtable", name), vec!())));
                }
            }
            for ref node in body.iter() {
                match **node {
                    AST::Definition(_, (ref name, ref ttype), ref value) => {
                        structdef.push((name.clone(), ttype.clone().unwrap()));
                    },
                    _ => { }
                }
            }

            let lltype = build_class_type(data, scope.clone(), name, structdef, vtable);

            //let alloc = String::from("__alloc__");
            //let classdef = scope.borrow().get_class_def(name);
            //if !classdef.borrow().contains_local(&alloc) {
            //    debug!("******* CREATING ALLOC: {}", name);
            //    let cname = scope.borrow().get_full_name(&Some(name.clone()), id.clone());
            //    classdef.borrow_mut().define(alloc.clone(), Some(Type::Function(vec!(), Box::new(Type::Object(name.clone(), types.clone())))));
            //    classdef.borrow_mut().assign(&alloc, build_allocator(data, tscope.clone(), &name, format!("{}_{}", cname, alloc).as_str(), lltype));
            //}
            collect_functions_vec(data, tscope.clone(), body);
            data.classes.push(node);
        },

        AST::Index(_, ref left, ref right, _) => {
            collect_functions_node(data, scope.clone(), left);
            collect_functions_node(data, scope.clone(), right);
        },

        AST::Resolver(_, ref left, ref right) => {
            collect_functions_node(data, scope.clone(), left);
        },

        AST::Accessor(_, ref left, ref right, _) => {
            collect_functions_node(data, scope.clone(), left);
        },

        AST::Assignment(_, ref left, ref right) => {
            collect_functions_node(data, scope.clone(), right);
        },

        AST::Import(_, _, ref decls) => {
            collect_functions_vec(data, scope.clone(), decls);
        },

        AST::Type(_, _, _) => panic!("NotImplementedError: not yet supported, {:?}", node),

        AST::Identifier(_, _) |
        AST::Noop | AST::Underscore | AST::Nil(_) |
        AST::Boolean(_) | AST::Integer(_) | AST::Real(_) | AST::String(_) => { }
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
    if LLVMGetTypeKind(LLVMTypeOf(value)) == llvm::LLVMTypeKind::LLVMPointerTypeKind {
        if LLVMGetTypeKind(ltype) == llvm::LLVMTypeKind::LLVMPointerTypeKind {
            LLVMBuildPointerCast(data.builder, value, ltype, label("ptr"))
        } else {
            LLVMBuildPtrToInt(data.builder, value, ltype, label("ptr"))
        }
    } else {
        if LLVMGetTypeKind(ltype) == llvm::LLVMTypeKind::LLVMPointerTypeKind {
            LLVMBuildIntToPtr(data.builder, value, ltype, label("ptr"))
        } else {
            panic!("I HAVEN'T DONE THIS");
        }
    }
}

pub unsafe fn build_cast_to_vartype(data: &LLVM, value: LLVMValueRef) -> LLVMValueRef {
    if LLVMGetTypeKind(LLVMTypeOf(value)) == llvm::LLVMTypeKind::LLVMPointerTypeKind {
        LLVMBuildPointerCast(data.builder, value, str_type(data), label("ptr"))
    } else {
        LLVMBuildIntToPtr(data.builder, value, str_type(data), label("ptr"))
    }
}

pub unsafe fn build_cast_from_vartype(data: &LLVM, value: LLVMValueRef, ltype: LLVMTypeRef) -> LLVMValueRef {
    if LLVMGetTypeKind(ltype) == llvm::LLVMTypeKind::LLVMPointerTypeKind {
        LLVMBuildPointerCast(data.builder, value, ltype, label("ptr"))
    } else {
        LLVMBuildPtrToInt(data.builder, value, ltype, label("ptr"))
    }
}


//pub unsafe fn get_attribute(data: &LLVM, name: &str) -> LLVMAttributeRef {
//    let kind = LLVMGetEnumAttributeKindForName(label(name), name.len());
//    LLVMCreateEnumAttribute(data.context, kind, 0)
//}

pub unsafe fn build_function_start(data: &LLVM, name: &str, mut args: Vec<LLVMTypeRef>, return_type: LLVMTypeRef) -> LLVMValueRef {
    let ftype = LLVMFunctionType(return_type, args.as_mut_ptr(), args.len() as u32, false as i32);
    let function = LLVMAddFunction(data.module, label(name), ftype);

    let nargs = LLVMCountParams(function) as usize;
    if nargs != 0 && nargs != args.len() {
        panic!("ArgsError: argument counts don't match");
    }

    // TODO maybe these shouldn't be here, but it causes problems for library functions without it
    let bb = LLVMAppendBasicBlockInContext(data.context, function, label("entry"));
    LLVMPositionBuilderAtEnd(data.builder, bb);

    function
}

unsafe fn build_function_body(data: &LLVM, node: &AST) {
    if let AST::Function(_, ref name, _, _, ref body, ref id) = *node {
        let fscope = data.map.get(id);
        let pscope = fscope.borrow().parent.clone().unwrap();
        let fname = pscope.borrow().get_full_name(name, id.clone());
        let function = LLVMGetNamedFunction(data.module, label(fname.as_str()));

        let bb = LLVMAppendBasicBlockInContext(data.context, function, label("entry"));
        LLVMPositionBuilderAtEnd(data.builder, bb);
        let ret = compile_node(data, function, None, fscope.clone(), body);
        LLVMBuildRet(data.builder, ret);

        //if llvm::analysis::LLVMVerifyFunction(function, llvm::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction) != 0 {
        //    panic!("VerifyError: verification failed");
        //}
        //LLVMRunFunctionPassManager(data.funcpass, function);
    }
}

pub unsafe fn build_call(data: &LLVM, name: &str, largs: &mut Vec<LLVMValueRef>) -> LLVMValueRef {
    let function = LLVMGetNamedFunction(data.module, label(name));
    LLVMBuildCall(data.builder, function, largs.as_mut_ptr(), largs.len() as u32, label("tmp"))
}

/*
pub unsafe fn build_allocator(data: &LLVM, scope: ScopeRef<Value, TypeValue>, cname: &String, fname: &str, lltype: LLVMTypeRef) -> LLVMValueRef {
    let function = build_function_start(data, fname, vec!(), lltype);
    //let obj = build_malloc(data, LLVMSizeOf(LLVMGetElementType(lltype)));
    let mem = LLVMBuildMalloc(data.builder, LLVMGetElementType(lltype), label("ptr"));
    let object = LLVMBuildPointerCast(data.builder, mem, lltype, label("ptr"));

    let structdef = scope.borrow().get_type_value(&cname).unwrap().structdef;
    //for &(name, ttype, expr) in &structdef {
    //    let value = compile_node(data, function, None, scope.clone(), &expr);
    //    let pointer = build_struct_access(data, scope.clone(), object, cname, &name);
    //    LLVMBuildStore(data.builder, pointer, value);
    //}

    LLVMBuildRet(data.builder, object);
    function
}
*/

pub unsafe fn vtable_has_member(scope: ScopeRef<Value, TypeValue>, typename: &String, field: &String) -> bool {
    scope.borrow().search_type(typename, |info| {
        if info.value.as_ref().unwrap().vtable.iter().position(|ref r| r.0 == *field).is_some() {
            Some(true)
        } else {
            Some(false)
        }
    }).unwrap_or(false)
}

pub unsafe fn struct_has_member(scope: ScopeRef<Value, TypeValue>, typename: &String, field: &String) -> bool {
    scope.borrow().search_type(typename, |info| {
        if info.value.as_ref().unwrap().structdef.iter().position(|ref r| r.0 == *field).is_some() {
            Some(true)
        } else {
            Some(false)
        }
    }).unwrap_or(false)
}

pub unsafe fn build_struct_access(data: &LLVM, scope: ScopeRef<Value, TypeValue>, object: LLVMValueRef, typename: &String, field: &String) -> LLVMValueRef {
    let structdef = scope.borrow().get_type_value(&typename).unwrap().structdef;
    let index = structdef.iter().position(|ref r| r.0 == *field).unwrap();
    let mut indices = vec!(i32_value(data, 0), i32_value(data, index));
    LLVMBuildGEP(data.builder, object, indices.as_mut_ptr(), indices.len() as u32, label("tmp"))
}

pub unsafe fn build_class_type(data: &LLVM, scope: ScopeRef<Value, TypeValue>, name: &String, structdef: Vec<(String, Type)>, vtable: Vec<(String, Type)>) -> LLVMTypeRef {
    let (vttype, pvttype) = if vtable.len() > 0 {
        let vtname = format!("{}_vtable", name);
        let vttype = LLVMStructCreateNamed(data.context, label(vtname.as_str()));
        let pvttype = LLVMPointerType(vttype, 0);
        scope.borrow_mut().define_type(vtname.clone(), Type::Object(vtname.clone(), vec!()));
        scope.borrow_mut().set_type_value(&vtname, TypeValue { structdef: vtable.clone(), value: pvttype, vtable: vec!(), vttype: None });
        (Some(vttype), Some(pvttype))
    } else {
        (None, None)
    };
    let lltype = LLVMStructCreateNamed(data.context, label(name));
    let pltype = LLVMPointerType(lltype, 0);
    scope.borrow_mut().set_type_value(name, TypeValue { structdef: structdef.clone(), value: pltype, vtable: vtable.clone(), vttype: pvttype });

    let mut types = vec!();
    for &(_, ref ttype) in &structdef {
        types.push(get_type(data, scope.clone(), ttype.clone(), true))
    }
    LLVMStructSetBody(lltype, types.as_mut_ptr(), types.len() as u32, false as i32);

    if let Some(vttype) = vttype {
        let mut types = vec!();
        for &(_, ref ttype) in &vtable {
            types.push(get_type(data, scope.clone(), ttype.clone(), true))
        }
        LLVMStructSetBody(vttype, types.as_mut_ptr(), types.len() as u32, false as i32);
    }

    pltype
}

pub unsafe fn get_type(data: &LLVM, scope: ScopeRef<Value, TypeValue>, ttype: Type, use_fptrs: bool) -> LLVMTypeRef {
    match ttype {
        Type::Object(ref tname, ref ptypes) => match tname.as_str() {
            "Nil" => str_type(data),
            "Bool" => bool_type(data),
            "Byte" => LLVMInt8TypeInContext(data.context),
            "Int" => int_type(data),
            "Real" => real_type(data),
            "String" => str_type(data),
            "Buffer" => ptr_type(data),

            _ => match scope.borrow().get_type_value(tname) {
                Some(typedata) => typedata.value,
                // TODO this should panic...  but Nil doesn't have a value (because it needs to know the type of null pointer it should be)
                //None => LLVMInt64TypeInContext(data.context),
                None => panic!("CompileError: unassigned type value, {:?}", tname),
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
        //Type::Variable(_, _) => LLVMInt64TypeInContext(data.context),
        Type::Variable(_, _) => str_type(data), //ptr_type(data),
        _ => panic!("InvalidType: cannot convert to llvm, {:?}", ttype),
    }
}

