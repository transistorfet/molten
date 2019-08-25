
use std::ptr;
use std::ffi::CString;
use std::cell::RefCell;
use std::collections::HashMap;

use std::fs::File;
use std::io::prelude::*;

use std::os::raw::c_uint;


extern crate llvm_sys;
use self::llvm_sys::*;
use self::llvm_sys::prelude::*;
use self::llvm_sys::core::*;
use self::llvm_sys::target::*;
use self::llvm_sys::target_machine::*;
use self::llvm_sys::transforms::pass_manager_builder::*;


use ast::NodeID;
use config::Options;
use session::Session;
use misc::{ UniqueID, r };

use llvm::llcode::{ LLType, LLLit, LLRef, LLCmpType, LLLink, LLCC, LLExpr, LLGlobal, LLBlock };


#[derive(Clone)]
pub struct LLVM<'sess> {
    pub session: &'sess Session,
    pub context: LLVMContextRef,
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,
    pub target: RefCell<LLVMTargetRef>,
    pub target_machine: RefCell<LLVMTargetMachineRef>,
    pub target_data: RefCell<LLVMTargetDataRef>,
    pub values: RefCell<HashMap<UniqueID, LLVMValueRef>>,
    pub types: RefCell<HashMap<UniqueID, LLVMTypeRef>>,
    pub curfunc: RefCell<LLVMValueRef>,
}



pub const TYPEVAR_ID: NodeID = UniqueID(2);
pub const EXCEPTION_ID: NodeID = UniqueID(3);

pub fn cstr(string: &str) -> *mut i8 {
    CString::new(string).unwrap().into_raw()
}

pub fn cstring(string: &String) -> *mut i8 {
    CString::new(string.as_str()).unwrap().into_raw()
}


impl<'sess> Drop for LLVM<'sess> {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.context);
        }
    }
}

impl<'sess> LLVM<'sess> {
    pub fn new(session: &Session) -> LLVM {
        unsafe {
            let context = LLVMContextCreate();
            let module = LLVMModuleCreateWithName(cstring(&session.name));
            let builder = LLVMCreateBuilderInContext(context);

            LLVM {
                session: session,
                context: context,
                module: module,
                builder: builder,
                target: RefCell::new(ptr::null_mut()),
                target_machine: RefCell::new(ptr::null_mut()),
                target_data: RefCell::new(ptr::null_mut()),
                values: RefCell::new(HashMap::new()),
                types: RefCell::new(HashMap::new()),
                curfunc: RefCell::new(ptr::null_mut()),
            }
        }
    }

    pub fn initialize(&self) {
        unsafe {
            self.initialize_target();

            self.set_type(TYPEVAR_ID, LLVMPointerType(LLVMStructCreateNamed(self.context, cstr("TypeVar")), 0));

            // TODO this buffer size is tricky... I had it set for 8 but got a bunch of segfaults in the testsuite.
            //      The correct value is probably platform specific but I don't know how to find that out.  Oddly enough
            //      it would work for some programs but some would segfault
            let jmpbuf = LLVMStructCreateNamed(self.context, cstr("JmpBuf"));

            // TODO either just store the data or also allocate space for a return value in the expoint
            LLVMStructSetBody(jmpbuf, vec!(LLVMArrayType(self.i64_type(), 10)).as_mut_ptr(), 1, 0);
            //let mut body = vec!(LLVMArrayType(self.i64_type(), 10), self.get_type(TYPEVAR_ID).unwrap());
            //LLVMStructSetBody(jmpbuf, body.as_mut_ptr(), body.len() as u32, 0);

            self.set_type(EXCEPTION_ID, jmpbuf);
        }
    }

    pub fn initialize_target(&self) {
        unsafe {
            LLVM_InitializeAllTargetInfos();
            LLVM_InitializeAllTargets();
            LLVM_InitializeAllTargetMCs();
            LLVM_InitializeAllAsmParsers();
            LLVM_InitializeAllAsmPrinters();

            let target_triple = LLVMGetDefaultTargetTriple();
            LLVMSetTarget(self.module, target_triple);

            let target: *mut LLVMTargetRef = Box::into_raw(Box::new(ptr::null_mut()));
            let err_msg: *mut *mut i8 = Box::into_raw(Box::new(ptr::null_mut()));
            if LLVMGetTargetFromTriple(target_triple, target, err_msg) != 0 {
                let err = CString::from_raw(*err_msg.as_ref().unwrap());
                eprintln!("Get LLVM target failed");
                eprintln!("{}", err.to_str().unwrap());
                panic!();
            }
            LLVMDisposeMessage(*err_msg.as_ref().unwrap());

            *self.target.borrow_mut() = *target.as_ref().unwrap();
            *self.target_machine.borrow_mut() = LLVMCreateTargetMachine(*self.target.borrow(), target_triple, cstr("generic"), cstr(""), LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault, LLVMRelocMode::LLVMRelocDefault, LLVMCodeModel::LLVMCodeModelDefault);
            *self.target_data.borrow_mut() = LLVMCreateTargetDataLayout(*self.target_machine.borrow());
            LLVMSetModuleDataLayout(self.module, *self.target_data.borrow());
        }
    }

    pub fn build_module(&self, globals: &Vec<LLGlobal>) {
        unsafe {
            self.build_declarations(globals);
            self.build_definitions(globals);
        }
    }

    pub fn emit_module(&self) -> String {
        unsafe { CString::from_raw(LLVMPrintModuleToString(self.module)).into_string().unwrap() }
    }

    pub fn print_module(&self) {
        if Options::as_ref().debug {
            println!("{}\n", self.emit_module());
        }
    }

    pub fn write_module(&self, filename: &str) {
        let mut file = File::create(filename).unwrap();
        file.write_all(self.emit_module().as_bytes()).unwrap();
    }

    pub fn optimize(&self, llvm_opt: u32) {
        unsafe {
            let builder = LLVMPassManagerBuilderCreate();
            // -O optimization level
            LLVMPassManagerBuilderSetOptLevel(builder, llvm_opt);

            let pass_manager = LLVMCreatePassManager();
            LLVMPassManagerBuilderPopulateModulePassManager(builder, pass_manager);
            LLVMPassManagerBuilderDispose(builder);

            LLVMRunPassManager(pass_manager, self.module);

            LLVMDisposePassManager(pass_manager);
        }
    }

    pub fn write_object_file(&self, filename: &str) {
        unsafe {
            let err_msg: *mut *mut i8 = Box::into_raw(Box::new(ptr::null_mut()));
            if LLVMTargetMachineEmitToFile(*self.target_machine.borrow(), self.module, cstr(filename), LLVMCodeGenFileType::LLVMObjectFile, err_msg) != 0 {
                let err = CString::from_raw(*err_msg.as_ref().unwrap());
                eprintln!("Emit to object file failed");
                eprintln!("{}", err.to_str().unwrap());
                panic!();
            }
        }
    }

    pub fn set_value(&self, id: UniqueID, value: LLVMValueRef) {
        debug!(">>>>>>>>> SET VALUE: {:?} = {:?} (existing value {:?})", id, value, self.values.borrow().get(&id));
        self.values.borrow_mut().insert(id, value);
    }

    pub fn get_value(&self, id: UniqueID) -> Option<LLVMValueRef> {
        debug!("<<<<<<<<< GET VALUE: {:?} = {:?}", id, self.values.borrow().get(&id).cloned());
        self.values.borrow().get(&id).cloned()
    }

    pub fn set_type(&self, id: UniqueID, value: LLVMTypeRef) {
        self.types.borrow_mut().insert(id, value);
    }

    pub fn get_type(&self, id: UniqueID) -> Option<LLVMTypeRef> {
        self.types.borrow().get(&id).cloned()
    }


    pub unsafe fn void_type(&self) -> LLVMTypeRef {
        LLVMVoidTypeInContext(self.context)
    }

    pub unsafe fn i1_type(&self) -> LLVMTypeRef {
        LLVMInt1TypeInContext(self.context)
    }

    pub unsafe fn i8_type(&self) -> LLVMTypeRef {
        LLVMInt8TypeInContext(self.context)
    }

    pub unsafe fn i32_type(&self) -> LLVMTypeRef {
        LLVMInt32TypeInContext(self.context)
    }

    pub unsafe fn i64_type(&self) -> LLVMTypeRef {
        LLVMInt64TypeInContext(self.context)
    }

    pub unsafe fn f64_type(&self) -> LLVMTypeRef {
        LLVMDoubleTypeInContext(self.context)
    }

    pub unsafe fn str_type(&self) -> LLVMTypeRef {
        LLVMPointerType(LLVMInt8TypeInContext(self.context), 0)
    }

    pub unsafe fn ptr_type(&self) -> LLVMTypeRef {
        LLVMPointerType(LLVMPointerType(LLVMInt8TypeInContext(self.context), 0), 0)
    }

    pub unsafe fn ptr_of(&self, etype: LLVMTypeRef) -> LLVMTypeRef {
        LLVMPointerType(etype, 0)
    }

    pub unsafe fn tvar_type(&self) -> LLVMTypeRef {
        self.get_type(TYPEVAR_ID).unwrap()
    }

    pub unsafe fn exp_type(&self) -> LLVMTypeRef {
        self.get_type(EXCEPTION_ID).unwrap()
    }

    pub unsafe fn exp_ref_type(&self) -> LLVMTypeRef {
        LLVMPointerType(self.get_type(EXCEPTION_ID).unwrap(), 0)
    }

    pub unsafe fn cfunc_type(&self, args: &Vec<LLType>, ret: &LLType) -> LLVMTypeRef {
        let mut argtypes = vec!();
        for ltype in args {
            argtypes.push(self.build_type(&ltype));
        }
        let rettype = self.build_type(ret);
        LLVMFunctionType(rettype, argtypes.as_mut_ptr(), argtypes.len() as u32, false as i32)
    }

    pub unsafe fn struct_type(&self, etypes: &Vec<LLType>) -> LLVMTypeRef {
        let mut rtypes = vec!();
        for ltype in etypes {
            rtypes.push(self.build_type(ltype));
        }
        LLVMStructType(rtypes.as_mut_ptr(), rtypes.len() as u32, false as i32)
    }

    pub unsafe fn array_type(&self, itype: &LLType, size: usize) -> LLVMTypeRef {
        LLVMArrayType(self.build_type(itype), size as u32)
    }

    pub unsafe fn build_type(&self, ltype: &LLType) -> LLVMTypeRef {
        match ltype {
            LLType::Void => self.void_type(),
            LLType::I1 => self.i1_type(),
            LLType::I8 => self.i8_type(),
            LLType::I32 => self.i32_type(),
            LLType::I64 => self.i64_type(),
            LLType::F64 => self.f64_type(),
            LLType::Var => self.tvar_type(),
            LLType::ExceptionPoint => self.exp_type(),
            LLType::Ptr(etype) => LLVMPointerType(self.build_type(etype), 0),
            LLType::Struct(etypes) => self.struct_type(etypes),
            LLType::Array(etype, size) => self.array_type(etype, *size),
            LLType::Function(argtypes, rettype) => self.cfunc_type(argtypes, rettype),
            LLType::Alias(id) => self.get_type(*id).unwrap(),
            LLType::Largest(types) => {
                let mut largest = 0;
                for ltype in types {
                    let size = LLVMStoreSizeOfType(*self.target_data.borrow(), self.build_type(ltype));
                    debug!("SIZE of {:#?}: {:?}", ltype, largest);
                    largest = if size > largest { size } else { largest }
                }
                //self.build_type(&LLType::Array(r(LLType::I64), (largest / 8) as usize))
                let ptrsize = LLVMPointerSize(*self.target_data.borrow());
                LLVMArrayType(LLVMIntType(ptrsize * 8), (largest as u32) / ptrsize)
            }
        }
    }

    pub unsafe fn build_cast(&self, rtype: LLVMTypeRef, value: LLVMValueRef) -> LLVMValueRef {
        if rtype != LLVMTypeOf(value) {
            let sourcekind = LLVMGetTypeKind(LLVMTypeOf(value));
            let destkind = LLVMGetTypeKind(rtype);
            debug!("{:?} -> {:?}", sourcekind, destkind);

            match (sourcekind, destkind) {
                (LLVMTypeKind::LLVMPointerTypeKind, LLVMTypeKind::LLVMPointerTypeKind) =>
                    LLVMBuildPointerCast(self.builder, value, rtype, cstr("ptr")),

                //(LLVMTypeKind::LLVMPointerTypeKind, _) =>
                //    LLVMBuildPtrToInt(self.builder, value, rtype, cstr("ptr")),
                //(_, LLVMTypeKind::LLVMPointerTypeKind) =>
                //    LLVMBuildIntToPtr(self.builder, value, rtype, cstr("ptr")),

                (LLVMTypeKind::LLVMPointerTypeKind, _) => {
                    let reference = LLVMBuildPointerCast(self.builder, value, LLVMPointerType(rtype, 0), cstr(""));
                    LLVMBuildLoad(self.builder, reference, cstr(""))
                },
                (_, LLVMTypeKind::LLVMPointerTypeKind) => {
                    let boxed = self.build_boxed(LLVMPointerType(LLVMTypeOf(value), 0), value);
                    LLVMBuildPointerCast(self.builder, boxed, rtype, cstr(""))
                },

                (LLVMTypeKind::LLVMStructTypeKind, LLVMTypeKind::LLVMStructTypeKind) => {
                    let pointer = LLVMBuildAlloca(self.builder, LLVMTypeOf(value), cstr(""));
                    LLVMBuildStore(self.builder, value, pointer);
                    LLVMBuildLoad(self.builder, LLVMBuildBitCast(self.builder, pointer, LLVMPointerType(rtype, 0), cstr("ptr")), cstr(""))
                },

                (LLVMTypeKind::LLVMIntegerTypeKind, LLVMTypeKind::LLVMDoubleTypeKind) =>
                    LLVMBuildCast(self.builder, llvm_sys::LLVMOpcode::LLVMSIToFP, value, rtype, cstr("tmp")),
                (LLVMTypeKind::LLVMDoubleTypeKind, LLVMTypeKind::LLVMIntegerTypeKind) =>
                    LLVMBuildCast(self.builder, llvm_sys::LLVMOpcode::LLVMFPToSI, value, rtype, cstr("tmp")),
                (LLVMTypeKind::LLVMIntegerTypeKind, LLVMTypeKind::LLVMIntegerTypeKind) => {
                    if LLVMGetIntTypeWidth(LLVMTypeOf(value)) > LLVMGetIntTypeWidth(rtype) {
                        LLVMBuildCast(self.builder, llvm_sys::LLVMOpcode::LLVMTrunc, value, rtype, cstr("tmp"))
                    } else {
                        LLVMBuildCast(self.builder, llvm_sys::LLVMOpcode::LLVMZExt, value, rtype, cstr("tmp"))
                    }
                }
                _ =>
                    panic!("I HAVEN'T DONE THIS"),
            }
        } else {
            value
        }
    }

    pub unsafe fn cast_typevars(&self, rtype: LLVMTypeRef, value: LLVMValueRef) -> LLVMValueRef {
        let vtype = LLVMTypeOf(value);
        let ttype = self.get_type(TYPEVAR_ID).unwrap();

        if rtype != vtype && rtype == ttype || vtype == ttype {
            self.build_cast(rtype, value)
        } else {
            value
        }
    }

    pub unsafe fn i1_const(&self, num: bool) -> LLVMValueRef {
        LLVMConstInt(self.i1_type(), if num { 1 } else { 0 }, 0)
    }

    pub unsafe fn i8_const(&self, num: i8) -> LLVMValueRef {
        LLVMConstInt(self.i8_type(), num as u64, 0)
    }

    pub unsafe fn i32_const(&self, num: i32) -> LLVMValueRef {
        LLVMConstInt(self.i32_type(), num as u64, 0)
    }

    pub unsafe fn i64_const(&self, num: i64) -> LLVMValueRef {
        LLVMConstInt(self.i64_type(), num as u64, 0)
    }

    pub unsafe fn u64_const(&self, num: u64) -> LLVMValueRef {
        LLVMConstInt(self.i64_type(), num as u64, 0)
    }

    pub unsafe fn null_const(&self, ltype: LLVMTypeRef) -> LLVMValueRef {
        LLVMConstNull(ltype)
    }

    pub unsafe fn build_linkage(&self, val: LLVMValueRef, link: LLLink) {
        let linktype = match link {
            LLLink::Private => LLVMLinkage::LLVMInternalLinkage,
            LLLink::Public => LLVMLinkage::LLVMExternalLinkage,
            LLLink::Once => LLVMLinkage::LLVMLinkOnceAnyLinkage,
        };
        LLVMSetLinkage(val, linktype);
    }

    pub unsafe fn get_callconv(&self, cc: LLCC) -> LLVMCallConv {
        match cc {
            LLCC::CCC => LLVMCallConv::LLVMCCallConv,
            LLCC::FastCC => LLVMCallConv::LLVMFastCallConv,
        }
    }

    pub unsafe fn build_literal(&self, lit: &LLLit) -> LLVMValueRef {
        match lit {
            LLLit::I1(num) => LLVMConstInt(self.i1_type(), *num as u64, 0),
            LLLit::I8(num) => LLVMConstInt(self.i8_type(), *num as u64, 0),
            LLLit::I32(num) => LLVMConstInt(self.i32_type(), *num as u64, 0),
            LLLit::I64(num) => LLVMConstInt(self.i64_type(), *num as u64, 0),
            LLLit::F64(num) => LLVMConstReal(self.f64_type(), *num),
            LLLit::Null(ltype) => self.null_const(self.build_type(ltype)),
            LLLit::ConstStr(string) => LLVMBuildGlobalStringPtr(self.builder, cstr(string.as_str()), cstr("__string")),
        }
    }

    pub unsafe fn build_expr_block(&self, exprs: &Vec<LLExpr>) -> LLVMValueRef {
        let mut last = ptr::null_mut();
        for expr in exprs {
            last = self.build_expr(expr);
        }
        last
    }

    pub unsafe fn build_list(&self, exprs: &Vec<LLExpr>) -> Vec<LLVMValueRef> {
        let mut values = vec!();
        for expr in exprs {
            values.push(self.build_expr(expr));
        }
        values
    }

    pub unsafe fn build_args(&self, args: &Vec<LLExpr>, lftype: LLVMTypeRef) -> Vec<LLVMValueRef> {
        let mut values = self.build_list(args);

        let lftype = LLVMGetElementType(lftype);
        let mut ltypes = Vec::with_capacity(LLVMCountParamTypes(lftype) as usize);
        ltypes.set_len(LLVMCountParamTypes(lftype) as usize);
        LLVMGetParamTypes(lftype, ltypes.as_mut_ptr());

        for i in 0 .. ltypes.len() {
            values[i] = self.build_cast(ltypes[i], values[i]);
        }

        values
    }

    pub unsafe fn build_call_by_name(&self, name: &str, largs: &mut Vec<LLVMValueRef>) -> LLVMValueRef {
        let function = LLVMGetNamedFunction(self.module, cstr(name));
        LLVMBuildCall(self.builder, function, largs.as_mut_ptr(), largs.len() as u32, cstr(""))
    }

    pub unsafe fn build_def_local(&self, name: &str, rtype: LLVMTypeRef, initval: LLVMValueRef) -> LLVMValueRef {
        let pointer = LLVMBuildAlloca(self.builder, rtype, cstr(&name));
        LLVMBuildStore(self.builder, self.build_cast(rtype, initval), pointer);
        pointer
    }

    pub unsafe fn build_load(&self, pointer: LLVMValueRef) -> LLVMValueRef {
        LLVMBuildLoad(self.builder, pointer, cstr(""))
    }

    pub unsafe fn build_store(&self, pointer: LLVMValueRef, value: LLVMValueRef) -> LLVMValueRef {
        LLVMBuildStore(self.builder, self.cast_typevars(LLVMGetElementType(LLVMTypeOf(pointer)), value), pointer);
        value
    }

    pub unsafe fn build_access_refs(&self, refs: &Vec<LLRef>) -> Vec<LLVMValueRef> {
        let mut indices = vec!(self.i32_const(0));
        for lref in refs {
            match &lref {
                LLRef::Deref => indices.push(self.i32_const(0)),
                LLRef::Field(index) => indices.push(self.i32_const(*index as i32)),
                LLRef::Index(index) => indices.push(self.i32_const(*index as i32)),
            }
        }
        indices
    }

    pub unsafe fn build_access(&self, base: LLVMValueRef, mut indices: Vec<LLVMValueRef>) -> LLVMValueRef {
        LLVMBuildGEP(self.builder, base, indices.as_mut_ptr(), indices.len() as u32, cstr(""))
    }


    pub unsafe fn build_struct(&self, rtype: LLVMTypeRef, evalues: Vec<LLVMValueRef>) -> LLVMValueRef {
        let pointer = LLVMBuildAlloca(self.builder, rtype, cstr(""));
        for (index, value) in evalues.iter().enumerate() {
            let mut indices = vec!(self.i32_const(0), self.i32_const(index as i32));
            let field = LLVMBuildGEP(self.builder, pointer, indices.as_mut_ptr(), indices.len() as u32, cstr(""));
            LLVMBuildStore(self.builder, *value, field);
        }
        LLVMBuildLoad(self.builder, pointer, cstr(""))
    }

    pub unsafe fn build_get_item(&self, data: LLVMValueRef, index: usize) -> LLVMValueRef {
        LLVMBuildExtractValue(self.builder, data, index as u32, cstr(""))
    }

    pub unsafe fn build_set_item(&self, data: LLVMValueRef, index: usize, value: LLVMValueRef) -> LLVMValueRef {
        LLVMBuildInsertValue(self.builder, data, value, index as u32, cstr(""))
    }

    pub unsafe fn build_boxed(&self, rtype: LLVMTypeRef, value: LLVMValueRef) -> LLVMValueRef {
        let mem = self.build_call_by_name("molten_malloc", &mut vec!(LLVMSizeOf(LLVMGetElementType(rtype))));
        let reference = LLVMBuildPointerCast(self.builder, mem, rtype, cstr(""));
        LLVMBuildStore(self.builder, value, reference);
        reference
    }

    pub unsafe fn build_basic_blocks(&self, label: &str, conds: &Vec<LLBlock>, blocks: &Vec<LLBlock>) -> (Vec<LLVMValueRef>, Vec<*mut LLVMBasicBlock>) {
        let mut cond_vals = vec!();
        let mut block_vals = vec!();
        let mut return_vals = vec!();

        for _ in blocks {
            cond_vals.push(LLVMAppendBasicBlockInContext(self.context, *self.curfunc.borrow(), cstring(&format!("{}_cond", label))));
            block_vals.push(LLVMAppendBasicBlockInContext(self.context, *self.curfunc.borrow(), cstring(&format!("{}_block", label))));
        }
        let merge_block = LLVMAppendBasicBlockInContext(self.context, *self.curfunc.borrow(), cstring(&format!("{}_end", label)));
        cond_vals.push(merge_block);

        LLVMBuildBr(self.builder, cond_vals[0]);

        let mut last_cond = ptr::null_mut();
        for i in 0 .. blocks.len() {
            LLVMPositionBuilderAtEnd(self.builder, cond_vals[i]);
            let result = self.build_expr_block(&conds[i]);
            LLVMBuildCondBr(self.builder, result, block_vals[i], cond_vals[i + 1]);
            last_cond = LLVMGetInsertBlock(self.builder);

            LLVMPositionBuilderAtEnd(self.builder, block_vals[i]);
            return_vals.push(self.build_expr_block(&blocks[i]));
            LLVMBuildBr(self.builder, merge_block);
            block_vals[i] = LLVMGetInsertBlock(self.builder);
        }
        LLVMPositionBuilderAtEnd(self.builder, merge_block);
        let lltype = LLVMTypeOf(return_vals[0]);
        return_vals.push(self.null_const(lltype));
        block_vals.push(last_cond);

        (return_vals, block_vals)
    }

    pub unsafe fn build_phi(&self, values: &mut Vec<LLVMValueRef>, blocks: &mut Vec<*mut LLVMBasicBlock>) -> LLVMValueRef {
        let phi = LLVMBuildPhi(self.builder, LLVMTypeOf(values[0]), cstr("phi"));
        LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), values.len() as u32);
        phi
    }

    pub unsafe fn build_loop(&self, cond: &LLBlock, body: &LLBlock) -> LLVMValueRef {
        let before_block = LLVMAppendBasicBlockInContext(self.context, *self.curfunc.borrow(), cstr("loop_before"));
        let body_block = LLVMAppendBasicBlockInContext(self.context, *self.curfunc.borrow(), cstr("loop_body"));
        let after_block = LLVMAppendBasicBlockInContext(self.context, *self.curfunc.borrow(), cstr("loop_after"));

        LLVMBuildBr(self.builder, before_block);

        LLVMPositionBuilderAtEnd(self.builder, before_block);
        let cond_val = self.build_expr_block(cond);
        LLVMBuildCondBr(self.builder, cond_val, body_block, after_block);

        LLVMPositionBuilderAtEnd(self.builder, body_block);
        self.build_expr_block(body);
        LLVMBuildBr(self.builder, before_block);

        LLVMPositionBuilderAtEnd(self.builder, after_block);

        self.i32_const(0)
    }

    pub unsafe fn build_expr(&self, expr: &LLExpr) -> LLVMValueRef {
        match expr {
            LLExpr::Literal(lit) => self.build_literal(lit),
            LLExpr::GetValue(id) => self.get_value(*id).unwrap(),
            LLExpr::SetValue(id, value) => {
                let value = self.build_expr(value);
                self.set_value(*id, value);
                value
            },

            LLExpr::GetNamed(name) => {
                LLVMGetNamedFunction(self.module, cstr(name))
            },

            LLExpr::Cast(ltype, expr) => {
                let value = self.build_expr(expr);
                let rtype = self.build_type(ltype);
                self.build_cast(rtype, value)
            },


            LLExpr::CallC(fexpr, args, cc) => {
                let function = self.build_expr(fexpr);
                let mut argvals = self.build_args(args, LLVMTypeOf(function));
                let call = LLVMBuildCall(self.builder, function, argvals.as_mut_ptr(), argvals.len() as u32, cstr(""));
                LLVMSetInstructionCallConv(call, self.get_callconv(*cc) as c_uint);
                //LLVMSetInstructionCallConv(call, LLVMGetFunctionCallConv(function));
                call
            },


            LLExpr::DefLocal(id, name, ltype, initializer) => {
                let initval = self.build_expr(initializer);
                let rtype = self.build_type(ltype);
                let def = self.build_def_local(name, rtype, initval);
                self.set_value(*id, def);
                def
            },

            LLExpr::GetLocal(id) | LLExpr::GetGlobal(id) => {
                let pointer = self.get_value(*id).unwrap();
                self.build_load(pointer)
            },

            LLExpr::SetLocal(id, expr) | LLExpr::SetGlobal(id, expr) => {
                let value = self.build_expr(expr);
                let pointer = self.get_value(*id).unwrap();
                self.build_store(pointer, value)
            },


            LLExpr::DefStruct(id, ltype, elements) => {
                let evalues = self.build_list(elements);
                let rtype = self.build_type(ltype);
                let data = self.build_struct(rtype, evalues);
                self.set_value(*id, data);
                data
            },

            LLExpr::GetItem(expr, index) => {
                let data = self.build_expr(expr);
                self.build_get_item(data, *index)
            },

            LLExpr::SetItem(expr, index, valexpr) => {
                let data = self.build_expr(expr);
                let value = self.build_expr(valexpr);
                self.build_set_item(data, *index, value)
            },

            LLExpr::AllocRef(id, ltype, code) => {
                let rtype = self.build_type(ltype);
                let value = match code {
                    Some(code) => self.build_expr(code),
                    None => LLVMConstNull(LLVMGetElementType(rtype)),
                };
                let pointer = self.build_boxed(rtype, value);
                self.set_value(*id, pointer);
                pointer
            },

            LLExpr::AccessRef(objexpr, refs) => {
                let base = self.build_expr(objexpr);
                // TODO this is temporary, for debugging
                if Options::as_ref().debug {
                    LLVMDumpType(LLVMTypeOf(base));
                    debug!("\n{:?}", expr);
                }
                self.build_access(base, self.build_access_refs(refs))
            },

            LLExpr::LoadRef(expr) => {
                let pointer = self.build_expr(expr);
                self.build_load(pointer)
            },

            LLExpr::StoreRef(expr, valexpr) => {
                let value = self.build_expr(valexpr);
                let pointer = self.build_expr(expr);
                self.build_store(pointer, value)
            },

            LLExpr::Cmp(cmp, expr1, expr2) => {
                let val1 = self.build_expr(expr1);
                let val2 = self.build_expr(expr2);
                let pred = match cmp {
                    LLCmpType::Equal => llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                    LLCmpType::NotEqual => llvm_sys::LLVMIntPredicate::LLVMIntNE,
                };
                LLVMBuildICmp(self.builder, pred, val1, val2, cstr(""))
            },

            LLExpr::Phi(conds, blocks) => {
                let (mut return_vals, mut block_vals) = self.build_basic_blocks("match", conds, blocks);
                self.build_phi(&mut return_vals, &mut block_vals)
            },

            LLExpr::Loop(cond, body) => {
                self.build_loop(cond, body)
            },

        }
    }

    pub unsafe fn build_declarations(&self, globals: &Vec<LLGlobal>) {
        for global in globals {
            match &global {
                LLGlobal::DefType(id, _, ltype) => {
                    // TODO this might need more later, particularly for objects
                    self.set_type(*id, self.build_type(ltype));
                },

                LLGlobal::DefGlobal(id, link, name, ltype) => {
                    let rtype = self.build_type(ltype);
                    let global = LLVMAddGlobal(self.module, rtype, cstr(name.as_str()));
                    LLVMSetInitializer(global, self.null_const(rtype));
                    self.build_linkage(global, *link);
                    self.set_value(*id, global);
                },

                LLGlobal::DeclCFunc(id, name, ltype, cc) |
                LLGlobal::DefCFunc(id, _, name, ltype, _, _, cc) => {
                    let ftype = self.build_type(ltype);
                    let function = LLVMAddFunction(self.module, cstring(&name), ftype);
                    LLVMSetFunctionCallConv(function, self.get_callconv(*cc) as c_uint);
                    self.set_value(*id, function);
                },


                LLGlobal::DefNamedStruct(id, name, use_ptr) => {
                    let s = LLVMStructCreateNamed(self.context, cstr(name.as_str()));
                    self.set_type(*id, if *use_ptr { LLVMPointerType(s, 0) } else { s });
                },

                LLGlobal::SetStructBody(id, items, use_ptr) => {
                    let ptype = self.get_type(*id).unwrap();
                    let rtype = if *use_ptr { LLVMGetElementType(ptype) } else { ptype };
                    let mut items: Vec<LLVMTypeRef> = items.iter().map(|item| self.build_type(item)).collect();
                    LLVMStructSetBody(rtype, items.as_mut_ptr(), items.len() as u32, false as i32);
                },
            }
        }
    }

    pub unsafe fn build_cfunc_def(&self, function: LLVMValueRef, args: &Vec<(NodeID, String)>, body: &Vec<LLExpr>) {
        let bb = LLVMAppendBasicBlockInContext(self.context, function, cstr("entry"));
        LLVMPositionBuilderAtEnd(self.builder, bb);
        *self.curfunc.borrow_mut() = function;

        for (i, ref arg) in args.iter().enumerate() {
            let llarg = LLVMGetParam(function, i as u32);
            LLVMSetValueName(llarg, cstring(&arg.1));
            self.set_value(arg.0, llarg);
        }

        let ret = self.build_expr_block(body);
        let rettype = LLVMGetReturnType(LLVMGetElementType(LLVMTypeOf(function)));
        LLVMBuildRet(self.builder, self.build_cast(rettype, ret));
    }

    pub unsafe fn build_definitions(&self, globals: &Vec<LLGlobal>) {
        for global in globals {
            match &global {
                LLGlobal::DefCFunc(id, link, _, _, args, body, _) => {
                    let function = self.get_value(*id).unwrap();
                    self.build_linkage(function, *link);
                    self.build_cfunc_def(function, args, body);
                },

                LLGlobal::DefType(_, _, _) |
                LLGlobal::DefGlobal(_, _, _, _) |
                LLGlobal::DeclCFunc(_, _, _, _) |
                LLGlobal::DefNamedStruct(_, _, _) |
                LLGlobal::SetStructBody(_, _, _) => { /* Nothing Needs To Be Done */ }
            }
        }
    }
}

