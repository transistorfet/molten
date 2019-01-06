
use std::ptr;
use std::ffi::CString;
use std::cell::RefCell;
use std::collections::HashMap;

use std::fs::File;
use std::io::prelude::*;


extern crate llvm_sys;
use self::llvm_sys::*;
use self::llvm_sys::prelude::*;
use self::llvm_sys::core::*;
use self::llvm_sys::target::*;
use self::llvm_sys::target_machine::*;
use self::llvm_sys::transforms::pass_manager_builder::*;


use ast::NodeID;
use utils::UniqueID;
use config::Options;
use session::Session;

use llvm::llcode::{ LLType, LLLit, LLRef, LLCmpType, LLExpr, LLGlobal, LLBlock };



#[derive(Clone, Debug, PartialEq)]
pub struct LLValue {
    value: LLVMValueRef,
    ltype: LLType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LLVM<'sess> {
    pub session: &'sess Session,
    pub context: LLVMContextRef,
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,
    pub values: RefCell<HashMap<UniqueID, LLVMValueRef>>,
    pub types: RefCell<HashMap<UniqueID, LLVMTypeRef>>,
    pub curfunc: RefCell<LLVMValueRef>,
}



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
                values: RefCell::new(HashMap::new()),
                types: RefCell::new(HashMap::new()),
                curfunc: RefCell::new(ptr::null_mut()),
            }
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

    pub unsafe fn build_type(&self, ltype: &LLType) -> LLVMTypeRef {
        match ltype {
            LLType::Void => self.void_type(),
            LLType::I1 => self.i1_type(),
            LLType::I8 => self.i8_type(),
            LLType::I32 => self.i32_type(),
            LLType::I64 => self.i64_type(),
            LLType::F64 => self.f64_type(),
            LLType::Var => self.str_type(),
            LLType::Ptr(etype) => LLVMPointerType(self.build_type(etype), 0),
            LLType::Struct(etypes) => self.struct_type(etypes),
            LLType::Function(argtypes, rettype, abi) => self.cfunc_type(argtypes, rettype),
            LLType::Alias(id) => self.get_type(*id).unwrap(),
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

    pub unsafe fn build_def_local(&self, name: &String, rtype: LLVMTypeRef, initval: LLVMValueRef) -> LLVMValueRef {
        let pointer = LLVMBuildAlloca(self.builder, rtype, cstring(&name));
        LLVMBuildStore(self.builder, self.build_cast(rtype, initval), pointer);
        pointer
    }

    pub unsafe fn build_load(&self, pointer: LLVMValueRef) -> LLVMValueRef {
        LLVMBuildLoad(self.builder, pointer, cstr(""))
    }

    pub unsafe fn build_store(&self, pointer: LLVMValueRef, value: LLVMValueRef) -> LLVMValueRef {
        LLVMBuildStore(self.builder, value, pointer);
        value
    }

    pub unsafe fn build_access(&self, base: LLVMValueRef, refs: &Vec<LLRef>) -> LLVMValueRef {
        let mut indices = vec!(self.i32_const(0));
        for lref in refs {
            match &lref {
                LLRef::Deref => indices.push(self.i32_const(0)),
                LLRef::Field(index) => indices.push(self.i32_const(*index as i32)),
                LLRef::Index(index) => indices.push(self.i32_const(*index as i32)),
            }
        }
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
        let mem = LLVMBuildMalloc(self.builder, LLVMGetElementType(rtype), cstr(""));
        let reference = LLVMBuildPointerCast(self.builder, mem, rtype, cstr(""));
        LLVMBuildStore(self.builder, value, reference);
        reference
    }

    pub unsafe fn build_basic_blocks(&self, label: &str, conds: &Vec<LLBlock>, blocks: &Vec<LLBlock>) -> (Vec<LLVMValueRef>, Vec<*mut LLVMBasicBlock>) {
        let mut cond_vals = vec!();
        let mut block_vals = vec!();
        let mut return_vals = vec!();

        for block in blocks {
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

    pub unsafe fn build_expr(&self, expr: &LLExpr) -> LLVMValueRef {
        match expr {
            LLExpr::Literal(lit) => self.build_literal(lit),
            LLExpr::GetValue(id) => self.get_value(*id).unwrap(),
            LLExpr::SetValue(id, value) => {
                let value = self.build_expr(value);
                self.set_value(*id, value);
                value
            },

            LLExpr::Cast(ltype, expr) => {
                let value = self.build_expr(expr);
                let rtype = self.build_type(ltype);
                self.build_cast(rtype, value)
            },


            LLExpr::CallC(fexpr, args) => {
                let function = self.build_expr(fexpr);
                let mut argvals = self.build_args(args, LLVMTypeOf(function));
                LLVMBuildCall(self.builder, function, argvals.as_mut_ptr(), argvals.len() as u32, cstr(""))
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
                self.build_access(base, refs)
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

            LLExpr::Phi(conds, blocks) => {
                let (mut return_vals, mut block_vals) = self.build_basic_blocks("match", conds, blocks);
                self.build_phi(&mut return_vals, &mut block_vals)
            },

        }
    }

    pub unsafe fn build_declarations(&self, globals: &Vec<LLGlobal>) {
        for global in globals {
            match &global {
                LLGlobal::DefType(id, name, ltype) => {
                    // TODO this might need more later, particularly for objects
                    self.set_type(*id, self.build_type(ltype));
                },

                LLGlobal::DefGlobal(id, name, ltype) => {
                    let rtype = self.build_type(ltype);
                    let global = LLVMAddGlobal(self.module, rtype, cstr(name.as_str()));
                    LLVMSetInitializer(global, self.null_const(rtype));
                    // TODO this is uesd by vtables but might not be wanted for other uses...
                    LLVMSetLinkage(global, LLVMLinkage::LLVMLinkOnceAnyLinkage);
                    self.set_value(*id, global);
                },

                LLGlobal::DeclCFunc(id, name, ltype) |
                LLGlobal::DefCFunc(id, name, ltype, _, _) => {
                    let ftype = self.build_type(ltype);
                    let function = LLVMAddFunction(self.module, cstring(&name), ftype);
                    self.set_value(*id, function);
                },


                LLGlobal::DefNamedStruct(id, name) => {
                    self.set_type(*id, LLVMPointerType(LLVMStructCreateNamed(self.context, cstr(name.as_str())), 0));
                },

                LLGlobal::SetStructBody(id, items) => {
                    let rtype = LLVMGetElementType(self.get_type(*id).unwrap());
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
        LLVMBuildRet(self.builder, ret);
    }

    pub unsafe fn build_definitions(&self, globals: &Vec<LLGlobal>) {
        for global in globals {
            match &global {
                LLGlobal::DefCFunc(id, name, _, args, body) => {
                    let function = self.get_value(*id).unwrap();
                    self.build_cfunc_def(function, args, body);
                },

                LLGlobal::DefType(_, _, _) |
                LLGlobal::DefGlobal(_, _, _) |
                LLGlobal::DeclCFunc(_, _, _) |
                LLGlobal::DefNamedStruct(_, _) |
                LLGlobal::SetStructBody(_, _) => { /* Nothing Needs To Be Done */ }
            }
        }
    }

    pub fn build_module(&self, globals: &Vec<LLGlobal>) {
        unsafe {
            self.build_declarations(globals);
            self.build_definitions(globals);
        }
    }
}








 
