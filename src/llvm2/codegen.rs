
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

use defs::Def;
use defs::classes::{ ClassDefRef, StructDef, StructDefRef };
use defs::functions::{ ClosureDef };

use llvm2::llcode::{ LLType, LLLit, LLRef, LLExpr, LLGlobal };
//use llvm2::lib::{ BuiltinDef, initialize_builtins };





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
                (LLVMTypeKind::LLVMPointerTypeKind, _) =>
                    LLVMBuildPtrToInt(self.builder, value, rtype, cstr("ptr")),
                (_, LLVMTypeKind::LLVMPointerTypeKind) =>
                    LLVMBuildIntToPtr(self.builder, value, rtype, cstr("ptr")),
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
            _ => panic!("Not Implemented: {:?}", lit),
        }
    }

    pub unsafe fn build_args(&self, args: &Vec<LLExpr>) -> Vec<LLVMValueRef> {
        let mut argvals = vec!();
        for argexpr in args {
            argvals.push(self.build_expr(argexpr));
        }
        argvals
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
        LLVMBuildStore(self.builder, value, pointer)
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
                let mut argvals = self.build_args(args);
                LLVMBuildCall(self.builder, function, argvals.as_mut_ptr(), argvals.len() as u32, cstr(""))
            },


            LLExpr::DefLocal(id, name, ltype, initializer) => {
                let initval = self.build_expr(initializer);
                let rtype = self.build_type(ltype);
                let def = self.build_def_local(name, rtype, initval);
                self.set_value(*id, def);
                def
            },

            LLExpr::GetLocal(id) => {
                let pointer = self.get_value(*id).unwrap();
                self.build_load(pointer)
            },

            LLExpr::SetLocal(id, expr) => {
                let value = self.build_expr(expr);
                let pointer = self.get_value(*id).unwrap();
                self.build_store(pointer, value)
            },


            LLExpr::DefStruct(id, ltype, elements) => {
                let evalues = self.build_args(elements);
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
                let value = self.build_expr(code);
                let rtype = self.build_type(ltype);
                let pointer = self.build_boxed(rtype, value);
                self.set_value(*id, pointer);
                pointer
            },

            LLExpr::AccessRef(expr, refs) => {
                let base = self.build_expr(expr);
                LLVMDumpType(LLVMTypeOf(base));
                debug!("\n{:?}", refs);
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

            _ => panic!("Not Implemented: {:?}", expr),
        }
    }

    pub unsafe fn build_declarations(&self, globals: &Vec<LLGlobal>) {
        for global in globals {
            match &global {
                LLGlobal::DefType(id, name, ltype) => {
                    // TODO this might need more later, particularly for objects
                    self.set_type(*id, self.build_type(ltype));
                },

                LLGlobal::DeclCFunc(id, name, ltype) |
                LLGlobal::DefCFunc(id, name, ltype, _, _) => {
                    let ftype = self.build_type(ltype);
                    let function = LLVMAddFunction(self.module, cstring(&name), ftype);
                    self.set_value(*id, function);
                },

                _ => panic!("Not Implemented: {:?}", global),
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

        let mut last = ptr::null_mut();
        for expr in body {
            last = self.build_expr(expr);
        }
        LLVMBuildRet(self.builder, last);
    }

    pub unsafe fn build_definitions(&self, globals: &Vec<LLGlobal>) {
        for global in globals {
            match &global {
                LLGlobal::DefCFunc(id, name, _, args, body) => {
                    let function = self.get_value(*id).unwrap();
                    self.build_cfunc_def(function, args, body);
                },

                LLGlobal::DefType(_, _, _) |
                LLGlobal::DeclCFunc(_, _, _) => { /* Nothing Needs To Be Done */ }

                _ => panic!("Not Implemented: {:?}", global),
            }
        }
    }

    pub unsafe fn build_module(&self, globals: &Vec<LLGlobal>) {
        self.build_declarations(globals);
        self.build_definitions(globals);
    }
}








 
