 
extern crate llvm_sys;
pub use self::llvm_sys::prelude::{ LLVMValueRef };

pub mod llcode;
pub mod codegen;
pub mod lib;


