
use types::Type;
use scope::ScopeRef;
use ast::{ NodeID, AST };
use session::{ Session, Error };
use llvm::LLVMValueRef;

/*
pub trait Overloadable {
    fn num_variants(&self) -> i32;
    fn add_variant(&self, id: NodeID);
}

trait Invokable {
    fn typecheck_invoke(&self, session: &Session, scope: ScopeRef, node: &mut AST) -> Result<Type, Error>;
    fn codegen_invoke(&self, args: Vec<LLVMValueRef>) -> LLVMValueRef;
}


trait CompileInvoke {
    fn compile(&self, args: Vec<LLVMValueRef>) -> LLVMValueRef;
}
*/



