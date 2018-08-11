
use ast::AST;
use types::Type;
use scope::ScopeRef;
use session::{ Session, Error };
use llvm::LLVMValueRef;

trait Invokable {
    fn typecheck_invoke(&self, session: &Session, scope: ScopeRef, node: &mut AST) -> Result<Type, Error>;
    fn codegen_invoke(&self, args: Vec<LLVMValueRef>) -> LLVMValueRef;
}

