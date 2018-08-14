
use session::{ Session, Error };

pub mod traits;
pub mod classes;
pub mod functions;

use defs::classes::ClassDefRef;
use defs::functions::{ FuncDefRef, OverloadDefRef, ClosureDefRef, MethodDefRef, CFuncDefRef };


#[derive(Clone, Debug, PartialEq)]
pub enum Def {
    Class(ClassDefRef),
    Func(FuncDefRef),
    Overload(OverloadDefRef),
    Closure(ClosureDefRef),
    Method(MethodDefRef),
    CFunc(CFuncDefRef)
}

impl Def {
    pub fn as_class(&self) -> Result<ClassDefRef, Error> {
        match self {
            Def::Class(class) => Ok(class.clone()),
            _ => Err(Error::new(format!("DefError: expected class def but found {:#?}", self))),
        }
    }

    /*
    pub fn as_function(&self) -> Result<FunctionDefRef, Error> {
        match self {
            VarDef::Function(function) => Ok(function.clone()),
            _ => Err(Error::new(format!("VarDefError: expected function def but found {:#?}", self))),
        }
    }

    pub fn as_function(&self) -> Result<FunctionDefRef, Error> {
        match self {
            VarDef::Function(function) => Ok(function.clone()),
            _ => Err(Error::new(format!("VarDefError: expected function def but found {:#?}", self))),
        }
    }
    */

    /*
    pub fn as_overloadable(&self) -> Option<Box<Overloadable>> {
        match self {
            Def::Func(def) => Some(Box::new(def.clone())),
            Def::Method(def) => Some(Box::new(def.func.clone())),
            _ => None
        }
    }
    */

    pub fn num_variants(&self, session: &Session) -> i32 {
        match self {
            Def::Func(_) |
            Def::Method(_) => 1,
            Def::Overload(def) => def.num_variants(session),
            _ => 0
        }
    }
}


