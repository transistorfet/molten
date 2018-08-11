
use session::Error;

pub mod traits;
pub mod classes;
pub mod functions;

use defs::classes::ClassDefRef;
//use defs::functions::ClosureDefRef;


#[derive(Clone, Debug, PartialEq)]
pub enum Def {
    Class(ClassDefRef),
    //Function(FunctionDefRef)
    //Closure(ClosureDefRef)
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
}


