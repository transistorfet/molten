
use session::Error;

pub mod classes;
//pub mod functions;

use defs::classes::ClassDefRef;
//use defs::functions::ClosureDefRef;

#[derive(Clone, Debug, PartialEq)]
pub enum VarDef {
    //Closure(ClosureDefRef)
}

/*
impl VarDef {
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
} 
*/

#[derive(Clone, Debug, PartialEq)]
pub enum TypeDef {
    Class(ClassDefRef),
    //Function(FunctionDefRef)
}

impl TypeDef {
    pub fn as_class(&self) -> Result<ClassDefRef, Error> {
        match self {
            TypeDef::Class(class) => Ok(class.clone()),
            _ => Err(Error::new(format!("TypeDefError: expected class def but found {:#?}", self))),
        }
    }
} 

