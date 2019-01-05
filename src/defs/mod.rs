
use scope::ScopeRef;
use session::{ Session, Error };

pub mod traits;
pub mod types;
pub mod classes;
pub mod variables;
pub mod functions;

use defs::types::{ TypeAliasDefRef };
use defs::variables::{ VarDefRef, ArgDefRef, FieldDefRef };
use defs::classes::{ ClassDefRef, StructDefRef };
use defs::functions::{ FuncDefRef, OverloadDefRef, ClosureDefRef, MethodDefRef, CFuncDefRef, BuiltinFuncDefRef };


#[derive(Clone, Debug, PartialEq)]
pub enum Def {
    TypeAlias(TypeAliasDefRef),
    Var(VarDefRef),
    Arg(ArgDefRef),
    Field(FieldDefRef),
    Class(ClassDefRef),
    Struct(StructDefRef),
    Func(FuncDefRef),
    Overload(OverloadDefRef),
    Closure(ClosureDefRef),
    Method(MethodDefRef),
    CFunc(CFuncDefRef),
    Builtin(BuiltinFuncDefRef),
}


impl Def {
    pub fn as_class(&self) -> Result<ClassDefRef, Error> {
        match *self {
            Def::Class(ref class) => Ok(class.clone()),
            _ => Err(Error::new(format!("DefError: expected class def but found {:#?}", self))),
        }
    }

    pub fn as_struct(&self) -> Result<StructDefRef, Error> {
        match *self {
            Def::Class(ref class) => Ok(class.structdef.clone()),
            Def::Struct(ref structdef) => Ok(structdef.clone()),
            _ => Err(Error::new(format!("DefError: expected class or struct def but found {:#?}", self))),
        }
    }

    pub fn get_vars(&self) -> Result<ScopeRef, Error> {
        match *self {
            Def::Class(ref class) => Ok(class.structdef.vars.clone()),
            Def::Struct(ref structdef) => Ok(structdef.vars.clone()),
            _ => Err(Error::new(format!("DefError: expected class or struct def but found {:#?}", self))),
        }
    }

    pub fn as_overload(&self) -> Result<OverloadDefRef, Error> {
        match *self {
            Def::Overload(ref class) => Ok(class.clone()),
            _ => Err(Error::new(format!("DefError: expected overload def but found {:#?}", self))),
        }
    }

    pub fn as_closure(&self) -> Result<ClosureDefRef, Error> {
        match *self {
            Def::Closure(ref cl) => Ok(cl.clone()),
            Def::Method(ref meth) => Ok(meth.closure.clone()),
            _ => Err(Error::new(format!("DefError: expected closure def but found {:#?}", self))),
        }
    }

    /*
    pub fn as_overloadable(&self) -> Option<Box<Overloadable>> {
        match self {
            Def::Func(def) => Some(Box::new(def.clone())),
            Def::Method(def) => Some(Box::new(def.func.clone())),
            _ => None
        }
    }
    */

    pub fn is_globally_accessible(&self) -> bool {
        match self {
            Def::CFunc(_) | Def::Func(_) => true,
            _ => false,
        }
    }

    pub fn is_mutable(&self) -> bool {
        match self {
            Def::Var(def) => def.mutable,
            Def::Arg(def) => def.mutable,
            Def::Field(def) => def.mutable,
            _ => false,
        }
    }

    pub fn num_variants(&self, session: &Session) -> i32 {
        match *self {
            Def::Func(_) |
            Def::Method(_) => 1,
            Def::Overload(ref def) => def.get_variants(session).len() as i32,
            _ => 0
        }
    }
}


