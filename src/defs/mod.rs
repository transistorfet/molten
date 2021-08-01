
use crate::scope::ScopeRef;
use crate::hir::Mutability;
use crate::session::{ Error };

pub mod enums;
pub mod types;
pub mod classes;
pub mod variables;
pub mod functions;
pub mod traits;

use crate::defs::enums::{ EnumDefRef };
use crate::defs::types::{ TypeAliasDefRef };
use crate::defs::traits::{ TraitDefRef, TraitImplRef };
use crate::defs::classes::{ ClassDefRef, StructDefRef };
use crate::defs::variables::{ VarDefRef, ArgDefRef, FieldDefRef };
use crate::defs::functions::{ OverloadDefRef, ClosureDefRef, MethodDefRef, CFuncDefRef };


#[derive(Clone, Debug, PartialEq)]
pub enum Def {
    // Values
    Var(VarDefRef),
    Arg(ArgDefRef),
    Field(FieldDefRef),
    Overload(OverloadDefRef),
    Closure(ClosureDefRef),
    Method(MethodDefRef),
    CFunc(CFuncDefRef),

    // Types
    Class(ClassDefRef),
    Struct(StructDefRef),
    Enum(EnumDefRef),
    TypeAlias(TypeAliasDefRef),

    // Not really either
    TraitDef(TraitDefRef),
    TraitImpl(TraitImplRef),
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

    pub fn as_enum(&self) -> Result<EnumDefRef, Error> {
        match *self {
            Def::Enum(ref enumdef) => Ok(enumdef.clone()),
            _ => Err(Error::new(format!("DefError: expected enum def but found {:#?}", self))),
        }
    }

    pub fn as_trait_def(&self) -> Result<TraitDefRef, Error> {
        match *self {
            Def::TraitDef(ref def) => Ok(def.clone()),
            _ => Err(Error::new(format!("DefError: expected trait def but found {:#?}", self))),
        }
    }

    pub fn as_trait_impl(&self) -> Result<TraitImplRef, Error> {
        match *self {
            Def::TraitImpl(ref def) => Ok(def.clone()),
            _ => Err(Error::new(format!("DefError: expected trait impl but found {:#?}", self))),
        }
    }

    pub fn get_vars(&self) -> Result<ScopeRef, Error> {
        match *self {
            Def::Class(ref class) => Ok(class.structdef.vars.clone()),
            Def::Struct(ref structdef) => Ok(structdef.vars.clone()),
            Def::Enum(ref enumdef) => Ok(enumdef.vars.clone()),
            Def::TraitDef(ref traitdef) => Ok(traitdef.vars.clone()),
            _ => Err(Error::new(format!("DefError: expected class, struct, enum, or trait def but found {:#?}", self))),
        }
    }

    pub fn as_closure(&self) -> Result<ClosureDefRef, Error> {
        match *self {
            Def::Closure(ref cl) => Ok(cl.clone()),
            Def::Method(ref meth) => Ok(meth.closure.clone()),
            _ => Err(Error::new(format!("DefError: expected closure def but found {:#?}", self))),
        }
    }

    /// Return true if the definition is always accessible globally (true), or must be accessed through a closure context (false)
    pub fn is_globally_accessible(&self) -> bool {
        match self {
            Def::Class(_) |
            Def::Struct(_) |
            Def::Enum(_) |
            Def::TypeAlias(_) |
            Def::CFunc(_) => true,
            _ => false,
        }
    }

    pub fn is_mutable(&self) -> bool {
        match self {
            Def::Var(def) if def.mutable == Mutability::Mutable => true,
            Def::Arg(def) if def.mutable == Mutability::Mutable => true,
            Def::Field(def) if def.mutable == Mutability::Mutable => true,
            _ => false,
        }
    }
}


