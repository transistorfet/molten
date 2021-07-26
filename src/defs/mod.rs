
use scope::ScopeRef;
use hir::Mutability;
use session::{ Error };

pub mod enums;
pub mod types;
pub mod classes;
pub mod variables;
pub mod functions;
pub mod traits;

use defs::enums::{ EnumDefRef };
use defs::types::{ TypeAliasDefRef };
use defs::traits::{ TraitDefRef, TraitImplRef };
use defs::classes::{ ClassDefRef, StructDefRef };
use defs::variables::{ VarDefRef, ArgDefRef, FieldDefRef };
use defs::functions::{ OverloadDefRef, ClosureDefRef, MethodDefRef, CFuncDefRef };


#[derive(Clone, Debug, PartialEq)]
pub enum Def {
    TypeAlias(TypeAliasDefRef),
    Var(VarDefRef),
    Arg(ArgDefRef),
    Field(FieldDefRef),
    Class(ClassDefRef),
    #[allow(dead_code)]
    Struct(StructDefRef),
    Enum(EnumDefRef),
    Overload(OverloadDefRef),
    Closure(ClosureDefRef),
    Method(MethodDefRef),
    CFunc(CFuncDefRef),
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

    pub fn is_globally_accessible(&self) -> bool {
        match self {
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


