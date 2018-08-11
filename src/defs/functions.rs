
use std::rc::Rc;

use ast::AST;
use types::*;
use defs::Def;
use utils::UniqueID;
use scope::{ Scope, ScopeRef };
use session::{ Session, Error };
// TODO this might move
use defs::classes::{ StructDef, StructDefRef };



#[derive(Clone, Debug, PartialEq)]
pub struct CFuncDef {
    pub id: UniqueID,
    //pub ftype: Type,
}

pub type CFuncDefRef = Rc<CFuncDef>;

impl CFuncDef {
    pub fn new(id: UniqueID) -> Self {
        Self {
            id: id,
        }
    }

    pub fn new_ref(id: UniqueID) -> CFuncDefRef {
        Rc::new(Self::new(id))
    }

}


#[derive(Clone, Debug, PartialEq)]
pub struct MethodDef {
    pub id: UniqueID,
}

pub type MethodDefRef = Rc<MethodDef>;

impl MethodDef {
    pub fn new(id: UniqueID) -> Self {
        Self {
            id: id,
        }
    }

    pub fn new_ref(id: UniqueID) -> MethodDefRef {
        Rc::new(Self::new(id))
    }

}




#[derive(Clone, Debug, PartialEq)]
pub struct ClosureDef {
    pub id: UniqueID,
    pub context: StructDefRef,
}

pub type ClosureDefRef = Rc<ClosureDef>;

impl ClosureDef {
    pub fn new(id: UniqueID) -> Self {
        Self {
            id: id,
            context: StructDef::new_ref(Type::Object(id.to_string(), vec!()), None),
        }
    }

    pub fn new_ref(id: UniqueID) -> ClosureDefRef {
        Rc::new(Self::new(id))
    }


}


/*
impl Invokable for Function {


}
*/
