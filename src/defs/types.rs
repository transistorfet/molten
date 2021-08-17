
use std::rc::Rc;

use crate::defs::Def;
use crate::types::Type;
use crate::misc::UniqueID;
use crate::scope::ScopeRef;
use crate::session::{ Session, Error };


#[derive(Clone, Debug, PartialEq)]
pub struct TypeAliasDef {
    pub id: UniqueID,
    pub deftype: Type,
}

pub type TypeAliasDefRef = Rc<TypeAliasDef>;


impl TypeAliasDef {
    pub fn new(defid: UniqueID, deftype: Type) -> Self {
        Self {
            id: defid,
            deftype: deftype,
        }
    }

    pub fn new_ref(defid: UniqueID, deftype: Type) -> TypeAliasDefRef {
        Rc::new(Self::new(defid, deftype))
    }

    #[must_use]
    pub fn define(session: &Session, scope: ScopeRef, defid: UniqueID, deftype: Type, aliastype: Type) -> Result<TypeAliasDefRef, Error> {
        let name = deftype.get_name()?;
        scope.define_type(name, defid)?;
        let typealiasdef = Self::new_ref(defid, deftype);
        session.set_def(defid, Def::TypeAlias(typealiasdef.clone()));
        session.set_type(defid, aliastype);
        Ok(typealiasdef)
    }

    pub fn resolve(&self, session: &Session, params: &Vec<Type>) -> Result<Type, Error> {
        let etype = session.get_type(self.id).ok_or(Error::new(format!("TypeError: no type set for id {:?}", self.id)))?;

        let defparams = self.deftype.get_params()?;
        if params.len() != defparams.len() {
            return Err(Error::new(format!("TypeError: expected {} type parameters to {} but got {}", defparams.len(), self.deftype.get_name()?, params.len())));
        }

        let mut map = Type::map_new();
        for (def, param) in defparams.iter().zip(params.iter()) {
            match def {
                Type::Universal(_, id) => { map.insert(*id, param.clone()); },
                _ => return Err(Error::new(format!("UnsupportedError: currently only universal type variables are supported as type parameters in type aliases"))),
            }
        }
        Ok(Type::map_typevars(session, &mut map, etype.clone()))
    }
}

