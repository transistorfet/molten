
use std::rc::Rc;

use defs::Def;
use types::Type;
use scope::{ Scope, ScopeRef };
use session::{ Session, Error };
use ast::{ NodeID, ClassSpec };


#[derive(Clone, Debug, PartialEq)]
pub struct TypeAliasDef {
    pub id: NodeID,
    pub deftype: Type,
}

pub type TypeAliasDefRef = Rc<TypeAliasDef>;


impl TypeAliasDef {
    pub fn new(id: NodeID, deftype: Type) -> Self {
        Self {
            id: id,
            deftype: deftype,
        }
    }

    pub fn new_ref(id: NodeID, deftype: Type) -> TypeAliasDefRef {
        Rc::new(Self::new(id, deftype))
    }

    #[must_use]
    pub fn define(session: &Session, scope: ScopeRef, id: NodeID, deftype: Type, aliastype: Type) -> Result<TypeAliasDefRef, Error> {
        let name = deftype.get_name()?;
        let typealiasdef = Self::new_ref(id, deftype);
        scope.define_type(name, Some(id))?;
        session.set_def(id, Def::TypeAlias(typealiasdef.clone()));
        session.set_type(id, aliastype);
        Ok(typealiasdef)
    }

    pub fn resolve(&self, session: &Session, params: Vec<Type>) -> Result<Type, Error> {
        let etype = session.get_type(self.id).ok_or(Error::new(format!("TypeError: no type set for id {:?}", self.id)))?;

        let defparams = self.deftype.get_params()?;
        if params.len() != defparams.len() {
            return Err(Error::new(format!("TypeError: expected {} type parameters to {} but got {}", defparams.len(), self.deftype.get_name()?, params.len())));
        }

        let tscope = Scope::new_ref(None);
        let mut map = Scope::map_new();
        for (def, param) in defparams.iter().zip(params) {
            match def {
                Type::Variable(_, ref id) => { map.insert(def.get_id()?, param); },
                _ => return Err(Error::new(format!("UnsupportedError: currently only typevars are supported as type parameters in type aliases"))),
            }
        }
        Ok(tscope.map_typevars(session, &mut map, etype))
    }
}

