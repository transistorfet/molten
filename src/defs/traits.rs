
use std::rc::Rc;
use std::cell::RefCell;

use misc::r;
use abi::ABI;
use defs::Def;
use types::Type;
use scope::{ Scope, ScopeRef };
use session::{ Session, Error };
use hir::{ NodeID, EnumVariant };


#[derive(Clone, Debug, PartialEq)]
pub struct TraitDef {
    pub id: NodeID,
    pub vars: ScopeRef,
    pub deftype: Type,
}

pub type TraitDefRef = Rc<TraitDef>;

impl TraitDef {
    pub fn new(id: NodeID, vars: ScopeRef, deftype: Type) -> Self {
        Self {
            id: id,
            vars: vars,
            deftype: deftype,
        }
    }

    pub fn new_ref(id: NodeID, vars: ScopeRef, deftype: Type) -> TraitDefRef {
        Rc::new(Self::new(id, vars, deftype))
    }

    #[must_use]
    pub fn define(session: &Session, scope: ScopeRef, id: NodeID, deftype: Type) -> Result<TraitDefRef, Error> {
        let name = deftype.get_name()?;
        let vars = Scope::new_ref(None);
        vars.set_basename(name.clone());

        let traitdef = Self::new_ref(id, vars, deftype.clone());
        scope.define_type(name, Some(id))?;
        session.set_def(id, Def::Trait(traitdef.clone()));
        session.set_type(id, deftype);

        Ok(traitdef)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TraitImpl {
    pub id: NodeID,
    pub trait_id: NodeID,
    pub impltype: Type,
    pub fortype: Type,
    pub vtable: Vtable,
}

pub type TraitImplRef = Rc<TraitImpl>;

impl TraitImpl {
    pub fn new(id: NodeID, trait_id: NodeID, vtable: Vtable) -> Self {
        Self {
            id: id,
            trait_id: trait_id,

            vtable: vtable,
        }
    }

    pub fn new_ref(id: NodeID, trait_id: NodeID, vtable: Vtable) -> TraitImplRef {
        Rc::new(Self::new(id, trait_id, vtable))
    }

    #[must_use]
    pub fn define(session: &Session, scope: ScopeRef, id: NodeID, impltype: Type, fortype: Type) -> Result<TraitImplRef, Error> {
        let name = deftype.get_name()?;
        let vars = Scope::new_ref(None);
        vars.set_basename(name.clone());

        let vtable = Vtable::create(session, NodeID::generate(), format!("{}_vtable", name.clone()))?;
        let traitimpl = Self::new_ref(id, vars, vtable);
        scope.define_type(name, Some(id))?;
        session.set_def(id, Def::TraitImpl(traitimpl.clone()));
        session.set_type(id, deftype);

        Ok(traitimpl)
    }
}

// parser should only accept decls in trait definition
// trait implementation should require all functions to be defined (name binding stage or after?)
// transform should create a vtable for each implementation

// trait type needs to be introduced somehow
// converting to a trait type needs to be tracked so it can be inserted into output
// 




