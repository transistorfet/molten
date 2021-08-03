
use std::rc::Rc;
use std::cell::RefCell;

use crate::defs::Def;
use crate::scope::{ Scope, ScopeRef, Context };
use crate::session::{ Session, Error };
use crate::types::{ Type, Check, check_type };
use crate::hir::{ NodeID, ClassSpec };
use crate::defs::classes::{ Vtable };
use crate::defs::types::{ TypeAliasDef };


#[derive(Clone, Debug, PartialEq)]
pub struct TraitDef {
    pub id: NodeID,
    pub vars: ScopeRef,
    pub deftype: Type,
    pub vtable: Vtable,
    pub impls: RefCell<Vec<TraitImplRef>>,
}

pub type TraitDefRef = Rc<TraitDef>;

impl TraitDef {
    pub fn new(id: NodeID, vars: ScopeRef, deftype: Type, vtable: Vtable) -> Self {
        Self {
            id: id,
            vars: vars,
            deftype: deftype,
            vtable: vtable,
            impls: RefCell::new(vec!()),
        }
    }

    pub fn new_ref(id: NodeID, vars: ScopeRef, deftype: Type, vtable: Vtable) -> TraitDefRef {
        Rc::new(Self::new(id, vars, deftype, vtable))
    }

    #[must_use]
    pub fn define(session: &Session, scope: ScopeRef, defid: NodeID, traitspec: ClassSpec) -> Result<TraitDefRef, Error> {
        let name = traitspec.name.as_str();
        let tscope = session.map.get_or_add(defid, name, Context::TraitDef(defid), Some(scope.clone()));
        tscope.define_type("Self", defid)?;

        let vars = Scope::new_ref(name, Context::TraitDef(defid), None);

        let deftype = Type::Universal(traitspec.name.clone(), defid);
        let vtable = Vtable::create(session, NodeID::generate(), format!("{}_vtable", name))?;
        let traitdef = Self::new_ref(defid, vars, deftype.clone(), vtable);
        scope.define_type(name, defid)?;
        session.set_def(defid, Def::TraitDef(traitdef.clone()));
        session.set_type(defid, deftype);
        session.set_constraints(defid, vec!(defid));

        Ok(traitdef)
    }

    pub fn add_impl(&self, traitimpl: TraitImplRef) {
        self.impls.borrow_mut().push(traitimpl);
    }

    pub fn find_impl(&self, session: &Session, ttype: &Type) -> Result<TraitImplRef, Error> {
        for traitimpl in self.impls.borrow().iter() {
            if check_type(session, Some(&traitimpl.impltype), Some(ttype), Check::Def, false).is_ok() {
                return Ok(traitimpl.clone());
            }
        }
        return Err(Error::new(format!("TraitError: no implementation for type {:?}", ttype)));
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TraitImpl {
    pub id: NodeID,
    pub trait_id: NodeID,
    pub impltype: Type,
    pub vtable: Vtable,
}

pub type TraitImplRef = Rc<TraitImpl>;

impl TraitImpl {
    pub fn new(id: NodeID, trait_id: NodeID, impltype: Type, vtable: Vtable) -> Self {
        Self {
            id: id,
            trait_id: trait_id,
            impltype: impltype,
            vtable: vtable,
        }
    }

    pub fn new_ref(id: NodeID, trait_id: NodeID, impltype: Type, vtable: Vtable) -> TraitImplRef {
        Rc::new(Self::new(id, trait_id, impltype, vtable))
    }

    #[must_use]
    pub fn define(session: &Session, scope: ScopeRef, impl_id: NodeID, trait_id: NodeID, impltype: Type) -> Result<TraitImplRef, Error> {
        let deftype = session.get_type(trait_id).unwrap();
        let name = format!("{}_{}", deftype.get_name()?, impltype.get_name()?);
        let tscope = session.map.get_or_add(impl_id, &name, Context::TraitImpl(trait_id, impl_id), Some(scope.clone()));
        // TODO this should be impl_id, and it should be an alias
        TypeAliasDef::define(session, tscope.clone(), NodeID::generate(), Type::Object("Self".to_string(), impl_id, vec!()), impltype.clone())?;
        //tscope.define_type("Self", impl_id)?;

        let vtable = Vtable::create(session, NodeID::generate(), format!("{}_vtable", name.clone()))?;
        let traitimpl = Self::new_ref(impl_id, trait_id, impltype.clone(), vtable);
        scope.define_type(&name, impl_id)?;
        session.set_def(impl_id, Def::TraitImpl(traitimpl.clone()));
        session.set_type(impl_id, impltype);

        let traitdef = session.get_def(trait_id).unwrap().as_trait_def().unwrap();
        traitdef.add_impl(traitimpl.clone());

        Ok(traitimpl)
    }
}


