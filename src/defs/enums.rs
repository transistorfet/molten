
use std::rc::Rc;
use std::cell::RefCell;

use crate::misc::r;
use crate::abi::ABI;
use crate::defs::Def;
use crate::types::Type;
use crate::scope::{ Scope, ScopeRef, Context };
use crate::session::{ Session, Error };
use crate::analysis::hir::{ NodeID, EnumVariant };


#[derive(Clone, Debug, PartialEq)]
pub struct EnumDef {
    pub id: NodeID,
    pub vars: ScopeRef,
    pub deftype: Type,
    pub variants: RefCell<Vec<EnumVariant>>,
}

pub type EnumDefRef = Rc<EnumDef>;


impl EnumDef {
    pub fn new(defid: NodeID, vars: ScopeRef, deftype: Type) -> Self {
        Self {
            id: defid,
            vars: vars,
            deftype: deftype,
            variants: RefCell::new(vec!()),
        }
    }

    pub fn new_ref(defid: NodeID, vars: ScopeRef, deftype: Type) -> EnumDefRef {
        Rc::new(Self::new(defid, vars, deftype))
    }

    #[must_use]
    pub fn define(session: &Session, scope: ScopeRef, defid: NodeID, deftype: Type) -> Result<EnumDefRef, Error> {
        let name = deftype.get_name()?;
        let vars = Scope::new_ref(name, Context::Enum(defid), None);

        let enumdef = Self::new_ref(defid, vars, deftype.clone());
        scope.define_type(name, defid)?;
        session.set_def(defid, Def::Enum(enumdef.clone()));
        session.set_type(defid, deftype);

        let tscope = session.map.get(defid).unwrap();
        tscope.define_type("Self", defid)?;

        Ok(enumdef)
    }

    #[must_use]
    pub fn add_variant(&self, session: &Session, variant: EnumVariant) -> Result<(), Error> {
        self.vars.define(&variant.name, variant.id)?;
        session.set_ref(variant.id, self.id);
        match &variant.ttype {
            Some(ttype) => session.set_type(variant.id, Type::Function(r(ttype.clone()), r(self.deftype.clone()), ABI::C)),
            None => session.set_type(variant.id, self.deftype.clone()),
        }
        self.variants.borrow_mut().push(variant);
        Ok(())
    }

    pub fn get_variant_by_id(&self, id: NodeID) -> Option<usize> {
        self.variants.borrow().iter().position(|variant| variant.id == id)
    }

    pub fn get_variant_type_by_id(&self, id: NodeID) -> Option<Type> {
        let index = self.get_variant_by_id(id)?;
        self.variants.borrow()[index].ttype.clone()
    }
}

