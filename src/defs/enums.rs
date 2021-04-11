
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
pub struct EnumDef {
    pub id: NodeID,
    pub vars: ScopeRef,
    pub deftype: Type,
    pub variants: RefCell<Vec<EnumVariant>>,
}

pub type EnumDefRef = Rc<EnumDef>;


impl EnumDef {
    pub fn new(id: NodeID, vars: ScopeRef, deftype: Type) -> Self {
        Self {
            id: id,
            vars: vars,
            deftype: deftype,
            variants: RefCell::new(vec!()),
        }
    }

    pub fn new_ref(id: NodeID, vars: ScopeRef, deftype: Type) -> EnumDefRef {
        Rc::new(Self::new(id, vars, deftype))
    }

    #[must_use]
    pub fn define(session: &Session, scope: ScopeRef, id: NodeID, deftype: Type) -> Result<EnumDefRef, Error> {
        let name = deftype.get_name()?;
        let vars = Scope::new_ref(None);
        vars.set_basename(name);

        let enumdef = Self::new_ref(id, vars, deftype.clone());
        scope.define_type(name, Some(id))?;
        session.set_def(id, Def::Enum(enumdef.clone()));
        session.set_type(id, deftype);

        Ok(enumdef)
    }

    #[must_use]
    pub fn add_variant(&self, session: &Session, variant: EnumVariant) -> Result<(), Error> {
        self.vars.define(&variant.ident.name, Some(variant.id))?;
        session.set_ref(variant.id, self.id);
        match variant.ttype {
            Some(ref ttype) => session.set_type(variant.id, Type::Function(r(ttype.clone()), r(self.deftype.clone()), ABI::C)),
            None => session.set_type(variant.id, self.deftype.clone()),
        }
        self.variants.borrow_mut().push(variant);
        Ok(())
    }

    pub fn get_variant_by_id(&self, id: NodeID) -> Option<usize> {
        self.variants.borrow().iter().position(|ref variant| variant.id == id)
    }

    pub fn get_variant_type_by_id(&self, id: NodeID) -> Option<Type> {
        let index = self.get_variant_by_id(id)?;
        self.variants.borrow()[index].ttype.clone()
    }
}

