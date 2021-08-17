
use std::rc::Rc;

use crate::defs::Def;
use crate::types::Type;
use crate::misc::UniqueID;
use crate::scope::{ Scope, ScopeRef, Context };
use crate::session::{ Session, Error };
use crate::analysis::hir::{ Mutability };


pub struct AnyVar();

impl AnyVar {
    #[must_use]
    pub fn define(session: &Session, scope: ScopeRef, id: UniqueID, mutable: Mutability, name: &str, ttype: Option<Type>) -> Result<Def, Error> {
        match scope.get_context() {
            Context::Class(_) =>
                FieldDef::define(session, scope, id, mutable, name, ttype),
            _ =>
                VarDef::define(session, scope, id, mutable, name, ttype),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDef {
    pub mutable: Mutability,

}

// TODO Mutable vs Immutable should be expressed as a field instead of it's own type
pub type VarDefRef = Rc<VarDef>;

impl VarDef {
    #[must_use]
    pub fn define(session: &Session, scope: ScopeRef, id: UniqueID, mutable: Mutability, name: &str, ttype: Option<Type>) -> Result<Def, Error> {

        let def = Def::Var(Rc::new(VarDef {
            mutable: mutable,

        }));

        VarDef::set_var_def(session, scope.clone(), id, name, def.clone(), ttype)?;
        Ok(def)
    }

    pub fn set_var_def(session: &Session, scope: ScopeRef, id: UniqueID, name: &str, def: Def, ttype: Option<Type>) -> Result<(), Error> {
        let dscope = Scope::target(session, scope.clone());

        dscope.define(name, id)?;
        session.set_def(id, def.clone());
        if let Some(ttype) = ttype {
            session.update_type(id, &ttype)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArgDef {
    pub mutable: Mutability,

}

pub type ArgDefRef = Rc<ArgDef>;

impl ArgDef {
    #[must_use]
    pub fn define(session: &Session, scope: ScopeRef, id: UniqueID, mutable: Mutability, name: &str, ttype: Option<Type>) -> Result<Def, Error> {
        let def = Def::Arg(Rc::new(ArgDef {
            mutable: mutable,

        }));

        VarDef::set_var_def(session, scope.clone(), id, name, def.clone(), ttype)?;
        Ok(def)
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct FieldDef {
    pub mutable: Mutability,

}

pub type FieldDefRef = Rc<FieldDef>;

impl FieldDef {
    #[must_use]
    pub fn define(session: &Session, scope: ScopeRef, id: UniqueID, mutable: Mutability, name: &str, ttype: Option<Type>) -> Result<Def, Error> {

        let def = Def::Field(Rc::new(FieldDef {
            mutable: mutable,

        }));

        VarDef::set_var_def(session, scope, id, name, def.clone(), ttype)?;
        Ok(def)
    }
}



