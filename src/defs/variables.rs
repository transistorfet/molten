
use std::rc::Rc;

use defs::Def;
use types::Type;
use hir::{ NodeID, Mutability };
use scope::{ Scope, ScopeRef };
use session::{ Session, Error };


pub struct AnyVar();

impl AnyVar {
    #[must_use]
    pub fn define(session: &Session, scope: ScopeRef, id: NodeID, mutable: Mutability, name: &String, ttype: Option<Type>) -> Result<Def, Error> {
        // TODO should you have a different one for Global, given that it'll compile to something different, until you remove it via closures...
        if scope.is_redirect() {
            FieldDef::define(session, scope, id, mutable, name, ttype)
        } else {
            VarDef::define(session, scope, id, mutable, name, ttype)
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
    pub fn define(session: &Session, scope: ScopeRef, id: NodeID, mutable: Mutability, name: &String, ttype: Option<Type>) -> Result<Def, Error> {

        let def = Def::Var(Rc::new(VarDef {
            mutable: mutable,

        }));

        VarDef::set_var_def(session, scope.clone(), id, name, def.clone(), ttype)?;
        Ok(def)
    }

    pub fn set_var_def(session: &Session, scope: ScopeRef, id: NodeID, name: &String, def: Def, ttype: Option<Type>) -> Result<(), Error> {
        let dscope = Scope::target(session, scope.clone());

        dscope.define(name.clone(), Some(id))?;
        session.set_def(id, def.clone());
        if let Some(ttype) = ttype {
            session.update_type(id, ttype)?;
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
    pub fn define(session: &Session, scope: ScopeRef, id: NodeID, mutable: Mutability, name: &String, ttype: Option<Type>) -> Result<Def, Error> {
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
    pub fn define(session: &Session, scope: ScopeRef, id: NodeID, mutable: Mutability, name: &String, ttype: Option<Type>) -> Result<Def, Error> {

        let def = Def::Field(Rc::new(FieldDef {
            mutable: mutable,

        }));

        VarDef::set_var_def(session, scope.clone(), id, name, def.clone(), ttype)?;
        Ok(def)
    }
}



