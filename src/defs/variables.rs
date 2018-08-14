
use std::rc::Rc;
use std::cell::RefCell;

use defs::Def;
use types::Type;
use utils::UniqueID;
use ast::{ NodeID, Ident, AST };
use scope::{ Scope, ScopeRef };
use session::{ Session, Error };


#[derive(Clone, Debug, PartialEq)]
pub struct VarDef {

}

pub type VarDefRef = Rc<VarDef>;

impl VarDef {
    pub fn define_var(session: &Session, scope: ScopeRef, id: NodeID, name: &String, ttype: Option<Type>) -> Result<Def, Error> {
        VarDef::define(session, scope, id, name, ttype)
    }

    pub fn define(session: &Session, scope: ScopeRef, id: NodeID, name: &String, ttype: Option<Type>) -> Result<Def, Error> {

        let def = Def::Var(Rc::new(VarDef {

        }));

        VarDef::set_var_def(session, scope.clone(), id, name, def.clone(), ttype)?;
        Ok(def)
    }

    pub fn set_var_def(session: &Session, scope: ScopeRef, id: NodeID, name: &String, def: Def, ttype: Option<Type>) -> Result<(), Error> {
        let dscope = Scope::target(session, scope.clone());
        dscope.define(name.clone(), ttype)?;
        dscope.set_var_def(&name, id);
        session.set_def(id, def.clone());
        Ok(())
    }
}

// TODO should you have ConstDef?  FieldDef?

