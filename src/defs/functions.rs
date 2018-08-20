
use std::rc::Rc;
use std::cell::RefCell;

use types::*;
use defs::Def;
use utils::UniqueID;
use ast::{ NodeID, Ident, AST };
use scope::{ Scope, ScopeRef };
use session::{ Session, Error };

use defs::traits::{  };
use defs::classes::{ StructDef, StructDefRef };


#[derive(Clone, Debug, PartialEq)]
pub struct FuncDef {

}

pub type FuncDefRef = Rc<FuncDef>;

impl FuncDef {
    pub fn define_func(session: &Session, scope: ScopeRef, id: NodeID, name: &Option<String>, abi: ABI, ttype: Option<Type>) -> Result<Def, Error> {
        match abi {
            ABI::C => CFuncDef::define(session, scope.clone(), id, name, ttype),
            ABI::Molten => {
                if scope.is_redirect() && name.is_some() {
                    MethodDef::define(session, scope.clone(), id, name, ttype)
                } else {
                    FuncDef::define(session, scope.clone(), id, name, ttype)
                }
            },
            _ => return Err(Error::new(format!("DefError: unsupported ABI {:?}", abi))),
        }
    }

    pub fn define(session: &Session, scope: ScopeRef, id: NodeID, name: &Option<String>, ttype: Option<Type>) -> Result<Def, Error> {

        let def = Def::Func(Rc::new(FuncDef {

        }));

        FuncDef::set_func_def(session, scope.clone(), id, name, def.clone(), ttype)?;
        Ok(def)
    }

    #[must_use]
    pub fn set_func_def(session: &Session, scope: ScopeRef, id: NodeID, name: &Option<String>, def: Def, ttype: Option<Type>) -> Result<(), Error> {
        session.set_def(id, def.clone());
        if let Some(ref name) = *name {
            let dscope = Scope::target(session, scope.clone());
            if let Some(previd) = dscope.get_var_def(&name) {
                OverloadDef::define(session, scope.clone(), &name, id, previd, ttype)?;
            } else {
                dscope.define(name.clone(), ttype.clone(), Some(id))?;
                if let Some(ttype) = ttype {
                    session.set_type(id, ttype);
                }
            }
        }
        Ok(())
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct OverloadDef {
    pub id: NodeID,
    pub parent: Option<NodeID>,
    pub variants: RefCell<Vec<NodeID>>
}

pub type OverloadDefRef = Rc<OverloadDef>;

impl OverloadDef {
    pub fn create(session: &Session, parent: Option<NodeID>, variants: Vec<NodeID>) -> NodeID {
        let defid = NodeID::generate();
        let def = Def::Overload(Rc::new(OverloadDef {
            id: defid,
            parent: parent,
            variants: RefCell::new(variants)
        }));
        session.set_def(defid, def.clone());
        defid
    }

    pub fn define(session: &Session, scope: ScopeRef, name: &String, id: NodeID, previd: NodeID, ttype: Option<Type>) -> Result<(), Error> {
        let dscope = Scope::target(session, scope.clone());

        if dscope.contains_local(&name) {
            match session.get_def(previd) {
                Ok(Def::Overload(prev)) => {
                    prev.add_variant(id);
                },
                Ok(Def::Func(_)) |
                Ok(Def::Method(_)) => {
                    let defid = OverloadDef::create(session, None, vec!(previd, id));
                    dscope.set_var_def(&name, defid);
                }
                _ => return Err(Error::new(format!("NameError: unable to overload {} because of a previous definitions", name)))
            }
        } else {
            let parentid = match session.get_def(previd) {
                Ok(Def::Overload(_)) => previd,
                _ => {
                    let newprev = OverloadDef::create(session, None, vec!(previd));
                    let pscope = Scope::locate_variable(dscope.clone(), name).unwrap();
                    pscope.set_var_def(name, newprev);
                    newprev
                },
            };
            let defid = OverloadDef::create(session, Some(parentid), vec!(id));
            dscope.define(name.clone(), dscope.get_variable_type(session, &name), Some(defid))?;
        }
        ttype.map(|ttype| Scope::add_func_variant(session, dscope.clone(), name, scope.clone(), ttype).unwrap());
        Ok(())
    }

    pub fn add_variant(&self, id: NodeID) {
        self.variants.borrow_mut().push(id);
    }

    pub fn num_variants(&self, session: &Session) -> i32 {
        let prev = match self.parent {
            Some(id) => session.get_def(id).unwrap().num_variants(session),
            _ => 0,
        };
        prev + self.variants.borrow().len() as i32
    }

    pub fn find_variant(&self, session: &Session, scope: ScopeRef, atypes: Type) -> Result<(NodeID, Type), Error> {
        for id in self.variants.borrow().iter() {
            let ttype = session.get_type(*id);
            match ttype {
                Some(Type::Function(ref btypes, _, _)) => {
                    match check_type(session, scope.clone(), Some(*btypes.clone()), Some(atypes.clone()), Check::Def, false) {
                        Ok(_) => return Ok((*id, ttype.clone().unwrap())),
                        Err(err) => return Err(err)
                    }
                },
                _ => return Err(Error::new(format!("OverloadError: variant is not typechecked or is not a function type: {:?}", ttype))),
            }
        }

        match self.parent {
            Some(ref id) => session.get_def(*id)?.as_overload()?.find_variant(session, scope, atypes),
            //_ => Err(Error::new(format!("OverloadError: No valid variant found for {}\n\tout of [{}]", atypes, Type::display_vec(&variants)))),
            _ => Err(Error::new(format!("OverloadError: No valid variant found for {}", atypes))),
        }
    }
}


/*
impl Overloadable for Rc<FuncDef> {
    fn num_variants(&self) -> i32 {
        self.variants.borrow().len() as i32
    }

    fn add_variant(&self, id: NodeID) {
        self.variants.borrow_mut().push(id);
    }
}
*/


#[derive(Clone, Debug, PartialEq)]
pub struct ClosureDef {
    pub id: UniqueID,
    pub context: StructDefRef,
}

pub type ClosureDefRef = Rc<ClosureDef>;

impl ClosureDef {


}


#[derive(Clone, Debug, PartialEq)]
pub struct MethodDef {
    //pub id: UniqueID,

}

pub type MethodDefRef = Rc<MethodDef>;

impl MethodDef {
    pub fn define(session: &Session, scope: ScopeRef, id: NodeID, name: &Option<String>, ttype: Option<Type>) -> Result<Def, Error> {
        let def = Def::Method(Rc::new(MethodDef {

        }));

        FuncDef::set_func_def(session, scope.clone(), id, name, def.clone(), ttype)?;
        Ok(def)
    }
}



#[derive(Clone, Debug, PartialEq)]
pub struct CFuncDef {
    //pub id: UniqueID,
    //pub ftype: Type,
}

pub type CFuncDefRef = Rc<CFuncDef>;

impl CFuncDef {
    pub fn define(session: &Session, scope: ScopeRef, id: NodeID, name: &Option<String>, ttype: Option<Type>) -> Result<Def, Error> {
        if scope.is_redirect() {
            return Err(Error::new(format!("DefError: cannot declare a C ABI function within a class body")));
        }

        let name = match name.as_ref() {
            Some(name) => name,
            None => return Err(Error::new(format!("NameError: C ABI function declared without a name"))),
        };

        let def = Def::CFunc(Rc::new(CFuncDef {

        }));

        scope.define(name.clone(), ttype, Some(id))?;
        session.set_def(id, def.clone());
        Ok(def)
    }
}


