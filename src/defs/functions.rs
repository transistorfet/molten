
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

        if let Some(ref ttype) = ttype {
            session.update_type(scope.clone(), id, ttype.clone())?;
        }

        if let Some(ref name) = *name {
            let dscope = Scope::target(session, scope.clone());
            if let Some(previd) = dscope.get_var_def(&name) {
                OverloadDef::define(session, scope.clone(), &name, id, previd, ttype)?;
            } else {
                dscope.define(name.clone(), ttype, Some(id))?;
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

    pub fn get_variants(&self, session: &Session) -> Vec<NodeID> {
        let mut variants = self.variants.borrow().clone();
        let mut prev = match self.parent {
            Some(id) => match session.get_def(id) {
                Ok(Def::Overload(ol)) => ol.get_variants(session),
                _ => vec!(id)
            },
            _ => vec!(),
        };
        variants.extend(prev.iter());
        variants
    }

    pub fn find_variant(&self, session: &Session, scope: ScopeRef, atypes: Type) -> Result<(NodeID, Type), Error> {
        let variants = self.get_variants(session);

        let mut found = vec!();
        let mut variant_types = vec!();
        for id in variants {
            let ttype = session.get_type(id);
            match ttype {
                Some(ttype) => {
                    if let Type::Function(ref btypes, _, _) = ttype {
                        if check_type(session, scope.clone(), Some(*btypes.clone()), Some(atypes.clone()), Check::Def, false).is_ok() {
                            found.push((id, ttype.clone()));
                        }
                    }
                    variant_types.push(ttype);
                },
                None => return Err(Error::new(format!("OverloadError: variant is not typechecked or is not a function type: {:?}", ttype))),
            }
        }

        match found.len() {
            0 => Err(Error::new(format!("OverloadError: No valid variant found for {}\n\tout of [{}]", atypes, Type::display_vec(&variant_types)))),
            1 => Ok(found.remove(0)),
            _ => Err(Error::new(format!("OverloadError: Ambiguous {}\n\tvariants found [{}]", atypes, found.iter().map(|(i, t)| format!("{}", t)).collect::<Vec<String>>().join(", ")))),
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


