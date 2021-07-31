
use std::rc::Rc;
use std::cell::RefCell;

use crate::abi::ABI;
use crate::defs::Def;
use crate::scope::{ Scope, ScopeRef };
use crate::session::{ Session, Error };
use crate::types::{ Type, Check, check_type };
use crate::hir::{ NodeID, Mutability, Visibility };

use crate::defs::classes::{ Define, StructDef, StructDefRef };


pub struct AnyFunc();

impl AnyFunc {
    #[must_use]
    pub fn define(session: &Session, scope: ScopeRef, id: NodeID, vis: Visibility, name: &str, abi: ABI, ttype: Option<Type>) -> Result<Def, Error> {
        match abi {
            ABI::C => CFuncDef::define(session, scope.clone(), id, vis, name, ttype),
            ABI::Molten => {
                if scope.is_redirect() && vis != Visibility::Anonymous {
                    MethodDef::define(session, scope.clone(), id, vis, name, ttype)
                } else {
                    ClosureDef::define(session, scope.clone(), id, vis, name, ttype)
                }
            },
            _ => return Err(Error::new(format!("DefError: unsupported ABI {:?}", abi))),
        }
    }

    #[must_use]
    pub fn set_func_def(session: &Session, scope: ScopeRef, id: NodeID, vis: Visibility, name: &str, def: Def, ttype: Option<Type>) -> Result<(), Error> {
        session.set_def(id, def.clone());

        if let Some(ttype) = &ttype {
            session.update_type(id, ttype)?;
        }

        if vis != Visibility::Anonymous {
            let dscope = Scope::target(session, scope.clone());
            if let Some(previd) = dscope.get_var_def(&name) {
                OverloadDef::define(session, scope.clone(), &name, id, previd, ttype)?;
            } else {
                dscope.define(name, id)?;
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
        let def = Rc::new(OverloadDef {
            id: defid,
            parent: parent,
            variants: RefCell::new(vec!())
        });
        for variant in variants {
            def.add_variant(session, variant);
        }
        session.set_def(defid, Def::Overload(def));
        defid
    }

    #[must_use]
    pub fn define(session: &Session, scope: ScopeRef, name: &str, id: NodeID, previd: NodeID, _ttype: Option<Type>) -> Result<(), Error> {
        let dscope = Scope::target(session, scope.clone());

        if dscope.contains_local(&name) {
            match session.get_def(previd) {
                Ok(Def::Overload(prev)) => {
                    prev.add_variant(session, id);
                },
                Ok(Def::Method(_)) |
                Ok(Def::Closure(_)) => {
                    let defid = OverloadDef::create(session, None, vec!(previd, id));
                    dscope.set_var_def(&name, defid);
                }
                _ => return Err(Error::new(format!("NameError: unable to overload {} because of a previous definitions", name)))
            }
        } else {
            let defid = OverloadDef::create(session, Some(previd), vec!(id));
            dscope.define(name, defid)?;
        }
        Ok(())
    }

    pub fn add_variant(&self, session: &Session, id: NodeID) {
        session.set_ref(id, self.id);
        self.variants.borrow_mut().push(id);
    }

    pub fn get_variants(&self, session: &Session) -> Vec<NodeID> {
        let mut variants = self.variants.borrow().clone();
        let prev = self.get_parent_variants(session);
        variants.extend(prev.iter());
        variants
    }

    pub fn get_parent_variants(&self, session: &Session) -> Vec<NodeID> {
        match self.parent {
            Some(id) => match session.get_def(id) {
                Ok(Def::Overload(ol)) => ol.get_variants(session),
                _ => vec!(id),
            },
            _ => vec!(),
        }
    }

    pub fn find_variant(&self, session: &Session, atypes: &Type) -> Result<(NodeID, Type), Error> {
        let (mut found, variant_types) = self.find_all_variants(session, atypes);

        match found.len() {
            0 => Err(Error::new(format!("OverloadError: No valid variant found for {}\n\tout of [{}]", atypes, Type::display_vec(&variant_types)))),
            1 => Ok(found.remove(0)),
            _ => Err(Error::new(format!("OverloadError: Ambiguous {}\n\tvariants found [{}]", atypes, found.iter().map(|(_, t)| format!("{}", t)).collect::<Vec<String>>().join(", ")))),
        }
    }

    pub fn find_all_variants(&self, session: &Session, atypes: &Type) -> (Vec<(NodeID, Type)>, Vec<Type>) {
        let variants = self.get_variants(session);
        self.find_variants_of(variants, session, atypes)
    }

    pub fn find_local_variants(&self, session: &Session, atypes: &Type) -> (Vec<(NodeID, Type)>, Vec<Type>) {
        let variants = self.variants.borrow().clone();
        self.find_variants_of(variants, session, atypes)
    }

    pub fn find_variants_of(&self, variants: Vec<NodeID>, session: &Session, atypes: &Type) -> (Vec<(NodeID, Type)>, Vec<Type>) {
        let mut found = vec!();
        let mut variant_types = vec!();
        for id in variants {
            debug!(">>>: {:?} = {:?}", id, session.get_type(id));

            // Fetch the variant's type and map its typevars if necessary
            let ttype = match session.get_type(id) {
                Some(ttype) => ttype.clone(),
                None => {
                    let vtype = session.new_typevar();
                    session.set_type(id, vtype.clone());
                    vtype
                }
            };

            if let Type::Function(btypes, _, _) = &ttype {
                if check_type(session, Some(btypes), Some(&atypes), Check::Def, false).is_ok() {
                    if found.len() < 1 || self.variants.borrow().iter().position(|lid| *lid == id).is_some() {
                        found.push((id, ttype.clone()));
                    }
                }
            }
            variant_types.push(ttype);
        }
        (found, variant_types)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClosureDef {
    pub id: NodeID,
    pub vis: Visibility,
    pub context_type_id: NodeID,
    pub context_struct: StructDefRef,
    pub compiled_func_id: NodeID,
    pub context_arg_id: NodeID,
}

pub type ClosureDefRef = Rc<ClosureDef>;

impl ClosureDef {
    #[must_use]
    pub fn define(session: &Session, scope: ScopeRef, id: NodeID, vis: Visibility, name: &str, ttype: Option<Type>) -> Result<Def, Error> {
        let ctid = NodeID::generate();
        let structdef = StructDef::new_ref(Scope::new_ref(Some(scope.clone())));

        let def = Def::Closure(Rc::new(ClosureDef {
            id: id,
            vis: vis,
            context_type_id: ctid,
            context_struct: structdef,
            compiled_func_id: NodeID::generate(),
            context_arg_id: NodeID::generate(),
        }));

        AnyFunc::set_func_def(session, scope.clone(), id, vis, name, def.clone(), ttype)?;
        Ok(def)
    }

    pub fn add_field(&self, session: &Session, id: NodeID, name: &str, ttype: Type, define: Define) {
        self.context_struct.add_field(session, id, Mutability::Mutable, name, ttype, define);
    }

    pub fn find_or_add_field(&self, session: &Session, id: NodeID, name: &str, ttype: &Type) -> usize {
        // TODO you should probably modify this to use the IDs
        match self.context_struct.find_field_by_id(id) {
            Some((index, _)) => index,
            None => {
                self.add_field(session, id, name, ttype.clone(), Define::IfNotExists);
                self.context_struct.fields.borrow().len() - 1
            }
        }
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct MethodDef {
    pub id: NodeID,
    pub vis: Visibility,
    pub closure: ClosureDefRef,
}

pub type MethodDefRef = Rc<MethodDef>;

impl MethodDef {
    #[must_use]
    pub fn define(session: &Session, scope: ScopeRef, id: NodeID, vis: Visibility, name: &str, ttype: Option<Type>) -> Result<Def, Error> {
        let closure = match ClosureDef::define(session, scope, id, vis, name, ttype) {
            Ok(Def::Closure(cl)) => cl,
            result @ _ => return result,
        };

        let def = Def::Method(Rc::new(MethodDef {
            id: id,
            vis: vis,
            closure: closure,
        }));

        session.set_def(id, def.clone());
        Ok(def)
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct CFuncDef {
    pub id: NodeID,
    pub vis: Visibility,
    //pub ftype: Type,
}

pub type CFuncDefRef = Rc<CFuncDef>;

impl CFuncDef {
    #[must_use]
    pub fn define(session: &Session, scope: ScopeRef, id: NodeID, vis: Visibility, name: &str, ttype: Option<Type>) -> Result<Def, Error> {
        if scope.is_redirect() {
            return Err(Error::new(format!("DefError: cannot declare a C ABI function within a class body")));
        }

        if vis == Visibility::Anonymous {
            return Err(Error::new(format!("NameError: C ABI function declared without a name")));
        }

        let def = Def::CFunc(Rc::new(CFuncDef {
            id: id,
            vis: vis,
        }));

        scope.define(name, id)?;
        session.set_def(id, def.clone());
        if let Some(ttype) = ttype {
            session.set_type(id, ttype);
        }
        Ok(def)
    }
}


