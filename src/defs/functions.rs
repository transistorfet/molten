
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


pub struct AnyFunc();

impl AnyFunc {
    pub fn define(session: &Session, scope: ScopeRef, id: NodeID, name: &Option<String>, abi: ABI, ttype: Option<Type>) -> Result<Def, Error> {
        match abi {
            ABI::C => CFuncDef::define(session, scope.clone(), id, name, ttype),
            ABI::Molten => {
                if scope.is_redirect() && name.is_some() {
                    MethodDef::define(session, scope.clone(), id, name, ttype)
                } else {
                    FuncDef::define(session, scope.clone(), id, name, ttype)
                    //ClosureDef::define(session, scope.clone(), id, name, ttype)
                }
            },
            _ => return Err(Error::new(format!("DefError: unsupported ABI {:?}", abi))),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FuncDef {

}

pub type FuncDefRef = Rc<FuncDef>;

impl FuncDef {
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
                dscope.define(name.clone(), Some(id))?;
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

    pub fn define(session: &Session, scope: ScopeRef, name: &String, id: NodeID, previd: NodeID, ttype: Option<Type>) -> Result<(), Error> {
        let dscope = Scope::target(session, scope.clone());

        if dscope.contains_local(&name) {
            match session.get_def(previd) {
                Ok(Def::Overload(prev)) => {
                    prev.add_variant(session, id);
                },
                Ok(Def::Func(_)) |
                Ok(Def::Closure(_)) |
                Ok(Def::Method(_)) => {
                    let defid = OverloadDef::create(session, None, vec!(previd, id));
                    dscope.set_var_def(&name, defid);
                }
                _ => return Err(Error::new(format!("NameError: unable to overload {} because of a previous definitions", name)))
            }
        } else {
            let defid = OverloadDef::create(session, Some(previd), vec!(id));
            dscope.define(name.clone(), Some(defid))?;
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

    pub fn find_variant(&self, session: &Session, tscope: ScopeRef, atypes: Type) -> Result<(NodeID, Type), Error> {
        let (mut found, variant_types) = self.find_all_variants(session, tscope, atypes.clone());

        match found.len() {
            0 => Err(Error::new(format!("OverloadError: No valid variant found for {}\n\tout of [{}]", atypes, Type::display_vec(&variant_types)))),
            1 => Ok(found.remove(0)),
            _ => Err(Error::new(format!("OverloadError: Ambiguous {}\n\tvariants found [{}]", atypes, found.iter().map(|(i, t)| format!("{}", t)).collect::<Vec<String>>().join(", ")))),
        }
    }

    pub fn find_all_variants(&self, session: &Session, tscope: ScopeRef, atypes: Type) -> (Vec<(NodeID, Type)>, Vec<Type>) {
        let variants = self.get_variants(session);
        self.find_variants_of(variants, session, tscope, atypes)
    }

    pub fn find_local_variants(&self, session: &Session, tscope: ScopeRef, atypes: Type) -> (Vec<(NodeID, Type)>, Vec<Type>) {
        let variants = self.variants.borrow().clone();
        self.find_variants_of(variants, session, tscope, atypes)
    }

    pub fn find_variants_of(&self, variants: Vec<NodeID>, session: &Session, tscope: ScopeRef, atypes: Type) -> (Vec<(NodeID, Type)>, Vec<Type>) {
        let mut found = vec!();
        let mut variant_types = vec!();
        for id in variants {
            // Fetch the variant's type and map its typevars if necessary
            let ttype = match session.get_type(id) {
                Some(vtype @ Type::Variable(_, _)) => vtype,
                Some(ttype) => tscope.map_all_typevars(session, ttype),
                None => tscope.new_typevar(session)
            };

            if let Type::Function(ref btypes, _, _) = ttype {
                if check_type(session, tscope.clone(), Some(*btypes.clone()), Some(atypes.clone()), Check::Def, false).is_ok() {
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
    pub id: UniqueID,
    pub varid: UniqueID,
    pub name: String,
    pub contextid: NodeID,
    pub contexttype: Type,
    pub context: StructDefRef,
}

pub type ClosureDefRef = Rc<ClosureDef>;

impl ClosureDef {
    pub fn define(session: &Session, scope: ScopeRef, id: NodeID, name: &Option<String>, ttype: Option<Type>) -> Result<Def, Error> {

        // TODO this isn't correct, it needs to mangle
        let fname = scope.get_full_name(name.clone(), id);

        // create and register new closure context type
        let ctid = NodeID::generate();
        // TODO the name is sloppy here
        let ctype = Type::Object(format!("{}_context_{}", fname, ctid), ctid, vec!());
        let structdef = StructDef::define(session, scope.clone(), ctid, ctype.clone())?;

        let def = Def::Closure(Rc::new(ClosureDef {
            id: id,
            varid: NodeID::generate(),
            name: fname,
            contextid: ctid,
            contexttype: ctype,
            context: structdef,
        }));

        FuncDef::set_func_def(session, scope.clone(), id, name, def.clone(), ttype)?;
        Ok(def)
    }

    pub fn add_field(&self, session: &Session, name: &str, ttype: Type) {
        self.context.add_field(session, false, name, ttype);
    }

    // TODO I don't like this
    pub fn add_context_to_ftype(&self, session: &Session) -> Type {
        match session.get_type(self.id) {
            Some(Type::Function(mut args, ret, abi)) => {
                let args = match *args {
                    Type::Tuple(mut items) => {
                        items.insert(0, self.contexttype.clone());
                        Type::Tuple(items)
                    },
                    ttype @ _ => Type::Tuple(vec!(self.contexttype.clone(), ttype)),
                };
                let ftype = Type::Function(Box::new(args), ret, abi);
                session.set_type(self.id, ftype.clone());
                ftype
            },
            ttype @ _ => panic!("Unexpected type when transforming closure: expected function but got {:?}", ttype),
        }
    }
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

mod llvm {
    //use llvm::compiler as llvm;

    /*
    impl CompileInvoke for MethodDef {
        fn compile(&self, ) -> Box<Compilable> {
            // this is a method, so a method call
            let lfunc = llvm::compile(session, 
        }
    }
    */

    /*
    impl Compilable for MethodDef {
        fn box_clone(&self) -> Box<Compilable> {
            Box::new((*self).clone())
        }

        fn get_ref(&self) -> LLVMValueRef {
            self.0
        }
    }
    */
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

        scope.define(name.clone(), Some(id))?;
        session.set_def(id, def.clone());
        if let Some(ttype) = ttype {
            session.set_type(id, ttype);
        }
        Ok(def)
    }
}


