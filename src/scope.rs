
use std::rc::Rc;
use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;

use crate::abi::ABI;
use crate::types::Type;
use crate::misc::{ UniqueID };
use crate::session::{ Session, Error };
use crate::defs::Def;

const NAMES: usize = 0;
const TYPES: usize = 1;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Context {
    Global,
    Block,
    Func(ABI, UniqueID),
    Enum(UniqueID),
    Class(UniqueID),
    TraitDef(UniqueID),
    TraitImpl(UniqueID, UniqueID),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    pub context: Cell<Context>,
    pub basename: RefCell<String>,
    pub namespaces: [RefCell<HashMap<String, UniqueID>>; 2],
    pub parent: Option<ScopeRef>,
}

pub type ScopeRef = Rc<Scope>;

impl Scope {
    pub fn new(name: &str, context: Context, parent: Option<ScopeRef>) -> Scope {
        Scope {
            context: Cell::new(context),
            basename: RefCell::new(String::from(name)),
            namespaces: [ RefCell::new(HashMap::new()), RefCell::new(HashMap::new()) ],
            parent: parent,
        }
    }

    pub fn new_ref(name: &str, context: Context, parent: Option<ScopeRef>) -> ScopeRef {
        Rc::new(Scope::new(name, context, parent))
    }

    pub fn get_context(&self) -> Context {
        self.context.get()
    }

    pub fn target(session: &Session, scope: ScopeRef) -> ScopeRef {
        match scope.context.get() {
            Context::Class(id) | Context::Enum(id) | Context::TraitDef(id) | Context::TraitImpl(_, id) => session.get_def(id).unwrap().get_vars().unwrap(),
            _ => scope,
        }
    }

    pub fn _modify_local<F>(&self, namespace: usize, name: &str, mut f: F) where F: FnMut(&mut UniqueID) {
        match self.namespaces[namespace].borrow_mut().get_mut(name) {
            None => panic!("NameError: undefined in this scope; {:?}", name),
            Some(entry) => f(entry),
        }
    }

    pub fn _search<F, U>(&self, namespace: usize, name: &str, f: F, default: U) -> U where F: Fn(UniqueID) -> U {
        if let Some(sym) = self.namespaces[namespace].borrow().get(name) {
            f(*sym)
        } else if let Some(parent) = &self.parent {
            parent._search(namespace, name, f, default)
        } else {
            default
        }
    }

    ///// Variable Functions /////

    pub fn define(&self, name: &str, defid: UniqueID) -> Result<(), Error> {
        let mut names = self.namespaces[NAMES].borrow_mut();
        match names.contains_key(name) {
            true => Err(Error::new(format!("NameError: variable is already defined; {:?}", name))),
            false => {
                names.insert(name.to_string(), defid);
                Ok(())
            },
        }
    }

    pub fn contains_local(&self, name: &str) -> bool {
        self.namespaces[NAMES].borrow().contains_key(name)
    }

    pub fn get_names(&self) -> HashMap<String, UniqueID> {
        self.namespaces[NAMES].borrow().clone()
    }

    pub fn set_var_def(&self, name: &str, defid: UniqueID) {
        self._modify_local(NAMES, name, move |sym| { *sym = defid; })
    }

    pub fn get_var_def(&self, name: &str) -> Option<UniqueID> {
        self._search(NAMES, name, |id| Some(id), None)
    }

    pub fn contains_context(&self, name: &str) -> bool {
        if self.namespaces[NAMES].borrow().get(name).is_some() {
            true
        } else {
            match (self.context.get(), &self.parent) {
                (Context::Block, Some(parent)) |
                (Context::Enum(_), Some(parent)) |
                (Context::Class(_), Some(parent)) |
                (Context::TraitDef(_), Some(parent)) => parent.contains_context(name),
                _ => false
            }
        }
    }

    pub fn foreach_name<F>(&self, f: F) where F: Fn(&str, UniqueID) {
        for (name, id) in self.namespaces[NAMES].borrow().iter() {
            f(&name, *id)
        }
    }

    ///// Type Functions /////

    pub fn define_type(&self, name: &str, defid: UniqueID) -> Result<(), Error> {
        let mut types = self.namespaces[TYPES].borrow_mut();
        match types.contains_key(name) {
            true => Err(Error::new(format!("NameError: type is already defined; {:?}", name))),
            false => {
                types.insert(name.to_string(), defid);
                Ok(())
            },
        }
    }

    pub fn get_type_def(&self, name: &str) -> Option<UniqueID> {
        self._search(TYPES, name, |id| Some(id), None)
    }

    pub fn find_type(&self, session: &Session, name: &str) -> Result<Type, Error> {
        self.get_type_def(&name).and_then(|defid| session.get_type(defid)).ok_or_else(|| Error::new(format!("TypeError: type not found: {:?}", name)))
    }

    pub fn find_type_def(&self, session: &Session, name: &str) -> Result<Def, Error> {
        session.get_def(self.get_type_def(&name).ok_or_else(|| Error::new(format!("TypeError: definition not set for {:?}", name)))?)
    }

    pub fn foreach_type<F>(&self, f: F) where F: Fn(&str, UniqueID) {
        for (name, id) in self.namespaces[TYPES].borrow().iter() {
            f(&name, *id)
        }
    }

    ///// Name Functions /////

    pub fn set_basename(&self, name: &str) {
        *self.basename.borrow_mut() = name.to_string();
    }

    pub fn get_full_name(&self, name: String) -> String {
        let mut base = self.get_basename();
        if &base != "" {
            base += "_";
        }
        base + &name
    }

    pub fn get_basename(&self) -> String {
        let name = self.parent.as_ref().map_or(String::from(""), |parent| parent.get_basename());
        if &name != "" {
            name +  "_" + &self.basename.borrow()
        } else {
            self.basename.borrow().clone()
        }
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct ScopeMapRef(RefCell<HashMap<UniqueID, ScopeRef>>);

impl ScopeMapRef {
    pub const GLOBAL: UniqueID = UniqueID(1);

    pub fn new() -> Self {
        ScopeMapRef(RefCell::new(HashMap::new()))
    }

    pub fn get_or_add(&self, id: UniqueID, name: &str, context: Context, parent: Option<ScopeRef>) -> ScopeRef {
        let tscope = self.0.borrow().get(&id).cloned();
        match tscope {
            Some(scope) => scope,
            None => self.add(id, name, context, parent)
        }
    }

    pub fn add(&self, id: UniqueID, name: &str, context: Context, parent: Option<ScopeRef>) -> ScopeRef {
        let scope = Scope::new_ref(name, context, parent);
        self.0.borrow_mut().insert(id, scope.clone());
        scope
    }

    pub fn get(&self, id: UniqueID) -> Option<ScopeRef> {
        self.0.borrow().get(&id).cloned()
    }

    pub fn get_global(&self) -> ScopeRef {
        self.get(ScopeMapRef::GLOBAL).unwrap()
    }
}

