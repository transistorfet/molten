
use std::rc::Rc;
use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;

use crate::types::Type;
use crate::hir::{ NodeID };
use crate::session::{ Session, Error };
use crate::misc::{ UniqueID };
use crate::defs::Def;

const NAMES: usize = 0;
const TYPES: usize = 1;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Context {
    Global,
    Local,
    Object,
    Block,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    pub context: Cell<Context>,
    pub basename: RefCell<String>,
    pub namespaces: [RefCell<HashMap<String, NodeID>>; 2],
    pub parent: Option<ScopeRef>,
}

pub type ScopeRef = Rc<Scope>;

impl Scope {
    pub fn new(parent: Option<ScopeRef>) -> Scope {
        Scope {
            context: Cell::new(Context::Local),
            basename: RefCell::new(String::from("")),
            namespaces: [ RefCell::new(HashMap::new()), RefCell::new(HashMap::new()) ],
            parent: parent,
        }
    }

    pub fn new_ref(parent: Option<ScopeRef>) -> ScopeRef {
        Rc::new(Scope::new(parent))
    }

    pub fn set_context(&self, context: Context) {
        self.context.set(context);
    }

    pub fn set_redirect(&self, value: bool) {
        self.context.set(if value { Context::Object } else { Context::Local });
    }

    pub fn is_global(&self) -> bool {
        self.context.get() == Context::Global
    }

    pub fn is_redirect(&self) -> bool {
        self.context.get() == Context::Object
    }

    pub fn target(session: &Session, scope: ScopeRef) -> ScopeRef {
        match scope.context.get() {
            Context::Object => scope.find_type_def(session, &scope.basename.borrow()).unwrap().get_vars().unwrap(),
            _ => scope,
        }
    }

    pub fn global(scope: ScopeRef) -> ScopeRef {
        if scope.is_global() {
            scope
        } else {
            Scope::global(scope.parent.clone().unwrap())
        }
    }

    pub fn _modify_local<F>(&self, namespace: usize, name: &str, mut f: F) where F: FnMut(&mut NodeID) {
        match self.namespaces[namespace].borrow_mut().get_mut(name) {
            None => panic!("NameError: undefined in this scope; {:?}", name),
            Some(entry) => f(entry),
        }
    }

    pub fn _search<F, U>(&self, namespace: usize, name: &str, f: F, default: U) -> U where F: Fn(NodeID) -> U {
        if let Some(sym) = self.namespaces[namespace].borrow().get(name) {
            f(*sym)
        } else if let Some(parent) = &self.parent {
            parent._search(namespace, name, f, default)
        } else {
            default
        }
    }

    ///// Variable Functions /////

    pub fn define(&self, name: &str, defid: NodeID) -> Result<(), Error> {
        let mut names = self.namespaces[NAMES].borrow_mut();
        match names.contains_key(name) {
            true => Err(Error::new(format!("NameError: variable is already defined; {:?}", name))),
            false => {
                names.insert(name.to_string(), defid);
                Ok(())
            },
        }
    }

    pub fn contains(&self, name: &str) -> bool {
        self._search(NAMES, name, |_| true, false)
    }

    pub fn contains_local(&self, name: &str) -> bool {
        self.namespaces[NAMES].borrow().contains_key(name)
    }

    pub fn get_names(&self) -> HashMap<String, NodeID> {
        self.namespaces[NAMES].borrow().clone()
    }

    pub fn set_var_def(&self, name: &str, defid: NodeID) {
        self._modify_local(NAMES, name, move |sym| { *sym = defid; })
    }

    pub fn get_var_def(&self, name: &str) -> Option<NodeID> {
        self._search(NAMES, name, |id| Some(id), None)
    }

    pub fn contains_context(&self, name: &str) -> bool {
        if self.namespaces[NAMES].borrow().get(name).is_some() {
            true
        } else {
            match (self.context.get(), &self.parent) {
                (Context::Block, Some(parent)) |
                (Context::Object, Some(parent)) => parent.contains_context(name),
                _ => false
            }
        }
    }

    pub fn foreach_name<F>(&self, f: F) where F: Fn(&str, NodeID) {
        for (name, id) in self.namespaces[NAMES].borrow().iter() {
            f(&name, *id)
        }
    }

    ///// Type Functions /////

    pub fn define_type(&self, name: &str, defid: NodeID) -> Result<(), Error> {
        let mut types = self.namespaces[TYPES].borrow_mut();
        match types.contains_key(name) {
            true => Err(Error::new(format!("NameError: type is already defined; {:?}", name))),
            false => {
                types.insert(name.to_string(), defid);
                Ok(())
            },
        }
    }

    pub fn get_type_def(&self, name: &str) -> Option<NodeID> {
        self._search(TYPES, name, |id| Some(id), None)
    }

    pub fn find_type(&self, session: &Session, name: &str) -> Result<Type, Error> {
        self.get_type_def(&name).and_then(|defid| session.get_type(defid)).ok_or_else(|| Error::new(format!("TypeError: type not found: {:?}", name)))
    }

    pub fn find_type_def(&self, session: &Session, name: &str) -> Result<Def, Error> {
        session.get_def(self.get_type_def(&name).ok_or_else(|| Error::new(format!("TypeError: definition not set for {:?}", name)))?)
    }

    pub fn foreach_type<F>(&self, f: F) where F: Fn(&str, NodeID) {
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

    pub fn get_or_add(&self, id: UniqueID, parent: Option<ScopeRef>) -> ScopeRef {
        let tscope = self.0.borrow().get(&id).cloned();
        match tscope {
            Some(scope) => scope,
            None => self.add(id, parent)
        }
    }

    pub fn add(&self, id: UniqueID, parent: Option<ScopeRef>) -> ScopeRef {
        let scope = Scope::new_ref(parent);
        self.0.borrow_mut().insert(id, scope.clone());
        scope
    }

    pub fn set(&self, id: UniqueID, scope: ScopeRef) {
        self.0.borrow_mut().insert(id, scope);
    }

    pub fn get(&self, id: UniqueID) -> Option<ScopeRef> {
        self.0.borrow().get(&id).cloned()
    }

    pub fn get_global(&self) -> ScopeRef {
        self.get(ScopeMapRef::GLOBAL).unwrap()
    }
}

