
use std::rc::Rc;
use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::hash_map::Entry;

use types::Type;
use ast::{ NodeID };
use session::{ Session, Error };
use misc::{ r, UniqueID };
use defs::Def;


#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Context {
    Primative,
    Global,
    Local,
    Object,
    Block,
}


#[derive(Clone, Debug, PartialEq)]
pub struct BindInfo {
    pub defid: Option<NodeID>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    pub context: Cell<Context>,
    pub basename: RefCell<String>,
    pub names: RefCell<HashMap<String, BindInfo>>,
    pub types: RefCell<HashMap<String, BindInfo>>,
    pub parent: Option<ScopeRef>,
}

pub type ScopeRef = Rc<Scope>;
//pub type ScopeRef = Rc<RefCell<Scope>>;
//#[derive(Clone, Debug, PartialEq)]
//pub struct ScopeRef(Rc<RefCell<Scope>>);
//pub struct ScopeRef(&'sess ScopeMapRef, UniqueID);


impl Scope {
    pub fn new(parent: Option<ScopeRef>) -> Scope {
        Scope {
            context: Cell::new(Context::Local),
            basename: RefCell::new(String::from("")),
            names: RefCell::new(HashMap::new()),
            types: RefCell::new(HashMap::new()),
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

    pub fn is_primative(&self) -> bool {
        self.context.get() == Context::Primative
    }

    pub fn is_global(&self) -> bool {
        self.context.get() == Context::Global || self.context.get() == Context::Primative
    }

    pub fn is_local(&self) -> bool {
        self.context.get() == Context::Local
    }

    pub fn is_redirect(&self) -> bool {
        self.context.get() == Context::Object
    }

    //pub fn set_parent(&mut self, parent: ScopeRef) {
    //    self.parent = Some(parent);
    //}

    pub fn get_parent(&self) -> Option<ScopeRef> {
        self.parent.clone()
    }

    pub fn target(session: &Session, scope: ScopeRef) -> ScopeRef {
        match scope.context.get() {
            Context::Object => scope.find_type_def(session, &scope.basename.borrow()).unwrap().get_vars().unwrap(),
            _ => scope,
        }
    }

    ///// Variable Functions /////

    #[must_use]
    pub fn define(&self, name: String, defid: Option<NodeID>) -> Result<(), Error> {
        let mut names = self.names.borrow_mut();
        match names.contains_key(&name) {
            true => Err(Error::new(format!("NameError: variable is already defined; {:?}", name))),
            false => {
                names.insert(name, BindInfo {
                    defid: defid,
                });
                Ok(())
            },
        }
    }

    pub fn modify_local<F>(&self, name: &String, mut f: F) -> () where F: FnMut(&mut BindInfo) -> () {
        match self.names.borrow_mut().entry(name.clone()) {
            Entry::Vacant(_) => panic!("NameError: variable is undefined in this scope; {:?}", name),
            Entry::Occupied(mut entry) => f(entry.get_mut()),
        }
    }

    /*
    pub fn modify<F>(&mut self, name: &String, mut f: F) -> () where F: FnMut(&mut BindInfo) -> () {
        // TODO this might be an issue with overloaded functions; this was changed to make overloading work when in different scopes, which might cause a name/type error if something in a
        // more global scope tries to access an overloaded type specified in a more local scope... maybe the solution is to create a new entry with the new variant only accessible from the
        // more local scope... but will that cause a duplicate name error somewhere?
        match self.names.entry(name.clone()) {
            Entry::Occupied(mut entry) => f(entry.get_mut()),
            _ => match self.parent {
                Some(ref parent) => parent.modify(name, f),
                _ => panic!("NameError: type is undefined; {:?}", name),
            },
        }
    }
    */

    pub fn _search<F, U>(&self, name: &String, f: F) -> Option<U> where F: Fn(&BindInfo) -> Option<U> {
        if let Some(sym) = self.names.borrow().get(name) {
            f(sym)
        } else if let Some(ref parent) = self.parent {
            parent._search(name, f)
        } else {
            None
        }
    }

    pub fn contains(&self, name: &String) -> bool {
        match self._search(name, |_sym| Some(true)) {
            Some(true) => true,
            _ => false,
        }
    }

    pub fn contains_local(&self, name: &String) -> bool {
        self.names.borrow().contains_key(name)
    }

    pub fn contains_context(&self, name: &String) -> bool {
        if let Some(_) = self.names.borrow().get(name) {
            true
        } else {
            match (self.context.get(), &self.parent) {
                (Context::Block, Some(ref parent)) |
                (Context::Object, Some(ref parent)) => parent.contains_context(name),
                _ => false
            }
        }
    }

    pub fn set_var_def(&self, name: &String, defid: NodeID) {
        self.modify_local(name, move |sym| { sym.defid = Some(defid.clone()); })
    }

    pub fn get_var_def(&self, name: &String) -> Option<NodeID> {
        self._search(name, |sym| {
            match sym.defid.as_ref() {
                Some(defid) => Some(defid.clone()),
                //None => Err(Error::new(format!("VarError: definition not set for {:?}", name))),
                None => None,
            }
        })
    }

    pub fn find_var_def(&self, session: &Session, name: &String) -> Result<Def, Error> {
        session.get_def(self.get_var_def(name).ok_or(Error::new(format!("VarError: definition not set for {:?}", name)))?)
    }


    ///// Type Functions /////

    #[must_use]
    pub fn define_type(&self, name: String, defid: Option<NodeID>) -> Result<(), Error> {
        let mut types = self.types.borrow_mut();
        match types.contains_key(&name) {
            true => Err(Error::new(format!("NameError: type is already defined; {:?}", name))),
            false => {
                types.insert(name, BindInfo {
                    defid: defid,
                });
                Ok(())
            },
        }
    }

    pub fn contains_type(&self, name: &String) -> bool {
        match self._search_type(name, |_info| Some(true)) {
            Some(true) => true,
            _ => false,
        }
    }

    pub fn contains_type_local(&self, name: &String) -> bool {
        self.types.borrow().contains_key(name)
    }

    pub fn _modify_type<F>(&self, name: &String, f: F) where F: Fn(&mut BindInfo) -> () {
        match self.types.borrow_mut().entry(name.clone()) {
            Entry::Occupied(mut entry) => f(entry.get_mut()),
            _ => match self.parent {
                Some(ref parent) => {
                    parent._modify_type(name, f);
                },
                _ => panic!("NameError: type is undefined; {:?}", name),
            },
        }
    }

    fn _search_type<F, U>(&self, name: &String, f: F) -> Option<U> where F: Fn(&BindInfo) -> Option<U> {
        if let Some(ref info) = self.types.borrow().get(name) {
            f(info)
        } else if let Some(ref parent) = self.parent {
            parent._search_type(name, f)
        } else {
            None
        }
    }

    pub fn find_type(&self, session: &Session, name: &String) -> Option<Type> {
        match self.get_type_def(name) {
            Some(defid) => session.get_type(defid),
            None => None,
        }
    }

    pub fn find_type_local(&self, session: &Session, name: &String) -> Option<Type> {
        match self.types.borrow().get(name).map(|info| info.defid) {
            Some(Some(defid)) => session.get_type(defid),
            _ => None,
        }
    }

    pub fn set_type_def(&self, name: &String, defid: NodeID) {
        self._modify_type(name, move |info| {
            info.defid = Some(defid.clone());
        })
    }

    pub fn get_type_def(&self, name: &String) -> Option<NodeID> {
        self._search_type(name, |info| {
            match info.defid.as_ref() {
                Some(defid) => Some(defid.clone()),
                //None => Err(Error::new(format!("TypeError: definition not set for {:?}", name))),
                None => None,
            }
        })
    }

    pub fn find_type_def(&self, session: &Session, name: &String) -> Result<Def, Error> {
        session.get_def(self.get_type_def(name).ok_or(Error::new(format!("TypeError: definition not set for {:?}", name)))?)
    }



    pub fn make_obj(&self, session: &Session, name: String, params: Vec<Type>) -> Result<Type, Error> {
        match self.find_type(session, &name) {
            Some(Type::Object(_, id, eparams)) => {
                if eparams.len() != params.len() {
                    return Err(Error::new(format!("TypeError: type parameters don't match.  Expected {:?} but found {:?}", eparams, params)));
                }
                Ok(Type::Object(name, id, params))
            },
            Some(ttype) => Err(Error::new(format!("TypeError: expected object type but found {:?}", ttype))),
            None => Err(Error::new(format!("TypeError: type not found: {:?}", name)))
        }
    }

    pub fn global(scope: ScopeRef) -> ScopeRef {
        if scope.is_global() {
            scope
        } else {
            Scope::global(scope.parent.clone().unwrap())
        }
    }


    ////// Type Variable Functions //////

    pub fn map_all_typevars(&self, session: &Session, ttype: Type) -> Type {
        let mut varmap = Scope::map_new();
        debug!("MAPPING ALL: {:?}", ttype);
        self.map_typevars(session, &mut varmap, ttype)
    }

    pub fn map_new() -> HashMap<UniqueID, Type> {
        HashMap::new()
    }

    pub fn map_typevars(&self, session: &Session, varmap: &mut HashMap<UniqueID, Type>, ttype: Type) -> Type {
        match ttype {
            Type::Variable(name, id, existential) => {
                match varmap.get(&id).map(|x| x.clone()) {
                    Some(ptype) => ptype,
                    None => {
                        let etype = self.find_type(session, &name);
                        debug!("EXISTING TYPEVAR for {:?}: {:?} vs {:?}", name, etype, id);
                        match etype {
                            Some(Type::Variable(_, ref eid, _)) if *eid == id && !existential => etype.clone().unwrap(),
                            None | Some(Type::Variable(_, _, _)) => {
                                let orgtype = Type::Variable(name.clone(), id, existential);
                                if existential {
                                    let maptype = self.new_typevar(session, false);
                                    varmap.insert(id, maptype.clone());
                                    debug!("MAPPED from {:?} to {:?}", orgtype, maptype);
                                    maptype
                                } else {
                                    debug!("NOT MAPPED: {:?}", orgtype);
                                    orgtype.clone()
                                }
                            },
                            _ => etype.clone().unwrap(),
                        }
                    }
                }
            },
            Type::Function(args, ret, abi) => Type::Function(r(self.map_typevars(session, varmap, *args)), r(self.map_typevars(session, varmap, *ret)), abi),
            // TODO why did I do this?  Was it because of a bug or just to reduce typevars, because it caused another bug with constructors
            //Type::Function(args, ret, abi) => Type::Function(r(self.map_typevars(session, varmap, *args)), ret, abi),
            Type::Tuple(types) => Type::Tuple(self.map_typevars_vec(session, varmap, types)),
            Type::Record(types) => Type::Record(types.into_iter().map(|(n, t)| (n, self.map_typevars(session, varmap, t))).collect()),
            Type::Ambiguous(variants) => Type::Ambiguous(self.map_typevars_vec(session, varmap, variants)),
            Type::Ref(ttype) => Type::Ref(r(self.map_typevars(session, varmap, *ttype))),
            Type::Object(name, id, types) => Type::Object(name.clone(), id, self.map_typevars_vec(session, varmap, types)),
        }
    }

    pub fn map_typevars_vec(&self, session: &Session, varmap: &mut HashMap<UniqueID, Type>, types: Vec<Type>) -> Vec<Type> {
        types.into_iter().map(|vtype| self.map_typevars(session, varmap, vtype)).collect()
    }

    pub fn new_typevar(&self, session: &Session, existential: bool) -> Type {
        let id = UniqueID::generate();
        //if id == UniqueID(903) { panic!("") }
        let name = self.new_typevar_name();
        let ttype = Type::Variable(name.clone(), id, existential);

        //self.define_type(name, ttype.clone()).unwrap();
        session.set_type(id, ttype.clone());
        debug!("NEW TYPEVAR: {:?} {:?}", self.get_basename(), ttype);
        ttype
    }

    pub fn new_typevar_name(&self) -> String {
        for ch1 in b'`' .. b'z' {
            let name = if ch1 == b'`' { String::from("") } else { (ch1 as char).to_string() };
            for ch2 in b'a' .. b'z' + 1 {
                let name = name.clone() + &(ch2 as char).to_string();
                if !self.contains_type(&name) {
                    return name;
                }
            }
        }
        panic!("SillyError: ran out of type variable names");
    }

    ///// Name Functions /////

    pub fn set_basename(&self, name: String) {
        *self.basename.borrow_mut() = name;
    }

    pub fn get_full_name(&self, name: Option<String>, id: UniqueID) -> String {
        let mut base = self.get_basename();
        if base.as_str() != "" {
            base = base + &"_";
        }
        base + &name.unwrap_or_else(|| format!("anon{}", id))
    }

    pub fn get_basename(&self) -> String {
        let name = self.parent.as_ref().map_or(String::from(""), |parent| parent.get_basename());
        if name.as_str() != "" {
            name +  &"_" + &self.basename.borrow()
        } else {
            self.basename.borrow().clone()
        }
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct ScopeMapRef(RefCell<HashMap<UniqueID, ScopeRef>>);

impl ScopeMapRef {
    pub const PRIMATIVE: UniqueID = UniqueID(0);
    pub const GLOBAL: UniqueID = UniqueID(1);

    pub fn new() -> Self {
        ScopeMapRef(RefCell::new(HashMap::new()))
    }

    pub fn get_or_add(&self, id: UniqueID, parent: Option<ScopeRef>) -> ScopeRef {
        let tscope = self.0.borrow().get(&id).map(|s| s.clone());
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

    pub fn get(&self, id: &UniqueID) -> ScopeRef {
        self.0.borrow().get(id).unwrap().clone()
    }

    pub fn get_global(&self) -> ScopeRef {
        self.get(&ScopeMapRef::GLOBAL)
    }

    //pub fn foreach<F>(&self, f: F) where F: Fn(ScopeRef) -> () {
    //    for (_, scope) in self.0.borrow().iter() {
    //        f(scope);
    //    }
    //}
}

