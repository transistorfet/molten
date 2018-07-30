
use std::str;
use std::rc::Rc;
use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::hash_map::Entry;

use parser;
use ast::Pos;
use abi::ABI;
use types::Type;
use session::Error;
use utils::UniqueID;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Context {
    Primative,
    Global,
    Local,
    Class,
}


pub type VarID = UniqueID;

#[derive(Clone, Debug, PartialEq)]
pub struct VarInfo {
    pub id: VarID,
    pub ttype: Option<Type>,
    pub funcdefs: i32,
    pub abi: Option<ABI>,
}


pub type TypeID = UniqueID;

#[derive(Clone, Debug, PartialEq)]
pub struct TypeInfo<'sess> {
    pub id: TypeID,
    pub ttype: Type,
    pub classdef: Option<ClassDef<'sess>>,
    pub parent: Option<(String, Vec<Type>)>,
}


#[derive(Clone, Debug, PartialEq)]
pub struct Scope<'sess> {
    pub context: Cell<Context>,
    pub basename: RefCell<String>,
    pub names: RefCell<HashMap<String, VarInfo>>,
    pub types: RefCell<HashMap<String, TypeInfo<'sess>>>,
    pub parent: Option<ScopeRef<'sess>>,
}

pub type ScopeRef<'sess> = &'sess Scope<'sess>;
//pub type ScopeRef = Rc<RefCell<Scope>>;
//#[derive(Clone, Debug, PartialEq)]
//pub struct ScopeRef(Rc<RefCell<Scope>>);
//pub struct ScopeRef<'sess>(&'sess ScopeMapRef, UniqueID);

pub type ClassDef<'sess> = Rc<Scope<'sess>>;


impl<'sess> Scope<'sess> {
    pub fn new(parent: Option<ScopeRef>) -> Scope {
        Scope {
            context: Cell::new(Context::Local),
            basename: RefCell::new(String::from("")),
            names: RefCell::new(HashMap::new()),
            types: RefCell::new(HashMap::new()),
            parent: parent,
        }
    }

    pub fn set_context(&self, context: Context) {
        self.context.set(context);
    }

    pub fn set_class(&self, value: bool) {
        self.context.set(if value { Context::Class } else { Context::Local });
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

    //pub fn set_parent(&mut self, parent: ScopeRef<'sess>) {
    //    self.parent = Some(parent);
    //}

    pub fn get_parent(&self) -> Option<ScopeRef<'sess>> {
        self.parent
    }

    pub fn target(scope: ScopeRef<'sess>) -> ScopeRef<'sess> {
        match scope.context.get() {
            Context::Class => scope.get_self_class_def().unwrap().as_ref(),
            _ => scope,
        }
    }

    ///// Variable Functions /////

    #[must_use]
    pub fn define(&self, name: String, ttype: Option<Type>) -> Result<VarID, Error> {
        let mut names = self.names.borrow_mut();
        match names.contains_key(&name) {
            true => Err(Error::new(format!("NameError: variable is already defined; {:?}", name))),
            false => {
                let id = VarID::generate();
                names.insert(name, VarInfo {
                    id: id,
                    ttype: ttype,
                    funcdefs: 0,
                    abi: None
                });
                Ok(id)
            },
        }
    }

    #[must_use]
    pub fn define_func(&self, name: String, ttype: Option<Type>, abi: ABI) -> Result<VarID, Error> {
        let funcs = self.search(&name, |sym| Some(sym.funcdefs)).unwrap_or(0);
        match self.names.borrow_mut().entry(name.clone()) {
            Entry::Vacant(entry) => {
                let id = VarID::generate();
                entry.insert(VarInfo {
                    id: id,
                    ttype: ttype,
                    funcdefs: funcs + 1,
                    abi: Some(abi)
                });
                Ok(id)
            },
            Entry::Occupied(mut entry) => match entry.get().funcdefs {
                0 => Err(Error::new(format!("NameError: variable is already defined as a non-function; {:?}", name))),
                // TODO we don't really do the right this with type here, but we don't have a scoperef to pass to Type::add_variant
                _ => {
                    if entry.get().abi.clone().unwrap_or(ABI::Unknown) != abi || !abi.can_overload() {
                        Err(Error::new(format!("NameError: variable already defined with abi {:?} and can't be overloaded; {:?}", abi, name)))
                    } else {
                        entry.get_mut().funcdefs += 1;
                        Ok(entry.get().id)
                    }
                },
            },
        }
    }

    #[must_use]
    pub fn define_func_variant(dscope: ScopeRef<'sess>, name: String, tscope: ScopeRef<'sess>, ftype: Type) -> Result<VarID, Error> {
        let abi = ftype.get_abi().unwrap_or(ABI::Molten);
        let id = dscope.define_func(name.clone(), None, abi)?;
        Scope::add_func_variant(dscope, &name, tscope, ftype)?;
        Ok(id)
    }

    pub fn modify_local<F>(&self, name: &String, mut f: F) -> () where F: FnMut(&mut VarInfo) -> () {
        match self.names.borrow_mut().entry(name.clone()) {
            Entry::Vacant(_) => panic!("NameError: variable is undefined in this scope; {:?}", name),
            Entry::Occupied(mut entry) => f(entry.get_mut()),
        }
    }

    /*
    pub fn modify<F>(&mut self, name: &String, mut f: F) -> () where F: FnMut(&mut VarInfo) -> () {
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

    pub fn search<F, U>(&self, name: &String, f: F) -> Option<U> where F: Fn(&VarInfo) -> Option<U> {
        if let Some(sym) = self.names.borrow().get(name) {
            f(sym)
        } else if let Some(ref parent) = self.parent {
            parent.search(name, f)
        } else {
            None
        }
    }

    pub fn contains(&self, name: &String) -> bool {
        match self.search(name, |_sym| Some(true)) {
            Some(true) => true,
            _ => false,
        }
    }

    pub fn contains_local(&self, name: &String) -> bool {
        self.names.borrow().contains_key(name)
    }

    pub fn variable_id(&self, name: &String) -> Result<VarID, Error> {
        match self.search(name, |sym| Some(sym.id)) {
            Some(id) => Ok(id),
            None => Err(Error::new(format!("NameError: variable is undefined; {:?}", name))),
        }
    }

    pub fn set_variable_type(&self, name: &String, ttype: Type) {
        self.modify_local(name, move |sym| sym.ttype = Some(ttype.clone()))
    }

    pub fn get_variable_type(&self, name: &String) -> Option<Type> {
        self.get_variable_type_full(name, false)
    }

    pub fn get_variable_type_full(&self, name: &String, local: bool) -> Option<Type> {
        let otype = self.search(name, |sym| {
            if sym.funcdefs > 1 {
                Some(Some(Type::Overload(self.get_all_variants(name, local))))
            } else {
                Some(sym.ttype.clone())
            }
        });
        match otype {
            Some(ttype) => ttype,
            None => panic!("NameError: variable is undefined; {:?}", name),
        }
    }

    pub fn get_all_variants(&self, name: &String, local: bool) -> Vec<Type> {
        let mut variants = self.names.borrow().get(name).as_ref().map(|sym| sym.ttype.as_ref().map(|ttype| ttype.get_variants()).unwrap_or(vec!())).unwrap_or(vec!());
        if !local {
            variants.extend(self.parent.as_ref().map(|parent| parent.get_all_variants(name, local)).unwrap_or(vec!()));
        }
        variants
    }

    /*
    pub fn get_all_variants(&self, name: &String, local: bool) -> Vec<Type> {
        let mut variants = self.names.get(name).as_ref().map(|sym| sym.ttype.as_ref().map(|ttype| ttype.get_variants()).unwrap_or(vec!())).unwrap_or(vec!());
        if !local && self.parent.is_some() {
            let parent = self.parent.clone().unwrap();
            //variants.extend(self.parent.as_ref().map(|parent| parent.borrow().get_all_variants(name, local)).unwrap_or(vec!()));
            'outer: for variant in parent.borrow().get_all_variants(name, local) {
                for existing in &variants {
                    if check_type(parent.clone(), Some(existing.clone()), Some(variant.clone()), Check::Def, false).is_ok() {
                        continue 'outer;
                    }
                }
                variants.push(variant);
            }
        }
        variants
    }
    */

    #[must_use]
    pub fn add_func_variant(dscope: ScopeRef<'sess>, name: &String, tscope: ScopeRef<'sess>, ftype: Type) -> Result<(), Error> {
        let otype = dscope.get_variable_type_full(name, true);
        let ftype = match otype {
            Some(ttype) => ttype.add_variant(tscope, ftype.clone())?,
            None => ftype,
        };
        dscope.set_variable_type(name, ftype.clone());
        Ok(())
    }

    pub fn num_funcdefs(&self, name: &String) -> i32 {
        self.search(name, |sym| Some(sym.funcdefs)).unwrap_or(0)
    }


    pub fn is_closure_var(&self, name: &String) -> bool {
        if self.names.borrow().contains_key(name) {
            return false;
        }

        let mut parent = self.parent;
        while parent.is_some() {
            let scope = parent.unwrap();
            if scope.is_global() {
                return false;
            }
            if scope.contains_local(name) {
                return true;
            }
            parent = scope.parent;
        }
        false
    }


    ///// Type Functions /////

    #[must_use]
    pub fn define_type(&self, name: String, ttype: Type) -> Result<TypeID, Error> {
        let types = self.types.borrow_mut();
        match types.contains_key(&name) {
            true => Err(Error::new(format!("NameError: type is already defined; {:?}", name))),
            false => {
                let id = TypeID::generate();
                types.insert(name, TypeInfo {
                    id: id,
                    ttype: ttype,
                    classdef: None,
                    parent: None,
                });
                Ok(id)
            },
        }
    }

    pub fn contains_type(&'sess self, name: &String) -> bool {
        match self._search_type(name, |_info| Some(true)) {
            Some(true) => true,
            _ => false,
        }
    }

    pub fn contains_type_local(&self, name: &String) -> bool {
        self.types.borrow().contains_key(name)
    }

    pub fn modify_type<'a, F>(&'a self, name: &String, f: F) where F: Fn(&'a mut TypeInfo<'sess>) -> () {
        match self.types.borrow_mut().entry(name.clone()) {
            Entry::Occupied(mut entry) => f(entry.get_mut()),
            _ => match self.parent {
                Some(ref parent) => {
                    parent.modify_type(name, f);
                },
                _ => panic!("NameError: type is undefined; {:?}", name),
            },
        }
    }

    fn _search_type<'a, F, U>(&'a self, name: &String, f: F) -> Option<U> where F: Fn(&'a TypeInfo<'sess>) -> Option<U> {
        if let Some(ref info) = self.types.borrow().get(name) {
            match info.ttype {
                Type::Object(ref sname, _) if sname != name => self._search_type(sname, f),
                _ => f(info),
            }
        } else if let Some(ref parent) = self.parent {
            parent._search_type(name, f)
        } else {
            None
        }
    }


    pub fn find_type(&self, name: &String) -> Option<Type> {
        self._search_type(name, |info| Some(info.ttype.clone()))
    }

    pub fn find_type_local(&self, name: &String) -> Option<Type> {
        self.types.borrow().get(name).map(|info| info.ttype.clone())
    }

    pub fn update_type(&self, name: &String, ttype: Type) {
        self.modify_type(name, |info| {
            debug!("CHANGE: {:?} from {:?} to {:?}", name, info.ttype, ttype);
            info.ttype = ttype.clone()
        })
    }

    pub fn create_class_def(&'sess self, pair: &(Pos, String, Vec<Type>), parent: Option<(Pos, String, Vec<Type>)>) -> Result<ClassDef<'sess>, Error> {
        let &(_, ref name, ref types) = pair;

        // Find the parent class definitions, which the new class will inherit from
        let parentclass: Option<ScopeRef<'sess>> = match parent {
             Some((_, ref pname, ref _types)) => match self._search_type(&pname, |info| info.classdef) {
                Some(ref parentdef) => Some(parentdef),
                None => return Err(Error::new(format!("NameError: undefined parent class {:?} for {:?}", pname, name))),
            },
            None => None
        };

        // Create class name bindings for checking ast::accessors
        let classdef = Rc::new(Scope::new(parentclass));
        classdef.set_basename(name.clone());

        // Define the class in the local scope
        self.define_type(name.clone(), Type::Object(name.clone(), types.clone()))?;
        self.set_class_def(name, parent.clone(), classdef);
        // TODO i don't like this type == Class thing, but i don't know how i'll do struct types yet either
        //self.define(name.clone(), Some(Type::Object(name.clone(), vec!())))?;
        Ok(classdef.clone())
    }

    pub fn set_class_def(&'sess self, name: &String, parentclass: Option<(Pos, String, Vec<Type>)>, classdef: ClassDef<'sess>) {
        self.modify_type(name, move |info| {
            info.parent = parentclass.clone().map(|c| (c.1, c.2));
            info.classdef = Some(classdef);
        })
    }

    pub fn get_class_def(&'sess self, name: &String) -> ClassDef<'sess> {
        let classdef = self._search_type(name, |info| {
            match info.classdef.as_ref() {
                Some(classdef) => Some(classdef.clone()),
                None => panic!("TypeError: class definition not set for {:?}", name),
            }
        });
        match classdef {
            Some(classdef) => classdef.clone(),
            None => panic!("NameError: type is undefined; {:?}", name),
        }
    }

    pub fn get_class_info(&self, name: &String) -> Option<(Type, Option<Type>)> {
        self._search_type(name, |info| {
            Some((info.ttype.clone(), info.parent.clone().map(|(name, params)| Type::Object(name, params))))
        })
    }

    pub fn get_self_class_def(&'sess self) -> Option<ClassDef<'sess>> {
        self._search_type(&self.basename.borrow(), |info| info.classdef.clone())
    }

    pub fn type_id(&self, name: &String) -> Result<TypeID, Error> {
        match self._search_type(name, |info| Some(info.id)) {
            Some(id) => Ok(id),
            None => Err(Error::new(format!("NameError: type is undefined; {:?}", name))),
        }
    }

    pub fn locate_variable(scope: ScopeRef<'sess>, name: &String) -> Option<ScopeRef<'sess>> {
        if let Some(_) = scope.names.borrow().get(name) {
            return Some(scope);
        }

        let parent = scope.parent;
        match parent {
            Some(parent) => Scope::locate_variable(parent, name),
            _ => None
        }
    }

    pub fn locate_type(scope: ScopeRef<'sess>, name: &String) -> Option<ScopeRef<'sess>> {
        if let Some(_) = scope.types.borrow().get(name) {
            return Some(scope);
        }

        let parent = scope.parent.clone();
        match parent {
            Some(parent) => Scope::locate_type(parent, name),
            _ => None
        }
    }

    pub fn global(scope: ScopeRef<'sess>) -> ScopeRef<'sess> {
        if scope.is_global() {
            scope
        } else {
            Scope::global(scope.parent.clone().unwrap())
        }
    }


    ////// Type Variable Functions //////

    pub fn map_all_typevars(&self, ttype: Type) -> Type {
        let mut varmap = Scope::map_new();
        debug!("MAPPING ALL: {:?}", ttype);
        self.map_typevars(&mut varmap, ttype)
    }

    pub fn map_new() -> HashMap<UniqueID, Type> {
        HashMap::new()
    }

    pub fn map_typevars<'a>(&'a self, varmap: &mut HashMap<UniqueID, Type>, ttype: Type) -> Type {
        match ttype {
            Type::Variable(name, id) => {
                match varmap.get(&id).map(|x| x.clone()) {
                    Some(ptype) => ptype,
                    None => {
                        let etype = self.find_type(&name);
                        debug!("EXISTING TYPEVAR for {:?}: {:?} vs {:?}", name, etype, id);
                        match etype {
                            Some(Type::Variable(_, ref eid)) if *eid == id => etype.clone().unwrap(),
                            None | Some(Type::Variable(_, _)) => {
                                let v = self.new_typevar();
                                varmap.insert(id, v.clone());
                                debug!("MAPPED from {:?} to {:?}", Type::Variable(name.clone(), id), v);
                                v
                            },
                            _ => etype.clone().unwrap(),
                        }
                    }
                }
            },
            Type::Function(args, ret, abi) => Type::Function(self.map_typevars_vec(varmap, args), Box::new(self.map_typevars(varmap, *ret)), abi),
            // TODO why did I do this?  Was it because of a bug or just to reduce typevars, because it caused another bug with constructors
            //Type::Function(args, ret, abi) => Type::Function(self.map_typevars_vec(varmap, args), ret, abi),
            Type::Overload(variants) => Type::Overload(self.map_typevars_vec(varmap, variants)),
            Type::Object(name, types) => Type::Object(name.clone(), self.map_typevars_vec(varmap, types)),
        }
    }

    pub fn map_typevars_vec<'a>(&'a self, varmap: &mut HashMap<UniqueID, Type>, types: Vec<Type>) -> Vec<Type> {
        types.into_iter().map(|vtype| self.map_typevars(varmap, vtype)).collect()
    }

    pub fn unmap_typevars(&self, varmap: &mut HashMap<UniqueID, Type>, ttype: Type) -> Type {
        match ttype {
            Type::Variable(name, id) => {
                for (oid, mtype) in varmap.iter() {
                    match *mtype {
                        Type::Variable(ref mname, ref mid) if *mid == id => { return Type::Variable(mname.clone(), *oid); },
                        _ => { },
                    };
                }
                Type::Variable(name, id)
            },
            Type::Function(args, ret, abi) => Type::Function(self.unmap_typevars_vec(varmap, args), Box::new(self.unmap_typevars(varmap, *ret)), abi),
            // TODO why did I do this?  Was it because of a bug or just to reduce typevars, because it caused another bug with constructors
            //Type::Function(args, ret, abi) => Type::Function(self.map_typevars_vec(varmap, args), ret, abi),
            Type::Overload(variants) => Type::Overload(self.unmap_typevars_vec(varmap, variants)),
            Type::Object(name, types) => Type::Object(name.clone(), self.unmap_typevars_vec(varmap, types)),
        }
    }

    pub fn unmap_typevars_vec(&self, varmap: &mut HashMap<UniqueID, Type>, types: Vec<Type>) -> Vec<Type> {
        types.into_iter().map(|vtype| self.unmap_typevars(varmap, vtype)).collect()
    }

    pub fn new_typevar<'a>(&'a self) -> Type {
        let id = UniqueID::generate();
        let name = self.new_typevar_name();
        let ttype = Type::Variable(name.clone(), id);

        //self.define_type(name, ttype.clone()).unwrap();
        if self.is_global() {
            self.define_type(id.to_string(), ttype.clone()).unwrap();
        } else {
            let gscope = Scope::global(self.parent.clone().unwrap());
            gscope.define_type(id.to_string(), ttype.clone()).unwrap();
        }
        debug!("NEW TYPEVAR: {:?} {:?}", self.get_basename(), ttype);
        ttype
    }

    pub fn new_typevar_name<'a>(&'a self) -> String {
        for mut ch1 in b'`' .. b'z' {
            let name = if ch1 == b'`' { String::from("") } else { (ch1 as char).to_string() };
            for ch2 in b'a' .. b'z' + 1 {
                let name = name.clone() + &(ch2 as char).to_string();
                if self.find_type(&name).is_none() {
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

    pub fn get_full_name(&self, name: &Option<String>, id: UniqueID) -> String {
        let mut base = self.get_basename();
        if base.as_str() != "" {
            base = base + &"_";
        }
        base + &name.as_ref().map_or_else(|| format!("anon{}", id), |name| name.clone())
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
pub struct ScopeMapRef<'sess>(RefCell<HashMap<UniqueID, Scope<'sess>>>);

impl<'sess> ScopeMapRef<'sess> {
    pub const PRIMATIVE: UniqueID = UniqueID(0);
    pub const GLOBAL: UniqueID = UniqueID(1);

    pub fn new() -> Self {
        ScopeMapRef(RefCell::new(HashMap::new()))
    }

    pub fn add(&'sess self, id: UniqueID, parent: Option<ScopeRef<'sess>>) -> ScopeRef<'sess> {
        let scope = Scope::new(parent);
        self.0.borrow_mut().insert(id, scope);
        &scope
    }

    pub fn get(&'sess self, id: &UniqueID) -> ScopeRef<'sess> {
        &self.0.borrow().get(id).unwrap()
    }

    pub fn get_global(&'sess self) -> ScopeRef<'sess> {
        self.get(&ScopeMapRef::GLOBAL)
    }

    //pub fn foreach<F>(&self, f: F) where F: Fn(ScopeRef<'sess>) -> () {
    //    for (_, scope) in self.0.borrow().iter() {
    //        f(scope);
    //    }
    //}
}

