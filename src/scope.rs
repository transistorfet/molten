
use std::str;
use std::rc::Rc;
use std::cell::RefCell;
use std::ops::DerefMut;
use std::collections::HashMap;
use std::collections::hash_map::Entry;

extern crate nom;
use nom::{ digit };

use parser;
use types::Type;
use utils::UniqueID;

#[derive(Clone, Debug, PartialEq)]
pub enum Context {
    Primative,
    Global,
    Local,
    Class,
}


#[derive(Clone, Debug, PartialEq)]
pub struct Symbol<V> {
    pub ttype: Option<Type>,
    pub value: Option<V>,
    pub funcdefs: i32,
    pub external: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeInfo<V, T> {
    pub ttype: Type,
    pub classdef: Option<ScopeRef<V, T>>,
    pub parent: Option<(String, Vec<Type>)>,
    pub value: Option<T>,
}


#[derive(Clone, Debug, PartialEq)]
pub struct Scope<V, T> {
    pub basename: String,
    pub context: Context,
    pub names: HashMap<String, Symbol<V>>,
    pub types: HashMap<String, TypeInfo<V, T>>,
    pub parent: Option<ScopeRef<V, T>>,
}

pub type ScopeRef<V, T> = Rc<RefCell<Scope<V, T>>>;
//#[derive(Clone, Debug, PartialEq)]
//pub struct ScopeRef(Rc<RefCell<Scope>>);


impl<V, T> Scope<V, T> where V: Clone, T: Clone {
    pub fn new(parent: Option<ScopeRef<V, T>>) -> Scope<V, T> {
        Scope {
            basename: String::from(""),
            context: Context::Local,
            names: HashMap::new(),
            types: HashMap::new(),
            parent: parent,
        }
    }

    pub fn new_ref(parent: Option<ScopeRef<V, T>>) -> ScopeRef<V, T> {
        Rc::new(RefCell::new(Scope::new(parent)))
    }

    pub fn set_context(&mut self, context: Context) {
        self.context = context;
    }

    pub fn set_class(&mut self, value: bool) {
        self.context = if value { Context::Class } else { Context::Local };
    }

    pub fn is_primative(&self) -> bool {
        self.context == Context::Primative
    }

    pub fn is_global(&self) -> bool {
        self.context == Context::Global || self.context == Context::Primative
    }

    pub fn is_local(&self) -> bool {
        self.context == Context::Local
    }

    pub fn set_parent(&mut self, parent: ScopeRef<V, T>) {
        self.parent = Some(parent);
    }

    pub fn get_parent(&self) -> Option<ScopeRef<V, T>> {
        self.parent.clone()
    }

    pub fn target(scope: ScopeRef<V, T>) -> ScopeRef<V, T> {
        match scope.borrow().context {
            Context::Class => scope.borrow().get_self_class_def().unwrap(),
            _ => scope.clone(),
        }
    }

    ///// Variable Functions /////

    pub fn define(&mut self, name: String, ttype: Option<Type>) {
        match self.names.contains_key(&name) {
            true => panic!("NameError: variable is already defined; {:?}", name),
            false => self.names.insert(name, Symbol { ttype: ttype, value: None, funcdefs: 0, external: false }),
        };
    }

    pub fn define_func(&mut self, name: String, ttype: Option<Type>, external: bool) {
        let funcs = self.search(&name, |sym| Some(sym.funcdefs)).unwrap_or(0);
        match self.names.entry(name.clone()) {
            Entry::Vacant(entry) => { entry.insert(Symbol { ttype: ttype, value: None, funcdefs: funcs + 1, external: external }); },
            Entry::Occupied(mut entry) => match entry.get().funcdefs {
                0 => panic!("NameError: variable is already defined as a non-function; {:?}", name),
                // TODO we don't really do the right this with type here, but we don't have a scoperef to pass to Type::add_variant
                _ => entry.get_mut().funcdefs += 1,
            },
        };
    }

    pub fn define_func_variant(dscope: ScopeRef<V, T>, name: String, tscope: ScopeRef<V, T>, ftype: Type) {
        dscope.borrow_mut().define_func(name.clone(), None, false);
        Scope::add_func_variant(dscope, &name, tscope, ftype);
    }

    pub fn modify_local<F>(&mut self, name: &String, mut f: F) -> () where F: FnMut(&mut Symbol<V>) -> () {
        match self.names.entry(name.clone()) {
            Entry::Vacant(_) => panic!("NameError: variable is undefined in this scope; {:?}", name),
            Entry::Occupied(mut entry) => f(entry.get_mut()),
        }
    }

    /*
    pub fn modify<F>(&mut self, name: &String, mut f: F) -> () where F: FnMut(&mut Symbol<V>) -> () {
        // TODO this might be an issue with overloaded functions; this was changed to make overloading work when in different scopes, which might cause a name/type error if something in a
        // more global scope tries to access an overloaded type specified in a more local scope... maybe the solution is to create a new entry with the new variant only accessible from the
        // more local scope... but will that cause a duplicate name error somewhere?
        match self.names.entry(name.clone()) {
            Entry::Occupied(mut entry) => f(entry.get_mut()),
            _ => match self.parent {
                Some(ref parent) => parent.borrow_mut().modify(name, f),
                _ => panic!("NameError: type is undefined; {:?}", name),
            },
        }
    }
    */

    pub fn search<F, U>(&self, name: &String, f: F) -> Option<U> where F: Fn(&Symbol<V>) -> Option<U> {
        if let Some(sym) = self.names.get(name) {
            f(sym)
        } else if let Some(ref parent) = self.parent {
            parent.borrow().search(name, f)
        } else {
            None
        }
    }

    pub fn contains(&self, name: &String) -> bool {
        match self.search(name, |sym| Some(true)) {
            Some(true) => true,
            _ => false,
        }
    }

    pub fn contains_local(&self, name: &String) -> bool {
        self.names.contains_key(name)
    }


    pub fn is_overloaded(&self, name: &String) -> bool {
        self.search(name, |sym| Some(sym.funcdefs >= 2)).unwrap()
    }

    pub fn assign(&mut self, name: &String, value: V) {
        self.modify_local(name, move |sym| sym.value = Some(value.clone()))
    }

    pub fn get_variable_value(&self, name: &String) -> Option<V> {
        match self.search(name, |sym| Some(sym.value.clone())) {
            Some(value) => value,
            None => panic!("NameError: variable is undefined; {:?}", name),
        }
    }

    pub fn set_variable_type(&mut self, name: &String, ttype: Type) {
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
        let mut variants = self.names.get(name).as_ref().map(|sym| sym.ttype.as_ref().map(|ttype| ttype.get_variants()).unwrap_or(vec!())).unwrap_or(vec!());
        if !local {
            variants.extend(self.parent.as_ref().map(|parent| parent.borrow().get_all_variants(name, local)).unwrap_or(vec!()));
        }
        variants
    }

    pub fn add_func_variant(dscope: ScopeRef<V, T>, name: &String, tscope: ScopeRef<V, T>, ftype: Type) {
        let otype = dscope.borrow().get_variable_type_full(name, true);
        let ftype = otype.map(|ttype| ttype.add_variant(tscope.clone(), ftype.clone())).unwrap_or(ftype);
        dscope.borrow_mut().set_variable_type(name, ftype.clone());
    }

    pub fn is_closure_var(&self, name: &String) -> bool {
        if self.names.contains_key(name) {
            return false;
        }

        let mut parent = self.parent.clone();
        while parent.is_some() {
            let scope = parent.unwrap();
            if scope.borrow().context == Context::Global {
                return false;
            }
            if scope.borrow().contains_local(name) {
                return true;
            }
            parent = scope.borrow().parent.clone();
        }
        false
    }


    ///// Type Functions /////

    pub fn define_type(&mut self, name: String, ttype: Type) {
        match self.types.contains_key(&name) {
            true => panic!("NameError: type is already defined; {:?}", name),
            false => self.types.insert(name, TypeInfo {
                ttype: ttype,
                classdef: None,
                parent: None,
                value: None
            }),
        };
    }

    pub fn contains_type(&self, name: &String) -> bool {
        match self.search_type(name, |info| Some(true)) {
            Some(true) => true,
            _ => false,
        }
    }

    pub fn contains_type_local(&self, name: &String) -> bool {
        self.types.contains_key(name)
    }

    pub fn modify_type<F>(&mut self, name: &String, f: F) where F: Fn(&mut TypeInfo<V, T>) -> () {
        match self.types.entry(name.clone()) {
            Entry::Occupied(mut entry) => f(entry.get_mut()),
            _ => match self.parent {
                Some(ref parent) => {
                    parent.borrow_mut().modify_type(name, f);
                },
                _ => panic!("NameError: type is undefined; {:?}", name),
            },
        }
    }

    pub fn search_type<F, U>(&self, name: &String, f: F) -> Option<U> where F: Fn(&TypeInfo<V, T>) -> Option<U> {
        if let Some(ref info) = self.types.get(name) {
            f(info)
        } else if let Some(ref parent) = self.parent {
            parent.borrow().search_type(name, f)
        } else {
            None
        }
    }


    pub fn find_type(&self, name: &String) -> Option<Type> {
        self.search_type(name, |info| Some(info.ttype.clone()))
    }

    pub fn update_type(&mut self, name: &String, ttype: Type) {
        self.modify_type(name, |info| {
            debug!("CHANGE: {:?} from {:?} to {:?}", name, info.ttype, ttype);
            info.ttype = ttype.clone()
        })
    }

    pub fn create_class_def(&mut self, pair: &(String, Vec<Type>), parent: Option<(String, Vec<Type>)>) -> ScopeRef<V, T> {
        // Create class name bindings for checking ast::accessors
        let &(ref name, ref types) = pair;
        let classdef = Scope::new_ref(None);
        classdef.borrow_mut().set_basename(name.clone());

        // Find the parent class definitions, which the new class will inherit from
        match parent {
             Some((ref pname, ref types)) => match self.search_type(&pname, |info| info.classdef.clone()) {
                Some(ref parentdef) => classdef.borrow_mut().set_parent(parentdef.clone()),
                None => panic!("NameError: undefined parent class {:?} for {:?}", pname, name),
            },
            None => { }
        };

        // Define the class in the local scope
        self.define_type(name.clone(), Type::Object(name.clone(), types.clone()));
        self.set_class_def(name, parent.clone(), classdef.clone());
        // TODO i don't like this type == Class thing, but i don't know how i'll do struct types yet either
        //self.define(name.clone(), Some(Type::Object(name.clone(), vec!())));
        classdef
    }

    pub fn set_class_def(&mut self, name: &String, parentclass: Option<(String, Vec<Type>)>, classdef: ScopeRef<V, T>) {
        self.modify_type(name, |info| {
            info.parent = parentclass.clone();
            info.classdef = Some(classdef.clone());
        })
    }

    pub fn get_class_def(&self, name: &String) -> ScopeRef<V, T> {
        let classdef = self.search_type(name, |info| {
            match info.classdef {
                Some(ref classdef) => Some(classdef.clone()),
                None => panic!("TypeError: class definition not set for {:?}", name),
            }
        });
        match classdef {
            Some(classdef) => classdef,
            None => panic!("NameError: type is undefined; {:?}", name),
        }
    }

    pub fn get_class_info(&self, name: &String) -> Option<(Type, Option<Type>)> {
        self.search_type(name, |info| {
            Some((info.ttype.clone(), info.parent.clone().map(|(name, params)| Type::Object(name, params))))
        })
    }

    pub fn get_self_class_def(&self) -> Option<ScopeRef<V, T>> {
        self.search_type(&self.basename, |info| info.classdef.clone())
    }

    pub fn set_type_value(&mut self, name: &String, value: T) {
        self.modify_type(name, |info| info.value = Some(value.clone()))
    }

    pub fn get_type_value(&self, name: &String) -> Option<T> {
        self.search_type(name, |info| info.value.clone())
    }

    pub fn locate_variable(scope: ScopeRef<V, T>, name: &String) -> Option<ScopeRef<V, T>> {
        if let Some(_) = scope.borrow().names.get(name) {
            return Some(scope.clone());
        }

        let parent = scope.borrow().parent.clone();
        match parent {
            Some(parent) => Scope::locate_variable(parent, name),
            _ => None
        }
    }

    pub fn locate_type(scope: ScopeRef<V, T>, name: &String) -> Option<ScopeRef<V, T>> {
        if let Some(_) = scope.borrow().types.get(name) {
            return Some(scope.clone());
        }

        let parent = scope.borrow().parent.clone();
        match parent {
            Some(parent) => Scope::locate_type(parent, name),
            _ => None
        }
    }

    pub fn global(scope: ScopeRef<V, T>) -> ScopeRef<V, T> {
        if scope.borrow().is_global() {
            scope
        } else {
            Scope::global(scope.borrow().parent.clone().unwrap())
        }
    }


    ////// Type Variable Functions //////

    pub fn map_all_typevars(&mut self, ttype: Type) -> Type {
        let mut varmap = Scope::<V, T>::map_new();
        self.map_typevars(&mut varmap, ttype)
    }

    pub fn map_new() -> HashMap<UniqueID, Type> {
        HashMap::new()
    }

    pub fn map_typevars(&mut self, varmap: &mut HashMap<UniqueID, Type>, ttype: Type) -> Type {
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
                                v
                            },
                            _ => etype.clone().unwrap(),
                        }
                    }
                }
            },
            Type::Function(args, ret) => Type::Function(self.map_typevars_vec(varmap, args), Box::new(self.map_typevars(varmap, *ret))),
            // TODO why did I do this?  Was it because of a bug or just to reduce typevars, because it caused another bug with constructors
            //Type::Function(args, ret) => Type::Function(self.map_typevars_vec(varmap, args), ret),
            Type::Overload(variants) => Type::Overload(self.map_typevars_vec(varmap, variants)),
            Type::Object(name, types) => Type::Object(name.clone(), self.map_typevars_vec(varmap, types)),
        }
    }

    pub fn map_typevars_vec(&mut self, varmap: &mut HashMap<UniqueID, Type>, types: Vec<Type>) -> Vec<Type> {
        types.into_iter().map(|vtype| self.map_typevars(varmap, vtype)).collect()
    }

/*
    pub fn raise_types(&mut self, fscope: ScopeRef<V, T>) {
        //debug!("RAISING");
        let mut names = vec!();
        for (name, info) in &mut fscope.borrow_mut().types {
            match info.ttype {
                Type::Variable(_, ref id) => {
                    //if name == vname {
                        debug!("RAISE TYPEVAR: {:?} {:?}", self.get_full_name(&Some(String::from("")), UniqueID(0)), info.ttype);
                        // TODO this is probably wrong because we might be unifying different type vars
                        //if !self.types.contains_key(name) {
                            self.define_type(name.clone(), info.ttype.clone());
                        //}
                        names.push(name.clone());
                    //}
                },
                _ => { },
            }
        }

        for name in names {
            fscope.borrow_mut().types.remove(&name);
        }
    }

    pub fn raise_type(&mut self, fscope: ScopeRef<V, T>, ttype: Type) {
        let mut names = vec!();
        Scope::<V, T>::collect_typevars(&mut names, ttype.clone());
        for name in names {
            if fscope.borrow().contains_type_local(&name) {
                debug!("RAISE TYPEVAR: {:?} {:?}", name, ttype);
                let otype = fscope.borrow().find_type(&name).unwrap();
                fscope.borrow_mut().types.remove(&name);
                self.define_type(name.clone(), otype);
            }
        }
    }

    pub fn collect_typevars(names: &mut Vec<String>, ttype: Type) {
        match ttype {
            Type::Variable(name) => {
                if !names.contains(&name) {
                    names.push(name);
                }
            },
            Type::Function(args, ret) => {
                for arg in args {
                    Scope::<V, T>::collect_typevars(names, arg);
                }
                Scope::<V, T>::collect_typevars(names, *ret);
            },
            Type::Overload(list) |
            Type::Object(_, list) => {
                for item in list {
                    Scope::<V, T>::collect_typevars(names, item)
                }
            },
        }
    }
*/

    pub fn new_typevar(&mut self) -> Type {
        let id = UniqueID::generate();
        let name = self.new_typevar_name();
        let ttype = Type::Variable(name.clone(), id.clone());

        self.define_type(name, ttype.clone());
        if self.is_global() {
            self.define_type(id.to_string(), ttype.clone());
        } else {
            let gscope = Scope::global(self.parent.clone().unwrap());
            gscope.borrow_mut().define_type(id.to_string(), ttype.clone());
        }
        debug!("NEW TYPEVAR: {:?} {:?}", self.get_basename(), ttype);
        ttype

/*
        let f = |gborrow: &mut Scope<V, T>| {
            let id = UniqueID::generate();
            let name = gborrow.new_typevar_name();
            let ttype = Type::Variable(name.clone(), id.clone());
            //let ttype = Type::Variable(String::from("blank"), id.clone());

            gborrow.define_type(name, ttype.clone());
            gborrow.define_type(id.to_string(), ttype.clone());
            debug!("NEW TYPEVAR: {:?}", ttype);
            ttype
        };

        let gscope = Scope::global(self.parent.clone().unwrap());
        if self.is_global() {
            f(self)
        } else {
            f(gscope.borrow_mut().deref_mut())
        }
*/
    }

    pub fn new_typevar_name(&mut self) -> String {
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

    pub fn set_basename(&mut self, name: String) {
        self.basename = name;
    }

    pub fn get_full_name(&self, name: &Option<String>, id: UniqueID) -> String {
        let mut base = self.get_basename();
        if base.as_str() != "" {
            base = base + &"_";
        }
        base + &name.as_ref().map_or_else(|| format!("anon{}", id), |name| name.clone())
    }

    pub fn get_basename(&self) -> String {
        let name = self.parent.as_ref().map_or(String::from(""), |parent| parent.borrow().get_basename());
        if name.as_str() != "" {
            name +  &"_" + &self.basename
        } else {
            self.basename.clone()
        }
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct ScopeMapRef<V, T>(Rc<RefCell<HashMap<UniqueID, ScopeRef<V, T>>>>);

impl<V, T> ScopeMapRef<V, T> where V: Clone, T: Clone {
    pub const PRIMATIVE: UniqueID = UniqueID(0);
    pub const GLOBAL: UniqueID = UniqueID(1);

    pub fn new() -> ScopeMapRef<V, T> {
        ScopeMapRef(Rc::new(RefCell::new(HashMap::new())))
    }

    pub fn add(&self, id: UniqueID, parent: Option<ScopeRef<V, T>>) -> ScopeRef<V, T> {
        let scope = Scope::new_ref(parent);
        self.0.borrow_mut().insert(id, scope.clone());
        scope
    }

    pub fn get(&self, id: &UniqueID) -> ScopeRef<V, T> {
        self.0.borrow().get(id).unwrap().clone()
    }

    pub fn get_global(&self) -> ScopeRef<V, T> {
        self.get(&ScopeMapRef::<V, T>::GLOBAL)
    }

    //pub fn foreach<F>(&self, f: F) where F: Fn(ScopeRef<V, T>) -> () {
    //    for (_, scope) in self.0.borrow().iter() {
    //        f(scope.clone());
    //    }
    //}
}

pub fn mangle_name(name: &String, argtypes: &Vec<Type>) -> String {
    let mut args = String::from("");
    for ttype in argtypes {
        args = args + &match *ttype {
            // TODO add type paramaters into name
            Type::Variable(_, _) => format!("V"),
            Type::Object(ref name, ref types) => format!("N{}{}", name.len(), name),
            // TODO this isn't complete, you have to deal with other types
            _ => String::from(""),
        };
    }
    format!("_Z{}{}{}", name.len(), name, args)
}

pub fn unmangle_name(name: &String) -> Option<String> {
    named!(unmangle(parser::Span) -> String,
        preceded!(tag!("_Z"),
            map_str!(length_bytes!(map!(digit, |s| usize::from_str_radix(str::from_utf8(s.fragment).unwrap(), 10).unwrap())))
        )
    );
    match unmangle(parser::Span::new(name.as_bytes())) {
        nom::IResult::Done(_, value) => Some(value),
        _ => None,
    }
}


