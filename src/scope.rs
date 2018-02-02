
use std::str;
use std::rc::Rc;
use std::fmt::Debug;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::hash_map::Entry;

extern crate nom;
use nom::{ digit };

use import;
use parser::AST;
use types::Type;
use utils::UniqueID;

#[derive(Clone, Debug, PartialEq)]
pub enum Context {
    Local,
    Global,
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
    //pub id: UniqueID,
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

    pub fn is_global(&self) -> bool {
        self.context == Context::Global
    }

    pub fn set_global(&mut self, value: bool) {
        self.context = if value { Context::Global } else { Context::Local };
    }

    pub fn set_class(&mut self, value: bool) {
        self.context = if value { Context::Class } else { Context::Local };
    }

    pub fn set_parent(&mut self, parent: ScopeRef<V, T>) {
        self.parent = Some(parent);
    }

    pub fn get_parent(&self) -> Option<ScopeRef<V, T>> {
        self.parent.clone()
    }

    ///// Variable Functions /////

    pub fn define(&mut self, name: String, ttype: Option<Type>) {
        match self.names.contains_key(&name) {
            true => panic!("NameError: variable is already defined; {:?}", name),
            false => self.names.insert(name, Symbol { ttype: ttype, value: None, funcdefs: 0, external: false }),
        };
    }

    pub fn define_func(&mut self, name: String, ttype: Option<Type>, external: bool) {
        // TODO we don't really do the right this with type here
        let funcs = self.search(&name, |sym| Some(sym.funcdefs)).unwrap_or(0);
        match self.names.entry(name.clone()) {
            Entry::Occupied(mut entry) => match entry.get().funcdefs {
                0 => panic!("NameError: variable is already defined as a non-function; {:?}", name),
                _ => entry.get_mut().funcdefs += 1,
            },
            Entry::Vacant(entry) => { entry.insert(Symbol { ttype: ttype, value: None, funcdefs: funcs + 1, external: external }); },
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

    pub fn search<F, U>(&self, name: &String, f: F) -> Option<U> where F: Fn(&Symbol<V>) -> Option<U> {
        if let Some(sym) = self.names.get(name) {
            f(sym)
        } else if let Some(ref parent) = self.parent {
            parent.borrow().search(name, f)
        } else {
            None
        }
    }

    pub fn contains_local(&self, name: &String) -> bool {
        self.names.contains_key(name)
    }


    //pub fn set_overloaded(&mut self, name: &String) {
    //    self.modify(name, move |sym| sym.overloaded = true)
    //}

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

    pub fn find(&self, name: &String) -> Option<Symbol<V>> {
        self.search(name, |sym| Some(sym.clone()))
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

    pub fn update_variable_type(&mut self, name: &String, ttype: Type) {
        self.modify_local(name, move |sym| sym.ttype = Some(ttype.clone()))
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
        let mut ftype = ftype.clone();
        ftype = otype.map(|ttype| ttype.add_variant(tscope.clone(), ftype.clone())).unwrap_or(ftype);
        dscope.borrow_mut().update_variable_type(name, ftype.clone());
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
        self.types.contains_key(name)
    }

    pub fn modify_type<F>(&mut self, name: &String, f: F) where F: Fn(&mut TypeInfo<V, T>) -> () {
        match self.types.entry(name.clone()) {
            Entry::Occupied(mut entry) => f(entry.get_mut()),
            _ => match self.parent {
                Some(ref parent) => parent.borrow_mut().modify_type(name, f),
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
        //if let Type::Variable(_) = ttype {
        //    return;
        //}
        self.modify_type(name, |info| {
            println!("CHANGE: {:?} from {:?} to {:?}", name, info.ttype, ttype);
            info.ttype = ttype.clone()
        })
    }

    //pub fn swap_type<F>(&mut self, name: &String, f: F) where F: Fn(Type) -> Type {
    //    self.modify_type(name, |info| info.ttype = f(info.ttype.clone()))
    //}

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

        // Add constructor method
        //classdef.borrow_mut().define(String::from("new"), Some(Type::Function(vec!(), Box::new(Type::Object(name.clone(), types.clone())))));

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
        self.search_type(name, |info| Some(match info.parent {
            Some((ref name, ref params)) => (info.ttype.clone(), Some(Type::Object(name.clone(), params.clone()))),
            None => (info.ttype.clone(), None),
        }))
    }

    pub fn get_self_class_def(&self) -> Option<ScopeRef<V, T>> {
        self.search_type(&self.basename, |info| info.classdef.clone())
    }

    pub fn target(scope: ScopeRef<V, T>) -> ScopeRef<V, T> {
        if scope.borrow().context == Context::Class {
            scope.borrow().get_self_class_def().unwrap()
        } else {
            scope
        }
    }

    pub fn set_type_value(&mut self, name: &String, value: T) {
        self.modify_type(name, |info| info.value = Some(value.clone()))
    }

    pub fn get_type_value(&self, name: &String) -> Option<T> {
        self.search_type(name, |info| info.value.clone())
    }


    ////// Type Variable Functions //////

    pub fn map_all_typevars(&mut self, ttype: Type) -> Type {
        let mut names = Scope::<V, T>::map_new();
        self.map_typevars(&mut names, ttype)
    }

    pub fn map_new() -> HashMap<String, Type> {
        HashMap::new()
    }

    pub fn map_typevars(&mut self, names: &mut HashMap<String, Type>, ttype: Type) -> Type {
        match ttype {
            Type::Variable(name) => {
                match names.get(&name).map(|x| x.clone()) {
                    Some(ptype) => ptype,
                    None => {
                        // TODO this maybe isn't right because it might link unrelated variables...
                        if let Some(dtype) = self.find_type(&name) {
                            //dtype.clone()
                            Type::Variable(name)
                        } else {
                            let v = self.new_typevar();
                            names.insert(name, v.clone());
                            v.clone()
                        }
                    }
                }
            },
            Type::Function(args, ret) => Type::Function(self.map_typevars_vec(names, args), Box::new(self.map_typevars(names, *ret))),
            Type::Overload(variants) => Type::Overload(self.map_typevars_vec(names, variants)),
            //Type::List(ref ltype) => Type::List(Box::new(self.map_typevars(names, ltype))),
            Type::Object(name, types) => Type::Object(name.clone(), self.map_typevars_vec(names, types)),
        }
    }

    pub fn map_typevars_vec(&mut self, names: &mut HashMap<String, Type>, types: Vec<Type>) -> Vec<Type> {
        types.into_iter().map(|vtype| self.map_typevars(names, vtype)).collect()
    }

    pub fn raise_types(&mut self, fscope: ScopeRef<V, T>) {
println!("RAISING");
        let mut names = vec!();
        for (name, info) in &mut fscope.borrow_mut().types {
            match info.ttype {
                Type::Variable(ref vname) => {
                    //if name == vname {
println!("RAISE TYPE: {:?} {:?}", self.get_full_name(&Some(String::from("")), UniqueID(0)), vname);
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

    pub fn new_typevar(&mut self) -> Type {
        for ch in b'a' .. b'z' + 1 {
            let name = (ch as char).to_string();
            if self.find_type(&name).is_none() {
                let ttype = Type::Variable(name.clone());
                self.define_type(name, ttype.clone());
println!("NEW TYPEVAR: {:?} {:?}", self.get_full_name(&Some(String::from("")), UniqueID(0)), ttype);
                return ttype;
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

    pub fn foreach<F>(&self, f: F) where F: Fn(ScopeRef<V, T>) -> () {
        for (i, scope) in self.0.borrow().iter() {
            f(scope.clone());
        }
    }
}

pub fn mangle_name(name: &String, argtypes: &Vec<Type>) -> String {
    let mut args = String::from("");
    for ttype in argtypes {
        args = args + &match *ttype {
            // TODO add type paramaters into name
            Type::Variable(_) => format!("V"),
            Type::Object(ref name, ref types) => format!("N{}{}", name.len(), name),
            // TODO this isn't complete, you have to deal with other types
            _ => String::from(""),
        };
    }
    format!("_Z{}{}{}", name.len(), name, args)
}

pub fn unmangle_name(name: &String) -> Option<String> {
    named!(unmangle<String>,
        preceded!(tag!("_Z"),
            map!(
                length_bytes!(map!(digit, |s| usize::from_str_radix(str::from_utf8(s).unwrap(), 10).unwrap())),
                |s| String::from(str::from_utf8(s).unwrap())
            )
        )
    );
    match unmangle(name.as_bytes()) {
        nom::IResult::Done(_, value) => Some(value),
        _ => None,
    }
}


pub fn bind_names<V, T>(map: ScopeMapRef<V, T>, code: &mut Vec<AST>) where V: Clone + Debug, T: Clone + Debug {
    bind_names_vec(map.clone(), map.get_global().clone(), code);
}

pub fn bind_names_vec<V, T>(map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>, code: &mut Vec<AST>) where V: Clone + Debug, T: Clone + Debug {
    for node in code {
        bind_names_node(map.clone(), scope.clone(), node);
    }
}

fn bind_names_node<V, T>(map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>, node: &mut AST) where V: Clone + Debug, T: Clone + Debug {
    match *node {
        AST::Function(ref name, ref args, ref ret, ref mut body, ref id) => {
            let fscope = map.add(id.clone(), Some(scope.clone()));
            fscope.borrow_mut().set_basename(name.as_ref().map_or(format!("anon{}", id), |name| name.clone()));

            if let Some(ref name) = *name {
                let dscope = Scope::target(scope.clone());
                dscope.borrow_mut().define_func(name.clone(), None, false);
            }

            for arg in args {
                check_for_typevars(scope.clone(), &arg.1);
                fscope.borrow_mut().define(arg.0.clone(), arg.1.clone());
            }
            check_for_typevars(scope.clone(), ret);

            bind_names_node(map.clone(), fscope.clone(), body)
        },

        AST::Invoke(ref mut fexpr, ref mut args, _) => {
            //if let AST::Accessor(ref mut expr, _, _) = **fexpr {
                //let mut obj = Box::new(AST::Underscore);
                //mem::swap(expr, &mut obj);
                //args.insert(0, *obj);
            //    args.insert(0, *expr.clone());
            //}
            bind_names_node(map.clone(), scope.clone(), fexpr);
            bind_names_vec(map.clone(), scope.clone(), args);
        },

        AST::Definition((ref name, ref ttype), ref mut code) => {
            check_for_typevars(scope.clone(), ttype);
            let dscope = Scope::target(scope.clone());
            dscope.borrow_mut().define(name.clone(), ttype.clone());
            bind_names_node(map.clone(), scope.clone(), code);
        },

        AST::Declare(ref name, ref ttype) => {
            let dscope = Scope::target(scope.clone());
            dscope.borrow_mut().define_func(name.clone(), Some(ttype.clone()), true);
            if let Some(ref mname) = unmangle_name(name) {
                println!("OVERLOAD: {:?}", mname);
                dscope.borrow_mut().define_func(mname.clone(), None, true);
                let mut stype = dscope.borrow().get_variable_type(mname).unwrap_or(Type::Overload(vec!()));
                stype = stype.add_variant(scope.clone(), ttype.clone());
                dscope.borrow_mut().update_variable_type(mname, stype);
            }
        },

        AST::Identifier(ref name) => {
            if scope.borrow().find(name).is_none() {
                panic!("NameError: undefined identifier {:?}", name);
            }
        },

        AST::SideEffect(_, ref mut args) => {
            bind_names_vec(map.clone(), scope.clone(), args);
        },

        AST::If(ref mut cond, ref mut texpr, ref mut fexpr) => {
            bind_names_node(map.clone(), scope.clone(), cond);
            bind_names_node(map.clone(), scope.clone(), texpr);
            bind_names_node(map.clone(), scope.clone(), fexpr);
        },

        AST::Try(ref mut cond, ref mut cases) |
        AST::Match(ref mut cond, ref mut cases) => {
            bind_names_node(map.clone(), scope.clone(), cond);
            // TODO check to make sure AST::Underscore only occurs as the last case, if at all
            for &mut (ref mut case, ref mut body) in cases {
                bind_names_node(map.clone(), scope.clone(), case);
                bind_names_node(map.clone(), scope.clone(), body);
            }
        },

        AST::Raise(ref mut expr) => {
            bind_names_node(map.clone(), scope.clone(), expr);
        },

        AST::While(ref mut cond, ref mut body) => {
            bind_names_node(map.clone(), scope.clone(), cond);
            bind_names_node(map.clone(), scope.clone(), body);
        },

        AST::For(ref name, ref mut cond, ref mut body, ref id) => {
            let lscope = map.add(id.clone(), Some(scope.clone()));
            lscope.borrow_mut().define(name.clone(), None);
            bind_names_node(map.clone(), lscope.clone(), cond);
            bind_names_node(map.clone(), lscope.clone(), body);
        },

        AST::List(ref mut code) |
        AST::Block(ref mut code) => { bind_names_vec(map, scope, code); },

        AST::Index(ref mut base, ref mut index, _) => {
            bind_names_node(map.clone(), scope.clone(), base);
            bind_names_node(map.clone(), scope.clone(), index);
        },


        AST::New((ref name, ref types)) => {
            if scope.borrow().find_type(name).is_none() {
                panic!("NameError: undefined identifier {:?}", name);
            }
        },

        AST::Class(ref pair, ref parent, ref mut body, ref id) => {
            let classdef = scope.borrow_mut().create_class_def(pair, parent.clone());
            let &(ref name, ref types) = pair;

            // Create a temporary invisible scope to name check the class body
            let tscope = map.add(id.clone(), Some(scope.clone()));
            tscope.borrow_mut().set_class(true);
            tscope.borrow_mut().set_basename(name.clone());

            // Define Self and Super, and check for typevars in the type params
            tscope.borrow_mut().define_type(String::from("Self"), Type::Object(name.clone(), types.clone()));
            types.iter().map(|ttype| check_for_typevars(tscope.clone(), &Some(ttype.clone()))).count();
            if let &Some((ref pname, ref ptypes)) = parent {
                tscope.borrow_mut().define_type(String::from("Super"), Type::Object(pname.clone(), ptypes.clone()));
                ptypes.iter().map(|ttype| check_for_typevars(tscope.clone(), &Some(ttype.clone()))).count();
            }

            bind_names_vec(map.clone(), tscope.clone(), body);
        },

        AST::Resolver(ref mut left, _) => {
            // TODO should this always work on a type reference, or should classes be added as values as well as types?
            //bind_names_node(map.clone(), scope, left);
            match **left {
                AST::Identifier(ref name) => {
                    if scope.borrow().find_type(name).is_none() {
                        panic!("NameError: undefined type {:?}", name);
                    }
                },
                _ => panic!("SyntaxError: left-hand side of scope resolver must be identifier")
            }
        },

        AST::Accessor(ref mut left, _, _) => {
            bind_names_node(map.clone(), scope, left);
        },

        AST::Assignment(ref mut left, ref mut right) => {
            match **left {
                AST::Accessor(_, _, _) | AST::Index(_, _, _) => { },
                _ => panic!("SyntaxError: assignment to something other than a list or class element: {:?}", left),
            };
            bind_names_node(map.clone(), scope.clone(), left);
            bind_names_node(map.clone(), scope.clone(), right);
        },

        AST::Import(ref name, ref mut decls) => {
            let path = name.replace(".", "/") + ".idx";
            *decls = import::load_index(scope.clone(), path.as_str());
            bind_names_vec(map.clone(), scope.clone(), decls);
        },

        AST::Type(_, _) => panic!("NotImplementedError: not yet supported, {:?}", node),

        AST::Noop | AST::Underscore | AST::Nil(_) |
        AST::Boolean(_) | AST::Integer(_) | AST::Real(_) | AST::String(_) => { }
    }
}

pub fn check_for_typevars<V, T>(scope: ScopeRef<V, T>, ttype: &Option<Type>) where V: Clone, T: Clone {
    match ttype {
        &Some(ref ttype) => match ttype {
            &Type::Object(_, ref types) => {
                for ttype in types {
                    check_for_typevars(scope.clone(), &Some(ttype.clone()))
                }
            },
            &Type::Function(ref args, ref ret) => {
                for atype in args {
                    check_for_typevars(scope.clone(), &Some(atype.clone()))
                }
                check_for_typevars(scope.clone(), &Some(*ret.clone()));
            },
            &Type::Variable(ref name) => {
                 if !scope.borrow().contains_type(name) {
                    scope.borrow_mut().define_type(name.clone(), ttype.clone());
                }
            },
            _ => { },
        },
        _ => { },
    }
}

