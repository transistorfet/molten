
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::hash_map::Entry;

use parser::{ AST, parse_type };
use types::Type;
//use interpreter;
//use compiler_llvm;
use utils::UniqueID;



#[derive(Clone, Debug, PartialEq)]
pub struct Symbol<V> {
    pub ttype: Type,
    pub value: Option<V>,
    //pub value: Option<interpreter::Value>,
    //pub address: Option<compiler_llvm::Value>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeInfo<V, T> {
    pub ttype: Type,
    pub structdef: Vec<(String, Type)>,
    pub parent: Option<String>,
    pub classdef: Option<ScopeRef<V, T>>,
    pub value: Option<T>,
}


#[derive(Clone, Debug, PartialEq)]
pub struct Scope<V, T> {
    //pub id: UniqueID,
    pub is_global: bool,
    pub no_lookup: bool,
    pub names: HashMap<String, Symbol<V>>,
    pub types: HashMap<String, TypeInfo<V, T>>,
    pub parent: Option<ScopeRef<V, T>>,
    //pub manager: Option<ScopeMapRef>,
}

pub type ScopeRef<V, T> = Rc<RefCell<Scope<V, T>>>;
//#[derive(Clone, Debug, PartialEq)]
//pub struct ScopeRef(Rc<RefCell<Scope>>);


impl<V, T> Scope<V, T> where V: Clone, T: Clone {
    pub fn new(parent: Option<ScopeRef<V, T>>) -> Scope<V, T> {
        Scope {
            is_global: false,
            no_lookup: false,
            names: HashMap::new(),
            types: HashMap::new(),
            parent: parent,
            //manager: None,
        }
    }

    pub fn new_ref(parent: Option<ScopeRef<V, T>>) -> ScopeRef<V, T> {
        Rc::new(RefCell::new(Scope::new(parent)))
    }

    pub fn is_global(&self) -> bool {
        self.is_global
    }

    pub fn set_global(&mut self, value: bool) {
        self.is_global = value;
    }

    pub fn set_no_lookup(&mut self, value: bool) {
        self.no_lookup = value;
    }

    pub fn set_parent(&mut self, parent: ScopeRef<V, T>) {
        self.parent = Some(parent);
    }

    pub fn get_parent(&self) -> Option<ScopeRef<V, T>> {
        match self.parent {
            Some(ref parent) => Some(parent.clone()),
            None => None
        }
    }

    pub fn define(&mut self, name: String, ttype: Option<Type>) {
        // TODO how do you allocate the type variables
        match self.names.contains_key(&name) {
            true => panic!("NameError: variable is already defined; {:?}", name),
            false => {
                let sym = Symbol { ttype: ttype.unwrap_or_else(|| self.new_typevar()), value: None };
                self.names.insert(name, sym)
            },
        };
    }

    pub fn modify<F>(&mut self, name: &String, mut f: F) -> () where F: FnMut(&mut Symbol<V>) -> () {
        match self.names.entry(name.clone()) {
            Entry::Vacant(_) => panic!("NameError: variable is undefined; {:?}", name),
            Entry::Occupied(mut entry) => f(entry.get_mut()),
        }
    } 

    pub fn assign(&mut self, name: &String, value: V) {
        self.modify(name, move |sym| sym.value = Some(value.clone()))
    }

    pub fn update_variable_type(&mut self, name: &String, ttype: Type) {
        self.modify(name, move |sym| sym.ttype = ttype.clone())
    }

    //pub fn swap_variable_type(&mut self, name: &String, f: &Fn(Type) -> Type) {
    //    match self.names.entry(name.clone()) {
    //        Entry::Vacant(_) => panic!("NameError: variable is undefined; {:?}", name),
    //        Entry::Occupied(mut entry) => {
    //            let sym = entry.get_mut();
    //            sym.ttype = f(sym.ttype.clone());
    //        },
    //    };
    //}

    pub fn find(&self, name: &String) -> Option<Symbol<V>> {
        let sym = self.names.get(name);
        if sym.is_some() && !self.no_lookup {
            Some(sym.unwrap().clone())
        }
        else if let Some(ref parent) = self.parent {
            parent.borrow().find(name).map(|x| x.clone())
        }
        else {
            None
        }
    }

    pub fn search<F, U>(&self, name: &String, f: F) -> Option<U> where F: Fn(&Symbol<V>) -> Option<U> {
        if let Some(ref info) = self.names.get(name) {
            f(info)
        }
        else if let Some(ref parent) = self.parent {
            parent.borrow().search(name, f)
        }
        else {
            None
        }
    }

    pub fn get_variable_type(&self, name: &String) -> Option<Type> {
        self.search(name, |sym| Some(sym.ttype.clone()))
    }

    /*
    pub fn find_and<F, R>(&mut self, name: &String, mut f: F) -> Option<R> where F: FnMut(&mut Symbol) -> Option<R> {
        if let Some(ref mut sym) = self.names.get(name) {
            f(sym)
        }
        else if let Some(ref parent) = self.parent {
            parent.borrow_mut().find_and(name, f)
        }
        else {
            None
        }
    }

    pub fn foreach<F>(&mut self, mut f: F) where F: FnMut(&mut Symbol) -> () {
        for (ref name, ref mut sym) in &mut self.names {
            f(sym)
        }
    }
    */


    pub fn define_type(&mut self, name: String, ttype: Type) {
        match self.types.contains_key(&name) {
            true => panic!("NameError: type is already defined; {:?}", name),
            //false => self.types.insert(name, ttype),
            false => self.types.insert(name, TypeInfo { ttype: ttype, structdef: vec!(), parent: None, classdef: None, value: None }),
        };
    }

    pub fn contains_type(&self, name: &String) -> bool {
        self.types.contains_key(name)
    }

    pub fn search_type<F, U>(&self, name: &String, f: F) -> Option<U> where F: Fn(&TypeInfo<V, T>) -> Option<U> {
        if let Some(ref info) = self.types.get(name) {
            f(info)
        }
        else if let Some(ref parent) = self.parent {
            parent.borrow().search_type(name, f)
        }
        else {
            None
        }
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


    pub fn find_type(&self, name: &String) -> Option<Type> {
        self.search_type(name, |info| Some(info.ttype.clone()))
/*
        if let Some(info) = self.types.get(name) {
            Some(info.ttype.clone())
        }
        else if let Some(ref parent) = self.parent {
            parent.borrow().find_type(name).map(|x| x.clone())
        }
        else {
            None
        }
*/
    }

    pub fn update_type(&mut self, name: &String, ttype: Type) {
        self.modify_type(name, |info| {
            println!("CHANGE: {:?} from {:?} to {:?}", name, info.ttype, ttype);
            info.ttype = ttype.clone()
        })
        /*
        if let Entry::Occupied(mut entry) = self.types.entry(name.clone()) {
            println!("CHANGE: {:?} from {:?} to {:?}", name, entry.get().ttype, ttype);
            // *entry.get_mut() = expect_type(Rc::new(RefCell::new(self.clone())), Some(entry.get().clone()), Some(ttype));
            (*entry.get_mut()).ttype = ttype;
        }
        else if let Some(ref parent) = self.parent {
            parent.borrow_mut().update_type(name, ttype);
        }
        */
    }

    pub fn swap_type<F>(&mut self, name: &String, f: F) where F: Fn(Type) -> Type {
        match self.types.entry(name.clone()) {
            Entry::Vacant(_) => panic!("NameError: type is undefined; {:?}", name),
            Entry::Occupied(mut entry) => {
                (*entry.get_mut()).ttype = f(entry.get().ttype.clone());
            },
        };
    }


    pub fn set_class_def(&mut self, name: &String, parentclass: Option<String>, classdef: ScopeRef<V, T>) {
        self.modify_type(name, |info| {
            info.parent = parentclass.clone();
            info.classdef = Some(classdef.clone());
        })
        /*
        if let Entry::Occupied(mut entry) = self.types.entry(name.clone()) {
            (*entry.get_mut()).parent = parentclass;
            (*entry.get_mut()).classdef = Some(classdef);
        }
        else if let Some(ref parent) = self.parent {
            parent.borrow_mut().set_class_def(name, parentclass, classdef);
        }
        */
    }

    pub fn find_class_def(&self, name: &String) -> Option<ScopeRef<V, T>> {
        self.search_type(name, |info| info.classdef.clone())
    }

    pub fn get_parent_class_name(&self, name: &String) -> Option<String> {
        self.search_type(name, |info| info.parent.clone())
    }

    pub fn get_parent_class_def(&self, name: &String) -> Option<ScopeRef<V, T>> {
        self.search_type(name, |info| {
            match info.parent {
                Some(ref name) => self.search_type(name, |info| info.classdef.clone()),
                None => None,
            }
        })
    }

    pub fn set_struct_def(&mut self, name: &String, structdef: Vec<(String, Type)>) {
        self.modify_type(name, |info| info.structdef = structdef.clone())
        /*
        if let Entry::Occupied(mut entry) = self.types.entry(name.clone()) {
            (*entry.get_mut()).structdef = structdef;
        }
        else if let Some(ref parent) = self.parent {
            parent.borrow_mut().set_struct_def(name, structdef);
        }
        */
    }

    pub fn find_struct_def(&self, name: &String) -> Option<Vec<(String, Type)>> {
        self.search_type(name, |info| Some(info.structdef.clone()))
    }

    pub fn set_type_value(&mut self, name: &String, value: T) {
        self.modify_type(name, |info| info.value = Some(value.clone()))
    }

    pub fn get_type_value(&self, name: &String) -> Option<T> {
        self.search_type(name, |info| info.value.clone())
    }

    pub fn new_typevar(&mut self) -> Type {
        for ch in b'a' .. b'z' + 1 {
            let name = (ch as char).to_string();
            if self.find_type(&name).is_none() {
                let ttype = Type::Variable(name.clone());
                self.define_type(name, ttype.clone());
                return ttype;
            }
        }
        panic!("Fuck");
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct ScopeMapRef<V, T>(Rc<RefCell<HashMap<UniqueID, ScopeRef<V, T>>>>);

impl<V, T> ScopeMapRef<V, T> where V: Clone, T: Clone {
    pub fn new() -> ScopeMapRef<V, T> {
        ScopeMapRef(Rc::new(RefCell::new(HashMap::new())))
    }

    pub fn add(&self, id: UniqueID, parent: Option<ScopeRef<V, T>>) -> ScopeRef<V, T> {
        let scope = Scope::new_ref(parent);
        self.0.borrow_mut().insert(id, scope.clone());
        //scope.borrow_mut().manager = Some(self.clone());
        scope
    }

    pub fn get(&self, id: &UniqueID) -> ScopeRef<V, T> {
        self.0.borrow().get(id).unwrap().clone()
    }

    pub fn get_global(&self) -> ScopeRef<V, T> {
        self.get(&UniqueID(1))
    }
}



pub fn make_global<V, T>(map: ScopeMapRef<V, T>) -> ScopeRef<V, T> where V: Clone, T: Clone {
    //let primatives = Scope::new_ref(None);
    let primatives = map.add(UniqueID(0), None);

    primatives.borrow_mut().define_type(String::from("Nil"), Type::Concrete(String::from("Nil")));
    primatives.borrow_mut().define_type(String::from("Int"), Type::Concrete(String::from("Int")));
    primatives.borrow_mut().define_type(String::from("Real"), Type::Concrete(String::from("Real")));
    primatives.borrow_mut().define_type(String::from("String"), Type::Concrete(String::from("String")));
    primatives.borrow_mut().define_type(String::from("Bool"), Type::Concrete(String::from("Bool")));
    primatives.borrow_mut().define_type(String::from("Class"), Type::Concrete(String::from("Class")));

    let bintype = parse_type("('a, 'a) -> 'a");
    let booltype = parse_type("('a, 'a) -> Bool");
    primatives.borrow_mut().define(String::from("*"), bintype.clone());
    primatives.borrow_mut().define(String::from("/"), bintype.clone());
    primatives.borrow_mut().define(String::from("^"), bintype.clone());
    primatives.borrow_mut().define(String::from("%"), bintype.clone());
    primatives.borrow_mut().define(String::from("+"), bintype.clone());
    primatives.borrow_mut().define(String::from("-"), bintype.clone());
    primatives.borrow_mut().define(String::from("<<"), bintype.clone());
    primatives.borrow_mut().define(String::from(">>"), bintype.clone());
    primatives.borrow_mut().define(String::from("<"), booltype.clone());
    primatives.borrow_mut().define(String::from(">"), booltype.clone());
    primatives.borrow_mut().define(String::from("<="), booltype.clone());
    primatives.borrow_mut().define(String::from(">="), booltype.clone());
    primatives.borrow_mut().define(String::from("=="), booltype.clone());
    primatives.borrow_mut().define(String::from("!="), booltype.clone());
    primatives.borrow_mut().define(String::from("&"), bintype.clone());
    primatives.borrow_mut().define(String::from("|"), bintype.clone());
    primatives.borrow_mut().define(String::from("and"), booltype.clone());
    primatives.borrow_mut().define(String::from("or"), booltype.clone());
    primatives.borrow_mut().define(String::from("~"), parse_type("(Int) -> Int"));
    primatives.borrow_mut().define(String::from("not"), parse_type("(Int) -> Bool"));

    primatives.borrow_mut().define(String::from("puts"), parse_type("(String) -> Bool"));
    primatives.borrow_mut().define(String::from("malloc"), parse_type("(Int) -> 'a"));
    primatives.borrow_mut().define(String::from("realloc"), parse_type("('a, Int) -> 'a"));
    primatives.borrow_mut().define(String::from("free"), parse_type("('a) -> Nil"));

    primatives.borrow_mut().define(String::from("str"), parse_type("(Int) -> String"));

    //let global = Scope::new_ref(Some(primatives));
    let global = map.add(UniqueID(1), Some(primatives));
    global.borrow_mut().set_global(true);
    return global;
}

pub fn bind_names<V, T>(code: &mut Vec<AST>) -> ScopeMapRef<V, T> where V: Clone, T: Clone {
    let map = ScopeMapRef::new();
    let global = make_global(map.clone());

    bind_names_vec(map.clone(), global.clone(), code);
    return map;
}

pub fn bind_names_vec<V, T>(map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>, code: &mut Vec<AST>) where V: Clone, T: Clone {
    for node in code {
        bind_names_node(map.clone(), scope.clone(), node);
    }
}

fn bind_names_node<V, T>(map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>, node: &mut AST) where V: Clone, T: Clone {
    match *node {
        AST::Definition((ref name, ref ttype), ref mut code) => {
            scope.borrow_mut().define(name.clone(), ttype.clone());
            bind_names_node(map.clone(), scope.clone(), code);
        },

        AST::Function(ref name, ref args, _, ref mut body, ref id) => {
            let fscope = map.add(id.clone(), Some(scope.clone()));

            if name.is_some() {
                scope.borrow_mut().define(name.clone().unwrap(), None);
            }

            for arg in args {
                fscope.borrow_mut().define(arg.0.clone(), arg.1.clone());
            }

            bind_names_node(map.clone(), fscope.clone(), body)
        },

        AST::Identifier(ref name) => {
            if scope.borrow().find(name).is_none() {
                panic!("NameError: undefined identifier {:?}", name);
            }
        }


        AST::List(ref mut code) |
        AST::Block(ref mut code) => { bind_names_vec(map, scope, code); },

        AST::Index(ref mut base, ref mut index) => {
            bind_names_node(map.clone(), scope.clone(), base);
            bind_names_node(map.clone(), scope.clone(), index);
        },

        AST::Resolver(ref mut left, ref mut right) => {
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

        AST::Accessor(ref mut left, ref mut right, _) => {
            bind_names_node(map.clone(), scope, left);
        },

        AST::Assignment(ref mut left, ref mut right) => {
            match **left {
                AST::Accessor(_, _, _) | AST::Index(_, _) => { },
                _ => panic!("SyntaxError: assignment to something other than a list or class element: {:?}", left),
            };
            bind_names_node(map.clone(), scope.clone(), left);
            bind_names_node(map.clone(), scope.clone(), right);
        },

        AST::Invoke(ref mut fexpr, ref mut args, ref ttype) => {
            if let AST::Accessor(ref mut expr, _, _) = **fexpr {
                args.insert(0, *expr.clone());
            }
            bind_names_node(map.clone(), scope.clone(), fexpr);
            bind_names_vec(map.clone(), scope.clone(), args);
        },

        AST::If(ref mut cond, ref mut texpr, ref mut fexpr) => {
            bind_names_node(map.clone(), scope.clone(), cond);
            bind_names_node(map.clone(), scope.clone(), texpr);
            bind_names_node(map.clone(), scope.clone(), fexpr);
        },

        AST::Raise(ref mut expr) => {
            bind_names_node(map.clone(), scope.clone(), expr);
        },

        AST::Try(ref mut cond, ref mut cases) |
        AST::Match(ref mut cond, ref mut cases) => {
            bind_names_node(map.clone(), scope.clone(), cond);
            for &mut (ref mut case, ref mut body) in cases {
                bind_names_node(map.clone(), scope.clone(), case);
                bind_names_node(map.clone(), scope.clone(), body);
            }
        },

        AST::For(ref name, ref mut cond, ref mut body, ref id) => {
            let lscope = map.add(id.clone(), Some(scope.clone()));
            //let lscope = map.get(id);
            //lscope.borrow_mut().set_parent(scope.clone());
            lscope.borrow_mut().define(name.clone(), None);
            bind_names_node(map.clone(), lscope.clone(), cond);
            bind_names_node(map.clone(), lscope.clone(), body);
        },

        AST::While(ref mut cond, ref mut body) => {
            bind_names_node(map.clone(), scope.clone(), cond);
            bind_names_node(map.clone(), scope.clone(), body);
        },

        AST::Class(ref name, ref parent, ref mut body, ref id) => {
            // Create class name bindings for checking ast::accessors
            let classdef = Scope::new_ref(None);

            // Find the parent class definitions, which the new class will inherit from
            match *parent {
                 Some(ref parent) => match scope.borrow().find_class_def(&parent) {
                    Some(ref parentdef) => classdef.borrow_mut().set_parent(parentdef.clone()),
                    None => panic!("NameError: undefined parent class {:?} for {:?}", parent, name),
                },
                None => { }
            };

            // Add all the name definitions to the class, so we can later name and type check ast::accessors
            classdef.borrow_mut().define(String::from("new"), Some(Type::Function(vec!(), Box::new(Type::Class(name.clone())))));
            for ref node in body.iter() {
                match **node {
                    AST::Definition((ref name, ref ttype), ref code) => { classdef.borrow_mut().define(name.clone(), ttype.clone()) },
                    AST::Function(ref name, _, _, _, _) => { classdef.borrow_mut().define(name.clone().unwrap(), None) },
                    _ => { }
                }
            }

            // Define the class in the local scope
            scope.borrow_mut().define_type(name.clone(), Type::Class(name.clone()));
            scope.borrow_mut().set_class_def(name, parent.clone(), classdef.clone());
            // TODO i don't like this type == Class thing, but i don't know how i'll do struct types yet either
            //scope.borrow_mut().define(name.clone(), Some(Type::Class(name.clone())));

            let tscope = map.add(id.clone(), Some(scope.clone()));
            // Create a temporary invisible scope to name check the class body
            tscope.borrow_mut().define_type(String::from("Self"), Type::Class(name.clone()));
            if parent.is_some() {
                tscope.borrow_mut().define_type(String::from("Super"), Type::Class(parent.clone().unwrap()));
            }
            tscope.borrow_mut().set_no_lookup(true);
            bind_names_vec(map.clone(), tscope.clone(), body);
            tscope.borrow_mut().set_no_lookup(false);
        },

        // TODO AST::Type

        _ => { }
    }
}

