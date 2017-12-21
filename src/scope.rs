
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::hash_map::Entry;

use parser::{ AST, parse_type };
use types::Type;
use interpreter;
use compiler_llvm;


#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
    pub ttype: Type,
    pub value: Option<interpreter::Value>,
    pub address: Option<compiler_llvm::Value>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeInfo {
    pub ttype: Type,
    pub members: ScopeRef,
}


#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    pub is_global: bool,
    pub names: HashMap<String, Symbol>,
    pub types: HashMap<String, Type>,
    pub parent: Option<ScopeRef>,
}

pub type ScopeRef = Rc<RefCell<Scope>>;


impl Scope {
    pub fn new(parent: Option<ScopeRef>) -> Scope {
        Scope {
            is_global: false,
            names: HashMap::new(),
            types: HashMap::new(),
            parent: parent,
        }
    }

    pub fn new_ref(parent: Option<ScopeRef>) -> ScopeRef {
        Rc::new(RefCell::new(Scope::new(parent)))
    }

    pub fn set_parent(&mut self, parent: ScopeRef) {
        self.parent = Some(parent);
    }

    pub fn get_parent(&self) -> Option<ScopeRef> {
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
                let sym = Symbol { ttype: ttype.unwrap_or_else(|| self.new_typevar()), value: None, address: None };
                self.names.insert(name, sym)
            },
        };
    }

    pub fn modify<F>(&mut self, name: &String, mut f: F) -> () where F: FnMut(&mut Symbol) -> () {
        match self.names.entry(name.clone()) {
            Entry::Vacant(_) => panic!("NameError: variable is undefined; {:?}", name),
            Entry::Occupied(mut entry) => f(entry.get_mut()),
        }
    } 

    pub fn assign(&mut self, name: &String, value: interpreter::Value) {
        //match self.names.entry(name.clone()) {
        //    Entry::Vacant(_) => panic!("NameError: variable is undefined; {:?}", name),
        //    Entry::Occupied(mut entry) => entry.get_mut().value = Some(value),
        //}
        self.modify(name, move |sym| sym.value = Some(value.clone()))
    }

    pub fn assign_addr(&mut self, name: &String, value: compiler_llvm::Value) {
        //match self.names.entry(name.clone()) {
        //    Entry::Vacant(_) => panic!("NameError: variable is undefined; {:?}", name),
        //    Entry::Occupied(mut entry) => entry.get_mut().address = Some(value),
        //}
        self.modify(name, move |sym| sym.address = Some(value))
    }

    pub fn update_variable_type(&mut self, name: &String, ttype: Type) {
        //match self.names.entry(name.clone()) {
        //    Entry::Vacant(_) => panic!("NameError: variable is undefined; {:?}", name),
        //    Entry::Occupied(mut entry) => entry.get_mut().ttype = ttype,
        //};
        self.modify(name, move |sym| sym.ttype = ttype.clone())
    }

    pub fn swap_variable_type(&mut self, name: &String, f: &Fn(Type) -> Type) {
        match self.names.entry(name.clone()) {
            Entry::Vacant(_) => panic!("NameError: variable is undefined; {:?}", name),
            Entry::Occupied(mut entry) => {
                let sym = entry.get_mut();
                sym.ttype = f(sym.ttype.clone());
            },
        };
    }

    pub fn find(&self, name: &String) -> Option<Symbol> {
        if let Some(x) = self.names.get(name) {
            Some(x.clone())
        }
        else if let Some(ref parent) = self.parent {
            parent.borrow().find(name).map(|x| x.clone())
        }
        else {
            None
        }
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
            false => self.types.insert(name, ttype),
            //false => self.types.insert(name, TypeInfo { ttype: ttype, members: Scope::new_ref() }),
        };
    }

    pub fn contains_type(&self, name: &String) -> bool {
        self.types.contains_key(name)
    }

    pub fn find_type(&self, name: &String) -> Option<Type> {
        if let Some(x) = self.types.get(name) {
            Some(x.clone())
        }
        else if let Some(ref parent) = self.parent {
            parent.borrow().find_type(name).map(|x| x.clone())
        }
        else {
            None
        }
    }

    pub fn update_type(&mut self, name: &String, ttype: Type) {
        if let Entry::Occupied(mut entry) = self.types.entry(name.clone()) {
            println!("CHANGE: {:?} from {:?} to {:?}", name, entry.get(), ttype);
            //*entry.get_mut() = expect_type(Rc::new(RefCell::new(self.clone())), Some(entry.get().clone()), Some(ttype));
            *entry.get_mut() = ttype;
        }
        else if let Some(ref parent) = self.parent {
            parent.borrow_mut().update_type(name, ttype);
        }
    }

    pub fn swap_type(&mut self, name: &String, f: &Fn(Type) -> Type) {
        match self.types.entry(name.clone()) {
            Entry::Vacant(_) => panic!("NameError: type is undefined; {:?}", name),
            Entry::Occupied(mut entry) => {
                *entry.get_mut() = f(entry.get().clone());
            },
        };
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


pub fn make_global() -> ScopeRef {
    let primatives = Scope::new_ref(None);

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

    let global = Scope::new_ref(Some(primatives));
    global.borrow_mut().is_global = true;
    return global;
}

pub fn bind_names(code: &mut Vec<AST>) -> ScopeRef {
    let scope = make_global();

    bind_names_vec(scope.clone(), code);
    return scope;
}

pub fn bind_names_vec(scope: ScopeRef, code: &mut Vec<AST>) {
    for node in code {
        bind_names_node(scope.clone(), node);
    }
}

fn bind_names_node(scope: ScopeRef, node: &mut AST) {
    match *node {
        AST::Definition((ref name, ref ttype), ref mut code) => {
            scope.borrow_mut().define(name.clone(), ttype.clone());
            bind_names_node(scope.clone(), code);
        },

        AST::Function(ref args, ref mut body, ref mut fscope, _, _) => {
            fscope.borrow_mut().set_parent(scope.clone());

            for arg in args {
                fscope.borrow_mut().define(arg.0.clone(), arg.1.clone());
            }

            bind_names_node(fscope.clone(), body)
        },

        AST::Identifier(ref name) => {
            if scope.borrow().find(name).is_none() {
                panic!("Undefined identifier: {:?}", name);
            }
        }


        AST::List(ref mut code) |
        AST::Block(ref mut code) => { bind_names_vec(scope, code); },

        AST::Index(ref mut base, ref mut index) => {
            bind_names_node(scope.clone(), base);
            bind_names_node(scope.clone(), index);
        },

        AST::Accessor(ref mut left, ref mut right) => {
            bind_names_node(scope, left);
            // NOTE we don't search right, because it will depend on the return type of left, which we don't know yet
        },

        AST::Invoke(ref name, ref mut code) => {
            if scope.borrow().find(name).is_none() {
                panic!("Undefined identifier: {:?}", name);
            }
            bind_names_vec(scope, code);
        },

        AST::If(ref mut cond, ref mut texpr, ref mut fexpr) => {
            bind_names_node(scope.clone(), cond);
            bind_names_node(scope.clone(), texpr);
            bind_names_node(scope.clone(), fexpr);
        },

        AST::Raise(ref mut expr) => {
            bind_names_node(scope.clone(), expr);
        },

        AST::Try(ref mut cond, ref mut cases) |
        AST::Match(ref mut cond, ref mut cases) => {
            bind_names_node(scope.clone(), cond);
            for &mut (ref mut case, ref mut body) in cases {
                bind_names_node(scope.clone(), case);
                bind_names_node(scope.clone(), body);
            }
        },

        AST::For(ref name, ref mut cond, ref mut body, ref mut lscope) => {
            lscope.borrow_mut().set_parent(scope.clone());
            lscope.borrow_mut().define(name.clone(), None);
            bind_names_node(scope.clone(), cond);
            bind_names_node(scope.clone(), body);
        },

        AST::While(ref mut cond, ref mut body) => {
            bind_names_node(scope.clone(), cond);
            bind_names_node(scope.clone(), body);
        },

        AST::Class(ref name, ref mut body, ref mut cscope) => {
            // TODO i don't like this type == Class thing, but i don't know how i'll do struct types yet either
            scope.borrow_mut().define(name.clone(), Some(Type::Concrete(String::from("Class"))));
            cscope.borrow_mut().set_parent(scope.clone());
            bind_names_vec(cscope.clone(), body);
        },

        // TODO AST::Type

        _ => { }
    }
}

