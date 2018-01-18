
use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use types::Type;
use utils::UniqueID;
use parser::{ AST, parse_type };
use scope::{ Scope, ScopeRef, ScopeMapRef };

#[derive(Clone, Debug, PartialEq)]
pub enum TypeValue {

}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    Boolean(bool),
    Integer(isize),
    Real(f64),
    String(String),
    List(Vec<Value>),
    Function(ScopeRef<Value, TypeValue>, Vec<(String, Option<Type>, Option<AST>)>, AST),
    Builtin(fn(ScopeRef<Value, TypeValue>, Vec<Value>) -> Value),
    AST(AST),
}

impl Value {
    fn is_true(&self) -> bool {
        self != &Value::Nil && self != &Value::Boolean(false)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Value::Function(ref fscope, ref args, ref body) => write!(f, "Function({:?})", body),
            _ => write!(f, "{:?}", self)
        }
    }
}




#[derive(Clone, Debug, PartialEq)]
pub struct Env {
    pub names: HashMap<String, Value>,
    pub parent: Option<EnvRef>,
}

pub type EnvRef = Rc<RefCell<Env>>;


impl Env {
    pub fn new(parent: Option<EnvRef>) -> Env {
        Env {
            names: HashMap::new(),
            parent: parent,
        }
    }

    pub fn new_ref(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(RefCell::new(Env::new(parent)))
    }

    pub fn set_parent(&mut self, parent: EnvRef) {
        self.parent = Some(parent);
    }

    pub fn assign(&mut self, name: String, value: Value) {
        //match self.names.entry(name.clone()) {
        //    Entry::Vacant(_) => panic!("NameError: variable is undefined; {:?}", name),
        //    Entry::Occupied(mut entry) => Rc::get_mut(entry.get_mut()).unwrap().value = Some(value),
        //}
        match self.names.contains_key(&name) {
            true => panic!("NameError: variable is already defined; {:?}", name),
            false => self.names.insert(name, value)
        };
    }

    pub fn find(&self, name: &String) -> Option<Value> {
        if let Some(x) = self.names.get(name) {
            return Some(x.clone());
        } else if let Some(ref parent) = self.parent {
            return parent.borrow().find(name).map(|x| x.clone());
        } else {
            return None;
        }
    }
}





pub fn execute(map: ScopeMapRef<Value, TypeValue>, code: &Vec<AST>) {
    let scope = map.get_global();
    let mut machine = Interpreter::new();
    builtins::assign_builtins(scope.borrow().get_parent().unwrap().clone());
    let result = machine.execute_vec(map, scope, code);

}

pub struct Interpreter {

}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {

        }
    }

    pub fn execute_vec(&mut self, map: ScopeMapRef<Value, TypeValue>, scope: ScopeRef<Value, TypeValue>, code: &Vec<AST>) -> Value {
        let mut last: Value = Value::Nil;

        for node in code {
            last = self.execute_node(map.clone(), scope.clone(), node);
            println!("PARTIAL: {}", last);
        }
        last
    }

    pub fn execute_node(&mut self, map: ScopeMapRef<Value, TypeValue>, scope: ScopeRef<Value, TypeValue>, node: &AST) -> Value {
        match *node {
            AST::Nil(_) => Value::Nil,
            AST::Boolean(boolean) => Value::Boolean(boolean),
            AST::Integer(num) => Value::Integer(num),
            AST::Real(num) => Value::Real(num),
            AST::String(ref string) => Value::String(string.clone()),

            AST::List(ref items) => {
                let mut list = vec!();
                for item in items {
                    list.push(self.execute_node(map.clone(), scope.clone(), item));
                }
                Value::List(list)
            },

            AST::Identifier(ref name) => {
                match scope.borrow().find(name).unwrap().value.clone() {
                    None => panic!("UnsetError: use before assignment {:?}", name),
                    Some(x) => x
                }
            },

            AST::Invoke(ref fexpr, ref args, _) => {
                let func = self.execute_node(map.clone(), scope.clone(), fexpr);

                let mut values = vec!();
                for arg in args {
                    values.push(self.execute_node(map.clone(), scope.clone(), arg));
                }

                if let Value::Function(ref fscope, ref argdefs, ref body) = func {
                    let lscope = Scope::new_ref(fscope.borrow().get_parent());
                    for (ref def, value) in argdefs.iter().zip(values.iter()) {
                        lscope.borrow_mut().define(def.0.clone(), def.1.clone());
                        lscope.borrow_mut().assign(&def.0, value.clone());
                    }

                    self.execute_node(map.clone(), lscope.clone(), body)
                } else if let Value::Builtin(ref funcptr) = func {
                    funcptr(scope.clone(), values)
                } else {
                    panic!("Function not found: {:?}", fexpr);
                }
            },

            AST::Function(ref name, ref args, ref rtype, ref body, ref id) => {
                let fscope = map.get(id);
                Value::Function(fscope.clone(), args.clone(), *body.clone())
            },

            AST::Definition((ref name, ref ttype), ref body) => {
                let value = self.execute_node(map.clone(), scope.clone(), body);
                scope.borrow_mut().assign(name, value.clone());
                value
            },

            AST::Block(ref body) => { self.execute_vec(map.clone(), scope.clone(), body) },

            AST::If(ref cond, ref texpr, ref fexpr) => {
                let result = self.execute_node(map.clone(), scope.clone(), cond);
                if result.is_true() {
                    self.execute_node(map.clone(), scope.clone(), texpr)
                } else {
                    self.execute_node(map.clone(), scope.clone(), fexpr)
                }
            },

    /*
            AST::Raise(ref expr) => {

            },

            AST::Try(ref cond, ref cases) => {

            },

            AST::For(ref name, ref cond, ref body, ref id) => {
                let lscope = map.get(id);

            },
    */

    /*
            AST::Match(ref cond, ref cases) => {
                let old = self.indent.clone();
                self.indent = old.clone() + &"    ";

                // TODO should you implement this as an if statement instead?
                let mut compiled_cases = vec!();
                for &(ref case, ref expr) in cases {
                    compiled_cases.push(format!("{space}  case {}:\n{indent}{}\n{indent}break;", self.execute_node(map.clone(), scope.clone(), case), self.execute_node(map.clone(), scope.clone(), expr), space=old, indent=self.indent));
                }
                let compiled = format!("select ({}) {{\n{}\n{space}}}", self.execute_node(map.clone(), scope.clone(), cond), compiled_cases.join("\n"), space=old);

                self.indent = old;
                compiled
            },

            AST::While(ref cond, ref body) => {
                let old = self.indent.clone();
                self.indent = old.clone() + &"    ";
                let compiled = format!("while ({}) {{\n{indent}{}\n{space}}}", self.execute_node(map.clone(), scope.clone(), cond), add_terminator(self.execute_node(map.clone(), scope.clone(), body)), space=old, indent=self.indent);
                self.indent = old;
                compiled
            },

            AST::Class(ref name, ref parent, ref body, ref id) => {
                let tscope = map.get(id);

            },

            AST::Index(ref base, ref index) => {

            },

            AST::Resolver(ref left, ref right) => {

            },

            AST::Accessor(ref left, ref right, _) => {

            },

            AST::Import(_) => { },

            AST::Type(_) => { },
    */

            _ => { Value::Nil },
        }
    }

}

pub fn make_global<V, T>() -> ScopeMapRef<V, T> where V: Clone, T: Clone {
    let map = ScopeMapRef::new();
    let primatives = map.add(UniqueID(0), None);

    primatives.borrow_mut().define_type(String::from("Nil"), Type::Object(String::from("Nil")));
    primatives.borrow_mut().define_type(String::from("Int"), Type::Object(String::from("Int")));
    primatives.borrow_mut().define_type(String::from("Real"), Type::Object(String::from("Real")));
    primatives.borrow_mut().define_type(String::from("String"), Type::Object(String::from("String")));
    primatives.borrow_mut().define_type(String::from("Bool"), Type::Object(String::from("Bool")));
    primatives.borrow_mut().define_type(String::from("Class"), Type::Object(String::from("Class")));

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
    primatives.borrow_mut().define(String::from("sprintf"), parse_type("('a, String, 'b) -> Int"));

    primatives.borrow_mut().define(String::from("str"), parse_type("(Int) -> String"));

    let global = map.add(UniqueID(1), Some(primatives));
    global.borrow_mut().set_global(true);

    return map;
}

mod builtins {
    use scope::ScopeRef;
    use interpreter::Value;
    use interpreter::TypeValue;

    pub fn assign_builtins(scope: ScopeRef<Value, TypeValue>) {
        let mut scope = scope.borrow_mut();
        scope.assign(&String::from("*"), Value::Builtin(multiply));
        scope.assign(&String::from("/"), Value::Builtin(divide));
        scope.assign(&String::from("+"), Value::Builtin(add));
        scope.assign(&String::from("-"), Value::Builtin(subtract));

        scope.assign(&String::from("=="), Value::Builtin(equals));
        scope.assign(&String::from("!="), Value::Builtin(not_equals));
        scope.assign(&String::from(">"), Value::Builtin(greater_than));
        scope.assign(&String::from("<"), Value::Builtin(less_than));

        //let ops = vec!("*", "/", "%", "+", "-", "<<", ">>", "<", ">", "<=", ">=", "==", "!=", "&", "|", "and", "or", "not");

    }

    macro_rules! for_numbers {
        ( $args:ident, $a:ident, $b:ident, $x:expr, $t:path ) => {
            match (&$args[0], &$args[1]) {
                (&Value::Integer(ref $a), &Value::Integer(ref $b)) => $t($x),
                (&Value::Real(ref $a), &Value::Real(ref $b)) => $t($x),
                _ => panic!("RuntimeError: expected Integer or Real value {:?} {:?}", $args[0], $args[1]),
            }
        };

        ( $args:ident, $a:ident, $b:ident, $x:expr ) => {
            match (&$args[0], &$args[1]) {
                (&Value::Integer(ref $a), &Value::Integer(ref $b)) => Value::Integer($x),
                (&Value::Real(ref $a), &Value::Real(ref $b)) => Value::Real($x),
                _ => panic!("RuntimeError: expected Integer or Real value {:?} {:?}", $args[0], $args[1]),
            }
        }
    }

    fn multiply(scope: ScopeRef<Value, TypeValue>, args: Vec<Value>) -> Value {
        for_numbers!(args, a, b, a * b)
    }

    fn divide(scope: ScopeRef<Value, TypeValue>, args: Vec<Value>) -> Value {
        for_numbers!(args, a, b, a / b)
    }

    fn add(scope: ScopeRef<Value, TypeValue>, args: Vec<Value>) -> Value {
        for_numbers!(args, a, b, a + b)
    }

    fn subtract(scope: ScopeRef<Value, TypeValue>, args: Vec<Value>) -> Value {
        for_numbers!(args, a, b, a - b)
    }

    fn equals(scope: ScopeRef<Value, TypeValue>, args: Vec<Value>) -> Value {
        for_numbers!(args, a, b, a == b, Value::Boolean)
    }

    fn not_equals(scope: ScopeRef<Value, TypeValue>, args: Vec<Value>) -> Value {
        for_numbers!(args, a, b, a != b, Value::Boolean)
    }

    fn greater_than(scope: ScopeRef<Value, TypeValue>, args: Vec<Value>) -> Value {
        for_numbers!(args, a, b, a > b, Value::Boolean)
    }

    fn less_than(scope: ScopeRef<Value, TypeValue>, args: Vec<Value>) -> Value {
        for_numbers!(args, a, b, a < b, Value::Boolean)
    }
}

