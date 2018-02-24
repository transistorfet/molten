
use parser::AST;
//use utils::UniqueID;
use scope::{ ScopeRef, ScopeMapRef };

macro_rules! debug {
    //($fmt:expr, $($arg:expr),*) => {
    //    println!($fmt, $(unsafe_render(&$arg)),*)
    //}
    ($fmt:expr, $($arg:tt)*) => {
        use config::Options;
        if Options::as_ref().debug {
            println!($fmt, $($arg)*)
        }
    }
}

pub fn print_types<V, T>(map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>, code: &Vec<AST>) where V: Clone, T: Clone {
    for node in code {
        print_types_node(map.clone(), scope.clone(), node);
    }
}

pub fn print_types_node<V, T>(map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>, node: &AST) where V: Clone, T: Clone {
    match *node {
        AST::Block(_, ref body) => print_types(map.clone(), scope, body),
        AST::Function(_, _, _, _, ref body, ref id) => {
            let fscope = map.get(id);
            print_types_scope(fscope.clone());
            print_types_node(map.clone(), fscope.clone(), body);
        },
        AST::Definition(_, (ref name, ref ttype), ref body) => {
            println!("\nDefining: {:?} {:?}", name, ttype);
            print_types_node(map.clone(), scope, body);
        },
        _ => ()
    }
}

pub fn print_types_scope<V, T>(scope: ScopeRef<V, T>) where V: Clone, T: Clone {
    println!("\nNames:");
    for (ref name, ref sym) in &scope.borrow_mut().names {
        println!("{:?} {:?} {:?}", name, sym.ttype, sym.funcdefs);
    }

    println!("\nTypes:");
    for (ref name, ref info) in &scope.borrow_mut().types {
        println!("{:?} {:?}", name, info.ttype);
    }
}

 
