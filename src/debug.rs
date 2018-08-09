
use ast::AST;
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

pub fn print_types(map: &ScopeMapRef, scope: ScopeRef, code: &Vec<AST>) {
    for node in code {
        print_types_node(map, scope.clone(), node);
    }
}

pub fn print_types_node(map: &ScopeMapRef, scope: ScopeRef, node: &AST) {
    match *node {
        AST::Block(_, _, ref body) => print_types(map, scope, body),
        AST::Function(ref id, _, _, _, _, ref body, _) => {
            let fscope = map.get(id);
            print_types_scope(fscope.clone());
            print_types_node(map, fscope, body);
        },
        AST::Definition(_, _, ref name, ref ttype, ref body) => {
            println!("\nDefining: {:?} {:?}", name, ttype);
            print_types_node(map, scope, body);
        },
        _ => ()
    }
}

pub fn print_types_scope(scope: ScopeRef) {
    println!("\nNames:");
    for (ref name, ref sym) in scope.names.borrow().iter() {
        println!("{:?} {:?} {:?}", name, sym.ttype, sym.funcdefs);
    }

    println!("\nTypes:");
    for (ref name, ref info) in scope.types.borrow().iter() {
        println!("{:?} {:?}", name, info.ttype);
    }
}

 
