
use ast::AST;
//use utils::UniqueID;
use session::Session;
use scope::{ ScopeRef };

macro_rules! debug {
    //($fmt:expr, $($arg:expr),*) => {
    //    println!($fmt, $(unsafe_render(&$arg)),*)
    //}
    ($fmt:expr, $($arg:tt)*) => {
        if ::config::Options::as_ref().debug {
            println!($fmt, $($arg)*)
        }
    }
}

pub fn print_types(session: &Session, scope: ScopeRef, code: &Vec<AST>) {
    for node in code {
        print_types_node(session, scope.clone(), node);
    }
}

pub fn print_types_node(session: &Session, scope: ScopeRef, node: &AST) {
    match *node {
        AST::Block(_, _, ref body) => print_types(session, scope, body),
        AST::Function(ref id, _, _, _, _, ref body, _) => {
            let fscope = session.map.get(id);
            print_types_scope(session, fscope.clone());
            print_types_node(session, fscope, body);
        },
        AST::Definition(_, _, ref mutable, ref name, ref ttype, ref body) => {
            println!("\nDefining {}: {:?} {:?}", mutable, name, ttype);
            print_types_node(session, scope, body);
        },
        _ => ()
    }
}

pub fn print_types_scope(session: &Session, scope: ScopeRef) {
    println!("\nNames:");
    for (ref name, ref sym) in scope.names.borrow().iter() {
        println!("{:?} {:?} {:?}", name, sym.defid.map(|defid| session.get_type(defid)), sym.defid);
    }

    println!("\nTypes:");
    for (ref name, ref info) in scope.types.borrow().iter() {
        println!("{:?} {:?}", name, info.defid.map(|defid| session.get_type(defid)));
    }
}

 
