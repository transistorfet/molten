
use session::Session;
use scope::{ ScopeRef };
use hir::{ Expr, ExprKind };

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

pub fn print_types(session: &Session, scope: ScopeRef, code: &Vec<Expr>) {
    for node in code {
        print_types_node(session, scope.clone(), node);
    }
}

pub fn print_types_node(session: &Session, scope: ScopeRef, node: &Expr) {
    match &node.kind {
        ExprKind::Block(body) => print_types(session, scope, body),
        ExprKind::Function(_, _, _, _, body, _) => {
            let defid = session.get_ref(node.id).unwrap();
            let fscope = session.map.get(&defid);
            print_types_scope(session, fscope.clone());
            print_types_node(session, fscope, body);
        },
        ExprKind::Definition(mutable, name, ttype, body) => {
            println!("\nDefining {:?}: {:?} {:?}", mutable, name, ttype);
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

