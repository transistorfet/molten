
use crate::session::Session;
use crate::scope::ScopeRef;
use crate::analysis::hir::{ Expr, ExprKind };

macro_rules! debug {
    //($fmt:expr, $($arg:expr),*) => {
    //    println!($fmt, $(unsafe_render(&$arg)),*)
    //}
    ($fmt:expr, $($arg:tt)*) => {
        if crate::config::Options::as_ref().debug {
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
        ExprKind::Function(func) => {
            let defid = session.get_ref(node.id).unwrap();
            let fscope = session.map.get(defid).unwrap();
            print_types_scope(session, fscope.clone());
            print_types(session, fscope, &func.body);
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
    scope.foreach_name(|name, id| {
        println!("{:?} {:?} {:?}", name, session.get_type(id), id);
    });

    println!("\nTypes:");
    scope.foreach_type(|name, id| {
        println!("{:?} {:?}", name, session.get_type(id));
    });
}

