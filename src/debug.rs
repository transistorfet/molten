
use parser::AST;
use scope::ScopeRef;

pub fn print_types(scope: ScopeRef, code: &Vec<AST>) {
    for node in code {
        print_types_node(scope.clone(), node);
    }
}

pub fn print_types_node(scope: ScopeRef, node: &AST) {
    match *node {
        AST::Block(ref body) => print_types(scope.clone(), body),
        AST::Function(ref args, ref body, ref fscope) => {
            print_types_scope(fscope.clone());
            print_types_node(fscope.clone(), body);
        },
        AST::Definition((ref name, ref ttype), ref body) => {
            println!("\nDefining: {:?} {:?}", name, ttype);
            print_types_node(scope.clone(), body);
        },
        _ => ()
    }
}

pub fn print_types_scope(scope: ScopeRef) {
    println!("\nNames:");
    for (ref name, ref sym) in &scope.borrow().names {
        println!("{:?} {:?}", name, sym.ttype);
    }

    println!("\nTypes:");
    for (ref name, ref ttype) in &scope.borrow().types {
        println!("{:?} {:?}", name, ttype);
    }
}

 
