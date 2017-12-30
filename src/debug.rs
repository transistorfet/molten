
use parser::AST;
//use utils::UniqueID;
use scope::{ ScopeRef, ScopeMapRef };

/*
print_code(code: &Vec<AST>) {
    for node in code {
        print_code_node(node);
    }
}

print_code_node(node: &AST) {
    println!("{:?}", node);
    match *node {
        AST::Block(ref body) => { print_code(body) },
        _ => 
    }
}
*/


pub fn print_types<V, T>(map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>, code: &Vec<AST>) where V: Clone, T: Clone {
    for node in code {
        print_types_node(map.clone(), scope.clone(), node);
    }
}

pub fn print_types_node<V, T>(map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>, node: &AST) where V: Clone, T: Clone {
    match *node {
        AST::Block(ref body) => print_types(map.clone(), scope, body),
        AST::Function(ref name, ref args, ref rtype, ref body, ref id) => {
            let fscope = map.get(id);
            print_types_scope(map.clone(), fscope.clone());
            print_types_node(map.clone(), fscope.clone(), body);
        },
        AST::Definition((ref name, ref ttype), ref body) => {
            println!("\nDefining: {:?} {:?}", name, ttype);
            print_types_node(map.clone(), scope, body);
        },
        _ => ()
    }
}

pub fn print_types_scope<V, T>(map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>) where V: Clone, T: Clone {
    println!("\nNames:");
    for (ref name, ref sym) in &scope.borrow_mut().names {
        println!("{:?} {:?}", name, sym.ttype);
    }

    println!("\nTypes:");
    for (ref name, ref info) in &scope.borrow_mut().types {
        println!("{:?} {:?}", name, info.ttype);
    }
}

 
