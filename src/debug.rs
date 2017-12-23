
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


pub fn print_types<V>(map: ScopeMapRef<V>, scope: ScopeRef<V>, code: &Vec<AST>) where V: Clone {
    for node in code {
        print_types_node(map.clone(), scope.clone(), node);
    }
}

pub fn print_types_node<V>(map: ScopeMapRef<V>, scope: ScopeRef<V>, node: &AST) where V: Clone {
    match *node {
        AST::Block(ref body) => print_types(map.clone(), scope, body),
        AST::Function(ref args, ref body, ref id, ref ftype) => {
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

pub fn print_types_scope<V>(map: ScopeMapRef<V>, scope: ScopeRef<V>) where V: Clone {
    println!("\nNames:");
    for (ref name, ref sym) in &scope.borrow_mut().names {
        println!("{:?} {:?}", name, sym.ttype);
    }

    println!("\nTypes:");
    for (ref name, ref info) in &scope.borrow_mut().types {
        println!("{:?} {:?}", name, info.ttype);
    }
}

 
