
use std::fs::File;
use std::io::prelude::*;

use types::Type;
use scope::{ ScopeRef, unmangle_name };
use parser::{ AST, Decl, parse_index_or_error };


pub fn load_index<V, T>(scope: ScopeRef<V, T>, filename: &str) -> Vec<Decl> where V: Clone, T: Clone {
    let mut f = File::open(filename).expect("Error: file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("Error reading file contents");

    let decl = parse_index_or_error(filename, contents.as_bytes());
    println!("DECL: {:?}", decl);
    load_index_vec(scope, &decl);
    decl
}

pub fn load_index_vec<V, T>(scope: ScopeRef<V, T>, code: &Vec<Decl>) where V: Clone, T: Clone {
    for node in code {
        load_index_node(scope.clone(), node);
    }
}

fn load_index_node<V, T>(scope: ScopeRef<V, T>, node: &Decl) where V: Clone, T: Clone {
    match *node {
        Decl::Symbol(ref name, ref ttype) => {
            scope.borrow_mut().define_func(name.clone(), Some(ttype.clone()), true);
            if let Some(ref name) = unmangle_name(name) {
                println!("OVERLOAD: {:?}", name);
                scope.borrow_mut().define_func(name.clone(), None, true);
                let mut stype = scope.borrow().get_variable_type(name).unwrap_or(Type::Overload(vec!()));
                stype = stype.add_variant(scope.clone(), ttype.clone());
                scope.borrow_mut().update_variable_type(name, stype);
            }
        },
        Decl::Class((ref name, ref types), ref parent, ref body) => {
            // TODO how will you import classes?  I guess it's just functions that get imported, but don't you still need the structdef too, to access members?
            let classdef = scope.borrow_mut().create_class_def(name, parent.clone());
            load_index_vec(classdef.clone(), body);
        }
    }
}

pub fn store_index<V, T>(scope: ScopeRef<V, T>, filename: &str, code: &Vec<AST>) where V: Clone, T: Clone {
    let index_text = build_index(scope, code);
    let mut index_file = File::create(filename).expect("Error creating index file");
    index_file.write_all(index_text.as_bytes()).unwrap();
    println!("{}", index_text);
}

pub fn build_index<V, T>(scope: ScopeRef<V, T>, code: &Vec<AST>) -> String where V: Clone, T: Clone {
    let mut index = String::new();
    for node in code {
        build_index_node(&mut index, scope.clone(), node);
    }
    index
}

fn build_index_node<V, T>(index: &mut String, scope: ScopeRef<V, T>, node: &AST) where V: Clone, T: Clone {
    match *node {
        AST::Function(ref name, ref args, ref rtype, ref body, ref id) => {
            if let Some(ref name) = *name {
                let ttype = scope.borrow().get_variable_type(name).unwrap();
                index.push_str(format!("decl {} : {}\n", name, unparse_type(ttype)).as_str());
            }
        },

        AST::Definition((_, _), ref body) => match **body {
            ref node @ AST::Class(_, _, _, _) => build_index_node(index, scope, node),
            _ => { },
        },

        AST::Class((ref name, ref types), ref parent, ref body, ref id) => {
            let classdef = scope.borrow().get_class_def(name).unwrap();
            index.push_str(format!("class {} {{\n", if parent.is_some() { format!("{} extends {}", name, unparse_type(Type::make_object(parent.clone().unwrap()))) } else { name.clone() }).as_str());
            for node in body {
                match *node {
                    AST::Definition((ref name, ref ttype), _) => {
                        //let stype = classdef.borrow().get_variable_type(name).unwrap();
                        //println!("WHICH ONE??? {:?} {:?}", ttype, stype);
                        index.push_str(format!("    decl {} : {}\n", name, unparse_type(ttype.clone().unwrap())).as_str());
                    },
                    AST::Function(ref name, _, _, _, _) => {
                        if let Some(ref name) = *name {
                            let ttype = classdef.borrow().get_variable_type(name).unwrap();
                            index.push_str(format!("    decl {} : {}\n", name, unparse_type(ttype)).as_str());
                        }
                    },
                    _ => {  },
                }
            }
            index.push_str(format!("}}\n").as_str());
        },
        _ => { },
    }
}

pub fn unparse_type(ttype: Type) -> String {
    match ttype {
        Type::Object(name, types) => {
            let params = if types.len() > 0 { format!("[{}]", types.iter().map(|p| unparse_type(p.clone())).collect::<Vec<String>>().join(", ")) } else { String::from("") };
            name.clone() + &params
        },
        Type::Variable(name) => format!("'{}", name),
        Type::List(stype) => format!("List[{}]", unparse_type(*stype)),
        Type::Function(args, ret) => {
            let argstr: Vec<String> = args.iter().map(|t| unparse_type(t.clone())).collect();
            format!("({}) -> {}", argstr.join(", "), unparse_type(*ret))
        }
        Type::Overload(variants) => {
            let varstr: Vec<String> = variants.iter().map(|v| unparse_type(v.clone())).collect();
            format!("Overload[{}]", varstr.join(", "))
        },
    }
}


