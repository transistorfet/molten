
use std::fmt::Debug;
use std::fs::File;
use std::io::prelude::*;

use types::Type;
use scope::{ ScopeRef };
use parser::{ AST, parse_or_error };


pub fn load_index<V, T>(scope: ScopeRef<V, T>, filename: &str) -> Vec<AST> where V: Clone + Debug, T: Clone + Debug {
    let mut f = File::open(filename).expect("Error: file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("Error reading file contents");

    let decl = parse_or_error(filename, contents.as_bytes());
    println!("DECL: {:?}", decl);
    //load_index_vec(scope, &decl);
    decl
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
        AST::Function(ref name, _, _, _, _) => {
            if let Some(ref name) = *name {
                let ttype = scope.borrow().get_variable_type(name).unwrap();
                index.push_str(format!("decl {} : {}\n", name, unparse_type(ttype)).as_str());
            }
        },

        AST::Definition((_, _), ref body) => match **body {
            ref node @ AST::Class(_, _, _, _) => build_index_node(index, scope, node),
            _ => { },
        },

        AST::Class((ref name, ref types), ref parent, ref body, _) => {
            let classdef = scope.borrow().get_class_def(name);
            let namespec = unparse_type(Type::Object(name.clone(), types.clone()));
            let fullspec = if parent.is_some() {
                format!("{} extends {}", namespec, unparse_type(Type::make_object(parent.clone().unwrap())))
            } else {
                namespec.clone()
            };

            index.push_str(format!("class {} {{\n", fullspec).as_str());
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


