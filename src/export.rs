
use std::fs::File;
use std::io::prelude::*;

use ast::AST;
use types::Type;
use utils::UniqueID;
use config::Options;
use scope::{ ScopeRef, ScopeMapRef };


pub fn write_exports<V, T>(map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>, filename: &str, code: &Vec<AST>) where V: Clone, T: Clone {
    let index_text = build_index(map, scope, code);
    let mut index_file = File::create(filename).expect("Error creating index file");
    index_file.write_all(index_text.as_bytes()).unwrap();
    if Options::as_ref().debug {
        println!("{}", index_text);
    }
}

pub fn build_index<V, T>(map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>, code: &Vec<AST>) -> String where V: Clone, T: Clone {
    let mut index = String::new();
    for node in code {
        build_index_node(&mut index, map.clone(), scope.clone(), node);
    }
    index
}

fn build_index_node<V, T>(index: &mut String, map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>, node: &AST) where V: Clone, T: Clone {
    match *node {
        AST::Function(_, ref name, _, _, _, _) => {
            if let Some(ref name) = *name {
                let ttype = scope.borrow().get_variable_type(name).unwrap();
                index.push_str(format!("decl {} : {}\n", name, unparse_type(scope.clone(), ttype)).as_str());
            }
        },

        AST::Definition(_, (_, _), ref body) => match **body {
            ref node @ AST::Class(_, _, _, _, _) => build_index_node(index, map, scope, node),
            _ => { },
        },

        AST::Class(_, (ref name, ref types), ref parent, ref body, ref id) => {
            let tscope = map.get(&id);
            let classdef = scope.borrow().get_class_def(name);
            let namespec = unparse_type(tscope.clone(), Type::Object(name.clone(), types.clone()));
            let fullspec = if parent.is_some() {
                format!("{} extends {}", namespec, unparse_type(tscope.clone(), Type::make_object(parent.clone().unwrap())))
            } else {
                namespec.clone()
            };

            index.push_str(format!("class {} {{\n", fullspec).as_str());
            //index.push_str(format!("    decl __alloc__ : () -> {}\n", namespec).as_str());
            //index.push_str(format!("    decl __init__ : ({}) -> Nil\n", namespec).as_str());
            for node in body {
                match *node {
                    AST::Definition(_, (ref name, ref ttype), _) => {
                        //let stype = classdef.borrow().get_variable_type(name).unwrap();
                        //println!("WHICH ONE??? {:?} {:?}", ttype, stype);
                        //index.push_str(format!("    decl {} : {}\n", name, unparse_type(tscope.clone(), ttype.clone().unwrap())).as_str());
                        index.push_str(format!("    let {} : {}\n", name, unparse_type(tscope.clone(), ttype.clone().unwrap())).as_str());
                    },
                    AST::Function(_, ref name, _, _, _, _) => {
                        if let Some(ref name) = *name {
                            let ttype = classdef.borrow().get_variable_type(name).unwrap();
                            index.push_str(format!("    decl {} : {}\n", name, unparse_type(tscope.clone(), ttype)).as_str());
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

pub fn unparse_type<V, T>(scope: ScopeRef<V, T>, ttype: Type) -> String where V: Clone, T: Clone {
    match ttype {
        Type::Object(name, types) => {
            let params = if types.len() > 0 { format!("<{}>", types.iter().map(|p| unparse_type(scope.clone(), p.clone())).collect::<Vec<String>>().join(", ")) } else { String::from("") };
            name.clone() + &params
        },
        // TODO should you possibly create a non-conflicting name if required here
        Type::Variable(mut name, id) => {
            let var = scope.borrow().find_type(&name);
            if var.is_none() || var.unwrap().get_varid().unwrap_or(UniqueID(0)) != id {
                name = scope.borrow_mut().new_typevar_name();
                scope.borrow_mut().define_type(name.clone(), Type::Variable(name.clone(), id.clone())).unwrap();
            }
            // TODO i'd really like to use the name...
            //format!("'{}", name)
            format!("'v{}", id)
        }
        Type::Function(args, ret) => {
            let argstr: Vec<String> = args.iter().map(|t| unparse_type(scope.clone(), t.clone())).collect();
            format!("({}) -> {}", argstr.join(", "), unparse_type(scope.clone(), *ret))
        }
        Type::Overload(variants) => {
            let varstr: Vec<String> = variants.iter().map(|v| unparse_type(scope.clone(), v.clone())).collect();
            format!("Overload[{}]", varstr.join(", "))
        },
    }
}


