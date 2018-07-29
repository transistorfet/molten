
use std::fs::File;
use std::io::prelude::*;

use ast::AST;
use types::Type;
use utils::UniqueID;
use config::Options;
use session::Session;
use scope::{ Scope, ScopeRef, ScopeMapRef };


pub fn write_exports(session: &Session, scope: ScopeRef, filename: &str, code: &Vec<AST>) {
    let index_text = build_index(session, scope, code);
    let mut index_file = File::create(filename).expect("Error creating index file");
    index_file.write_all(index_text.as_bytes()).unwrap();
    if Options::as_ref().debug {
        println!("{}", index_text);
    }
}

pub fn build_index(session: &Session, scope: ScopeRef, code: &Vec<AST>) -> String {
    let mut index = String::new();
    for node in code {
        build_index_node(&mut index, session, scope.clone(), node);
    }
    index
}

fn build_index_node(index: &mut String, session: &Session, scope: ScopeRef, node: &AST) {
    match *node {
        AST::Function(_, ref name, _, _, _, _, _) => {
            if let Some(ref name) = *name {
                let ttype = scope.borrow().get_variable_type(name).unwrap();
                index.push_str(format!("decl {} : {}\n", name, unparse_type(scope.clone(), ttype)).as_str());
            }
        },

        AST::Definition(_, (_, _, _), ref body) => match **body {
            ref node @ AST::Class(_, _, _, _, _) => build_index_node(index, session, scope, node),
            _ => { },
        },

        AST::Class(_, (_, ref name, ref types), ref parent, ref body, ref id) => {
            let tscope = session.map.get(&id);
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
                    AST::Definition(_, (_, ref name, ref ttype), _) => {
                        //let stype = classdef.borrow().get_variable_type(name).unwrap();
                        //println!("WHICH ONE??? {:?} {:?}", ttype, stype);
                        //index.push_str(format!("    decl {} : {}\n", name, unparse_type(tscope.clone(), ttype.clone().unwrap())).as_str());
                        index.push_str(format!("    let {} : {}\n", name, unparse_type(tscope.clone(), ttype.clone().unwrap())).as_str());
                    },
                    AST::Function(_, ref name, _, _, _, _, _) => {
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

pub fn unparse_type(scope: ScopeRef, ttype: Type) -> String {
    match ttype {
        Type::Object(name, types) => {
            let params = if types.len() > 0 { format!("<{}>", types.iter().map(|p| unparse_type(scope.clone(), p.clone())).collect::<Vec<String>>().join(", ")) } else { String::from("") };
            name.clone() + &params
        },
        Type::Variable(mut name, id) => {
            let var = scope.borrow().find_type(&name);
            if var.is_none() || var.unwrap().get_varid().unwrap_or(UniqueID(0)) != id {
                let gscope = Scope::global(scope.clone());
                name = gscope.borrow_mut().new_typevar_name();
                gscope.borrow_mut().define_type(name.clone(), Type::Variable(name.clone(), id)).unwrap();
            }
            //format!("'v{}", id)
            format!("'{}", name)
        }
        Type::Function(args, ret, abi) => {
            let argstr: Vec<String> = args.iter().map(|t| unparse_type(scope.clone(), t.clone())).collect();
            format!("({}) -> {}{}", argstr.join(", "), unparse_type(scope.clone(), *ret), abi)
        }
        Type::Overload(variants) => {
            let varstr: Vec<String> = variants.iter().map(|v| unparse_type(scope.clone(), v.clone())).collect();
            format!("Overload[{}]", varstr.join(", "))
        },
    }
}


