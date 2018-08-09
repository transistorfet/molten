
use std::fs::File;
use std::io::prelude::*;

use ast::AST;
use types::Type;
use utils::UniqueID;
use config::Options;
use session::Session;
use scope::{ Scope, ScopeRef };


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
        AST::Function(_, _, ref ident, _, _, _, _) => {
            if let Some(ref ident) = *ident {
                let ttype = scope.get_variable_type(&ident.name).unwrap();
                index.push_str(format!("decl {} : {}\n", ident.name, unparse_type(scope.clone(), ttype)).as_str());
            }
        },

        AST::Definition(_, _, _, _, ref body) => match **body {
            ref node @ AST::Class(_, _, _, _, _) => build_index_node(index, session, scope.clone(), node),
            _ => { },
        },

        AST::Class(ref id, _, ref classspec, ref parentspec, ref body) => {
            let tscope = session.map.get(&id);
            let classdef = scope.get_type_def(&classspec.ident.name).as_class().unwrap();
            let namespec = unparse_type(tscope.clone(), Type::from_spec(classspec.clone()));
            let fullspec = if parentspec.is_some() {
                format!("{} extends {}", namespec, unparse_type(tscope.clone(), Type::from_spec(parentspec.clone().unwrap())))
            } else {
                namespec.clone()
            };

            index.push_str(format!("class {} {{\n", fullspec).as_str());
            //index.push_str(format!("    decl __alloc__ : () -> {}\n", namespec).as_str());
            //index.push_str(format!("    decl __init__ : ({}) -> Nil\n", namespec).as_str());
            for node in body {
                match *node {
                    AST::Definition(_, _, ref ident, ref ttype, _) => {
                        //let stype = classdef.get_variable_type(name).unwrap();
                        //println!("WHICH ONE??? {:?} {:?}", ttype, stype);
                        //index.push_str(format!("    decl {} : {}\n", ident.name, unparse_type(tscope.clone(), ttype.clone().unwrap())).as_str());
                        index.push_str(format!("    let {} : {}\n", ident.name, unparse_type(tscope.clone(), ttype.clone().unwrap())).as_str());
                    },
                    AST::Function(_, _, ref ident, _, _, _, _) => {
                        if let Some(ref ident) = *ident {
                            let ttype = classdef.classvars.get_variable_type(&ident.name).unwrap();
                            index.push_str(format!("    decl {} : {}\n", ident.name, unparse_type(tscope.clone(), ttype)).as_str());
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
            let var = scope.find_type(&name);
            if var.is_none() || var.unwrap().get_varid().unwrap_or(UniqueID(0)) != id {
                let gscope = Scope::global(scope.clone());
                name = gscope.new_typevar_name();
                gscope.define_type(name.clone(), Type::Variable(name.clone(), id)).unwrap();
            }
            //format!("'v{}", id)
            format!("'{}", name)
        }
        Type::Tuple(types) => {
            let tuple: Vec<String> = types.iter().map(|t| unparse_type(scope.clone(), t.clone())).collect();
            format!("({})", tuple.join(", "))

        },
        Type::Function(args, ret, abi) => {
            format!("{} -> {}{}", unparse_type(scope.clone(), *args), unparse_type(scope.clone(), *ret), abi)
        }
        Type::Overload(variants) => {
            let varstr: Vec<String> = variants.iter().map(|v| unparse_type(scope.clone(), v.clone())).collect();
            format!("Overload[{}]", varstr.join(", "))
        },
    }
}


