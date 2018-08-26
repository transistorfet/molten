
use std::fs::File;
use std::io::prelude::*;

use abi::ABI;
use types::Type;
use utils::UniqueID;
use config::Options;
use session::Session;
use ast::{ NodeID, AST };
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
        AST::Function(ref id, _, ref ident, _, _, _, _) => {
            if let Some(ref ident) = *ident {
                //let ttype = scope.get_variable_type(session, &ident.name).unwrap();
                //let name = get_mangled_name(session, scope.clone(), &ident.name, *id);
                let ttype = session.get_type(*id).unwrap();
                index.push_str(format!("decl {} : {}\n", ident.name.clone(), unparse_type(session, scope.clone(), ttype)).as_str());
            }
        },

        AST::Definition(_, _, _, _, ref body) => match **body {
            ref node @ AST::Class(_, _, _, _, _) => build_index_node(index, session, scope.clone(), node),
            _ => { },
        },

        AST::Class(ref id, _, ref classspec, ref parentspec, ref body) => {
            let tscope = session.map.get(&id);
            //let classdef = scope.find_type_def(session, &classspec.ident.name).unwrap().as_class().unwrap();
            let namespec = unparse_type(session, tscope.clone(), Type::from_spec(classspec.clone(), UniqueID(0)));
            let fullspec = if parentspec.is_some() {
                format!("{} extends {}", namespec, unparse_type(session, tscope.clone(), Type::from_spec(parentspec.clone().unwrap(), UniqueID(0))))
            } else {
                namespec.clone()
            };

            index.push_str(format!("class {} {{\n", fullspec).as_str());
            //index.push_str(format!("    decl __alloc__ : () -> {}\n", namespec).as_str());
            //index.push_str(format!("    decl __init__ : ({}) -> Nil\n", namespec).as_str());
            for node in body {
                match *node {
                    AST::Definition(ref id, _, ref ident, _, _) => {
                        let ttype = session.get_type(*id).unwrap();
                        //let stype = classdef.get_variable_type(session, name).unwrap();
                        //println!("WHICH ONE??? {:?} {:?}", ttype, stype);
                        //index.push_str(format!("    decl {} : {}\n", ident.name, unparse_type(session, tscope.clone(), ttype.clone().unwrap())).as_str());
                        index.push_str(format!("    let {} : {}\n", ident.name, unparse_type(session, tscope.clone(), ttype)).as_str());
                    },
                    AST::Function(ref id, _, ref ident, _, _, _, _) => {
                        if let Some(ref ident) = *ident {
                            //let ttype = classdef.classvars.get_variable_type(session, &ident.name).unwrap();
                            //let name = get_mangled_name(session, tscope.clone(), &ident.name, *id);
                            let ttype = session.get_type(*id).unwrap();
                            index.push_str(format!("    decl {} : {}\n", ident.name.clone(), unparse_type(session, tscope.clone(), ttype)).as_str());
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

pub fn unparse_type(session: &Session, scope: ScopeRef, ttype: Type) -> String {
    match ttype {
        Type::Object(name, _, types) => {
            let params = if types.len() > 0 { format!("<{}>", types.iter().map(|p| unparse_type(session, scope.clone(), p.clone())).collect::<Vec<String>>().join(", ")) } else { String::from("") };
            name.clone() + &params
        },
        Type::Variable(mut name, id) => {
            let var = scope.find_type(session, &name);
            if var.is_none() || var.unwrap().get_id().unwrap_or(UniqueID(0)) != id {
                let gscope = Scope::global(scope.clone());
                name = gscope.new_typevar_name();
                // TODO there is no def tied to the defid, but is it needed?
                gscope.define_type(name.clone(), Some(id)).unwrap();
                session.set_type(id, Type::Variable(name.clone(), id));
            }
            //format!("'v{}", id)
            format!("'{}", name)
        }
        Type::Tuple(types) => {
            let tuple: Vec<String> = types.iter().map(|t| unparse_type(session, scope.clone(), t.clone())).collect();
            format!("({})", tuple.join(", "))

        },
        Type::Function(args, ret, abi) => {
            format!("{} -> {}{}", unparse_type(session, scope.clone(), *args), unparse_type(session, scope.clone(), *ret), abi)
        }
        Type::Overload(variants) => {
            let varstr: Vec<String> = variants.iter().map(|v| unparse_type(session, scope.clone(), v.clone())).collect();
            format!("Overload[{}]", varstr.join(", "))
        },
    }
}


