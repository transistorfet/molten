
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

/*
pub fn load_index_vec<V, T>(scope: ScopeRef<V, T>, code: &Vec<Decl>) where V: Clone + Debug, T: Clone + Debug {
    for node in code {
        load_index_node(scope.clone(), node);
    }
}

fn load_index_node<V, T>(scope: ScopeRef<V, T>, node: &Decl) where V: Clone + Debug, T: Clone + Debug {
    match *node {
        Decl::Symbol(ref name, ref ttype) => {
            let dscope = Scope::target(scope.clone());
            dscope.borrow_mut().define_func(name.clone(), Some(ttype.clone()), true);
            if let Some(ref name) = unmangle_name(name) {
                println!("OVERLOAD: {:?}", name);
                dscope.borrow_mut().define_func(name.clone(), None, true);
                let mut stype = dscope.borrow().get_variable_type(name).unwrap_or(Type::Overload(vec!()));
                stype = stype.add_variant(scope.clone(), ttype.clone());
                dscope.borrow_mut().update_variable_type(name, stype);
            }
        },
        Decl::Class(ref pair, ref parent, ref body) => {
            let classdef = scope.borrow_mut().create_class_def(pair, parent.clone());
            let tscope = Scope::new_ref(Some(scope.clone()));
            tscope.borrow_mut().set_class(true);
            tscope.borrow_mut().set_basename(pair.0.clone());
            //pair.1.iter().map(|ttype| check_for_typevars(tscope.clone(), &Some(ttype.clone()))).count();
            //if let &Some((ref pname, ref ptypes)) = parent {
            //    ptypes.iter().map(|ttype| check_for_typevars(tscope.clone(), &Some(ttype.clone()))).count();
            //}
            load_index_vec(tscope.clone(), body);
        }
    }
}
*/

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
        //Type::List(stype) => format!("List[{}]", unparse_type(*stype)),
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


