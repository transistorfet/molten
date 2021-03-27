
use std::fs::File;
use std::io::prelude::*;

use types::Type;
use misc::UniqueID;
use config::Options;
use session::Session;
use scope::{ ScopeRef };
use hir::{ NodeID, Visibility, Mutability, Expr, ExprKind };


pub fn write_exports(session: &Session, scope: ScopeRef, filename: &str, code: &Vec<Expr>) {
    let declarations_text = build_declarations(session, scope, code);
    let mut declarations_file = File::create(filename).expect("Error creating declarations file");
    declarations_file.write_all(declarations_text.as_bytes()).unwrap();
    if Options::as_ref().debug {
        println!("{}", declarations_text);
    }
}

pub fn build_declarations(session: &Session, scope: ScopeRef, code: &Vec<Expr>) -> String {
    let mut declarations = String::new();
    for node in code {
        build_declarations_node(&mut declarations, session, scope.clone(), node);
    }
    declarations
}

fn build_declarations_node(declarations: &mut String, session: &Session, scope: ScopeRef, node: &Expr) {
    match &node.kind {
        ExprKind::Declare(vis, ident, _) => {
            declarations.push_str(&emit_declaration(session, scope.clone(), node.id, *vis, &ident.name));
        },

        ExprKind::Function(vis, ident, _, _, _, _) => {
            if let Some(ref ident) = *ident {
                declarations.push_str(&emit_declaration(session, scope.clone(), node.id, *vis, &ident.name));
            }
        },

        ExprKind::Definition(_, _, _, body) => match &body.kind {
            ExprKind::Class(_, _, _) => build_declarations_node(declarations, session, scope.clone(), node),
            _ => { },
        },

        ExprKind::Class(classspec, parentspec, body) => {
            let tscope = session.map.get(&node.id);
            let namespec = unparse_type(session, tscope.clone(), Type::from_spec(classspec.clone(), UniqueID(0)));
            let fullspec = if parentspec.is_some() {
                format!("{} extends {}", namespec, unparse_type(session, tscope.clone(), Type::from_spec(parentspec.clone().unwrap(), UniqueID(0))))
            } else {
                namespec.clone()
            };

            declarations.push_str(format!("class {} {{\n", fullspec).as_str());
            //declarations.push_str(format!("    decl __alloc__() -> {}\n", namespec).as_str());
            //declarations.push_str(format!("    decl __init__({}) -> Nil\n", namespec).as_str());
            for node in body {
                match &node.kind {
                    ExprKind::Definition(mutable, ident, _, _) => {
                        declarations.push_str("    ");
                        declarations.push_str(&emit_field(session, tscope.clone(), node.id, *mutable, &ident.name));
                    },
                    ExprKind::Declare(vis, ident, _) => {
                        declarations.push_str("    ");
                        declarations.push_str(&emit_declaration(session, tscope.clone(), node.id, *vis, &ident.name));
                    },
                    ExprKind::Function(vis, ident, _, _, _, _) => {
                        if let Some(ref ident) = *ident {
                            declarations.push_str("    ");
                            declarations.push_str(&emit_declaration(session, tscope.clone(), node.id, *vis, &ident.name));
                        }
                    },
                    _ => {  },
                }
            }
            declarations.push_str(format!("}}\n").as_str());
        },
        _ => { },
    }
}

fn emit_declaration(session: &Session, scope: ScopeRef, id: NodeID, vis: Visibility, name: &String) -> String {
    if vis == Visibility::Public {
        //let name = get_mangled_name(session, tscope.clone(), &ident.name, *id);
        let ttype = session.get_type(id).unwrap();
        format!("decl {}{}\n", name, unparse_type(session, scope.clone(), ttype))
    } else {
        String::from("")
    }
}

fn emit_field(session: &Session, scope: ScopeRef, id: NodeID, mutable: Mutability, name: &String) -> String {
    let ttype = session.get_type(id).unwrap();
    let mutable_str = if let Mutability::Mutable = mutable { "mut " } else { "" };
    format!("let {}{}: {}\n", mutable_str, name, unparse_type(session, scope.clone(), ttype))
}

pub fn unparse_type(session: &Session, scope: ScopeRef, ttype: Type) -> String {
    match ttype {
        Type::Object(name, _, types) => {
            let params = if types.len() > 0 { format!("<{}>", types.iter().map(|p| unparse_type(session, scope.clone(), p.clone())).collect::<Vec<String>>().join(", ")) } else { String::from("") };
            name.clone() + &params
        },
        Type::Variable(name, _id, _) => {
            /*
            // TODO this doesn't work because if a name isnt' found, then all tyyevars with that id are made independent
            let var = scope.find_type(session, &name);
            if var.is_none() || var.unwrap().get_id().unwrap_or(UniqueID(0)) != id {
                let gscope = Scope::global(scope.clone());
                name = gscope.new_typevar_name();
                // TODO there is no def tied to the defid, but is it needed?
                gscope.define_type(name.clone(), Some(id)).unwrap();
                session.set_type(id, Type::Variable(name.clone(), id));
            }
            */
            //format!("'v{}", id)
            format!("'{}", name)
        },
        Type::Tuple(types) => {
            let tuple: Vec<String> = types.iter().map(|t| unparse_type(session, scope.clone(), t.clone())).collect();
            format!("({})", tuple.join(", "))
        },
        Type::Record(types) => {
            let tuple: Vec<String> = types.iter().map(|(n, t)| format!("{}: {}", n, unparse_type(session, scope.clone(), t.clone()))).collect();
            format!("{{ {} }}", tuple.join(", "))
        },
        Type::Function(args, ret, abi) => {
            format!("{} -> {}{}", unparse_type(session, scope.clone(), *args), unparse_type(session, scope.clone(), *ret), abi)
        },
        Type::Ref(ttype) => {
            format!("ref {}", unparse_type(session, scope.clone(), *ttype))
        },
        Type::Ambiguous(variants) => {
            let varstr: Vec<String> = variants.iter().map(|v| unparse_type(session, scope.clone(), v.clone())).collect();
            format!("Ambiguous[{}]", varstr.join(", "))
        },
    }
}


