
use std::fs::File;
use std::io::prelude::*;

use abi::ABI;
use types::Type;
use misc::UniqueID;
use config::Options;
use scope::{ ScopeRef };
use session::{ Session, Error };
use visitor::{ Visitor, ScopeStack };
use hir::{ NodeID, Visibility, Mutability, Ident, ClassSpec, Argument, Expr, ExprKind };


pub fn write_exports(session: &Session, scope: ScopeRef, filename: &str, code: &Vec<Expr>) {
    let mut collector = ExportsCollector::new(session, scope);
    collector.visit(code).unwrap();
    let mut declarations_file = File::create(filename).expect("Error creating declarations file");
    declarations_file.write_all(collector.declarations.as_bytes()).unwrap();
    if Options::as_ref().debug {
        println!("{}", collector.declarations);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExportsCollector<'sess> {
    pub session: &'sess Session,
    pub stack: ScopeStack,
    pub declarations: String,
}

impl<'sess> ExportsCollector<'sess> {
    pub fn new(session: &'sess Session, scope: ScopeRef) -> Self {
        let collector = ExportsCollector {
            session: session,
            stack: ScopeStack::new(),
            declarations: String::new(),
        };

        collector.stack.push_scope(scope);
        collector
    }
}

impl<'sess> Visitor for ExportsCollector<'sess> {
    type Return = ();

    fn default_return(&self) -> () {
        ()
    }

    fn get_scope_stack<'a>(&'a self) -> &'a ScopeStack {
        &self.stack
    }

    fn get_scope_by_id(&self, id: NodeID) -> ScopeRef {
        self.session.map.get(&id)
    }

    fn handle_error(&mut self, node: &Expr, err: Error) -> Result<(), Error> {
        self.session.print_error(err.add_pos(&node.get_pos()));
        Ok(())
    }

    fn visit_import(&mut self, _id: NodeID, _ident: &Ident, _decls: &Vec<Expr>) -> Result<Self::Return, Error> {
        // Don't traverse imports
        Ok(())
    }

    fn visit_declare(&mut self, id: NodeID, vis: Visibility, ident: &Ident, _ttype: &Type) -> Result<Self::Return, Error> {
        let defid = self.session.get_ref(id)?;
        self.declarations.push_str(&emit_declaration(self.session, defid, vis, &ident.name));
        Ok(())
    }

    fn visit_function(&mut self, id: NodeID, vis: Visibility, ident: &Option<Ident>, _args: &Vec<Argument>, _rettype: &Option<Type>, _body: &Expr, _abi: ABI) -> Result<Self::Return, Error> {
        if let Some(ref ident) = *ident {
            let defid = self.session.get_ref(id)?;
            self.declarations.push_str(&emit_declaration(self.session, defid, vis, &ident.name));
        }
        // TODO we would walk the body here to search everything that's publically visable, but currently only top level functions are exported
        Ok(())
    }

    fn visit_class(&mut self, _id: NodeID, classspec: &ClassSpec, parentspec: &Option<ClassSpec>, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        let namespec = unparse_type(self.session, Type::from_spec(classspec.clone(), UniqueID(0)));
        let fullspec = if parentspec.is_some() {
            format!("{} extends {}", namespec, unparse_type(self.session, Type::from_spec(parentspec.clone().unwrap(), UniqueID(0))))
        } else {
            namespec.clone()
        };

        self.declarations.push_str(format!("class {} {{\n", fullspec).as_str());
        //self.declarations.push_str(format!("    decl __alloc__() -> {}\n", namespec).as_str());
        //self.declarations.push_str(format!("    decl __init__({}) -> Nil\n", namespec).as_str());
        for node in body {
            match &node.kind {
                ExprKind::Definition(mutable, ident, _, _) => {
                    let defid = self.session.get_ref(node.id)?;
                    self.declarations.push_str("    ");
                    self.declarations.push_str(&emit_field(self.session, defid, *mutable, &ident.name));
                },
                ExprKind::Declare(vis, ident, _) => {
                    let defid = self.session.get_ref(node.id)?;
                    self.declarations.push_str("    ");
                    self.declarations.push_str(&emit_declaration(self.session, defid, *vis, &ident.name));
                },
                ExprKind::Function(vis, ident, _, _, _, _) => {
                    if let Some(ref ident) = *ident {
                        let defid = self.session.get_ref(node.id)?;
                        self.declarations.push_str("    ");
                        self.declarations.push_str(&emit_declaration(self.session, defid, *vis, &ident.name));
                    }
                },
                _ => {  },
            }
        }
        self.declarations.push_str(format!("}}\n").as_str());
        Ok(())
    }

}

fn emit_declaration(session: &Session, id: NodeID, vis: Visibility, name: &String) -> String {
    if vis == Visibility::Public {
        //let name = get_mangled_name(session, &ident.name, *id);
        let ttype = session.get_type(id).unwrap();
        format!("decl {}{}\n", name, unparse_type(session, ttype))
    } else {
        String::from("")
    }
}

fn emit_field(session: &Session, id: NodeID, mutable: Mutability, name: &String) -> String {
    let ttype = session.get_type(id).unwrap();
    let mutable_str = if let Mutability::Mutable = mutable { "mut " } else { "" };
    format!("let {}{}: {}\n", mutable_str, name, unparse_type(session, ttype))
}

pub fn unparse_type(session: &Session, ttype: Type) -> String {
    match ttype {
        Type::Object(name, _, types) => {
            let params = if types.len() > 0 { format!("<{}>", types.iter().map(|p| unparse_type(session, p.clone())).collect::<Vec<String>>().join(", ")) } else { String::from("") };
            name.clone() + &params
        },
        Type::Variable(id) => {
            format!("?{}", id)
        },
        Type::Universal(name, _id) => {
            format!("'{}", name)
        },
        Type::Tuple(types) => {
            let tuple: Vec<String> = types.iter().map(|t| unparse_type(session, t.clone())).collect();
            format!("({})", tuple.join(", "))
        },
        Type::Record(types) => {
            let tuple: Vec<String> = types.iter().map(|(n, t)| format!("{}: {}", n, unparse_type(session, t.clone()))).collect();
            format!("{{ {} }}", tuple.join(", "))
        },
        Type::Function(args, ret, abi) => {
            format!("{} -> {}{}", unparse_type(session, *args), unparse_type(session, *ret), abi)
        },
        Type::Ref(ttype) => {
            format!("ref {}", unparse_type(session, *ttype))
        },
        Type::Ambiguous(variants) => {
            let varstr: Vec<String> = variants.iter().map(|v| unparse_type(session, v.clone())).collect();
            format!("Ambiguous[{}]", varstr.join(", "))
        },
    }
}


