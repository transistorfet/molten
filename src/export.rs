
use std::fs::File;
use std::io::prelude::*;

use abi::ABI;
use types::Type;
use config::Options;
use scope::{ ScopeRef };
use session::{ Session, Error };
use visitor::{ Visitor, ScopeStack };
use hir::{ NodeID, Visibility, Mutability, ClassSpec, Argument, WhereClause, Expr, ExprKind };


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
        self.session.print_error(&err.add_pos(node.get_pos()));
        Ok(())
    }

    fn visit_import(&mut self, _id: NodeID, _ident: &str, _decls: &Vec<Expr>) -> Result<Self::Return, Error> {
        // Don't traverse imports
        Ok(())
    }

    fn visit_declare(&mut self, id: NodeID, vis: Visibility, name: &str, _ttype: &Type, whereclause: &WhereClause) -> Result<Self::Return, Error> {
        let defid = self.session.get_ref(id)?;
        self.declarations.push_str(&emit_declaration(self.session, defid, vis, name, &whereclause.constraints));
        Ok(())
    }

    fn visit_function(&mut self, id: NodeID, vis: Visibility, name: Option<&str>, _args: &Vec<Argument>, _rettype: &Option<Type>, _body: &Vec<Expr>, _abi: ABI, whereclause: &WhereClause) -> Result<Self::Return, Error> {
        if let Some(name) = name {
            let defid = self.session.get_ref(id)?;
            self.declarations.push_str(&emit_declaration(self.session, defid, vis, name, &whereclause.constraints));
        }
        // TODO we would walk the body here to search everything that's publically visable, but currently only top level functions are exported
        Ok(())
    }

    fn visit_class(&mut self, _id: NodeID, classspec: &ClassSpec, parentspec: &Option<ClassSpec>, whereclause: &WhereClause, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        let mut namespec = unparse_type(self.session, Type::from(classspec));
        if parentspec.is_some() {
            namespec = format!("{} extends {}", namespec, unparse_type(self.session, Type::from(parentspec.as_ref().unwrap())))
        }
        if whereclause.constraints.len() > 0 {
            namespec += &emit_where_clause(&whereclause.constraints);
        }

        self.declarations.push_str(format!("class {} {{\n", namespec).as_str());
        //self.declarations.push_str(format!("    decl __alloc__() -> {}\n", namespec).as_str());
        //self.declarations.push_str(format!("    decl __init__({}) -> Nil\n", namespec).as_str());
        for node in body {
            match &node.kind {
                ExprKind::Definition(mutable, name, _, _) => {
                    let defid = self.session.get_ref(node.id)?;
                    self.declarations.push_str("    ");
                    self.declarations.push_str(&emit_field(self.session, defid, *mutable, name));
                },
                ExprKind::Declare(vis, name, _, whereclause) => {
                    let defid = self.session.get_ref(node.id)?;
                    self.declarations.push_str("    ");
                    self.declarations.push_str(&emit_declaration(self.session, defid, *vis, name, &whereclause.constraints));
                },
                ExprKind::Function(vis, name, _, _, _, _, whereclause) => {
                    if let Some(name) = name {
                        let defid = self.session.get_ref(node.id)?;
                        self.declarations.push_str("    ");
                        self.declarations.push_str(&emit_declaration(self.session, defid, *vis, name, &whereclause.constraints));
                    }
                },
                _ => {  },
            }
        }
        self.declarations.push_str(format!("}}\n").as_str());
        Ok(())
    }

    fn visit_trait_def(&mut self, _id: NodeID, traitspec: &ClassSpec, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        let namespec = unparse_type(self.session, Type::from(traitspec));

        self.declarations.push_str(format!("trait {} {{\n", namespec).as_str());
        for node in body {
            match &node.kind {
                ExprKind::Declare(_, name, _, whereclause) => {
                    let defid = self.session.get_ref(node.id)?;
                    self.declarations.push_str("    ");
                    self.declarations.push_str(&emit_declaration(self.session, defid, Visibility::Public, name, &whereclause.constraints));
                },
                _ => {  },
            }
        }
        self.declarations.push_str(format!("}}\n").as_str());
        Ok(())
    }

    fn visit_trait_impl(&mut self, _id: NodeID, _traitspec: &ClassSpec, _impltype: &Type, _body: &Vec<Expr>) -> Result<Self::Return, Error> {
        // TODO we need something to cause the implementation vtable to be declared external when importing (so that a trait object of that type can be created external)
        Ok(())
    }
}

fn emit_declaration(session: &Session, id: NodeID, vis: Visibility, name: &str, constraints: &Vec<(String, String)>) -> String {
    if vis == Visibility::Public {
        //let name = get_mangled_name(session, &ident.name, *id);
        let ttype = session.get_type(id).unwrap();
        let wherestr = if constraints.len() > 0 {
            emit_where_clause(constraints)
        } else {
            "".to_string()
        };
        format!("decl {}{}{}\n", name, unparse_type(session, ttype), wherestr)
    } else {
        String::from("")
    }
}

fn emit_field(session: &Session, id: NodeID, mutable: Mutability, name: &str) -> String {
    let ttype = session.get_type(id).unwrap();
    let mutable_str = if let Mutability::Mutable = mutable { "mut " } else { "" };
    format!("let {}{}: {}\n", mutable_str, name, unparse_type(session, ttype))
}

fn emit_where_clause(constraints: &Vec<(String, String)>) -> String {
    format!(" where {}", constraints.iter().map(|(var, cons)| format!("{}: {}", var, cons)).collect::<Vec<String>>().join(", "))
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
    }
}


