
use std::fs::File;
use std::io::prelude::*;

use crate::types::Type;
use crate::misc::UniqueID;
use crate::config::Options;
use crate::scope::ScopeRef;
use crate::session::{ Session, Error };
use crate::analysis::visitor::{ Visitor, ScopeStack };
use crate::analysis::hir::{ Visibility, Mutability, EnumVariant, WhereClause, Function, Expr, ExprKind };


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
            session,
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

    fn get_scope_stack(&self) -> &ScopeStack {
        &self.stack
    }

    fn get_session(&self) -> &Session {
        self.session
    }

    fn handle_error(&mut self, node: &Expr, err: Error) -> Result<(), Error> {
        self.session.print_error(&err.add_pos(node.get_pos()));
        Ok(())
    }

    fn visit_module(&mut self, _refid: UniqueID, name: &str, code: &Expr, _memo_id: UniqueID) -> Result<Self::Return, Error> {
        self.declarations.push_str(&format!("module {}\n", name));
        match &code.kind {
            ExprKind::Function(func) => {
                self.visit_vec(&func.body)
            },
            _ => panic!("SyntaxError: expected module to contain one function, found {:?}", code),
        }
    }

    fn visit_import(&mut self, _refid: UniqueID, _ident: &str, _decls: &Vec<Expr>) -> Result<Self::Return, Error> {
        // Don't traverse imports
        Ok(())
    }

    fn visit_declare(&mut self, refid: UniqueID, vis: Visibility, name: &str, _ttype: &Type, whereclause: &WhereClause) -> Result<Self::Return, Error> {
        let defid = self.session.get_ref(refid)?;
        self.declarations.push_str(&emit_declaration(self.session, defid, vis, name, &whereclause.constraints));
        Ok(())
    }

    fn visit_function(&mut self, refid: UniqueID, func: &Function) -> Result<Self::Return, Error> {
        let defid = self.session.get_ref(refid)?;
        self.declarations.push_str(&emit_declaration(self.session, defid, func.vis, &func.name, &func.whereclause.constraints));
        // TODO we would walk the body here to search everything that's publically visable, but currently only top level functions are exported
        Ok(())
    }

    fn visit_class(&mut self, _refid: UniqueID, classtype: &Type, parenttype: &Option<Type>, whereclause: &WhereClause, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        let mut namespec = unparse_type(self.session, classtype);
        if parenttype.is_some() {
            namespec = format!("{} extends {}", namespec, unparse_type(self.session, parenttype.as_ref().unwrap()))
        }
        if whereclause.constraints.len() > 0 {
            namespec += &emit_where_clause(&whereclause.constraints);
        }

        self.declarations.push_str(&format!("class {} {{\n", namespec));
        for node in body {
            match &node.kind {
                ExprKind::Field(mutable, name, _) => {
                    let defid = self.session.get_ref(node.id)?;
                    self.declarations.push_str("    ");
                    self.declarations.push_str(&emit_field(self.session, defid, *mutable, name));
                },
                ExprKind::Declare(vis, name, _, whereclause) => {
                    let defid = self.session.get_ref(node.id)?;
                    self.declarations.push_str("    ");
                    self.declarations.push_str(&emit_declaration(self.session, defid, *vis, name, &whereclause.constraints));
                },
                ExprKind::Function(func) => {
                    let defid = self.session.get_ref(node.id)?;
                    self.declarations.push_str("    ");
                    self.declarations.push_str(&emit_declaration(self.session, defid, func.vis, &func.name, &func.whereclause.constraints));
                },
                _ => {  },
            }
        }
        self.declarations.push_str(&format!("}}\n"));
        Ok(())
    }


    fn visit_methods(&mut self, _refid: UniqueID, ttype: &Type, whereclause: &WhereClause, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        let mut namespec = unparse_type(self.session, ttype);
        if whereclause.constraints.len() > 0 {
            namespec += &emit_where_clause(&whereclause.constraints);
        }

        self.declarations.push_str(&format!("methods {} {{\n", namespec));
        for node in body {
            match &node.kind {
                ExprKind::Declare(vis, name, _, whereclause) => {
                    let defid = self.session.get_ref(node.id)?;
                    self.declarations.push_str("    ");
                    self.declarations.push_str(&emit_declaration(self.session, defid, *vis, name, &whereclause.constraints));
                },
                ExprKind::Function(func) => {
                    let defid = self.session.get_ref(node.id)?;
                    self.declarations.push_str("    ");
                    self.declarations.push_str(&emit_declaration(self.session, defid, func.vis, &func.name, &func.whereclause.constraints));
                },
                _ => {  },
            }
        }
        self.declarations.push_str(&format!("}}\n"));
        Ok(())
    }

    fn visit_enum(&mut self, _refid: UniqueID, enumtype: &Type, whereclause: &WhereClause, variants: &Vec<EnumVariant>) -> Result<Self::Return, Error> {
        let mut namespec = unparse_type(self.session, enumtype);
        if whereclause.constraints.len() > 0 {
            namespec += &emit_where_clause(&whereclause.constraints);
        }

        self.declarations.push_str(&format!("enum {} =\n", namespec));
        for variant in variants {
            let typename = match &variant.ttype {
                Some(t) => format!("({})", t.as_vec().iter().map(|t| unparse_type(self.session, t)).collect::<Vec<String>>().join(",")),
                None => "".to_string(),
            };
            self.declarations.push_str(&format!("| {}{}\n", variant.name, typename));
        }
        Ok(())
    }

    fn visit_trait_def(&mut self, _refid: UniqueID, traitname: &str, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        self.declarations.push_str(&format!("trait {} {{\n", traitname));
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
        self.declarations.push_str(&format!("}}\n"));
        Ok(())
    }

    fn visit_trait_impl(&mut self, _refid: UniqueID, traitname: &str, impltype: &Type, whereclause: &WhereClause, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        let mut namespec = unparse_type(self.session, impltype);
        if whereclause.constraints.len() > 0 {
            namespec += &emit_where_clause(&whereclause.constraints);
        }

        self.declarations.push_str(&format!("impl {} for {} {{\n", traitname, namespec));
        for node in body {
            match &node.kind {
                ExprKind::Function(func) => {
                    let defid = self.session.get_ref(node.id)?;
                    self.declarations.push_str("    ");
                    self.declarations.push_str(&emit_declaration(self.session, defid, Visibility::Public, &func.name, &func.whereclause.constraints));
                },
                _ => {  },
            }
        }
        self.declarations.push_str(&format!("}}\n"));
        Ok(())
    }
}

fn emit_declaration(session: &Session, defid: UniqueID, vis: Visibility, name: &str, constraints: &Vec<(String, String)>) -> String {
    if vis == Visibility::Public {
        //let name = get_mangled_name(session, &ident.name, *id);
        let ttype = session.get_type(defid).unwrap();
        let wherestr = if constraints.len() > 0 {
            emit_where_clause(constraints)
        } else {
            "".to_string()
        };
        format!("decl {}{}{}\n", name, unparse_type(session, &ttype), wherestr)
    } else {
        String::from("")
    }
}

fn emit_field(session: &Session, defid: UniqueID, mutable: Mutability, name: &str) -> String {
    let ttype = session.get_type(defid).unwrap();
    let mutable_str = if let Mutability::Mutable = mutable { "mut " } else { "" };
    format!("val {}{}: {}\n", mutable_str, name, unparse_type(session, &ttype))
}

fn emit_where_clause(constraints: &Vec<(String, String)>) -> String {
    format!(" where {}", constraints.iter().map(|(var, cons)| format!("{}: {}", var, cons)).collect::<Vec<String>>().join(", "))
}

pub fn unparse_type(session: &Session, ttype: &Type) -> String {
    match ttype {
        Type::Object(name, _, types) => {
            let params = if types.len() > 0 { format!("<{}>", types.iter().map(|p| unparse_type(session, p)).collect::<Vec<String>>().join(", ")) } else { String::from("") };
            name.clone() + &params
        },
        Type::Variable(id) => {
            format!("?{}", id)
        },
        Type::Universal(name, _id) => {
            format!("'{}", name)
        },
        Type::Tuple(types) => {
            let tuple: Vec<String> = types.iter().map(|t| unparse_type(session, t)).collect();
            format!("({})", tuple.join(", "))
        },
        Type::Record(types) => {
            let tuple: Vec<String> = types.iter().map(|(n, t)| format!("{}: {}", n, unparse_type(session, t))).collect();
            format!("{{ {} }}", tuple.join(", "))
        },
        Type::Function(args, ret, abi) => {
            format!("{} -> {}{}", unparse_type(session, args), unparse_type(session, ret), abi)
        },
        Type::Ref(ttype) => {
            format!("ref {}", unparse_type(session, ttype))
        },
    }
}


