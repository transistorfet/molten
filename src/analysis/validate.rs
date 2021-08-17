
use std::collections::HashMap;

use crate::defs::Def;
use crate::misc::UniqueID;
use crate::types::{ Type, Check, check_type };
use crate::session::{ Session, Error };
use crate::scope::{ ScopeRef };
use crate::analysis::hir::{ Function, WhereClause, Expr, ExprKind };
use crate::analysis::visitor::{ self, Visitor, ScopeStack };


#[derive(Clone, Debug, PartialEq)]
pub struct Validator<'sess> {
    pub session: &'sess Session,
    pub stack: ScopeStack,
}


impl<'sess> Validator<'sess> {
    pub fn validate(session: &'sess Session, scope: ScopeRef, code: &Vec<Expr>) -> () {
        let mut validator = Validator {
            session: session,
            stack: ScopeStack::new(),
        };

        validator.stack.push_scope(scope);
        validator.visit_vec(code).unwrap();
        if session.errors.get() > 0 {
            panic!("Exiting due to previous errors");
        }
    }
}

impl<'sess> Visitor for Validator<'sess> {
    type Return = ();

    fn default_return(&self) -> () {
        ()
    }

    fn get_scope_stack<'a>(&'a self) -> &'a ScopeStack {
        &self.stack
    }

    fn get_session<'b>(&'b self) -> &'b Session {
        self.session
    }

    fn handle_error(&mut self, node: &Expr, err: Error) -> Result<Self::Return, Error> {
        self.session.print_error(&err.add_pos(node.get_pos()));
        Ok(self.default_return())
    }

    fn visit_vec(&mut self, code: &Vec<Expr>) -> Result<Self::Return, Error> {
        // Check that "raise" always appears as the last item in a block, or else there will be a placeholder unification error (this just makes it easier to debug)
        for i in 0..code.len() {
            match &code[i].kind {
                ExprKind::Raise(_) if i != code.len() - 1 => {
                    return Err(Error::new("RaiseError: found raise expression that is not the last node in a block".to_string()));
                },
                _ => { },
            }
        }
        visitor::walk_vec(self, code)
    }

    fn visit_function(&mut self, refid: UniqueID, func: &Function) -> Result<Self::Return, Error> {
        visitor::walk_function(self, refid, func)?;

        // Check that each overloaded variant of a function have distinct types from each other
        let defid = self.session.get_ref(refid)?;
        let ftype = self.session.get_type(defid).unwrap();
        if let Ok(Def::Overload(ol)) = self.session.get_def_from_ref(defid) {
            let (mut found, _) = ol.find_local_variants(self.session, ftype.get_argtypes()?);
            found = found.into_iter().filter(|(fid, _)| defid != *fid).collect();
            if found.len() > 0 {
                return Err(Error::new(format!("OverloadError: in definition of {:?}\nexisting variants found [{}]", func.name, found.iter().map(|(_, t)| format!("{}", t)).collect::<Vec<String>>().join(", "))));
            }
        }
        Ok(())
    }

    fn visit_class(&mut self, refid: UniqueID, _classtype: &Type, _parenttype: &Option<Type>, _whereclause: &WhereClause, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        let classdef = self.session.get_def_from_ref(refid)?.as_class()?;

        visitor::walk_class(self, refid, body)?;

        // Collect all local fields of this class definition
        let mut names = HashMap::new();
        for node in body.iter() {
            match &node.kind {
                ExprKind::Field(_, name, _) => {
                    let defid = self.session.get_ref(node.id)?;
                    names.insert(name, defid);
                },
                _ => { }
            }
        }

        // Check every function named "new" to make sure it initializes the class properly
        let mut has_new = false;
        let mut calls_super_new = false;
        for expr in body {
            match &expr.kind {
                ExprKind::Function(func) if func.name.as_str() == "new" => {
                    has_new = true;

                    for expr in &func.body {
                        // Check off any fields that are assigned to during the constructor
                        if let ExprKind::Assignment(left, _, _) = &expr.kind {
                            if let ExprKind::Accessor(left, field, _) = &left.kind {
                                if left.kind == ExprKind::Identifier("self".to_string()) {
                                    names.remove(field);
                                }
                            }
                        }

                        // Record if any calls to Super::new are made during the constructor
                        if let ExprKind::Invoke(fexpr, _, _) = &expr.kind {
                            if let ExprKind::Resolver(left, field, _) = &fexpr.kind {
                                if left.kind == ExprKind::Identifier("Super".to_string()) && field.as_str() == "new" {
                                    calls_super_new = true;
                                }
                            }
                        }
                    }
                },
                _ => { },
            }
        }

        // If any local field names are not checked off, then raise an error
        if names.len() > 0 {
            return Err(Error::new(format!("ClassError: constructor doesn't set the following fields: {:?}", names.keys().map(|s| s.as_str()).collect::<Vec<&str>>().join(", "))));
        }

        // If the constructor doesn't have a call to Super::new(), but the parent class does has a "new" method, then raise an error
        let parent_has_new = classdef.structdef.vars.parent.as_ref().map(|scope| scope.get_var_def("new")).unwrap_or(None);
        if has_new && !calls_super_new && parent_has_new.is_some() {
            return Err(Error::new("ClassError: constructor doesn't call Super::new()".to_string()));
        }

        Ok(())
    }

    fn visit_trait_impl(&mut self, refid: UniqueID, _traitname: &str, _impltype: &Type, _whereclause: &WhereClause, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        let impl_id = self.session.get_ref(refid)?;

        visitor::walk_trait_impl(self, refid, body)?;

        // Check the functions in the body and make sure all the functions defined in the trait def are defined with the same types, and no additional functions are present
        let traitdef = self.session.get_def(self.session.get_ref(impl_id)?)?.as_trait_def()?;

        let mut is_decl = false;
        let mut names = traitdef.vars.get_names();
        for node in body {
            match &node.kind {
                ExprKind::Function(func) => {
                    let name = &func.name;
                    let defid = *names.get(name).ok_or(Error::new(format!("TraitError: function not declared in the trait def, but found in the trait impl, {:?}", name)))?;
                    let deftype = self.session.get_type(defid);
                    let impltype = self.session.get_type(node.id);
                    check_type(self.session, deftype.as_ref(), impltype.as_ref(), Check::Def, false)?;
                    let traitfunc = self.session.get_def(self.session.get_ref(node.id).unwrap()).unwrap().as_trait_func().unwrap();
                    traitfunc.set_def_func_id(defid);
                    names.remove(name);
                }
                ExprKind::Declare(_, _, _, _) => {
                    is_decl = true;
                },
                _ => panic!("InternalError: expected function definition, found {:?}", node),
            }
        }

        if !is_decl && names.len() > 0 {
            return Err(Error::new(format!("TraitError: impl doesn't define the following: {:?}", names.keys().collect::<Vec<&String>>())));
        }

        Ok(())
    }

    fn visit_import(&mut self, _id: UniqueID, _ident: &str, _decls: &Vec<Expr>) -> Result<Self::Return, Error> {
        // Skip checking imports
        Ok(())
    }
}



