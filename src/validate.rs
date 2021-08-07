
use crate::defs::Def;
use crate::session::{ Session, Error };
use crate::scope::{ ScopeRef };
use crate::hir::{ NodeID, Function, Expr };
use crate::visitor::{ self, Visitor, ScopeStack };


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


    fn visit_function(&mut self, refid: NodeID, func: &Function) -> Result<Self::Return, Error> {
        visitor::walk_function(self, refid, func)?;

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
}



