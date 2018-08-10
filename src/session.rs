
use std::str;
use std::fmt;
use std::fs::File;
use std::path::Path;
use std::cell::Cell;
use std::cell::RefCell;
use std::io::prelude::*;
use std::collections::HashMap;

use parser;
use refinery;
use types::Type;
use config::Options;
use ast::{ NodeID, Pos, AST };
use defs::{ VarDef, TypeDef };
use scope::{ Scope, ScopeRef, ScopeMapRef };


#[derive(Clone, Debug, PartialEq)]
pub struct Session {
    pub name: String,
    pub map: ScopeMapRef,
    // TODO I really want to unify these, but I figured I should wait and refactor later when it's known to actually work
    pub vardefs: RefCell<HashMap<NodeID, VarDef>>,
    pub typedefs: RefCell<HashMap<NodeID, TypeDef>>,
    pub files: RefCell<Vec<(String, String)>>,
    pub target: String,
    pub errors: Cell<u32>,
}


impl Session {
    pub fn new() -> Self {
        Session {
            name: String::from(""),
            //builtins: builtins,
            map: ScopeMapRef::new(),
            vardefs: RefCell::new(HashMap::new()),
            typedefs: RefCell::new(HashMap::new()),
            files: RefCell::new(vec!()),
            target: String::from(""),
            errors: Cell::new(0),
        }
    }

    pub fn find_file(filename: &str, search: bool) -> File {
        let current = vec!(".");
        let locations = if search { &Options::as_ref().libpath } else { &current };
        for ref path in locations {
            match File::open(Path::new(path).join(filename)) {
                Ok(f) => return f,
                Err(_) => { },
            }
        }
        panic!("Error: file not found, {}", filename);
    }

    pub fn parse_string(&self, name: &str, contents: String) -> Vec<AST> {
        self.files.borrow_mut().push((String::from(name), contents));
        let mut code = parser::parse_or_error(name, self.files.borrow().last().unwrap().1.as_bytes());
        code = refinery::refine(code);
        if Options::as_ref().debug {
            println!("\n{:?}\n", code);
        }
        code
    }

    pub fn parse_file(&self, filename: &str, search: bool) -> Vec<AST> {
        let mut f = Session::find_file(filename, search);
        let mut contents = String::new();
        f.read_to_string(&mut contents).expect("Error reading file contents");
        self.parse_string(filename, contents)
    }



    pub fn print_error(&self, err: Error) {
        self.errors.set(self.errors.get() + 1);
        let fborrow = self.files.borrow();
        if let Some(ref pos) = err.pos {
            let filename = &fborrow[pos.filenum as usize].0;
            let exerpt = pos.exerpt(fborrow[pos.filenum as usize].1.as_bytes());
            println!("\x1B[1;31m{}:{:?}: {}\n\tat {}\x1B[0m", filename, pos, err.msg, exerpt);
        } else {
            println!("\x1B[1;31m{}\n\x1B[0m", err.msg);
        }
    }

    pub fn raise_error(&self, pos: &Pos, msg: String) -> Error {
        let err = Error::new_pos(pos, msg);
        self.print_error(err.clone());
        err
    }

    pub fn get_def(&self, id: NodeID) -> Result<VarDef, Error> {
        match self.vardefs.borrow().get(&id) {
            Some(def) => Ok(def.clone()),
            None => Err(Error::new(format!("DefinitionError: var definition not set for {:?}", id))),
        }
    }

    pub fn set_def(&self, id: NodeID, def: VarDef) {
        self.vardefs.borrow_mut().insert(id, def);
    }

    pub fn define(&self, scope: ScopeRef, name: &str, id: NodeID) {
        let dscope = Scope::target(self, scope.clone());
        dscope.define(String::from(name), None);
        dscope.set_var_def(&String::from(name), id);
    }

    pub fn find_def(&self, scope: ScopeRef, name: &str) -> Result<VarDef, Error> {
        // TODO replace String arguments in scope methods
        self.get_def(scope.get_var_def(&String::from(name)).ok_or(Error::new(format!("VarError: definition not set for {:?}", name)))?)
    }



    pub fn set_type_def(&self, id: NodeID, def: TypeDef) {
        //debug!("------- {:?} {:#?}", id, def);
        self.typedefs.borrow_mut().insert(id, def);
    }

    pub fn get_type_def(&self, id: NodeID) -> Result<TypeDef, Error> {
        match self.typedefs.borrow().get(&id) {
            Some(def) => Ok(def.clone()),
            None => Err(Error::new(format!("DefinitionError: type definition not set for {:?}", id))),
        }
    }

    pub fn define_type(&self, scope: ScopeRef, name: &str, ttype: Type, id: NodeID) {
        let dscope = Scope::target(self, scope.clone());
        dscope.define_type(String::from(name), ttype);
        dscope.set_type_def(&String::from(name), id);
    }

    pub fn find_type_def(&self, scope: ScopeRef, name: &str) -> Result<TypeDef, Error> {
        // TODO replace String arguments in scope methods
        self.get_type_def(scope.get_type_def(&String::from(name)).ok_or(Error::new(format!("TypeError: definition not set for {:?}", name)))?)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Error {
    pub pos: Option<Pos>,
    pub msg: String,
}

impl Error {
    pub fn new(msg: String) -> Error {
        Error { pos: None, msg: msg }
    }

    pub fn new_pos(pos: &Pos, msg: String) -> Error {
        Error { pos: Some(pos.clone()), msg: msg }
    }

    pub fn add_pos(mut self, pos: &Pos) -> Error {
        self.pos = Some(pos.clone());
        self
    }

    /*
    pub fn pos<R>(res: Result<R, Error>, pos: &Pos) -> Result<R, Error> {
        res.map_err(|mut err| { err.pos = pos.clone(); err })
    }
    */
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\x1B[1;31m{:?}: {}\x1B[0m", self.pos.as_ref().unwrap_or(&Pos::empty()), self.msg)
    }
}

