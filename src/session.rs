
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
use defs::{ Def };
use scope::{ Scope, ScopeRef, ScopeMapRef };


#[derive(Clone, Debug, PartialEq)]
pub struct Session {
    pub name: String,
    pub files: RefCell<Vec<(String, String)>>,
    pub map: ScopeMapRef,
    pub defs: RefCell<HashMap<NodeID, Def>>,
    pub target: String,
    pub errors: Cell<u32>,
}


impl Session {
    pub fn new() -> Self {
        Session {
            name: String::from(""),
            files: RefCell::new(vec!()),
            map: ScopeMapRef::new(),
            defs: RefCell::new(HashMap::new()),
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

    pub fn get_def(&self, id: NodeID) -> Result<Def, Error> {
        match self.defs.borrow().get(&id) {
            Some(def) => Ok(def.clone()),
            None => Err(Error::new(format!("DefinitionError: definition not set for {:?}", id))),
        }
    }

    pub fn set_def(&self, id: NodeID, def: Def) {
        self.defs.borrow_mut().insert(id, def);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeTable(RefCell<HashMap<NodeID, Type>>);

impl TypeTable {
    pub fn new() -> Self {
        TypeTable(RefCell::new(HashMap::new()))
    }

    pub fn set(&self, id: NodeID, ttype: Type) {
        self.0.borrow_mut().insert(id, ttype);
    }

    pub fn get_def(&self, id: NodeID) -> Option<Type> {
        self.0.borrow().get(&id).map(|ttype| ttype.clone())
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

