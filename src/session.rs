
use std::str;
use std::fmt;
use std::fs::File;
use std::path::Path;
use std::cell::Cell;
use std::cell::RefCell;
use std::io::prelude::*;

use parser;
use refinery;
use config::Options;
use ast::{ AST, Pos };
use scope::{ ScopeMapRef };


#[derive(Clone, Debug, PartialEq)]
pub struct Session<'sess> {
    pub name: String,
    pub map: ScopeMapRef<'sess>,
    pub files: RefCell<Vec<(String, String)>>,
    pub target: String,
    pub errors: Cell<u32>,
}


impl<'sess> Session<'sess> {
    pub fn new() -> Self {
        Session {
            name: String::from(""),
            //builtins: builtins,
            map: ScopeMapRef::new(),
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

