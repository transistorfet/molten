
use std::str;
use std::fmt;
use std::rc::Rc;
use std::fs::File;
use std::path::Path;
use std::cell::Cell;
use std::cell::RefCell;
use std::io::prelude::*;

use refinery;
use config::Options;
use parser::{ self, AST, Pos };
use scope::{ ScopeMapRef };


#[derive(Clone, Debug, PartialEq)]
pub struct Session<V, T> {
    pub name: String,
    pub map: ScopeMapRef<V, T>,
    pub files: RefCell<Vec<(String, String)>>,
    pub errors: Cell<u32>,
}

//pub type SessionRef<V, T> = Rc<RefCell<Session<V, T>>>;

impl<V, T> Session<V, T> where V: Clone, T: Clone {
    pub fn new() -> Session<V, T> {
        Session {
            name: String::from(""),
            //builtins: builtins,
            map: ScopeMapRef::new(),
            files: RefCell::new(vec!()),
            errors: Cell::new(0),
        }
    }

    pub fn find_file(filename: &str) -> File {
        for ref path in &Options::as_ref().libpath {
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
        println!("\n{:?}\n", code);
        code
    }

    pub fn parse_file(&self, filename: &str) -> Vec<AST> {
        let mut f = Session::<V, T>::find_file(filename);
        let mut contents = String::new();
        f.read_to_string(&mut contents).expect("Error reading file contents");
        self.parse_string(filename, contents)
    }



    pub fn print_error(&self, err: Error) {
        self.errors.set(self.errors.get() + 1);
        let fborrow = self.files.borrow();
        let filename = &fborrow[err.pos.filenum as usize].0;
        //let exerpt = fborrow[err.pos.filenum as usize].1.as_bytes()[err.pos.offset];
        let exerpt = err.pos.exerpt(fborrow[err.pos.filenum as usize].1.as_bytes());
        println!("\x1B[1;31m{}:{:?}: {}\n        at {}\x1B[0m", filename, err.pos, err.msg, exerpt)
    }

    pub fn raise_error(&self, pos: &Pos, msg: String) -> Error {
        let err = Error::new_pos(pos, msg);
        self.print_error(err.clone());
        err
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Error {
    pub pos: Pos,
    pub msg: String,
}

impl Error {
    pub fn new(msg: String) -> Error {
        Error { pos: Pos::empty(), msg: msg }
    }

    pub fn new_pos(pos: &Pos, msg: String) -> Error {
        Error { pos: pos.clone(), msg: msg }
    }

    pub fn add_pos(mut self, pos: &Pos) -> Error {
        self.pos = pos.clone();
        self
    }

    pub fn pos<R>(res: Result<R, Error>, pos: &Pos) -> Result<R, Error> {
        res.map_err(|mut err| { err.pos = pos.clone(); err })
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\x1B[1;31m{:?}: {}\x1B[0m", self.pos, self.msg)
    }
}

