
use std::str;
use std::rc::Rc;
use std::fs::File;
use std::path::Path;
use std::cell::RefCell;
use std::io::prelude::*;

use config::Options;
use parser::{ self, AST };
use scope::{ ScopeMapRef };


#[derive(Clone, Debug, PartialEq)]
pub struct Session<V, T> {
    pub name: String,
    pub map: ScopeMapRef<V, T>,
    pub files: Vec<(String, String)>,
}

pub type SessionRef<V, T> = Rc<RefCell<Session<V, T>>>;

impl<V, T> Session<V, T> where V: Clone, T: Clone {
    pub fn new() -> Session<V, T> {
        //let builtins = lib_llvm::get_builtins();
        //let map = lib_llvm::make_global(&builtins);

        Session {
            name: String::from(""),
            //builtins: builtins,
            map: ScopeMapRef::new(),
            files: vec!(),
        }
    }

    pub fn new_ref() -> SessionRef<V, T> {
        Rc::new(RefCell::new(Session::new()))
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

    pub fn load_index(&mut self, filename: &str) -> Vec<AST> {
        let mut f = Session::<V, T>::find_file(filename);
        let mut contents = String::new();
        f.read_to_string(&mut contents).expect("Error reading file contents");
        self.files.push((String::from(filename), contents));

        let decl = parser::parse_or_error(filename, self.files[self.files.len() - 1].1.as_bytes());
        println!("DECL: {:?}", decl);
        decl
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Error {
    pub msg: String,
}

/*
impl Error {
    fn new(pos: Pos, name: &str, msg: String) -> Error {
        //Error { msg: format!("
    }
}
*/

