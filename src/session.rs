
use std::str;
use std::fmt;
use std::fs::File;
use std::path::Path;
use std::cell::Cell;
use std::cell::RefCell;
use std::io::prelude::*;
use std::collections::HashMap;

use parser;
use types::Type;
use config::Options;
use refinery::Refinery;
use ast::{ Pos, AST };
use hir::{ NodeID };
use defs::{ Def };
use scope::{ ScopeMapRef };


#[derive(Clone, Debug, PartialEq)]
pub struct Session {
    pub files: RefCell<Vec<(String, String)>>,
    pub name: String,
    pub target: String,
    pub errors: Cell<u32>,
    pub map: ScopeMapRef,
    pub defs: RefCell<HashMap<NodeID, Def>>,
    pub refs: RefCell<HashMap<NodeID, NodeID>>,
    pub types: RefCell<HashMap<NodeID, Type>>,
}


impl Session {
    pub fn new() -> Self {
        Session {
            files: RefCell::new(vec!()),
            name: String::from(""),
            target: String::from(""),
            errors: Cell::new(0),
            map: ScopeMapRef::new(),
            defs: RefCell::new(HashMap::new()),
            refs: RefCell::new(HashMap::new()),
            types: RefCell::new(HashMap::new()),
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
        parser::parse_or_error(name, self.files.borrow().last().unwrap().1.as_str())
    }

    pub fn parse_file(&self, filename: &str, import: bool) -> Vec<AST> {
        if import && Options::as_ref().linkfile_only {
            self.files.borrow_mut().push((String::from(filename), String::from("")));
            vec!()
        } else {
            let mut f = Session::find_file(filename, import);
            let mut contents = String::new();
            f.read_to_string(&mut contents).expect("Error reading file contents");
            self.parse_string(filename, contents)
        }
    }

    pub fn write_link_file(&self) {
        let mut link_text = String::new();
        for (ref file, _) in self.files.borrow().iter() {
            let source = file.rsplitn(2, '.').collect::<Vec<&str>>()[1];
            link_text = link_text + source + "\n";
        }

        let mut link_file = File::create(format!("{}.l", self.target)).expect("Error creating link file");
        link_file.write_all(link_text.as_bytes()).unwrap();
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

    #[allow(dead_code)]
    pub fn raise_error(&self, pos: &Pos, msg: String) -> Error {
        let err = Error::new_pos(pos, msg);
        self.print_error(err.clone());
        err
    }

    pub fn new_def_id(&self, refid: NodeID) -> NodeID {
        let defid = NodeID::generate();
        self.set_ref(refid, defid);
        debug!("NEW DEF: {:?} created from ref {:?}", defid, refid);
        defid
    }

    pub fn set_def(&self, id: NodeID, def: Def) {
        debug!("SET DEF: {:?} = {:?}", id, def);
        //if let Some(existing) = self.defs.borrow().get(&id) {
        //    panic!("Definition {} already set to {:#?} but trying to set it to {:#?}", id, existing, def);
        //}
        self.defs.borrow_mut().insert(id, def);
    }

    pub fn get_def(&self, id: NodeID) -> Result<Def, Error> {
        match self.defs.borrow().get(&id) {
            Some(def) => Ok(def.clone()),
            None => Err(Error::new(format!("DefinitionError: definition not set for {:?}", id))),
        }
    }


    pub fn set_ref(&self, id: NodeID, defid: NodeID) {
        debug!("SET REF: {:?} -> {:?}", id, defid);
        //if let Some(existing) = self.refs.borrow().get(&id) {
        //    if defid != *existing {
        //        panic!("Ref {} already set to {} but trying to set it to {}", id, existing, defid);
        //    }
        //}
        self.refs.borrow_mut().insert(id, defid);
    }

    pub fn get_ref(&self, id: NodeID) -> Result<NodeID, Error> {
        match self.refs.borrow().get(&id) {
            Some(defid) => Ok(defid.clone()),
            None => Err(Error::new(format!("ReferenceError: reference not set for {:?}", id))),
        }
    }

    pub fn get_def_from_ref(&self, id: NodeID) -> Result<Def, Error> {
        match self.refs.borrow().get(&id) {
            Some(defid) => self.get_def(*defid),
            None => Err(Error::new(format!("ReferenceError: reference not set for {:?}", id))),
        }
    }

    pub fn get_type_from_ref(&self, id: NodeID) -> Result<Type, Error> {
        let defid = self.get_ref(id)?;
        self.get_type(defid).ok_or(Error::new(format!("DefinitionError: no type is set for {:?}", defid)))
    }


    pub fn set_type(&self, id: NodeID, ttype: Type) {
        debug!("SET TYPE: {:?} <= {:?} (previously {:?})", id, ttype, self.types.borrow().get(&id));
        self.types.borrow_mut().insert(id, ttype);
    }

    pub fn get_type(&self, id: NodeID) -> Option<Type> {
        self.types.borrow().get(&id).map(|ttype| ttype.clone())
    }

    pub fn new_typevar(&self) -> Type {
        let id = NodeID::generate();
        let ttype = Type::Variable(format!("{}", id), id, false);
        self.set_type(id, ttype.clone());
        debug!("NEW TYPEVAR: {:?}", ttype);
        ttype
    }

    #[must_use]
    pub fn update_type(&self, id: NodeID, ttype: Type) -> Result<(), Error> {
        use types;

        let etype = self.get_type(id);
        let ntype = match etype.clone() {
            // NOTE don't update a variable with itself or it will cause infinite recursion due to the update call in check_type
            Some(Type::Variable(_, eid, _)) if eid == id => ttype,
            etype @ Some(_) => {
                types::check_type(self, etype, Some(ttype.clone()), types::Check::Def, true)?
            },
            None => ttype,
        };
        debug!("Updating type id {:?} from {:?} -> {:?}", id, etype, ntype);
        self.set_type(id, ntype);
        Ok(())
    }

    pub fn resolve_types(&self) {
        use types;

        let keys: Vec<NodeID> = self.types.borrow().keys().map(|k| *k).collect();
        for key in keys {
            let ttype = self.get_type(key).unwrap();
            match types::resolve_type(self, ttype.clone(), true) {
                Ok(ntype) => {
                    debug!("Resolving type for id {:?} from {:?} to {:?}", key, ttype, ntype);
                    self.set_type(key, ntype);
                },
                Err(err) => self.print_error(err),
            }
        }
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

