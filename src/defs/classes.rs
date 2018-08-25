
use std::rc::Rc;
use std::cell::RefCell;
 
use types::Type;
use defs::Def;
use ast::{ NodeID, Ident, ClassSpec, AST };
use scope::{ Scope, ScopeRef };
use session::{ Session, Error };


pub type ClassVars = ScopeRef;

#[derive(Clone, Debug, PartialEq)]
pub struct ClassDef {
    pub classname: String,
    pub classtype: Type,
    pub parenttype: Option<Type>,
    pub classvars: ClassVars,
    pub structdef: RefCell<Vec<(String, Type)>>,
    pub vtable: RefCell<Vtable>,
}

pub type ClassDefRef = Rc<ClassDef>;


impl ClassDef {
    pub fn new(classname: String, classtype: Type, parenttype: Option<Type>, classvars: ClassVars) -> Self {
        Self {
            classname: classname,
            classtype: classtype,
            parenttype: parenttype,
            classvars: classvars,
            structdef: RefCell::new(vec!()),
            vtable: RefCell::new(Vtable::new()),
        }
    }

    pub fn new_ref(classname: String, classtype: Type, parenttype: Option<Type>, classvars: ClassVars) -> ClassDefRef {
        Rc::new(Self::new(classname, classtype, parenttype, classvars))
    }

    pub fn create_class_scope(session: &Session, scope: ScopeRef, id: NodeID) -> ScopeRef {
        // Create a temporary invisible scope to name check the class body
        let tscope = session.map.add(id, Some(scope.clone()));
        tscope
    }

    pub fn define_class(session: &Session, scope: ScopeRef, id: NodeID, classtype: Type, parenttype: Option<Type>) -> Result<ClassDefRef, Error> {
        let name = classtype.get_name()?;
        let tscope = session.map.get(&id);
        tscope.set_redirect(true);
        tscope.set_basename(name.clone());

        // Define Self and Super, and check for typevars in the type params
        tscope.define_type(String::from("Self"), Some(id))?;
        if let Some(ref ptype) = parenttype {
            tscope.define_type(String::from("Super"), scope.get_type_def(&ptype.get_name()?))?;
        }

        let classdef = Self::create_class(session, scope.clone(), id, classtype.clone(), parenttype)?;

        // Define the class in the local scope
        scope.define_type(name.clone(), Some(id))?;
        session.set_def(id, Def::Class(classdef.clone()));
        session.set_type(id, classtype);
        // TODO i don't like this type == Class thing, but i don't know how i'll do struct types yet either
        //scope.define(name.clone(), Some(Type::Object(name.clone(), vec!())))?;

        Ok(classdef)
    }

    pub fn create_class(session: &Session, scope: ScopeRef, id: NodeID, classtype: Type, parenttype: Option<Type>) -> Result<ClassDefRef, Error> {
        // Find the parent class definitions, which the new class will inherit from
        let parentclass = match parenttype {
            Some(Type::Object(ref pname, _)) => Some(scope.find_type_def(session, &pname)?.as_class()?),
            _ => None
        };

        // Create class name bindings for checking ast::accessors
        let classvars = Scope::new_ref(parentclass.map(|p| p.classvars.clone()));
        classvars.set_basename(classtype.get_name()?);

        let classdef = ClassDef::new_ref(classtype.get_name()?, classtype, parenttype, classvars);
        Ok(classdef)
    }


    pub fn build_vtable(&self, session: &Session, scope: ScopeRef, body: &Vec<AST>) {
        let parentclass = match self.parenttype {
            // TODO this needs to be fixed
            Some(ref ptype) => Some(scope.find_type_def(session, &ptype.get_name().unwrap()).unwrap().as_class().unwrap()),
            None => None,
        };

        if let Some(parentclass) = parentclass {
            self.vtable.borrow_mut().inherit(&*parentclass.vtable.borrow());
        }
        self.vtable.borrow_mut().build_vtable(session, scope, body);

/*
        let mut vtable = self.vtable.borrow_mut();
        //let mut vtable = parentclass.map_or(vec!(), |c| c.vtable.borrow().clone());
        *vtable = parentclass.map_or(vec!(), |c| c.vtable.borrow().clone());

        for ref node in body.iter() {
            match **node {
                AST::Function(ref id, _, ref fident, ref args, ref rtype, _, ref abi) => {
                    if let Some(Ident { ref name, .. }) = fident {
                        //let uname = abi.unmangle_name(&name).unwrap_or(fname.clone());
                        //if parent.contains(&uname) {
                        debug!("***************: {:?}:{:?}", self.classname, name);
                        //if let Some(index) = vtable.iter().position(|ref r| r.1.as_str() == name.as_str()) {
                        if let Some(index) = self.get_vtable_index(name.as_str()) {
                            vtable[index].0 = *id;
                        } else {
                            vtable.push((*id, name.clone(), session.get_type(*id).unwrap()));
                        }
                    }
                },
                AST::Declare(ref id, _, ref fident, _) => {
                    let ttype = session.get_type(*id).unwrap();
                    match ttype {
                        Type::Function(_, _, _) => {
                            let name = &fident.name;

                            debug!("***************: {:?}:{:?}", self.classname, name);
                            if let Some(index) = vtable.iter().position(|ref r| r.1.as_str() == name.as_str()) {
                                vtable[index].0 = *id;
                            } else {
                                vtable.push((*id, name.clone(), ttype));
                            }
                        },
                        _ => { },
                    }
                },
                _ => { }
            }
        }

        // *self.vtable.borrow_mut() = vtable;
*/
    }

    pub fn build_structdef(&self, session: &Session, scope: ScopeRef, body: &Vec<AST>) {
        let parentclass = match self.parenttype {
            Some(ref ptype) => Some(scope.find_type_def(session, &ptype.get_name().unwrap()).unwrap().as_class().unwrap()),
            None => None,
        };
        let mut structdef = parentclass.map_or(vec!(), |c| c.structdef.borrow().clone());

        if self.has_vtable() {
            if let Some(index) = structdef.iter().position(|ref r| r.0.as_str() == "__vtable__") {
                structdef[index].1 = Type::Object(format!("{}_vtable", self.classname), vec!());
            } else {
                structdef.push((String::from("__vtable__"), Type::Object(format!("{}_vtable", self.classname), vec!())));
            }
        }
        for ref node in body.iter() {
            match **node {
                AST::Definition(_, _, ref ident, ref ttype, ref value) => {
                    structdef.push((ident.name.clone(), ttype.clone().unwrap()));
                },
                _ => { }
            }
        }

        *self.structdef.borrow_mut() = structdef;
    }

    pub fn get_struct_index(&self, field: &str) -> Option<usize> {
        self.structdef.borrow().iter().position(|ref r| r.0.as_str() == field)
    }

    pub fn get_struct_vtable_index(&self) -> Option<usize> {
        self.get_struct_index("__vtable__")
    }

    pub fn get_struct_type(&self, index: usize) -> Type {
        self.structdef.borrow()[index].1.clone()
    }

    pub fn get_vtable_index(&self, field: &str, ftype: &Type) -> Option<usize> {
        //self.vtable.borrow().iter().position(|ref r| r.1.as_str() == field)
        self.vtable.borrow().get_index(field, ftype)
    }

    pub fn get_vtable_type(&self, index: usize) -> Type {
        //self.vtable.borrow()[index].2.clone()
        self.vtable.borrow().get_type(index)
    }

    pub fn has_vtable(&self) -> bool {
        self.vtable.borrow().len() > 0
    }
}


pub mod llvm {
    use llvm::compiler::*;

    /*
    impl Compilable for ClassDef {

    }
    */
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructDef {
    pub ttype: Type,
    pub vars: ScopeRef,
    pub structdef: RefCell<Vec<(String, Type)>>,
}

pub type StructDefRef = Rc<StructDef>;

impl StructDef {
    pub fn new(ttype: Type, parent: Option<ScopeRef>) -> Self {
        let vars = Scope::new_ref(parent);
        vars.set_basename(ttype.get_name().unwrap());

        Self {
            ttype: ttype,
            vars: vars,
            structdef: RefCell::new(vec!()),
        }
    }

    pub fn new_ref(ttype: Type, parent: Option<ScopeRef>) -> StructDefRef {
        Rc::new(Self::new(ttype, parent))
    }

    #[must_use]
    pub fn add_field(&self, name: &str, ttype: Type) -> Result<(), Error> {
        // TODO this should be assigned a Var or something...
        self.vars.define(String::from(name), None)?;
        self.structdef.borrow_mut().push((String::from(name), ttype));
        Ok(())
    }

    pub fn get_struct_index(&self, field: &str) -> Option<usize> {
        self.structdef.borrow().iter().position(|ref r| r.0.as_str() == field)
    }

    pub fn get_struct_type(&self, index: usize) -> Type {
        self.structdef.borrow()[index].1.clone()
    }


    pub fn define_struct(session: &Session, scope: ScopeRef, id: NodeID, ttype: Type, parent: Option<ScopeRef>) -> Result<StructDefRef, Error> {
        let structdef = StructDef::new_ref(ttype.clone(), None);

        scope.define_type(ttype.get_name()?, Some(id))?;
        session.set_def(id, Def::Struct(structdef.clone()));
        session.set_type(id, ttype);
        Ok(structdef)
    }
}




#[derive(Clone, Debug, PartialEq)]
pub struct Vtable(pub Vec<(NodeID, String, Type)>);

impl Vtable {
    pub fn new() -> Self {
        Vtable(vec!())
    }

    pub fn inherit(&mut self, inherit: &Vtable) {
        self.0 = inherit.0.clone();
    }

    pub fn build_vtable(&mut self, session: &Session, scope: ScopeRef, body: &Vec<AST>) {
        for ref node in body.iter() {
            match **node {
                AST::Function(ref id, _, ref ident, ref args, ref rtype, _, ref abi) => {
                    if let Some(Ident { ref name, .. }) = ident {
                        self.add_entry(*id, name.as_str(), session.get_type(*id).unwrap());
                    }
                },
                AST::Declare(ref id, _, ref ident, _) => {
                    let ttype = session.get_type(*id).unwrap();
                    match ttype {
                        Type::Function(_, _, _) => {
                            self.add_entry(*id, ident.as_str(), ttype);
                        },
                        _ => { },
                    }
                },
                _ => { }
            }
        }

        //*self.vtable.borrow_mut() = vtable;
    }

    pub fn add_entry(&mut self, id: NodeID, name: &str, ftype: Type) {
        debug!("***************: {:?} {:?}", name, ftype);
        if let Some(index) = self.get_index(name, &ftype) {
            self.0[index].0 = id;
        } else {
            self.0.push((id, String::from(name), ftype));
        }
    }


    pub fn get_index(&self, name: &str, ftype: &Type) -> Option<usize> {
        // TODO check types for overloaded match
        self.0.iter().position(|ref r| r.1.as_str() == name)
    }

    pub fn get_type(&self, index: usize) -> Type {
        self.0[index].2.clone()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}



