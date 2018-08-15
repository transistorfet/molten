
use std::rc::Rc;
use std::cell::RefCell;
 
use types::Type;
use defs::Def;
use ast::{ NodeID, ClassSpec, AST };
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
    pub vtable: RefCell<Vec<(String, Type)>>,
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
            vtable: RefCell::new(vec!()),
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
        tscope.define_type(String::from("Self"), classtype.clone(), Some(id))?;
        if let Some(ref ptype) = parenttype {
            tscope.define_type(String::from("Super"), ptype.clone(), scope.get_var_def(&ptype.get_name()?))?;
        }

        let classdef = Self::create_class(session, scope.clone(), id, classtype.clone(), parenttype)?;

        // Define the class in the local scope
        scope.define_type(name.clone(), classtype, Some(id))?;
        session.set_def(id, Def::Class(classdef.clone()));
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
            Some(ref ptype) => Some(scope.find_type_def(session, &ptype.get_name().unwrap()).unwrap().as_class().unwrap()),
            None => None,
        };
        let mut vtable = parentclass.map_or(vec!(), |c| c.vtable.borrow().clone());

        for ref node in body.iter() {
            match **node {
                AST::Function(_, _, ref fident, ref args, ref rtype, _, ref abi) => {
                    if fident.is_some() {
                        let fname = &fident.as_ref().unwrap().name;
                        let uname = abi.unmangle_name(&fname).unwrap_or(fname.clone());
                        //if parent.contains(&uname) {
                            debug!("***************: {:?}:{:?}", self.classname, uname);
                            vtable.push((uname, Type::Function(Box::new(Type::Tuple(args.iter().map(|arg| arg.ttype.clone().unwrap()).collect())), Box::new(rtype.clone().unwrap()), *abi)));
                        //}
                    }
                },
                AST::Declare(_, _, ref fident, ref ttype) => {
                    match *ttype {
                        Type::Function(_, _, ref abi) => {
                            let uname = abi.unmangle_name(fident.name.as_ref()).unwrap_or(fident.name.clone());
                            //if parent.contains(&uname) {
                                debug!("+++++++++++++++: {:?}:{:?}", self.classname, uname);
                                vtable.push((uname.clone(), ttype.clone()))
                            //}
                        },
                        _ => { },
                    }
                },
                _ => { }
            }
        }

        *self.vtable.borrow_mut() = vtable;
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

    pub fn get_vtable_index(&self, field: &str) -> Option<usize> {
        self.vtable.borrow().iter().position(|ref r| r.0.as_str() == field)
    }

    pub fn get_vtable_type(&self, index: usize) -> Type {
        self.vtable.borrow()[index].1.clone()
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
        self.vars.define(String::from(name), Some(ttype.clone()), None)?;
        self.structdef.borrow_mut().push((String::from(name), ttype));
        Ok(())
    }

    pub fn get_struct_index(&self, field: &str) -> Option<usize> {
        self.structdef.borrow().iter().position(|ref r| r.0.as_str() == field)
    }

    pub fn get_struct_type(&self, index: usize) -> Type {
        self.structdef.borrow()[index].1.clone()
    }


    pub fn define_struct(scope: ScopeRef, classspec: &ClassSpec, parent: Option<ScopeRef>) -> Result<StructDefRef, Error> {
        let &ClassSpec { ref ident, ref types, .. } = classspec;

        let structdef = StructDef::new_ref(Type::from_spec(classspec.clone()), None);

        // Define the class in the local scope
        //scope.define_type(ident.name.clone(), Type::Object(ident.name.clone(), types.clone()))?;
        //scope.set_type_def(&ident.name, Def::Struct(structdef.clone()));
        Ok(structdef)
    }
}

