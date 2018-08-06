
use std::rc::Rc;
use std::cell::RefCell;
 
use types::Type;
use session::Error;
use ast::{ ClassSpec, AST };
use scope::{ Scope, ScopeRef };


pub type ClassVars = ScopeRef;

#[derive(Clone, Debug, PartialEq)]
pub struct ClassDef {
    pub classspec: ClassSpec,
    pub parentspec: Option<ClassSpec>,
    pub classvars: ClassVars,
    pub structdef: RefCell<Vec<(String, Type)>>,
    pub vtable: RefCell<Vec<(String, Type)>>,
}

pub type ClassDefRef = Rc<ClassDef>;


impl ClassDef {
    pub fn new(classspec: ClassSpec, parentspec: Option<ClassSpec>, classvars: ClassVars) -> ClassDef {
        ClassDef {
            classspec: classspec,
            parentspec: parentspec,
            classvars: classvars,
            structdef: RefCell::new(vec!()),
            vtable: RefCell::new(vec!()),
        }
    }

    pub fn new_ref(classspec: ClassSpec, parentspec: Option<ClassSpec>, classvars: ClassVars) -> ClassDefRef {
        Rc::new(Self::new(classspec, parentspec, classvars))
    }

    pub fn define_class(scope: ScopeRef, classspec: &ClassSpec, parentspec: Option<ClassSpec>) -> Result<ClassDefRef, Error> {
        let &ClassSpec { ref ident, ref types, .. } = classspec;

        // Find the parent class definitions, which the new class will inherit from
        let parentclass = match parentspec {
            Some(ClassSpec { ident: ref pident, .. }) => Some(scope.get_class_def(&pident.name)),
            None => None
        };

        // Create class name bindings for checking ast::accessors
        let classvars = Scope::new_ref(parentclass.map(|p| p.classvars.clone()));
        classvars.set_basename(ident.name.clone());

        // Define the class in the local scope
        let classdef = ClassDef::new_ref(classspec.clone(), parentspec.clone(), classvars);
        scope.define_type(ident.name.clone(), Type::Object(ident.name.clone(), types.clone()))?;
        scope.set_class_def(&ident.name, classdef.clone());
        // TODO i don't like this type == Class thing, but i don't know how i'll do struct types yet either
        //scope.define(name.clone(), Some(Type::Object(name.clone(), vec!())))?;
        Ok(classdef)
    }

    pub fn get_parent_spec(&self) -> Option<ClassSpec> {
        self.parentspec.clone()
    }


    pub fn build_vtable(&self, scope: ScopeRef, body: &Vec<AST>) {
        let parentclass = match self.parentspec {
            Some(ref spec) => Some(scope.get_class_def(&spec.ident.name)),
            None => None,
        };
        let mut vtable = parentclass.map_or(vec!(), |c| c.vtable.borrow().clone());

        for ref node in body.iter() {
            match **node {
                AST::Function(_, ref fident, ref args, ref rtype, _, _, ref abi) => {
                    if fident.is_some() {
                        let fname = &fident.as_ref().unwrap().name;
                        let uname = abi.unmangle_name(&fname).unwrap_or(fname.clone());
                        //if parent.contains(&uname) {
                            debug!("***************: {:?}:{:?}", self.classspec.ident.name, uname);
                            vtable.push((uname, Type::Function(Box::new(Type::Tuple(args.iter().map(|arg| arg.ttype.clone().unwrap()).collect())), Box::new(rtype.clone().unwrap()), *abi)));
                        //}
                    }
                },
                AST::Declare(_, ref fident, ref ttype) => {
                    match *ttype {
                        Type::Function(_, _, ref abi) => {
                            let uname = abi.unmangle_name(fident.name.as_ref()).unwrap_or(fident.name.clone());
                            //if parent.contains(&uname) {
                                debug!("+++++++++++++++: {:?}:{:?}", self.classspec.ident.name, uname);
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

    pub fn build_structdef(&self, scope: ScopeRef, body: &Vec<AST>) {
        let parentclass = match self.parentspec {
            Some(ref spec) => Some(scope.get_class_def(&spec.ident.name)),
            None => None
        };
        let mut structdef = parentclass.map_or(vec!(), |c| c.structdef.borrow().clone());

        if self.has_vtable() {
            if let Some(index) = structdef.iter().position(|ref r| r.0.as_str() == "__vtable__") {
                structdef[index].1 = Type::Object(format!("{}_vtable", self.classspec.ident.name), vec!());
            } else {
                structdef.push((String::from("__vtable__"), Type::Object(format!("{}_vtable", self.classspec.ident.name), vec!())));
            }
        }
        for ref node in body.iter() {
            match **node {
                AST::Definition(_, ref ident, ref ttype, ref value) => {
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

