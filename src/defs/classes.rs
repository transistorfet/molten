
use std::rc::Rc;
use std::cell::RefCell;
 
use types::Type;
use defs::TypeDef;
use ast::{ NodeID, ClassSpec, AST };
use scope::{ Scope, ScopeRef };
use session::{ Session, Error };


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
    pub fn new(classspec: ClassSpec, parentspec: Option<ClassSpec>, classvars: ClassVars) -> Self {
        Self {
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

    /*
    pub fn define(session: &Session, scope: ScopeRef, node: &mut AST) -> Result<(), Error> {
        if let AST::Class(ref id, _, ref mut classspec, ref mut parentspec, ref mut body) = *node {
            // Create a temporary invisible scope to name check the class body
            let tscope = session.map.add(*id, Some(scope.clone()));
            tscope.set_class(true);
            tscope.set_basename(classspec.ident.name.clone());

            // Define Self and Super, and check for typevars in the type params
            classspec.types.iter_mut().map(|ref mut ttype| declare_typevars(tscope.clone(), Some(ttype), true).unwrap()).count();
            tscope.define_type(String::from("Self"), Type::from_spec(classspec.clone()))?;
            if let &mut Some(ClassSpec { ident: ref pident, types: ref mut ptypes, .. }) = parentspec {
                ptypes.iter_mut().map(|ref mut ttype| declare_typevars(tscope.clone(), Some(ttype), false).unwrap()).count();
                tscope.define_type(String::from("Super"), Type::Object(pident.name.clone(), ptypes.clone()))?;
            }

            ClassDef::define_class(scope, classspec, parentspec.clone())?;
            bind_names_vec(session, tscope, body);
        }
    }
    */

    pub fn define_class(session: &Session, scope: ScopeRef, id: NodeID, classspec: &ClassSpec, parentspec: Option<ClassSpec>) -> Result<ClassDefRef, Error> {
        let &ClassSpec { ref ident, ref types, .. } = classspec;

        // Find the parent class definitions, which the new class will inherit from
        let parentclass = match parentspec {
            Some(ClassSpec { ident: ref pident, .. }) => Some(session.find_type_def(scope.clone(), &pident.as_str())?.as_class()?),
            None => None
        };

        // Create class name bindings for checking ast::accessors
        let classvars = Scope::new_ref(parentclass.map(|p| p.classvars.clone()));
        classvars.set_basename(ident.name.clone());

        // Define the class in the local scope
        let classdef = ClassDef::new_ref(classspec.clone(), parentspec.clone(), classvars);
        scope.define_type(ident.name.clone(), Type::Object(ident.name.clone(), types.clone()))?;
        scope.set_type_def(&ident.name, id);
        session.set_type_def(id, TypeDef::Class(classdef.clone()));
        // TODO i don't like this type == Class thing, but i don't know how i'll do struct types yet either
        //scope.define(name.clone(), Some(Type::Object(name.clone(), vec!())))?;
        Ok(classdef)
    }

    pub fn get_parent_spec(&self) -> Option<ClassSpec> {
        self.parentspec.clone()
    }


    pub fn build_vtable(&self, session: &Session, scope: ScopeRef, body: &Vec<AST>) {
        let parentclass = match self.parentspec {
            Some(ref spec) => Some(session.find_type_def(scope.clone(), &spec.ident.as_str()).unwrap().as_class().unwrap()),
            None => None,
        };
        let mut vtable = parentclass.map_or(vec!(), |c| c.vtable.borrow().clone());

        for ref node in body.iter() {
            match **node {
                AST::Function(ref id, _, ref fident, ref args, ref rtype, _, ref abi) => {
                    if fident.is_some() {
                        let fname = &fident.as_ref().unwrap().name;
                        let uname = abi.unmangle_name(&fname).unwrap_or(fname.clone());
                        //if parent.contains(&uname) {
                            debug!("***************: {:?}:{:?}", self.classspec.ident.name, uname);
                            vtable.push((uname, Type::Function(Box::new(Type::Tuple(args.iter().map(|arg| arg.ttype.clone().unwrap()).collect())), Box::new(rtype.clone().unwrap()), *abi)));
                        //}
                    }
                },
                AST::Declare(ref id, _, ref fident, ref ttype) => {
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

    pub fn build_structdef(&self, session: &Session, scope: ScopeRef, body: &Vec<AST>) {
        let parentclass = match self.parentspec {
            Some(ref spec) => Some(session.find_type_def(scope.clone(), &spec.ident.as_str()).unwrap().as_class().unwrap()),
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
                AST::Definition(ref id, _, ref ident, ref ttype, ref value) => {
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

    pub fn add_field(&self, name: &str, ttype: Type) {
        self.vars.define(String::from(name), Some(ttype.clone()));
        self.structdef.borrow_mut().push((String::from(name), ttype));
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
        //scope.set_type_def(&ident.name, TypeDef::Struct(structdef.clone()));
        Ok(structdef)
    }
}

