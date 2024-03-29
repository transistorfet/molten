
use std::rc::Rc;
use std::cell::RefCell;

use crate::defs::Def;
use crate::types::Type;
use crate::misc::UniqueID;
use crate::scope::{ Scope, ScopeRef, Context };
use crate::session::{ Session, Error };
use crate::types::{ check_type, Check };
use crate::analysis::hir::{ Mutability, Visibility, Expr, ExprKind };

use crate::defs::variables::FieldDef;



#[derive(Clone, Debug, PartialEq)]
pub struct ClassDef {
    pub id: UniqueID,
    pub initid: UniqueID,
    pub classname: String,
    pub classtype: Type,
    pub parenttype: Option<Type>,
    pub structdef: StructDefRef,
    pub vtable: Vtable,
}

pub type ClassDefRef = Rc<ClassDef>;


impl ClassDef {
    pub fn new(defid: UniqueID, classname: String, classtype: Type, parenttype: Option<Type>, vars: ScopeRef, vtable: Vtable) -> Self {
        Self {
            id: defid,
            initid: UniqueID::generate(),
            classname,
            classtype,
            parenttype,
            structdef: StructDef::new_ref(defid, vars),
            vtable,
        }
    }

    pub fn new_ref(defid: UniqueID, classname: String, classtype: Type, parenttype: Option<Type>, vars: ScopeRef, vtable: Vtable) -> ClassDefRef {
        Rc::new(Self::new(defid, classname, classtype, parenttype, vars, vtable))
    }

    #[must_use]
    pub fn define(session: &Session, scope: ScopeRef, defid: UniqueID, classtype: Type, parenttype: Option<Type>) -> Result<ClassDefRef, Error> {
        debug!("DEF CLASS: {:?}", classtype);
        let name = classtype.get_name()?;
        let tscope = session.map.get(defid).unwrap();

        // Define Self and Super, and check for typevars in the type params
        tscope.define_type("Self", defid)?;
        if let Some(ptype) = &parenttype {
            tscope.define_type("Super", scope.get_type_def(&ptype.get_name()?).unwrap())?;
        }

        let classdef = Self::create_class(session, scope.clone(), defid, classtype.clone(), parenttype)?;

        // Define the class in the local scope
        scope.define_type(name, defid)?;
        session.set_def(defid, Def::Class(classdef.clone()));
        session.set_type(defid, classtype);

        Ok(classdef)
    }

    pub fn create_class(session: &Session, scope: ScopeRef, defid: UniqueID, classtype: Type, parenttype: Option<Type>) -> Result<ClassDefRef, Error> {
        let name = classtype.get_name()?;

        // Find the parent class definitions, which the new class will inherit from
        let parentclass = match &parenttype {
            Some(Type::Object(pname, _, _)) => Some(scope.find_type_def(session, pname)?.as_class()?),
            _ => None
        };

        // Create class name bindings for checking ast::accessors
        let vars = Scope::new_ref(name, Context::Class(defid), parentclass.map(|p| p.structdef.vars.clone()));

        session.set_type(defid, classtype.clone());
        let vtable = Vtable::create(session, UniqueID::generate(), format!("{}_vtable", name))?;
        let classdef = ClassDef::new_ref(defid, name.to_string(), classtype, parenttype, vars, vtable);
        Ok(classdef)
    }

    fn get_parent_class(&self, session: &Session) -> Option<ClassDefRef> {
        match self.parenttype {
            Some(Type::Object(_, id, _)) => Some(session.get_def(id).unwrap().as_class().unwrap()),
            None => None,
            _ => panic!("UnexpectedError: parent class type should be Type:Object()"),
        }
    }

    pub fn build_vtable(&self, session: &Session, body: &Vec<Expr>) {
        let parentclass = self.get_parent_class(session);
        if let Some(parentclass) = parentclass {
            self.vtable.inherit(&parentclass.vtable);
        }

        self.vtable.build_vtable(session, body);
    }

    pub fn build_structdef(&self, session: &Session, body: &Vec<Expr>) {
        let parentclass = self.get_parent_class(session);
        if let Some(cls) = parentclass {
            self.structdef.inherit(&cls.structdef);
        }

        let vtype = session.get_type(self.vtable.id).unwrap().clone();
        if self.has_vtable() {
            if let Some(index) = self.get_struct_vtable_index() {
                self.structdef.fields.borrow_mut()[index].2 = vtype;
            } else {
                self.structdef.add_field(session, self.vtable.id, Mutability::Immutable, "__vtable__", vtype, Define::IfNotExists);
            }
        }
        for node in body.iter() {
            match &node.kind {
                ExprKind::Field(mutable, name, _) => {
                    let defid = session.get_ref(node.id).unwrap();
                    self.structdef.add_field(session, defid, *mutable, &name, session.get_type(defid).unwrap().clone(), Define::IfNotExists);
                },
                _ => { }
            }
        }
    }

    pub fn get_struct_index(&self, field: &str) -> Option<usize> {
        self.structdef.get_index(field)
    }

    pub fn get_struct_vtable_index(&self) -> Option<usize> {
        self.get_struct_index("__vtable__")
    }

    pub fn has_vtable(&self) -> bool {
        self.vtable.len() > 0
    }
}


#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Define {
    IfNotExists,
    #[allow(dead_code)]
    Never,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructDef {
    pub id: UniqueID,
    pub vars: ScopeRef,
    pub fields: RefCell<Vec<(UniqueID, String, Type)>>,
}

pub type StructDefRef = Rc<StructDef>;

impl StructDef {
    pub fn new(id: UniqueID, vars: ScopeRef) -> Self {
        Self {
            id,
            vars,
            fields: RefCell::new(vec!()),
        }
    }

    pub fn new_ref(id: UniqueID, vars: ScopeRef) -> StructDefRef {
        Rc::new(Self::new(id, vars))
    }

    pub fn inherit(&self, inherit: &StructDef) {
        *self.fields.borrow_mut() = inherit.fields.borrow().clone();
    }

    pub fn add_field(&self, session: &Session, id: UniqueID, mutable: Mutability, name: &str, ttype: Type, define: Define) {
        if define == Define::IfNotExists && self.vars.get_var_def(name).is_none() {
            FieldDef::define(session, self.vars.clone(), id, mutable, name, Some(ttype.clone())).unwrap();
        }
        self.fields.borrow_mut().push((id, name.to_string(), ttype));
    }

    pub fn get_index(&self, field: &str) -> Option<usize> {
        self.fields.borrow().iter().position(|r| r.1 == field)
    }

    pub fn find_field_by_id(&self, id: UniqueID) -> Option<(usize, Type)> {
        let index = self.get_index_by_id(id)?;
        Some((index, self.fields.borrow()[index].2.clone()))
    }

    pub fn get_index_by_id(&self, id: UniqueID) -> Option<usize> {
        self.fields.borrow().iter().position(|r| r.0 == id)
    }

    pub fn foreach_field<F>(&self, mut f: F) where F: FnMut(UniqueID, &str, &Type) -> () {
        for field in self.fields.borrow().iter() {
            f(field.0, &field.1, &field.2);
        }
    }
}




#[derive(Clone, Debug, PartialEq)]
pub struct Vtable {
    pub id: UniqueID,
    pub table: RefCell<Vec<(UniqueID, String, Type)>>,
}

impl Vtable {
    pub fn new(id: UniqueID) -> Self {
        Vtable {
            id,
            table: RefCell::new(vec!()),
        }
    }

    pub fn create(session: &Session, id: UniqueID, name: String) -> Result<Vtable, Error> {
        let vtable = Vtable::new(id);
        session.set_type(id, Type::Object(name, id, vec!()));
        Ok(vtable)
    }

    pub fn inherit(&self, inherit: &Vtable) {
        *self.table.borrow_mut() = inherit.table.borrow().clone();
    }

    pub fn build_vtable(&self, session: &Session, body: &Vec<Expr>) {
        for node in body.iter() {
            match &node.kind {
                ExprKind::Function(func) => {
                    if func.vis != Visibility::Anonymous {
                        let defid = session.get_ref(node.id).unwrap();
                        self.add_entry(session, defid, &func.name, session.get_type(defid).unwrap().clone());
                    }
                },
                ExprKind::Declare(_, name, _, _) => {
                    let defid = session.get_ref(node.id).unwrap();
                    let ttype = session.get_type(defid).unwrap();
                    match ttype {
                        Type::Function(_, _, _) => {
                            self.add_entry(session, defid, &name, ttype.clone());
                        },
                        _ => { },
                    }
                },
                _ => { }
            }
        }
    }

    pub fn add_entry(&self, session: &Session, id: UniqueID, name: &str, ftype: Type) {
        debug!("ADDING VTABLE ENTRY: {} {:?} {:?}", id, name, ftype);
        if let Some(index) = self.get_index(session, name, &ftype) {
            self.table.borrow_mut()[index].0 = id;
        } else {
            self.table.borrow_mut().push((id, String::from(name), ftype));
        }
    }

    pub fn get_index(&self, session: &Session, name: &str, ftype: &Type) -> Option<usize> {
        self.table.borrow().iter().position(|(_, ename, etype)| {
            ename == name && check_type(session, Some(etype), Some(ftype), Check::Def, false).is_ok()
        })
    }

    pub fn get_index_by_id(&self, id: UniqueID) -> Option<usize> {
        self.table.borrow().iter().position(|r| r.0 == id)
    }

    pub fn len(&self) -> usize {
        self.table.borrow().len()
    }

    pub fn foreach_entry<F>(&self, mut f: F) where F: FnMut(UniqueID, &str, &Type) -> () {
        for entry in self.table.borrow().iter() {
            f(entry.0, &entry.1, &entry.2);
        }
    }

    pub fn foreach_enumerated<F>(&self, mut f: F) where F: FnMut(usize, UniqueID, &str, &Type) -> () {
        for (i, entry) in self.table.borrow().iter().enumerate() {
            f(i, entry.0, &entry.1, &entry.2);
        }
    }
}
