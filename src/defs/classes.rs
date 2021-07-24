
use std::rc::Rc;
use std::cell::RefCell;

use defs::Def;
use types::Type;
use scope::{ Scope, ScopeRef };
use session::{ Session, Error };
use types::{ check_type, Check };
use hir::{ NodeID, Mutability, Ident, Expr, ExprKind };

use defs::variables::FieldDef;



#[derive(Clone, Debug, PartialEq)]
pub struct ClassDef {
    pub id: NodeID,
    pub initid: NodeID,
    pub classname: String,
    pub classtype: Type,
    pub parenttype: Option<Type>,
    pub structdef: StructDefRef,
    pub vtable: Vtable,
}

pub type ClassDefRef = Rc<ClassDef>;


impl ClassDef {
    pub fn new(id: NodeID, classname: String, classtype: Type, parenttype: Option<Type>, vars: ScopeRef, vtable: Vtable) -> Self {
        Self {
            id: id,
            initid: NodeID::generate(),
            classname: classname,
            classtype: classtype,
            parenttype: parenttype,
            structdef: StructDef::new_ref(vars),
            vtable: vtable,
        }
    }

    pub fn new_ref(id: NodeID, classname: String, classtype: Type, parenttype: Option<Type>, vars: ScopeRef, vtable: Vtable) -> ClassDefRef {
        Rc::new(Self::new(id, classname, classtype, parenttype, vars, vtable))
    }

    #[must_use]
    pub fn define(session: &Session, scope: ScopeRef, id: NodeID, classtype: Type, parenttype: Option<Type>) -> Result<ClassDefRef, Error> {
        debug!("DEF CLASS: {:?}", classtype);
        let name = classtype.get_name()?;
        let tscope = session.map.get_or_add(id, Some(scope.clone()));
        tscope.set_redirect(true);
        tscope.set_basename(name);

        // Define Self and Super, and check for typevars in the type params
        tscope.define_type("Self", Some(id))?;
        if let Some(ref ptype) = parenttype {
            tscope.define_type("Super", scope.get_type_def(&ptype.get_name()?))?;
        }

        let classdef = Self::create_class(session, scope.clone(), id, classtype.clone(), parenttype)?;

        // Define the class in the local scope
        scope.define_type(name, Some(id))?;
        session.set_def(id, Def::Class(classdef.clone()));
        session.set_type(id, classtype);

        Ok(classdef)
    }

    pub fn create_class(session: &Session, scope: ScopeRef, id: NodeID, classtype: Type, parenttype: Option<Type>) -> Result<ClassDefRef, Error> {
        let name = classtype.get_name()?;

        // Find the parent class definitions, which the new class will inherit from
        let parentclass = match parenttype {
            Some(Type::Object(ref pname, _, _)) => Some(scope.find_type_def(session, &pname)?.as_class()?),
            _ => None
        };

        // Create class name bindings for checking ast::accessors
        let vars = Scope::new_ref(parentclass.map(|p| p.structdef.vars.clone()));
        vars.set_basename(name);

        session.set_type(id, classtype.clone());
        let vtable = Vtable::create(session, NodeID::generate(), format!("{}_vtable", name.clone()))?;
        let classdef = ClassDef::new_ref(id, name.to_string(), classtype, parenttype, vars, vtable);
        Ok(classdef)
    }

    fn get_parent_class(&self, session: &Session) -> Option<ClassDefRef> {
        match self.parenttype {
            Some(Type::Object(_, ref id, _)) => Some(session.get_def(*id).unwrap().as_class().unwrap()),
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

        let vtype = session.get_type(self.vtable.id).unwrap();
        if self.has_vtable() {
            if let Some(index) = self.get_struct_vtable_index() {
                self.structdef.fields.borrow_mut()[index].2 = vtype;
            } else {
                self.structdef.add_field(session, self.vtable.id, Mutability::Immutable, "__vtable__", vtype, Define::IfNotExists);
            }
        }
        for ref node in body.iter() {
            match &node.kind {
                ExprKind::Definition(ref mutable, ref ident, _, _) => {
                    let defid = session.get_ref(node.id).unwrap();
                    self.structdef.add_field(session, defid, *mutable, ident.name.as_str(), session.get_type(defid).unwrap(), Define::IfNotExists);
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
    pub vars: ScopeRef,
    pub fields: RefCell<Vec<(NodeID, String, Type)>>,
}

pub type StructDefRef = Rc<StructDef>;

impl StructDef {
    pub fn new(vars: ScopeRef) -> Self {
        Self {
            vars: vars,
            fields: RefCell::new(vec!()),
        }
    }

    pub fn new_ref(vars: ScopeRef) -> StructDefRef {
        Rc::new(Self::new(vars))
    }

    pub fn inherit(&self, inherit: &StructDef) {
        *self.fields.borrow_mut() = inherit.fields.borrow().clone();
    }

    pub fn add_field(&self, session: &Session, id: NodeID, mutable: Mutability, name: &str, ttype: Type, define: Define) {
        if define == Define::IfNotExists && self.vars.get_var_def(name).is_none() {
            FieldDef::define(session, self.vars.clone(), id, mutable, name, Some(ttype.clone())).unwrap();
        }
        self.fields.borrow_mut().push((id, name.to_string(), ttype));
    }

    pub fn get_index(&self, field: &str) -> Option<usize> {
        self.fields.borrow().iter().position(|ref r| r.1.as_str() == field)
    }

    pub fn find_field_by_id(&self, id: NodeID) -> Option<(usize, Type)> {
        let index = self.get_index_by_id(id)?;
        Some((index, self.fields.borrow()[index].2.clone()))
    }

    pub fn get_index_by_id(&self, id: NodeID) -> Option<usize> {
        self.fields.borrow().iter().position(|ref r| r.0 == id)
    }

    pub fn foreach_field<F>(&self, mut f: F) where F: FnMut(NodeID, &str, &Type) -> () {
        for field in self.fields.borrow().iter() {
            f(field.0, &field.1, &field.2);
        }
    }
}




#[derive(Clone, Debug, PartialEq)]
pub struct Vtable {
    pub id: NodeID,
    pub table: RefCell<Vec<(NodeID, String, Type)>>,
}

impl Vtable {
    pub fn new(id: NodeID) -> Self {
        Vtable {
            id: id,
            table: RefCell::new(vec!()),
        }
    }

    pub fn create(session: &Session, id: NodeID, name: String) -> Result<Vtable, Error> {
        let vtable = Vtable::new(id);
        session.set_type(id, Type::Object(name, id, vec!()));
        Ok(vtable)
    }

    pub fn inherit(&self, inherit: &Vtable) {
        *self.table.borrow_mut() = inherit.table.borrow().clone();
    }

    pub fn build_vtable(&self, session: &Session, body: &Vec<Expr>) {
        for ref node in body.iter() {
            match &node.kind {
                ExprKind::Function(_, ref ident, _, _, _, _) => {
                    if let Some(Ident { ref name, .. }) = ident {
                        let defid = session.get_ref(node.id).unwrap();
                        self.add_entry(session, defid, name.as_str(), session.get_type(defid).unwrap());
                    }
                },
                ExprKind::Declare(_, ref ident, _) => {
                    let defid = session.get_ref(node.id).unwrap();
                    let ttype = session.get_type(defid).unwrap();
                    match ttype {
                        Type::Function(_, _, _) => {
                            self.add_entry(session, defid, ident.as_str(), ttype);
                        },
                        _ => { },
                    }
                },
                _ => { }
            }
        }
    }

    pub fn add_entry(&self, session: &Session, id: NodeID, name: &str, ftype: Type) {
        debug!("ADDING VTABLE ENTRY: {} {:?} {:?}", id, name, ftype);
        if let Some(index) = self.get_index(session, name, &ftype) {
            self.table.borrow_mut()[index].0 = id;
        } else {
            self.table.borrow_mut().push((id, String::from(name), ftype));
        }
    }

    pub fn get_index(&self, session: &Session, name: &str, ftype: &Type) -> Option<usize> {
        self.table.borrow().iter().position(|(_, ref ename, ref etype)| {
            ename.as_str() == name && check_type(session, Some(etype.clone()), Some(ftype.clone()), Check::Def, false).is_ok()
        })
    }

    pub fn get_index_by_id(&self, id: NodeID) -> Option<usize> {
        self.table.borrow().iter().position(|ref r| r.0 == id)
    }

    pub fn len(&self) -> usize {
        self.table.borrow().len()
    }

    pub fn foreach_entry<F>(&self, mut f: F) where F: FnMut(NodeID, &str, &Type) -> () {
        for entry in self.table.borrow().iter() {
            f(entry.0, &entry.1, &entry.2);
        }
    }

    pub fn foreach_enumerated<F>(&self, mut f: F) where F: FnMut(usize, NodeID, &str, &Type) -> () {
        for (i, entry) in self.table.borrow().iter().enumerate() {
            f(i, entry.0, &entry.1, &entry.2);
        }
    }
}
