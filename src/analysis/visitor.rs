
use std::cell::RefCell;

use crate::abi::ABI;
use crate::types::Type;
use crate::misc::UniqueID;
use crate::scope::ScopeRef;
use crate::session::{ Session, Error };
use crate::analysis::hir::{ Visibility, Mutability, AssignType, Literal, MatchCase, EnumVariant, WhereClause, Function, Pattern, PatKind, Expr, ExprKind };

/*
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CodeContext {
    Empty,
    Func(ABI, UniqueID),
    Class(UniqueID),
    TraitImpl(UniqueID, UniqueID),
    Import,
}
*/

#[derive(Clone, Debug, PartialEq)]
pub struct ScopeStack {
    stack: RefCell<Vec<ScopeRef>>,
}

impl ScopeStack {
    pub fn new() -> Self {
        Self {
            stack: RefCell::new(Vec::new())
        }
    }

    pub fn push_scope(&self, scope: ScopeRef) {
        self.stack.borrow_mut().push(scope);
    }

    pub fn pop_scope(&self) {
        self.stack.borrow_mut().pop();
    }

    pub fn get_scope(&self) -> ScopeRef {
        self.stack.borrow().last().cloned().unwrap()
    }
}


pub trait TypeVisitor: Sized {
    type Return;
    type Error;

    fn default_type_return(&mut self) -> Result<Self::Return, Self::Error>;

    fn visit_type(&mut self, ttype: &Type) -> Result<Self::Return, Self::Error> {
        walk_type(self, ttype)
    }

    fn visit_type_vec(&mut self, items: &Vec<Type>) -> Result<Self::Return, Self::Error> {
        for item in items {
            self.visit_type(item)?;
        }
        self.default_type_return()
    }

    fn visit_type_object(&mut self, _name: &str, _id: UniqueID, params: &Vec<Type>) -> Result<Self::Return, Self::Error> {
        self.visit_type_vec(params)
    }

    fn visit_type_tuple(&mut self, items: &Vec<Type>) -> Result<Self::Return, Self::Error> {
        self.visit_type_vec(items)
    }

    fn visit_type_record(&mut self, items: &Vec<(String, Type)>) -> Result<Self::Return, Self::Error> {
        for (_, item) in items {
            self.visit_type(item)?;
        }
        self.default_type_return()
    }

    fn visit_type_ref(&mut self, ttype: &Type) -> Result<Self::Return, Self::Error> {
        self.visit_type(ttype)
    }

    fn visit_type_function(&mut self, args: &Type, ret: &Type, _abi: ABI) -> Result<Self::Return, Self::Error> {
        self.visit_type(args)?;
        self.visit_type(ret)?;
        self.default_type_return()
    }

    fn visit_type_variable(&mut self, _id: UniqueID) -> Result<Self::Return, Self::Error> {
        self.default_type_return()
    }

    fn visit_type_universal(&mut self, _name: &str, _id: UniqueID) -> Result<Self::Return, Self::Error> {
        self.default_type_return()
    }
}

pub fn walk_type<V, R, E>(visitor: &mut V, ttype: &Type) -> Result<R, E> where V: TypeVisitor<Return=R, Error=E> {
    match ttype {
        Type::Object(name, id, params) => visitor.visit_type_object(name, *id, params),
        Type::Tuple(items) => visitor.visit_type_tuple(items),
        Type::Record(items) => visitor.visit_type_record(items),
        Type::Ref(ttype) => visitor.visit_type_ref(ttype),
        Type::Function(args, ret, abi) => visitor.visit_type_function(args, ret, *abi),
        Type::Variable(id) => visitor.visit_type_variable(*id),
        Type::Universal(name ,id) => visitor.visit_type_universal(name, *id),
    }
}


pub trait Visitor: Sized {
    type Return;

    fn default_return(&self) -> Self::Return;

    fn get_scope_stack<'a>(&'a self) -> &'a ScopeStack;
    fn get_session<'b>(&'b self) -> &'b Session;

    fn with_scope<F>(&mut self, scope: ScopeRef, f: F) -> Result<Self::Return, Error> where F: FnOnce(&mut Self) -> Result<Self::Return, Error> {
        self.get_scope_stack().push_scope(scope);
        let ret = f(self);
        self.get_scope_stack().pop_scope();
        ret
    }


    #[must_use]
    fn visit(&mut self, code: &Vec<Expr>) -> Result<Self::Return, Error> {
        self.visit_vec(code)
    }

    fn visit_literal(&mut self, _id: UniqueID, _lit: &Literal) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_annotation(&mut self, _id: UniqueID, _ttype: &Type, expr: &Expr) -> Result<Self::Return, Error> {
        self.visit_node(expr)
    }

    fn visit_pack_universal(&mut self, _id: UniqueID, _ttype: &Type, expr: &Expr) -> Result<Self::Return, Error> {
        self.visit_node(expr)
    }

    fn visit_unpack_universal(&mut self, _id: UniqueID, _ttype: &Type, expr: &Expr) -> Result<Self::Return, Error> {
        self.visit_node(expr)
    }

    fn visit_ref(&mut self, _id: UniqueID, expr: &Expr) -> Result<Self::Return, Error> {
        self.visit_node(expr)
    }

    fn visit_deref(&mut self, _id: UniqueID, expr: &Expr) -> Result<Self::Return, Error> {
        self.visit_node(expr)
    }

    fn visit_tuple(&mut self, _id: UniqueID, items: &Vec<Expr>) -> Result<Self::Return, Error> {
        self.visit_vec(items)
    }

    fn visit_record(&mut self, _id: UniqueID, items: &Vec<(String, Expr)>) -> Result<Self::Return, Error> {
        walk_record(self, items)
    }

    fn visit_record_update(&mut self, _id: UniqueID, record: &Expr, items: &Vec<(String, Expr)>) -> Result<Self::Return, Error> {
        walk_record_update(self, record, items)
    }

    fn visit_identifier(&mut self, _id: UniqueID, _name: &str) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_resolver(&mut self, _id: UniqueID, left: &Expr, _right: &str, _oid: UniqueID) -> Result<Self::Return, Error> {
        self.visit_node(left)
    }

    fn visit_accessor(&mut self, _id: UniqueID, left: &Expr, _right: &str, _oid: UniqueID) -> Result<Self::Return, Error> {
        self.visit_node(left)
    }


    fn visit_block(&mut self, _id: UniqueID, code: &Vec<Expr>) -> Result<Self::Return, Error> {
        self.visit_vec(code)
    }

    fn visit_invoke(&mut self, _id: UniqueID, func: &Expr, args: &Vec<Expr>, _fid: UniqueID) -> Result<Self::Return, Error> {
        walk_invoke(self, func, args)
    }


    fn visit_side_effect(&mut self, _id: UniqueID, _op: &str, args: &Vec<Expr>) -> Result<Self::Return, Error> {
        self.visit_vec(args)
    }


    fn visit_if(&mut self, _id: UniqueID, cond: &Expr, texpr: &Expr, fexpr: &Expr) -> Result<Self::Return, Error> {
        walk_if(self, cond, texpr, fexpr)
    }

    fn visit_raise(&mut self, _id: UniqueID, expr: &Expr) -> Result<Self::Return, Error> {
        self.visit_node(expr)
    }

    fn visit_try(&mut self, _id: UniqueID, cond: &Expr, cases: &Vec<MatchCase>) -> Result<Self::Return, Error> {
        walk_try(self, cond, cases)
    }

    fn visit_match(&mut self, _id: UniqueID, cond: &Expr, cases: &Vec<MatchCase>) -> Result<Self::Return, Error> {
        walk_match(self,cond, cases)
    }

    fn visit_while(&mut self, _id: UniqueID, cond: &Expr, body: &Expr) -> Result<Self::Return, Error> {
        walk_while(self, cond, body)
    }


    fn visit_declare(&mut self, _id: UniqueID, _vis: Visibility, _name: &str, _ttype: &Type, _whereclause: &WhereClause) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_function(&mut self, id: UniqueID, func: &Function) -> Result<Self::Return, Error> {
        walk_function(self, id, func)
    }

    fn visit_alloc_object(&mut self, _id: UniqueID, _ttype: &Type) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_class(&mut self, id: UniqueID, _classtype: &Type, _parenttype: &Option<Type>, _whereclause: &WhereClause, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        walk_class(self, id, body)
    }

    fn visit_type_alias(&mut self, _id: UniqueID, _deftype: &Type, _ttype: &Type) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_enum(&mut self, _id: UniqueID, _enumtype: &Type, _whereclause: &WhereClause, _variants: &Vec<EnumVariant>) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_trait_def(&mut self, id: UniqueID, _traitname: &str, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        walk_trait_def(self, id, body)
    }

    fn visit_trait_impl(&mut self, id: UniqueID, _traitname: &str, _impltype: &Type, _whereclause: &WhereClause, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        walk_trait_impl(self, id, body)
    }

    fn visit_methods(&mut self, id: UniqueID, _ttype: &Type, _whereclause: &WhereClause, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        walk_methods(self, id, body)
    }


    fn visit_import(&mut self, _id: UniqueID, _ident: &str, decls: &Vec<Expr>) -> Result<Self::Return, Error> {
        self.visit_vec(decls)
    }

    fn visit_definition(&mut self, _id: UniqueID, _mutable: Mutability, _name: &str, _ttype: &Option<Type>, expr: &Expr) -> Result<Self::Return, Error> {
        self.visit_node(expr)
    }

    fn visit_field(&mut self, _id: UniqueID, _mutable: Mutability, _name: &str, _ttype: &Option<Type>) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_assignment(&mut self, _id: UniqueID, left: &Expr, right: &Expr, ty: AssignType) -> Result<Self::Return, Error> {
        walk_assignment(self, left, right, ty)
    }

    fn visit_module(&mut self, _id: UniqueID, _name: &str, code: &Expr, _memo_id: UniqueID) -> Result<Self::Return, Error> {
        self.visit_node(code)
    }

    fn visit_module_decl(&mut self, _id: UniqueID, _name: &str) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }


    fn visit_pattern_wild(&mut self, _id: UniqueID) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_pattern_literal(&mut self, _id: UniqueID, _lit: &Literal, _fid: UniqueID) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_pattern_binding(&mut self, _id: UniqueID, _name: &str) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_pattern_annotation(&mut self, _id: UniqueID, _ttype: &Type, subpat: &Pattern) -> Result<Self::Return, Error> {
        self.visit_pattern(subpat)
    }

    fn visit_pattern_pack_universal(&mut self, _id: UniqueID, _ttype: &Type, subpat: &Pattern) -> Result<Self::Return, Error> {
        self.visit_pattern(subpat)
    }

    fn visit_pattern_unpack_universal(&mut self, _id: UniqueID, _ttype: &Type, subpat: &Pattern) -> Result<Self::Return, Error> {
        self.visit_pattern(subpat)
    }

    fn visit_pattern_ref(&mut self, _id: UniqueID, subpat: &Pattern) -> Result<Self::Return, Error> {
        self.visit_pattern(subpat)
    }

    fn visit_pattern_tuple(&mut self, _id: UniqueID, items: &Vec<Pattern>) -> Result<Self::Return, Error> {
        walk_pattern_vec(self, items)
    }

    fn visit_pattern_record(&mut self, _id: UniqueID, items: &Vec<(String, Pattern)>) -> Result<Self::Return, Error> {
        walk_pattern_record(self, items)
    }

    fn visit_pattern_enum_variant(&mut self, _id: UniqueID, _path: &Vec<String>, args: &Vec<Pattern>, _eid: UniqueID) -> Result<Self::Return, Error> {
        walk_pattern_vec(self, args)
    }


    fn handle_error(&mut self, _node: &Expr, err: Error) -> Result<Self::Return, Error> {
        Err(err)
    }


    fn visit_vec(&mut self, code: &Vec<Expr>) -> Result<Self::Return, Error> {
        walk_vec(self, code)
    }

    fn visit_node(&mut self, node: &Expr) -> Result<Self::Return, Error> {
        walk_node(self, node)
    }

    fn visit_pattern(&mut self, pat: &Pattern) -> Result<Self::Return, Error> {
        walk_pattern(self, pat)
    }

}

pub fn walk_record<R, V: Visitor<Return = R>>(visitor: &mut V, items: &Vec<(String, Expr)>) -> Result<R, Error> {
    for item in items {
        visitor.visit_node(&item.1)?;
    }
    Ok(visitor.default_return())
}

pub fn walk_record_update<R, V: Visitor<Return = R>>(visitor: &mut V, record: &Expr, items: &Vec<(String, Expr)>) -> Result<R, Error> {
    visitor.visit_node(record)?;
    for item in items {
        visitor.visit_node(&item.1)?;
    }
    Ok(visitor.default_return())
}

pub fn walk_invoke<R, V: Visitor<Return = R>>(visitor: &mut V, func: &Expr, args: &Vec<Expr>) -> Result<R, Error> {
    visitor.visit_node(func)?;
    visitor.visit_vec(args)
}

pub fn walk_if<R, V: Visitor<Return = R>>(visitor: &mut V, cond: &Expr, texpr: &Expr, fexpr: &Expr) -> Result<R, Error> {
    visitor.visit_node(cond)?;
    visitor.visit_node(texpr)?;
    visitor.visit_node(fexpr)?;
    Ok(visitor.default_return())
}

pub fn walk_try<R, V: Visitor<Return = R>>(visitor: &mut V, cond: &Expr, cases: &Vec<MatchCase>) -> Result<R, Error> {
    visitor.visit_node(cond)?;
    for case in cases {
        let lscope = visitor.get_session().map.get(case.id).unwrap();
        walk_match_case(visitor, lscope, case)?;
    }
    Ok(visitor.default_return())
}

pub fn walk_match<R, V: Visitor<Return = R>>(visitor: &mut V, cond: &Expr, cases: &Vec<MatchCase>) -> Result<R, Error> {
    visitor.visit_node(cond)?;
    for case in cases {
        let lscope = visitor.get_session().map.get(case.id).unwrap();
        walk_match_case(visitor, lscope, case)?;
    }
    Ok(visitor.default_return())
}

pub fn walk_match_case<R, V: Visitor<Return = R>>(visitor: &mut V, lscope: ScopeRef, case: &MatchCase) -> Result<R, Error> {
    visitor.with_scope(lscope, |visitor| {
        visitor.visit_pattern(&case.pat)?;
        visitor.visit_node(&case.body)?;
        Ok(visitor.default_return())
    })
}

pub fn walk_while<R, V: Visitor<Return = R>>(visitor: &mut V, cond: &Expr, body: &Expr) -> Result<R, Error> {
    visitor.visit_node(cond)?;
    visitor.visit_node(body)?;
    Ok(visitor.default_return())
}

pub fn walk_function<R, V: Visitor<Return = R>>(visitor: &mut V, refid: UniqueID, func: &Function) -> Result<R, Error> {
    let defid = visitor.get_session().get_ref(refid)?;
    let fscope = visitor.get_session().map.get(defid).unwrap();
    // TODO visit arguments
    visitor.with_scope(fscope, |visitor| {
        visitor.visit_vec(&func.body)
    })?;
    Ok(visitor.default_return())
}

pub fn walk_class<R, V: Visitor<Return = R>>(visitor: &mut V, refid: UniqueID, body: &Vec<Expr>) -> Result<R, Error> {
    let defid = visitor.get_session().get_ref(refid)?;
    let tscope = visitor.get_session().map.get(defid).unwrap();
    visitor.with_scope(tscope, |visitor| {
        visitor.visit_vec(body)
    })
}

pub fn walk_trait_def<R, V: Visitor<Return = R>>(visitor: &mut V, refid: UniqueID, body: &Vec<Expr>) -> Result<R, Error> {
    let defid = visitor.get_session().get_ref(refid)?;
    let tscope = visitor.get_session().map.get(defid).unwrap();
    visitor.with_scope(tscope, |visitor| {
        visitor.visit_vec(body)
    })
}

pub fn walk_trait_impl<R, V: Visitor<Return = R>>(visitor: &mut V, refid: UniqueID, body: &Vec<Expr>) -> Result<R, Error> {
    let defid = visitor.get_session().get_ref(refid)?;
    let tscope = visitor.get_session().map.get(defid).unwrap();
    visitor.with_scope(tscope, |visitor| {
        visitor.visit_vec(body)
    })
}

pub fn walk_methods<R, V: Visitor<Return = R>>(visitor: &mut V, refid: UniqueID, body: &Vec<Expr>) -> Result<R, Error> {
    let defid = visitor.get_session().get_ref(refid)?;
    let tscope = visitor.get_session().map.get(defid).unwrap();
    visitor.with_scope(tscope, |visitor| {
        visitor.visit_vec(body)
    })
}

pub fn walk_assignment<R, V: Visitor<Return = R>>(visitor: &mut V, left: &Expr, right: &Expr, _ty: AssignType) -> Result<R, Error> {
    visitor.visit_node(left)?;
    visitor.visit_node(right)?;
    Ok(visitor.default_return())
}

pub fn walk_pattern_vec<R, V: Visitor<Return = R>>(visitor: &mut V, items: &Vec<Pattern>) -> Result<R, Error> {
    for item in items {
        visitor.visit_pattern(item)?;
    }
    Ok(visitor.default_return())
}

pub fn walk_pattern_record<R, V: Visitor<Return = R>>(visitor: &mut V, items: &Vec<(String, Pattern)>) -> Result<R, Error> {
    for (_, item) in items {
        visitor.visit_pattern(item)?;
    }
    Ok(visitor.default_return())
}

pub fn walk_vec<R, V: Visitor<Return = R>>(visitor: &mut V, code: &Vec<Expr>) -> Result<R, Error> {
    let mut last: R = visitor.default_return();
    for node in code {
        last = match visitor.visit_node(node) {
            Ok(v) => v,
            Err(err) => visitor.handle_error(node, err)?,
        };
    }
    Ok(last)
}

pub fn walk_node<R, V: Visitor<Return = R>>(visitor: &mut V, node: &Expr) -> Result<R, Error> {
    match &node.kind {
        ExprKind::Literal(lit) => {
            visitor.visit_literal(node.id, lit)
        },

        ExprKind::Annotation(ttype, expr) => {
            visitor.visit_annotation(node.id, ttype, expr)
        },

        ExprKind::PackUniversal(ttype, expr) => {
            visitor.visit_pack_universal(node.id, ttype, expr)
        },

        ExprKind::UnpackUniversal(ttype, expr) => {
            visitor.visit_unpack_universal(node.id, ttype, expr)
        },

        ExprKind::Ref(expr) => {
            visitor.visit_ref(node.id, expr)
        },

        ExprKind::Deref(expr) => {
            visitor.visit_deref(node.id, expr)
        },

        ExprKind::Tuple(items) => {
            visitor.visit_tuple(node.id, items)
        },

        ExprKind::Record(items) => {
            visitor.visit_record(node.id, items)
        },

        ExprKind::RecordUpdate(record, items) => {
            visitor.visit_record_update(node.id, record, items)
        },


        ExprKind::Identifier(ident) => {
            visitor.visit_identifier(node.id, &ident)
        },

        ExprKind::Resolver(left, right, oid) => {
            visitor.visit_resolver(node.id, left, &right, *oid)
        },

        ExprKind::Accessor(left, right, oid) => {
            visitor.visit_accessor(node.id, left, &right, *oid)
        },


        ExprKind::Block(code) => {
            visitor.visit_block(node.id, code)
        },

        ExprKind::Invoke(func, args, fid) => {
            visitor.visit_invoke(node.id, func, args, *fid)
        },


        ExprKind::SideEffect(op, args) => {
            visitor.visit_side_effect(node.id, &op, args)
        },

        ExprKind::If(cond, texpr, fexpr) => {
            visitor.visit_if(node.id, cond, texpr, fexpr)
        },

        ExprKind::Raise(expr) => {
            visitor.visit_raise(node.id, expr)
        },

        ExprKind::Try(cond, cases) => {
            visitor.visit_try(node.id, cond, cases)
        },

        ExprKind::Match(cond, cases) => {
            visitor.visit_match(node.id, cond, cases)
        },

        ExprKind::While(cond, body) => {
            visitor.visit_while(node.id, cond, body)
        },


        ExprKind::Declare(vis, name, ttype, whereclause) => {
            visitor.visit_declare(node.id, *vis, name, ttype, whereclause)
        },

        ExprKind::Function(func) => {
            visitor.visit_function(node.id, func)
        },

        ExprKind::AllocObject(ttype) => {
            visitor.visit_alloc_object(node.id, ttype)
        },

        ExprKind::Class(classtype, parenttype, whereclause, body) => {
            visitor.visit_class(node.id, classtype, parenttype, whereclause, body)
        },

        ExprKind::TypeAlias(deftype, ttype) => {
            visitor.visit_type_alias(node.id, deftype, ttype)
        },

        ExprKind::Enum(enumtype, whereclause, variants) => {
            visitor.visit_enum(node.id, enumtype, whereclause, variants)
        },

        ExprKind::TraitDef(traitname, body) => {
            visitor.visit_trait_def(node.id, traitname, body)
        },

        ExprKind::TraitImpl(traitname, impltype, whereclause, body) => {
            visitor.visit_trait_impl(node.id, traitname, impltype, whereclause, body)
        },

        ExprKind::Methods(ttype, whereclause, body) => {
            visitor.visit_methods(node.id, ttype, whereclause, body)
        },


        ExprKind::Import(ident, decls) => {
            visitor.visit_import(node.id, &ident, decls)
        },

        ExprKind::Definition(mutable, name, ttype, expr) => {
            visitor.visit_definition(node.id, *mutable, &name, ttype, expr)
        },

        ExprKind::Field(mutable, name, ttype) => {
            visitor.visit_field(node.id, *mutable, &name, ttype)
        },

        ExprKind::Assignment(left, right, ty) => {
            visitor.visit_assignment(node.id, left, right, *ty)
        },

        ExprKind::Module(name, code, memo_id) => {
            visitor.visit_module(node.id, name, code, *memo_id)
        },

        ExprKind::ModuleDecl(name) => {
            visitor.visit_module_decl(node.id, name)
        },
    }
}

pub fn walk_pattern<R, V: Visitor<Return = R>>(visitor: &mut V, pat: &Pattern) -> Result<R, Error> {
    match &pat.kind {
        PatKind::Wild => {
            visitor.visit_pattern_wild(pat.id)
        },

        PatKind::Literal(lit, fid) => {
            visitor.visit_pattern_literal(pat.id, lit, *fid)
        },

        PatKind::Binding(name) => {
            visitor.visit_pattern_binding(pat.id, &name)
        },

        PatKind::Annotation(ttype, subpat) => {
            visitor.visit_pattern_annotation(pat.id, ttype, subpat)
        },

        PatKind::PackUniversal(ttype, subpat) => {
            visitor.visit_pattern_pack_universal(pat.id, ttype, subpat)
        },

        PatKind::UnpackUniversal(ttype, subpat) => {
            visitor.visit_pattern_unpack_universal(pat.id, ttype, subpat)
        },

        PatKind::Ref(subpat) => {
            visitor.visit_pattern_ref(pat.id, subpat)
        },

        PatKind::Tuple(items) => {
            visitor.visit_pattern_tuple(pat.id, items)
        },

        PatKind::Record(items) => {
            visitor.visit_pattern_record(pat.id, items)
        },

        PatKind::EnumVariant(path, args, eid) => {
            visitor.visit_pattern_enum_variant(pat.id, path, args, *eid)
        },
    }
}

