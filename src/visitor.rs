
use std::cell::RefCell;

use crate::types::Type;
use crate::session::Error;
use crate::scope::ScopeRef;
use crate::hir::{ NodeID, Visibility, Mutability, AssignType, Literal, ClassSpec, MatchCase, EnumVariant, WhereClause, Function, Pattern, PatKind, Expr, ExprKind };


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
        self.stack.borrow().last().map(|s| s.clone()).unwrap()
    }
}


pub trait Visitor: Sized {
    type Return;

    fn default_return(&self) -> Self::Return;

    fn get_scope_stack<'a>(&'a self) -> &'a ScopeStack;
    fn get_scope_by_id(&self, id: NodeID) -> ScopeRef;

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

    fn visit_literal(&mut self, _id: NodeID, _lit: &Literal) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_nil(&mut self, _id: NodeID) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_annotation(&mut self, _id: NodeID, _ttype: &Type, code: &Expr) -> Result<Self::Return, Error> {
        self.visit_node(code)
    }

    fn visit_ref(&mut self, _id: NodeID, expr: &Expr) -> Result<Self::Return, Error> {
        self.visit_node(expr)
    }

    fn visit_deref(&mut self, _id: NodeID, expr: &Expr) -> Result<Self::Return, Error> {
        self.visit_node(expr)
    }

    fn visit_tuple(&mut self, _id: NodeID, items: &Vec<Expr>) -> Result<Self::Return, Error> {
        self.visit_vec(items)
    }

    fn visit_record(&mut self, id: NodeID, items: &Vec<(String, Expr)>) -> Result<Self::Return, Error> {
        walk_record(self, id, items)
    }

    fn visit_record_update(&mut self, id: NodeID, record: &Expr, items: &Vec<(String, Expr)>) -> Result<Self::Return, Error> {
        walk_record_update(self, id, record, items)
    }

    fn visit_identifier(&mut self, _id: NodeID, _name: &str) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_resolver(&mut self, _id: NodeID, left: &Expr, _right: &str, _oid: NodeID) -> Result<Self::Return, Error> {
        self.visit_node(left)
    }

    fn visit_accessor(&mut self, _id: NodeID, left: &Expr, _right: &str, _oid: NodeID) -> Result<Self::Return, Error> {
        self.visit_node(left)
    }


    fn visit_block(&mut self, _id: NodeID, code: &Vec<Expr>) -> Result<Self::Return, Error> {
        self.visit_vec(code)
    }

    fn visit_invoke(&mut self, id: NodeID, func: &Expr, args: &Vec<Expr>, _fid: NodeID) -> Result<Self::Return, Error> {
        walk_invoke(self, id, func, args)
    }


    fn visit_side_effect(&mut self, _id: NodeID, _op: &str, args: &Vec<Expr>) -> Result<Self::Return, Error> {
        self.visit_vec(args)
    }


    fn visit_if(&mut self, id: NodeID, cond: &Expr, texpr: &Expr, fexpr: &Expr) -> Result<Self::Return, Error> {
        walk_if(self, id, cond, texpr, fexpr)
    }

    fn visit_raise(&mut self, _id: NodeID, expr: &Expr) -> Result<Self::Return, Error> {
        self.visit_node(expr)
    }

    fn visit_try(&mut self, id: NodeID, cond: &Expr, cases: &Vec<MatchCase>) -> Result<Self::Return, Error> {
        walk_try(self, id, cond, cases)
    }

    fn visit_match(&mut self, id: NodeID, cond: &Expr, cases: &Vec<MatchCase>) -> Result<Self::Return, Error> {
        walk_match(self, id, cond, cases)
    }

    fn visit_while(&mut self, id: NodeID, cond: &Expr, body: &Expr) -> Result<Self::Return, Error> {
        walk_while(self, id, cond, body)
    }


    fn visit_declare(&mut self, _id: NodeID, _vis: Visibility, _name: &str, _ttype: &Type, _whereclause: &WhereClause) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_function(&mut self, id: NodeID, func: &Function) -> Result<Self::Return, Error> {
        walk_function(self, id, func)
    }

    fn visit_alloc_object(&mut self, _id: NodeID, _ttype: &Type) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_class(&mut self, id: NodeID, classspec: &ClassSpec, parentspec: &Option<ClassSpec>, whereclause: &WhereClause, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        walk_class(self, id, classspec, parentspec, whereclause, body)
    }

    fn visit_type_alias(&mut self, _id: NodeID, _classspec: &ClassSpec, _ttype: &Type) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_enum(&mut self, _id: NodeID, _classspec: &ClassSpec, _variants: &Vec<EnumVariant>) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_trait_def(&mut self, _id: NodeID, _traitspec: &ClassSpec, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        self.visit_vec(body)
    }

    fn visit_trait_impl(&mut self, _id: NodeID, _traitspec: &ClassSpec, _impltype: &Type, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        self.visit_vec(body)
    }

    fn visit_unpack_trait_obj(&mut self, _id: NodeID, _impltype: &Type, expr: &Expr) -> Result<Self::Return, Error> {
        self.visit_node(expr)
    }


    fn visit_import(&mut self, _id: NodeID, _ident: &str, decls: &Vec<Expr>) -> Result<Self::Return, Error> {
        self.visit_vec(decls)
    }

    fn visit_definition(&mut self, _id: NodeID, _mutable: Mutability, _name: &str, _ttype: &Option<Type>, expr: &Expr) -> Result<Self::Return, Error> {
        self.visit_node(expr)
    }

    fn visit_assignment(&mut self, id: NodeID, left: &Expr, right: &Expr, ty: AssignType) -> Result<Self::Return, Error> {
        walk_assignment(self, id, left, right, ty)
    }

    fn visit_module(&mut self, _id: NodeID, _name: &str, code: &Vec<Expr>, _memo_id: NodeID) -> Result<Self::Return, Error> {
        self.visit_vec(code)
    }


    fn visit_pattern_binding(&mut self, _id: NodeID, _name: &str) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_pattern_annotation(&mut self, _id: NodeID, _ttype: &Type, subpat: &Pattern) -> Result<Self::Return, Error> {
        self.visit_pattern(subpat)
    }

    fn visit_pattern_resolve(&mut self, _id: NodeID, left: &Pattern, _field: &str, _oid: NodeID) -> Result<Self::Return, Error> {
        self.visit_pattern(left)
    }

    fn visit_pattern_enum_args(&mut self, id: NodeID, left: &Pattern, args: &Vec<Pattern>, _eid: NodeID) -> Result<Self::Return, Error> {
        walk_pattern_enum_args(self, id, left, args)
    }

    fn visit_pattern_tuple(&mut self, id: NodeID, items: &Vec<Pattern>) -> Result<Self::Return, Error> {
        walk_pattern_tuple(self, id, items)
    }

    fn visit_pattern_record(&mut self, id: NodeID, items: &Vec<(String, Pattern)>) -> Result<Self::Return, Error> {
        walk_pattern_record(self, id, items)
    }

    fn visit_pattern_wild(&mut self, _id: NodeID) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_pattern_literal(&mut self, id: NodeID, lit: &Literal) -> Result<Self::Return, Error> {
        self.visit_literal(id, lit)
    }

    fn visit_pattern_identifier(&mut self, _id: NodeID, _name: &str) -> Result<Self::Return, Error> {
        Ok(self.default_return())
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

pub fn walk_record<R, V: Visitor<Return = R>>(visitor: &mut V, _id: NodeID, items: &Vec<(String, Expr)>) -> Result<R, Error> {
    for item in items {
        visitor.visit_node(&item.1)?;
    }
    Ok(visitor.default_return())
}

pub fn walk_record_update<R, V: Visitor<Return = R>>(visitor: &mut V, _id: NodeID, record: &Expr, items: &Vec<(String, Expr)>) -> Result<R, Error> {
    visitor.visit_node(record)?;
    for item in items {
        visitor.visit_node(&item.1)?;
    }
    Ok(visitor.default_return())
}

pub fn walk_invoke<R, V: Visitor<Return = R>>(visitor: &mut V, _id: NodeID, func: &Expr, args: &Vec<Expr>) -> Result<R, Error> {
    visitor.visit_node(func)?;
    visitor.visit_vec(args)
}

pub fn walk_if<R, V: Visitor<Return = R>>(visitor: &mut V, _id: NodeID, cond: &Expr, texpr: &Expr, fexpr: &Expr) -> Result<R, Error> {
    visitor.visit_node(cond)?;
    visitor.visit_node(texpr)?;
    visitor.visit_node(fexpr)?;
    Ok(visitor.default_return())
}

pub fn walk_try<R, V: Visitor<Return = R>>(visitor: &mut V, _id: NodeID, cond: &Expr, cases: &Vec<MatchCase>) -> Result<R, Error> {
    visitor.visit_node(cond)?;
    for case in cases {
        let lscope = visitor.get_scope_by_id(case.id);
        walk_match_case(visitor, lscope, case)?;
    }
    Ok(visitor.default_return())
}

pub fn walk_match<R, V: Visitor<Return = R>>(visitor: &mut V, _id: NodeID, cond: &Expr, cases: &Vec<MatchCase>) -> Result<R, Error> {
    visitor.visit_node(cond)?;
    for case in cases {
        let lscope = visitor.get_scope_by_id(case.id);
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

pub fn walk_while<R, V: Visitor<Return = R>>(visitor: &mut V, _id: NodeID, cond: &Expr, body: &Expr) -> Result<R, Error> {
    visitor.visit_node(cond)?;
    visitor.visit_node(body)?;
    Ok(visitor.default_return())
}

pub fn walk_function<R, V: Visitor<Return = R>>(visitor: &mut V, id: NodeID, func: &Function) -> Result<R, Error> {
    let fscope = visitor.get_scope_by_id(id);
    // TODO visit arguments
    visitor.with_scope(fscope, |visitor| {
        visitor.visit_vec(&func.body)
    })?;
    Ok(visitor.default_return())
}

pub fn walk_class<R, V: Visitor<Return = R>>(visitor: &mut V, id: NodeID, _classspec: &ClassSpec, _parentspec: &Option<ClassSpec>, _whereclause: &WhereClause, body: &Vec<Expr>) -> Result<R, Error> {
    let tscope = visitor.get_scope_by_id(id);
    visitor.with_scope(tscope, |visitor| {
        visitor.visit_vec(body)
    })
}

pub fn walk_assignment<R, V: Visitor<Return = R>>(visitor: &mut V, _id: NodeID, left: &Expr, right: &Expr, _ty: AssignType) -> Result<R, Error> {
    visitor.visit_node(left)?;
    visitor.visit_node(right)?;
    Ok(visitor.default_return())
}

pub fn walk_pattern_enum_args<R, V: Visitor<Return = R>>(visitor: &mut V, _id: NodeID, left: &Pattern, args: &Vec<Pattern>) -> Result<R, Error> {
    visitor.visit_pattern(left)?;
    for arg in args {
        visitor.visit_pattern(arg)?;
    }
    Ok(visitor.default_return())
}

pub fn walk_pattern_tuple<R, V: Visitor<Return = R>>(visitor: &mut V, _id: NodeID, items: &Vec<Pattern>) -> Result<R, Error> {
    for item in items {
        visitor.visit_pattern(item)?;
    }
    Ok(visitor.default_return())
}

pub fn walk_pattern_record<R, V: Visitor<Return = R>>(visitor: &mut V, _id: NodeID, items: &Vec<(String, Pattern)>) -> Result<R, Error> {
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

        ExprKind::Nil => {
            visitor.visit_nil(node.id)
        },

        ExprKind::Annotation(ttype, code) => {
            visitor.visit_annotation(node.id, ttype, code)
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

        ExprKind::Class(classspec, parentspec, whereclause, body) => {
            visitor.visit_class(node.id, classspec, parentspec, whereclause, body)
        },

        ExprKind::TypeAlias(classspec, ttype) => {
            visitor.visit_type_alias(node.id, classspec, ttype)
        },

        ExprKind::Enum(classspec, variants) => {
            visitor.visit_enum(node.id, classspec, variants)
        },

        ExprKind::TraitDef(traitspec, body) => {
            visitor.visit_trait_def(node.id, traitspec, body)
        },

        ExprKind::TraitImpl(traitspec, impltype, body) => {
            visitor.visit_trait_impl(node.id, traitspec, impltype, body)
        },

        ExprKind::UnpackTraitObject(impltype, expr) => {
            visitor.visit_unpack_trait_obj(node.id, impltype, expr)
        },


        ExprKind::Import(ident, decls) => {
            visitor.visit_import(node.id, &ident, decls)
        },

        ExprKind::Definition(mutable, name, ttype, expr) => {
            visitor.visit_definition(node.id, *mutable, &name, ttype, expr)
        },

        ExprKind::Assignment(left, right, ty) => {
            visitor.visit_assignment(node.id, left, right, *ty)
        },

        ExprKind::Module(name, code, memo_id) => {
            visitor.visit_module(node.id, name, code, *memo_id)
        },
    }
}

pub fn walk_pattern<R, V: Visitor<Return = R>>(visitor: &mut V, pat: &Pattern) -> Result<R, Error> {
    match &pat.kind {
        PatKind::Binding(name) => {
            visitor.visit_pattern_binding(pat.id, &name)
        },

        PatKind::Annotation(ttype, subpat) => {
            visitor.visit_pattern_annotation(pat.id, ttype, subpat)
        },

        PatKind::Resolve(left, name, oid) => {
            visitor.visit_pattern_resolve(pat.id, left, &name, *oid)
        },

        PatKind::EnumArgs(left, args, eid) => {
            visitor.visit_pattern_enum_args(pat.id, left, args, *eid)
        },

        PatKind::Tuple(items) => {
            visitor.visit_pattern_tuple(pat.id, items)
        },

        PatKind::Record(items) => {
            visitor.visit_pattern_record(pat.id, items)
        },

        PatKind::Wild => {
            visitor.visit_pattern_wild(pat.id)
        },

        PatKind::Literal(lit) => {
            visitor.visit_pattern_literal(pat.id, lit)
        },

        PatKind::Identifier(name) => {
            visitor.visit_pattern_identifier(pat.id, &name)
        },
    }
}

