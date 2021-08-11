
use crate::session::Error;
use crate::scope::ScopeRef;
use crate::session::Session;
use crate::hir::{ NodeID, Pattern, PatKind, Expr, ExprKind };
use crate::visitor::{ ScopeStack };

pub trait MutVisitor: Sized {
    fn get_scope_stack<'a>(&'a self) -> &'a ScopeStack;
    fn get_session<'a>(&'a self) -> &'a Session;
    fn get_scope_by_id(&self, id: NodeID) -> ScopeRef;

    fn handle_error(&mut self, node: &Expr, err: Error);

    fn with_scope<F>(&mut self, scope: ScopeRef, f: F) where F: FnOnce(&mut Self) {
        self.get_scope_stack().push_scope(scope);
        f(self);
        self.get_scope_stack().pop_scope();
    }

    fn visit(&mut self, code: &mut Vec<Expr>) {
        self.visit_vec(code)
    }

    fn visit_vec(&mut self, code: &mut Vec<Expr>) {
        walk_mut_vec(self, code)
    }

    fn visit_node(&mut self, node: &mut Expr) {
        walk_mut_node(self, node)
    }

    fn visit_pattern(&mut self, pat: &mut Pattern) {
        walk_mut_pattern(self, pat)
    }
}

#[allow(dead_code)]
pub fn walk_mut_vec<V: MutVisitor>(visitor: &mut V, code: &mut Vec<Expr>) {
    for node in code {
        visitor.visit_node(node);
    }
}

#[allow(dead_code)]
pub fn walk_mut_node<V: MutVisitor>(visitor: &mut V, node: &mut Expr) {
    match &mut node.kind {
        ExprKind::Literal(_) => { },
        ExprKind::Identifier(_) => { },
        ExprKind::Declare(_, _, _, _) => { },
        ExprKind::AllocObject(_) => { },
        ExprKind::TypeAlias(_, _) => { },
        ExprKind::Enum(_, _, _) => { },
        ExprKind::TraitDef(_, _) => { },
        ExprKind::ModuleDecl(_) => { },
        ExprKind::Field(_, _, _) => { },

        ExprKind::Annotation(_, expr) |
        ExprKind::Ref(expr) |
        ExprKind::Deref(expr) |
        ExprKind::Resolver(expr, _, _) |
        ExprKind::Accessor(expr, _, _) |
        ExprKind::Raise(expr) |
        ExprKind::Module(_, expr, _) |
        ExprKind::Definition(_, _, _, expr) => {
            visitor.visit_node(expr);
        },

        ExprKind::Tuple(body) |
        ExprKind::Block(body) |
        ExprKind::SideEffect(_, body) |
        ExprKind::TraitImpl(_, _, _, body) |
        ExprKind::Methods(_, _, body) |
        ExprKind::Import(_, body) => {
            visitor.visit_vec(body);
        },

        ExprKind::Record(items) => {
            for (_, expr) in items {
                visitor.visit_node(expr);
            }
        },

        ExprKind::RecordUpdate(record, items) => {
            visitor.visit_node(record);
            for (_, expr) in items {
                visitor.visit_node(expr);
            }
        },

        ExprKind::Invoke(func, args, _) => {
            visitor.visit_node(func);
            visitor.visit_vec(args);
        },

        ExprKind::If(cond, texpr, fexpr) => {
            visitor.visit_node(cond);
            visitor.visit_node(texpr);
            visitor.visit_node(fexpr);
        },

        ExprKind::Assignment(left, right, _) => {
            visitor.visit_node(left);
            visitor.visit_node(right);
        },

        ExprKind::While(cond, body) => {
            visitor.visit_node(cond);
            visitor.visit_node(body);
        },

        ExprKind::Try(cond, cases) |
        ExprKind::Match(cond, cases) => {
            visitor.visit_node(cond);
            for case in cases {
                let lscope = visitor.get_scope_by_id(case.id);
                visitor.with_scope(lscope, |visitor| {
                    visitor.visit_node(&mut case.body);
                });
            }
        },

        ExprKind::Function(func) => {
            let defid = visitor.get_session().get_ref(node.id).unwrap();
            let fscope = visitor.get_scope_by_id(defid);
            visitor.with_scope(fscope, |visitor| {
                visitor.visit_vec(&mut func.body);
            });
        },

        ExprKind::Class(_, _, _, body) => {
            let defid = visitor.get_session().get_ref(node.id).unwrap();
            let tscope = visitor.get_scope_by_id(defid);
            visitor.with_scope(tscope, |visitor| {
                visitor.visit_vec(body);
            });
        },


    }
}

#[allow(dead_code)]
pub fn walk_mut_pattern_vec<V: MutVisitor>(visitor: &mut V, items: &mut Vec<Pattern>) {
    for item in items {
        visitor.visit_pattern(item);
    }
}

#[allow(dead_code)]
pub fn walk_mut_pattern<V: MutVisitor>(visitor: &mut V, pat: &mut Pattern) {
    match &mut pat.kind {
        PatKind::Wild => { },
        PatKind::Literal(_, _) => { },
        PatKind::Binding(_) => { },
        PatKind::Identifier(_) => { },

        PatKind::Annotation(_, subpat) |
        PatKind::Resolve(subpat, _, _) => {
            visitor.visit_pattern(subpat);
        },

        PatKind::EnumArgs(left, args, _) => {
            visitor.visit_pattern(left);
            walk_mut_pattern_vec(visitor, args);
        },

        PatKind::Tuple(items) => {
            walk_mut_pattern_vec(visitor, items);
        },

        PatKind::Record(items) => {
            for (_, item) in items {
                visitor.visit_pattern(item);
            }
        },
    }
}


