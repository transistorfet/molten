
use crate::types::Type;
use crate::misc::UniqueID;
use crate::session::{ Session, Error };
use crate::scope::{ ScopeRef };
use crate::analysis::hir::{ Pattern, PatKind, Expr, ExprKind };
use crate::analysis::visitor::ScopeStack;
use crate::analysis::mutvisitor::{ MutVisitor, walk_mut_node, walk_mut_pattern, swap_node_value, swap_pattern_value };


#[derive(Clone, Debug, PartialEq)]
pub struct AutoPack<'sess> {
    pub session: &'sess Session,
    pub stack: ScopeStack,
}


impl<'sess> AutoPack<'sess> {
    pub fn visit(session: &'sess Session, scope: ScopeRef, code: &mut Vec<Expr>) {
        let mut convert = AutoPack {
            session: session,
            stack: ScopeStack::new(),
        };

        convert.stack.push_scope(scope);
        convert.visit(code);
    }
}

impl<'sess> MutVisitor for AutoPack<'sess> {
    fn get_scope_stack<'a>(&'a self) -> &'a ScopeStack {
        &self.stack
    }

    fn get_session<'a>(&'a self) -> &'a Session {
        self.session
    }

    fn get_scope_by_id(&self, id: UniqueID) -> ScopeRef {
        self.session.map.get(id).unwrap()
    }

    fn handle_error(&mut self, _node: &Expr, err: Error) {
        panic!("{}", err);
    }

    fn visit_node(&mut self, node: &mut Expr) {
        walk_mut_node(self, node);

        match &mut node.kind {
            ExprKind::Invoke(_, args, fid) => {
                let ftype = self.session.get_type(*fid).unwrap();
                let argtypes = ftype.get_argtypes().unwrap().as_vec();
                let rettype = ftype.get_rettype().unwrap();

                for i in 0..args.len() {
                    let src_type = self.session.get_type(args[i].id).unwrap();
                    self.check_expr_for_type_cast(&mut args[i], &argtypes[i], &src_type);
                }

                let result_type = self.session.get_type(node.id).unwrap();
                self.check_expr_for_type_cast(node, &result_type, &rettype);

            },

            ExprKind::Definition(_, _, _, expr) => {
                let defid = self.session.get_ref(node.id).unwrap();
                let dtype = self.session.get_type(defid).unwrap();
                let etype = self.session.get_type(expr.id).unwrap();
                self.check_expr_for_type_cast(node, &dtype, &etype);
            },

            ExprKind::Annotation(_, expr) => {
                // TODO check for a boxing or unboxing
                let dtype = self.session.get_type(node.id).unwrap();
                let etype = self.session.get_type(expr.id).unwrap();
                self.check_expr_for_type_cast(node, &dtype, &etype);
            },

            ExprKind::Assignment(left, right, _) => {
                match left.kind {
                    ExprKind::Accessor(_, _, _) => {
                        let defid = self.session.get_ref(left.id).unwrap();
                        let dtype = self.session.get_type(defid).unwrap();
                        let etype = self.session.get_type(right.id).unwrap();
                        self.check_expr_for_type_cast(right, &dtype, &etype);
                    },
                    _ => { /* Skip assigment to Derefs */ },
                }
            },

            _ => { },
        }
    }

    fn visit_pattern(&mut self, pat: &mut Pattern) {
        walk_mut_pattern(self, pat);

        match &mut pat.kind {
            PatKind::EnumVariant(_, args, eid) => {
                let argtypes = self.session.get_type(*eid).unwrap().as_vec();
                for i in 0..args.len() {
                    let unpacked_type = self.session.get_type(args[i].id).unwrap();
                    self.check_pattern_for_type_cast(&mut args[i], &unpacked_type, &argtypes[i]);
                }
            },

            _ => { },
        }
    }
}

impl<'sess> AutoPack<'sess> {
    fn check_expr_for_type_cast(&mut self, expr: &mut Expr, dest_type: &Type, src_type: &Type) {
        match (dest_type, src_type) {
            (Type::Universal(_, _), Type::Universal(_, _)) => {
                // If the source of data is also a universal, then don't convert
                // NOTE this assumes the constraints on both universals is identical
            },
            (utype @ Type::Universal(_, _), _) => {
                swap_node_value(expr, |arg| {
                    let expr = Expr::make_pack_universal(utype.clone(), arg);
                    self.session.set_type(expr.id, src_type.clone());
                    expr
                });
            },
            (ttype, Type::Universal(_, _)) => {
                swap_node_value(expr, |arg| {
                    let expr = Expr::make_unpack_universal(ttype.clone(), arg);
                    self.session.set_type(expr.id, ttype.clone());
                    expr
                });
            },
            _ => { },
        }
    }

    fn check_pattern_for_type_cast(&mut self, pat: &mut Pattern, dest_type: &Type, src_type: &Type) {
        match (dest_type, src_type) {
            (Type::Universal(_, _), Type::Universal(_, _)) => {
                // If the source of data is also a universal, then don't convert
                // NOTE this assumes the constraints on both universals is identical
            },
            (utype @ Type::Universal(_, _), _) => {
                swap_pattern_value(pat, |arg| {
                    let pat = Pattern::make_pack_universal(arg.pos, utype.clone(), arg);
                    self.session.set_type(pat.id, src_type.clone());
                    pat
                });
            },
            (ttype, Type::Universal(_, _)) => {
                swap_pattern_value(pat, |arg| {
                    let pat = Pattern::make_unpack_universal(arg.pos, ttype.clone(), arg);
                    self.session.set_type(pat.id, ttype.clone());
                    pat
                });
            },
            _ => { },
        }
    }
}

