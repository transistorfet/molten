
use std::cell::RefCell;

use crate::abi::ABI;
use crate::types::Type;
use crate::ast::{ Pos, AST };
use crate::misc::{ r, UniqueID };
use crate::session::{ Session, Error };
use crate::defs::modules::{ ModuleDef };
use crate::hir::{ NodeID, Visibility, Mutability, AssignType, Literal, Argument, MatchCase, EnumVariant, Function, WhereClause, Pattern, Expr, ExprKind };


#[derive(Copy, Clone, Debug, PartialEq)]
enum CodeContext {
    Func(ABI),
    ClassBody,
    Constructor,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Refinery<'sess> {
    pub session: &'sess Session,
    context: RefCell<Vec<CodeContext>>,
}

impl<'sess> Refinery<'sess> {
    pub fn refine(session: &'sess Session, code: Vec<AST>) -> Vec<Expr> {
        let refinery = Refinery {
            session: session,
            context: RefCell::new(vec!()),
        };

        let refined = refinery.refine_module(code);
        if session.errors.get() > 0 {
            panic!("Exiting due to previous errors");
        }
        refined
    }

    fn with_context<F, R>(&self, context: CodeContext, f: F) -> R where F: FnOnce() -> R {
        self.context.borrow_mut().push(context);
        let ret = f();
        self.context.borrow_mut().pop();
        ret
    }

    fn get_context(&self) -> Option<CodeContext> {
        self.context.borrow().last().map(|c| *c)
    }

    // TODO this is a hack to keep the old pub/private visibility as before, until we settle on the behaviour
    fn top_level(&self) -> bool {
        let len = self.context.borrow().len();
        len == 0 || self.get_context() == Some(CodeContext::ClassBody) && len == 1
    }

    pub fn refine_module(&self, code: Vec<AST>) -> Vec<Expr> {
        let module_name = ModuleDef::get_module_name(&self.session.name);
        let memo = ModuleDef::get_memo_name(&module_name);

        let mut init_code = vec!();
        init_code.push(Expr::make_assign(Pos::empty(), Expr::make_ident_from_str(Pos::empty(), &memo), Expr::make_lit(Literal::Boolean(true)), AssignType::Update));
        init_code.extend(self.refine_vec(code));
        init_code.push(Expr::make_lit(Literal::Boolean(true)));

        let body = vec!(
            Expr::make_match(Pos::empty(), Expr::make_ident_from_str(Pos::empty(), &memo), vec!(
                MatchCase::new(Pattern::make_lit(Literal::Boolean(true)), Expr::make_lit(Literal::Boolean(true))),
                MatchCase::new(Pattern::make_lit(Literal::Boolean(false)), Expr::make_block(Pos::empty(), init_code)),
            ))
        );

        let module_run_name = ModuleDef::get_run_name(&module_name);
        let func = Expr::new(Pos::empty(), ExprKind::Function(Function::new(Visibility::Public, module_run_name, vec!(), None, body, ABI::Molten, WhereClause::empty())));
        vec!(Expr::make_module(module_name, func))
    }

    pub fn refine_vec(&self, code: Vec<AST>) -> Vec<Expr> {
        let mut block = vec!();
        for node in code {
            let pos = node.get_pos();
            match self.refine_node(node) {
                Ok(refined) => block.push(refined),
                Err(err) => self.session.print_error(&err.add_pos(pos)),
            }
        }
        block
    }

    pub fn refine_node(&self, node: AST) -> Result<Expr, Error> {
        Ok(match node {
            AST::Block(pos, mut code) => {
                if code.len() == 0 {
                    code.push(AST::Literal(Literal::Unit))
                }
                Expr::make_block(pos, self.refine_vec(code))
            },

            AST::Definition(pos, mutable, ident, ttype, code) => {
                Expr::make_def(pos, mutable, ident, ttype, self.refine_node(*code)?)
            },

            AST::Declare(pos, vis, ident, ttype, whereclause) => {
                Expr::make_decl(pos, vis, ident, ttype, whereclause)
            },

            AST::Function(pos, _vis, name, args, ret, body, abi, whereclause) => {
                // TODO visibility is forced here so I don't have to add 'pub' keywords yet
                let vis = if self.top_level() {
                    Visibility::Public
                } else {
                    Visibility::Private
                };

                let args = args.into_iter().map(|arg| Argument::new(arg.pos, arg.name, arg.ttype)).collect();

                let id = NodeID::generate();
                let name = name.unwrap_or(format!("anon{}", id));
                let context = if self.get_context() == Some(CodeContext::ClassBody) && &name == "new" {
                    CodeContext::Constructor
                } else {
                    CodeContext::Func(abi)
                };
                self.with_context(context, || {
                    let func = Function::new(vis, name, args, ret, self.refine_vec(body), abi, whereclause);
                    Ok(Expr::new_with_id(id, pos, ExprKind::Function(func)))
                })?
            },

            AST::Invoke(pos, fexpr, args) => {
                let convert = match *fexpr {
                    AST::Accessor(_, _, _) => true,
                    _ => false,
                };

                let fexpr = self.refine_node(*fexpr)?;
                let mut args = self.refine_vec(args);

                match fexpr.kind {
                    ExprKind::Accessor(expr, field, _oid) if convert => {
                        let tmpname = format!("{}", UniqueID::generate());
                        args.insert(0, Expr::make_ident_from_str(pos, &tmpname));

                        Expr::make_block(pos, vec!(
                            Expr::make_def(pos, Mutability::Immutable, tmpname.to_string(), None, *expr), 
                            Expr::make_invoke(pos, Expr::make_access(pos, Expr::make_ident_from_str(pos, &tmpname), field), args),
                        ))
                    },
                    _ =>
                        Expr::make_invoke(pos, fexpr, args),
                }
            },

            AST::Bracketed(expr) => {
                self.refine_node(*expr)?
            },

            AST::SideEffect(pos, op, args) => {
                Expr::make_side_effect(pos, op, self.refine_vec(args))
            },

            AST::If(pos, cond, texpr, fexpr) => {
                Expr::make_if(pos, self.refine_node(*cond)?, self.refine_node(*texpr)?, self.refine_node(*fexpr)?)
            },

            AST::Match(pos, cond, cases) => {
                let cases = self.refine_cases(cases)?;
                Expr::make_match(pos, self.refine_node(*cond)?, cases)
            },

            AST::Try(pos, cond, cases) => {
                let cases = self.refine_cases(cases)?;
                Expr::make_try(pos, self.refine_node(*cond)?, cases)
            },

            AST::Raise(pos, expr) => {
                match self.get_context() {
                    Some(CodeContext::ClassBody) |
                    Some(CodeContext::Func(ABI::C)) =>
                        return Err(Error::new("SyntaxError: raise keyword cannot appear in this context".to_string())),
                    _ => { },
                }
                Expr::make_raise(pos, self.refine_node(*expr)?)
            },

            AST::While(pos, cond, body) => {
                Expr::make_while(pos, self.refine_node(*cond)?, self.refine_node(*body)?)
            },

            AST::For(pos, ident, array, body) => {
                self.desugar_for_loop(pos, ident, *array, *body)?
            },

            AST::Ref(pos, expr) => { Expr::make_ref(pos, self.refine_node(*expr)?) },
            AST::Deref(pos, expr) => { Expr::make_deref(pos, self.refine_node(*expr)?) },

            AST::Tuple(pos, items) => { Expr::make_tuple(pos, self.refine_vec(items)) },

            AST::Record(pos, mut items) => {
                items.sort_unstable_by(|a, b| a.0.cmp(&b.0));
                let mut refined = vec!();
                for (i, e) in items {
                    refined.push((i, self.refine_node(e)?));
                }
                Expr::make_record(pos, refined)
            },

            AST::RecordUpdate(pos, record, mut items) => {
                items.sort_unstable_by(|a, b| a.0.cmp(&b.0));
                let mut refined = vec!();
                for (i, e) in items {
                    refined.push((i, self.refine_node(e)?));
                }
                Expr::make_record_update(pos, self.refine_node(*record)?, refined)
            },

            AST::Enum(pos, enumtype, whereclause, variants) => {
                let variants = variants.into_iter().map(|(pos, ident, ttype)| EnumVariant::new(pos, ident, ttype)).collect();
                Expr::make_enum(pos, enumtype, whereclause, variants)
            },

            AST::TraitDef(pos, traitname, body) => {
                for node in body.iter() {
                    match node {
                        AST::Declare(_, _, _, _, _) => { },
                        node => return Err(Error::new(format!("SyntaxError: only decls are allowed in trait definitions, found {:?}", node))),
                    }
                }
                Expr::make_trait_def(pos, traitname, self.refine_vec(body))
            },

            AST::TraitImpl(pos, traitname, impltype, whereclause, body) => {
                self.desugar_trait_impl(pos, traitname, impltype, whereclause, body)?
            },

            AST::Array(pos, items) => {
                self.desugar_array(pos, items)?
            },

            AST::New(pos, ttype, args) => {
                let object = Expr::make_alloc_object(pos, ttype.clone());

                let mut args = args.into_iter().map(|arg| self.refine_node(arg)).collect::<Result<Vec<_>, _>>()?;
                args.insert(0, object);
                Expr::make_annotation(ttype.clone(),
                    Expr::make_invoke(pos, Expr::make_resolve_ident(pos, ttype.get_name()?, "new"), args))
            },

            AST::Class(pos, classtype, parenttype, whereclause, body) => {
                self.desugar_class(pos, classtype, parenttype, whereclause, body)?
            },

            AST::Field(pos, mutable, ident, ttype) => {
                Expr::make_field(pos, mutable, ident, ttype)
            },

            AST::Index(pos, base, index) => {
                self.refine_node(AST::Invoke(pos, r(AST::Accessor(pos, base, "[]".to_string())), vec!(*index)))?
            },

            AST::Resolver(pos, left, right) => {
                // TODO should this also allow a non-type specifier?
                match *left {
                    AST::Identifier(_, _) => { },
                    _ => return Err(Error::new("SyntaxError: left-hand side of scope resolver must be identifier".to_string())),
                }
                Expr::make_resolve(pos, self.refine_node(*left)?, right)
            },

            AST::Accessor(pos, left, right) => {
                Expr::make_access(pos, self.refine_node(*left)?, right)
            },

            AST::Assignment(pos, left, right, ty) => {
                let left = *left;
                match left {
                    //AST::Identifier_, _) |
                    AST::Deref(_, _) => {
                        Expr::make_assign(pos, self.refine_node(left)?, self.refine_node(*right)?, ty)
                    },
                    AST::Accessor(_, ref obj, _) => {
                        // Assignments within a constructor should be marked as initialization assignments rather than updating assignments
                        let ty = match &**obj {
                            AST::Identifier(_, name) if name.as_str() == "self" && self.get_context() == Some(CodeContext::Constructor) => AssignType::Initialize,
                            _ => AssignType::Update,
                        };
                        Expr::make_assign(pos, self.refine_node(left)?, self.refine_node(*right)?, ty)
                    },
                    AST::Index(ipos, base, index) => {
                        self.refine_node(AST::Invoke(pos, r(AST::Accessor(ipos, base, "[]".to_string())), vec!(*index, *right)))?
                    },
                    _ => return Err(Error::new(format!("SyntaxError: assignment to to an invalid element: {:?}", left))),
                }
            },

            AST::ModuleDecl(pos, name) => {
                Expr::make_module_decl(pos.clone(), name)
            },

            AST::Import(pos, ident, _) => {
                let path = ident.replace(".", "/") + ".dec";
                let ast = self.session.parse_file(&path, true);
                let decls = self.refine_vec(ast);
                Expr::make_import(pos, ident, decls)
            },

            AST::Annotation(ttype, value) => {
                Expr::make_annotation(ttype, self.refine_node(*value)?)
            },

            AST::Nil => { Expr::make_nil() },
            AST::Literal(val) => { Expr::make_lit(val) },
            AST::Identifier(pos, ident) => { Expr::make_ident(pos, ident) },
            AST::TypeAlias(pos, deftype, ttype) => { Expr::make_type_alias(pos, deftype, ttype) },
        })
    }

    pub fn refine_cases(&self, cases: Vec<(Pattern, AST)>) -> Result<Vec<MatchCase>, Error> {
        let mut refined = vec!();
        for case in cases {
            let pat = self.refine_pattern(case.0)?;
            let body = self.refine_node(case.1)?;
            refined.push(MatchCase::new(pat, body));
        }
        Ok(refined)
    }

    pub fn refine_pattern(&self, pat: Pattern) -> Result<Pattern, Error> {
        // TODO refine the pattern, if needed
        Ok(pat)
    }

    pub fn desugar_for_loop(&self, pos: Pos, ident: String, array: AST, body: AST) -> Result<Expr, Error> {
        let mut block = vec!();
        let mut cond_block = vec!();
        let mut body_block = vec!();

        let iter = format!("{}", NodeID::generate());
        let arrayname = format!("{}", NodeID::generate());

        let access_iter = || AST::Identifier(pos, iter.clone());
        let access_array = || AST::Identifier(pos, arrayname.clone());
        let access_array_field = |field: &str| AST::Accessor(pos, r(access_array()), field.to_string());
        let invoke = |func, args| AST::Invoke(pos, func, args);

        // define the array variable
        block.push(Expr::make_def(pos, Mutability::Mutable, arrayname.clone(), None, self.refine_node(array)?));

        // define the iterator index variable
        block.push(Expr::make_def(pos, Mutability::Mutable, iter.clone(), None, Expr::make_lit(Literal::Integer(0))));

        // compare if iterator index is < length of array
        cond_block.push(self.refine_node(invoke(r(AST::Identifier(pos, "<".to_string())),
            vec!(access_iter(), invoke(r(access_array_field("len")), vec!()))))?);

        // assign the next value to the item variable
        body_block.push(Expr::make_def(pos, Mutability::Immutable, ident, None,
            self.refine_node(invoke(r(access_array_field("get")), vec!(access_iter())))?));

        body_block.push(self.refine_node(body)?);

        // increment the iterator index variable
        body_block.push(Expr::make_assign(pos, self.refine_node(access_iter())?, self.refine_node(invoke(r(AST::Identifier(pos, "+".to_string())),
            vec!(access_iter(), AST::Literal(Literal::Integer(1)))))?, AssignType::Update));

        block.push(Expr::make_while(pos, Expr::make_block(pos, cond_block), Expr::make_block(pos, body_block)));
        Ok(Expr::make_block(pos, block))
    }

    pub fn desugar_class(&self, pos: Pos, classtype: Type, parenttype: Option<Type>, whereclause: WhereClause, body: Vec<AST>) -> Result<Expr, Error> {
        // Make sure constructors take "self" as the first argument, and return "self" at the end
        let mut newbody = vec!();
        for node in body {
            let node = match node {
                AST::Function(pos, vis, name, args, ret, mut body, abi, whereclause) => {
                    if name.as_deref() == Some("new") {
                        if args.len() > 0 && &args[0].name == "self" {
                            body.push(AST::Identifier(pos, "self".to_string()));
                        } else {
                            return Err(Error::new("SyntaxError: the \"new\" method on a class must have \"self\" as its first parameter".to_string()));
                        }
                    }
                    AST::Function(pos, vis, name, args, ret, body, abi, whereclause)
                },
                _ => node
            };
            newbody.push(node);
        }

        let body = self.with_context(CodeContext::ClassBody, || {
            self.refine_vec(newbody)
        });
        Ok(Expr::make_class(pos, classtype, parenttype, whereclause, body))
    }

    pub fn desugar_array(&self, pos: Pos, items: Vec<AST>) -> Result<Expr, Error> {
        let mut block = vec!();
        let tmparray = format!("{}", UniqueID::generate());

        block.push(AST::Definition(pos, Mutability::Immutable, tmparray.clone(), None,
            r(AST::New(pos, Type::Object("Array".to_string(), UniqueID(0), vec!(Type::Variable(UniqueID::generate()))), vec!(/*, AST::Integer(items.len() as isize)*/)))));
        for item in items {
            block.push(AST::Invoke(pos,
                r(AST::Accessor(pos, r(AST::Identifier(pos, tmparray.clone())), "push".to_string())),
                vec!(item)));
        }
        block.push(AST::Identifier(pos, tmparray.clone()));
        Ok(Expr::make_block(pos, self.refine_vec(block)))
    }

    pub fn desugar_trait_impl(&self, pos: Pos, traitname: String, impltype: Type, whereclause: WhereClause, body: Vec<AST>) -> Result<Expr, Error> {
        let mut trait_body = vec!();
        for node in body {
            match node {
                AST::Function(pos, _vis, name, args, ret, body, abi, whereclause) if name.is_some() => {
                    let vis = Visibility::Public;
                    let name = name.ok_or(Error::new("SyntaxError: anonymous functions are not allowed in trait implementations".to_string()))?;
                    trait_body.push(self.refine_node(AST::Function(pos, vis, Some(name), args, ret, body, abi, whereclause))?);
                },
                node => return Err(Error::new(format!("SyntaxError: only functions are allowed in trait impls, found {:?}", node))),
            }
        }
        Ok(Expr::make_trait_impl(pos, traitname, impltype, whereclause, trait_body))
    }
}

