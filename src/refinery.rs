
use std::cell::RefCell;

use abi::ABI;
use types::Type;
use ast::{ Pos, AST };
use misc::{ r, UniqueID };
use session::{ Session, Error };
use hir::{ NodeID, Visibility, Mutability, AssignType, Literal, Ident, Argument, ClassSpec, MatchCase, EnumVariant, Pattern, Expr, ExprKind };


#[derive(Copy, Clone, Debug, PartialEq)]
enum CodeContext {
    Func(ABI),
    ClassBody,
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
        //let mut body = self.refine_vec(code);
        //body.push(Expr::make_lit(Literal::Boolean(true)));

        let module_name = self.session.name.replace(".", "_");
        let memo = format!("memo.{}", module_name);

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

        vec!(Expr::make_module(module_name, body))
    }

    pub fn refine_vec(&self, code: Vec<AST>) -> Vec<Expr> {
        let mut block = vec!();
        for node in code {
            let pos = node.get_pos();
            match self.refine_node(node) {
                Ok(refined) => block.push(refined),
                Err(err) => self.session.print_error(&err.add_pos(&pos)),
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

            AST::Declare(pos, vis, ident, ttype) => {
                Expr::make_decl(pos, vis, ident, ttype)
            },

            AST::Function(pos, _vis, ident, args, ret, body, abi) => {
                // TODO visibility is forced here so I don't have to add 'pub' keywords yet
                let vis = if self.top_level() {
                    Visibility::Public
                } else {
                    Visibility::Private
                };

                self.with_context(CodeContext::Func(abi), || {
                    Ok(Expr::make_func(pos, vis, ident, args, ret, self.refine_vec(body), abi))
                })?
            },

            AST::Invoke(pos, fexpr, args) => {
                let fexpr = self.refine_node(*fexpr)?;
                let mut args = self.refine_vec(args);

                // We refine fexpr before cloning here, so that the IDs will be identical (otherwise typechecking wont unify the accessed object and first argument)
                if let ExprKind::Accessor(expr, field, _oid) = fexpr.kind {
                    let tmpname = format!("{}", UniqueID::generate());
                    args.insert(0, Expr::make_ident_from_str(pos.clone(), &tmpname));

                    Expr::make_block(pos.clone(), vec!(
                        Expr::make_def(pos.clone(), Mutability::Immutable, Ident::from_str(&tmpname), None, *expr), 
                        Expr::make_invoke(pos.clone(), Expr::make_access(pos.clone(), Expr::make_ident_from_str(pos.clone(), &tmpname), field), args),
                    ))
                } else {
                    Expr::make_invoke(pos, fexpr, args)
                }
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
                        return Err(Error::new(format!("SyntaxError: raise keyword cannot appear in this context"))),
                    _ => { },
                }
                Expr::make_raise(pos, self.refine_node(*expr)?)
            },

            AST::While(pos, cond, body) => {
                Expr::make_while(pos, self.refine_node(*cond)?, self.refine_node(*body)?)
            },

            AST::For(pos, ident, list, body) => {
                self.desugar_for_loop(pos, ident, *list, *body)?
            },

            AST::Ref(pos, expr) => { Expr::make_ref(pos, self.refine_node(*expr)?) },
            AST::Deref(pos, expr) => { Expr::make_deref(pos, self.refine_node(*expr)?) },

            AST::Tuple(pos, items) => { Expr::make_tuple(pos, self.refine_vec(items)) },

            AST::Record(pos, mut items) => {
                items.sort_unstable_by(|a, b| a.0.name.cmp(&b.0.name));
                let mut refined = vec!();
                for (i, e) in items {
                    refined.push((i, self.refine_node(e)?));
                }
                Expr::make_record(pos, refined)
            },

            AST::RecordUpdate(pos, record, mut items) => {
                items.sort_unstable_by(|a, b| a.0.name.cmp(&b.0.name));
                let mut refined = vec!();
                for (i, e) in items {
                    refined.push((i, self.refine_node(e)?));
                }
                Expr::make_record_update(pos, self.refine_node(*record)?, refined)
            },

            AST::Enum(pos, classspec, variants) => {
                let variants = variants.into_iter().map(|(pos, ident, ttype)| EnumVariant::new(pos, ident, ttype)).collect();
                Expr::make_enum(pos, classspec, variants)
            },

            AST::List(pos, items) => {
                self.desugar_list(pos, items)?
            },

            AST::New(pos, classspec, args) => {
                let ttype = Type::from(&classspec);

                let object =
                    Expr::make_invoke(pos.clone(), Expr::make_resolve_ident(pos.clone(), &classspec.ident, "__init__"),
                        vec!(Expr::make_alloc_object(pos.clone(), ttype.clone())));

                let mut args = args.into_iter().map(|arg| self.refine_node(arg)).collect::<Result<Vec<_>, _>>()?;
                args.insert(0, object);
                Expr::make_annotation(ttype.clone(),
                    Expr::make_invoke(pos.clone(), Expr::make_resolve_ident(pos.clone(), &classspec.ident, "new"), args))
            },

            AST::Class(pos, classspec, parentspec, body) => {
                self.desugar_class(pos, classspec, parentspec, body)?
            },

            AST::Index(pos, base, index) => {
                self.refine_node(AST::Invoke(pos.clone(), r(AST::Accessor(pos.clone(), base, Ident::from_str("[]"))), vec!(*index)))?
            },

            AST::Resolver(pos, left, right) => {
                // TODO should this also allow a non-type specifier?
                match *left {
                    AST::Identifier(_, _) => { },
                    _ => return Err(Error::new(format!("SyntaxError: left-hand side of scope resolver must be identifier"))),
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
                    AST::Deref(_, _) |
                    AST::Accessor(_, _, _) => {
                        Expr::make_assign(pos, self.refine_node(left)?, self.refine_node(*right)?, ty)
                    },
                    AST::Index(ipos, base, index) => {
                        self.refine_node(AST::Invoke(pos, r(AST::Accessor(ipos.clone(), base, Ident::from_str("[]"))), vec!(*index, *right)))?
                    },
                    _ => return Err(Error::new(format!("SyntaxError: assignment to to an invalid element: {:?}", left))),
                }
            },

            AST::Import(pos, ident, _) => {
                let path = ident.name.replace(".", "/") + ".dec";
                let ast = self.session.parse_file(path.as_str(), true);
                let decls = self.refine_vec(ast);
                Expr::make_import(pos, ident, decls)
            },

            AST::Annotation(ttype, value) => {
                Expr::make_annotation(ttype, self.refine_node(*value)?)
            },

            AST::Nil => { Expr::make_nil() },
            AST::Literal(val) => { Expr::make_lit(val) },
            AST::Identifier(pos, ident) => { Expr::make_ident(pos, ident) },
            AST::TypeAlias(pos, classspec, ttype) => { Expr::make_type_alias(pos, classspec, ttype) },
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

    pub fn desugar_for_loop(&self, pos: Pos, ident: Ident, list: AST, body: AST) -> Result<Expr, Error> {
        let mut block = vec!();
        let mut cond_block = vec!();
        let mut body_block = vec!();

        let iter = format!("{}", NodeID::generate());
        let listname = format!("{}", NodeID::generate());

        let access_iter = || AST::Identifier(pos.clone(), Ident::new(iter.clone()));
        let access_list = || AST::Identifier(pos.clone(), Ident::new(listname.clone()));
        let access_list_field = |field| AST::Accessor(pos.clone(), r(access_list()), Ident::from_str(field));
        let invoke = |func, args| AST::Invoke(pos.clone(), func, args);

        // define the list variable
        block.push(Expr::make_def(pos.clone(), Mutability::Mutable, Ident::new(listname.clone()), None, self.refine_node(list)?));

        // define the iterator index variable
        block.push(Expr::make_def(pos.clone(), Mutability::Mutable, Ident::new(iter.clone()), None, Expr::make_lit(Literal::Integer(0))));

        // compare if iterator index is < length of list
        cond_block.push(self.refine_node(invoke(r(AST::Identifier(pos.clone(), Ident::from_str("<"))),
            vec!(access_iter(), invoke(r(access_list_field("len")), vec!()))))?);

        // assign the next value to the item variable
        body_block.push(Expr::make_def(pos.clone(), Mutability::Immutable, ident.clone(), None,
            self.refine_node(invoke(r(access_list_field("get")), vec!(access_iter())))?));

        body_block.push(self.refine_node(body)?);

        // increment the iterator index variable
        body_block.push(Expr::make_assign(pos.clone(), self.refine_node(access_iter())?, self.refine_node(invoke(r(AST::Identifier(pos.clone(), Ident::from_str("+"))),
            vec!(access_iter(), AST::Literal(Literal::Integer(1)))))?, AssignType::Update));

        block.push(Expr::make_while(pos.clone(), Expr::make_block(pos.clone(), cond_block), Expr::make_block(pos.clone(), body_block)));
        Ok(Expr::make_block(pos.clone(), block))
    }

    pub fn desugar_class(&self, pos: Pos, classspec: ClassSpec, parentspec: Option<ClassSpec>, body: Vec<AST>) -> Result<Expr, Error> {
        // Make sure constructors take "self" as the first argument, and return "self" at the end
        let mut has_new = false;
        let mut has_init = false;
        let mut newbody = vec!();
        for node in body {
            let node = match node {
                AST::Function(pos, vis, ident, args, ret, mut body, abi) => {
                    if ident.as_ref().map(|i| i.name.as_str()) == Some("new") {
                        has_new = true;
                        if args.len() > 0 && args[0].ident.as_str() == "self" {
                            body.push(AST::Identifier(pos.clone(), Ident::from_str("self")));
                        } else {
                            return Err(Error::new(format!("SyntaxError: the \"new\" method on a class must have \"self\" as its first parameter")));
                        }
                    }
                    ident.as_ref().map(|ref ident| if ident.as_str() == "__init__" { has_init = true; });
                    AST::Function(pos, vis, ident, args, ret, body, abi)
                },
                AST::Declare(pos, vis, ident, ttype) => {
                    if ident.as_str() == "new" {
                        has_new = true;
                    }
                    if ident.as_str() == "__init__" { has_init = true; }
                    AST::Declare(pos, vis, ident, ttype)
                },
                _ => node
            };
            newbody.push(node);
        }
        if !has_new {
            //newbody.insert(0, Expr::new(pos.clone(), ExprKind::Function(Some(String::from("new")), vec!((String::from("self"), None, None)), None, r(AST::Identifier(id, pos.clone(), String::from("self"))), UniqueID::generate(), ABI::Molten)));
            //return Err(Error::new(format!("SyntaxError: you must declare a \"new\" method on a class")));
        }

        // Create an __init__ function to initialize the fields of a newly created class object
        if !has_init {
            let mut init = vec!();
            for node in &newbody {
                match node {
                    AST::Definition(_, _, ident, _, value) => {
                        init.push(AST::Assignment(pos.clone(),
                            r(AST::Accessor(pos.clone(), r(AST::make_ident_from_str(pos.clone(), "self")), ident.clone())),
                            r(*value.clone()),
                            AssignType::Initialize));
                    },
                    _ => { },
                }
            }

            let iargs = vec!(Argument::new(pos.clone(), Ident::from_str("self"), None, None));
            if let Some(parentspec) = parentspec.as_ref() {
                init.insert(0, AST::Invoke(pos.clone(),
                    r(AST::make_resolve_ident(pos.clone(), parentspec.ident.clone(), "__init__")),
                    vec!(AST::Identifier(pos.clone(), Ident::from_str("self")))));
            }
            init.push(AST::make_ident_from_str(pos.clone(), "self"));
            let initcode = AST::Function(pos.clone(), Visibility::Public, Some(Ident::from_str("__init__")), iargs, None, init, ABI::Molten);
            newbody.push(initcode);
        }

        let body = self.with_context(CodeContext::ClassBody, || {
            self.refine_vec(newbody)
        });
        Ok(Expr::make_class(pos, classspec, parentspec, body))
    }

    pub fn desugar_list(&self, pos: Pos, items: Vec<AST>) -> Result<Expr, Error> {
        let mut block = vec!();
        let tmplist = format!("{}", UniqueID::generate());

        // TODO this makes lists immutable, which might not be what we want
        block.push(AST::Definition(pos.clone(), Mutability::Immutable, Ident::new(tmplist.clone()), None,
            r(AST::New(pos.clone(), ClassSpec::new(pos.clone(), Ident::from_str("List"), vec!(Type::Variable(UniqueID::generate()))), vec!(/*, AST::Integer(items.len() as isize)*/)))));
        for item in items {
            block.push(AST::Invoke(pos.clone(),
                r(AST::Accessor(pos.clone(), r(AST::Identifier(pos.clone(), Ident::new(tmplist.clone()))), Ident::from_str("push"))),
                vec!(item)));
        }
        block.push(AST::Identifier(pos.clone(), Ident::new(tmplist.clone())));
        Ok(Expr::make_block(pos.clone(), self.refine_vec(block)))
    }
}

