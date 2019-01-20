
use std::cell::RefCell;

use rand;

use abi::ABI;
use types::Type;
use session::Session;
use misc::{ r, UniqueID };
//use hcode::{ HExpr };
use ast::{ NodeID, AST, Mutability, Visibility, Ident, ClassSpec, Argument, Pattern, Literal };


#[derive(Copy, Clone, Debug, PartialEq)]
enum CodeContext {
    Func,
    ClassBody,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Refinery<'sess> {
    pub session: &'sess Session,
    context: RefCell<Vec<CodeContext>>,
}

impl<'sess> Refinery<'sess> {
    pub fn refine(session: &'sess Session, code: Vec<AST>) -> Vec<AST> {
        let refinery = Refinery {
            session: session,
            context: RefCell::new(vec!()),    
        };

        //vec!(AST::make_func(Pos::empty(), Some(Ident::new(Pos::empty(), format!("init.{}", "test"))), vec!(), None,
        //    r(AST::make_block(Pos::empty(), refine_vec(code))),
        //ABI::Molten))
        refinery.refine_vec(code)
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


    pub fn refine_vec(&self, code: Vec<AST>) -> Vec<AST> {
        let mut block = vec!();
        for node in code {
            block.push(self.refine_node(node));
        }
        block
    }

    pub fn refine_node(&self, node: AST) -> AST {
        match node {
            AST::Block(id, pos, mut code) => {
                if code.len() == 0 {
                    code.push(AST::make_lit(Literal::Unit))
                }
                AST::Block(id, pos, self.refine_vec(code))
            },

            AST::Definition(id, pos, mutable, ident, ttype, code) => {
                AST::Definition(id, pos, mutable, ident, ttype, r(self.refine_node(*code)))
            },

            AST::Declare(id, pos, vis, ident, ttype) => {
                AST::Declare(id, pos, vis, ident, ttype)
            },

            AST::Function(id, pos, vis, ident, args, ret, body, abi) => {
                let vis = if self.top_level() {
                    Visibility::Public
                } else {
                    Visibility::Private
                };

                self.with_context(CodeContext::Func, || {
                    AST::Function(id, pos, vis, ident, args, ret, r(self.refine_node(*body)), abi)
                })
            },

            AST::Invoke(id, pos, fexpr, mut args) => {
                if let AST::Accessor(_, _, ref expr, _, _) = *fexpr {
                    args.insert(0, *expr.clone());
                }
                AST::Invoke(id, pos, r(self.refine_node(*fexpr)), self.refine_vec(args))
            },

            AST::SideEffect(id, pos, op, args) => {
                AST::SideEffect(id, pos, op, self.refine_vec(args))
            },

            AST::If(id, pos, cond, texpr, fexpr) => {
                AST::If(id, pos, r(self.refine_node(*cond)), r(self.refine_node(*texpr)), r(self.refine_node(*fexpr)))
            },

            AST::Match(id, pos, cond, cases, cid) => {
                let cases = cases.into_iter().map(move |(case, body)| ( self.refine_pattern(case), self.refine_node(body) )).collect();
                AST::Match(id, pos, r(self.refine_node(*cond)), cases, cid)
            },

            AST::Try(id, pos, cond, cases, cid) => {
                let cases = cases.into_iter().map(move |(case, body)| ( self.refine_pattern(case), self.refine_node(body) )).collect();
                AST::Try(id, pos, r(self.refine_node(*cond)), cases, cid)
            },

            AST::Raise(id, pos, expr) => {
                AST::Raise(id, pos, r(self.refine_node(*expr)))
            },

            AST::While(id, pos, cond, body) => {
                AST::While(id, pos, r(self.refine_node(*cond)), r(self.refine_node(*body)))
            },

            AST::For(id, pos, ident, list, body) => {
                let mut block = vec!();
                let mut cond_block = vec!();
                let mut body_block = vec!();

                let iter = format!("{}", NodeID::generate());
                let listname = format!("{}", NodeID::generate());

                let access_iter = || AST::make_ident(pos.clone(), Ident::new(iter.clone()));
                let access_item = || AST::make_ident(pos.clone(), ident.clone());
                let access_list = || AST::make_ident(pos.clone(), Ident::new(listname.clone()));
                let access_list_field = |field| AST::make_access(pos.clone(), access_list(), Ident::from_str(field));
                let invoke = |func, args| AST::make_invoke(pos.clone(), func, args);

                // define the list variable
                block.push(AST::make_def(pos.clone(), Mutability::Mutable, Ident::new(listname.clone()), None, self.refine_node(*list)));

                // define the iterator index variable
                block.push(AST::make_def(pos.clone(), Mutability::Mutable, Ident::new(iter.clone()), None, AST::make_lit(Literal::Integer(0))));

                // compare if iterator index is < length of list
                cond_block.push(self.refine_node(invoke(AST::make_ident(pos.clone(), Ident::from_str("<")),
                    vec!(access_iter(), invoke(access_list_field("len"), vec!())))));

                // assign the next value to the item variable
                body_block.push(AST::make_def(pos.clone(), Mutability::Immutable, ident.clone(), None,
                    self.refine_node(invoke(access_list_field("get"), vec!(access_iter())))));

                body_block.push(self.refine_node(*body));

                // increment the iterator index variable
                body_block.push(AST::make_assign(pos.clone(), access_iter(), self.refine_node(invoke(AST::make_ident(pos.clone(), Ident::from_str("+")),
                    vec!(access_iter(), AST::make_lit(Literal::Integer(1)))))));

                block.push(AST::While(id, pos.clone(), r(AST::make_block(pos.clone(), cond_block)), r(AST::make_block(pos.clone(), body_block))));
                AST::make_block(pos.clone(), block)
                //AST::For(id, pos, ident, r(self.refine_node(*cond)), r(self.refine_node(*body)))
            },

            AST::Ref(id, pos, expr) => { AST::Ref(id, pos, r(self.refine_node(*expr))) },
            AST::Deref(id, pos, expr) => { AST::Deref(id, pos, r(self.refine_node(*expr))) },

            AST::Tuple(id, pos, items) => { AST::Tuple(id, pos, self.refine_vec(items)) },

            AST::Record(id, pos, mut items) => {
                items.sort_unstable_by(|a, b| a.0.name.cmp(&b.0.name));
                AST::Record(id, pos, items.into_iter().map(|(i, e)| (i, self.refine_node(e))).collect())
            },

            //AST::List(id, pos, items, ttype) => { AST::List(id, pos, self.refine_vec(items), ttype) },
            AST::List(_, pos, items) => {
                let mut block = vec!();
                let tmplist = format!("{}", UniqueID::generate());
                let typevar = rand::random::<i32>();

                // TODO this makes lists immutable, which might not be what we want
                block.push(AST::make_def(pos.clone(), Mutability::Immutable, Ident::new(tmplist.clone()), None,
                    AST::make_invoke(pos.clone(),
                        AST::make_resolve(pos.clone(), AST::make_ident(pos.clone(), Ident::from_str("List")), Ident::from_str("new")),
                        vec!(
                            AST::make_new(pos.clone(), ClassSpec::new(pos.clone(), Ident::from_str("List"), vec!(Type::Variable(typevar.to_string(), UniqueID(0)))))
                            /*, AST::Integer(items.len() as isize)*/
                        ))));
                for item in items {
                    block.push(AST::make_invoke(pos.clone(),
                        AST::make_access(pos.clone(), AST::make_ident(pos.clone(), Ident::new(tmplist.clone())), Ident::from_str("push")),
                        vec!(item)));
                }
                block.push(AST::make_ident(pos.clone(), Ident::new(tmplist.clone())));
                AST::make_block(pos.clone(), self.refine_vec(block))
            },

            //AST::New(_, _, _) => { node },
            AST::New(id, pos, classspec) => {
                AST::make_invoke(pos.clone(),
                    AST::make_resolve(pos.clone(), AST::Identifier(NodeID::generate(), pos.clone(), classspec.ident.clone()), Ident::from_str("__init__")),
                    vec!(AST::New(id, pos.clone(), classspec)))
            },

            AST::Class(id, pos, classspec, parentspec, body) => {
                // Make sure constructors take "self" as the first argument, and return "self" at the end
                let mut has_new = false;
                let mut has_init = false;
                let mut body: Vec<AST> = body.into_iter().map(|node| {
                    match node {
                        AST::Function(id, pos, vis, ident, args, ret, mut body, abi) => {
                            if ident.as_ref().map(|i| i.name.as_str()) == Some("new") {
                                has_new = true;
                                if args.len() > 0 && args[0].ident.as_str() == "self" {
                                    body = r(AST::Block(NodeID::generate(), pos.clone(), vec!(*body, AST::Identifier(NodeID::generate(), pos.clone(), Ident::new(String::from("self"))))));
                                } else {
                                    panic!("SyntaxError: the \"new\" method on a class must have \"self\" as its first parameter");
                                }
                            }
                            ident.as_ref().map(|ref ident| if ident.as_str() == "__init__" { has_init = true; });
                            AST::Function(id, pos, vis, ident, args, ret, body, abi)
                        },
                        AST::Declare(id, pos, vis, ident, ttype) => {
                            if ident.as_str() == "new" {
                                has_new = true;
                            }
                            if ident.as_str() == "__init__" { has_init = true; }
                            AST::Declare(id, pos, vis, ident, ttype)
                        },
                        _ => node
                    }
                }).collect();
                if !has_new {
                    //body.insert(0, AST::Function(id, pos.clone(), Some(String::from("new")), vec!((String::from("self"), None, None)), None, r(AST::Identifier(id, pos.clone(), String::from("self"))), UniqueID::generate(), ABI::Molten));
                    //panic!("SyntaxError: you must declare a \"new\" method on a class");
                }

                // Create an __init__ function to initialize the fields of a newly created class object
                if !has_init {
                    let mut init = vec!();
                    for node in &body {
                        match node {
                            AST::Definition(_, _, _, ident, _, value) => {
                                init.push(AST::make_assign(pos.clone(),
                                    AST::make_access(pos.clone(), AST::make_ident_from_str(pos.clone(), "self"), ident.clone()),
                                    *value.clone()));
                            },
                            _ => { },
                        }
                    }

                    let initid = NodeID::generate();
                    let iargs = vec!(Argument::new(pos.clone(), Ident::from_str("self"), None, None));
                    if let Some(parentspec) = parentspec.as_ref() {
                        init.insert(0, AST::make_invoke(pos.clone(),
                            AST::make_resolve(pos.clone(), AST::make_ident(pos.clone(), parentspec.ident.clone()), Ident::from_str("__init__")),
                            vec!(AST::make_ident(pos.clone(), Ident::from_str("self")))));
                    }
                    init.push(AST::make_ident_from_str(pos.clone(), "self"));
                    let mut initcode = AST::Function(initid, pos.clone(), Visibility::Public, Some(Ident::from_str("__init__")), iargs, None, r(AST::make_block(pos.clone(), init)), ABI::Molten);
                    body.push(initcode);
                }

                let body = self.with_context(CodeContext::ClassBody, || {
                    self.refine_vec(body)
                });
                AST::Class(id, pos, classspec, parentspec, body)
            },

            AST::Index(id, pos, base, index) => {
                self.refine_node(AST::Invoke(id, pos.clone(), r(AST::Accessor(NodeID::generate(), pos.clone(), base, Ident::new(String::from("[]")), NodeID::generate())), vec!(*index)))
            },

            AST::Resolver(id, pos, left, right, oid) => {
                // TODO should this also allow a non-type specifier?
                match *left {
                    AST::Identifier(_, _, _) => { },
                    _ => panic!("SyntaxError: left-hand side of scope resolver must be identifier")
                }
                AST::Resolver(id, pos, r(self.refine_node(*left)), right, oid)
            },

            AST::Accessor(id, pos, left, right, oid) => {
                AST::Accessor(id, pos, r(self.refine_node(*left)), right, oid)
            },

            AST::Assignment(id, pos, left, right) => {
                let left = *left;
                match left {
                    //AST::Identifier(_, _, _) |
                    AST::Deref(_, _, _) |
                    AST::Accessor(_, _, _, _, _) => {
                        AST::Assignment(id, pos, r(self.refine_node(left)), r(self.refine_node(*right)))
                    },
                    AST::Index(iid, ipos, base, index) => {
                        self.refine_node(AST::Invoke(id, pos, r(AST::Accessor(iid, ipos.clone(), base, Ident::new(String::from("[]")), NodeID::generate())), vec!(*index, *right)))
                    },
                    _ => panic!("SyntaxError: assignment to to an invalid element: {:?}", left),
                }
            },

            AST::Import(_, _, _, _) => { node },
            //AST::Import(id, pos, ident, _) => {
            //    let path = ident.replace(".", "/") + ".dec";
            //    let decls = session.parse_file(path.as_str());
            //    AST::Import(id, pos, ident, self.refine_vec(decls))
            //},

            AST::PtrCast(ttype, value) => {
                AST::PtrCast(ttype, r(self.refine_node(*value)))
            },

            AST::GetValue(_) => { node },
            AST::Literal(_, _) => { node },
            AST::Nil(_) => { node },
            AST::Identifier(_, _, _) => { node },
            AST::TypeAlias(_, _, _, _) => { node },

            //node @ _ => { node }
        }
    }

    pub fn refine_pattern(&self, pat: Pattern) -> Pattern {
        // TODO refine the pattern, if needed
        pat
    }
}

