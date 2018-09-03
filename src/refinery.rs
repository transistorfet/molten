
use rand;

use abi::ABI;
use types::Type;
use utils::UniqueID;
use ast::{ NodeID, Pos, AST, Ident, ClassSpec };

pub fn refine(code: Vec<AST>) -> Vec<AST> {
    //vec!(AST::make_func(Pos::empty(), Some(Ident::new(Pos::empty(), format!("init.{}", "test"))), vec!(), None,
    //    Box::new(AST::make_block(Pos::empty(), refine_vec(code))),
    //ABI::Molten))
    refine_vec(code)
}

pub fn refine_vec(code: Vec<AST>) -> Vec<AST> {
    let mut block = vec!();
    for node in code {
        block.push(refine_node(node));
    }
    block
}

pub fn refine_node(node: AST) -> AST {
    match node {
        AST::Block(id, pos, mut code) => {
            if code.len() == 0 {
                code.push(AST::make_nil())
            }
            AST::Block(id, pos, refine_vec(code))
        },

        AST::Definition(id, pos, ident, ttype, code) => {
            AST::Definition(id, pos, ident, ttype, Box::new(refine_node(*code)))
        },

        AST::Declare(id, pos, ident, ttype) => {
            AST::Declare(id, pos, ident, ttype)
        },

        AST::Function(id, pos, ident, args, ret, body, abi) => {
            AST::Function(id, pos, ident, args, ret, Box::new(refine_node(*body)), abi)
        },

        AST::Invoke(id, pos, fexpr, mut args) => {
            if let AST::Accessor(_, _, ref expr, _, _) = *fexpr {
                args.insert(0, *expr.clone());
            }
            AST::Invoke(id, pos, Box::new(refine_node(*fexpr)), refine_vec(args))
        },

        AST::SideEffect(id, pos, op, args) => {
            AST::SideEffect(id, pos, op, refine_vec(args))
        },

        AST::If(id, pos, cond, texpr, fexpr) => {
            AST::If(id, pos, Box::new(refine_node(*cond)), Box::new(refine_node(*texpr)), Box::new(refine_node(*fexpr)))
        },

        AST::Match(id, pos, cond, cases, cid) => {
            let cases = cases.into_iter().map(move |(case, body)| ( refine_node(case), refine_node(body) )).collect();
            AST::Match(id, pos, Box::new(refine_node(*cond)), cases, cid)
        },

        AST::Try(id, pos, cond, cases, cid) => {
            let cases = cases.into_iter().map(move |(case, body)| ( refine_node(case), refine_node(body) )).collect();
            AST::Try(id, pos, Box::new(refine_node(*cond)), cases, cid)
        },

        AST::Raise(id, pos, expr) => {
            AST::Raise(id, pos, Box::new(refine_node(*expr)))
        },

        AST::While(id, pos, cond, body) => {
            AST::While(id, pos, Box::new(refine_node(*cond)), Box::new(refine_node(*body)))
        },

        AST::For(id, pos, ident, cond, body) => {
            AST::For(id, pos, ident, Box::new(refine_node(*cond)), Box::new(refine_node(*body)))
        },

        AST::Tuple(id, pos, code) => { AST::Tuple(id, pos, refine_vec(code)) },
        //AST::List(id, pos, code, ttype) => { AST::List(id, pos, refine_vec(code), ttype) },
        AST::List(id, pos, code) => {
            let mut block = vec!();
            let tmplist = format!("{}", UniqueID::generate());
            let typevar = rand::random::<i32>();

            block.push(AST::make_def(pos.clone(), Ident::new(pos.clone(), tmplist.clone()), None,
                Box::new(AST::make_invoke(pos.clone(),
                    Box::new(AST::make_resolve(pos.clone(), Box::new(AST::make_ident(pos.clone(), Ident::from_str("List"))), Ident::from_str("new"))),
                    vec!(
                        AST::make_new(pos.clone(), ClassSpec::new(pos.clone(), Ident::from_str("List"), vec!(Type::Variable(typevar.to_string(), UniqueID(0)))))
                        /*, AST::Integer(code.len() as isize)*/
                    )))));
            for item in code {
                block.push(AST::make_invoke(pos.clone(),
                    Box::new(AST::make_access(pos.clone(), Box::new(AST::make_ident(pos.clone(), Ident::new(pos.clone(), tmplist.clone()))), Ident::from_str("push"))),
                    vec!(item)));
            }
            block.push(AST::make_ident(pos.clone(), Ident::new(pos.clone(), tmplist.clone())));
            AST::make_block(pos.clone(), refine_vec(block))
        },

        AST::Class(id, pos, classspec, parentspec, body) => {
            // Make sure constructors take "self" as the first argument, and return "self" at the end
            let mut has_new = false;
            let mut body: Vec<AST> = body.into_iter().map(|node| {
                match node {
                    AST::Function(id, pos, ident, args, ret, mut body, abi) => {
                        if ident.as_ref().map(|i| i.name.as_str()) == Some("new") {
                            has_new = true;
                            if args.len() > 0 && args[0].ident.as_str() == "self" {
                                body = Box::new(AST::Block(NodeID::generate(), pos.clone(), vec!(*body, AST::Identifier(NodeID::generate(), pos.clone(), Ident::new(pos.clone(), String::from("self"))))));
                            } else {
                                panic!("SyntaxError: the \"new\" method on a class must have \"self\" as its first parameter");
                            }
                        }
                        AST::Function(id, pos, ident, args, ret, body, abi)
                    },
                    AST::Declare(id, pos, ident, ttype) => {
                        if ident.name == String::from("new") {
                            has_new = true;
                        }
                        AST::Declare(id, pos, ident, ttype)
                    },
                    _ => node
                }
            }).collect();
            if !has_new {
                //body.insert(0, AST::Function(id, pos.clone(), Some(String::from("new")), vec!((String::from("self"), None, None)), None, Box::new(AST::Identifier(id, pos.clone(), String::from("self"))), UniqueID::generate(), ABI::Molten));
                //panic!("SyntaxError: you must declare a \"new\" method on a class");
            }
            AST::Class(id, pos, classspec, parentspec, refine_vec(body))
        },

        AST::Index(id, pos, base, index) => {
            //AST::Index(id, pos, Box::new(refine_node(*base)), Box::new(refine_node(*index)), stype)
            refine_node(AST::Invoke(id, pos.clone(), Box::new(AST::Accessor(NodeID::generate(), pos.clone(), base, Ident::new(pos.clone(), String::from("[]")), NodeID::generate())), vec!(*index)))
        },

        AST::Resolver(id, pos, left, right) => {
            // TODO should this also allow a non-type specifier?
            match *left {
                AST::Identifier(_, _, _) => { },
                _ => panic!("SyntaxError: left-hand side of scope resolver must be identifier")
            }
            AST::Resolver(id, pos, Box::new(refine_node(*left)), right)
        },

        AST::Accessor(id, pos, left, right, oid) => {
            AST::Accessor(id, pos, Box::new(refine_node(*left)), right, oid)
        },

        AST::Assignment(id, pos, left, right) => {
            let left = *left;
            match left {
                AST::Accessor(_, _, _, _, _) => {
                    AST::Assignment(id, pos, Box::new(refine_node(left)), Box::new(refine_node(*right)))
                },
                AST::Index(iid, ipos, base, index) => {
                    refine_node(AST::Invoke(id, pos, Box::new(AST::Accessor(iid, ipos.clone(), base, Ident::new(ipos.clone(), String::from("[]")), NodeID::generate())), vec!(*index, *right)))
                },
                _ => panic!("SyntaxError: assignment to something other than a list or class element: {:?}", left),
            }
            //AST::Assignment(Box::new(refine_node(*left)), Box::new(refine_node(*right)))
        },

        AST::Import(_, _, _, _) => { node },
        //AST::Import(id, pos, ident, _) => {
        //    let path = ident.replace(".", "/") + ".dec";
        //    let decls = session.parse_file(path.as_str());
        //    AST::Import(id, pos, ident, refine_vec(decls))
        //},

        AST::Underscore => { node },
        AST::Literal(_, _) => { node },
        AST::Nil(_) => { node },
        AST::PtrCast(_, _) => { node },
        AST::Recall(_, _) => { node },
        AST::Identifier(_, _, _) => { node },
        AST::New(_, _, _) => { node },
        AST::TypeDef(_, _, _, _) => { node },

        //node @ _ => { node }
    }
}

