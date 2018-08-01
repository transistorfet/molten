

use abi::ABI;
use types::Type;
use utils::UniqueID;
use ast::{ AST, Ident };

pub fn refine(code: Vec<AST>) -> Vec<AST> {
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
        AST::Block(pos, mut code) => {
            if code.len() == 0 {
                code.push(AST::Nil(None))
            }
            AST::Block(pos, refine_vec(code))
        },

        AST::Definition(pos, (dpos, ident, ttype), code) => {
            AST::Definition(pos, (dpos, ident, ttype), Box::new(refine_node(*code)))
        },

        AST::Declare(pos, ident, ttype) => {
            AST::Declare(pos, ident, ttype)
        },

        AST::Function(pos, ident, args, ret, body, id, abi) => {
            AST::Function(pos, ident, args, ret, Box::new(refine_node(*body)), id, abi)
        },

        AST::Invoke(pos, fexpr, mut args, ttype) => {
            if let AST::Accessor(_, ref expr, _, _) = *fexpr {
                args.insert(0, *expr.clone());
            }
            AST::Invoke(pos, Box::new(refine_node(*fexpr)), refine_vec(args), ttype)
        },

        AST::SideEffect(pos, op, args) => {
            AST::SideEffect(pos, op, refine_vec(args))
        },

        AST::If(pos, cond, texpr, fexpr) => {
            AST::If(pos, Box::new(refine_node(*cond)), Box::new(refine_node(*texpr)), Box::new(refine_node(*fexpr)))
        },

        AST::Match(pos, cond, cases, condtype) => {
            let cases = cases.into_iter().map(move |(case, body)| ( refine_node(case), refine_node(body) )).collect();
            AST::Match(pos, Box::new(refine_node(*cond)), cases, condtype)
        },

        AST::Try(pos, cond, cases, condtype) => {
            let cases = cases.into_iter().map(move |(case, body)| ( refine_node(case), refine_node(body) )).collect();
            AST::Try(pos, Box::new(refine_node(*cond)), cases, condtype)
        },

        AST::Raise(pos, expr) => {
            AST::Raise(pos, Box::new(refine_node(*expr)))
        },

        AST::While(pos, cond, body) => {
            AST::While(pos, Box::new(refine_node(*cond)), Box::new(refine_node(*body)))
        },

        AST::For(pos, ident, cond, body, id) => {
            AST::For(pos, ident, Box::new(refine_node(*cond)), Box::new(refine_node(*body)), id)
        },

        AST::List(pos, code, ttype) => { AST::List(pos, refine_vec(code), ttype) },

        AST::Class(pos, pair, parent, body, id) => {
            // Make sure constructors take "self" as the first argument, and return "self" at the end
            let mut has_new = false;
            let mut body: Vec<AST> = body.into_iter().map(|node| {
                match node {
                    AST::Function(pos, ident, args, ret, mut body, id, abi) => {
                        if ident.as_ref().map(|i| i.name.as_str()) == Some("new") {
                            has_new = true;
                            if args.len() > 0 && args[0].ident.as_str() == "self" {
                                body = Box::new(AST::Block(pos.clone(), vec!(*body, AST::Identifier(pos.clone(), Ident::new(pos.clone(), String::from("self"))))));
                            } else {
                                panic!("SyntaxError: the \"new\" method on a class must have \"self\" as its first parameter");
                            }
                        }
                        AST::Function(pos, ident, args, ret, body, id, abi)
                    },
                    AST::Declare(pos, ident, ttype) => {
                        if ident.name == String::from("new") {
                            has_new = true;
                        }
                        AST::Declare(pos, ident, ttype)
                    },
                    _ => node
                }
            }).collect();
            if !has_new {
                //body.insert(0, AST::Function(pos.clone(), Some(String::from("new")), vec!((String::from("self"), None, None)), None, Box::new(AST::Identifier(pos.clone(), String::from("self"))), UniqueID::generate(), ABI::Molten));
                //panic!("SyntaxError: you must declare a \"new\" method on a class");
            }
            AST::Class(pos, pair, parent, refine_vec(body), id)
        },

        AST::Index(pos, base, index, _) => {
            //AST::Index(pos, Box::new(refine_node(*base)), Box::new(refine_node(*index)), stype)
            refine_node(AST::Invoke(pos.clone(), Box::new(AST::Accessor(pos.clone(), base, Ident::new(pos.clone(), String::from("[]")), None)), vec!(*index), None))
        },

        AST::Resolver(pos, left, right) => {
            // TODO should this also allow a non-type specifier?
            match *left {
                AST::Identifier(_, _) => { },
                _ => panic!("SyntaxError: left-hand side of scope resolver must be identifier")
            }
            AST::Resolver(pos, Box::new(refine_node(*left)), right)
        },

        AST::Accessor(pos, left, right, stype) => {
            AST::Accessor(pos, Box::new(refine_node(*left)), right, stype)
        },

        AST::Assignment(pos, left, right) => {
            let left = *left;
            match left {
                AST::Accessor(_, _, _, _) => {
                    AST::Assignment(pos, Box::new(refine_node(left)), Box::new(refine_node(*right)))
                },
                AST::Index(ipos, base, index, _) => {
                    refine_node(AST::Invoke(pos, Box::new(AST::Accessor(ipos.clone(), base, Ident::new(ipos.clone(), String::from("[]")), None)), vec!(*index, *right), None))
                },
                _ => panic!("SyntaxError: assignment to something other than a list or class element: {:?}", left),
            }
            //AST::Assignment(Box::new(refine_node(*left)), Box::new(refine_node(*right)))
        },

        AST::Import(_, _, _) => { node },
        //AST::Import(pos, ident, _) => {
        //    let path = ident.replace(".", "/") + ".dec";
        //    let decls = import::load_index(path.as_str());
        //    //let decls = session.parse_file(path.as_str());
        //    AST::Import(pos, ident, refine_vec(decls))
        //},

        AST::Noop => { node },
        AST::Underscore => { node },
        AST::Boolean(_) => { node },
        AST::Integer(_) => { node },
        AST::Real(_) => { node },
        AST::String(_) => { node },
        AST::Nil(_) => { node },
        AST::PtrCast(_, _) => { node },
        AST::Recall(_, _) => { node },
        AST::Identifier(_, _) => { node },
        AST::New(_, _) => { node },
        AST::Type(_, _, _) => { node },

        //node @ _ => { node }
    }
}

