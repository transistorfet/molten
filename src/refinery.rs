

use import;
use parser::AST;
use utils::UniqueID;

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

        AST::Definition(pos, (name, ttype), code) => {
            AST::Definition(pos, (name, ttype), Box::new(refine_node(*code)))
        },

        AST::Declare(pos, name, ttype) => {
            AST::Declare(pos, name, ttype)
        },

        AST::Function(pos, name, args, ret, body, id) => {
            AST::Function(pos, name, args, ret, Box::new(refine_node(*body)), id)
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

        AST::Match(pos, cond, cases) => {
            let cases = cases.into_iter().map(move |(case, body)| ( refine_node(case), refine_node(body) )).collect();
            AST::Match(pos, Box::new(refine_node(*cond)), cases)
        },

        AST::Try(pos, cond, cases) => {
            let cases = cases.into_iter().map(move |(case, body)| ( refine_node(case), refine_node(body) )).collect();
            AST::Try(pos, Box::new(refine_node(*cond)), cases)
        },

        AST::Raise(pos, expr) => {
            AST::Raise(pos, Box::new(refine_node(*expr)))
        },

        AST::While(pos, cond, body) => {
            AST::While(pos, Box::new(refine_node(*cond)), Box::new(refine_node(*body)))
        },

        AST::For(pos, name, cond, body, id) => {
            AST::For(pos, name, Box::new(refine_node(*cond)), Box::new(refine_node(*body)), id)
        },

        AST::List(pos, code) => { AST::List(pos, refine_vec(code)) },
        // TODO this almost works, but it needs to reference the list that's created, which it can't because blocks don't create their own scope, and a list variable would collide
        //AST::List(code) => {
        //    let mut block = vec!();
        //    block.push(AST::Invoke(Box::new(AST::Resolver(Box::new(AST::Identifier(format!("List"))), String::from("new"))), vec!(AST::Integer(code.len() as isize)), None));
        //    for item in code {
        //        block.push(AST::Invoke(Box::new(AST::Resolver(Box::new(AST::Identifier(format!("List"))), String::from("push"))), vec!(item), None));
        //    }
        //    AST::Block(refine_vec(block))
        //},

        AST::Class(pos, pair, parent, body, id) => {
            // Make sure constructors take "self" as the first argument, and return "self" at the end
            let mut has_new = false;
            let mut body: Vec<AST> = body.into_iter().map(|node| {
                match node {
                    AST::Function(pos, name, args, ret, mut body, id) => {
                        if name == Some(String::from("new")) {
                            has_new = true;
                            if args.len() > 0 && args[0].0 == String::from("self") {
                                body = Box::new(AST::Block(pos.clone(), vec!(*body, AST::Identifier(pos.clone(), String::from("self")))));
                            } else {
                                panic!("SyntaxError: the \"new\" method on a class must have \"self\" as its first parameter");
                            }
                        }
                        AST::Function(pos, name, args, ret, body, id)
                    },
                    _ => node
                }
            }).collect();
            if !has_new {
                body.insert(0, AST::Function(pos.clone(), Some(String::from("new")), vec!((String::from("self"), None, None)), None, Box::new(AST::Identifier(pos.clone(), String::from("self"))), UniqueID::generate()));
                //panic!("SyntaxError: you must declare a \"new\" method on a class");
            }
            AST::Class(pos, pair, parent, refine_vec(body), id)
        },

        AST::Index(pos, base, index, _) => {
            //AST::Index(pos, Box::new(refine_node(*base)), Box::new(refine_node(*index)), stype)
            refine_node(AST::Invoke(pos.clone(), Box::new(AST::Accessor(pos, base, String::from("[]"), None)), vec!(*index), None))
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
                    refine_node(AST::Invoke(pos, Box::new(AST::Accessor(ipos, base, String::from("[]"), None)), vec!(*index, *right), None))
                },
                _ => panic!("SyntaxError: assignment to something other than a list or class element: {:?}", left),
            }
            //AST::Assignment(Box::new(refine_node(*left)), Box::new(refine_node(*right)))
        },

        AST::Import(pos, name, _) => {
            let path = name.replace(".", "/") + ".dec";
            let decls = import::load_index(path.as_str());
            //*decls = session.borrow_mut().load_index(path.as_str());
            AST::Import(pos, name, decls)
        },

        AST::Noop => { node },
        AST::Underscore => { node },
        AST::Boolean(_) => { node },
        AST::Integer(_) => { node },
        AST::Real(_) => { node },
        AST::String(_) => { node },
        AST::Nil(_) => { node },
        AST::Identifier(_, _) => { node },
        AST::New(_, _) => { node },
        AST::Type(_, _, _) => { node },

        //node @ _ => { node }
    }
}

