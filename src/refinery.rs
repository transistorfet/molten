

use parser::AST;

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
        AST::Block(code) => { AST::Block(refine_vec(code)) },

        AST::Definition((name, ttype), code) => {
            AST::Definition((name, ttype), Box::new(refine_node(*code)))
        },

        AST::Function(name, args, ret, body, id) => {
            AST::Function(name, args, ret, Box::new(refine_node(*body)), id)
        },

        AST::Invoke(fexpr, mut args, ttype) => {
            if let AST::Accessor(ref expr, _, _) = *fexpr {
                args.insert(0, *expr.clone());
            }
            AST::Invoke(Box::new(refine_node(*fexpr)), refine_vec(args), ttype)
        },

        /*
        AST::Infix(_, _, _) => {
            let old = mem::replace(node, AST::Noop);
            if let AST::Infix(name, left, right) = old {
                mem::replace(node, AST::Invoke(Box::new(AST::Identifier(name)), vec!(*left, *right), None));
                println!("STUFF: {:?}", node);
            } else {panic!("FUGK"); }
        },
        */

        AST::SideEffect(op, args) => {
            AST::SideEffect(op, refine_vec(args))
        },

        AST::If(cond, texpr, fexpr) => {
            AST::If(Box::new(refine_node(*cond)), Box::new(refine_node(*texpr)), Box::new(refine_node(*fexpr)))
        },

        AST::Match(cond, cases) => {
            let cases = cases.into_iter().map(move |(case, body)| ( refine_node(case), refine_node(body) )).collect();
            AST::Match(Box::new(refine_node(*cond)), cases)
        },

        AST::Try(cond, cases) => {
            let cases = cases.into_iter().map(move |(case, body)| ( refine_node(case), refine_node(body) )).collect();
            AST::Try(Box::new(refine_node(*cond)), cases)
        },

        AST::Raise(expr) => {
            AST::Raise(Box::new(refine_node(*expr)))
        },

        AST::While(cond, body) => {
            AST::While(Box::new(refine_node(*cond)), Box::new(refine_node(*body)))
        },

        AST::For(name, cond, body, id) => {
            AST::For(name, Box::new(refine_node(*cond)), Box::new(refine_node(*body)), id)
        },

        AST::List(code) => { AST::List(refine_vec(code)) }

        AST::Class(name, parent, body, id) => {
            AST::Class(name, parent, refine_vec(body), id)
        },

        AST::Index(base, index, _) => {
            //AST::Index(Box::new(refine_node(*base)), Box::new(refine_node(*index)), stype)
            refine_node(AST::Invoke(Box::new(AST::Accessor(base, String::from("[]"), None)), vec!(*index), None))
        },

        AST::Resolver(left, right) => {
            // TODO should this also allow a non-type specifier?
            match *left {
                AST::Identifier(_) => { },
                _ => panic!("SyntaxError: left-hand side of scope resolver must be identifier")
            }
            AST::Resolver(Box::new(refine_node(*left)), right)
        },

        AST::Accessor(left, right, stype) => {
            AST::Accessor(Box::new(refine_node(*left)), right, stype)
        },

        AST::Assignment(left, right) => {
            let left = *left;
            match left {
                AST::Accessor(_, _, _) => {
                    AST::Assignment(Box::new(refine_node(left)), Box::new(refine_node(*right)))
                },
                AST::Index(base, index, _) => {
                    refine_node(AST::Invoke(Box::new(AST::Accessor(base, String::from("[]"), None)), vec!(*index, *right), None))
                },
                _ => panic!("SyntaxError: assignment to something other than a list or class element: {:?}", left),
            }
            //AST::Assignment(Box::new(refine_node(*left)), Box::new(refine_node(*right)))
        },

        AST::Noop => { node },
        AST::Underscore => { node },
        AST::Boolean(_) => { node },
        AST::Integer(_) => { node },
        AST::Real(_) => { node },
        AST::String(_) => { node },
        AST::Nil(_) => { node },
        AST::Identifier(_) => { node },
        AST::Import(_, _) => { node },
        AST::Type(_, _) => { node },

        //node @ _ => { node }
    }
}

