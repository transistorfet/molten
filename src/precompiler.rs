
use std::fmt::Debug;

use parser::AST;
use types::{ resolve_type };
use typecheck::{ update_scope_variable_types };
use scope::{ ScopeRef, ScopeMapRef };
//use utils::UniqueID;


pub fn precompile<V, T>(map: ScopeMapRef<V, T>, code: Vec<AST>) -> Vec<AST> where V: Clone + Debug, T: Clone + Debug {
    update_scope_variable_types(map.get_global());
    precompile_vec(map.clone(), map.get_global(), code)
}

pub fn precompile_vec<V, T>(map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>, code: Vec<AST>) -> Vec<AST> where V: Clone + Debug, T: Clone + Debug {
    let mut block = vec!();
    for node in code {
        block.push(precompile_node(map.clone(), scope.clone(), node));
    }
    block
}

pub fn precompile_node<V, T>(map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>, node: AST) -> AST where V: Clone + Debug, T: Clone + Debug {
    match node {
        AST::Block(pos, code) => { AST::Block(pos, precompile_vec(map.clone(), scope.clone(), code)) },

        AST::Definition(pos, (name, ttype), code) => {
            AST::Definition(pos, (name, Some(resolve_type(scope.clone(), ttype.unwrap()))), Box::new(precompile_node(map.clone(), scope.clone(), *code)))
        },

        AST::Declare(pos, name, ttype) => {
            AST::Declare(pos, name, resolve_type(scope.clone(), ttype))
        },

        AST::Function(pos, name, mut args, ret, body, id) => {
            let fscope = map.get(&id);
            update_scope_variable_types(fscope.clone());
            /*
            if scope.borrow().is_local() {
                let mut code = vec!();

                let cid = UniqueID::generate();
                let tscope = map.add(cid.clone(), Some(scope.clone()));
                tscope.borrow_mut().set_class(true);
                tscope.borrow_mut().set_basename(format!("closure{}", cid));

                // Create class type named __closure__
                let cpair = (String::from(format!("__closure{}__", cid)), vec!());
                let classdef = scope.borrow_mut().create_class_def(&cpair, None);
                let ctype = Type::make_object(cpair.clone());
                tscope.borrow_mut().define_type(String::from("Self"), ctype.clone());

                // Add closure context argument to function definition
                args.push((String::from("__context__"), Some(ctype.clone()), None));
                let ftype = Type::Function(args.iter().map(|t| t.1.clone().unwrap()).collect(), Box::new(ret.clone().unwrap()));
                fscope.borrow_mut().define(String::from("__context__"), Some(ctype.clone()));
                let body = precompile_node(map.clone(), fscope.clone(), *body);

                // Create the definition of the closure context class
                let mut classbody = vec!();
                classbody.push(AST::Definition((String::from("__func__"), Some(ftype.clone())), Box::new(AST::Underscore)));
                for (name, sym) in &classdef.borrow().names {
                    classbody.push(AST::Definition((name.clone(), sym.ttype.clone()), Box::new(AST::Underscore)));
                }
                code.push(AST::Class(cpair.clone(), None, classbody, cid.clone()));

                // Create an instance of the class and assign the current values to its members
                let cref = format!("__closure{}__", cid);
                scope.borrow_mut().define(cref.clone(), Some(ctype.clone()));
                code.push(AST::Definition((cref.clone(), Some(ctype.clone())), Box::new(AST::New(cpair.clone()))));
                for (name, sym) in &classdef.borrow().names {
                    code.push(AST::Assignment(Box::new(AST::Accessor(Box::new(AST::Identifier(cref.clone())), name.clone(), Some(ctype.clone()))), Box::new(AST::Identifier(name.clone()))));
                }

                // Assign the function itself to the context class
                code.push(AST::Assignment(
                    Box::new(AST::Accessor(Box::new(AST::Identifier(cref.clone())), String::from("__func__"), Some(ctype.clone()))),
                    Box::new(AST::Function(name, args, ret, Box::new(body), id))
                ));
                code.push(AST::Identifier(cref.clone()));

                // Return the whole block of AST in place of the original function definition
                AST::Block(code)
            } else {
            */
                // TODO resolve types of args
                AST::Function(pos, name, args, Some(resolve_type(fscope.clone(), ret.unwrap())), Box::new(precompile_node(map.clone(), fscope.clone(), *body)), id)
            //}
        },

        AST::Invoke(pos, fexpr, args, stype) => {
            // TODO you could detect a closure with an explicit tag on variable??? no you'de have to process fexpr
            //if something {
            //args.insert(0, precompile_node(map.clone(), scope.clone(), *fexpr));
            //}
            AST::Invoke(pos, Box::new(precompile_node(map.clone(), scope.clone(), *fexpr)), precompile_vec(map.clone(), scope.clone(), args), Some(resolve_type(scope.clone(), stype.unwrap())))
        },

        AST::Identifier(pos, name) => {
            /*
            if scope.borrow().is_closure_var(&name) {
                let ctype = scope.borrow().get_variable_type(&String::from("__context__")).unwrap();
                let classdef = scope.borrow().get_class_def(&ctype.get_name());
                let ttype = scope.borrow().get_variable_type(&name);
                classdef.borrow_mut().define(name.clone(), ttype);
                AST::Accessor(Box::new(AST::Identifier(String::from("__context__"))), name, scope.borrow().get_variable_type(&String::from("__context__")))
            } else {
            */
                AST::Identifier(pos, name)
            //}
        },

        AST::SideEffect(pos, op, args) => {
            AST::SideEffect(pos, op, precompile_vec(map.clone(), scope.clone(), args))
        },

        AST::If(pos, cond, texpr, fexpr) => {
            AST::If(pos, Box::new(precompile_node(map.clone(), scope.clone(), *cond)), Box::new(precompile_node(map.clone(), scope.clone(), *texpr)), Box::new(precompile_node(map.clone(), scope.clone(), *fexpr)))
        },

        AST::Match(pos, cond, cases) => {
            let cases = cases.into_iter().map(|(case, body)| ( precompile_node(map.clone(), scope.clone(), case), precompile_node(map.clone(), scope.clone(), body) )).collect();
            AST::Match(pos, Box::new(precompile_node(map.clone(), scope.clone(), *cond)), cases)
        },

        AST::Try(pos, cond, cases) => {
            let cases = cases.into_iter().map(|(case, body)| ( precompile_node(map.clone(), scope.clone(), case), precompile_node(map.clone(), scope.clone(), body) )).collect();
            AST::Try(pos, Box::new(precompile_node(map.clone(), scope.clone(), *cond)), cases)
        },

        AST::Raise(pos, expr) => {
            AST::Raise(pos, Box::new(precompile_node(map.clone(), scope.clone(), *expr)))
        },

        AST::While(pos, cond, body) => {
            AST::While(pos, Box::new(precompile_node(map.clone(), scope.clone(), *cond)), Box::new(precompile_node(map.clone(), scope.clone(), *body)))
        },

        AST::For(pos, name, cond, body, id) => {
            let lscope = map.get(&id);
            AST::For(pos, name, Box::new(precompile_node(map.clone(), lscope.clone(), *cond)), Box::new(precompile_node(map.clone(), lscope.clone(), *body)), id)
        },

        AST::List(pos, code) => { AST::List(pos, precompile_vec(map.clone(), scope.clone(), code)) },

        AST::New(_, _) => { node },

        AST::Class(pos, pair, parent, body, id) => {
            let tscope = map.get(&id);
            //update_scope_variable_types(tscope.clone());
            //let classdef = scope.borrow().get_class_def(&pair.0);
            //update_scope_variable_types(classdef.clone());
            let mut body = precompile_vec(map.clone(), tscope.clone(), body);

            /*
            let initname = String::from("__init__");
            let objtype = Type::make_object(pair.clone());
            if !classdef.borrow().contains_local(&initname) {
                let initid = UniqueID::generate();

                let mut initbody = vec!();
                if parent.is_some() {
                    //initbody.push(AST::Invoke(
                }
                for ref node in &body {
                    match **node {
                        AST::Definition((ref name, ref ttype), ref value) => {
                            initbody.push(AST::Assignment(Box::new(AST::Accessor(Box::new(AST::Identifier(String::from("object"))), name.clone(), Some(objtype.clone()))), value.clone()));
                        },
                        _ => { }
                    }
                }
                initbody.push(AST::Nil(Some(Type::Object(String::from("Nil"), vec!()))));

                let init = AST::Function(
                    Some(initname.clone()),
                    vec!((String::from("object"), Some(objtype.clone()), None)),
                    Some(Type::Object(String::from("Nil"), vec!())),
                    Box::new(AST::Block(initbody)),
                    initid.clone()
                );

                register_function(map.clone(), tscope.clone(), &init);
                body.insert(0, init);
            }
            */

            AST::Class(pos, pair, parent, body, id)
        },


        AST::Resolver(pos, left, right) => {
            AST::Resolver(pos, Box::new(precompile_node(map.clone(), scope.clone(), *left)), right)
        },

        AST::Accessor(pos, left, right, stype) => {
            AST::Accessor(pos, Box::new(precompile_node(map.clone(), scope.clone(), *left)), right, Some(resolve_type(scope.clone(), stype.unwrap())))
        },

        AST::Assignment(pos, left, right) => {
            AST::Assignment(pos, Box::new(precompile_node(map.clone(), scope.clone(), *left)), Box::new(precompile_node(map.clone(), scope.clone(), *right)))
        },

        AST::Import(pos, name, decls) => {
            AST::Import(pos, name, precompile_vec(map.clone(), scope.clone(), decls))
        },

        AST::Nil(stype) => { AST::Nil(Some(resolve_type(scope.clone(), stype.unwrap()))) },

        AST::Noop => { node },
        AST::Underscore => { node },
        AST::Boolean(_) => { node },
        AST::Integer(_) => { node },
        AST::Real(_) => { node },
        AST::String(_) => { node },
        AST::Type(_, _, _) => { node },

        AST::Index(_, _, _, _) => panic!("InternalError: ast element shouldn't appear at this late phase: {:?}", node),
    }
}

/*
pub fn register_function<V, T>(map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>, function: &AST) where V: Clone + Debug, T: Clone + Debug {
    if let &AST::Function(ref name, ref args, ref ret, ref body, ref id) = function {
        let name = name.clone().unwrap();
        let fscope = map.add(id.clone(), Some(scope.clone()));
        fscope.borrow_mut().set_basename(name.clone());

        for arg in args {
            fscope.borrow_mut().define(arg.0.clone(), arg.1.clone());
        }

        let ftype = Type::Function(args.iter().map(|t| t.1.clone().unwrap()).collect(), Box::new(ret.clone().unwrap()));
        let dscope = Scope::target(scope.clone());
        dscope.borrow_mut().define(name, Some(ftype.clone()));
    }
}
*/

