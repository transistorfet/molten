

use std::fmt::Debug;

use import;
use parser::AST;
use types::Type;
use scope::{ self, Scope, ScopeRef, ScopeMapRef };


pub fn bind_names<V, T>(map: ScopeMapRef<V, T>, code: &mut Vec<AST>) where V: Clone + Debug, T: Clone + Debug {
    bind_names_vec(map.clone(), map.get_global().clone(), code);
}

pub fn bind_names_vec<V, T>(map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>, code: &mut Vec<AST>) where V: Clone + Debug, T: Clone + Debug {
    for node in code {
        bind_names_node(map.clone(), scope.clone(), node);
    }
}

fn bind_names_node<V, T>(map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>, node: &mut AST) where V: Clone + Debug, T: Clone + Debug {
    match *node {
        AST::Function(ref name, ref args, ref ret, ref mut body, ref id) => {
            let fscope = map.add(id.clone(), Some(scope.clone()));
            fscope.borrow_mut().set_basename(name.as_ref().map_or(format!("anon{}", id), |name| name.clone()));

            if let Some(ref name) = *name {
                let dscope = Scope::target(scope.clone());
                dscope.borrow_mut().define_func(name.clone(), None, false);
            }

            for arg in args {
                declare_typevars(scope.clone(), &arg.1);
                fscope.borrow_mut().define(arg.0.clone(), arg.1.clone());
            }
            declare_typevars(scope.clone(), ret);

            bind_names_node(map.clone(), fscope.clone(), body)
        },

        AST::Invoke(ref mut fexpr, ref mut args, _) => {
            bind_names_node(map.clone(), scope.clone(), fexpr);
            bind_names_vec(map.clone(), scope.clone(), args);
        },

        AST::Definition((ref name, ref ttype), ref mut code) => {
            declare_typevars(scope.clone(), ttype);
            let dscope = Scope::target(scope.clone());
            dscope.borrow_mut().define(name.clone(), ttype.clone());
            bind_names_node(map.clone(), scope.clone(), code);
        },

        AST::Declare(ref name, ref ttype) => {
            let dscope = Scope::target(scope.clone());
            dscope.borrow_mut().define_func(name.clone(), Some(ttype.clone()), true);
            if let Some(ref mname) = scope::unmangle_name(name) {
                dscope.borrow_mut().define_func(mname.clone(), None, true);
                let mut stype = dscope.borrow().get_variable_type(mname).unwrap_or(Type::Overload(vec!()));
                stype = stype.add_variant(scope.clone(), ttype.clone());
                dscope.borrow_mut().update_variable_type(mname, stype);
            }
        },

        AST::Identifier(ref name) => {
            if scope.borrow().find(name).is_none() {
                panic!("NameError: undefined identifier {:?}", name);
            }
        },

        AST::SideEffect(_, ref mut args) => {
            bind_names_vec(map.clone(), scope.clone(), args);
        },

        AST::If(ref mut cond, ref mut texpr, ref mut fexpr) => {
            bind_names_node(map.clone(), scope.clone(), cond);
            bind_names_node(map.clone(), scope.clone(), texpr);
            bind_names_node(map.clone(), scope.clone(), fexpr);
        },

        AST::Try(ref mut cond, ref mut cases) |
        AST::Match(ref mut cond, ref mut cases) => {
            bind_names_node(map.clone(), scope.clone(), cond);
            // TODO check to make sure AST::Underscore only occurs as the last case, if at all
            for &mut (ref mut case, ref mut body) in cases {
                bind_names_node(map.clone(), scope.clone(), case);
                bind_names_node(map.clone(), scope.clone(), body);
            }
        },

        AST::Raise(ref mut expr) => {
            bind_names_node(map.clone(), scope.clone(), expr);
        },

        AST::While(ref mut cond, ref mut body) => {
            bind_names_node(map.clone(), scope.clone(), cond);
            bind_names_node(map.clone(), scope.clone(), body);
        },

        AST::For(ref name, ref mut cond, ref mut body, ref id) => {
            let lscope = map.add(id.clone(), Some(scope.clone()));
            lscope.borrow_mut().define(name.clone(), None);
            bind_names_node(map.clone(), lscope.clone(), cond);
            bind_names_node(map.clone(), lscope.clone(), body);
        },

        AST::List(ref mut code) |
        AST::Block(ref mut code) => { bind_names_vec(map, scope, code); },

        AST::Index(ref mut base, ref mut index, _) => {
            bind_names_node(map.clone(), scope.clone(), base);
            bind_names_node(map.clone(), scope.clone(), index);
        },


        AST::New((ref name, _)) => {
            if scope.borrow().find_type(name).is_none() {
                panic!("NameError: undefined identifier {:?}", name);
            }
        },

        AST::Class(ref pair, ref parent, ref mut body, ref id) => {
            let classdef = scope.borrow_mut().create_class_def(pair, parent.clone());
            let &(ref name, ref types) = pair;

            // Create a temporary invisible scope to name check the class body
            let tscope = map.add(id.clone(), Some(scope.clone()));
            tscope.borrow_mut().set_class(true);
            tscope.borrow_mut().set_basename(name.clone());

            // Define Self and Super, and check for typevars in the type params
            tscope.borrow_mut().define_type(String::from("Self"), Type::Object(name.clone(), types.clone()));
            types.iter().map(|ttype| declare_typevars(tscope.clone(), &Some(ttype.clone()))).count();
            if let &Some((ref pname, ref ptypes)) = parent {
                tscope.borrow_mut().define_type(String::from("Super"), Type::Object(pname.clone(), ptypes.clone()));
                ptypes.iter().map(|ttype| declare_typevars(tscope.clone(), &Some(ttype.clone()))).count();
            }

            bind_names_vec(map.clone(), tscope.clone(), body);
        },

        AST::Resolver(ref mut left, _) => {
            // TODO should this always work on a type reference, or should classes be added as values as well as types?
            //bind_names_node(map.clone(), scope, left);
            match **left {
                AST::Identifier(ref name) => {
                    if scope.borrow().find_type(name).is_none() {
                        panic!("NameError: undefined type {:?}", name);
                    }
                },
                _ => panic!("SyntaxError: left-hand side of scope resolver must be identifier")
            }
        },

        AST::Accessor(ref mut left, _, _) => {
            bind_names_node(map.clone(), scope, left);
        },

        AST::Assignment(ref mut left, ref mut right) => {
            match **left {
                AST::Accessor(_, _, _) | AST::Index(_, _, _) => { },
                _ => panic!("SyntaxError: assignment to something other than a list or class element: {:?}", left),
            };
            bind_names_node(map.clone(), scope.clone(), left);
            bind_names_node(map.clone(), scope.clone(), right);
        },

        AST::Import(ref name, ref mut decls) => {
            let path = name.replace(".", "/") + ".dec";
            *decls = import::load_index(path.as_str());
            bind_names_vec(map.clone(), scope.clone(), decls);
        },

        AST::Type(_, _) => panic!("NotImplementedError: not yet supported, {:?}", node),

        AST::Noop | AST::Underscore | AST::Nil(_) |
        AST::Boolean(_) | AST::Integer(_) | AST::Real(_) | AST::String(_) => { }
    }
}

pub fn declare_typevars<V, T>(scope: ScopeRef<V, T>, ttype: &Option<Type>) where V: Clone, T: Clone {
    match ttype {
        &Some(ref ttype) => match ttype {
            &Type::Object(_, ref types) => {
                for ttype in types {
                    declare_typevars(scope.clone(), &Some(ttype.clone()))
                }
            },
            &Type::Function(ref args, ref ret) => {
                for atype in args {
                    declare_typevars(scope.clone(), &Some(atype.clone()))
                }
                declare_typevars(scope.clone(), &Some(*ret.clone()));
            },
            &Type::Variable(ref name) => {
                if !scope.borrow().contains_type(name) {
                    scope.borrow_mut().define_type(name.clone(), ttype.clone());
                }
            },
            _ => { },
        },
        _ => { },
    }
}

