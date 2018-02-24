

use std::fmt::Debug;

use import;
use parser::AST;
use types::Type;
use session::SessionRef;
use scope::{ self, Scope, ScopeRef, ScopeMapRef };
use utils::UniqueID;


pub fn bind_names<V, T>(session: SessionRef<V, T>, code: &mut Vec<AST>) where V: Clone + Debug, T: Clone + Debug {
    bind_names_vec(session.clone(), session.borrow().map.get_global().clone(), code);
}

pub fn bind_names_vec<V, T>(session: SessionRef<V, T>, scope: ScopeRef<V, T>, code: &mut Vec<AST>) where V: Clone + Debug, T: Clone + Debug {
    for node in code {
        bind_names_node(session.clone(), scope.clone(), node);
    }
}

fn bind_names_node<V, T>(session: SessionRef<V, T>, scope: ScopeRef<V, T>, node: &mut AST) where V: Clone + Debug, T: Clone + Debug {
    match *node {
        AST::Function(ref pos, ref name, ref mut args, ref mut ret, ref mut body, ref id) => {
            let fscope = session.borrow().map.add(id.clone(), Some(scope.clone()));
            fscope.borrow_mut().set_basename(name.as_ref().map_or(format!("anon{}", id), |name| name.clone()));

            if let Some(ref name) = *name {
                let dscope = Scope::target(scope.clone());
                dscope.borrow_mut().define_func(name.clone(), None, false);
            }

            for ref mut arg in &mut args.iter_mut() {
                declare_typevars(fscope.clone(), arg.1.as_mut(), false);
                fscope.borrow_mut().define(arg.0.clone(), arg.1.clone());
            }
            declare_typevars(fscope.clone(), ret.as_mut(), false);

            bind_names_node(session.clone(), fscope.clone(), body)
        },

        AST::Invoke(_, ref mut fexpr, ref mut args, _) => {
            bind_names_node(session.clone(), scope.clone(), fexpr);
            bind_names_vec(session.clone(), scope.clone(), args);
        },

        AST::Definition(ref pos, (ref name, ref mut ttype), ref mut code) => {
            declare_typevars(scope.clone(), ttype.as_mut(), false);
            let dscope = Scope::target(scope.clone());
            dscope.borrow_mut().define(name.clone(), ttype.clone());
            bind_names_node(session.clone(), scope.clone(), code);
        },

        AST::Declare(ref pos, ref name, ref mut ttype) => {
            declare_typevars(scope.clone(), Some(ttype), false);
            let dscope = Scope::target(scope.clone());
            dscope.borrow_mut().define_func(name.clone(), Some(ttype.clone()), true);
            if let Some(ref mname) = scope::unmangle_name(name) {
                dscope.borrow_mut().define_func(mname.clone(), None, true);
                let mut stype = dscope.borrow().get_variable_type(mname).unwrap_or(Type::Overload(vec!()));
                stype = stype.add_variant(scope.clone(), ttype.clone());
                dscope.borrow_mut().set_variable_type(mname, stype);
            }
        },

        AST::Identifier(ref pos, ref name) => {
            if !scope.borrow().contains(name) {
                panic!("NameError:{:?}: undefined identifier {:?}", pos, name);
            }
        },

        AST::SideEffect(_, _, ref mut args) => {
            bind_names_vec(session.clone(), scope.clone(), args);
        },

        AST::If(_, ref mut cond, ref mut texpr, ref mut fexpr) => {
            bind_names_node(session.clone(), scope.clone(), cond);
            bind_names_node(session.clone(), scope.clone(), texpr);
            bind_names_node(session.clone(), scope.clone(), fexpr);
        },

        AST::Try(_, ref mut cond, ref mut cases) |
        AST::Match(_, ref mut cond, ref mut cases) => {
            bind_names_node(session.clone(), scope.clone(), cond);
            // TODO check to make sure AST::Underscore only occurs as the last case, if at all
            for &mut (ref mut case, ref mut body) in cases {
                bind_names_node(session.clone(), scope.clone(), case);
                bind_names_node(session.clone(), scope.clone(), body);
            }
        },

        AST::Raise(_, ref mut expr) => {
            bind_names_node(session.clone(), scope.clone(), expr);
        },

        AST::While(_, ref mut cond, ref mut body) => {
            bind_names_node(session.clone(), scope.clone(), cond);
            bind_names_node(session.clone(), scope.clone(), body);
        },

        AST::For(_, ref name, ref mut cond, ref mut body, ref id) => {
            let lscope = session.borrow().map.add(id.clone(), Some(scope.clone()));
            lscope.borrow_mut().define(name.clone(), None);
            bind_names_node(session.clone(), lscope.clone(), cond);
            bind_names_node(session.clone(), lscope.clone(), body);
        },

        AST::List(_, ref mut code) |
        AST::Block(_, ref mut code) => { bind_names_vec(session, scope, code); },

        AST::Index(_, ref mut base, ref mut index, _) => {
            bind_names_node(session.clone(), scope.clone(), base);
            bind_names_node(session.clone(), scope.clone(), index);
        },


        AST::New(ref pos, (ref name, ref mut types)) => {
            types.iter_mut().map(|ref mut ttype| declare_typevars(scope.clone(), Some(ttype), false)).count();
            if scope.borrow().find_type(name).is_none() {
                panic!("NameError:{:?}: undefined identifier {:?}", pos, name);
            }
        },

        AST::Class(ref pos, ref mut pair, ref mut parent, ref mut body, ref id) => {
            let &mut (ref name, ref mut types) = pair;

            // Create a temporary invisible scope to name check the class body
            let tscope = session.borrow().map.add(id.clone(), Some(scope.clone()));
            tscope.borrow_mut().set_class(true);
            tscope.borrow_mut().set_basename(name.clone());

            // Define Self and Super, and check for typevars in the type params
            types.iter_mut().map(|ref mut ttype| declare_typevars(tscope.clone(), Some(ttype), true)).count();
            tscope.borrow_mut().define_type(String::from("Self"), Type::Object(name.clone(), types.clone()));
            if let &mut Some((ref pname, ref mut ptypes)) = parent {
                ptypes.iter_mut().map(|ref mut ttype| declare_typevars(tscope.clone(), Some(ttype), false)).count();
                tscope.borrow_mut().define_type(String::from("Super"), Type::Object(pname.clone(), ptypes.clone()));
            }

            let classdef = scope.borrow_mut().create_class_def(&(name.clone(), types.clone()), parent.clone());
            bind_names_vec(session.clone(), tscope.clone(), body);
        },

        AST::Resolver(ref pos, ref mut left, _) => {
            // TODO should this always work on a type reference, or should classes be added as values as well as types?
            //bind_names_node(session.clone(), scope, left);
            match **left {
                AST::Identifier(_, ref name) => {
                    if scope.borrow().find_type(name).is_none() {
                        panic!("NameError:{:?}: undefined type {:?}", pos, name);
                    }
                },
                _ => panic!("SyntaxError:{:?}: left-hand side of scope resolver must be identifier", pos)
            }
        },

        AST::Accessor(_, ref mut left, _, _) => {
            bind_names_node(session.clone(), scope, left);
        },

        AST::Assignment(ref pos, ref mut left, ref mut right) => {
            match **left {
                AST::Accessor(_, _, _, _) | AST::Index(_, _, _, _) => { },
                _ => panic!("SyntaxError:{:?}: assignment to something other than a list or class element: {:?}", pos, left),
            };
            bind_names_node(session.clone(), scope.clone(), left);
            bind_names_node(session.clone(), scope.clone(), right);
        },

        AST::Import(_, ref name, ref mut decls) => {
            //let path = name.replace(".", "/") + ".dec";
            //*decls = import::load_index(path.as_str());
            //*decls = session.borrow_mut().load_index(path.as_str());
            bind_names_vec(session.clone(), scope.clone(), decls);
        },

        AST::Type(_, _, _) => panic!("NotImplementedError: not yet supported, {:?}", node),

        AST::Noop | AST::Underscore | AST::Nil(_) |
        AST::Boolean(_) | AST::Integer(_) | AST::Real(_) | AST::String(_) => { }
    }
}

pub fn declare_typevars<V, T>(scope: ScopeRef<V, T>, ttype: Option<&mut Type>, always_new: bool) where V: Clone, T: Clone {
    match ttype {
        Some(ttype) => match ttype {
            &mut Type::Object(_, ref mut types) => {
                for ttype in types.iter_mut() {
                    declare_typevars(scope.clone(), Some(ttype), always_new)
                }
            },
            &mut Type::Function(ref mut args, ref mut ret) => {
                for atype in args.iter_mut() {
                    declare_typevars(scope.clone(), Some(atype), always_new)
                }
                declare_typevars(scope.clone(), Some(ret.as_mut()), always_new);
            },
            &mut Type::Variable(ref name, ref mut id) => {
                let vtype = scope.borrow().find_type(name);
                match vtype {
                    Some(Type::Variable(_, ref eid)) if !always_new => *id = eid.clone(),
                    _ => {
                        *id = UniqueID::generate();
                        let gscope = Scope::global(scope.clone());
                        gscope.borrow_mut().define_type(id.to_string(), Type::Variable(name.clone(), id.clone()));
                        if !scope.borrow().contains_type_local(name) {
                            scope.borrow_mut().define_type(name.clone(), Type::Variable(name.clone(), id.clone()));
                        }
                    }
                }
            },
            &mut Type::Overload(_) => { },
        },
        _ => { },
    }
}

