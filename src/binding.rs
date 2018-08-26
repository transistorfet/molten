

use abi::ABI;
use types::Type;
use ast::{ NodeID, Ident, ClassSpec, AST };
use session::{ Session, Error };
use scope::{ Scope, ScopeRef };
use utils::UniqueID;

use defs::classes::ClassDef;
use defs::variables::VarDef;
use defs::functions::FuncDef;


pub fn bind_names(session: &Session, code: &mut Vec<AST>) {
    bind_names_vec(session, session.map.get_global(), code);
    if session.errors.get() > 0 {
        panic!("Exiting due to previous errors");
    }
}

pub fn bind_names_vec(session: &Session, scope: ScopeRef, code: &mut Vec<AST>) {
    for node in code {
        bind_names_node(session, scope.clone(), node);
    }
}

pub fn bind_names_node(session: &Session, scope: ScopeRef, node: &mut AST) {
    match bind_names_node_or_error(session, scope, node) {
        Ok(_) => { },
        Err(err) => session.print_error(err.add_pos(&node.get_pos())),
    }
}

fn bind_names_node_or_error(session: &Session, scope: ScopeRef, node: &mut AST) -> Result<(), Error> {
    match *node {
        AST::Function(ref id, _, ref ident, ref mut args, ref mut ret, ref mut body, ref abi) => {
            let fscope = session.map.add(*id, Some(scope.clone()));
            fscope.set_basename(ident.as_ref().map_or(format!("anon{}", id), |ident| ident.name.clone()));

            // Check for typevars in the type params
            for ref mut arg in &mut args.iter_mut() {
                declare_typevars(session, fscope.clone(), arg.ttype.as_mut(), false)?;
            }
            declare_typevars(session, fscope.clone(), ret.as_mut(), false)?;


            // Define the function variable and it's arguments variables
            FuncDef::define_func(session, scope.clone(), *id, &ident.as_ref().map(|ref ident| String::from(ident.as_str())), *abi, None)?;

            for ref arg in args.iter() {
                //fscope.define(arg.ident.name.clone(), arg.ttype.clone(), Some(arg.id))?;
                VarDef::define_var(session, fscope.clone(), arg.id, &arg.ident.name, arg.ttype.clone())?;
            }

            bind_names_node(session, fscope, body)
        },

        AST::Invoke(ref id, _, ref mut fexpr, ref mut args) => {
            bind_names_node(session, scope.clone(), fexpr);
            bind_names_vec(session, scope, args);
        },

        AST::Definition(ref id, _, ref ident, ref mut ttype, ref mut code) => {
            declare_typevars(session, scope.clone(), ttype.as_mut(), false)?;
            VarDef::define_var(session, scope.clone(), *id, &ident.name, ttype.clone())?;
            bind_names_node(session, scope, code);
        },

        AST::Declare(ref id, _, ref ident, ref mut ttype) => {
            declare_typevars(session, scope.clone(), Some(ttype), false)?;
            let abi = ttype.get_abi().unwrap_or(ABI::Molten);
            FuncDef::define_func(session, scope.clone(), *id, &Some(ident.name.clone()), abi, Some(ttype.clone()))?;
        },

        AST::Identifier(ref id, _, ref ident) => {
            match scope.get_var_def(&ident.name) {
                Some(defid) => session.set_ref(*id, defid),
                None => return Err(Error::new(format!("NameError: undefined identifier {:?}", ident.name)))
            }
        },

        AST::SideEffect(ref id, _, _, ref mut args) => {
            bind_names_vec(session, scope, args);
        },

        AST::If(ref id, _, ref mut cond, ref mut texpr, ref mut fexpr) => {
            bind_names_node(session, scope.clone(), cond);
            bind_names_node(session, scope.clone(), texpr);
            bind_names_node(session, scope, fexpr);
        },

        AST::Try(ref id, _, ref mut cond, ref mut cases, _) |
        AST::Match(ref id, _, ref mut cond, ref mut cases, _) => {
            bind_names_node(session, scope.clone(), cond);
            // TODO check to make sure AST::Underscore only occurs as the last case, if at all
            for &mut (ref mut case, ref mut body) in cases {
                bind_names_node(session, scope.clone(), case);
                bind_names_node(session, scope.clone(), body);
            }
        },

        AST::Raise(ref id, _, ref mut expr) => {
            bind_names_node(session, scope, expr);
        },

        AST::While(ref id, _, ref mut cond, ref mut body) => {
            bind_names_node(session, scope.clone(), cond);
            bind_names_node(session, scope, body);
        },

        AST::For(ref id, _, ref ident, ref mut cond, ref mut body) => {
            let lscope = session.map.add(*id, Some(scope.clone()));
            VarDef::define_var(session, lscope.clone(), *id, &ident.name, None)?;
            bind_names_node(session, lscope.clone(), cond);
            bind_names_node(session, lscope, body);
        },

        AST::Tuple(ref id, _, ref mut code) |
        AST::List(ref id, _, ref mut code) |
        AST::Block(ref id, _, ref mut code) => { bind_names_vec(session, scope, code); },

        AST::Index(ref id, _, ref mut base, ref mut index, _) => {
            bind_names_node(session, scope.clone(), base);
            bind_names_node(session, scope, index);
        },


        AST::PtrCast(ref mut ttype, ref mut code) => {
            declare_typevars(session, scope.clone(), Some(ttype), false)?;
            bind_names_node(session, scope, code)
        },

        AST::New(ref id, _, ref mut classspec) => {
            classspec.types.iter_mut().map(|ref mut ttype| declare_typevars(session, scope.clone(), Some(ttype), false).unwrap()).count();
            match scope.get_type_def(&classspec.ident.name) {
                Some(defid) => session.set_ref(*id, defid),
                None => return Err(Error::new(format!("NameError: undefined identifier {:?}", classspec.ident.name)))
            }
        },

        AST::Class(ref id, _, ref mut classspec, ref mut parentspec, ref mut body) => {
            let tscope = ClassDef::create_class_scope(session, scope.clone(), *id);

            // Check for typevars in the type params
            declare_classspec_typevars(session, tscope.clone(), classspec, true)?;
            if let &mut Some(ref mut pspec) = parentspec {
                declare_classspec_typevars(session, tscope.clone(), pspec, false)?;
            }

            let classtype = Type::Object(classspec.ident.name.clone(), *id, classspec.types.clone());
            let parenttype = parentspec.clone().map(|p| scope.make_obj(session, p.ident.name.clone(), p.types).unwrap());
            //let classtype = Type::from_spec(classspec.clone());
            //let parenttype = parentspec.clone().map(|p| Type::from_spec(p));

            let classdef = ClassDef::define_class(session, scope, *id, classtype, parenttype)?;
            let tscope = session.map.get(id);

            bind_names_vec(session, tscope, body);
        },

        AST::TypeDef(ref id, ref pos, ref mut classspec, ref mut fields) => {
            //let mut types = vec!();
            // TODO this is nearly identical to class... these need to be separated
            let tscope = session.map.add(*id, None);
            tscope.set_redirect(true);
            tscope.set_basename(classspec.ident.name.clone());

            classspec.types.iter_mut().map(|ref mut ttype| declare_typevars(session, tscope.clone(), Some(ttype), true).unwrap()).count();
            for field in fields {
                declare_typevars(session, scope.clone(), field.ttype.as_mut(), true)?;
                VarDef::define_var(session, scope.clone(), *id, &field.ident.name, field.ttype.clone())?;
            }
        },

        AST::Resolver(ref id, _, ref mut left, _) => {
            // TODO should this always work on a type reference, or should classes be added as values as well as types?
            //bind_names_node(session, scope, left);
            match **left {
                AST::Identifier(_, _, ref ident) => {
                    match scope.get_type_def(&ident.name) {
                        Some(defid) => session.set_ref(*id, defid),
                        None => return Err(Error::new(format!("NameError: undefined type {:?}", ident.name)))
                    }
                },
                _ => { return Err(Error::new(format!("SyntaxError: left-hand side of scope resolver must be identifier"))); }
            }
        },

        AST::Accessor(ref id, _, ref mut left, _, _) => {
            bind_names_node(session, scope, left);
        },

        AST::Assignment(ref id, _, ref mut left, ref mut right) => {
            match **left {
                AST::Accessor(_, _, _, _, _) | AST::Index(_, _, _, _, _) => { },
                _ => return Err(Error::new(format!("SyntaxError: assignment to something other than a list or class element: {:?}", left))),
            };
            bind_names_node(session, scope.clone(), left);
            bind_names_node(session, scope, right);
        },

        AST::Import(ref id, _, ref ident, ref mut decls) => {
            let path = ident.name.replace(".", "/") + ".dec";
            *decls = session.parse_file(path.as_str(), true);
            bind_names_vec(session, scope, decls);
        },

        AST::Recall(_, _) |
        AST::Underscore | AST::Nil(_) |
        AST::Literal(_, _) => { }
    }
    Ok(())
}

#[must_use]
pub fn declare_typevars(session: &Session, scope: ScopeRef, ttype: Option<&mut Type>, always_new: bool) -> Result<(), Error> {
    match ttype {
        Some(ttype) => match ttype {
            &mut Type::Object(ref name, ref mut id, ref mut types) => {
                match scope.find_type(session, name) {
                    Some(Type::Object(_, ref eid, _)) => *id = *eid,
                    _ => if !always_new {
                        panic!("UndefinedType: {:?}", name)
                    } else if *id == UniqueID(0) {
                        *id = UniqueID::generate();
                    },
                }

                for ttype in types.iter_mut() {
                    declare_typevars(session, scope.clone(), Some(ttype), always_new)?;
                }
            },
            &mut Type::Tuple(ref mut types) => {
                for ttype in types.iter_mut() {
                    declare_typevars(session, scope.clone(), Some(ttype), always_new)?;
                }
            },
            &mut Type::Function(ref mut args, ref mut ret, _) => {
                declare_typevars(session, scope.clone(), Some(args.as_mut()), always_new)?;
                declare_typevars(session, scope, Some(ret.as_mut()), always_new)?;
            },
            &mut Type::Variable(ref name, ref mut id) => {
                let vtype = match always_new {
                    true => scope.find_type_local(session, name),
                    false => scope.find_type(session, name),
                };
                match vtype {
                    Some(Type::Variable(_, ref eid)) => {
                        *id = *eid
                    },
                    _ => {
                        *id = UniqueID::generate();
                        let ttype = Type::Variable(name.clone(), *id);
                        // TODO i think this contains check should be removable
                        if !scope.contains_type_local(name) && !scope.is_primative() {
                            scope.define_type(name.clone(), Some(*id))?;
                        }
                        session.set_type(*id, ttype);
                    }
                }
            },
            &mut Type::Overload(_) => { },
        },
        _ => { },
    }
    Ok(())
}

#[must_use]
pub fn declare_classspec_typevars(session: &Session, scope: ScopeRef, classspec: &mut ClassSpec, always_new: bool) -> Result<(), Error> {
    let &mut ClassSpec { ref ident, ref mut types, .. } = classspec;
    types.iter_mut().map(|ref mut ttype| declare_typevars(session, scope.clone(), Some(ttype), always_new)).count();
    Ok(())
}

/*
pub fn declare_typevars(session: &Session, scope: ScopeRef, ttype: Type, always_new: bool) -> Type {
    convert(|ttype| {
        match ttype {
            Type::Variable(name, id) => {
                let vtype = match always_new {
                    true => scope.find_type_local(session, name),
                    false => scope.find_type(session, name),
                };
                match vtype {
                    Some(Type::Variable(_, ref eid)) => *id = *eid,
                    _ => {
                        *id = UniqueID::generate();
                        if !scope.contains_type_local(name) && !scope.is_primative() {
                            scope.define_type(name.clone(), Type::Variable(name.clone(), *id))?;
                        }
                        session.set_type(*id, ttype);
                    }
                }
            },
            node @ _ => node
        }
    });
*/

