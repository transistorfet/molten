

use abi::ABI;
use types::Type;
use classes::ClassDef;
use ast::{ ClassSpec, AST };
use session::{ Session, Error };
use scope::{ Scope, ScopeRef };
use utils::UniqueID;


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
        AST::Function(_, ref ident, ref mut args, ref mut ret, ref mut body, ref id, ref abi) => {
            let fscope = session.map.add(*id, Some(scope.clone()));
            fscope.set_basename(ident.as_ref().map_or(format!("anon{}", id), |ident| ident.name.clone()));

            if let Some(ref ident) = *ident {
                let dscope = Scope::target(scope);
                dscope.define_func(ident.name.clone(), None, *abi)?;
            }

            for ref mut arg in &mut args.iter_mut() {
                declare_typevars(fscope.clone(), arg.ttype.as_mut(), false)?;
                fscope.define(arg.ident.name.clone(), arg.ttype.clone())?;
            }
            declare_typevars(fscope.clone(), ret.as_mut(), false)?;

            bind_names_node(session, fscope, body)
        },

        AST::Invoke(_, ref mut fexpr, ref mut args, _) => {
            bind_names_node(session, scope.clone(), fexpr);
            bind_names_vec(session, scope, args);
        },

        AST::Definition(_, ref ident, ref mut ttype, ref mut code) => {
            declare_typevars(scope.clone(), ttype.as_mut(), false)?;
            let dscope = Scope::target(scope.clone());
            dscope.define(ident.name.clone(), ttype.clone())?;
            bind_names_node(session, scope, code);
        },

        AST::Declare(_, ref ident, ref mut ttype) => {
            declare_typevars(scope.clone(), Some(ttype), false)?;
            let dscope = Scope::target(scope.clone());
            let abi = ttype.get_abi().unwrap_or(ABI::Molten);
            dscope.define_func(ident.name.clone(), Some(ttype.clone()), abi)?;
            if let Some(ref mname) = abi.unmangle_name(ident.as_str()) {
                dscope.define_func(mname.clone(), None, abi)?;
                let mut stype = dscope.get_variable_type(mname).unwrap_or(Type::Overload(vec!()));
                stype = stype.add_variant(scope, ttype.clone())?;
                dscope.set_variable_type(mname, stype);
            }
        },

        AST::Recall(_, ref ident) |
        AST::Identifier(_, ref ident) => {
            if !scope.contains(&ident.name) {
                return Err(Error::new(format!("NameError: undefined identifier {:?}", ident.name)));
            }
        },

        AST::SideEffect(_, _, ref mut args) => {
            bind_names_vec(session, scope, args);
        },

        AST::If(_, ref mut cond, ref mut texpr, ref mut fexpr) => {
            bind_names_node(session, scope.clone(), cond);
            bind_names_node(session, scope.clone(), texpr);
            bind_names_node(session, scope, fexpr);
        },

        AST::Try(_, ref mut cond, ref mut cases, _) |
        AST::Match(_, ref mut cond, ref mut cases, _) => {
            bind_names_node(session, scope.clone(), cond);
            // TODO check to make sure AST::Underscore only occurs as the last case, if at all
            for &mut (ref mut case, ref mut body) in cases {
                bind_names_node(session, scope.clone(), case);
                bind_names_node(session, scope.clone(), body);
            }
        },

        AST::Raise(_, ref mut expr) => {
            bind_names_node(session, scope, expr);
        },

        AST::While(_, ref mut cond, ref mut body) => {
            bind_names_node(session, scope.clone(), cond);
            bind_names_node(session, scope, body);
        },

        AST::For(_, ref ident, ref mut cond, ref mut body, ref id) => {
            let lscope = session.map.add(*id, Some(scope.clone()));
            lscope.define(ident.name.clone(), None)?;
            bind_names_node(session, lscope.clone(), cond);
            bind_names_node(session, lscope, body);
        },

        AST::List(_, ref mut code, _) |
        AST::Block(_, ref mut code) => { bind_names_vec(session, scope, code); },

        AST::Index(_, ref mut base, ref mut index, _) => {
            bind_names_node(session, scope.clone(), base);
            bind_names_node(session, scope, index);
        },


        AST::PtrCast(ref mut ttype, ref mut code) => {
            declare_typevars(scope.clone(), Some(ttype), false)?;
            bind_names_node(session, scope, code)
        },

        AST::New(_, ref mut classspec) => {
            classspec.types.iter_mut().map(|ref mut ttype| declare_typevars(scope.clone(), Some(ttype), false).unwrap()).count();
            if scope.find_type(&classspec.ident.name).is_none() {
                return Err(Error::new(format!("NameError: undefined identifier {:?}", classspec.ident.name)));
            }
        },

        AST::Class(_, ref mut classspec, ref mut parentspec, ref mut body, ref id) => {
            // Create a temporary invisible scope to name check the class body
            let tscope = session.map.add(*id, Some(scope.clone()));
            tscope.set_class(true);
            tscope.set_basename(classspec.ident.name.clone());

            // Define Self and Super, and check for typevars in the type params
            classspec.types.iter_mut().map(|ref mut ttype| declare_typevars(tscope.clone(), Some(ttype), true).unwrap()).count();
            tscope.define_type(String::from("Self"), Type::from_spec(classspec.clone()))?;
            if let &mut Some(ClassSpec { ident: ref pident, types: ref mut ptypes, .. }) = parentspec {
                ptypes.iter_mut().map(|ref mut ttype| declare_typevars(tscope.clone(), Some(ttype), false).unwrap()).count();
                tscope.define_type(String::from("Super"), Type::Object(pident.name.clone(), ptypes.clone()))?;
            }

            ClassDef::define_class(scope, classspec, parentspec.clone())?;
            bind_names_vec(session, tscope, body);
        },

        AST::Resolver(_, ref mut left, _) => {
            // TODO should this always work on a type reference, or should classes be added as values as well as types?
            //bind_names_node(session, scope, left);
            match **left {
                AST::Identifier(_, ref ident) => {
                    if scope.find_type(&ident.name).is_none() {
                        return Err(Error::new(format!("NameError: undefined type {:?}", ident.name)));
                    }
                },
                _ => { return Err(Error::new(format!("SyntaxError: left-hand side of scope resolver must be identifier"))); }
            }
        },

        AST::Accessor(_, ref mut left, _, _) => {
            bind_names_node(session, scope, left);
        },

        AST::Assignment(_, ref mut left, ref mut right) => {
            match **left {
                AST::Accessor(_, _, _, _) | AST::Index(_, _, _, _) => { },
                _ => return Err(Error::new(format!("SyntaxError: assignment to something other than a list or class element: {:?}", left))),
            };
            bind_names_node(session, scope.clone(), left);
            bind_names_node(session, scope, right);
        },

        AST::Import(_, ref ident, ref mut decls) => {
            let path = ident.name.replace(".", "/") + ".dec";
            *decls = session.parse_file(path.as_str(), true);
            bind_names_vec(session, scope, decls);
        },

        AST::Type(_, _, _) => panic!("NotImplementedError: not yet supported, {:?}", node),

        AST::Noop | AST::Underscore | AST::Nil(_) |
        AST::Boolean(_) | AST::Integer(_) | AST::Real(_) | AST::String(_) => { }
    }
    Ok(())
}

#[must_use]
pub fn declare_typevars(scope: ScopeRef, ttype: Option<&mut Type>, always_new: bool) -> Result<(), Error> {
    match ttype {
        Some(ttype) => match ttype {
            &mut Type::Object(_, ref mut types) => {
                for ttype in types.iter_mut() {
                    declare_typevars(scope.clone(), Some(ttype), always_new)?;
                }
            },
            &mut Type::Function(ref mut args, ref mut ret, _) => {
                for atype in args.iter_mut() {
                    declare_typevars(scope.clone(), Some(atype), always_new)?;
                }
                declare_typevars(scope, Some(ret.as_mut()), always_new)?;
            },
            &mut Type::Variable(ref name, ref mut id) => {
                let vtype = match always_new {
                    true => scope.find_type_local(name),
                    false => scope.find_type(name),
                };
                match vtype {
                    Some(Type::Variable(_, ref eid)) => *id = *eid,
                    _ => {
                        *id = UniqueID::generate();
                        let gscope = Scope::global(scope.clone());
                        gscope.define_type(id.to_string(), Type::Variable(name.clone(), *id))?;
                        if !scope.contains_type_local(name) && !scope.is_primative() {
                            scope.define_type(name.clone(), Type::Variable(name.clone(), *id))?;
                        }
                    }
                }
            },
            &mut Type::Overload(_) => { },
        },
        _ => { },
    }
    Ok(())
}

