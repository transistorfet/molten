

use abi::ABI;
use types::Type;
use session::{ Session, Error };
use scope::{ ScopeRef, Context };
use ast::{ NodeID, Mutability, ClassSpec, Pattern, AST };
use misc::{ UniqueID, r };

use defs::enums::EnumDef;
use defs::classes::ClassDef;
use defs::functions::AnyFunc;
use defs::types::TypeAliasDef;
use defs::variables::{ AnyVar, VarDef, ArgDef };


pub fn bind_names(session: &Session, scope: ScopeRef, code: &Vec<AST>) {
    bind_names_vec(session, scope, code);
    if session.errors.get() > 0 {
        panic!("Exiting due to previous errors");
    }
    debug!("POST-BINDING: {:?}", code);
}

pub fn bind_names_vec(session: &Session, scope: ScopeRef, code: &Vec<AST>) {
    for node in code {
        bind_names_node(session, scope.clone(), node);
    }
}

pub fn bind_names_node(session: &Session, scope: ScopeRef, node: &AST) {
    match bind_names_node_or_error(session, scope, node) {
        Ok(_) => { },
        Err(err) => session.print_error(err.add_pos(&node.get_pos())),
    }
}

#[must_use]
fn bind_names_node_or_error(session: &Session, scope: ScopeRef, node: &AST) -> Result<(), Error> {
    match *node {
        AST::Function(ref id, _, ref vis, ref ident, ref args, ref ret, ref body, ref abi) => {
            let fscope = session.map.add(*id, Some(scope.clone()));
            fscope.set_basename(ident.as_ref().map_or(format!("anon{}", id), |ident| ident.name.clone()));

            // Check for typevars in the type params
            let mut argtypes = vec!();
            for ref arg in args.iter() {
                let mut ttype = arg.ttype.clone();
                bind_type_names(session, fscope.clone(), ttype.as_mut(), false)?;
                // TODO this is assumed to be always immutable, but maybe shouldn't be
                ArgDef::define(session, fscope.clone(), arg.id, Mutability::Immutable, &arg.ident.name, ttype.clone())?;
                argtypes.push(ttype.unwrap_or_else(|| scope.new_typevar(session, false)));
            }
            let mut ret = ret.clone();
            bind_type_names(session, fscope.clone(), ret.as_mut(), false)?;

            // Build type according to the definition, using typevars for missing types
            let nftype = Type::Function(r(Type::Tuple(argtypes)), r(ret.unwrap_or_else(|| scope.new_typevar(session, false))), *abi);

            // Define the function variable and it's arguments variables
            AnyFunc::define(session, scope.clone(), *id, *vis, &ident.as_ref().map(|ref ident| String::from(ident.as_str())), *abi, Some(nftype))?;

            bind_names_node(session, fscope, body)
        },

        AST::Invoke(_, _, ref fexpr, ref args) => {
            bind_names_node(session, scope.clone(), fexpr);
            bind_names_vec(session, scope, args);
        },

        AST::Definition(ref id, _, ref mutable, ref ident, ref ttype, ref code) => {
            let mut ttype = ttype.clone();
            bind_type_names(session, scope.clone(), ttype.as_mut(), false)?;
            AnyVar::define(session, scope.clone(), *id, *mutable, &ident.name, ttype)?;
            bind_names_node(session, scope, code);
        },

        AST::Declare(ref id, _, ref vis, ref ident, ref ttype) => {
            let mut ttype = ttype.clone();
            bind_type_names(session, scope.clone(), Some(&mut ttype), false)?;
            let abi = ttype.get_abi().unwrap_or(ABI::Molten);
            AnyFunc::define(session, scope.clone(), *id, *vis, &Some(ident.name.clone()), abi, Some(ttype))?;
        },

        AST::Identifier(ref id, _, ref ident) => {
            if session.get_ref(*id).is_err() {
                // TODO you must check to make sure if we are accessing a variable or argument, that it is either local or we are in a closure...
                //      (the latter being more difficult to figure out; we need to some kind of context value)
                match scope.get_var_def(&ident.name) {
                    Some(defid) => session.set_ref(*id, defid),
                    None => return Err(Error::new(format!("NameError: undefined identifier {:?}", ident.name)))
                }
            }
        },

        AST::SideEffect(_, _, _, ref args) => {
            bind_names_vec(session, scope, args);
        },

        AST::If(_, _, ref cond, ref texpr, ref fexpr) => {
            bind_names_node(session, scope.clone(), cond);
            bind_names_node(session, scope.clone(), texpr);
            bind_names_node(session, scope, fexpr);
        },

        AST::Try(_, _, ref cond, ref cases) |
        AST::Match(_, _, ref cond, ref cases) => {
            bind_names_node(session, scope.clone(), cond);
            // TODO check to make sure Pattern::Wild only occurs as the last case, if at all
            for ref mut case in cases {
                let lscope = session.map.add(case.id, Some(scope.clone()));
                lscope.set_context(Context::Block);
                bind_names_pattern(session, lscope.clone(), &case.pat)?;
                bind_names_node(session, lscope.clone(), &case.body);
            }
        },

        AST::Raise(_, _, ref expr) => {
            bind_names_node(session, scope, expr);
        },

        AST::While(_, _, ref cond, ref body) => {
            bind_names_node(session, scope.clone(), cond);
            bind_names_node(session, scope, body);
        },

        AST::Ref(_, _, ref code) |
        AST::Deref(_, _, ref code) => { bind_names_node(session, scope, code); },

        AST::Tuple(_, _, ref code) |
        AST::Block(_, _, ref code) => { bind_names_vec(session, scope, code); },

        AST::Record(_, _, ref items) => {
            for &(_, ref expr) in items {
                bind_names_node(session, scope.clone(), expr);
            }
        },

        AST::RecordUpdate(_, _, ref record, ref items) => {
            bind_names_node(session, scope.clone(), record);
            for &(_, ref expr) in items {
                bind_names_node(session, scope.clone(), expr);
            }
        },


        AST::PtrCast(ref id, ref ttype, ref code) => {
            let mut ttype = ttype.clone();
            bind_type_names(session, scope.clone(), Some(&mut ttype), false)?;
            session.set_type(*id, ttype);
            bind_names_node(session, scope, code)
        },

        AST::New(ref id, _, ref classspec) => {
            let mut classspec = classspec.clone();
            bind_classspec_type_names(session, scope.clone(), &mut classspec, false)?;
            let classtype = scope.make_obj(session, classspec.ident.name.clone(), classspec.types.clone())?;
            session.set_type(*id, classtype);
            match scope.get_type_def(&classspec.ident.name) {
                Some(defid) => session.set_ref(*id, defid),
                None => return Err(Error::new(format!("NameError: undefined identifier {:?}", classspec.ident.name)))
            }
        },

        AST::Class(ref id, _, ref classspec, ref parentspec, ref body) => {
            let tscope = ClassDef::create_class_scope(session, scope.clone(), *id);

            // Check for typevars in the type params
            let mut classspec = classspec.clone();
            let mut parentspec = parentspec.clone();
            bind_classspec_type_names(session, tscope.clone(), &mut classspec, true)?;
            if let &mut Some(ref mut pspec) = &mut parentspec {
                bind_classspec_type_names(session, tscope.clone(), pspec, false)?;
            }

            let classtype = Type::Object(classspec.ident.name, *id, classspec.types);
            let parenttype = match parentspec {
                Some(p) => Some(scope.make_obj(session, p.ident.name.clone(), p.types.clone())?),
                None => None
            };
            //let classtype = Type::from_spec(classspec.clone());
            //let parenttype = parentspec.clone().map(|p| Type::from_spec(p));

            ClassDef::define(session, scope, *id, classtype, parenttype)?;
            let tscope = session.map.get(id);

            bind_names_vec(session, tscope, body);
        },

        AST::TypeAlias(ref id, _, ref classspec, ref ttype) => {
            let mut ttype = ttype.clone();
            let mut classspec = classspec.clone();
            bind_classspec_type_names(session, scope.clone(), &mut classspec, true)?;
            bind_type_names(session, scope.clone(), Some(&mut ttype), false)?;

            let deftype = Type::Object(classspec.ident.name, *id, classspec.types);
            TypeAliasDef::define(session, scope.clone(), *id, deftype, ttype)?;
            //scope.define_type(classspec.ident.name.clone(), Some(*id));
            //session.set_type(*id, ttype.clone());
        },

        AST::TypeEnum(ref id, _, ref classspec, ref variants) => {
            let mut classspec = classspec.clone();
            bind_classspec_type_names(session, scope.clone(), &mut classspec, true)?;
            let deftype = Type::Object(classspec.ident.name, *id, classspec.types);
            let enumdef = EnumDef::define(session, scope.clone(), *id, deftype)?;

            for variant in variants.iter() {
                // TODO this still requires mut
                let mut variant = variant.clone();
                bind_type_names(session, scope.clone(), variant.ttype.as_mut(), false)?;
                check_recursive_type(&variant.ttype.as_ref(), *id)?;
                enumdef.add_variant(session, scope.clone(), variant)?;
            }
        },

        AST::Resolver(_, _, ref left, _, ref oid) => {
            // TODO should this always work on a type reference, or should classes be added as values as well as types?
            //bind_names_node(session, scope, left);
            match **left {
                AST::Identifier(_, _, ref ident) => {
                    match scope.get_type_def(&ident.name) {
                        Some(defid) => session.set_ref(*oid, defid),
                        None => return Err(Error::new(format!("NameError: undefined type {:?}", ident.name)))
                    }
                },
                _ => { return Err(Error::new(format!("SyntaxError: left-hand side of scope resolver must be identifier"))); }
            }
        },

        AST::Accessor(_, _, ref left, _, _) => {
            bind_names_node(session, scope, left);
        },

        AST::Assignment(_, _, ref left, ref right, _) => {
            bind_names_node(session, scope.clone(), left);
            bind_names_node(session, scope, right);
        },

        AST::Import(_, _, _, ref decls) => {
            bind_names_vec(session, scope, decls);
        },

        AST::Nil(_) |
        AST::GetValue(_) |
        AST::Literal(_, _) => { }

        AST::List(_, _, _) |
        AST::For(_, _, _, _, _) |
        AST::Index(_, _, _, _) => { panic!("InternalError: ast element shouldn't appear at this late phase: {:?}", node) }
    }
    Ok(())
}

#[must_use]
pub fn bind_names_pattern(session: &Session, scope: ScopeRef, pat: &Pattern) -> Result<(), Error> {
    match pat {
        Pattern::Binding(id, ident) => {
            VarDef::define(session, scope.clone(), *id, Mutability::Immutable, &ident.name, None)?;
        },
        Pattern::Annotation(id, ttype, pat) => {
            let mut ttype = ttype.clone();
            bind_type_names(session, scope.clone(), Some(&mut ttype), false)?;
            session.set_type(*id, ttype);
            bind_names_pattern(session, scope, pat)?
        },
        Pattern::Resolve(id, left, ident, oid) => {
            match **left {
                Pattern::Identifier(_, ref ident) => {
                    match scope.get_type_def(&ident.name) {
                        Some(defid) => session.set_ref(*oid, defid),
                        None => return Err(Error::new(format!("NameError: undefined type {:?}", ident.name)))
                    }
                },
                _ => { return Err(Error::new(format!("SyntaxError: left-hand side of scope resolver must be identifier"))); }
            }
        },
        Pattern::EnumArgs(id, left, args) => {
            bind_names_pattern(session, scope.clone(), left);
            for arg in args {
                bind_names_pattern(session, scope.clone(), arg);
            }
        },
        Pattern::Tuple(id, items) => {
            for item in items {
                bind_names_pattern(session, scope.clone(), item);
            }
        },
        Pattern::Record(id, items) => {
            for (_, item) in items {
                bind_names_pattern(session, scope.clone(), item);
            }
        },

        Pattern::Wild |
        Pattern::Literal(_, _) |
        Pattern::Identifier(_, _) => { },
    }
    Ok(())
}


#[must_use]
pub fn bind_type_names(session: &Session, scope: ScopeRef, ttype: Option<&mut Type>, always_new: bool) -> Result<(), Error> {
    match ttype {
        Some(ttype) => match ttype {
            &mut Type::Object(ref name, ref mut id, ref mut types) => {
                match scope.get_type_def(name) {
                    Some(defid) => *id = defid,
                    None => if !always_new {
                        panic!("UndefinedType: {:?}", name)
                    } else if *id == UniqueID(0) {
                        *id = UniqueID::generate();
                    },
                }

                for ttype in types.iter_mut() {
                    bind_type_names(session, scope.clone(), Some(ttype), always_new)?;
                }
            },
            &mut Type::Tuple(ref mut types) => {
                for ttype in types.iter_mut() {
                    bind_type_names(session, scope.clone(), Some(ttype), always_new)?;
                }
            },
            &mut Type::Record(ref mut types) => {
                types.sort_unstable_by(|a, b| a.0.cmp(&b.0));
                for (_, ttype) in types.iter_mut() {
                    bind_type_names(session, scope.clone(), Some(ttype), always_new)?;
                }
            },
            &mut Type::Function(ref mut args, ref mut ret, _) => {
                bind_type_names(session, scope.clone(), Some(args.as_mut()), always_new)?;
                bind_type_names(session, scope, Some(ret.as_mut()), always_new)?;
            },
            &mut Type::Variable(ref name, ref mut id, existential) => {
                debug!("DECLARING TYPEVAR: {:?} {:?}", name, id);
                let vtype = match always_new {
                    true => scope.find_type_local(session, name),
                    false => scope.find_type(session, name),
                };
                match vtype {
                    Some(Type::Variable(_, ref eid, _)) => {
                        debug!("FOUND VAR {:?}", eid);
                        *id = *eid
                    },
                    _ => {
                        *id = UniqueID::generate();
                        debug!("SETTING {:?} TO {:?}", name, id);
                        let ttype = Type::Variable(name.clone(), *id, existential);
                        scope.define_type(name.clone(), Some(*id))?;
                        session.set_type(*id, ttype);
                    }
                }
            },
            &mut Type::Ref(ref mut ttype) => {
                bind_type_names(session, scope.clone(), Some(ttype), always_new)?;
            },
            &mut Type::Ambiguous(_) => { },
        },
        None => { },
    }
    Ok(())
}

#[must_use]
pub fn bind_classspec_type_names(session: &Session, scope: ScopeRef, classspec: &mut ClassSpec, always_new: bool) -> Result<(), Error> {
    let &mut ClassSpec { ref mut types, .. } = classspec;
    types.iter_mut().map(|ref mut ttype| bind_type_names(session, scope.clone(), Some(ttype), always_new)).count();
    Ok(())
}

/*
pub fn bind_type_names(session: &Session, scope: ScopeRef, ttype: Type, always_new: bool) -> Type {
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

#[must_use]
pub fn check_recursive_type(ttype: &Option<&Type>, forbidden_id: NodeID) -> Result<(), Error> {
    match ttype {
        Some(ttype) => match ttype {
            Type::Object(ref name, ref id, ref types) => {
                if *id == forbidden_id {
                    return Err(Error::new(format!("TypeError: an enum cannot contain itself in {:?}", name)));
                }

                for ttype in types.iter() {
                    check_recursive_type(&Some(ttype), forbidden_id)?;
                }
            },
            Type::Tuple(ref types) => {
                for ttype in types.iter() {
                    check_recursive_type(&Some(ttype), forbidden_id)?;
                }
            },
            Type::Record(ref types) => {
                for (_, ttype) in types.iter() {
                    check_recursive_type(&Some(ttype), forbidden_id)?;
                }
            },
            Type::Variable(ref name, ref id, ref existential) => {
                // TODO this might be an error?
            },
            Type::Ref(_) |
            Type::Function(_, _, _) => {
                // Any reference or function containing the type is ok
            },
            Type::Ambiguous(_) => { },
        },
        None => { },
    }
    Ok(())
}
