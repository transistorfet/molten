

use defs::Def;
use session::{ Session, Error };
use scope::{ Scope, ScopeRef };
use ast::{ NodeID, ClassSpec, Literal, AST };
use types::{ Type, Check, ABI, expect_type, resolve_type, check_type_params };


pub fn check_types(session: &Session, scope: ScopeRef, code: &Vec<AST>) -> Type {
    let ttype = check_types_vec(session, scope, code);
    if session.errors.get() > 0 {
        panic!("Exiting due to previous errors");
    }
    ttype
}

pub fn check_types_vec(session: &Session, scope: ScopeRef, code: &Vec<AST>) -> Type {
    let mut last: Type = scope.make_obj(session, String::from("Nil"), vec!()).unwrap();
    for node in code {
        last = check_types_node(session, scope.clone(), node, None);
    }
    last
}

pub fn check_types_node(session: &Session, scope: ScopeRef, node: &AST, expected: Option<Type>) -> Type {
    match check_types_node_or_error(session, scope.clone(), node, expected) {
        Ok(ttype) => ttype,
        Err(err) => {

            session.print_error(err.add_pos(&node.get_pos()));
            //Type::Object(String::from("Nil"), vec!())
            scope.new_typevar(session)
        }
    }
}

pub fn check_types_node_or_error(session: &Session, scope: ScopeRef, node: &AST, expected: Option<Type>) -> Result<Type, Error> {
    let rtype = match *node {
        AST::Literal(ref _id, ref literal) => {
            match literal {
                Literal::Unit => scope.make_obj(session, String::from("()"), vec!())?,
                Literal::Boolean(_) => scope.make_obj(session, String::from("Bool"), vec!())?,
                Literal::Integer(_) => scope.make_obj(session, String::from("Int"), vec!())?,
                Literal::Real(_) => scope.make_obj(session, String::from("Real"), vec!())?,
                Literal::String(_) => scope.make_obj(session, String::from("String"), vec!())?,
            }
        },

        AST::Function(ref id, _, ref ident, ref args, ref rtype, ref body, ref abi) => {
            let fscope = session.map.get(id);

            let mut argtypes = vec!();
            for arg in args.iter() {
                let vtype = arg.default.clone().map(|ref vexpr| check_types_node(session, scope.clone(), vexpr, arg.ttype.clone()));
                let mut atype = expect_type(session, fscope.clone(), arg.ttype.clone(), vtype, Check::Def)?;
                if &arg.ident.name[..] == "self" {
                    let mut stype = fscope.find_type(session, &String::from("Self")).unwrap();
                    stype = fscope.map_all_typevars(session, stype);
                    atype = expect_type(session, fscope.clone(), Some(atype), Some(stype), Check::Def)?;
                }
                session.update_type(fscope.clone(), arg.id, atype.clone())?;
                argtypes.push(atype);
            }

            let rettype = expect_type(session, fscope.clone(), rtype.clone(), Some(check_types_node(session, fscope.clone(), body, rtype.clone())), Check::Def)?;

            // Resolve type variables that can be
            for i in 0 .. argtypes.len() {
                argtypes[i] = resolve_type(session, argtypes[i].clone());
                session.update_type(fscope.clone(), args[i].id, argtypes[i].clone())?;
            }

            let tupleargs = Type::Tuple(argtypes);
            let nftype = Type::Function(Box::new(tupleargs.clone()), Box::new(rettype), *abi);

            if let Ok(Def::Overload(ol)) = session.get_def_from_ref(*id) {
                if ol.find_local_variants(session, scope.clone(), tupleargs.clone()).0.len() > 0 {
                    return Err(Error::new(format!("OverloadError: things {:?}", ident)));
                }
            }

            // TODO build closure context from body (all references)
            session.update_type(scope.clone(), *id, nftype.clone())?;
            nftype
        },

        AST::Invoke(ref id, _, ref fexpr, ref args) => {
            let mut atypes = vec!();
            for ref mut value in args {
                atypes.push(check_types_node(session, scope.clone(), value, None));
            }
            let atypes = Type::Tuple(atypes);

            let tscope = Scope::new_ref(Some(scope.clone()));
            let dtype = session_find_variant(session, tscope.clone(), *id, fexpr.as_ref(), &atypes)?;
            let etype = match dtype {
                Type::Variable(_, _) => dtype.clone(),
                _ => tscope.map_all_typevars(session, dtype.clone()),
            };

            let ftype = match etype {
                Type::Function(ref args, _, ref abi) => {
                    //let ftype = expect_type(session, tscope.clone(), Some(etype.clone()), Some(Type::Function(Box::new(atypes), Box::new(expected.unwrap_or_else(|| tscope.new_typevar())), abi)), Check::Update)?;
                    let ftype = expect_type(session, tscope.clone(), Some(etype.clone()), Some(Type::Function(Box::new(atypes), Box::new(etype.get_rettype()?.clone()), *abi)), Check::Def)?;
                    // TODO should this actually be another expect, so type resolutions that occur in later args affect earlier args?  Might not be needed unless you add typevar constraints
                    let ftype = resolve_type(session, ftype);        // NOTE This ensures the early arguments are resolved despite typevars not being assigned until later in the signature

                    ftype
                },
                Type::Variable(_, ref vid) => {
                    let ftype = Type::Function(Box::new(atypes), Box::new(expected.unwrap_or_else(|| tscope.new_typevar(session))), ABI::Unknown);
                    session.update_type(tscope, *vid, ftype.clone())?;
                    ftype
                },
                _ => return Err(Error::new(format!("NotAFunction: {:?}", fexpr))),
            };

            session.update_type(scope.clone(), *id, ftype.clone())?;
            ftype.get_rettype()?.clone()
        },

        AST::SideEffect(_, _, _, ref args) => {
            let mut ltype = None;
            for ref expr in args {
                ltype = Some(expect_type(session, scope.clone(), ltype.clone(), Some(check_types_node(session, scope.clone(), expr, ltype.clone())), Check::List)?);
            }
            ltype.unwrap()
        },

        AST::Definition(ref id, _, _, ref ident, ref ttype, ref body) => {
            let btype = expect_type(session, scope.clone(), ttype.clone(), Some(check_types_node(session, scope.clone(), body, ttype.clone())), Check::Def)?;
            session.update_type(scope.clone(), *id, btype.clone())?;
            btype
        },

        AST::Declare(ref id, _, ref _ident, ref ttype) => {
            session.update_type(scope.clone(), *id, ttype.clone())?;
            ttype.clone()
        },

        AST::Identifier(_, _, ref ident) => {
            //session.get_type_from_ref(*id)?;
            //scope.get_variable_type(session, &ident.name).unwrap_or_else(|| expected.unwrap_or_else(|| scope.new_typevar(session)))
            match scope.get_var_def(&ident.name) {
                //Some(defid) => match session.get_def(defid) {
                //    Ok(Def::Overload(_)) => return Err(Error::new(format!("TypeError: reference is overloaded for {:?}", ident.name))),
                //    _ => session.get_type(defid).unwrap(),
                //},
                Some(defid) => match session.get_type(defid) {
                    Some(ttype) => ttype,
                    None => return Err(Error::new(format!("TypeError: the reference {:?} has no type or has an ambiguous type", ident.name))),
                }
                None => panic!("InternalError: ident {:?} is undefined, but should have been caught in the name binding phase", ident.name),
            }
        },

        AST::Block(_, _, ref body) => check_types_vec(session, scope, body),

        AST::If(_, _, ref cond, ref texpr, ref fexpr) => {
            // TODO should this require the cond type to be Bool?
            check_types_node(session, scope.clone(), cond, None);
            let ttype = check_types_node(session, scope.clone(), texpr, None);
            let ftype = check_types_node(session, scope.clone(), fexpr, Some(ttype.clone()));
            expect_type(session, scope, Some(ttype), Some(ftype), Check::List)?
        },

        AST::Try(ref id, _, ref cond, ref cases, ref cid) |
        AST::Match(ref id, _, ref cond, ref cases, ref cid) => {
            let mut ctype = check_types_node(session, scope.clone(), cond, None);
            let mut rtype = None;
            for &(ref case, ref expr) in cases {
                ctype = expect_type(session, scope.clone(), Some(ctype.clone()), Some(check_types_node(session, scope.clone(), case, Some(ctype.clone()))), Check::List)?;
                rtype = Some(expect_type(session, scope.clone(), rtype.clone(), Some(check_types_node(session, scope.clone(), expr, rtype.clone())), Check::List)?);
            }
            //*condtype = ctype;
            session.set_type(*cid, ctype);
            rtype.unwrap()
        },

        AST::Raise(_, _, ref expr) => {
            // TODO should you check for a special error/exception type?
            check_types_node(session, scope, expr, None)
        },

        AST::While(_, _, ref cond, ref body) => {
            // TODO should this require the cond type to be Bool?
            check_types_node(session, scope.clone(), cond, None);
            check_types_node(session, scope.clone(), body, None);
            //Type::Object(String::from("Nil"), vec!())
            scope.make_obj(session, String::from("Nil"), vec!())?
        },

        AST::For(ref id, _, ref ident, ref list, ref body) => {
            let lscope = session.map.get(id);
            let itype = lscope.get_variable_type(session, &ident.name).unwrap_or_else(|| expected.unwrap_or_else(|| scope.new_typevar(session)));
            let etype = Some(scope.make_obj(session, String::from("List"), vec!(itype))?);
            let ltype = expect_type(session, lscope.clone(), etype.clone(), Some(check_types_node(session, lscope.clone(), list, etype.clone())), Check::Def)?;
            session.update_type(lscope.clone(), *id, ltype.get_params()?[0].clone())?;
            check_types_node(session, lscope, body, None)
        },

        AST::Nil(ref id) => {
            let ttype = expected.unwrap_or_else(|| scope.new_typevar(session));
            session.set_type(*id, ttype.clone());
            ttype
        },

        AST::Ref(ref id, _, ref expr) => {
            let ttype = check_types_node(session, scope.clone(), expr, None);
            let rtype = Type::Ref(Box::new(ttype));
            let rtype = expect_type(session, scope.clone(), expected.clone(), Some(rtype), Check::Def)?;
            session.set_type(*id, rtype.clone());
            rtype
        },

        AST::Tuple(ref id, _, ref items) => {
            // TODO would this not be a bug if the expected type was for some reason a variable?
            //let etypes = match expected {
            //    Some(ref e) => e.get_types()?.iter().map(|i| Some(i.clone())).collect(),
            //    None => vec![None; items.len()]
            //};
            let etypes = vec![None; items.len()];

            if etypes.len() != items.len() {
                return Err(Error::new(format!("TypeError: number of tuple items don't match: expected {:?} with {} items but found {} items", expected, etypes.len(), items.len())));
            }

            let mut types = vec!();
            for (ref expr, etype) in items.iter().zip(etypes.iter()) {
                types.push(expect_type(session, scope.clone(), etype.clone(), Some(check_types_node(session, scope.clone(), expr, etype.clone())), Check::List)?);
            }
            session.set_type(*id, Type::Tuple(types.clone()));
            Type::Tuple(types)
        },

        AST::Record(ref id, _, ref items) => {
            let mut types = vec!();
            for (ref ident, ref expr) in items {
                types.push((ident.name.clone(), check_types_node(session, scope.clone(), expr, None)));
            }
            session.set_type(*id, Type::Record(types.clone()));
            Type::Record(types)
        },

        AST::List(_, _, ref items) => {
            let mut ltype = None;
            for ref expr in items {
                ltype = Some(expect_type(session, scope.clone(), ltype.clone(), Some(check_types_node(session, scope.clone(), expr, ltype.clone())), Check::List)?);
            }
            let ltype = ltype.unwrap_or_else(|| expected.unwrap_or_else(|| scope.new_typevar(session)));
            scope.make_obj(session, String::from("List"), vec!(ltype))?
        },

        AST::TypeAlias(ref id, _, ref classspec, ref ttype) => {
            /*
            let tscope = session.map.get(id);
            for field in fields {
                // TODO check the types such that any unprovided types are infered
                //expect_type(session, tscope, Some(ttype.clone()), Some(ctype), Check::List)?;
            }
            */
            scope.make_obj(session, String::from("Nil"), vec!())?
        },

        AST::PtrCast(ref ttype, ref code) => {
            let ctype = check_types_node(session, scope.clone(), code, Some(ttype.clone()));
            debug!("PTRCAST: {:?} <- {:?}", ttype, ctype);
            // TODO is this quite right?
            expect_type(session, scope, Some(ttype.clone()), Some(ctype), Check::List)?;
            ttype.clone()
        },

        AST::New(ref id, _, ClassSpec { ref ident, ref types, .. }) => {
            let dtype = session.get_type_from_ref(*id)?;
            let tscope = Scope::new_ref(Some(scope.clone()));
            let mtype = tscope.map_all_typevars(session, dtype.clone());
            check_type_params(session, scope.clone(), &mtype.get_params()?, types, Check::Def, false)?;
            scope.make_obj(session, ident.name.clone(), types.clone())?
        },

        AST::Class(ref id, _, _, _, ref body) => {
            let tscope = session.map.get(id);
            check_types_vec(session, tscope.clone(), body);
            //Type::Object(String::from("Nil"), vec!())
            scope.make_obj(session, String::from("Nil"), vec!())?
        },

        AST::Resolver(_, _, _, _) |
        AST::Accessor(_, _, _, _, _) => {
            let (refid, defid) = get_access_ids(session, scope.clone(), node)?.unwrap();
            session.set_ref(refid, defid);
            session.get_type(defid).unwrap_or_else(|| expected.unwrap_or_else(|| scope.new_typevar(session)))
        },

        AST::Assignment(ref id, _, ref left, ref right) => {
            // TODO this is duplicated in check_types_node(left)... can we avoid that
            let (refid, defid) = get_access_ids(session, scope.clone(), left)?.unwrap();
            if !session.get_def(defid).map(|d| d.is_mutable()).unwrap_or(false) {
                return Err(Error::new(format!("MutableError: attempting to assign to an immutable variable")));
            }

            let ltype = check_types_node(session, scope.clone(), left, None);
            let rtype = check_types_node(session, scope.clone(), right, Some(ltype.clone()));
            expect_type(session, scope, Some(ltype), Some(rtype), Check::Def)?
        },

        AST::Import(_, _, _, ref decls) => {
            check_types_vec(session, scope.clone(), decls);
            //Type::Object(String::from("Nil"), vec!())
            scope.make_obj(session, String::from("Nil"), vec!())?
        },

        AST::Underscore => expected.unwrap_or_else(|| scope.new_typevar(session)),

        AST::Recall(_, _) => panic!("InternalError: Recall ast element shouldn't appear this early"),
        AST::Index(_, _, _, _) => panic!("InternalError: ast element shouldn't appear at this late phase: {:?}", node),
    };
    
    debug!("CHECK: {:?} {:?}", rtype, node);
    Ok(rtype)
}

pub fn get_access_ids(session: &Session, scope: ScopeRef, node: &AST) -> Result<Option<(NodeID, NodeID)>, Error> {
    match node {
        AST::Identifier(ref id, _, _) => {
            Ok(Some((*id, session.get_ref(*id)?)))
        },
        AST::Resolver(ref id, _, ref left, ref field) => {
            let ltype = session.get_type_from_ref(*id).unwrap();

            let vars = session.get_def(ltype.get_id()?)?.get_vars()?;
            Ok(Some((*id, vars.get_var_def(&field.name).ok_or(Error::new(format!("VarError: definition not set for {:?}", field.name)))?)))
        },
        AST::Accessor(ref id, _, ref left, ref field, ref oid) => {
            let ltype = resolve_type(session, check_types_node(session, scope.clone(), left, None));
            session.set_type(*oid, ltype.clone());

            match ltype {
                Type::Object(_, lid, _) => {
                    let vars = session.get_def(ltype.get_id()?)?.get_vars()?;
                    Ok(Some((*id, vars.get_var_def(&field.name).ok_or(Error::new(format!("VarError: definition not set for {:?}", field.name)))?)))
                },
                Type::Record(ref items) => {
                    let defid = NodeID::generate();
                    let index = items.iter().position(|(name, _)| *name == field.name).unwrap();
                    session.set_type(defid, items[index].1.clone());
                    Ok(Some((*id, defid)))
                },
                Type::Tuple(ref items) => {
                    let defid = NodeID::generate();
                    let index = field.name.parse::<usize>().unwrap();
                    session.set_type(defid, items[index].clone());
                    Ok(Some((*id, defid)))
                },
                _ => panic!("")
            }
        },
        _ => { Ok(None) },
    }
}

pub fn session_find_variant(session: &Session, scope: ScopeRef, invid: NodeID, fexpr: &AST, argtypes: &Type) -> Result<Type, Error> {
    let (refid, defid) = match get_access_ids(session, scope.clone(), fexpr)? {
        Some(ids) => ids,
        None => { return check_types_node_or_error(session, scope.clone(), fexpr, None); },
    };

    let def = session.get_def(defid)?;
    debug!("***: {:?} {:#?}", defid, def);
    let (fid, ftype) = match def {
        Def::Overload(ref ol) => ol.find_variant(session, scope.clone(), argtypes.clone())?,
        _ => (defid, session.get_type(defid).unwrap_or_else(|| scope.new_typevar(session)))
    };
    debug!("&&&: {:?} {:?} {:?}", fid, ftype, refid);

    session.set_ref(refid, fid);
    session.set_ref(invid, fid);
    Ok(ftype)
}




