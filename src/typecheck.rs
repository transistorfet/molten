

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
        AST::Literal(ref id, ref literal) => {
            match literal {
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
                let vtype = arg.default.clone().map(|ref mut vexpr| check_types_node(session, scope.clone(), vexpr, arg.ttype.clone()));
                let mut atype = expect_type(session, fscope.clone(), arg.ttype.clone(), vtype, Check::Def)?;
                if &arg.ident.name[..] == "self" {
                    let mut stype = fscope.find_type(session, &String::from("Self")).unwrap();
                    stype = fscope.map_all_typevars(session, stype);
                    atype = expect_type(session, fscope.clone(), Some(atype), Some(stype), Check::Def)?;
                }
                session.update_type(fscope.clone(), arg.id, atype.clone());
                argtypes.push(atype);
            }

            let rettype = expect_type(session, fscope.clone(), rtype.clone(), Some(check_types_node(session, fscope.clone(), body, rtype.clone())), Check::Def)?;

            // Resolve type variables that can be
            for i in 0 .. argtypes.len() {
                argtypes[i] = resolve_type(session, fscope.clone(), argtypes[i].clone());
                session.update_type(fscope.clone(), args[i].id, argtypes[i].clone());
            }
            // TODO is this needed?
            //update_scope_variable_types(session, fscope.clone());

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
                    let ftype = resolve_type(session, tscope, ftype);        // NOTE This ensures the early arguments are resolved despite typevars not being assigned until later in the signature

                    ftype
                },
                Type::Variable(_, ref vid) => {
                    let ftype = Type::Function(Box::new(atypes), Box::new(expected.unwrap_or_else(|| tscope.new_typevar(session))), ABI::Unknown);
                    session.update_type(tscope, *vid, ftype.clone());
                    ftype
                },
                _ => return Err(Error::new(format!("NotAFunction: {:?}", fexpr))),
            };

            session.update_type(scope.clone(), *id, ftype.clone())?;
            ftype.get_rettype()?.clone()
        },

        AST::SideEffect(ref id, _, _, ref args) => {
            let mut ltype = None;
            for ref mut expr in args {
                ltype = Some(expect_type(session, scope.clone(), ltype.clone(), Some(check_types_node(session, scope.clone(), expr, ltype.clone())), Check::List)?);
            }
            ltype.unwrap()
        },

        AST::Definition(ref id, _, ref ident, ref ttype, ref body) => {
            let btype = expect_type(session, scope.clone(), ttype.clone(), Some(check_types_node(session, scope.clone(), body, ttype.clone())), Check::Def)?;
            session.update_type(scope.clone(), *id, btype.clone())?;
            btype
        },

        AST::Declare(ref id, _, ref _ident, ref ttype) => {
            session.update_type(scope.clone(), *id, ttype.clone())?;
            ttype.clone()
        },

        AST::Identifier(ref id, _, ref ident) => {
            //session.get_type_from_ref(*id)?;
            scope.get_variable_type(session, &ident.name).unwrap_or_else(|| expected.unwrap_or_else(|| scope.new_typevar(session)))
        },

        AST::Block(ref id, _, ref body) => check_types_vec(session, scope, body),

        AST::If(ref id, _, ref cond, ref texpr, ref fexpr) => {
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

        AST::Raise(ref id, _, ref expr) => {
            // TODO should you check for a special error/exception type?
            check_types_node(session, scope, expr, None)
        },

        AST::While(ref id, _, ref cond, ref body) => {
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
            session.update_type(lscope.clone(), *id, ltype.get_params()?[0].clone());
            check_types_node(session, lscope, body, None)
        },

        AST::Nil(ref id) => {
            let ttype = expected.unwrap_or_else(|| scope.new_typevar(session));
            session.set_type(*id, ttype.clone());
            ttype
        },

        AST::Tuple(ref id, _, ref items) => {
            let etypes = match expected {
                Some(ref e) => e.get_types()?.iter().map(|i| Some(i.clone())).collect(),
                None => vec![None; items.len()]
            };

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

        AST::List(ref id, _, ref items) => {
            let mut ltype = None;
            for ref expr in items {
                ltype = Some(expect_type(session, scope.clone(), ltype.clone(), Some(check_types_node(session, scope.clone(), expr, ltype.clone())), Check::List)?);
            }
            let ltype = ltype.unwrap_or_else(|| expected.unwrap_or_else(|| scope.new_typevar(session)));
            scope.make_obj(session, String::from("List"), vec!(ltype))?
        },

        AST::TypeDef(ref id, _, _, _) => return Err(Error::new(format!("NotImplementedError: not yet supported, {:?}", node))),

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
            //Type::Object(ident.name.clone(), types.clone())
            scope.make_obj(session, ident.name.clone(), types.clone())?
        },

        AST::Class(ref id, _, _, _, ref body) => {
            let tscope = session.map.get(id);
            check_types_vec(session, tscope.clone(), body);
            //Type::Object(String::from("Nil"), vec!())
            scope.make_obj(session, String::from("Nil"), vec!())?
        },

        AST::Resolver(ref id, _, ref left, ref field) => {
            let ltype = match **left {
                // TODO this caused an issue with types that have typevars that aren't declared (ie. Buffer['item])
                //AST::Identifier(_, ref ident) => resolve_type(session, scope.clone(), scope.find_type(session, &ident.name).unwrap().clone()),
                AST::Identifier(_, _, ref ident) => scope.find_type(session, &ident.name).unwrap().clone(),
                _ => return Err(Error::new(format!("SyntaxError: left-hand side of scope resolver must be identifier")))
            };

            let classvars = scope.find_type_def(session, &ltype.get_name()?)?.as_class()?.classvars.clone();
            let defid = classvars.get_var_def(&field.name).unwrap();
            session.set_ref(*id, defid);
            classvars.get_variable_type(session, &field.name).unwrap_or_else(|| expected.unwrap_or_else(|| scope.new_typevar(session)))
        },

        AST::Accessor(ref id, _, ref left, ref field, ref oid) => {
            let ltype = resolve_type(session, scope.clone(), check_types_node(session, scope.clone(), left, None));
            session.set_type(*oid, ltype.clone());

            let classvars = scope.find_type_def(session, &ltype.get_name()?)?.as_class()?.classvars.clone();
            let defid = classvars.get_var_def(&field.name).unwrap();
            session.set_ref(*id, defid);
            classvars.get_variable_type(session, &field.name).unwrap_or_else(|| expected.unwrap_or_else(|| scope.new_typevar(session)))
        },

        AST::Assignment(ref id, _, ref left, ref right) => {
            let ltype = check_types_node(session, scope.clone(), left, None);
            let rtype = check_types_node(session, scope.clone(), right, Some(ltype.clone()));
            expect_type(session, scope, Some(ltype), Some(rtype), Check::Def)?
        },

        AST::Import(ref id, _, _, ref decls) => {
            check_types_vec(session, scope.clone(), decls);
            //Type::Object(String::from("Nil"), vec!())
            scope.make_obj(session, String::from("Nil"), vec!())?
        },

        AST::Underscore => expected.unwrap_or_else(|| scope.new_typevar(session)),

        AST::Recall(ref id, _) => panic!("InternalError: Recall ast element shouldn't appear this early"),
        AST::Index(ref id, _, _, _) => panic!("InternalError: ast element shouldn't appear at this late phase: {:?}", node),
    };
    
    debug!("CHECK: {:?} {:?}", rtype, node);
    Ok(rtype)
}

pub fn session_find_variant(session: &Session, scope: ScopeRef, invid: NodeID, fexpr: &AST, argtypes: &Type) -> Result<Type, Error> {
    let (refid, defid) = match fexpr {
        AST::Identifier(ref id, _, _) => {
            (*id, session.get_ref(*id)?)
        },
        AST::Accessor(ref id, _, ref left, ref field, ref oid) => {
            let ltype = resolve_type(session, scope.clone(), check_types_node(session, scope.clone(), left, None));
            session.set_type(*oid, ltype.clone());

            let classvars = scope.find_type_def(session, &ltype.get_name()?)?.as_class()?.classvars.clone();
            (*id, classvars.get_var_def(&field.name).ok_or(Error::new(format!("VarError: definition not set for {:?}", field.name)))?)
        },
        AST::Resolver(ref id, _, ref left, ref field) => {
            let ltype = match **left {
                AST::Identifier(_, _, ref ident) => scope.find_type(session, &ident.name).unwrap().clone(),
                _ => return Err(Error::new(format!("SyntaxError: left-hand side of scope resolver must be identifier")))
            };

            let classvars = scope.find_type_def(session, &ltype.get_name()?)?.as_class()?.classvars.clone();
            (*id, classvars.get_var_def(&field.name).ok_or(Error::new(format!("VarError: definition not set for {:?}", field.name)))?)
        },
        _ => { return check_types_node_or_error(session, scope.clone(), fexpr, None); },
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


pub fn update_scope_variable_types(session: &Session, scope: ScopeRef) {
    let dscope = Scope::target(session, scope.clone());
    let mut names = vec!();
    for name in dscope.names.borrow().keys() {
        names.push(name.clone());
    }

    for name in &names {
        match dscope.get_variable_type(session, name) {
            Some(otype) => {
                let ntype = resolve_type(session, scope.clone(), otype.clone());
if otype != ntype { println!("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!: {:?} {:?}", otype, ntype); }
                match dscope.get_var_def(name) {
                    Some(defid) => { session.update_type(scope.clone(), defid, ntype).unwrap(); },
                    None => { },
                }
            },
            _ => { },
        }
    }
}



#[cfg(test)]
mod tests {
    use super::*;
    use parser::*;
    use scope::*;
    use session::Session;

    #[test]
    fn basic_types() {
        assert_eq!(
            typecheck("5 * 3 + 8 / 100".as_bytes()),
            parse_type("Int").unwrap()
        );
        assert_eq!(
            typecheck("let x = 2 + 4 * 7 - 1 / 20  == 10 - 50 * 12".as_bytes()),
            parse_type("Bool").unwrap()
        );
    }

    #[test]
    fn function_types() {
        assert_eq!(
            typecheck("fn x, y -> { let a = x * 1.0; y * y }".as_bytes()),
            parse_type("(Real, 'e) -> 'e").unwrap()
        );
    }

    #[test]
    #[should_panic]
    fn type_errors_basic() {
        typecheck("5 * 3.0 + 8 / 100".as_bytes());
    }

    #[test]
    #[should_panic]
    fn type_errors_mismatch() {
        typecheck("let b : Bool = 123.24".as_bytes());
    }

    fn typecheck(text: &[u8]) -> Type {
        let result = parse(text);
        let mut code = result.unwrap().1;
        let session = Session::new();
        let map: ScopeMapRef<()> = bind_names(&mut code);
        check_types_vec(map, map.get_global(), &mut code)
    }
}


