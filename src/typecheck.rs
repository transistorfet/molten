

use ast::{ ClassSpec, AST };
use session::{ Session, Error };
use scope::{ Scope, ScopeRef };
use types::{ Type, Check, ABI, expect_type, resolve_type, find_variant, check_type_params };


pub fn check_types(session: &Session, scope: ScopeRef, code: &mut Vec<AST>) -> Type {
    let ttype = check_types_vec(session, scope, code);
    if session.errors.get() > 0 {
        panic!("Exiting due to previous errors");
    }
    ttype
}

pub fn check_types_vec(session: &Session, scope: ScopeRef, code: &mut Vec<AST>) -> Type {
    let mut last: Type = Type::Object(String::from("Nil"), vec!());
    for node in code {
        last = check_types_node(session, scope.clone(), node, None);
    }
    last
}

pub fn check_types_node(session: &Session, scope: ScopeRef, node: &mut AST, expected: Option<Type>) -> Type {
    match check_types_node_or_error(session, scope.clone(), node, expected) {
        Ok(ttype) => ttype,
        Err(err) => {

            session.print_error(err.add_pos(&node.get_pos()));
            //Type::Object(String::from("Nil"), vec!())
            scope.new_typevar()
        }
    }
}

pub fn check_types_node_or_error(session: &Session, scope: ScopeRef, node: &mut AST, expected: Option<Type>) -> Result<Type, Error> {
    let rtype = match *node {
        AST::Boolean(_) => Type::Object(String::from("Bool"), vec!()),
        AST::Integer(_) => Type::Object(String::from("Int"), vec!()),
        AST::Real(_) => Type::Object(String::from("Real"), vec!()),
        AST::String(_) => Type::Object(String::from("String"), vec!()),

        AST::Function(ref id, _, ref mut ident, ref mut args, ref mut rtype, ref mut body, ref abi) => {
            let fscope = session.map.get(id);

            let mut argtypes = vec!();
            for arg in args.iter() {
                let vtype = arg.default.clone().map(|ref mut vexpr| check_types_node(session, scope.clone(), vexpr, arg.ttype.clone()));
                let mut atype = expect_type(session, fscope.clone(), arg.ttype.clone(), vtype, Check::Def)?;
                if &arg.ident.name[..] == "self" {
                    let mut stype = fscope.find_type(&String::from("Self")).unwrap();
                    stype = fscope.map_all_typevars(stype);
                    atype = expect_type(session, fscope.clone(), Some(atype), Some(stype), Check::Def)?;
                }
                //fscope.set_variable_type(&arg.ident.name, atype.clone());
                Type::update_variable_type(session, fscope.clone(), &arg.ident.name, atype.clone());
                argtypes.push(atype);
            }

            let rettype = expect_type(session, fscope.clone(), rtype.clone(), Some(check_types_node(session, fscope.clone(), body, rtype.clone())), Check::Def)?;
            *rtype = Some(rettype.clone());

            // Resolve type variables that can be
            for i in 0 .. argtypes.len() {
                argtypes[i] = resolve_type(fscope.clone(), argtypes[i].clone());
                // TODO this is still not checking for type compatibility
                //fscope.set_variable_type(&args[i].pos, argtypes[i].clone());
                Type::update_variable_type(session, fscope.clone(), &args[i].ident.name, argtypes[i].clone());
                args[i].ttype = Some(argtypes[i].clone());
            }
            update_scope_variable_types(session, fscope.clone());

            let tupleargs = Type::Tuple(argtypes);
            let mut nftype = Type::Function(Box::new(tupleargs.clone()), Box::new(rettype), *abi);
            if ident.is_some() {
                let dscope = Scope::target(session, scope.clone());
                let dname = ident.as_ref().unwrap().name.clone();
                let fname = abi.mangle_name(dname.as_str(), &tupleargs, dscope.num_funcdefs(session, &dname));
                if !dscope.contains(&fname) {
                    dscope.define(fname.clone(), Some(nftype.clone()))?;
                    ident.as_mut().unwrap().name = fname;
                }
                Scope::add_func_variant(session, dscope, &dname, scope, nftype.clone())?;
            }
            // TODO build closure context from body (all references)
            nftype
        },

        AST::Invoke(ref id, _, ref mut fexpr, ref mut args, ref mut stype) => {
            let mut atypes = vec!();
            for ref mut value in args {
                atypes.push(check_types_node(session, scope.clone(), value, None));
            }

            let tscope = Scope::new_ref(Some(scope.clone()));
            let dtype = check_types_node(session, scope.clone(), fexpr, None);
            let etype = match dtype {
                Type::Variable(_, _) => dtype.clone(),
                _ => tscope.map_all_typevars(dtype.clone()),
            };
            let etype = match etype.is_overloaded() {
                true => find_variant(session, tscope.clone(), etype, Type::Tuple(atypes.clone()), Check::Def)?,
                false => etype,
            };

            let ftype = match etype {
                Type::Function(ref args, _, ref abi) => {
                    /*
                    match **fexpr {
                        AST::Resolver(_, _, _, ref mut name) |
                        AST::Accessor(_, _, _, ref mut name, _) |
                        AST::Identifier(_, _, ref mut name) => {
                            *name = etype.get_abi().unwrap_or(ABI::Molten).mangle_name(name, args, dtype.num_funcdefs());
                        },
                        _ =>  { } //return Err(Error::new(format!("OverloadError: calling an overloaded function must be by name: not {:?}", fexpr))),
                    }
                    */
                    get_accessor_name(session, tscope.clone(), fexpr.as_mut(), &etype)?;

                    //let ftype = expect_type(session, tscope.clone(), Some(etype.clone()), Some(Type::Function(Box::new(Type::Tuple(atypes)), Box::new(expected.unwrap_or_else(|| tscope.new_typevar())), abi)), Check::Update)?;
                    let ftype = expect_type(session, tscope.clone(), Some(etype.clone()), Some(Type::Function(Box::new(Type::Tuple(atypes)), Box::new(etype.get_rettype()?.clone()), *abi)), Check::Def)?;
                    // TODO should this actually be another expect, so type resolutions that occur in later args affect earlier args?  Might not be needed unless you add typevar constraints
                    let ftype = resolve_type(tscope, ftype);        // NOTE This ensures the early arguments are resolved despite typevars not being assigned until later in the signature

                    ftype
                },
                Type::Variable(_, ref id) => {
                    let ftype = Type::Function(Box::new(Type::Tuple(atypes.clone())), Box::new(expected.unwrap_or_else(|| tscope.new_typevar())), ABI::Unknown);
                    // TODO This is suspect... we might be updating type without checking for a conflict
                    // TODO we also aren't handling other function types, like accessor and resolve
                    //if let AST::Identifier(ref id, ref fname) = **fexpr {
                    //    update_type(scope.clone(), fname, ftype.clone());
                    //}
                    //tscope.update_type(name, ftype.clone());
                    Type::update_type(session, tscope, &id.to_string(), ftype.clone());
                    ftype
                },
                _ => return Err(Error::new(format!("NotAFunction: {:?}", fexpr))),
            };

            *stype = Some(ftype.clone());
            ftype.get_rettype()?.clone()
        },

        AST::SideEffect(ref id, _, _, ref mut args) => {
            let mut ltype = None;
            for ref mut expr in args {
                ltype = Some(expect_type(session, scope.clone(), ltype.clone(), Some(check_types_node(session, scope.clone(), expr, ltype.clone())), Check::List)?);
            }
            ltype.unwrap()
        },

        AST::Definition(ref id, _, ref ident, ref mut ttype, ref mut body) => {
            //let dscope = Scope::target(session, scope);
            let btype = expect_type(session, scope.clone(), ttype.clone(), Some(check_types_node(session, scope.clone(), body, ttype.clone())), Check::Def)?;
            //dscope.set_variable_type(&ident.name, btype.clone());
            Type::update_variable_type(session, scope, &ident.name, btype.clone());
            *ttype = Some(btype.clone());
            btype
        },

        AST::Declare(ref id, _, ref _ident, ref ttype) => {
            ttype.clone()
        },

        AST::Recall(ref id, _, ref ident) |
        AST::Identifier(ref id, _, ref ident) => {
            scope.get_variable_type(session, &ident.name).unwrap_or_else(|| expected.unwrap_or_else(|| scope.new_typevar()))
        },

        AST::Block(ref id, _, ref mut body) => check_types_vec(session, scope, body),

        AST::If(ref id, _, ref mut cond, ref mut texpr, ref mut fexpr) => {
            // TODO should this require the cond type to be Bool?
            check_types_node(session, scope.clone(), cond, None);
            let ttype = check_types_node(session, scope.clone(), texpr, None);
            let ftype = check_types_node(session, scope.clone(), fexpr, Some(ttype.clone()));
            expect_type(session, scope, Some(ttype), Some(ftype), Check::List)?
        },

        AST::Try(ref id, _, ref mut cond, ref mut cases, ref mut condtype) |
        AST::Match(ref id, _, ref mut cond, ref mut cases, ref mut condtype) => {
            let mut ctype = Some(check_types_node(session, scope.clone(), cond, None));
            let mut rtype = None;
            for &mut (ref mut case, ref mut expr) in cases {
                ctype = Some(expect_type(session, scope.clone(), ctype.clone(), Some(check_types_node(session, scope.clone(), case, ctype.clone())), Check::List)?);
                rtype = Some(expect_type(session, scope.clone(), rtype.clone(), Some(check_types_node(session, scope.clone(), expr, rtype.clone())), Check::List)?);
            }
            *condtype = ctype;
            rtype.unwrap()
        },

        AST::Raise(ref id, _, ref mut expr) => {
            // TODO should you check for a special error/exception type?
            check_types_node(session, scope, expr, None)
        },

        AST::While(ref id, _, ref mut cond, ref mut body) => {
            // TODO should this require the cond type to be Bool?
            check_types_node(session, scope.clone(), cond, None);
            check_types_node(session, scope, body, None);
            Type::Object(String::from("Nil"), vec!())
        },

        AST::For(ref id, _, ref ident, ref mut list, ref mut body) => {
            let lscope = session.map.get(id);
            let itype = lscope.get_variable_type(session, &ident.name).unwrap_or_else(|| expected.unwrap_or_else(|| scope.new_typevar()));
            let etype = Some(Type::Object(String::from("List"), vec!(itype)));
            let ltype = expect_type(session, lscope.clone(), etype.clone(), Some(check_types_node(session, lscope.clone(), list, etype.clone())), Check::Def)?;
            //lscope.set_variable_type(&ident.name, ltype.get_params()[0].clone());
            Type::update_variable_type(session, lscope.clone(), &ident.name, ltype.get_params()?[0].clone());
            check_types_node(session, lscope, body, None)
        },

        AST::Nil(ref mut ttype) => {
            *ttype = Some(expected.unwrap_or_else(|| scope.new_typevar()));
            ttype.clone().unwrap()
        },

        AST::Tuple(ref id, _, ref mut items, ref mut stype) => {
            let etypes = match expected {
                Some(ref e) => e.get_types()?.iter().map(|i| Some(i.clone())).collect(),
                None => vec![None; items.len()]
            };

            if etypes.len() != items.len() {
                return Err(Error::new(format!("TypeError: number of tuple items don't match: expected {:?} with {} items but found {} items", expected, etypes.len(), items.len())));
            }
            let mut types = vec!();
            for (ref mut expr, etype) in items.iter_mut().zip(etypes.iter()) {
                types.push(expect_type(session, scope.clone(), etype.clone(), Some(check_types_node(session, scope.clone(), expr, etype.clone())), Check::List)?);
            }
            *stype = Some(Type::Tuple(types.clone()));
            Type::Tuple(types)
        },

        AST::List(ref id, _, ref mut items, ref mut stype) => {
            let mut ltype = None;
            for ref mut expr in items {
                ltype = Some(expect_type(session, scope.clone(), ltype.clone(), Some(check_types_node(session, scope.clone(), expr, ltype.clone())), Check::List)?);
            }
            let ltype = ltype.unwrap_or_else(|| expected.unwrap_or_else(|| scope.new_typevar()));
            *stype = Some(ltype.clone());
            Type::Object(String::from("List"), vec!(ltype))
        },

        AST::TypeDef(ref id, _, _, _) => return Err(Error::new(format!("NotImplementedError: not yet supported, {:?}", node))),

        AST::PtrCast(ref ttype, ref mut code) => {
            let ctype = check_types_node(session, scope.clone(), code, Some(ttype.clone()));
            debug!("PTRCAST: {:?} <- {:?}", ttype, ctype);
            // TODO is this quite right?
            expect_type(session, scope, Some(ttype.clone()), Some(ctype), Check::List)?;
            ttype.clone()
        },

        AST::New(ref id, _, ClassSpec { ref ident, ref types, .. }) => {
            let odtype = scope.find_type(&ident.name);
            match odtype {
                Some(dtype) => {
                    let tscope = Scope::new_ref(Some(scope.clone()));
                    let mtype = tscope.map_all_typevars(dtype.clone());
                    check_type_params(session, scope, &mtype.get_params()?, types, Check::Def, false)?;
                },
                None => return Err(Error::new(format!("TypeError: undefined type {:?}", ident.name))),
            };
            Type::Object(ident.name.clone(), types.clone())
        },

        AST::Class(ref id, _, _, _, ref mut body) => {
            let tscope = session.map.get(id);
            check_types_vec(session, tscope.clone(), body);
            Type::Object(String::from("Nil"), vec!())
        },

        AST::Resolver(ref id, _, ref mut left, ref mut field) => {
            let ltype = match **left {
                // TODO this caused an issue with types that have typevars that aren't declared (ie. Buffer['item])
                //AST::Identifier(_, ref ident) => resolve_type(scope.clone(), scope.find_type(&ident.name).unwrap().clone()),
                AST::Identifier(_, _, ref ident) => scope.find_type(&ident.name).unwrap().clone(),
                _ => return Err(Error::new(format!("SyntaxError: left-hand side of scope resolver must be identifier")))
            };

            let classvars = session.find_type_def(scope.clone(), &ltype.get_name()?)?.as_class()?.classvars.clone();
            classvars.get_variable_type(session, &field.name).unwrap_or_else(|| expected.unwrap_or_else(|| scope.new_typevar()))
        },

        AST::Accessor(ref id, _, ref mut left, ref mut field, ref mut stype) => {
            let ltype = resolve_type(scope.clone(), check_types_node(session, scope.clone(), left, None));
            *stype = Some(ltype.clone());

            let classvars = session.find_type_def(scope.clone(), &ltype.get_name()?)?.as_class()?.classvars.clone();
            classvars.get_variable_type(session, &field.name).unwrap_or_else(|| expected.unwrap_or_else(|| scope.new_typevar()))
        },

        AST::Assignment(ref id, _, ref mut left, ref mut right) => {
            let ltype = check_types_node(session, scope.clone(), left, None);
            let rtype = check_types_node(session, scope.clone(), right, Some(ltype.clone()));
            expect_type(session, scope, Some(ltype), Some(rtype), Check::Def)?
        },

        AST::Import(ref id, _, _, ref mut decls) => {
            check_types_vec(session, scope, decls);
            Type::Object(String::from("Nil"), vec!())
        },

        AST::Underscore => expected.unwrap_or_else(|| scope.new_typevar()),

        AST::Index(ref id, _, _, _, _) => panic!("InternalError: ast element shouldn't appear at this late phase: {:?}", node),
    };
    
    debug!("CHECK: {:?} {:?}", rtype, node);
    Ok(rtype)
}

pub fn get_accessor_name(session: &Session, scope: ScopeRef, fexpr: &mut AST, etype: &Type) -> Result<i32, Error> {
    let funcdefs = match *fexpr {
        AST::Resolver(ref id, _, ref left, ref mut ident) => {
            let ltype = match **left {
                // TODO this caused an issue with types that have typevars that aren't declared (ie. Buffer['item])
                //AST::Identifier(ref id, _, ref ident) => resolve_type(scope.clone(), scope.find_type(&ident.name).unwrap().clone()),
                AST::Identifier(ref id, _, ref ident) => scope.find_type(&ident.name).unwrap().clone(),
                _ => return Err(Error::new(format!("SyntaxError: left-hand side of scope resolver must be identifier")))
            };

            let classvars = session.find_type_def(scope.clone(), &ltype.get_name()?)?.as_class()?.classvars.clone();
debug!("!!!: {:?}", classvars.get_variable_type(session, &ident.name));
            let funcdefs = classvars.num_funcdefs(session, &ident.name);
            funcdefs
        },
        AST::Accessor(ref id, _, _, ref mut ident, ref ltype) => {
            let classvars = session.find_type_def(scope.clone(), &ltype.as_ref().unwrap().get_name()?)?.as_class()?.classvars.clone();
            let funcdefs = classvars.num_funcdefs(session, &ident.name);
            funcdefs
        },
        AST::Identifier(ref id, _, ref mut ident) => {
            scope.num_funcdefs(session, &ident.name)
        },
        _ =>  { 0 } //return Err(Error::new(format!("OverloadError: calling an overloaded function must be by &ident.name: not {:?}", fexpr))),
    };

    match *fexpr {
        AST::Resolver(ref id, _, _, ref mut ident) |
        AST::Accessor(ref id, _, _, ref mut ident, _) |
        AST::Identifier(ref id, _, ref mut ident) => {
            ident.name = etype.get_abi().unwrap_or(ABI::Molten).mangle_name(&ident.name, etype.get_argtypes()?, funcdefs);
            debug!("^^^^^^^^^^ MANGLE NAME {:?} {:?} {:?}", &ident.name, funcdefs, etype);
        },
        _ =>  { } //return Err(Error::new(format!("OverloadError: calling an overloaded function must be by &ident.name: not {:?}", fexpr))),
    }

    Ok(funcdefs)
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
                let ntype = resolve_type(scope.clone(), otype);
                dscope.set_variable_type(name, ntype);
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


