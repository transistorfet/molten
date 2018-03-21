
use std::fmt::Debug;

use ast::AST;
use session::{ Session, Error };
use scope::{ self, Scope, ScopeRef };
use types::{ Type, Check, ABI, expect_type, resolve_type, find_variant, check_type_params };


pub fn check_types<V, T>(session: &Session<V, T>, scope: ScopeRef<V, T>, code: &mut Vec<AST>) -> Type where V: Clone + Debug, T: Clone + Debug {
    let ttype = check_types_vec(session, scope, code);
    if session.errors.get() > 0 {
        panic!("Exiting due to previous errors");
    }
    ttype
}

pub fn check_types_vec<V, T>(session: &Session<V, T>, scope: ScopeRef<V, T>, code: &mut Vec<AST>) -> Type where V: Clone + Debug, T: Clone + Debug {
    let mut last: Type = Type::Object(String::from("Nil"), vec!());
    for node in code {
        last = check_types_node(session, scope.clone(), node, None);
    }
    last
}

pub fn check_types_node<V, T>(session: &Session<V, T>, scope: ScopeRef<V, T>, node: &mut AST, expected: Option<Type>) -> Type where V: Clone + Debug, T: Clone + Debug {
    match check_types_node_or_error(session, scope.clone(), node, expected) {
        Ok(ttype) => ttype,
        Err(err) => {
            session.print_error(err.add_pos(&node.get_pos()));
            //Type::Object(String::from("Nil"), vec!())
            scope.borrow_mut().new_typevar()
        }
    }
}

pub fn check_types_node_or_error<V, T>(session: &Session<V, T>, scope: ScopeRef<V, T>, node: &mut AST, expected: Option<Type>) -> Result<Type, Error> where V: Clone + Debug, T: Clone + Debug {
    let rtype = match *node {
        AST::Boolean(_) => Type::Object(String::from("Bool"), vec!()),
        AST::Integer(_) => Type::Object(String::from("Int"), vec!()),
        AST::Real(_) => Type::Object(String::from("Real"), vec!()),
        AST::String(_) => Type::Object(String::from("String"), vec!()),

        AST::Function(_, ref mut name, ref mut args, ref mut rtype, ref mut body, ref id, ref abi) => {
            let fscope = session.map.get(id);

            let mut argtypes = vec!();
            for &(ref name, ref ttype, ref value) in &*args {
                let vtype = value.clone().map(|ref mut vexpr| check_types_node(session, scope.clone(), vexpr, ttype.clone()));
                let mut atype = expect_type(fscope.clone(), ttype.clone(), vtype, Check::Def)?;
                if &name[..] == "self" {
                    let mut stype = fscope.borrow().find_type(&String::from("Self")).unwrap();
                    stype = fscope.borrow_mut().map_all_typevars(stype);
                    atype = expect_type(fscope.clone(), Some(atype), Some(stype), Check::Def)?;
                }
                //fscope.borrow_mut().set_variable_type(name, atype.clone());
                Type::update_variable_type(fscope.clone(), name, atype.clone());
                argtypes.push(atype);
            }

            let rettype = expect_type(fscope.clone(), rtype.clone(), Some(check_types_node(session, fscope.clone(), body, rtype.clone())), Check::Def)?;
            *rtype = Some(rettype.clone());

            // Resolve type variables that can be
            for i in 0 .. argtypes.len() {
                argtypes[i] = resolve_type(fscope.clone(), argtypes[i].clone());
                // TODO this is still not checking for type compatibility
                //fscope.borrow_mut().set_variable_type(&args[i].0, argtypes[i].clone());
                Type::update_variable_type(fscope.clone(), &args[i].0, argtypes[i].clone());
                args[i].1 = Some(argtypes[i].clone());
            }
            update_scope_variable_types(fscope.clone());

            let mut nftype = Type::Function(argtypes.clone(), Box::new(rettype), abi.clone());
            if name.is_some() {
                let dscope = Scope::target(scope.clone());
                let dname = name.clone().unwrap();
                //if dscope.borrow().is_overloaded(&dname) {
                    let fname = abi.mangle_name(dname.as_str(), &argtypes, dscope.borrow().num_funcdefs(&dname));
                    if !dscope.borrow().contains(&fname) {
                        dscope.borrow_mut().define(fname.clone(), Some(nftype.clone()))?;
                        *name = Some(fname);
                    }
                //}
                Scope::add_func_variant(dscope.clone(), &dname, scope.clone(), nftype.clone())?;
            }
            nftype
        },

        AST::Invoke(_, ref mut fexpr, ref mut args, ref mut stype) => {
            let mut atypes = vec!();
            for ref mut value in args {
                atypes.push(check_types_node(session, scope.clone(), value, None));
            }

            let tscope = Scope::new_ref(Some(scope.clone()));
            let dtype = check_types_node(session, scope.clone(), fexpr, None);
            let mut etype = tscope.borrow_mut().map_all_typevars(dtype.clone());
            let etype = match etype.is_overloaded() {
                true => find_variant(tscope.clone(), etype, atypes.clone(), Check::Def)?,
                false => etype,
            };

            let ftype = match etype {
                Type::Function(ref args, _, ref abi) => {
                    /*
                    match **fexpr {
                        AST::Resolver(_, _, ref mut name) |
                        AST::Accessor(_, _, ref mut name, _) |
                        AST::Identifier(_, ref mut name) => {
                            *name = etype.get_abi().unwrap_or(ABI::Molten).mangle_name(name, args, dtype.num_funcdefs());
                        },
                        _ =>  { } //return Err(Error::new(format!("OverloadError: calling an overloaded function must be by name: not {:?}", fexpr))),
                    }
                    */
                    get_accessor_name(tscope.clone(), fexpr.as_mut(), &etype)?;

                    let ftype = expect_type(tscope.clone(), Some(etype.clone()), Some(Type::Function(atypes, Box::new(etype.get_rettype()?.clone()), abi.clone())), Check::Def)?;
                    // TODO should this actually be another expect, so type resolutions that occur in later args affect earlier args?  Might not be needed unless you add typevar constraints
                    let ftype = resolve_type(tscope.clone(), ftype);        // NOTE This ensures the early arguments are resolved despite typevars not being assigned until later in the signature

                    ftype
                },
                Type::Variable(_, ref id) => {
                    let ftype = Type::Function(atypes.clone(), Box::new(expected.unwrap_or_else(|| tscope.borrow_mut().new_typevar())), ABI::Unknown);
                    // TODO This is suspect... we might be updating type without checking for a conflict
                    // TODO we also aren't handling other function types, like accessor and resolve
                    //if let AST::Identifier(ref fname) = **fexpr {
                    //    update_type(scope.clone(), fname, ftype.clone());
                    //}
                    //tscope.borrow_mut().update_type(name, ftype.clone());
                    Type::update_type(tscope.clone(), &id.to_string(), ftype.clone());
                    ftype
                },
                _ => return Err(Error::new(format!("NotAFunction: {:?}", fexpr))),
            };

            *stype = Some(ftype.clone());
            ftype.get_rettype()?.clone()
        },

        AST::SideEffect(_, _, ref mut args) => {
            let mut ltype = None;
            for ref mut expr in args {
                ltype = Some(expect_type(scope.clone(), ltype.clone(), Some(check_types_node(session, scope.clone(), expr, ltype.clone())), Check::List)?);
            }
            ltype.unwrap()
        },

        AST::Definition(_, (ref name, ref mut ttype), ref mut body) => {
            //let dscope = Scope::target(scope.clone());
            let btype = expect_type(scope.clone(), ttype.clone(), Some(check_types_node(session, scope.clone(), body, ttype.clone())), Check::Def)?;
            //dscope.borrow_mut().set_variable_type(name, btype.clone());
            Type::update_variable_type(scope.clone(), name, btype.clone());
            *ttype = Some(btype.clone());
            btype
        },

        AST::Declare(_, ref _name, ref ttype) => {
            ttype.clone()
        },

        AST::Recall(_, ref name) |
        AST::Identifier(_, ref name) => {
            let mut bscope = scope.borrow_mut();
            bscope.get_variable_type(name).unwrap_or_else(|| expected.unwrap_or_else(|| bscope.new_typevar()))
        },

        AST::Block(_, ref mut body) => check_types_vec(session, scope, body),

        AST::If(_, ref mut cond, ref mut texpr, ref mut fexpr) => {
            // TODO should this require the cond type to be Bool?
            check_types_node(session, scope.clone(), cond, None);
            let ttype = check_types_node(session, scope.clone(), texpr, None);
            let ftype = check_types_node(session, scope.clone(), fexpr, Some(ttype.clone()));
            expect_type(scope.clone(), Some(ttype), Some(ftype), Check::List)?
        },

        AST::Try(_, ref mut cond, ref mut cases) |
        AST::Match(_, ref mut cond, ref mut cases) => {
            let mut ctype = Some(check_types_node(session, scope.clone(), cond, None));
            let mut rtype = None;
            for &mut (ref mut case, ref mut expr) in cases {
                ctype = Some(expect_type(scope.clone(), ctype.clone(), Some(check_types_node(session, scope.clone(), case, ctype.clone())), Check::List)?);
                rtype = Some(expect_type(scope.clone(), rtype.clone(), Some(check_types_node(session, scope.clone(), expr, rtype.clone())), Check::List)?);
            }
            rtype.unwrap()
        },

        AST::Raise(_, ref mut expr) => {
            // TODO should you check for a special error/exception type?
            check_types_node(session, scope.clone(), expr, None)
        },

        AST::While(_, ref mut cond, ref mut body) => {
            // TODO should this require the cond type to be Bool?
            check_types_node(session, scope.clone(), cond, None);
            check_types_node(session, scope.clone(), body, None);
            Type::Object(String::from("Nil"), vec!())
        },

        AST::For(_, ref name, ref mut list, ref mut body, ref id) => {
            let lscope = session.map.get(id);
            let itype = lscope.borrow().get_variable_type(name).unwrap_or_else(|| expected.unwrap_or_else(|| scope.borrow_mut().new_typevar()));
            let etype = Some(Type::Object(String::from("List"), vec!(itype)));
            let ltype = expect_type(lscope.clone(), etype.clone(), Some(check_types_node(session, lscope.clone(), list, etype.clone())), Check::Def)?;
            //lscope.borrow_mut().set_variable_type(name, ltype.get_params()[0].clone());
            Type::update_variable_type(lscope.clone(), name, ltype.get_params()?[0].clone());
            check_types_node(session, lscope.clone(), body, None)
        },

        AST::Nil(ref mut ttype) => {
            *ttype = Some(expected.unwrap_or_else(|| scope.borrow_mut().new_typevar()));
            ttype.clone().unwrap()
        },

        AST::List(_, ref mut items) => {
            let mut ltype = None;
            for ref mut expr in items {
                ltype = Some(expect_type(scope.clone(), ltype.clone(), Some(check_types_node(session, scope.clone(), expr, ltype.clone())), Check::List)?);
            }
            Type::Object(String::from("List"), vec!(ltype.unwrap_or_else(|| expected.unwrap_or_else(|| scope.borrow_mut().new_typevar()))))
        },

        AST::PtrCast(ref ttype, ref mut code) => {
            let ctype = check_types_node(session, scope.clone(), code, Some(ttype.clone()));
            debug!("PTRCAST: {:?} <- {:?}", ttype, ctype);
            // TODO is this quite right?
            expect_type(scope.clone(), Some(ttype.clone()), Some(ctype), Check::List)?;
            ttype.clone()
        },

        AST::New(_, (ref name, ref types)) => {
            let odtype = scope.borrow().find_type(name);
            match odtype {
                Some(dtype) => {
                    let tscope = Scope::new_ref(Some(scope.clone()));
                    let mtype = tscope.borrow_mut().map_all_typevars(dtype.clone());
                    check_type_params(scope.clone(), &mtype.get_params()?, types, Check::Def, false)?;
                },
                None => return Err(Error::new(format!("TypeError: undefined type {:?}", name))),
            };
            Type::Object(name.clone(), types.clone())
        },

        AST::Class(_, _, _, ref mut body, ref id) => {
            let tscope = session.map.get(id);
            check_types_vec(session, tscope.clone(), body);
            Type::Object(String::from("Nil"), vec!())
        },

        AST::Resolver(_, ref mut left, ref mut field) => {
            let ltype = match **left {
                // TODO this caused an issue with types that have typevars that aren't declared (ie. Buffer['item])
                //AST::Identifier(_, ref name) => resolve_type(scope.clone(), scope.borrow().find_type(name).unwrap().clone()),
                AST::Identifier(_, ref name) => scope.borrow().find_type(name).unwrap().clone(),
                _ => return Err(Error::new(format!("SyntaxError: left-hand side of scope resolver must be identifier")))
            };

            let classdef = scope.borrow().get_class_def(&ltype.get_name()?);
            let mut cborrow = classdef.borrow_mut();
            cborrow.get_variable_type(field).unwrap_or_else(|| expected.unwrap_or_else(|| scope.borrow_mut().new_typevar()))
        },

        AST::Accessor(_, ref mut left, ref mut field, ref mut stype) => {
            let ltype = resolve_type(scope.clone(), check_types_node(session, scope.clone(), left, None));
            *stype = Some(ltype.clone());

            let classdef = scope.borrow().get_class_def(&ltype.get_name()?);
            let mut cborrow = classdef.borrow_mut();
            cborrow.get_variable_type(field).unwrap_or_else(|| expected.unwrap_or_else(|| scope.borrow_mut().new_typevar()))
        },

        AST::Assignment(_, ref mut left, ref mut right) => {
            let ltype = check_types_node(session, scope.clone(), left, None);
            let rtype = check_types_node(session, scope.clone(), right, Some(ltype.clone()));
            expect_type(scope.clone(), Some(ltype), Some(rtype), Check::Def)?
        },

        AST::Import(_, _, ref mut decls) => {
            check_types_vec(session, scope.clone(), decls);
            Type::Object(String::from("Nil"), vec!())
        },

        AST::Noop => Type::Object(String::from("Nil"), vec!()),

        AST::Underscore => expected.unwrap_or_else(|| scope.borrow_mut().new_typevar()),

        AST::Type(_, _, _) => return Err(Error::new(format!("NotImplementedError: not yet supported, {:?}", node))),

        AST::Index(_, _, _, _) => panic!("InternalError: ast element shouldn't appear at this late phase: {:?}", node),
    };
    
    debug!("CHECK: {:?} {:?}", rtype, node);
    Ok(rtype)
}

pub fn get_accessor_name<V, T>(scope: ScopeRef<V, T>, fexpr: &mut AST, etype: &Type) -> Result<i32, Error> where V: Clone, T: Clone {
    let funcdefs = match *fexpr {
        AST::Resolver(_, ref left, ref mut name) => {
            let ltype = match **left {
                // TODO this caused an issue with types that have typevars that aren't declared (ie. Buffer['item])
                //AST::Identifier(_, ref name) => resolve_type(scope.clone(), scope.borrow().find_type(name).unwrap().clone()),
                AST::Identifier(_, ref name) => scope.borrow().find_type(name).unwrap().clone(),
                _ => return Err(Error::new(format!("SyntaxError: left-hand side of scope resolver must be identifier")))
            };

            let classdef = scope.borrow().get_class_def(&ltype.get_name()?);
debug!("!!!: {:?}", classdef.borrow().get_variable_type(name));
            let funcdefs = classdef.borrow().num_funcdefs(name);
            funcdefs
        },
        AST::Accessor(_, _, ref mut name, ref ltype) => {
            let classdef = scope.borrow().get_class_def(&ltype.as_ref().unwrap().get_name()?);
            let funcdefs = classdef.borrow().num_funcdefs(name);
            funcdefs
        },
        AST::Identifier(_, ref mut name) => {
            scope.borrow().num_funcdefs(name)
        },
        _ =>  { 0 } //return Err(Error::new(format!("OverloadError: calling an overloaded function must be by name: not {:?}", fexpr))),
    };

    match *fexpr {
        AST::Resolver(_, _, ref mut name) |
        AST::Accessor(_, _, ref mut name, _) |
        AST::Identifier(_, ref mut name) => {
            *name = etype.get_abi().unwrap_or(ABI::Molten).mangle_name(name, etype.get_argtypes()?, funcdefs);
            debug!("^^^^^^^^^^ MANGLE NAME {:?} {:?} {:?}", name, funcdefs, etype);
        },
        _ =>  { } //return Err(Error::new(format!("OverloadError: calling an overloaded function must be by name: not {:?}", fexpr))),
    }

    Ok(funcdefs)
}

pub fn update_scope_variable_types<V, T>(scope: ScopeRef<V, T>) where V: Clone, T: Clone {
    let dscope = Scope::target(scope.clone());
    let mut names = vec!();
    for name in dscope.borrow().names.keys() {
        names.push(name.clone());
    }

    for name in &names {
        let otype = dscope.borrow_mut().get_variable_type(name).unwrap().clone();
        let ntype = resolve_type(scope.clone(), otype);
        dscope.borrow_mut().set_variable_type(name, ntype);
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
        check_types_vec(map.clone(), map.get_global(), &mut code)
    }
}


