

use std::fmt::Debug;

use parser::AST;
use scope::{ Scope, ScopeRef, ScopeMapRef, mangle_name };
use utils::UniqueID;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Object(String, Vec<Type>),
    Variable(String),
    Function(Vec<Type>, Box<Type>),
    Overload(Vec<Type>),
    //Generic(String, Vec<Type>),
    //Constrained(Box<&mut AST>),
}

impl Type {
    pub fn get_name(&self) -> String {
        match *self {
            Type::Object(ref name, _) => name.clone(),
            _ => panic!("TypeError: expected a class or concrete type, found {:?}", self),
        }
    }

    pub fn get_params(&self) -> Vec<Type> {
        match *self {
            Type::Object(_, ref params) => params.clone(),
            _ => panic!("TypeError: expected a class or concrete type, found {:?}", self),
        }
    }

    pub fn get_argtypes(&self) -> &Vec<Type> {
        match self {
            &Type::Function(ref args, _) => args,
            _ => panic!("TypeError: expected function, found {:?}", self),
        }
    }

    pub fn get_rettype(&self) -> &Type {
        match self {
            &Type::Function(_, ref ret) => &**ret,
            _ => panic!("TypeError: expected function, found {:?}", self),
        }
    }

    pub fn is_overloaded(&self) -> bool {
        match *self {
            Type::Overload(_) => true,
            _ => false
        }
    }

    pub fn get_variants(&self) -> Vec<Type> {
        match self {
            &Type::Overload(ref variants) => variants.clone(),
            _ => vec!(self.clone()),
        }
    }

    pub fn add_variant<V, T>(self, scope: ScopeRef<V, T>, ntype: Type) -> Type where V: Clone, T: Clone {
        let rtype = match self {
            Type::Overload(ref variants) => {
                for variant in variants {
                    if check_type(scope.clone(), Some(variant.clone()), Some(ntype.clone()), Check::Def, false).is_ok() {
                        panic!("OverloadError: function definitions overlap; {:?} vs {:?}", variant, ntype);
                    }
                }
                let mut nvariants = variants.clone();
                nvariants.push(ntype);
                Type::Overload(nvariants)
            },
            _ => expect_type(scope.clone(), Some(self), Some(ntype.clone()), Check::Def),
        };
        rtype
    }

    pub fn make_object(nametypes: (String, Vec<Type>)) -> Type {
        Type::Object(nametypes.0, nametypes.1)
    }

}


pub fn check_types<V, T>(map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>, code: &mut Vec<AST>) -> Type where V: Clone + Debug, T: Clone + Debug {
    let mut last: Type = Type::Object(String::from("Nil"), vec!());
    for node in code {
        last = check_types_node(map.clone(), scope.clone(), node, None);
    }
    last
}

pub fn check_types_node<V, T>(map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>, node: &mut AST, expected: Option<Type>) -> Type where V: Clone + Debug, T: Clone + Debug {
    let x = match *node {
        AST::Boolean(_) => Type::Object(String::from("Bool"), vec!()),
        AST::Integer(_) => Type::Object(String::from("Int"), vec!()),
        AST::Real(_) => Type::Object(String::from("Real"), vec!()),
        AST::String(_) => Type::Object(String::from("String"), vec!()),

        AST::Function(ref mut name, ref mut args, ref mut rtype, ref mut body, ref id) => {
            let fscope = map.get(id);

            let mut argtypes = vec!();
            for &(ref name, ref ttype, ref value) in &*args {
                let vtype = value.clone().map(|ref mut vexpr| check_types_node(map.clone(), scope.clone(), vexpr, ttype.clone()));
                let mut atype = expect_type(fscope.clone(), ttype.clone(), vtype, Check::Def);
                if &name[..] == "self" {
                    let mut stype = fscope.borrow().find_type(&String::from("Self")).unwrap();
                    stype = fscope.borrow_mut().map_all_typevars(stype);
                    atype = expect_type(fscope.clone(), Some(atype), Some(stype), Check::Def);
                }
                fscope.borrow_mut().update_variable_type(name, atype.clone());
                //Type::update_variable_type(fscope.clone(), name, atype.clone());
                argtypes.push(atype);
            }

            let rettype = expect_type(fscope.clone(), rtype.clone(), Some(check_types_node(map.clone(), fscope.clone(), body, rtype.clone())), Check::Def);
            *rtype = Some(rettype.clone());

            // Resolve type variables that can be
            for i in 0 .. argtypes.len() {
                argtypes[i] = resolve_type(fscope.clone(), argtypes[i].clone());
                // TODO this is still not checking for type compatibility
                fscope.borrow_mut().update_variable_type(&args[i].0, argtypes[i].clone());
                //Type::update_variable_type(fscope.clone(), &args[i].0, argtypes[i].clone());
                args[i].1 = Some(argtypes[i].clone());
            }
            update_scope_variable_types(fscope.clone());
            // TODO this fixes the type error, but makes a ton of stupid typevars
            //scope.borrow_mut().raise_types(fscope.clone());

            println!("FUNC: {:?} {:?} {:?}", name, argtypes, rettype);
            let mut nftype = Type::Function(argtypes.clone(), Box::new(rettype));
            if name.is_some() {
                let dscope = Scope::target(scope.clone());
                let dname = name.clone().unwrap();
                if dscope.borrow().is_overloaded(&dname) {
                    let fname = mangle_name(&dname, &argtypes);
                    dscope.borrow_mut().define(fname.clone(), Some(nftype.clone()));
                    *name = Some(fname);
                }

                //let mut otype = dscope.borrow().get_variable_type(&dname);
                //nftype = otype.map(|ttype| ttype.add_variant(scope.clone(), nftype.clone())).unwrap_or(nftype);
                //dscope.borrow_mut().update_variable_type(&dname, nftype.clone());
                Scope::add_func_variant(dscope.clone(), &dname, scope.clone(), nftype.clone());
            }
            nftype
        },

        AST::Invoke(ref mut fexpr, ref mut args, ref mut stype) => {
            let mut atypes = vec!();
            for ref mut value in args {
                atypes.push(check_types_node(map.clone(), scope.clone(), value, None));
            }

            let tscope = Scope::new_ref(Some(scope.clone()));
            let mut etype = check_types_node(map.clone(), scope.clone(), fexpr, None);
            etype = tscope.borrow_mut().map_all_typevars(etype.clone());

            if let Type::Overload(_) = etype {
                etype = find_variant(tscope.clone(), etype, atypes.clone());
                match **fexpr {
                    AST::Resolver(_, ref mut name) |
                    AST::Accessor(_, ref mut name, _) |
                    AST::Identifier(ref mut name) => *name = mangle_name(name, etype.get_argtypes()),
                    _ => panic!("OverloadError: call to overloaded method not allowed here"),
                }
            }

            let ftype = match etype {
                Type::Function(_, _) => {
                    let ftype = expect_type(tscope.clone(), Some(etype.clone()), Some(Type::Function(atypes, Box::new(etype.get_rettype().clone()))), Check::Def);
                    // TODO should this actually be another expect, so type resolutions that occur in later args affect earlier args?  Might not be needed unless you add typevar constraints
                    let ftype = resolve_type(tscope.clone(), ftype);        // NOTE This ensures the early arguments are resolved despite typevars not being assigned until later in the signature

                    ftype
                },
                Type::Variable(ref name) => {
                    let ftype = Type::Function(atypes.clone(), Box::new(expected.unwrap_or_else(|| tscope.borrow_mut().new_typevar())));
                    // TODO This is suspect... we might be updating type without checking for a conflict
                    // TODO we also aren't handling other function types, like accessor and resolve
                    //if let AST::Identifier(ref fname) = **fexpr {
                    //    update_type(scope.clone(), fname, ftype.clone());
                    //}
                    tscope.borrow_mut().update_type(name, ftype.clone());
                    //Type::update_type(scope.clone(), name, ftype.clone());
                    ftype
                },
                _ => panic!("Not a function: {:?}", fexpr),
            };

            scope.borrow_mut().raise_types(tscope.clone());
            *stype = Some(ftype.clone());
            ftype.get_rettype().clone()
        },

        AST::SideEffect(_, ref mut args) => {
            let mut ltype = None;
            for ref mut expr in args {
                ltype = Some(expect_type(scope.clone(), ltype.clone(), Some(check_types_node(map.clone(), scope.clone(), expr, ltype.clone())), Check::List));
            }
            ltype.unwrap()
        },

        AST::Definition((ref name, ref mut ttype), ref mut body) => {
            let dscope = Scope::target(scope.clone());
            let btype = expect_type(scope.clone(), ttype.clone(), Some(check_types_node(map.clone(), scope.clone(), body, ttype.clone())), Check::Def);
            dscope.borrow_mut().update_variable_type(name, btype.clone());
            //Type::update_variable_type(dscope.clone(), name, btype.clone());
            *ttype = Some(btype.clone());
            btype
        },

        AST::Declare(ref name, ref ttype) => {
            //let dscope = Scope::target(scope.clone());
            //dscope.borrow_mut().update_variable_type(name, ttype.clone());
            ttype.clone()
        },

        AST::Identifier(ref name) => {
            let mut bscope = scope.borrow_mut();
            bscope.get_variable_type(name).unwrap_or_else(|| expected.unwrap_or_else(|| bscope.new_typevar()))
        },

        AST::Block(ref mut body) => check_types(map, scope, body),

        AST::If(ref mut cond, ref mut texpr, ref mut fexpr) => {
            // TODO should this require the cond type to be Bool?
            check_types_node(map.clone(), scope.clone(), cond, None);
            let ttype = check_types_node(map.clone(), scope.clone(), texpr, None);
            let ftype = check_types_node(map.clone(), scope.clone(), fexpr, Some(ttype.clone()));
            expect_type(scope.clone(), Some(ttype), Some(ftype), Check::List)
        },

        AST::Try(ref mut cond, ref mut cases) |
        AST::Match(ref mut cond, ref mut cases) => {
            let mut ctype = Some(check_types_node(map.clone(), scope.clone(), cond, None));
            let mut rtype = None;
            for &mut (ref mut case, ref mut expr) in cases {
                ctype = Some(expect_type(scope.clone(), ctype.clone(), Some(check_types_node(map.clone(), scope.clone(), case, ctype.clone())), Check::List));
                rtype = Some(expect_type(scope.clone(), rtype.clone(), Some(check_types_node(map.clone(), scope.clone(), expr, rtype.clone())), Check::List));
            }
            rtype.unwrap()
        },

        AST::Raise(ref mut expr) => {
            // TODO should you check for a special error/exception type?
            check_types_node(map.clone(), scope.clone(), expr, None)
        },

        AST::While(ref mut cond, ref mut body) => {
            // TODO should this require the cond type to be Bool?
            check_types_node(map.clone(), scope.clone(), cond, None);
            check_types_node(map.clone(), scope.clone(), body, None);
            Type::Object(String::from("Nil"), vec!())
        },

        AST::For(ref name, ref mut list, ref mut body, ref id) => {
            let lscope = map.get(id);
            let itype = lscope.borrow().get_variable_type(name).unwrap_or_else(|| expected.unwrap_or_else(|| scope.borrow_mut().new_typevar()));
            let etype = Some(Type::Object(String::from("List"), vec!(itype)));
            let ltype = expect_type(lscope.clone(), etype.clone(), Some(check_types_node(map.clone(), lscope.clone(), list, etype.clone())), Check::Def);
            lscope.borrow_mut().update_variable_type(name, ltype.get_params()[0].clone());
            //Type::update_variable_type(lscope.clone(), name, ltype);
            check_types_node(map.clone(), lscope.clone(), body, None)
        },

        AST::Nil(ref mut ttype) => {
            *ttype = Some(expected.unwrap_or_else(|| scope.borrow_mut().new_typevar()));
            ttype.clone().unwrap()
        },

        AST::List(ref mut items) => {
            let mut ltype = None;
            for ref mut expr in items {
                ltype = Some(expect_type(scope.clone(), ltype.clone(), Some(check_types_node(map.clone(), scope.clone(), expr, ltype.clone())), Check::List));
            }
            Type::Object(String::from("List"), vec!(ltype.unwrap_or_else(|| expected.unwrap_or_else(|| scope.borrow_mut().new_typevar()))))
        },

        AST::New((ref name, ref types)) => {
            let odtype = scope.borrow().find_type(name);
            match odtype {
                Some(dtype) => {
                    let tscope = Scope::new_ref(Some(scope.clone()));
                    let mtype = tscope.borrow_mut().map_all_typevars(dtype.clone());
                    if let Err(msg) = check_type_params(tscope.clone(), &mtype.get_params(), types, Check::Def, false) {
                        panic!(msg);
                    }
                    scope.borrow_mut().raise_types(tscope.clone());
                },
                None => panic!("TypeError: undefined type {:?}", name),
            };
            Type::Object(name.clone(), types.clone())
        },

        AST::Class((ref name, ref types), ref parent, ref mut body, ref id) => {
            let tscope = map.get(id);
            check_types(map.clone(), tscope.clone(), body);
            Type::Object(String::from("Nil"), vec!())
        },

        AST::Resolver(ref mut left, ref mut field) => {
            let ltype = match **left {
                // TODO this caused an issue with types that have typevars that aren't declared (ie. Buffer['item])
                //AST::Identifier(ref name) => resolve_type(scope.clone(), scope.borrow().find_type(name).unwrap().clone()),
                AST::Identifier(ref name) => scope.borrow().find_type(name).unwrap().clone(),
                _ => panic!("SyntaxError: left-hand side of scope resolver must be identifier")
            };

            let classdef = scope.borrow().get_class_def(&ltype.get_name());
            let mut cborrow = classdef.borrow_mut();
            cborrow.get_variable_type(field).unwrap_or_else(|| expected.unwrap_or_else(|| cborrow.new_typevar()))
        },

        AST::Accessor(ref mut left, ref mut field, ref mut stype) => {
            let ltype = resolve_type(scope.clone(), check_types_node(map.clone(), scope.clone(), left, None));
            *stype = Some(ltype.clone());

            let classdef = scope.borrow().get_class_def(&ltype.get_name());
            let mut cborrow = classdef.borrow_mut();
            cborrow.get_variable_type(field).unwrap_or_else(|| expected.unwrap_or_else(|| cborrow.new_typevar()))
        },

        AST::Assignment(ref mut left, ref mut right) => {
            let ltype = check_types_node(map.clone(), scope.clone(), left, None);
            let rtype = check_types_node(map.clone(), scope.clone(), right, Some(ltype.clone()));
            expect_type(scope.clone(), Some(ltype), Some(rtype), Check::Def)
        },

        AST::Import(_, ref mut decls) => {
            check_types(map.clone(), scope.clone(), decls);
            Type::Object(String::from("Nil"), vec!())
        },

        AST::Noop => Type::Object(String::from("Nil"), vec!()),

        AST::Underscore => expected.unwrap_or_else(|| scope.borrow_mut().new_typevar()),

        AST::Type(_, _) => panic!("NotImplementedError: not yet supported, {:?}", node),

        AST::Index(_, _, _) => panic!("InternalError: ast element shouldn't appear at this late phase: {:?}", node),
    };
    
    println!("CHECK: {:?} {:?}", x, node);
    x
}


#[derive(Clone)]
pub enum Check {
    Def,
    List,
}

pub fn expect_type<V, T>(scope: ScopeRef<V, T>, ottype: Option<Type>, obtype: Option<Type>, mode: Check) -> Type where V: Clone, T: Clone {
    match check_type(scope, ottype, obtype, mode, true) {
        Ok(ttype) => ttype,
        Err(msg) => panic!(msg),
    }
}

//
// Compare the defined type to the calculated type and return the common type, or error if they do not match
// dtype < ctype: ie. ctype will be downcast to match dtype, but not the inverse
//
pub fn check_type<V, T>(scope: ScopeRef<V, T>, odtype: Option<Type>, octype: Option<Type>, mode: Check, update: bool) -> Result<Type, String> where V: Clone, T: Clone {
    if odtype.is_none() {
        // TODO should the else case just be Nil... 
        Ok(octype.unwrap_or_else(|| scope.borrow_mut().new_typevar()))
    } else if octype.is_none() {
        Ok(odtype.unwrap())
    } else {
        let dtype = resolve_type(scope.clone(), odtype.unwrap());
        let ctype = resolve_type(scope.clone(), octype.unwrap());

        if let Type::Variable(ref name) = dtype {
            println!("UPDATING: {:?} {:?}", name, dtype);
            if update { scope.borrow_mut().update_type(name, ctype.clone()); }
            //if update { Type::update_type(scope.clone(), name, ctype.clone()); }
            Ok(ctype)
        } else if let Type::Variable(ref name) = ctype {
            println!("UPDATING: {:?} {:?}", name, dtype);
            if update { scope.borrow_mut().update_type(name, dtype.clone()); }
            //if update { Type::update_type(scope.clone(), name, dtype.clone()); }
            Ok(dtype)
        } else {
            match (dtype.clone(), ctype.clone()) {
                //(Type::List(ref a), Type::List(ref b)) => Ok(Type::List(Box::new(check_type(scope.clone(), Some(*a.clone()), Some(*b.clone()), mode, update)?))),
                (Type::Function(ref aargs, ref aret), Type::Function(ref bargs, ref bret)) => {
                    if aargs.len() == bargs.len() {
                        let mut argtypes = vec!();
                        for (atype, btype) in aargs.iter().zip(bargs.iter()) {
                            argtypes.push(check_type(scope.clone(), Some(atype.clone()), Some(btype.clone()), mode.clone(), update)?);
                        }
                        Ok(Type::Function(argtypes, Box::new(check_type(scope.clone(), Some(*aret.clone()), Some(*bret.clone()), mode, update)?)))
                    } else {
                        Err(format!("TypeError: type mismatch, expected {:?} but found {:?}", dtype, ctype))
                    }
                },
                (Type::Object(ref aname, ref atypes), Type::Object(ref bname, ref btypes)) => {
                    match is_subclass_of(scope.clone(), (bname, btypes), (aname, atypes), mode.clone(), true) {
                        ok @ Ok(_) => ok,
                        err @ Err(_) => match mode {
                            Check::List => is_subclass_of(scope.clone(), (aname, atypes), (bname, btypes), mode.clone(), true),
                            _ => err,
                        }
                    }
                },
                (_, Type::Overload(ref variants)) |
                (Type::Overload(ref variants), _) => Err(format!("TypeError: overloaded types are not allowed here...")),
                _ => {
                    if dtype == ctype {
                        Ok(dtype)
                    } else {
                        Err(format!("TypeError: type mismatch, expected {:?} but found {:?}", dtype, ctype))
                    }
                }
            }
        }
    }
}


fn is_subclass_of<V, T>(scope: ScopeRef<V, T>, adef: (&String, &Vec<Type>), bdef: (&String, &Vec<Type>), mode: Check, update: bool) -> Result<Type, String> where V: Clone, T: Clone {
    let tscope = Scope::new_ref(Some(scope.clone()));
    let mut adef = (adef.0.clone(), adef.1.clone());

    loop {
        let (mut class, parent) = scope.borrow().get_class_info(&adef.0).unwrap();
        let mut names = Scope::<V, T>::map_new();
        if update {
            class = tscope.borrow_mut().map_typevars(&mut names, class);
        }
        adef.1 = check_type_params(tscope.clone(), &class.get_params(), &adef.1, mode.clone(), true)?;

        if *bdef.0 == adef.0 {
            if bdef.1.len() > 0 || adef.1.len() > 0 {
                let ptypes = check_type_params(tscope.clone(), &adef.1, bdef.1, mode.clone(), true)?;
                if update {
                    scope.borrow_mut().raise_types(tscope.clone());
                }
                println!("GENERIC CHECK WORKING: {:?}", ptypes);
                return Ok(Type::Object(adef.0, ptypes));
            } else {
                return Ok(Type::Object(adef.0, vec!()));
            }
        }
        if parent.is_none() {
            return Err(format!("TypeError: type mismatch, expected {:?} but found {:?}", Type::Object(adef.0.clone(), adef.1), Type::Object(bdef.0.clone(), bdef.1.clone())));
        }
        let parent = if update {
            tscope.borrow_mut().map_typevars(&mut names, parent.unwrap())
        } else {
            parent.unwrap()
        };
        match resolve_type(tscope.clone(), parent) {
            Type::Object(name, params) => adef = (name, params),
            ttype @ _ => return Err(format!("TypeError: expected Object but found {:?}", ttype)),
        }
    }
}

pub fn check_type_params<V, T>(scope: ScopeRef<V, T>, dtypes: &Vec<Type>, ctypes: &Vec<Type>, mode: Check, update: bool) -> Result<Vec<Type>, String> where V: Clone, T: Clone {
    if dtypes.len() != ctypes.len() {
        Err(format!("TypeError: number of type parameters don't match: expected {:?} but found {:?}", dtypes, ctypes))
    } else {
        let mut ptypes = vec!();
        for (dtype, ctype) in dtypes.iter().zip(ctypes.iter()) {
            ptypes.push(check_type(scope.clone(), Some(dtype.clone()), Some(ctype.clone()), mode.clone(), update)?);
        }
        Ok(ptypes)
    }
}

pub fn resolve_type<V, T>(scope: ScopeRef<V, T>, ttype: Type) -> Type where V: Clone, T: Clone {
    match ttype {
        Type::Object(ref name, ref types) => {
            match scope.borrow().find_type(name) {
                Some(x) => {
                    let params = types.iter().map(|ptype| resolve_type(scope.clone(), ptype.clone())).collect();
                    // TODO we are purposely returning the original type here so as not to over-resolve types... but we should probably still fully resolve for checking purposes
                    //x
                    //ttype.clone()
                    Type::Object(name.clone(), params)
                },
                None => panic!("TypeError: undefined type {:?}", name),
            }
        },
        Type::Variable(ref name) => {
println!("VARIABLE: {:?} {:?} {:?}", scope.borrow().get_full_name(&Some(String::from("")), UniqueID(0)), name, scope.borrow().find_type(name));
            match scope.borrow().find_type(name) {
                Some(vtype) => {
                    if Type::Variable(name.clone()) == vtype {
                        vtype
                    } else {
                        resolve_type(scope.clone(), vtype)
                    }
                },
                // TODO I changed this because I was getting a bunch of errors... maybe i need better rules on typevars?
                //None => Type::Variable(name.clone()),//panic!("TypeError: undefined type variable {:?}", name),
                None => panic!("TypeError: undefined type variable {:?}", name),
            }
        },
        //Type::List(ref ttype) => {
        //    Type::List(Box::new(resolve_type(scope.clone(), *ttype.clone())))
        //},
        Type::Function(ref args, ref ret) => {
            let argtypes = args.iter().map(|arg| resolve_type(scope.clone(), arg.clone())).collect();
            Type::Function(argtypes, Box::new(resolve_type(scope.clone(), *ret.clone())))
        },
        Type::Overload(ref variants) => {
            let newvars = variants.iter().map(|variant| resolve_type(scope.clone(), variant.clone())).collect();
            Type::Overload(newvars)
        },
    }
}

fn find_variant<V, T>(scope: ScopeRef<V, T>, otype: Type, atypes: Vec<Type>) -> Type where V: Clone, T: Clone {
    match otype {
        Type::Overload(ref variants) => {
            'outer: for variant in variants {
                if let Type::Function(ref otypes, _) = *variant {
                    if otypes.len() != atypes.len() {
                        continue 'outer;
                    }
                    for (atype, btype) in otypes.iter().zip(atypes.iter()) {
                        if check_type(scope.clone(), Some(atype.clone()), Some(btype.clone()), Check::Def, false).is_err() {
                            continue 'outer;
                        }
                    }
                    return variant.clone();
                }
            }
            panic!("No valid variant found for {:?}\n\t out of {:?}", atypes, variants);
        },
        _ => otype.clone(),
    }
}


fn update_scope_variable_types<V, T>(scope: ScopeRef<V, T>) where V: Clone, T: Clone {
    let mut names = vec!();
    for name in scope.borrow().names.keys() {
        names.push(name.clone());
    }

    for name in &names {
        let otype = scope.borrow_mut().get_variable_type(name).unwrap().clone();
        let ntype = resolve_type(scope.clone(), otype);
        scope.borrow_mut().update_variable_type(name, ntype);
    }
}



#[cfg(test)]
mod tests {
    use super::*;
    use parser::*;
    use scope::*;

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
        let map: ScopeMapRef<()> = bind_names(&mut code);
        check_types(map.clone(), map.get_global(), &mut code)
    }
}


