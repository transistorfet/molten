

use std::fmt::Debug;
use std::collections::HashMap;

use parser::AST;
use utils::UniqueID;
use scope::{ Scope, ScopeRef, ScopeMapRef, mangle_name };

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Object(String),
    Variable(String),
    List(Box<Type>),
    Function(Vec<Type>, Box<Type>),
    Overload(Vec<Type>),
    //Constrained(Box<&mut AST>),
}

impl Type {
    pub fn get_name(&self) -> Option<String> {
        match *self {
            Type::Object(ref name) => Some(name.clone()),
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
            _ => vec!(),
        }
    }

    pub fn add_variant<V, T>(self, scope: ScopeRef<V, T>, ntype: Type) -> Type where V: Clone, T: Clone {
        println!("BEFORE: {:?}", self);
        let rtype = match self {
            Type::Overload(ref variants) => {
                for variant in variants {
                    if check_type(scope.clone(), Some(variant.clone()), Some(ntype.clone()), false).is_ok() {
                        panic!("OverloadError: function definitions overlap; {:?} vs {:?}", variant, ntype);
                    }
                }
                let mut nvariants = variants.clone();
                nvariants.push(ntype);
                Type::Overload(nvariants)
            },
            _ => expect_type(scope.clone(), Some(self), Some(ntype.clone())),
        };
        println!("AFTER: {:?}", rtype);
        rtype
    }
}


pub fn check_types<V, T>(map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>, code: &mut Vec<AST>) -> Type where V: Clone + Debug, T: Clone + Debug {
    let mut last: Type = Type::Object(String::from("Nil"));
    for node in code {
        last = check_types_node(map.clone(), scope.clone(), node);
    }
    last
}

pub fn check_types_node<V, T>(map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>, node: &mut AST) -> Type where V: Clone + Debug, T: Clone + Debug {
    let x = match *node {
        AST::Nil => Type::Object(String::from("Nil")),
        AST::Boolean(_) => Type::Object(String::from("Bool")),
        AST::Integer(_) => Type::Object(String::from("Int")),
        AST::Real(_) => Type::Object(String::from("Real")),
        AST::String(_) => Type::Object(String::from("String")),

        AST::Function(ref mut name, ref mut args, ref mut rtype, ref mut body, ref id) => {
            let fscope = map.get(id);

            let mut argtypes = vec!();
            for &(ref name, ref ttype, ref value) in &*args {
                let vtype = value.clone().map(|ref mut vexpr| check_types_node(map.clone(), scope.clone(), vexpr));
                let mut atype = expect_type(fscope.clone(), ttype.clone(), vtype);
                if &name[..] == "self" {
                    let ctype = fscope.borrow().find_type(&String::from("Self"));
                    atype = expect_type(fscope.clone(), Some(atype), ctype);
                }
                fscope.borrow_mut().update_variable_type(name, atype.clone());
                argtypes.push(atype);
            }

            let rettype = expect_type(fscope.clone(), rtype.clone(), Some(check_types_node(map.clone(), fscope.clone(), body)));
            *rtype = Some(rettype.clone());

            // Resolve type variables that can be
            for i in 0 .. argtypes.len() {
                argtypes[i] = resolve_type(fscope.clone(), argtypes[i].clone());
                // TODO this is still not checking for type compatibility
                fscope.borrow_mut().update_variable_type(&args[i].0, argtypes[i].clone());
                //update_type(fscope.clone(), &args[i].0, argtypes[i].clone());
                args[i].1 = Some(argtypes[i].clone());
            }
            update_scope_variable_types(fscope.clone());

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

                let mut otype = dscope.borrow().get_variable_type(&dname);
                nftype = otype.map(|ttype| ttype.add_variant(scope.clone(), nftype.clone())).unwrap_or(nftype);
                dscope.borrow_mut().update_variable_type(&dname, nftype.clone());
            }
            nftype
        },

        AST::Invoke(ref mut fexpr, ref mut args, ref mut stype) => {
            let mut atypes = vec!();
            for ref mut value in args {
                atypes.push(check_types_node(map.clone(), scope.clone(), value));
            }

            let mut etype = check_types_node(map.clone(), scope.clone(), fexpr);
            //let mangle = match etype {
            //    Type::Overload(_) => { true },
            //    _ => { etype = Type::Overload(vec!(etype)); false }
            //};
            if let Type::Overload(_) = etype {
                etype = find_variant(scope.clone(), etype, atypes.clone());
                match **fexpr {
                    AST::Resolver(_, ref mut name) |
                    AST::Accessor(_, ref mut name, _) |
                    AST::Identifier(ref mut name) => *name = mangle_name(name, etype.get_argtypes()),
                    _ => panic!("OverloadError: call to overloaded method not allowed here"),
                }
            }

            //fn check_invoke_type<V, T>(scope: ScopeRef<V, T>, fexpr: &mut Box<AST>, etype: &Type, atypes: &Vec<Type>, mangle: bool) -> Result<Type, String> where V: Clone + Debug, T: Clone + Debug {
            //match *etype {
            let ftype = match etype {
                Type::Function(ref types, ref ret) => {
                    let tscope = Scope::new_ref(Some(scope.clone()));

                    let ptype = tscope.borrow_mut().map_all_typevars(&etype.clone());
                    let ftype = expect_type(tscope.clone(), Some(Type::Function(atypes, Box::new(ptype.get_rettype().clone()))), Some(ptype));
                    let ftype = resolve_type(tscope.clone(), ftype);        // NOTE This ensures the early arguments are resolved despite typevars not being assigned until later in the signature

                    //if mangle {
                    //    match **fexpr {
                    //        AST::Resolver(_, ref mut name) |
                    //        AST::Accessor(_, ref mut name, _) |
                    //        AST::Identifier(ref mut name) => *name = mangle_name(name, etype.get_argtypes()),
                    //        _ => panic!("OverloadError: call to overloaded method not allowed here"),
                    //    }
                    //}
                    scope.borrow_mut().raise_types(tscope.clone());
                    //Ok(ftype)
                    ftype
                },
                Type::Variable(ref name) => {
                    let rtype = scope.borrow_mut().new_typevar();
                    let ftype = Type::Function(atypes.clone(), Box::new(rtype.clone()));
                    // TODO This is suspect... we might be updating type without checking for a conflict
                    // TODO we also aren't handling other function types, like accessor and resolve
                    //if let AST::Identifier(ref fname) = **fexpr {
                    //    update_type(scope.clone(), fname, ftype.clone());
                    //}
                    scope.borrow_mut().update_type(name, ftype.clone());
                    //Ok(ftype)
                    ftype
                },
                _ => panic!("Not a function: {:?}", fexpr),
            //}
            };

            /*
            match etype {
                Type::Overload(ref variants) => {
                    for variant in variants {
                        match check_invoke_type(scope.clone(), fexpr, &variant, &atypes, mangle) {
                            Err(_) => continue,
                            Ok(ftype) => {
                                *stype = Some(ftype.clone());
                                return ftype.get_rettype().clone();
                            }
                        }
                    }
                    panic!("FUCK2");
                },
                _ => panic!("FUCK"),
            }
            */

            *stype = Some(ftype.clone());
            return ftype.get_rettype().clone();
        },

        AST::SideEffect(ref name, ref mut args) => {
            let mut ltype = None;
            for ref mut expr in args {
                ltype = Some(expect_type(scope.clone(), ltype, Some(check_types_node(map.clone(), scope.clone(), expr))));
            }
            ltype.unwrap()
        },

        AST::Definition((ref name, ref mut ttype), ref mut body) => {
            let dscope = Scope::target(scope.clone());
            let btype = expect_type(scope.clone(), ttype.clone(), Some(check_types_node(map.clone(), scope.clone(), body)));
            dscope.borrow_mut().update_variable_type(name, btype.clone());
            *ttype = Some(btype.clone());
            btype
        },

        AST::Identifier(ref name) => {
            let mut bscope = scope.borrow_mut();
            bscope.get_variable_type(name).unwrap_or_else(|| bscope.new_typevar())
        },

        AST::Block(ref mut body) => check_types(map, scope, body),

        AST::If(ref mut cond, ref mut texpr, ref mut fexpr) => {
            // TODO should this require the cond type to be Bool?
            check_types_node(map.clone(), scope.clone(), cond);
            let ttype = check_types_node(map.clone(), scope.clone(), texpr);
            let ftype = check_types_node(map.clone(), scope.clone(), fexpr);
            expect_type(scope.clone(), Some(ttype), Some(ftype))
        },

        AST::Try(ref mut cond, ref mut cases) |
        AST::Match(ref mut cond, ref mut cases) => {
            let mut ctype = Some(check_types_node(map.clone(), scope.clone(), cond));
            let mut rtype = None;
            for &mut (ref mut case, ref mut expr) in cases {
                ctype = Some(expect_type(scope.clone(), ctype, Some(check_types_node(map.clone(), scope.clone(), case))));
                rtype = Some(expect_type(scope.clone(), rtype, Some(check_types_node(map.clone(), scope.clone(), expr))));
            }
            rtype.unwrap()
        },

        AST::Raise(ref mut expr) => {
            // TODO should you check for a special error/exception type?
            check_types_node(map.clone(), scope.clone(), expr)
        },

        AST::For(ref name, ref mut cond, ref mut body, ref id) => {
            let lscope = map.get(id);
            let ctype = lscope.borrow_mut().get_variable_type(name).unwrap();

            expect_type(lscope.clone(), Some(Type::List(Box::new(ctype))), Some(check_types_node(map.clone(), lscope.clone(), cond)));
            check_types_node(map.clone(), lscope.clone(), body)
        },

        AST::While(ref mut cond, ref mut body) => {
            // TODO should this require the cond type to be Bool?
            check_types_node(map.clone(), scope.clone(), cond);
            check_types_node(map.clone(), scope.clone(), body)
        },

        AST::List(ref mut items) => {
            let mut ltype = None;
            for ref mut expr in items {
                ltype = Some(expect_type(scope.clone(), ltype, Some(check_types_node(map.clone(), scope.clone(), expr))));
            }
            Type::List(Box::new(ltype.unwrap()))
        },

        AST::Index(ref mut base, ref mut index) => {
            let ltype = resolve_type(scope.clone(), check_types_node(map.clone(), scope.clone(), base));
            let itype = expect_type(scope.clone(), Some(Type::Object(String::from("Int"))), Some(check_types_node(map.clone(), scope.clone(), index)));
            match ltype {
                Type::List(ref ttype) => *ttype.clone(),
                _ => panic!("TypeError: List type required for indexing, but found {:?}", ltype),
            }
        },

        AST::Class(ref name, ref parent, ref mut body, ref id) => {
            let tscope = map.get(id);
            check_types(map.clone(), tscope.clone(), body);

            //let classdef = scope.borrow().get_class_def(name).unwrap();
            //for (name, sym) in &tscope.borrow().names {
            //    if !classdef.borrow().names.contains_key(name) {
            //        classdef.borrow_mut().define(name.clone(), sym.ttype.clone());
            //    } else {
            //        classdef.borrow_mut().update_variable_type(name, sym.ttype.clone().unwrap());
            //    }
            //}

            Type::Object(String::from("Class"))     // NOTE: the class expression returns a class, and not an object of that class
        },

        AST::Resolver(ref mut left, ref mut field) => {
            let ltype = match **left {
                AST::Identifier(ref name) => resolve_type(scope.clone(), scope.borrow().find_type(name).unwrap().clone()),
                _ => panic!("SyntaxError: left-hand side of scope resolver must be identifier")
            };

            let classdef = scope.borrow().get_class_def(&ltype.get_name().unwrap()).unwrap();
            let mut cborrow = classdef.borrow_mut();
            cborrow.get_variable_type(field).unwrap_or_else(|| cborrow.new_typevar())
        },

        AST::Accessor(ref mut left, ref mut field, ref mut stype) => {
            let ltype = resolve_type(scope.clone(), check_types_node(map.clone(), scope.clone(), left));
            *stype = Some(ltype.clone());

            let classdef = scope.borrow().get_class_def(&ltype.get_name().unwrap()).unwrap();
            let mut cborrow = classdef.borrow_mut();
            cborrow.get_variable_type(field).unwrap_or_else(|| cborrow.new_typevar())
        },

        AST::Assignment(ref mut left, ref mut right) => {
            let ltype = check_types_node(map.clone(), scope.clone(), left);
            let rtype = check_types_node(map.clone(), scope.clone(), right);
            expect_type(scope.clone(), Some(ltype), Some(rtype))
        },

        AST::Noop |
        AST::Import(_) => Type::Object(String::from("Nil")),

        AST::Type(_, _) => panic!("NotImplementedError: not yet supported, {:?}", node),

        AST::Underscore => scope.borrow_mut().new_typevar(),
    };
    
    println!("CHECK: {:?} {:?}", x, node);
    x
}

pub fn expect_type<V, T>(scope: ScopeRef<V, T>, ottype: Option<Type>, obtype: Option<Type>) -> Type where V: Clone, T: Clone {
    match check_type(scope, ottype, obtype, true) {
        Ok(ttype) => ttype,
        Err(msg) => panic!(msg),
    }
}

// ottype must be equal to, or a superset of obtype; if obtype is less specific, it will be forced to match
pub fn check_type<V, T>(scope: ScopeRef<V, T>, ottype: Option<Type>, obtype: Option<Type>, update: bool) -> Result<Type, String> where V: Clone, T: Clone {
    if ottype.is_none() {
        // TODO should the else case just be Nil... 
        Ok(obtype.unwrap_or_else(|| scope.borrow_mut().new_typevar()))
    } else if obtype.is_none() {
        Ok(ottype.unwrap())
    } else {
        let ttype = resolve_type(scope.clone(), ottype.unwrap());
        let btype = resolve_type(scope.clone(), obtype.unwrap());

        if let Type::Variable(ref name) = btype {
            println!("UPDATING: {:?} {:?}", name, ttype);
            if update { scope.borrow_mut().update_type(name, ttype.clone()); }
            //if update { update_type(scope.clone(), name, ttype.clone()); }
            Ok(ttype)
        } else if let Type::Variable(ref name) = ttype {
            println!("UPDATING: {:?} {:?}", name, ttype);
            if update { scope.borrow_mut().update_type(name, btype.clone()); }
            //if update { update_type(scope.clone(), name, btype.clone()); }
            Ok(btype)
        } else {
            match (ttype.clone(), btype.clone()) {
                (Type::List(ref a), Type::List(ref b)) => Ok(Type::List(Box::new(check_type(scope.clone(), Some(*a.clone()), Some(*b.clone()), update)?))),
                (Type::Function(ref aargs, ref aret), Type::Function(ref bargs, ref bret)) => {
                    if aargs.len() == bargs.len() {
                        let mut argtypes = vec!();
                        for (atype, btype) in aargs.iter().zip(bargs.iter()) {
                            argtypes.push(check_type(scope.clone(), Some(atype.clone()), Some(btype.clone()), update)?);
                        }
                        Ok(Type::Function(argtypes, Box::new(check_type(scope.clone(), Some(*aret.clone()), Some(*bret.clone()), update)?)))
                    } else {
                        Err(format!("TypeError: type mismatch, expected {:?} but found {:?}", ttype, btype))
                    }
                },
                (Type::Object(ref aname), Type::Object(ref bname)) => {
                    println!("{:?} {:?}", aname, bname);
                    if is_subclass_of(scope.clone(), aname, bname) {
                        Ok(btype)
                    } else if is_subclass_of(scope.clone(), bname, aname) {
                        Ok(ttype)
                    } else {
                        Err(format!("TypeError: type mismatch, expected {:?} but found {:?}", ttype, btype))
                    }
                },
                (_, Type::Overload(ref variants)) |
                (Type::Overload(ref variants), _) => Err(format!("TypeError: overloaded types are not allowed here...")),
                _ => {
                    if ttype == btype {
                        Ok(ttype)
                    } else {
                        Err(format!("TypeError: type mismatch, expected {:?} but found {:?}", ttype, btype))
                    }
                }
            }
        }
    }
}

fn is_subclass_of<V, T>(scope: ScopeRef<V, T>, aname: &String, bname: &String) -> bool where V: Clone, T: Clone {
    let mut cname = aname.clone();
    loop {
        if *bname == cname {
            return true;
        }
        match scope.borrow_mut().get_parent_class_name(&cname) {
            Some(name) => cname = name,
            None => return false,
        }
    }
}

pub fn resolve_type<V, T>(scope: ScopeRef<V, T>, ttype: Type) -> Type where V: Clone, T: Clone {
    match ttype {
        Type::Object(ref name) => {
            match scope.borrow().find_type(name) {
                // TODO we are purposely returning the original type here so as not to over-resolve types... but we should probably still fully resolve for checking purposes
                Some(x) => /* x */ ttype.clone(),
                None => panic!("TypeError: undefined type {:?}", name),
            }
        },
        Type::Variable(ref name) => {
            match scope.borrow().find_type(name) {
                Some(vtype) => {
                    if Type::Variable(name.clone()) == vtype {
                        vtype
                    } else {
                        resolve_type(scope.clone(), vtype)
                    }
                },
                None => panic!("TypeError: undefined type variable {:?}", name),
            }
        },
        Type::List(ref ttype) => {
            Type::List(Box::new(resolve_type(scope.clone(), *ttype.clone())))
        },
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
                        if check_type(scope.clone(), Some(atype.clone()), Some(btype.clone()), false).is_err() {
                            continue 'outer;
                        }
                    }
                    return variant.clone();
                }
            }
            panic!("No valid variant found for {:?}", atypes);
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

fn update_variable_type<V, T>(scope: ScopeRef<V, T>, name: &String, ttype: Type) where V: Clone, T: Clone {
    let otype = scope.borrow_mut().get_variable_type(name).unwrap().clone();
    let ntype = expect_type(scope.clone(), Some(otype.clone()), Some(ttype.clone()));
    scope.borrow_mut().update_variable_type(name, ntype);
}

fn update_type<V, T>(scope: ScopeRef<V, T>, name: &String, ttype: Type) where V: Clone, T: Clone {
    // TODO you would do the typecheck here rather than in the Scope update_type function
    //let otype = scope.borrow().find_type(name).unwrap().clone();
    //scope.borrow_mut().update_type(name, expect_type(scope.clone(), Some(otype), Some(ttype)));
    //scope.borrow_mut().swap_variable_type(name, &|otype| expect_type(scope.clone(), Some(otype), Some(ttype.clone())));
    //scope.borrow_mut().swap_type(name, &|otype| expect_type(scope.clone(), Some(otype), Some(ttype.clone())));

    let otype = scope.borrow().find_type(name).clone();
    if otype.is_some() {
        let ntype = check_type(scope.clone(), otype, Some(ttype), false);
        //scope.borrow_mut().update_type(name, ntype);
        scope.borrow_mut().update_type(name, ntype.unwrap());
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


