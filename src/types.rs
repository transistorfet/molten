

use parser::AST;
use scope::{ Scope, ScopeRef };

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Concrete(String),
    Variable(String),
    List(Box<Type>),
    Function(Vec<Type>, Box<Type>)
}

pub fn check_types(scope: ScopeRef, code: &mut Vec<AST>) -> Type {
    //let types = vec!();
    let mut last: Type = Type::Concrete(String::from("Nil"));

    for node in code {
        //types.push(check_types_node(scope.clone(), node));
        last = check_types_node(scope.clone(), node);
        println!("TYPE: {:?}", last);
    }

    return last;
}

pub fn check_types_node(scope: ScopeRef, node: &mut AST) -> Type {
    let x = match *node {
        AST::Nil => Type::Concrete(String::from("Nil")),
        AST::Boolean(_) => Type::Concrete(String::from("Bool")),
        AST::Integer(_) => Type::Concrete(String::from("Int")),
        AST::Real(_) => Type::Concrete(String::from("Real")),
        AST::String(_) => Type::Concrete(String::from("String")),

        AST::List(ref mut items) => {
            let mut ltype = None;
            for ref mut expr in items {
                ltype = Some(expect_type(scope.clone(), ltype, Some(check_types_node(scope.clone(), expr))));
            }
            Type::List(Box::new(ltype.unwrap()))
        },

        AST::Identifier(ref name) => scope.borrow().find(name).unwrap().ttype.clone(),

        AST::Invoke(ref name, ref mut args) => {
            let mut ftype = match scope.borrow().find(name) {
                None => panic!("Function not found: {:?}", name),
                Some(sym) => sym.ttype.clone(),
            };

            let tscope = Scope::new_ref(Some(scope.clone()));
            if let Type::Function(ref mut types, ref ret) = ftype {
                // TODO I think this needs to reallocate the type variables so they don't conflict with types in the parent context?  Or maybe this is somehow why i didn't give the scope a parent in the other one
                for ttype in types.clone() {
                    if let Type::Variable(ref name) = ttype {
                        if !tscope.borrow_mut().contains_type(name) {
                            tscope.borrow_mut().define_type(name.clone(), ttype.clone());
                        }
                    }
                }

                //let mut argtypes = vec!();
                for (ttype, ref mut value) in types.iter().zip(args.iter()) {
                    //argtypes.push(check_types_node(tscope.clone(), &mut value.clone()));
                    // TODO do you have to do these in separate steps
                    let atype = check_types_node(tscope.clone(), &mut value.clone());
                    // NOTE atype should take priority over ttype because ttype is locally defined, whereas atype variable are defined in the parent scope
                    //      actually i don't have a clue why this works, but it does, could still be broken
                    expect_type(tscope.clone(), Some(atype), Some(ttype.clone()));
                }

                //for (ttype, atype) in types.iter().zip(argtypes.iter()) {
                //    expect_type(tscope.clone(), Some(ttype.clone()), Some(atype.clone()));
                //}

                println!("FUNCTION SCOPE: {:?}", tscope);
                resolve_type(tscope.clone(), *ret.clone())
            }
            else if let Type::Variable(ref name) = ftype {
                let mut atypes = vec!();
                for ref mut value in args {
                    atypes.push(check_types_node(scope.clone(), value));
                }
                let rettype = scope.borrow_mut().new_typevar();
                // TODO This is suspect... we might be updating type without checking for a conflict
                //scope.borrow_mut().update_type(name, Type::Function(atypes, Box::new(rettype.clone())));
                update_type(scope.clone(), name, Type::Function(atypes, Box::new(rettype.clone())));
                rettype
            }
            else {
                panic!("Not a function: {:?}", name);
            }
        },

        AST::Function(ref mut args, ref mut body, ref fscope, ref mut ftype, _) => {
            let mut argtypes: Vec<Type> = vec!();
            for &(ref name, ref ttype, ref value) in &*args {
                let vtype = match value.clone() {
                    Some(ref mut vexpr) => Some(check_types_node(scope.clone(), vexpr)),
                    None => None
                };
                let atype = expect_type(fscope.clone(), ttype.clone(), vtype);
                fscope.borrow_mut().update_variable_type(name, atype.clone());
                argtypes.push(atype);
            }

            let rettype = resolve_type(fscope.clone(), check_types_node(fscope.clone(), body));

            // Resolve type variables that can be
            for i in 0 .. argtypes.len() {
                argtypes[i] = resolve_type(fscope.clone(), argtypes[i].clone());
                // TODO this is still not checking for type compatibility
                fscope.borrow_mut().update_variable_type(&args[i].0, argtypes[i].clone());
                //println!("NTHOEU: {} {:?}", args[i].0, argtypes[i]);
                //update_type(fscope.clone(), &args[i].0, argtypes[i].clone());
                args[i].1 = Some(argtypes[i].clone());
            }
            use debug;
            //debug::print_types_scope(scope.clone());
            println!("{:?}", fscope);
            update_scope_variable_types(fscope.clone());

            println!("FUNC: {:?} {:?}", argtypes, rettype);
            let nftype = Type::Function(argtypes, Box::new(rettype));
            *ftype = Some(nftype.clone());
            nftype
        },


        AST::Definition((ref name, ref mut ttype), ref mut body) => {
            let btype = expect_type(scope.clone(), ttype.clone(), Some(check_types_node(scope.clone(), body)));
            scope.borrow_mut().update_variable_type(name, btype.clone());
            *ttype = Some(btype.clone());
            btype
        },

        AST::Block(ref mut body) => check_types(scope, body),

        AST::If(ref mut cond, ref mut texpr, ref mut fexpr) => {
            check_types_node(scope.clone(), cond);
            let ttype = check_types_node(scope.clone(), texpr);
            let ftype = check_types_node(scope.clone(), fexpr);
            expect_type(scope.clone(), Some(ttype), Some(ftype))
        },

        AST::Raise(ref mut expr) => {
            // TODO should you check for a special error/exception type?
            check_types_node(scope.clone(), expr)
        },

        AST::Try(ref mut cond, ref mut cases) |
        AST::Match(ref mut cond, ref mut cases) => {
            let ctype = check_types_node(scope.clone(), cond);
            let mut rtype = None;
            for &mut (ref mut case, ref mut expr) in cases {
                expect_type(scope.clone(), Some(ctype.clone()), Some(check_types_node(scope.clone(), case)));
                rtype = Some(expect_type(scope.clone(), rtype, Some(check_types_node(scope.clone(), expr))));
            }
            rtype.unwrap()
        },

        AST::For(ref name, ref mut cond, ref mut body, ref mut lscope) => {
            let ctype = match lscope.borrow().find(name) {
                None => panic!("Variable not found: {:?}", name),
                Some(sym) => sym.ttype.clone(),
            };

            expect_type(lscope.clone(), Some(Type::List(Box::new(ctype))), Some(check_types_node(lscope.clone(), cond)));
            check_types_node(lscope.clone(), body)
        },

        AST::While(ref mut cond, ref mut body) => {
            check_types_node(scope.clone(), cond);
            check_types_node(scope.clone(), body)
        },

        AST::Class(ref name, ref mut body, ref mut cscope) => {
            check_types(cscope.clone(), body);
            Type::Concrete(String::from("Class"))
        },

        AST::Index(ref mut base, ref mut index) => {
            let ttype = Type::List(Box::new(scope.borrow_mut().new_typevar()));
            let ltype = expect_type(scope.clone(), Some(ttype), Some(check_types_node(scope.clone(), base)));
            let itype = expect_type(scope.clone(), Some(Type::Concrete(String::from("Int"))), Some(check_types_node(scope.clone(), index)));
            if let Type::List(ref ttype) = ltype {
                *ttype.clone()
            }
            else {
                panic!("TypeError: List type required for indexing, but found {:?}", ltype);
            }
        },

        AST::Accessor(ref mut left, ref mut right) => {
            let ltype = expect_type(scope.clone(), None, Some(check_types_node(scope.clone(), left)));
            // TODO how do you finish this?  I guess only a class or a typedef type can be accessed
            //match ltype {
            //}
            ltype
        },

        AST::Noop |
        AST::Import(_) => Type::Concrete(String::from("Nil")),

        // TODO AST::Type

        _ => scope.borrow_mut().new_typevar(),
    };
    
    println!("CHECK: {:?} {:?}", x, node);
    x
}

// ottype must be equal to, or a superset of obtype; if obtype is less specific, it will be forced to match
pub fn expect_type(scope: ScopeRef, ottype: Option<Type>, obtype: Option<Type>) -> Type {
    if ottype.is_none() {
        // TODO should the else case just be Nil... 
        obtype.unwrap_or_else(|| scope.borrow_mut().new_typevar())
    }
    else if obtype.is_none() {
        ottype.unwrap()
    }
    else {
        let ttype = resolve_type(scope.clone(), ottype.unwrap());
        let btype = resolve_type(scope.clone(), obtype.unwrap());

        if let Type::Variable(ref name) = btype {
            println!("UPDATING: {:?} {:?}", name, ttype);
            scope.borrow_mut().update_type(name, ttype.clone());
            //update_type(scope.clone(), name, ttype.clone());
            ttype
        }
        else if let Type::Variable(ref name) = ttype {
            println!("UPDATING: {:?} {:?}", name, ttype);
            scope.borrow_mut().update_type(name, btype.clone());
            //update_type(scope.clone(), name, ttype.clone());
            btype
        }
        else {
            match (ttype.clone(), btype.clone()) {
                (Type::List(ref a), Type::List(ref b)) => Type::List(Box::new(expect_type(scope.clone(), Some(*a.clone()), Some(*b.clone())))),
                (Type::Function(ref aargs, ref aret), Type::Function(ref bargs, ref bret)) => {
                    let mut argtypes = vec!();
                    for (atype, btype) in aargs.iter().zip(bargs.iter()) {
                        argtypes.push(expect_type(scope.clone(), Some(atype.clone()), Some(btype.clone())));
                    }
                    Type::Function(argtypes, Box::new(expect_type(scope.clone(), Some(*aret.clone()), Some(*bret.clone()))))
                },
                _ => {
                    if ttype == btype {
                        ttype
                    }
                    else {
                        panic!("TypeError: type mismatch, expected {:?} but found {:?}", ttype, btype);
                    }
                }
            }
        }
    }
}

/*
fn resolve_typevar(scope: ScopeRef, ttype: &mut Type) {
    if let Type::Variable(ref name) = ttype.clone() {
        let stype = scope.borrow().find_type(name);
        println!("HERE {:?} {:?}", name, stype);
        if stype != Some(Type::Variable(name.clone())) {
            *ttype = stype.unwrap();
        }
    }
}
*/

fn resolve_type(scope: ScopeRef, ttype: Type) -> Type {
    match ttype {
        Type::Concrete(ref name) => {
            match scope.borrow().find_type(name) {
                // TODO we are purposely returning the original type here so as not to over-resolve types... but we should probably still fully resolve for checking purposes
                Some(x) => /* x */ ttype.clone(),
                None => panic!("TypeError: undefined type {:?}", name),
            }
        },
        Type::Variable(ref name) => {
            match scope.borrow().find_type(name) {
                Some(x) => {
                    if Type::Variable(name.clone()) == x {
                        x
                    }
                    else {
                        resolve_type(scope.clone(), x)
                    }
                },
                None => panic!("TypeError: undefined type variable {:?}", name),
            }
        },
        Type::List(ref ttype) => {
            Type::List(Box::new(resolve_type(scope.clone(), *ttype.clone())))
        },
        Type::Function(ref args, ref ret) => {
            let mut argtypes = vec!();
            for arg in args {
                argtypes.push(resolve_type(scope.clone(), arg.clone()))
            }
            Type::Function(argtypes, Box::new(resolve_type(scope.clone(), *ret.clone())))
        },
    }
}

fn update_scope_variable_types(scope: ScopeRef) {
    //use std::rc::Rc;

    //for (ref name, ref mut sym) in &mut scope.borrow_mut().names {
    //    Rc::get_mut(sym).unwrap().ttype = resolve_type(scope.clone(), sym.ttype.clone());
    //}

    let mut names = vec!();
    for name in scope.borrow().names.keys() {
        names.push(name.clone());
    }

    for name in &names {
        let otype = scope.borrow().find(name).unwrap().ttype.clone();
        let ntype = resolve_type(scope.clone(), otype);
        scope.borrow_mut().update_variable_type(name, ntype);
    }
}
/*
*/

fn update_type(scope: ScopeRef, name: &String, ttype: Type) {
    // TODO you would do the typecheck here rather than in the Scope update_type function
    //let otype = scope.borrow().find_type(name).unwrap().clone();
    //scope.borrow_mut().update_type(name, expect_type(scope.clone(), Some(otype), Some(ttype)));
    //scope.borrow_mut().swap_variable_type(name, &|otype| expect_type(scope.clone(), Some(otype), Some(ttype.clone())));
    //scope.borrow_mut().swap_type(name, &|otype| expect_type(scope.clone(), Some(otype), Some(ttype.clone())));

    let otype = scope.borrow().find_type(name).unwrap().clone();
    let ntype = expect_type(scope.clone(), Some(otype), Some(ttype));
    scope.borrow_mut().update_type(name, ntype);
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
        let global = bind_names(&mut code);
        check_types(global.clone(), &mut code)
    }
}


