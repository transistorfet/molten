

use std::fmt::Debug;
use std::collections::HashMap;

use parser::AST;
use scope::{ Scope, ScopeRef, ScopeMapRef };

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Object(String),
    //Concrete(String),
    //Class(String),
    Variable(String),
    List(Box<Type>),
    Function(Vec<Type>, Box<Type>)
}

impl Type {
    pub fn get_name(&self) -> Option<String> {
        match *self {
            Type::Object(ref name) => Some(name.clone()),
            _ => panic!("TypeError: expected a class or concrete type, found {:?}", self),
        }
    }
}

pub fn check_types<V, T>(map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>, code: &mut Vec<AST>) -> Type where V: Clone + Debug, T: Clone + Debug {
    //let types = vec!();
    let mut last: Type = Type::Object(String::from("Nil"));

    for node in code {
        //types.push(check_types_node(map.clone(), scope.clone(), node));
        last = check_types_node(map.clone(), scope.clone(), node);
        println!("TYPE: {:?}", last);
    }

    return last;
}

pub fn check_types_node<V, T>(map: ScopeMapRef<V, T>, scope: ScopeRef<V, T>, node: &mut AST) -> Type where V: Clone + Debug, T: Clone + Debug {
    let x = match *node {
        AST::Nil => Type::Object(String::from("Nil")),
        AST::Boolean(_) => Type::Object(String::from("Bool")),
        AST::Integer(_) => Type::Object(String::from("Int")),
        AST::Real(_) => Type::Object(String::from("Real")),
        AST::String(_) => Type::Object(String::from("String")),

        AST::List(ref mut items) => {
            let mut ltype = None;
            for ref mut expr in items {
                ltype = Some(expect_type(scope.clone(), ltype, Some(check_types_node(map.clone(), scope.clone(), expr))));
            }
            Type::List(Box::new(ltype.unwrap()))
        },

        AST::Function(ref name, ref mut args, ref mut rtype, ref mut body, ref id) => {
            let fscope = map.get(id);
            let mut argtypes: Vec<Type> = vec!();
            for &(ref name, ref ttype, ref value) in &*args {
                let vtype = match value.clone() {
                    Some(ref mut vexpr) => Some(check_types_node(map.clone(), scope.clone(), vexpr)),
                    None => None
                };
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
                //println!("NTHOEU: {} {:?}", args[i].0, argtypes[i]);
                //update_type(fscope.clone(), &args[i].0, argtypes[i].clone());
                args[i].1 = Some(argtypes[i].clone());
            }
            update_scope_variable_types(fscope.clone());

            println!("FUNC: {:?} {:?}", argtypes, rettype);
            let nftype = Type::Function(argtypes, Box::new(rettype));
            //*ftype = Some(nftype.clone());
            if name.is_some() {
                scope.borrow_mut().update_variable_type(name.as_ref().unwrap(), nftype.clone());
            }
            nftype
        },

        AST::Invoke(ref mut fexpr, ref mut args, ref mut stype) => {
            let mut atypes = vec!();
            for ref mut value in args {
                atypes.push(check_types_node(map.clone(), scope.clone(), value));
            }

            //match **fexpr {
            //    AST::Accessor(_, ref mut name, _) | AST::Identifier(ref mut name) => {
            //        if scope.borrow().is_mangled(name) {
            //            *name = mangle_name(name, &atypes);
            //        }
            //    },
            //    _ => {},
            //};
            let etype = check_types_node(map.clone(), scope.clone(), fexpr);

            match etype {
                Type::Function(ref types, ref ret) => {
                    let tscope = Scope::new_ref(Some(scope.clone()));
                    let mut ptypes: Vec<Type> = vec!();
                    let mut names: HashMap<&String, Type> = HashMap::new();
                    for ttype in types {
                        if let Type::Variable(ref name) = *ttype {
                            let ptype = names.get(name).map(|x| x.clone());
                            match ptype {
                                Some(ptype) => ptypes.push(ptype),
                                None => { 
                                    let v = tscope.borrow_mut().new_typevar();
                                    names.insert(name, v.clone());
                                    ptypes.push(v.clone());
                                }
                            }
                        }
                        else {
                            ptypes.push(ttype.clone());
                        }
                    }

                    let rtype = if let Type::Variable(ref name) = **ret {
                        let ptype = names.get(name).map(|x| x.clone());
                        match ptype {
                            Some(ptype) => ptype,
                            None => tscope.borrow_mut().new_typevar(),
                        }
                    } else {
                        *ret.clone()
                    };

                    /*
                    let mut stypes = vec!();
                    for (ttype, ref mut value) in types.iter().zip(args.iter_mut()) {
                        //argtypes.push(check_types_node(map.clone(), tscope.clone(), value));
                        // TODO do you have to do these in separate steps
                        let atype = check_types_node(map.clone(), tscope.clone(), value);
                        // NOTE atype should take priority over ttype because ttype is locally defined, whereas atype variable are defined in the parent scope
                        //      actually i don't have a clue why this works, but it does, could still be broken
                        stypes.push(expect_type(tscope.clone(), Some(atype), Some(ttype.clone())));
                    }
                    */

                    let mut stypes = vec!();
                    for (ttype, atype) in ptypes.iter().zip(atypes.iter()) {
                        // NOTE atype should take priority over ttype because ttype is locally defined, whereas atype variable are defined in the parent scope
                        //      actually i don't have a clue why this works, but it does, could still be broken
                        stypes.push(expect_type(tscope.clone(), Some(atype.clone()), Some(ttype.clone())));
                        expect_type(tscope.clone(), Some(ttype.clone()), Some(atype.clone()));
                    }

                    //println!("FUNCTION SCOPE: {:?}", tscope);
                    let rtype = resolve_type(tscope.clone(), rtype.clone());
                    *stype = Some(Type::Function(stypes.iter().map(|ttype| resolve_type(tscope.clone(), ttype.clone())).collect(), Box::new(rtype.clone())));
                    //println!("{:?}", tscope);
                    scope.borrow_mut().raise_types(tscope);
                    rtype
                },
                Type::Variable(ref name) => {
                    let rtype = scope.borrow_mut().new_typevar();
                    let ftype = Type::Function(atypes, Box::new(rtype.clone()));
                    // TODO This is suspect... we might be updating type without checking for a conflict
                    if let AST::Identifier(ref fname) = **fexpr {
                        update_type(scope.clone(), fname, ftype.clone());
                    }
                    *stype = Some(ftype);
                    rtype
                }
                _ => panic!("Not a function: {:?}", fexpr),
            }
        },

        AST::Definition((ref name, ref mut ttype), ref mut body) => {
            //let ntype = scope.borrow_mut().new_typevar();
            //scope.borrow_mut().update_variable_type(name, ntype);
            let btype = expect_type(scope.clone(), ttype.clone(), Some(check_types_node(map.clone(), scope.clone(), body)));
            scope.borrow_mut().update_variable_type(name, btype.clone());
            *ttype = Some(btype.clone());
            btype
        },

        AST::Identifier(ref name) => {
            //scope.borrow_mut().get_variable_type(name).unwrap()
            let mut bscope = scope.borrow_mut();
            bscope.get_variable_type(name).unwrap_or_else(|| bscope.new_typevar())
            //get_variable_type(scope, name)
        },

        AST::Block(ref mut body) => check_types(map, scope, body),

        AST::If(ref mut cond, ref mut texpr, ref mut fexpr) => {
            check_types_node(map.clone(), scope.clone(), cond);
            let ttype = check_types_node(map.clone(), scope.clone(), texpr);
            let ftype = check_types_node(map.clone(), scope.clone(), fexpr);
            println!("{:?}", scope.borrow().types);
            expect_type(scope.clone(), Some(ttype), Some(ftype))
        },

        AST::Try(ref mut cond, ref mut cases) |
        AST::Match(ref mut cond, ref mut cases) => {
            let ctype = check_types_node(map.clone(), scope.clone(), cond);
            let mut rtype = None;
            for &mut (ref mut case, ref mut expr) in cases {
                expect_type(scope.clone(), Some(ctype.clone()), Some(check_types_node(map.clone(), scope.clone(), case)));
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
            check_types_node(map.clone(), scope.clone(), cond);
            check_types_node(map.clone(), scope.clone(), body)
        },

        AST::Class(ref name, ref parent, ref mut body, ref id) => {
            let tscope = map.get(id);
            check_types(map.clone(), tscope.clone(), body);

            let classdef = scope.borrow().find_class_def(name).unwrap();
            for (name, sym) in &tscope.borrow().names {
                classdef.borrow_mut().update_variable_type(name, sym.ttype.clone().unwrap());
            }

            Type::Object(String::from("Class"))     // NOTE: the class expression returns a class, and not an object of that class
        },

        AST::Index(ref mut base, ref mut index) => {
            //let ttype = Type::List(Box::new(scope.borrow_mut().new_typevar()));
            let ltype = expect_type(scope.clone(), None, Some(check_types_node(map.clone(), scope.clone(), base)));
            let itype = expect_type(scope.clone(), Some(Type::Object(String::from("Int"))), Some(check_types_node(map.clone(), scope.clone(), index)));
            match ltype {
                Type::List(ref ttype) => *ttype.clone(),
                _ => panic!("TypeError: List type required for indexing, but found {:?}", ltype),
            }
        },

        AST::Resolver(ref mut left, ref mut field) => {
            let ltype = match **left {
                AST::Identifier(ref name) => expect_type(scope.clone(), None, Some(scope.borrow().find_type(name).unwrap().clone())),
                _ => panic!("SyntaxError: left-hand side of scope resolver must be identifier")
            };

            let name = ltype.get_name().unwrap();
            let classdef = scope.borrow().find_class_def(&name).unwrap();
            let rtype = match classdef.borrow().find(field) {
                Some(info) => info.ttype.unwrap().clone(),
                None => panic!("NameError: type {:?} doesn't contain field name {:?}", ltype, field)
            };
            rtype
        },

        AST::Accessor(ref mut left, ref mut field, ref mut stype) => {
            let ltype = expect_type(scope.clone(), None, Some(check_types_node(map.clone(), scope.clone(), left)));
            *stype = Some(ltype.clone());

            let name = ltype.get_name().unwrap();
            let classdef = scope.borrow().find_class_def(&name).unwrap();
            let rtype = match classdef.borrow().find(field) {
                Some(info) => info.ttype.unwrap().clone(),
                None => panic!("NameError: type {:?} doesn't contain field name {:?}", ltype, field)
            };
            rtype
        },

        AST::Assignment(ref mut left, ref mut right) => {
            let ltype = check_types_node(map.clone(), scope.clone(), left);
            let rtype = check_types_node(map.clone(), scope.clone(), right);
            expect_type(scope.clone(), Some(ltype), Some(rtype))
        },

        AST::Noop |
        AST::Import(_) => Type::Object(String::from("Nil")),

        // TODO AST::Type

        _ => scope.borrow_mut().new_typevar(),
    };
    
    println!("CHECK: {:?} {:?}", x, node);
    x
}

// ottype must be equal to, or a superset of obtype; if obtype is less specific, it will be forced to match
pub fn expect_type<V, T>(scope: ScopeRef<V, T>, ottype: Option<Type>, obtype: Option<Type>) -> Type where V: Clone, T: Clone {
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
                (Type::Object(ref aname), Type::Object(ref bname)) => {
                    println!("{:?} {:?}", aname, bname);
                    if is_subclass_of(scope.clone(), aname, bname) {
                        btype
                    }
                    else if is_subclass_of(scope.clone(), bname, aname) {
                        ttype
                    }
                    else {
                        panic!("TypeError: type mismatch, expected {:?} but found {:?}", ttype, btype);
                    }
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

fn update_scope_variable_types<V, T>(scope: ScopeRef<V, T>) where V: Clone, T: Clone {
    //use std::rc::Rc;

    //for (ref name, ref mut sym) in &mut scope.borrow_mut().names {
    //    Rc::get_mut(sym).unwrap().ttype = resolve_type(scope.clone(), sym.ttype.clone());
    //}

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
/*
*/

fn update_type<V, T>(scope: ScopeRef<V, T>, name: &String, ttype: Type) where V: Clone, T: Clone {
    // TODO you would do the typecheck here rather than in the Scope update_type function
    //let otype = scope.borrow().find_type(name).unwrap().clone();
    //scope.borrow_mut().update_type(name, expect_type(scope.clone(), Some(otype), Some(ttype)));
    //scope.borrow_mut().swap_variable_type(name, &|otype| expect_type(scope.clone(), Some(otype), Some(ttype.clone())));
    //scope.borrow_mut().swap_type(name, &|otype| expect_type(scope.clone(), Some(otype), Some(ttype.clone())));

    let otype = scope.borrow().find_type(name).clone();
    if otype.is_some() {
        let ntype = expect_type(scope.clone(), otype, Some(ttype));
        scope.borrow_mut().update_type(name, ntype);
    }
}

fn mangle_name(name: &String, atypes: &Vec<Type>) -> String {
    format!("_{}", name)
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


