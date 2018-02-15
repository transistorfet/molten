
//use std::rc::Rc;

use scope::{ Scope, ScopeRef };
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

    pub fn update_type<V, T>(scope: ScopeRef<V, T>, name: &String, ttype: Type) where V: Clone, T: Clone {
        let pscope = Scope::locate_type(scope.clone(), name).unwrap();
        let otype = pscope.borrow().find_type(name).clone();
        if otype.is_some() && !pscope.borrow().is_primative() {
            let ntype = check_type(scope.clone(), otype.clone(), Some(ttype.clone()), Check::Def, false);
            match ntype {
                Ok(utype) => scope.borrow_mut().update_type(name, utype),
                Err(msg) => panic!("while updating type {:?}:\n{:?}", name, msg),
            }

            //if !Rc::ptr_eq(&pscope, &scope) {
            //    println!("RAISING: {:?} {:?} {:?}", pscope.borrow().get_basename(), otype, ttype);
            //    pscope.borrow_mut().raise_type(scope.clone(), otype.unwrap());
            //    pscope.borrow_mut().raise_type(scope.clone(), ttype);
            //}
        }
    }
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
            //if update { scope.borrow_mut().update_type(name, ctype.clone()); }
            if update { Type::update_type(scope.clone(), name, ctype.clone()); }
            Ok(ctype)
        } else if let Type::Variable(ref name) = ctype {
            //if update { scope.borrow_mut().update_type(name, dtype.clone()); }
            if update { Type::update_type(scope.clone(), name, dtype.clone()); }
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
                (_, Type::Overload(_)) |
                (Type::Overload(_), _) => Err(format!("TypeError: overloaded types are not allowed here...")),
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
                Some(_) => {
                    let params = types.iter().map(|ptype| resolve_type(scope.clone(), ptype.clone())).collect();
                    // TODO we are purposely returning the original type here so as not to over-resolve types... but we should probably still fully resolve for checking purposes
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

pub fn find_variant<V, T>(scope: ScopeRef<V, T>, otype: Type, atypes: Vec<Type>) -> Type where V: Clone, T: Clone {
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


