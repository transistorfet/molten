
use std::fmt;

use ast::ClassSpec;
use utils::UniqueID;
use session::{ Session, Error };
use scope::{ Scope, ScopeRef };

pub use abi::ABI;


#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Object(String, Vec<Type>),
    Variable(String, UniqueID),
    Function(Box<Type>, Box<Type>, ABI),
    Tuple(Vec<Type>),
    Overload(Vec<Type>),
    //Generic(String, Vec<Type>),
    //Constrained(Box<&mut AST>),
}

impl Type {
    pub fn get_name(&self) -> Result<String, Error> {
        match *self {
            Type::Object(ref name, _) => Ok(name.clone()),
            _ => Err(Error::new(format!("TypeError: expected a class or concrete type, found {:?}", self))),
        }
    }

    pub fn get_params(&self) -> Result<Vec<Type>, Error> {
        match *self {
            Type::Object(_, ref params) => Ok(params.clone()),
            _ => Err(Error::new(format!("TypeError: expected a class or concrete type, found {:?}", self))),
        }
    }


    pub fn is_tuple(&self) -> bool {
        match *self {
            Type::Tuple(_) => true,
            _ => false
        }
    }

    pub fn get_types(&self) -> Result<&Vec<Type>, Error> {
        match self {
            &Type::Tuple(ref types) => Ok(types),
            _ => Err(Error::new(format!("TypeError: expected tuple type, found {:?}", self))),
        }
    }


    pub fn is_function(&self) -> bool {
        match *self {
            Type::Function(_, _, _) => true,
            _ => false
        }
    }


    pub fn get_argtypes(&self) -> Result<&Type, Error> {
        match self {
            &Type::Function(ref args, _, _) => Ok(args),
            _ => Err(Error::new(format!("TypeError: expected function type, found {:?}", self))),
        }
    }

    pub fn get_rettype(&self) -> Result<&Type, Error> {
        match self {
            &Type::Function(_, ref ret, _) => Ok(&**ret),
            _ => Err(Error::new(format!("TypeError: expected function type, found {:?}", self))),
        }
    }

    pub fn get_abi(&self) -> Result<ABI, Error> {
        match self {
            &Type::Function(_, _, ref abi) => Ok(*abi),
            _ => Err(Error::new(format!("TypeError: expected function type, found {:?}", self))),
        }
    }


    pub fn is_variable(&self) -> bool {
        match *self {
            Type::Variable(_, _) => true,
            _ => false
        }
    }

    pub fn get_varname(&self) -> Result<String, Error> {
        match self {
            &Type::Variable(ref name, _) => Ok(name.clone()),
            _ => Err(Error::new(format!("TypeError: expected variable type, found {:?}", self))),
        }
    }

    pub fn get_varid(&self) -> Result<UniqueID, Error> {
        match self {
            &Type::Variable(_, ref id) => Ok(*id),
            _ => Err(Error::new(format!("TypeError: expected variable type, found {:?}", self))),
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

    pub fn num_funcdefs(&self) -> i32 {
        match *self {
            Type::Overload(ref variants) => variants.len() as i32,
            Type::Function(_, _, _) => 1,
            _ => 0
        }
    }

    pub fn add_variant(self, session: &Session, scope: ScopeRef, ntype: Type) -> Result<Type, Error> {
        let rtype = match self {
            Type::Overload(ref variants) => {
                for variant in variants {
                    if check_type(session, scope.clone(), Some(variant.clone()), Some(ntype.clone()), Check::Def, false).is_ok() {
                        return Err(Error::new(format!("OverloadError: function definitions overlap; {:?} vs {:?}", variant, ntype)));
                    }
                }
                let mut nvariants = variants.clone();
                nvariants.push(ntype);
                Type::Overload(nvariants)
            },
            _ => expect_type(session, scope, Some(self), Some(ntype.clone()), Check::Def)?,
        };
        Ok(rtype)
    }

    pub fn from_spec(classspec: ClassSpec) -> Type {
        Type::Object(classspec.ident.name, classspec.types)
    }

    pub fn update_variable_type(session: &Session, scope: ScopeRef, name: &String, ttype: Type) {
        let dscope = Scope::target(session, scope.clone());
        let pscope = Scope::locate_variable(dscope.clone(), name).unwrap();
        let otype = pscope.get_variable_type(name).clone();
        if !pscope.is_primative() {
            let ntype = check_type(session, scope.clone(), otype.clone(), Some(ttype.clone()), Check::Def, false);
            match ntype {
                Ok(utype) => dscope.set_variable_type(name, utype),
                Err(err) => panic!("while updating variable type {:?}:\n{:?}", name, err),
            }
        }
    }

    pub fn update_type(session: &Session, scope: ScopeRef, idname: &String, ttype: Type) {
        let ttype = resolve_type(scope.clone(), ttype);
        let pscope = Scope::locate_type(scope.clone(), idname).unwrap();
        let otype = pscope.find_type(idname).clone();
        debug!("UPDATE TYPE: from {:?} to {:?}", otype, ttype);
        if !pscope.is_primative() {
            let entype = check_type(session, scope.clone(), otype.clone(), Some(ttype.clone()), Check::Def, false);
            match entype {
                Ok(ntype) => {
                    //let ntype = check_type(session, scope, Some(ttype.clone()), Some(ntype.clone()), Check::Def, false).unwrap();
                    pscope.update_type(idname, ntype.clone());
                    //if ntype.is_variable() {
                    //    match ttype {
                    //        Type::Variable(ref name, ref id) if &id.to_string() != idname => { println!("$$$$: {:?} {:?}", id, ntype); scope.update_type(&id.to_string(), ntype.clone()) },
                    //        _ => { },
                    //    }
                    //}
                    match otype {
                        Some(Type::Variable(_, ref id)) if &id.to_string() != idname => Type::update_type(session, scope.clone(), &id.to_string(), ntype),
                        _ => { },
                    }
                },
                Err(err) => panic!("while updating type {:?}:\n{:?}", idname, err),
            }

            //if !Rc::ptr_eq(&pscope, &scope) {
            //    debug!("RAISING: {:?} {:?} {:?}", pscope.get_basename(), otype, ttype);
            //    pscope.raise_type(scope, otype.unwrap());
            //    pscope.raise_type(scope, ttype);
            //}
        }
    }

    /*
    pub fn move_typevars(scope: ScopeRef, fscope: ScopeRef, ttype: Type) -> Type {
        match ttype {
            Type::Variable(name, id) => {
                if !scope.contains(&name) {
                    let vtype = scope.new_typevar();
                    //fscope.types.remove(&name);
                    let pscope = Scope::locate_type(fscope, &id.to_string()).unwrap();
                    Type::update_type(pscope, &id.to_string(), vtype.clone());
                    vtype
                } else {
                    Type::Variable(name, id)
                }
            },
            Type::Function(args, ret, abi) => Type::Function(args.into_iter().map(|ttype| Type::move_typevars(scope, fscope, ttype)).collect(), Box::new(Type::move_typevars(scope, fscope, *ret)), abi),
            Type::Overload(variants) => Type::Overload(variants.into_iter().map(|ttype| Type::move_typevars(scope, fscope, ttype)).collect()),
            Type::Object(name, types) => Type::Object(name, types.into_iter().map(|ttype| Type::move_typevars(scope, fscope, ttype)).collect()),
        }
    }
    */

    pub fn display_vec(list: &Vec<Type>) -> String {
        list.iter().map(|t| format!("{}", t)).collect::<Vec<String>>().join(", ")
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Object(ref name, ref types) => {
                let params = if types.len() > 0 { format!("<{}>", types.iter().map(|p| format!("{}", p)).collect::<Vec<String>>().join(", ")) } else { String::from("") };
                write!(f, "{}{}", name, params)
            },
            Type::Variable(ref name, ref _id) => {
                write!(f, "'{}", name)
            }
            Type::Tuple(ref types) => {
                let tuple: Vec<String> = types.iter().map(|t| format!("{}", t)).collect();
                write!(f, "({})", tuple.join(", "))
            }
            Type::Function(ref args, ref ret, ref abi) => {
                write!(f, "{} -> {}{}", args, *ret, abi)
            }
            Type::Overload(ref variants) => {
                let varstr: Vec<String> = variants.iter().map(|v| format!("{}", v)).collect();
                write!(f, "Overload[{}]", varstr.join(", "))
            },
        }
    }
}

//impl fmt::Display for Vec<Type> {
//    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//        self.iter().map(|t| format!("{}", t)).collect().join(", ")
//    }
//}


#[derive(Clone, Debug, PartialEq)]
pub enum Check {
    Def,
    List,
    Update,
}

pub fn expect_type(session: &Session, scope: ScopeRef, odtype: Option<Type>, octype: Option<Type>, mode: Check) -> Result<Type, Error> {
    //match check_type(scope, odtype, octype, mode, true) {
    //    Ok(ttype) => ttype,
    //    Err(err) => panic!("{}", err),
    //}
    check_type(session, scope, odtype, octype, mode, true)
}

//
// Compare the defined type to the calculated type and return the common type, or error if they do not match
// dtype < ctype: ie. ctype will be downcast to match dtype, but not the inverse
//
pub fn check_type(session: &Session, scope: ScopeRef, odtype: Option<Type>, octype: Option<Type>, mode: Check, update: bool) -> Result<Type, Error> {
    debug!("TYPECHECK: {:?} {:?}", odtype, octype);
    if odtype.is_none() {
        // TODO should the else case just be Nil... 
        //Ok(octype.unwrap_or_else(|| scope.new_typevar()))
        match octype {
            Some(ctype) => Ok(resolve_type(scope, ctype)),
            None => Ok(scope.new_typevar()),
        }
    } else if octype.is_none() {
        Ok(odtype.unwrap())
    } else {
        let dtype = resolve_type(scope.clone(), odtype.unwrap());
        let ctype = resolve_type(scope.clone(), octype.unwrap());


/*
        if let Type::Variable(ref name, ref id) = ctype {
            //if mode == Check::Update && dtype.is_variable() {
            if dtype.is_variable() && !scope.contains_type(&dtype.get_varname().unwrap()) {
            //if dtype.is_variable() && !scope.contains_type(name) {
                if update { Type::update_type(scope, &dtype.get_varid().unwrap().to_string(), ctype.clone()); }
                //if update { scope.update_type(&dtype.get_varid().unwrap().to_string(), ctype.clone()); }
                Ok(resolve_type(scope, ctype.clone()))
            } else {
                if update { Type::update_type(scope, &id.to_string(), dtype.clone()); }
                //if update { scope.update_type(&id.to_string(), dtype.clone()); }
                Ok(resolve_type(scope, dtype))
            }
        } else if let Type::Variable(_, ref id) = dtype {
            if update { Type::update_type(scope, &id.to_string(), ctype.clone()); }
            //if update { scope.update_type(&id.to_string(), ctype.clone()); }
            Ok(resolve_type(scope, ctype))
        } else {
*/
        if let Type::Variable(ref _dname, ref did) = dtype {

            if let Type::Variable(ref cname, ref cid) = ctype {
                if scope.contains_type(cname) {
                    if update { scope.update_type(&did.to_string(), ctype.clone()); }
                    Ok(resolve_type(scope, ctype.clone()))
                } else {
                    if update { scope.update_type(&cid.to_string(), dtype.clone()); }
                    Ok(resolve_type(scope, dtype.clone()))
                }
            } else {
                if update {
                    Type::update_type(session, scope.clone(), &did.to_string(), ctype.clone());
                    Ok(resolve_type(scope, dtype.clone()))
                } else {
                    Ok(resolve_type(scope, ctype.clone()))
                }
            }
        } else if let Type::Variable(_, ref cid) = ctype {
                if update {
                    Type::update_type(session, scope.clone(), &cid.to_string(), dtype.clone());
                    Ok(resolve_type(scope, ctype.clone()))
                } else {
                    Ok(resolve_type(scope, dtype.clone()))
                }
        } else {
            match (dtype.clone(), ctype.clone()) {
                (Type::Function(ref aargs, ref aret, ref aabi), Type::Function(ref bargs, ref bret, ref babi)) => {
                    let oabi = aabi.compare(babi);
                    if oabi.is_some() {
                        let mut argtypes = check_type(session, scope.clone(), Some(*aargs.clone()), Some(*bargs.clone()), mode.clone(), update)?;
                        Ok(Type::Function(Box::new(argtypes), Box::new(check_type(session, scope, Some(*aret.clone()), Some(*bret.clone()), mode, update)?), oabi.unwrap()))
                    } else {
                        Err(Error::new(format!("TypeError: type mismatch, expected {} but found {}", dtype, ctype)))
                    }
                },
                (Type::Tuple(ref atypes), Type::Tuple(ref btypes)) => {
                    let mut types = vec!();
                    if atypes.len() == btypes.len() {
                        for (atype, btype) in atypes.iter().zip(btypes.iter()) {
                            types.push(check_type(session, scope.clone(), Some(atype.clone()), Some(btype.clone()), mode.clone(), update)?);
                        }
                        Ok(Type::Tuple(types))
                    } else {
                        Err(Error::new(format!("TypeError: type mismatch, expected {} but found {}", dtype, ctype)))
                    }
                },
                (Type::Object(ref aname, ref atypes), Type::Object(ref bname, ref btypes)) => {
                    match is_subclass_of(session, scope.clone(), (bname, btypes), (aname, atypes), mode.clone()) {
                        ok @ Ok(_) => ok,
                        err @ Err(_) => match mode {
                            Check::List => is_subclass_of(session, scope, (aname, atypes), (bname, btypes), mode.clone()),
                            _ => err,
                        }
                    }
                },
                (_, Type::Overload(_)) |
                (Type::Overload(_), _) => Err(Error::new(format!("TypeError: overloaded types are not allowed here..."))),
                _ => {
                    if dtype == ctype {
                        Ok(dtype)
                    } else {
                        Err(Error::new(format!("TypeError: type mismatch, expected {} but found {}", dtype, ctype)))
                    }
                }
            }
        }
    }
}


fn is_subclass_of(session: &Session, scope: ScopeRef, adef: (&String, &Vec<Type>), bdef: (&String, &Vec<Type>), mode: Check) -> Result<Type, Error> {
    debug!("IS SUBCLASS: {:?} of {:?}", adef, bdef);
    let tscope = Scope::new_ref(Some(scope.clone()));
    let mut names = Scope::map_new();
    let mut adef = (adef.0.clone(), adef.1.clone());

    loop {
        let mut class = scope.find_type(&adef.0).unwrap();
        class = tscope.map_typevars(&mut names, class);
        adef.1 = check_type_params(session, tscope.clone(), &class.get_params()?, &adef.1, mode.clone(), true)?;

        if *bdef.0 == adef.0 {
            let ptypes = if bdef.1.len() > 0 || adef.1.len() > 0 {
                check_type_params(session, tscope.clone(), &adef.1, bdef.1, mode.clone(), true)?
            } else {
                vec!()
            };
            //let rtype = Type::Object(adef.0, ptypes);
            //let rtype = resolve_type(&tscope, Type::Object(adef.0, ptypes));
            let rtype = resolve_type(tscope.clone(), tscope.unmap_typevars(&mut names, Type::Object(adef.0, ptypes)));
            debug!("DONE SUBCLASS: {:?}", rtype);
            return Ok(rtype);
            //return Ok(tscope.unmap_typevars(&mut names, Type::Object(adef.0, ptypes)));
        }

        let classdef = session.find_type_def(scope.clone(), &adef.0.as_str())?.as_class()?;
        let parent = classdef.parentspec.clone().map(|s| Type::from_spec(s));
        if parent.is_none() {
            return Err(Error::new(format!("TypeError: type mismatch, expected {} but found {}", Type::Object(adef.0.clone(), adef.1), Type::Object(bdef.0.clone(), bdef.1.clone()))));
        }
        let parent = tscope.map_typevars(&mut names, parent.unwrap());
        match resolve_type(tscope.clone(), parent) {
            Type::Object(name, params) => adef = (name, params),
            ttype @ _ => return Err(Error::new(format!("TypeError: expected Object but found {}", ttype))),
        }
    }
}

pub fn check_type_params(session: &Session, scope: ScopeRef, dtypes: &Vec<Type>, ctypes: &Vec<Type>, mode: Check, update: bool) -> Result<Vec<Type>, Error> {
    if dtypes.len() != ctypes.len() {
        Err(Error::new(format!("TypeError: number of type parameters don't match: expected {} but found {}", Type::display_vec(dtypes), Type::display_vec(ctypes))))
    } else {
        let mut ptypes = vec!();
        for (dtype, ctype) in dtypes.iter().zip(ctypes.iter()) {
            ptypes.push(check_type(session, scope.clone(), Some(dtype.clone()), Some(ctype.clone()), mode.clone(), update)?);
        }
        Ok(ptypes)
    }
}

pub fn resolve_type(scope: ScopeRef, ttype: Type) -> Type {
    match ttype {
        Type::Object(ref name, ref types) => {
            match scope.find_type(name) {
                Some(_) => {
                    let params = types.iter().map(|ptype| resolve_type(scope.clone(), ptype.clone())).collect();
                    // TODO we are purposely returning the original type here so as not to over-resolve types... but we should probably still fully resolve for checking purposes
                    Type::Object(name.clone(), params)
                },
                None => panic!("TypeError: undefined type {:?}", name),
            }
        },
        Type::Variable(_, ref id) => {
            //debug!("VARIABLE: in {:?}: {:?} resolves to {:?}", scope.get_full_name(&Some(String::from("")), UniqueID(0)), ttype, scope.find_type(&id.to_string()));
            match scope.find_type(&id.to_string()) {
                Some(vtype) => {
                    match vtype {
                        Type::Variable(_, ref eid) if eid == id => vtype.clone(),
                        _ => resolve_type(scope, vtype),
                    }
                },
                None => panic!("TypeError: undefined type variable {}", ttype),
            }
        },
        Type::Tuple(ref types) => {
            let types = types.iter().map(|ttype| resolve_type(scope.clone(), ttype.clone())).collect();
            Type::Tuple(types)
        },
        Type::Function(ref args, ref ret, ref abi) => {
            Type::Function(Box::new(resolve_type(scope.clone(), *args.clone())), Box::new(resolve_type(scope, *ret.clone())), *abi)
        },
        Type::Overload(ref variants) => {
            let newvars = variants.iter().map(|variant| resolve_type(scope.clone(), variant.clone())).collect();
            Type::Overload(newvars)
        },
    }
}

pub fn find_variant(session: &Session, scope: ScopeRef, otype: Type, atypes: Type, mode: Check) -> Result<Type, Error> {
    match otype {
        Type::Overload(variants) => {
            let variants = remove_duplicates(session, scope.clone(), variants);
            let mut found = vec!();
            'outer: for variant in &variants {
                if let Type::Function(ref otypes, _, _) = *variant {
                    debug!("**CHECKING VARIANT: {:?} {:?}", otypes, atypes);
                    if check_type(session, scope.clone(), Some(*otypes.clone()), Some(atypes.clone()), mode.clone(), false).is_err() {
                        continue 'outer;
                    }
                    found.push(variant);
                }
            }

            match found.len() {
                0 => Err(Error::new(format!("OverloadError: No valid variant found for ({})\n\tout of {}", atypes, Type::display_vec(&variants)))),
                1 => Ok(found[0].clone()),
                _ => Err(Error::new(format!("OverloadError: Ambiguous ({})\n\tvariants found {}", atypes, found.iter().map(|t| format!("{}", t)).collect::<Vec<String>>().join(", ")))),
            }
        },
        _ => Ok(otype.clone()),
    }
}

// TODO this is sort of a hack that can maybe be integrated into get_variable_type instead
pub fn remove_duplicates(session: &Session, scope: ScopeRef, mut variants: Vec<Type>) -> Vec<Type> {
    let mut i = 1;

    while i < variants.len() {
        for j in 0 .. i {
            if check_type(session, scope.clone(), Some(variants[i].clone()), Some(variants[j].clone()), Check::List, false).is_ok() {
                variants.remove(i);
                i -= 1;
                break;
            }
        }
        i += 1;
    }
    return variants;
}


