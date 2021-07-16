
use std::fmt;
use std::collections::HashMap;

use defs::Def;
use hir::ClassSpec;
use misc::{ R, r, UniqueID };
use session::{ Session, Error };

pub use abi::ABI;


#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Object(String, UniqueID, Vec<Type>),
    Tuple(Vec<Type>),
    Record(Vec<(String, Type)>),
    Ref(R<Type>),
    Function(R<Type>, R<Type>, ABI),

    Variable(String, UniqueID, bool),
    //Existential(UniqueID),

    // TODO this isn't used atm, I don't think, but we could use it for a constrained type
    Ambiguous(Vec<Type>),
    //Constrained(R<&mut AST>),
}

impl Type {
    pub fn get_name<'a>(&'a self) -> Result<&'a str, Error> {
        match &self {
            Type::Object(name, _, _) => Ok(name),
            _ => Err(Error::new(format!("TypeError: expected a class or concrete type, found {:?}", self))),
        }
    }

    pub fn get_params(&self) -> Result<Vec<Type>, Error> {
        match *self {
            Type::Object(_, _, ref params) => Ok(params.clone()),
            _ => Err(Error::new(format!("TypeError: expected a class or concrete type, found {:?}", self))),
        }
    }

    #[allow(dead_code)]
    pub fn is_tuple(&self) -> bool {
        match *self {
            Type::Tuple(_) => true,
            _ => false
        }
    }

    #[allow(dead_code)]
    pub fn as_vec(&self) -> Vec<Type> {
        match self {
            &Type::Tuple(ref types) => types.clone(),
            _ => vec!(self.clone()),
        }
    }


    #[allow(dead_code)]
    pub fn is_record(&self) -> bool {
        match *self {
            Type::Record(_) => true,
            _ => false
        }
    }

    pub fn get_record_types(&self) -> Result<&Vec<(String, Type)>, Error> {
        match self {
            &Type::Record(ref items) => Ok(items),
            _ => Err(Error::new(format!("TypeError: expected record type, found {:?}", self))),
        }
    }

    pub fn get_record_field(&self, name: &str) -> Result<&Type, Error> {
        match self {
            &Type::Record(ref items) => {
                for (ref ident, ref ttype) in items {
                    if ident.as_str() == name {
                        return Ok(ttype);
                    }
                }
                Err(Error::new(format!("TypeError: no field named {:?} exists in record of type {:?}", name, self)))
            },
            _ => Err(Error::new(format!("TypeError: expected record type, found {:?}", self))),
        }
    }


    #[allow(dead_code)]
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

    pub fn get_function_types(&self) -> Result<(&Type, &Type, ABI), Error> {
        match self {
            Type::Function(atypes, rtype, abi) => Ok((atypes, rtype, *abi)),
            ftype @ _ => Err(Error::new(format!("TypeError: expected function type: {:?}", ftype))),
        }
    }


    #[allow(dead_code)]
    pub fn is_variable(&self) -> bool {
        match *self {
            Type::Variable(_, _, _) => true,
            _ => false
        }
    }

    #[allow(dead_code)]
    pub fn get_varname(&self) -> Result<String, Error> {
        match self {
            &Type::Variable(ref name, _, _) => Ok(name.clone()),
            _ => Err(Error::new(format!("TypeError: expected variable type, found {:?}", self))),
        }
    }

    pub fn get_id(&self) -> Result<UniqueID, Error> {
        match self {
            &Type::Object(_, ref id, _) => Ok(*id),
            &Type::Variable(_, ref id, _) => Ok(*id),
            _ => Err(Error::new(format!("TypeError: expected object or variable type, found {:?}", self))),
        }
    }

    #[allow(dead_code)]
    pub fn is_ambiguous(&self) -> bool {
        match *self {
            Type::Ambiguous(_) => true,
            _ => false
        }
    }

    pub fn from_spec(classspec: ClassSpec, id: UniqueID) -> Type {
        Type::Object(classspec.ident.name, id, classspec.types)
    }

    /*
    pub fn convert<F>(self, f: &F) -> Type where F: FnOnce(Type) -> Type {
        let ttype = match self {
            Type::Object(name, id, types) => {
                let types = types.into_iter().map(move |ttype| {
                    ttype.convert(f)
                }).collect();
                Type::Object(name, id, types)
            },
            Type::Ambiguous(types) => {
                let types = types.into_iter().map(move |ttype| {
                    ttype.convert(f)
                }).collect();
                Type::Ambiguous(types)
            },
            Type::Tuple(types) => {
                let types = types.into_iter().map(move |ttype| {
                    ttype.convert(f)
                }).collect();
                Type::Tuple(types)
            },
            Type::Function(args, ret, abi) => {
                let args = args.convert(f);
                let ret = ret.convert(f);
                Type::Function(r(args), r(ret), abi)
            },
            Type::Variable(name, id, universal) => {
                Type::Variable(name, id, universal)
            }
        };
        f(ttype)
    }
    */


    pub fn display_vec(list: &Vec<Type>) -> String {
        list.iter().map(|t| format!("{}", t)).collect::<Vec<String>>().join(", ")
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Object(ref name, ref _id, ref types) => {
                let params = if types.len() > 0 { format!("<{}>", types.iter().map(|p| format!("{}", p)).collect::<Vec<String>>().join(", ")) } else { String::from("") };
                write!(f, "{}{}", name, params)
            },
            Type::Variable(ref name, ref _id, _) => {
                write!(f, "'{}", name)
            }
            Type::Tuple(ref types) => {
                let tuple: Vec<String> = types.iter().map(|t| format!("{}", t)).collect();
                write!(f, "({})", tuple.join(", "))
            }
            Type::Record(ref types) => {
                let tuple: Vec<String> = types.iter().map(|(n, t)| format!("{}: {}", n, t)).collect();
                write!(f, "{{ {} }}", tuple.join(", "))
            }
            Type::Function(ref args, ref ret, ref abi) => {
                write!(f, "{} -> {}{}", args, *ret, abi)
            }
            Type::Ref(ref ttype) => {
                write!(f, "ref {}", ttype)
            }
            Type::Ambiguous(ref variants) => {
                let varstr: Vec<String> = variants.iter().map(|v| format!("{}", v)).collect();
                write!(f, "Ambiguous[{}]", varstr.join(", "))
            },
        }
    }
}

//impl fmt::Display for Vec<Type> {
//    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//        self.iter().map(|t| format!("{}", t)).collect().join(", ")
//    }
//}


impl Type {
    pub fn map_all_typevars(session: &Session, ttype: Type) -> Type {
        let mut varmap = Type::map_new();
        debug!("MAPPING ALL: {:?}", ttype);
        Type::map_typevars(session, &mut varmap, ttype)
    }

    pub fn map_new() -> HashMap<UniqueID, Type> {
        HashMap::new()
    }

    pub fn map_typevars(session: &Session, varmap: &mut HashMap<UniqueID, Type>, ttype: Type) -> Type {
        match ttype.clone() {
            Type::Variable(name, id, universal) => {
                match varmap.get(&id).map(|x| x.clone()) {
                    Some(ptype) => ptype,
                    None => {
                        let etype = session.get_type(id);
                        debug!("EXISTING TYPEVAR for {:?}: {:?} vs {:?}", name, etype, id);
                        match Some(ttype) {
                            Some(Type::Variable(_, ref eid, _)) if *eid == id && !universal => etype.clone().unwrap(),
                            None | Some(Type::Variable(_, _, _)) => {
                                let orgtype = Type::Variable(name.clone(), id, universal);
                                if universal {
                                    let maptype = session.new_typevar();
                                    varmap.insert(id, maptype.clone());
                                    debug!("MAPPED from {:?} to {:?}", orgtype, maptype);
                                    maptype
                                } else {
                                    debug!("NOT MAPPED: {:?}", orgtype);
                                    orgtype.clone()
                                }
                            },
                            _ => etype.clone().unwrap(),
                        }
                    }
                }
            },
            Type::Function(args, ret, abi) => Type::Function(r(Type::map_typevars(session, varmap, *args)), r(Type::map_typevars(session, varmap, *ret)), abi),
            Type::Tuple(types) => Type::Tuple(Type::map_typevars_vec(session, varmap, types)),
            Type::Record(types) => Type::Record(types.into_iter().map(|(n, t)| (n, Type::map_typevars(session, varmap, t))).collect()),
            Type::Ambiguous(variants) => Type::Ambiguous(Type::map_typevars_vec(session, varmap, variants)),
            Type::Ref(ttype) => Type::Ref(r(Type::map_typevars(session, varmap, *ttype))),
            Type::Object(name, id, types) => Type::Object(name.clone(), id, Type::map_typevars_vec(session, varmap, types)),
        }
    }

    pub fn map_typevars_vec(session: &Session, varmap: &mut HashMap<UniqueID, Type>, types: Vec<Type>) -> Vec<Type> {
        types.into_iter().map(|vtype| Type::map_typevars(session, varmap, vtype)).collect()
    }
}


#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Check {
    Def,
    List,
}

pub fn expect_type(session: &Session, odtype: Option<Type>, octype: Option<Type>, mode: Check) -> Result<Type, Error> {
    check_type(session, odtype, octype, mode, true)
}

//
// Compare the defined type to the calculated type and return the common type, or error if they do not match
// dtype < ctype: ie. ctype will be downcast to match dtype, but not the inverse
//
pub fn check_type(session: &Session, odtype: Option<Type>, octype: Option<Type>, mode: Check, update: bool) -> Result<Type, Error> {
    //debug!("TYPECHECK: {:?} {:?}", odtype, octype);
    if odtype.is_none() {
        // TODO should the else case just be Nil...
        match octype {
            Some(ctype) => Ok(resolve_type(session, ctype, false)?),
            None => Ok(session.new_typevar()),
        }
    } else if octype.is_none() {
        Ok(odtype.unwrap())
    } else {
        let dtype = resolve_type(session, odtype.unwrap(), false)?;
        let ctype = resolve_type(session, octype.unwrap(), false)?;

        debug!("CHECK TYPE {:?} {:?} {:?}", dtype, ctype, update);
        if let Type::Variable(ref _dname, ref did, duni) = dtype {

            if let Type::Variable(ref _cname, ref cid, cuni) = ctype {
                if cuni {
                    if update && !duni { session.set_type(*did, ctype.clone()); }
                    Ok(resolve_type(session, ctype.clone(), false)?)
                } else {
                    if update && !cuni { session.set_type(*cid, dtype.clone()); }
                    Ok(resolve_type(session, dtype.clone(), false)?)
                }
            } else {
                if update && !duni { session.update_type(*did, ctype.clone())?; }
                Ok(resolve_type(session, ctype.clone(), false)?)
            }
        } else if let Type::Variable(_, ref cid, cuni) = ctype {
            if update && !cuni { session.update_type(*cid, dtype.clone())?; }
            Ok(resolve_type(session, dtype.clone(), false)?)
            //Err(Error::new(format!("TypeError: attempting to cast a type variable {} to a concrete {}", ctype, dtype)))
        } else {
            match (dtype.clone(), ctype.clone()) {
                (Type::Function(ref aargs, ref aret, ref aabi), Type::Function(ref bargs, ref bret, ref babi)) => {
                    let oabi = aabi.compare(babi);
                    if oabi.is_some() {
                        let argtypes = check_type(session, Some(*aargs.clone()), Some(*bargs.clone()), mode, update)?;
                        Ok(Type::Function(r(argtypes), r(check_type(session, Some(*aret.clone()), Some(*bret.clone()), mode, update)?), oabi.unwrap()))
                    } else {
                        Err(Error::new(format!("TypeError: type mismatch, expected {} but found {}", dtype, ctype)))
                    }
                },
                (Type::Tuple(ref atypes), Type::Tuple(ref btypes)) => {
                    let mut types = vec!();
                    if atypes.len() == btypes.len() {
                        for (atype, btype) in atypes.iter().zip(btypes.iter()) {
                            types.push(check_type(session, Some(atype.clone()), Some(btype.clone()), mode, update)?);
                        }
                        Ok(Type::Tuple(types))
                    } else {
                        Err(Error::new(format!("TypeError: type mismatch, expected {} but found {}", dtype, ctype)))
                    }
                },
                (Type::Record(ref atypes), Type::Record(ref btypes)) => {
                    let mut types = vec!();
                    if atypes.len() == btypes.len() {
                        for ((aname, atype), (bname, btype)) in atypes.iter().zip(btypes.iter()) {
                            if aname != bname {
                                return Err(Error::new(format!("TypeError: type mismatch, expected {} but found {}", dtype, ctype)));
                            }
                            types.push((aname.clone(), check_type(session, Some(atype.clone()), Some(btype.clone()), mode, update)?));
                        }
                        Ok(Type::Record(types))
                    } else {
                        Err(Error::new(format!("TypeError: type mismatch, expected {} but found {}", dtype, ctype)))
                    }
                },
                (Type::Object(ref aname, ref aid, ref atypes), Type::Object(ref bname, ref bid, ref btypes)) => {
                    match is_subclass_of(session, (bname, *bid, btypes), (aname, *aid, atypes), mode) {
                        ok @ Ok(_) => ok,
                        err @ Err(_) => match mode {
                            Check::List => is_subclass_of(session, (aname, *aid, atypes), (bname, *bid, btypes), mode),
                            _ => err,
                        }
                    }
                },
                (Type::Ref(ref atype), Type::Ref(ref btype)) => {
                    let ttype = check_type(session, Some(*atype.clone()), Some(*btype.clone()), mode, update)?;
                    Ok(Type::Ref(r(ttype)))
                },
                (_, Type::Ambiguous(_)) |
                (Type::Ambiguous(_), _) => Err(Error::new(format!("TypeError: overloaded types are not allowed here..."))),
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


fn is_subclass_of(session: &Session, adef: (&String, UniqueID, &Vec<Type>), bdef: (&String, UniqueID, &Vec<Type>), mode: Check) -> Result<Type, Error> {
    debug!("IS SUBCLASS: {:?} of {:?}", adef, bdef);
    let mut names = Type::map_new();
    let mut adef = (adef.0.clone(), adef.1, adef.2.clone());
    //let mut deftype = session.get_type(adef.1)?.get_params()?;

    loop {
        let mut class = session.get_type(adef.1).unwrap();
        class = Type::map_typevars(session, &mut names, class);
        adef.2 = check_type_params(session, &class.get_params()?, &adef.2, mode, true)?;

        if *bdef.0 == adef.0 {
            let ptypes = if bdef.2.len() > 0 || adef.2.len() > 0 {
                check_type_params(session, &adef.2, bdef.2, mode, true)?
            } else {
                vec!()
            };
            let rtype = Type::Object(adef.0, adef.1, ptypes);
            debug!("DONE SUBCLASS: {:?}", rtype);
            return Ok(rtype);
        }

        let classdef = session.get_def(adef.1)?.as_class()?;
        if classdef.parenttype.is_none() {
            return Err(Error::new(format!("TypeError: type mismatch, expected {} but found {}", Type::Object(bdef.0.clone(), bdef.1, bdef.2.clone()), Type::Object(adef.0.clone(), adef.1, adef.2))));
        }
        let parent = Type::map_typevars(session, &mut names, classdef.parenttype.clone().unwrap());
        match resolve_type(session, parent, false)? {
            Type::Object(name, id, params) => adef = (name, id, params),
            ttype @ _ => return Err(Error::new(format!("TypeError: expected Object but found {}", ttype))),
        }
    }
}

pub fn check_type_params(session: &Session, dtypes: &Vec<Type>, ctypes: &Vec<Type>, mode: Check, update: bool) -> Result<Vec<Type>, Error> {
    if dtypes.len() != ctypes.len() {
        Err(Error::new(format!("TypeError: number of type parameters don't match: expected {} but found {}", Type::display_vec(dtypes), Type::display_vec(ctypes))))
    } else {
        let mut ptypes = vec!();
        for (dtype, ctype) in dtypes.iter().zip(ctypes.iter()) {
            ptypes.push(check_type(session, Some(dtype.clone()), Some(ctype.clone()), mode, update)?);
        }
        Ok(ptypes)
    }
}

pub fn resolve_type(session: &Session, ttype: Type, require_resolve: bool) -> Result<Type, Error> {
    match ttype {
        Type::Object(ref name, ref id, ref types) => {
            let params = types.iter().map(|ptype| resolve_type(session, ptype.clone(), require_resolve)).collect::<Result<Vec<Type>, Error>>()?;

            match session.get_def(*id) {
                Ok(Def::TypeAlias(alias)) => {
                    Ok(alias.resolve(session, params).unwrap())
                },
                _ => match session.get_type(*id) {
                    // TODO we are purposely returning the original type here so as not to over-resolve types... but we should probably still fully resolve for checking purposes
                    Some(_) => Ok(Type::Object(name.clone(), *id, params)),
                    None => Err(Error::new(format!("TypeError: undefined type {:?}", name))).unwrap(),
                },
            }
        },
        Type::Variable(_, ref id, _) => {
            match session.get_type(*id) {
                Some(vtype) => {
                    debug!("~~~~~~ {:?} -> {:?}", id, vtype);
                    match vtype {
                        Type::Variable(_, ref eid, ref euni) if eid == id => {
                            if !require_resolve || *euni {
                                Ok(vtype.clone())
                            } else {
                                Err(Error::new(format!("TypeError: unification variable unresolved: {}", vtype)))
                            }
                        },
                        _ => resolve_type(session, vtype, require_resolve),
                    }
                },
                None => Err(Error::new(format!("TypeError: undefined type variable {}", ttype))),
            }
        },
        Type::Tuple(ref types) => {
            let types = types.iter().map(|ttype| resolve_type(session, ttype.clone(), require_resolve)).collect::<Result<Vec<Type>, Error>>()?;
            Ok(Type::Tuple(types))
        },
        Type::Record(ref types) => {
            let types = types.iter().map(|(name, ttype)| {
                let ttype = resolve_type(session, ttype.clone(), require_resolve)?;
                Ok((name.clone(), ttype))
            }).collect::<Result<Vec<(String, Type)>, Error>>()?;
            Ok(Type::Record(types))
        },
        Type::Function(ref args, ref ret, ref abi) => {
            Ok(Type::Function(r(resolve_type(session, *args.clone(), require_resolve)?), r(resolve_type(session, *ret.clone(), require_resolve)?), *abi))
        },
        Type::Ref(ref ttype) => {
            Ok(Type::Ref(r(resolve_type(session, *ttype.clone(), require_resolve)?)))
        },
        Type::Ambiguous(ref variants) => {
            let newvars = variants.iter().map(|variant| resolve_type(session, variant.clone(), require_resolve)).collect::<Result<Vec<Type>, Error>>()?;
            Ok(Type::Ambiguous(newvars))
        },
    }
}


