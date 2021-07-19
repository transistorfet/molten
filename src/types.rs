
use std::fmt;
use std::collections::HashMap;

use defs::Def;
use hir::ClassSpec;
use misc::{ R, r, UniqueID };
use session::{ Session, Error };

pub use abi::ABI;


/*
#[derive(Clone, Debug, PartialEq)]
struct Constraint {
    name: String,
    params: Vec<Type>,
}
*/


#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Object(String, UniqueID, Vec<Type>),
    Tuple(Vec<Type>),
    Record(Vec<(String, Type)>),
    Ref(R<Type>),
    Function(R<Type>, R<Type>, ABI),
    Variable(UniqueID),
    Universal(String, UniqueID),
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
            Type::Universal(_, _) => true,
            Type::Variable(_) => true,
            _ => false
        }
    }

    pub fn get_id(&self) -> Result<UniqueID, Error> {
        match self {
            &Type::Object(_, ref id, _) => Ok(*id),
            &Type::Variable(ref id) => Ok(*id),
            &Type::Universal(_, ref id) => Ok(*id),
            _ => Err(Error::new(format!("TypeError: expected object or variable type, found {:?}", self))),
        }
    }

    pub fn from_spec(classspec: ClassSpec, id: UniqueID) -> Type {
        Type::Object(classspec.ident.name, id, classspec.types)
    }

    pub fn display_vec(list: &Vec<Type>) -> String {
        list.iter().map(|t| format!("{}", t)).collect::<Vec<String>>().join(", ")
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Object(ref name, _, ref types) => {
                let params = if types.len() > 0 { format!("<{}>", types.iter().map(|p| format!("{}", p)).collect::<Vec<String>>().join(", ")) } else { String::from("") };
                write!(f, "{}{}", name, params)
            },
            Type::Variable(ref id) => {
                write!(f, "?{}", id)
            }
            Type::Universal(ref name, _) => {
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
        }
    }
}

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
            Type::Universal(_, id) => {
                match varmap.get(&id).map(|x| x.clone()) {
                    Some(ptype) => ptype,
                    None => {
                        let maptype = session.new_typevar();
                        varmap.insert(id, maptype.clone());
                        maptype
                    }
                }
            },
            Type::Variable(id) => Type::Variable(id),
            Type::Function(args, ret, abi) => Type::Function(r(Type::map_typevars(session, varmap, *args)), r(Type::map_typevars(session, varmap, *ret)), abi),
            Type::Tuple(types) => Type::Tuple(Type::map_typevars_vec(session, varmap, types)),
            Type::Record(types) => Type::Record(types.into_iter().map(|(n, t)| (n, Type::map_typevars(session, varmap, t))).collect()),
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

/// Compare the defined type to the calculated type and return the common type, or error if they cannot be unified
///
/// If mode is Check::Def, then dtype <: ctype (ie. ctype will be downcast to match dtype, but not the inverse)
/// If mode is Check::List, then forall X, X <: dtype AND X <: ctype (ie. the type that both given types can downcast to)
pub fn check_type(session: &Session, odtype: Option<Type>, octype: Option<Type>, mode: Check, update: bool) -> Result<Type, Error> {
    if odtype.is_none() {
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

        // If the update flag is false, then universal variables can unify with any type.  This
        // assumes that update is only false when not type checking, such as looking for overloaded
        // function variants, or making sure overloaded functions definitions don't overlap
        if !update {
            match (&dtype, &ctype) {
                (Type::Universal(_, _), ntype) |
                (ntype, Type::Universal(_, _)) if !update => {
                    return Ok(ntype.clone());
                }
                _ => { },
            }
        }

        match (&dtype, &ctype) {
            (Type::Variable(_), Type::Variable(ref id)) => {
                if update { session.set_type(*id, dtype.clone()); }
                Ok(resolve_type(session, dtype, false)?)
            },

            (Type::Variable(ref id), ntype) |
            (ntype, Type::Variable(ref id)) => {
                if update { session.update_type(*id, ntype.clone())?; }
                Ok(resolve_type(session, ntype.clone(), false)?)
            }

            (Type::Universal(_, ref aid), Type::Universal(_, ref bid)) if aid == bid => {
                Ok(dtype.clone())
            },

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
                match is_subclass_of(session, (bname, *bid, btypes), (aname, *aid, atypes), mode, update) {
                    ok @ Ok(_) => ok,
                    err @ Err(_) => match mode {
                        Check::List => is_subclass_of(session, (aname, *aid, atypes), (bname, *bid, btypes), mode, update),
                        _ => err,
                    }
                }
            },

            (Type::Ref(ref atype), Type::Ref(ref btype)) => {
                let ttype = check_type(session, Some(*atype.clone()), Some(*btype.clone()), mode, update)?;
                Ok(Type::Ref(r(ttype)))
            },

            _ => {
                Err(Error::new(format!("TypeError: type mismatch, expected {} but found {}", dtype, ctype)))
            }
        }
    }
}


fn is_subclass_of(session: &Session, adef: (&String, UniqueID, &Vec<Type>), bdef: (&String, UniqueID, &Vec<Type>), mode: Check, update: bool) -> Result<Type, Error> {
    debug!("IS SUBCLASS: {:?} of {:?}", adef, bdef);
    let mut names = Type::map_new();
    let mut adef = (adef.0.clone(), adef.1, adef.2.clone());
    //let mut deftype = session.get_type(adef.1)?.get_params()?;

    loop {
        let mut class = session.get_type(adef.1).unwrap();
        if update {
            class = Type::map_typevars(session, &mut names, class);
        }
        adef.2 = check_type_params(session, &class.get_params()?, &adef.2, mode, update)?;

        if *bdef.0 == adef.0 {
            let ptypes = if bdef.2.len() > 0 || adef.2.len() > 0 {
                check_type_params(session, &adef.2, bdef.2, mode, update)?
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
        let parent = classdef.parenttype.clone().unwrap();
        let parent = if update {
            Type::map_typevars(session, &mut names, parent)
        } else {
            parent
        };
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
        Type::Variable(ref id) => {
            match session.get_type(*id) {
                Some(vtype) => {
                    match vtype {
                        Type::Variable(ref eid) if eid == id => {
                            if !require_resolve {
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
        Type::Universal(ref name, ref id) => {
            Ok(Type::Universal(name.clone(), *id))
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
    }
}


