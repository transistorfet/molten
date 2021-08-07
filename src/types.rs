
use std::fmt;
use std::convert::From;
use std::collections::HashMap;

use crate::defs::Def;
use crate::misc::{ R, r, UniqueID };
use crate::hir::{ NodeID };
use crate::session::{ Session, Error };

pub use crate::abi::ABI;


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
    #[allow(dead_code)]
    pub fn is_tuple(&self) -> bool {
        match self {
            Type::Tuple(_) => true,
            _ => false
        }
    }

    #[allow(dead_code)]
    pub fn is_record(&self) -> bool {
        match self {
            Type::Record(_) => true,
            _ => false
        }
    }

    #[allow(dead_code)]
    pub fn is_variable(&self) -> bool {
        match self {
            Type::Universal(_, _) => true,
            Type::Variable(_) => true,
            _ => false
        }
    }

    #[allow(dead_code)]
    pub fn is_function(&self) -> bool {
        match self {
            Type::Function(_, _, _) => true,
            _ => false
        }
    }


    pub fn get_name<'a>(&'a self) -> Result<&'a str, Error> {
        match self {
            Type::Object(name, _, _) => Ok(name),
            Type::Universal(name, _) => Ok(name),
            _ => Err(Error::new(format!("TypeError: expected a class or concrete type, found {:?}", self))),
        }
    }

    pub fn get_params(&self) -> Result<Vec<Type>, Error> {
        match self {
            Type::Object(_, _, params) => Ok(params.clone()),
            _ => Err(Error::new(format!("TypeError: expected a class or concrete type, found {:?}", self))),
        }
    }

    pub fn as_vec(&self) -> Vec<Type> {
        match self {
            Type::Tuple(types) => types.clone(),
            _ => vec!(self.clone()),
        }
    }

    pub fn get_record_types(&self) -> Result<&Vec<(String, Type)>, Error> {
        match self {
            Type::Record(items) => Ok(items),
            _ => Err(Error::new(format!("TypeError: expected record type, found {:?}", self))),
        }
    }

    pub fn get_record_field(&self, name: &str) -> Result<&Type, Error> {
        match self {
            Type::Record(items) => {
                for (ident, ttype) in items {
                    if ident == name {
                        return Ok(ttype);
                    }
                }
                Err(Error::new(format!("TypeError: no field named {:?} exists in record of type {:?}", name, self)))
            },
            _ => Err(Error::new(format!("TypeError: expected record type, found {:?}", self))),
        }
    }

    pub fn get_argtypes(&self) -> Result<&Type, Error> {
        match self {
            Type::Function(args, _, _) => Ok(args),
            _ => Err(Error::new(format!("TypeError: expected function type, found {:?}", self))),
        }
    }

    pub fn get_rettype(&self) -> Result<&Type, Error> {
        match self {
            Type::Function(_, ret, _) => Ok(&**ret),
            _ => Err(Error::new(format!("TypeError: expected function type, found {:?}", self))),
        }
    }

    pub fn get_abi(&self) -> Result<ABI, Error> {
        match self {
            Type::Function(_, _, abi) => Ok(*abi),
            _ => Err(Error::new(format!("TypeError: expected function type, found {:?}", self))),
        }
    }

    pub fn get_function_types(&self) -> Result<(&Type, &Type, ABI), Error> {
        match self {
            Type::Function(atypes, rtype, abi) => Ok((atypes, rtype, *abi)),
            ftype @ _ => Err(Error::new(format!("TypeError: expected function type: {:?}", ftype))),
        }
    }

    pub fn get_id(&self) -> Result<UniqueID, Error> {
        match self {
            Type::Object(_, id, _) => Ok(*id),
            Type::Variable(id) => Ok(*id),
            Type::Universal(_, id) => Ok(*id),
            _ => Err(Error::new(format!("TypeError: expected object or variable type, found {:?}", self))),
        }
    }

    pub fn set_id(mut self, newid: NodeID) -> Result<Type, Error> {
        match &mut self {
            Type::Object(_, id, _) => {
                *id = newid;
                Ok(self)
            },
            _ => Err(Error::new(format!("TypeError: expected object when setting ID, found {:?}", self))),
        }
    }
}


impl Type {
    pub fn display_vec(list: &Vec<Type>) -> String {
        list.iter().map(|t| format!("{}", t)).collect::<Vec<String>>().join(", ")
    }

    pub fn display_full(session: &Session, ttype: &Type) -> String {
        let mut typename = format!("{}", ttype);
        match ttype {
            Type::Universal(_, id) | Type::Variable(id) => {
                let constraints = session.get_constraints(*id);
                if !constraints.is_empty() {
                    typename += &format!(" with constraints {}", constraints.iter().map(|t| {
                        let traitdef = session.get_def(*t).unwrap().as_trait_def().unwrap();
                        format!("{}", traitdef.deftype.get_name().unwrap())
                    }).collect::<Vec<String>>().join(", "));
                }
            },
            _ => { },
        }
        typename
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Object(name, _, types) => {
                let params = if types.len() > 0 { format!("<{}>", types.iter().map(|p| format!("{}", p)).collect::<Vec<String>>().join(", ")) } else { String::from("") };
                write!(f, "{}{}", name, params)
            },
            Type::Variable(id) => {
                write!(f, "?{}", id)
            }
            Type::Universal(name, _) => {
                write!(f, "'{}", name)
            }
            Type::Tuple(types) => {
                let tuple: Vec<String> = types.iter().map(|t| format!("{}", t)).collect();
                write!(f, "({})", tuple.join(", "))
            }
            Type::Record(types) => {
                let tuple: Vec<String> = types.iter().map(|(n, t)| format!("{}: {}", n, t)).collect();
                write!(f, "{{ {} }}", tuple.join(", "))
            }
            Type::Function(args, ret, abi) => {
                write!(f, "{} -> {}{}", args, *ret, abi)
            }
            Type::Ref(ttype) => {
                write!(f, "{}", ttype)
            }
        }
    }
}

/// Map universal type variables into placeholder variables and return the resulting type
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
                        session.set_constraints(maptype.get_id().unwrap(), session.get_constraints(id));
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

/// Recursively resolve any placeholder variables and type aliases in the given type
/// If require_resolve is true, an error will thrown if a placeholder variable cannot be resolved to another type
pub fn resolve_type(session: &Session, ttype: Type, require_resolve: bool) -> Result<Type, Error> {
    match ttype {
        Type::Object(name, id, types) => {
            let params = types.into_iter().map(|ptype| resolve_type(session, ptype, require_resolve)).collect::<Result<Vec<Type>, Error>>()?;

            match session.get_def(id) {
                Ok(Def::TypeAlias(alias)) => {
                    Ok(alias.resolve(session, &params)?)
                },
                _ => match session.get_type(id) {
                    // NOTE we have a special case for objects because we want to return the resolved parameters and not the parameters the type was defined with
                    Some(Type::Object(sname, sid, _)) => {
                        match id == sid {
                            true => Ok(Type::Object(name, id, params)),
                            false => Err(Error::new(format!("Unexpected type alias used, {} points to {}", name, sname))),
                        }
                    },
                    Some(stype) => Ok(stype.clone()),
                    None => Err(Error::new(format!("TypeError: undefined type {:?}", name))),
                },
            }
        },
        Type::Variable(id) => {
            match session.get_type(id) {
                Some(vtype) => {
                    match vtype {
                        Type::Variable(eid) if eid == id => {
                            if !require_resolve {
                                Ok(vtype.clone())
                            } else {
                                Err(Error::new(format!("TypeError: unification variable unresolved: {}", vtype)))
                            }
                        },
                        _ => resolve_type(session, vtype.clone(), require_resolve),
                    }
                },
                None => Err(Error::new(format!("TypeError: undefined type variable {}", ttype))),
            }
        },
        Type::Universal(name, id) => {
            Ok(Type::Universal(name, id))
        },
        Type::Tuple(types) => {
            let types = types.into_iter().map(|ttype| resolve_type(session, ttype, require_resolve)).collect::<Result<Vec<Type>, Error>>()?;
            Ok(Type::Tuple(types))
        },
        Type::Record(types) => {
            let types = types.into_iter().map(|(name, ttype)| {
                let ttype = resolve_type(session, ttype, require_resolve)?;
                Ok((name, ttype))
            }).collect::<Result<Vec<(String, Type)>, Error>>()?;
            Ok(Type::Record(types))
        },
        Type::Function(args, ret, abi) => {
            Ok(Type::Function(r(resolve_type(session, *args, require_resolve)?), r(resolve_type(session, *ret, require_resolve)?), abi))
        },
        Type::Ref(ttype) => {
            Ok(Type::Ref(r(resolve_type(session, *ttype, require_resolve)?)))
        },
    }
}


#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Check {
    Def,
    List,
}

pub fn expect_type(session: &Session, odtype: Option<&Type>, octype: Option<&Type>, mode: Check) -> Result<Type, Error> {
    check_type(session, odtype, octype, mode, true)
}

/// Compare the defined type to the calculated type and return the common type, or error if they cannot be unified
///
/// If mode is Check::Def, then dtype <: ctype (ie. ctype will be downcast to match dtype, but not the inverse)
/// If mode is Check::List, dtype <: ctype OR ctype <: dtype (ie. the most general unifier between the two types)
/// This behavioral difference is limited to object (class) subtyping
pub fn check_type(session: &Session, odtype: Option<&Type>, octype: Option<&Type>, mode: Check, update: bool) -> Result<Type, Error> {
    if odtype.is_none() {
        match octype {
            Some(ctype) => Ok(resolve_type(session, ctype.clone(), false)?),
            None => Ok(session.new_typevar()),
        }
    } else if octype.is_none() {
        Ok(odtype.unwrap().clone())
    } else {
        let dtype = resolve_type(session, odtype.unwrap().clone(), false)?;
        let ctype = resolve_type(session, octype.unwrap().clone(), false)?;

        debug!("CHECK TYPE {:?} {:?} {:?} {:?}", dtype, ctype, mode, update);

        // If the update flag is false, then universal variables can unify with any type.  This
        // assumes that update is only false when not type checking, such as looking for overloaded
        // function variants, or making sure overloaded functions definitions don't overlap
        if !update {
            match (&dtype, &ctype) {
                (Type::Universal(_, id), ntype) |
                (ntype, Type::Universal(_, id)) if check_trait_constraints(session, *id, ntype) => {
                    return Ok(ntype.clone());
                },
                _ => { },
            }
        }

        match (&dtype, &ctype) {
            (Type::Variable(aid), Type::Variable(bid)) => {
                if update {
                    let mut constraints = session.get_constraints(*aid);
                    constraints.extend(session.get_constraints(*bid));
                    constraints.sort();
                    constraints.dedup();
                    session.set_constraints(*aid, constraints.clone());
                    session.set_constraints(*bid, constraints);
                    session.set_type(*bid, dtype.clone());
                }
                Ok(resolve_type(session, dtype, false)?)
            },

            (Type::Variable(id), ntype) |
            (ntype, Type::Variable(id)) if check_trait_constraints(session, *id, ntype) => {
                if update {
                    session.update_type(*id, ntype)?;
                }
                Ok(resolve_type(session, ntype.clone(), false)?)
            }

            (Type::Universal(_, aid), Type::Universal(_, bid)) if aid == bid => {
                Ok(ctype.clone())
            },

            (Type::Function(aargs, aret, aabi), Type::Function(bargs, bret, babi)) => {
                let oabi = aabi.compare(*babi);
                if oabi.is_some() {
                    let argtypes = check_type(session, Some(aargs), Some(bargs), mode, update)?;
                    Ok(Type::Function(r(argtypes), r(check_type(session, Some(aret), Some(bret), mode, update)?), oabi.unwrap()))
                } else {
                    Err(Error::new(format!("TypeError: type mismatch, expected {} but found {}", dtype, ctype)))
                }
            },

            (Type::Tuple(atypes), Type::Tuple(btypes)) => {
                let mut types = vec!();
                if atypes.len() == btypes.len() {
                    for (atype, btype) in atypes.iter().zip(btypes.iter()) {
                        types.push(check_type(session, Some(atype), Some(btype), mode, update)?);
                    }
                    Ok(Type::Tuple(types))
                } else {
                    Err(Error::new(format!("TypeError: type mismatch, expected {} but found {}", dtype, ctype)))
                }
            },

            (Type::Record(atypes), Type::Record(btypes)) => {
                let mut types = vec!();
                if atypes.len() == btypes.len() {
                    for ((aname, atype), (bname, btype)) in atypes.iter().zip(btypes.iter()) {
                        if aname != bname {
                            return Err(Error::new(format!("TypeError: type mismatch, expected {} but found {}", dtype, ctype)));
                        }
                        types.push((aname.clone(), check_type(session, Some(atype), Some(btype), mode, update)?));
                    }
                    Ok(Type::Record(types))
                } else {
                    Err(Error::new(format!("TypeError: type mismatch, expected {} but found {}", dtype, ctype)))
                }
            },

            (Type::Object(aname, aid, atypes), Type::Object(bname, bid, btypes)) => {
                match is_subclass_of(session, (bname, *bid, btypes), (aname, *aid, atypes), mode, update) {
                    ok @ Ok(_) => ok,
                    err @ Err(_) => match mode {
                        Check::List => is_subclass_of(session, (aname, *aid, atypes), (bname, *bid, btypes), mode, update),
                        _ => err,
                    }
                }
            },

            (Type::Ref(atype), Type::Ref(btype)) => {
                let ttype = check_type(session, Some(atype), Some(btype), mode, update)?;
                Ok(Type::Ref(r(ttype)))
            },

            _ => {
                Err(Error::new(format!("TypeError: type mismatch, expected {} but found {}", Type::display_full(session, &dtype), Type::display_full(session, &ctype))))
            }
        }
    }
}


fn is_subclass_of(session: &Session, sdef: (&String, UniqueID, &Vec<Type>), pdef: (&String, UniqueID, &Vec<Type>), mode: Check, update: bool) -> Result<Type, Error> {
    //debug!("IS SUBCLASS: {:?} of {:?}", sdef, pdef);
    let mut names = Type::map_new();
    let mut sdef = (sdef.0.clone(), sdef.1, sdef.2.clone());
    //let mut deftype = session.get_type(sdef.1)?.get_params()?;

    loop {
        let mut class = session.get_type(sdef.1).unwrap().clone();
        if update {
            class = Type::map_typevars(session, &mut names, class);
        }
        sdef.2 = check_type_params(session, &class.get_params()?, &sdef.2, mode, update)?;

        // Compare the IDs of the type (since it could be an alias in the case of "Self")
        if pdef.1 == sdef.1 {
            let ptypes = if pdef.2.len() > 0 || sdef.2.len() > 0 {
                check_type_params(session, &sdef.2, pdef.2, mode, update)?
            } else {
                vec!()
            };
            let rtype = Type::Object(sdef.0, sdef.1, ptypes);
            //debug!("DONE SUBCLASS: {:?}", rtype);
            return Ok(rtype);
        }

        match session.get_def(sdef.1)? {
            Def::Class(classdef) if classdef.parenttype.is_some() => {
                let parent = if update {
                    Type::map_typevars(session, &mut names, classdef.parenttype.clone().unwrap())
                } else {
                    classdef.parenttype.clone().unwrap()
                };
                match resolve_type(session, parent, false)? {
                    Type::Object(name, id, params) => sdef = (name, id, params),
                    ttype @ _ => return Err(Error::new(format!("TypeError: expected Object but found {}", ttype))),
                }
            },
            _ => {
                return Err(Error::new(format!("TypeError: type mismatch, expected {} but found {}", Type::Object(pdef.0.clone(), pdef.1, pdef.2.clone()), Type::Object(sdef.0.clone(), sdef.1, sdef.2))));
            }
        }
    }
}

fn check_type_params(session: &Session, dtypes: &Vec<Type>, ctypes: &Vec<Type>, mode: Check, update: bool) -> Result<Vec<Type>, Error> {
    if dtypes.len() != ctypes.len() {
        Err(Error::new(format!("TypeError: number of type parameters don't match: expected {} but found {}", Type::display_vec(dtypes), Type::display_vec(ctypes))))

    } else {
        let mut ptypes = vec!();
        for (dtype, ctype) in dtypes.iter().zip(ctypes.iter()) {
            ptypes.push(check_type(session, Some(dtype), Some(ctype), mode, update)?);
        }
        Ok(ptypes)
    }
}

fn check_trait_constraints(session: &Session, var_id: NodeID, ctype: &Type) -> bool {
    for trait_id in session.get_constraints(var_id) {
        if !check_trait_is_implemented(session, trait_id, ctype) {
            return false;
        }
    }
    return true;
}

fn check_trait_is_implemented(session: &Session, trait_id: NodeID, ctype: &Type) -> bool {
    // If the class matching doesn't work, then try checking for trait matching, but this is a bad hack.  We should use universals with constraints instead of Type::Object
    match session.get_def(trait_id).and_then(|def| def.as_trait_def()) {
        Ok(traitdef) => {
            for traitimpl in traitdef.impls.borrow().iter() {
                if let Ok(_) = check_type(session, Some(&traitimpl.impltype), Some(ctype), Check::Def, false) {
                    return true;
                }
            }
        },
        _ => panic!("InternalError: trait is not defined, but should be by this point {:?}", trait_id),
    }
    return false;
}

