

use defs::Def;
use session::{ Session, Error };
use scope::{ Scope, ScopeRef };
use ast::{ NodeID, AssignType, ClassSpec, Literal, Pattern, AST };
use types::{ Type, Check, ABI, expect_type, resolve_type, check_type_params };
use misc::{ r };


#[derive(Clone, Debug, PartialEq)]
pub struct TypeChecker<'sess> {
    pub session: &'sess Session,
    //pub context: RefCell<Vec<CodeContext>>,
}


impl<'sess> TypeChecker<'sess> {
    pub fn check(session: &'sess Session, scope: ScopeRef, code: &Vec<AST>) -> Type {
        let typechecker = TypeChecker {
            session: session,
            //context: RefCell::new(vec!()),
        };

        let ttype = typechecker.check_vec(scope, code);
        if session.errors.get() > 0 {
            panic!("Exiting due to previous errors");
        }
        ttype
    }

    pub fn check_vec(&self, scope: ScopeRef, code: &Vec<AST>) -> Type {
        let mut last: Type = scope.make_obj(self.session, String::from("()"), vec!()).unwrap();
        for node in code {
            last = self.check_node(scope.clone(), node, None);
        }
        last
    }

    pub fn check_node(&self, scope: ScopeRef, node: &AST, expected: Option<Type>) -> Type {
        match self.check_node_or_error(scope.clone(), node, expected) {
            Ok(ttype) => ttype,
            Err(err) => {
                self.session.print_error(err.add_pos(&node.get_pos()));
                //Type::Object(String::from("()"), vec!())
                scope.new_typevar(self.session, false)
            }
        }
    }

    pub fn check_node_or_error(&self, scope: ScopeRef, node: &AST, expected: Option<Type>) -> Result<Type, Error> {
        let rtype = match *node {
            AST::Literal(ref _id, ref literal) => {
                match literal {
                    Literal::Unit => scope.make_obj(self.session, String::from("()"), vec!())?,
                    Literal::Boolean(_) => scope.make_obj(self.session, String::from("Bool"), vec!())?,
                    Literal::Integer(_) => scope.make_obj(self.session, String::from("Int"), vec!())?,
                    Literal::Real(_) => scope.make_obj(self.session, String::from("Real"), vec!())?,
                    Literal::String(_) => scope.make_obj(self.session, String::from("String"), vec!())?,
                }
            },

            AST::Function(ref id, _, _, ref ident, ref args, _, ref body, ref abi) => {
                let fscope = self.session.map.get(id);
                let dftype = self.session.get_type(*id).unwrap();
                let rtype = dftype.get_rettype()?;

                let mut argtypes = vec!();
                for arg in args.iter() {
                    let ttype = self.session.get_type(arg.id);
                    let vtype = arg.default.clone().map(|ref vexpr| self.check_node(scope.clone(), vexpr, ttype.clone()));
                    let mut atype = expect_type(self.session, fscope.clone(), ttype, vtype, Check::Def)?;
                    if &arg.ident.name[..] == "self" {
                        let stype = fscope.find_type(self.session, &String::from("Self")).unwrap();
                        atype = expect_type(self.session, fscope.clone(), Some(atype), Some(stype), Check::Def)?;
                    }
                    self.session.update_type(fscope.clone(), arg.id, atype.clone())?;
                    argtypes.push(atype);
                }

                let rettype = expect_type(self.session, fscope.clone(), Some(rtype.clone()), Some(self.check_node(fscope.clone(), body, Some(rtype.clone()))), Check::Def)?;

                // Resolve type variables that can be
                for i in 0 .. argtypes.len() {
                    argtypes[i] = resolve_type(self.session, argtypes[i].clone(), false)?;
                    self.session.update_type(fscope.clone(), args[i].id, argtypes[i].clone())?;
                }

                let tupleargs = Type::Tuple(argtypes);
                let nftype = Type::Function(r(tupleargs.clone()), r(rettype), *abi);

                if let Ok(Def::Overload(ol)) = self.session.get_def_from_ref(*id) {
                    let (mut found, _) = ol.find_local_variants(self.session, scope.clone(), tupleargs.clone());
                    found = found.into_iter().filter(|(fid, _)| *id != *fid).collect();
                    if found.len() > 0 {
                        return Err(Error::new(format!("OverloadError: things {:?}\nvariants found [{}]", ident, found.iter().map(|(_, t)| format!("{}", t)).collect::<Vec<String>>().join(", "))));
                    }
                }

                debug!("FINISHED FUNCTION: {:?} -> {:?}", self.session.get_type(*id), nftype);
                self.session.update_type(scope.clone(), *id, nftype.clone())?;
                nftype
            },

            AST::Invoke(ref id, _, ref fexpr, ref args) => {
                let mut atypes = vec!();
                for ref mut value in args {
                    atypes.push(self.check_node(scope.clone(), value, None));
                }
                let atypes = Type::Tuple(atypes);

                let tscope = Scope::new_ref(Some(scope.clone()));
                let dtype = self.session_find_variant(tscope.clone(), *id, fexpr.as_ref(), &atypes)?;
                debug!("INVOKE TYPE: {:?} has type {:?}", id, dtype);
                let etype = match dtype {
                    Type::Variable(_, _, _) => dtype.clone(),
                    _ => tscope.map_all_typevars(self.session, dtype.clone()),
                };

                let ftype = match etype {
                    Type::Function(_, _, ref abi) => {
                        //let ftype = expect_type(self.session, tscope.clone(), Some(etype.clone()), Some(Type::Function(r(atypes), r(expected.unwrap_or_else(|| tscope.new_typevar(self.session, false))), abi)), Check::Update)?;
                        let ftype = expect_type(self.session, tscope.clone(), Some(etype.clone()), Some(Type::Function(r(atypes), r(etype.get_rettype()?.clone()), *abi)), Check::Def)?;
                        // TODO should this actually be another expect, so type resolutions that occur in later args affect earlier args?  Might not be needed unless you add typevar constraints
                        let ftype = resolve_type(self.session, ftype, false)?;        // NOTE This ensures the early arguments are resolved despite typevars not being assigned until later in the signature

                        ftype
                    },
                    Type::Variable(_, ref vid, _) => {
                        let ftype = Type::Function(r(atypes), r(expected.unwrap_or_else(|| tscope.new_typevar(self.session, false))), ABI::Unknown);
                        self.session.update_type(tscope, *vid, ftype.clone())?;
                        ftype
                    },
                    _ => return Err(Error::new(format!("NotAFunction: {:?}", fexpr))),
                };

                self.session.update_type(scope.clone(), *id, ftype.clone())?;
                ftype.get_rettype()?.clone()
            },

            AST::SideEffect(_, _, _, ref args) => {
                let mut ltype = None;
                for ref expr in args {
                    ltype = Some(expect_type(self.session, scope.clone(), ltype.clone(), Some(self.check_node(scope.clone(), expr, ltype.clone())), Check::List)?);
                }
                ltype.unwrap()
            },

            AST::Definition(ref id, _, _, _, _, ref body) => {
                let dtype = self.session.get_type(*id);
                let btype = expect_type(self.session, scope.clone(), dtype.clone(), Some(self.check_node(scope.clone(), body, dtype)), Check::Def)?;
                self.session.update_type(scope.clone(), *id, btype.clone())?;
                btype
            },

            AST::Declare(ref id, _, _, _, _) => {
                self.session.get_type(*id).unwrap()
            },

            AST::Identifier(ref id, _, ref ident) => {
                if let Ok(ttype) = self.session.get_type_from_ref(*id) {
                    ttype
                } else {
                    match scope.get_var_def(&ident.name) {
                        Some(defid) => match self.session.get_type(defid) {
                            Some(ttype) => ttype,
                            None => return Err(Error::new(format!("TypeError: the reference {:?} has no type or has an ambiguous type", ident.name))),
                        }
                        None => panic!("InternalError: ident {:?} is undefined, but should have been caught in the name binding phase", ident.name),
                    }
                }
            },

            AST::Block(_, _, ref body) => self.check_vec(scope, body),

            AST::If(_, _, ref cond, ref texpr, ref fexpr) => {
                // TODO should this require the cond type to be Bool?
                self.check_node(scope.clone(), cond, None);
                let ttype = self.check_node(scope.clone(), texpr, None);
                let ftype = self.check_node(scope.clone(), fexpr, Some(ttype.clone()));
                expect_type(self.session, scope, Some(ttype), Some(ftype), Check::List)?
            },

            AST::Try(_, _, ref cond, ref cases) |
            AST::Match(_, _, ref cond, ref cases) => {
                let mut ctype = match node {
                    AST::Match(_, _, _, _) => self.check_node(scope.clone(), cond, None),
                    AST::Try(_, _, _, _) => {
                        self.check_node(scope.clone(), cond, None);
                        // TODO this is problematic.  If the value that's raised is not used, this var will be unresolved.  We aren't really checking the raised type anywhere though
                        scope.new_typevar(self.session, false)
                    },
                    _ => panic!(""),
                };
                let mut rtype = None;
                for ref case in cases {
                    let lscope = self.session.map.get(&case.id);
                    ctype = expect_type(self.session, lscope.clone(), Some(ctype.clone()), Some(self.check_pattern(lscope.clone(), &case.pat, Some(ctype.clone()))?), Check::List)?;
                    rtype = Some(expect_type(self.session, lscope.clone(), rtype.clone(), Some(self.check_node(lscope.clone(), &case.body, rtype.clone())), Check::List)?);
                }

                rtype.unwrap()
            },

            AST::Raise(_, _, ref expr) => {
                // TODO should you check for a special error/exception type?
                self.check_node(scope.clone(), expr, None);
                scope.make_obj(self.session, String::from("()"), vec!())?
            },

            AST::While(_, _, ref cond, ref body) => {
                // TODO should this require the cond type to be Bool?
                self.check_node(scope.clone(), cond, None);
                self.check_node(scope.clone(), body, None);
                scope.make_obj(self.session, String::from("()"), vec!())?
            },

            AST::Nil(ref id) => {
                let ttype = expected.unwrap_or_else(|| scope.new_typevar(self.session, false));
                self.session.set_type(*id, ttype.clone());
                ttype
            },

            AST::Ref(ref id, _, ref expr) => {
                let ttype = self.check_node(scope.clone(), expr, None);
                let rtype = Type::Ref(r(ttype));
                let rtype = expect_type(self.session, scope.clone(), expected.clone(), Some(rtype), Check::Def)?;
                self.session.set_type(*id, rtype.clone());
                rtype
            },

            AST::Deref(ref id, _, ref expr) => {
                match self.check_node(scope.clone(), expr, None) {
                    Type::Ref(etype) => {
                        let etype = expect_type(self.session, scope.clone(), expected.clone(), Some(*etype), Check::Def)?;
                        self.session.set_type(*id, etype.clone());
                        etype
                    },
                    Type::Variable(_, vid, _) => {
                        let etype = expect_type(self.session, scope.clone(), expected.clone(), None, Check::Def)?;
                        self.session.update_type(scope.clone(), vid, Type::Ref(r(etype.clone())))?;
                        self.session.set_type(*id, etype.clone());
                        etype
                    },
                    ttype @ _ => return Err(Error::new(format!("TypeError: attempting to dereference a non-reference: {:?}", ttype))),
                }
            },

            AST::Tuple(ref id, _, ref items) => {
                // TODO would this not be a bug if the expected type was for some reason a variable?
                //let etypes = match expected {
                //    Some(ref e) => e.get_types()?.iter().map(|i| Some(i.clone())).collect(),
                //    None => vec![None; items.len()]
                //};
                let etypes = vec![None; items.len()];

                if etypes.len() != items.len() {
                    return Err(Error::new(format!("TypeError: number of tuple items don't match: expected {:?} with {} items but found {} items", expected, etypes.len(), items.len())));
                }

                let mut types = vec!();
                for (ref expr, etype) in items.iter().zip(etypes.iter()) {
                    types.push(expect_type(self.session, scope.clone(), etype.clone(), Some(self.check_node(scope.clone(), expr, etype.clone())), Check::List)?);
                }
                self.session.set_type(*id, Type::Tuple(types.clone()));
                Type::Tuple(types)
            },

            AST::Record(ref id, _, ref items) => {
                let mut types = vec!();
                for (ref ident, ref expr) in items {
                    types.push((ident.name.clone(), self.check_node(scope.clone(), expr, None)));
                }
                self.session.set_type(*id, Type::Record(types.clone()));
                Type::Record(types)
            },

            AST::RecordUpdate(ref id, _, ref record, ref items) => {
                let rtype = self.check_node(scope.clone(), record, None);

                for (ref ident, ref expr) in items {
                    let ftype = rtype.get_record_field(ident.name.as_str())?;
                    let itype = self.check_node(scope.clone(), expr, None);
                    expect_type(self.session, scope.clone(), Some(ftype.clone()), Some(itype), Check::Def)?;
                }
                self.session.set_type(*id, rtype.clone());
                rtype
            },

            AST::TypeEnum(_, _, _, _) |
            AST::TypeAlias(_, _, _, _) => {
                scope.make_obj(self.session, String::from("()"), vec!())?
            },

            AST::PtrCast(ref id, _, ref code) => {
                let ttype = self.session.get_type(*id);
                let ctype = self.check_node(scope.clone(), code, ttype.clone());
                debug!("PTRCAST: {:?} <- {:?}", ttype, ctype);
                expect_type(self.session, scope, ttype.clone(), Some(ctype), Check::List)?;
                ttype.unwrap()
            },

            AST::New(ref id, _, _) => {
                let classtype = self.session.get_type(*id).unwrap();
                let dtype = self.session.get_type_from_ref(*id)?;
                let tscope = Scope::new_ref(Some(scope.clone()));
                let mtype = tscope.map_all_typevars(self.session, dtype.clone());
                check_type_params(self.session, scope.clone(), &mtype.get_params()?, &classtype.get_params()?, Check::Def, true)?;
                classtype
            },

            AST::Class(ref id, _, _, _, ref body) => {
                let tscope = self.session.map.get(id);
                self.check_vec(tscope.clone(), body);
                scope.make_obj(self.session, String::from("()"), vec!())?
            },

            AST::Resolver(_, _, _, _, _) |
            AST::Accessor(_, _, _, _, _) => {
                let (refid, defid) = self.get_access_ids(scope.clone(), node)?.unwrap();
                self.session.set_ref(refid, defid);
                self.get_type_or_new_typevar(scope, defid, expected)
            },

            AST::Assignment(_, _, ref left, ref right, ref ty) => {
                // TODO this is duplicated in check_types_node(left)... can we avoid that
                match self.get_access_ids(scope.clone(), left)? {
                    Some((_refid, defid)) => {
                        if *ty == AssignType::Update && !self.session.get_def(defid).map(|d| d.is_mutable()).unwrap_or(false) {
                            return Err(Error::new(format!("MutableError: attempting to assign to an immutable variable")));
                        }
                    },
                    None => { }
                }

                let ltype = self.check_node(scope.clone(), left, None);
                let rtype = self.check_node(scope.clone(), right, Some(ltype.clone()));
                expect_type(self.session, scope, Some(ltype), Some(rtype), Check::Def)?
            },

            AST::Import(_, _, _, ref decls) => {
                self.check_vec(scope.clone(), decls);
                scope.make_obj(self.session, String::from("()"), vec!())?
            },

            AST::GetValue(_) |
            AST::List(_, _, _) |
            AST::For(_, _, _, _, _) |
            AST::Index(_, _, _, _) => { panic!("InternalError: ast element shouldn't appear at this late phase: {:?}", node) }
        };

        debug!("CHECK: {:?} {:?}", rtype, node);
        Ok(rtype)
    }

    pub fn check_pattern(&self, scope: ScopeRef, pat: &Pattern, expected: Option<Type>) -> Result<Type, Error> {
        match pat {
            Pattern::Wild => Ok(expected.unwrap_or_else(|| scope.new_typevar(self.session, false))),
            Pattern::Literal(ref id, ref node) => {
                let ltype = self.check_node_or_error(scope.clone(), node, expected)?;
                self.link_comparison_func(scope.clone(), *id, &ltype)?;
                Ok(ltype)
            },
            Pattern::Binding(ref id, _) => {
                let btype = expected.unwrap_or_else(|| scope.new_typevar(self.session, false));
                self.session.update_type(scope.clone(), *id, btype.clone())?;
                Ok(btype)
            },
            Pattern::Annotation(ref id, _, ref pat) => {
                let ttype = self.session.get_type(*id);
                let etype = self.check_pattern(scope.clone(), pat, ttype.clone())?;
                expect_type(self.session, scope, ttype, Some(etype), Check::Def)
            },
            Pattern::Resolve(ref id, ref left, ref field, ref oid) => {
                let ltype = self.session.get_type_from_ref(*oid).unwrap();

                let vars = self.session.get_def(ltype.get_id()?)?.get_vars()?;
                let defid = vars.get_var_def(&field.name).ok_or(Error::new(format!("VarError: definition not set for {:?}", field.name)))?;
                self.session.set_ref(*id, defid);
                let ttype = self.session.get_type(defid).ok_or(Error::new(format!("TypeError: no type set for id {:?}", defid)))?;
                expect_type(self.session, scope, Some(ttype), expected, Check::Def)
            },
            Pattern::EnumArgs(id, left, args) => {
                let etype = self.check_pattern(scope.clone(), left, None)?;
                let variant_id = self.session.get_ref(left.get_id())?;
                self.session.set_ref(*id, variant_id);
                let enumdef = self.session.get_def(self.session.get_ref(variant_id)?)?.as_enum()?;
                match enumdef.get_variant_type_by_id(variant_id) {
                    None => return Err(Error::new(format!("TypeError: enum variant doesn't expect any arguments, but found {:?}", args))),
                    Some(ttype) => {
                        let types = ttype.as_vec();
                        if types.len() != args.len() {
                            return Err(Error::new(format!("TypeError: number of enum arguments expected doesn't match. Expected {:?}, found {:?}", types.len(), args.len())));
                        }

                        let mut argtypes = vec!();
                        for (arg, ttype) in args.iter().zip(types.iter()) {
                            argtypes.push(self.check_pattern(scope.clone(), &arg, Some(ttype.clone()))?);
                        }

                        expect_type(self.session, scope.clone(), Some(ttype), Some(Type::Tuple(argtypes)), Check::Def)?;
                        Ok(enumdef.deftype.clone())
                    },
                }
            },
            _ => panic!("Not Implemented: {:?}", pat),
        }
    }

    #[must_use]
    pub fn link_comparison_func(&self, scope: ScopeRef, refid: NodeID, ctype: &Type) -> Result<(), Error> {
        match scope.get_var_def(&String::from("==")) {
            None => return Err(Error::new(format!("NameError: no \"==\" function defined for type {:?}", ctype))),
            Some(defid) => {
                let (fid, ftype) = self.session_find_variant_id(scope.clone(), defid, &Type::Tuple(vec!(ctype.clone(), ctype.clone())))?;
                self.session.set_ref(refid, fid);
                self.session.set_type(refid, ftype);
            }
        }
        Ok(())
    }


    pub fn get_access_ids(&self, scope: ScopeRef, node: &AST) -> Result<Option<(NodeID, NodeID)>, Error> {
        match node {
            AST::Identifier(ref id, _, _) => {
                Ok(Some((*id, self.session.get_ref(*id)?)))
            },
            AST::Resolver(ref id, _, _, ref field, ref oid) => {
                let ltype = self.session.get_type_from_ref(*oid).unwrap();

                let vars = self.session.get_def(ltype.get_id()?)?.get_vars()?;
                Ok(Some((*id, vars.get_var_def(&field.name).ok_or(Error::new(format!("VarError: definition not set for {:?}", field.name)))?)))
            },
            AST::Accessor(ref id, _, ref left, ref field, ref oid) => {
                let ltype = resolve_type(self.session, self.check_node(scope.clone(), left, None), false)?;
                self.session.set_type(*oid, ltype.clone());

                match ltype {
                    Type::Object(_, _, _) => {
                        let vars = self.session.get_def(ltype.get_id()?)?.get_vars()?;
                        Ok(Some((*id, vars.get_var_def(&field.name).ok_or(Error::new(format!("VarError: definition not set for {:?}", field.name)))?)))
                    },
                    Type::Record(ref items) => {
                        let defid = NodeID::generate();
                        let index = items.iter().position(|(name, _)| *name == field.name).unwrap();
                        self.session.set_type(defid, items[index].1.clone());
                        Ok(Some((*id, defid)))
                    },
                    Type::Tuple(ref items) => {
                        let defid = NodeID::generate();
                        let index = field.name.parse::<usize>().unwrap();
                        self.session.set_type(defid, items[index].clone());
                        Ok(Some((*id, defid)))
                    },
                    _ => Err(Error::new(format!("TypeError: attempting to access within a non-accessible value: {:?}", ltype)))
                }
            },
            _ => { Ok(None) },
        }
    }

    pub fn session_find_variant(&self, scope: ScopeRef, invid: NodeID, fexpr: &AST, argtypes: &Type) -> Result<Type, Error> {
        let (refid, defid) = match self.get_access_ids(scope.clone(), fexpr)? {
            Some(ids) => ids,
            None => { return self.check_node_or_error(scope.clone(), fexpr, None); },
        };


        let (fid, ftype) = self.session_find_variant_id(scope.clone(), defid, argtypes)?;
        self.session.set_ref(refid, fid);
        self.session.set_ref(invid, fid);

        debug!("CHECK VARIANT: {:?} {:?}", ftype, fexpr);
        Ok(ftype)
    }

    pub fn session_find_variant_id(&self, scope: ScopeRef, defid: NodeID, argtypes: &Type) -> Result<(NodeID, Type), Error> {
        match self.session.get_def(defid) {
            Ok(Def::Overload(ref ol)) => ol.find_variant(self.session, scope.clone(), argtypes.clone()),
            _ => Ok((defid, self.get_type_or_new_typevar(scope, defid, None)))
        }
    }

    pub fn get_type_or_new_typevar(&self, scope: ScopeRef, defid: NodeID, expected: Option<Type>) -> Type {
        match self.session.get_type(defid) {
            Some(ttype) => ttype,
            None => {
                let ttype = expected.unwrap_or_else(|| scope.new_typevar(self.session, false));
                self.session.set_type(defid, ttype.clone());
                ttype
            }
        }
    }
}


