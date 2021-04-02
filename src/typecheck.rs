

use defs::Def;
use session::{ Session, Error };
use scope::{ Scope, ScopeRef };
use hir::{ NodeID, AssignType, Literal, Pattern, PatKind, Expr, ExprKind };
use types::{ Type, Check, ABI, expect_type, resolve_type, check_type_params };
use misc::{ r };
use visitor::{ self, Visitor, ScopeStack };


#[derive(Clone, Debug, PartialEq)]
pub struct TypeChecker<'sess> {
    pub session: &'sess Session,
    pub stack: ScopeStack,
    //pub context: RefCell<Vec<CodeContext>>,
}


impl<'sess> TypeChecker<'sess> {
    pub fn check(session: &'sess Session, scope: ScopeRef, code: &Vec<Expr>) -> Type {
        let typechecker = TypeChecker {
            session: session,
            stack: ScopeStack::new(),
            //context: RefCell::new(vec!()),
        };

        typechecker.stack.push_scope(scope);
        let ttype = typechecker.check_vec(code);
        if session.errors.get() > 0 {
            panic!("Exiting due to previous errors");
        }
        ttype
    }

    fn with_scope<F, R>(&self, scope: ScopeRef, f: F) -> Result<R, Error> where F: FnOnce(&Self) -> Result<R, Error> {
        self.stack.push_scope(scope);
        let ret = f(self);
        self.stack.pop_scope();
        ret
    }

    pub fn check_vec(&self, code: &Vec<Expr>) -> Type {
        let scope = self.stack.get_scope();
        let mut last: Type = scope.make_obj(self.session, String::from("()"), vec!()).unwrap();
        for node in code {
            last = self.check_node(node, None);
        }
        last
    }

    pub fn check_node(&self, node: &Expr, expected: Option<Type>) -> Type {
        let scope = self.stack.get_scope();
        if let Some(ttype) = expected {
            if let Err(err) = self.session.update_type(scope.clone(), node.id, ttype) {
                self.session.print_error(err.add_pos(&node.get_pos()));
            }
        }

        let ttype = match self.check_node_or_error(node) {
            Ok(ttype) => ttype,
            Err(err) => {
                self.session.print_error(err.add_pos(&node.get_pos()));
                //Type::Object(String::from("()"), vec!())
                scope.new_typevar(self.session, false)
            }
        };

        /*
        match self.session.update_type(scope.clone(), node.id, ttype.clone()) {
            Ok(_) => ttype,
            Err(err) => {
                self.session.print_error(err.add_pos(&node.get_pos()));
                ttype
            },
        }
        */
        ttype
    }

    pub fn check_node_or_error(&self, node: &Expr) -> Result<Type, Error> {
        let rtype = match &node.kind {
            ExprKind::Literal(literal) => {
                self.check_literal(literal)?
            },

            ExprKind::Function(_, ident, args, _, body, abi) => {
                let scope = self.stack.get_scope();
                let defid = self.session.get_ref(node.id)?;
                let fscope = self.session.map.get(&defid);
                let dftype = self.session.get_type(defid).unwrap();
                let rtype = dftype.get_rettype()?;

                let mut argtypes = vec!();
                for arg in args.iter() {
                    let arg_defid = self.session.get_ref(arg.id)?;
                    let ttype = self.session.get_type(arg_defid);
                    let vtype = arg.default.clone().map(|ref vexpr| self.check_node(vexpr, ttype.clone()));
                    let mut atype = expect_type(self.session, fscope.clone(), ttype, vtype, Check::Def)?;
                    if &arg.ident.name[..] == "self" {
                        let stype = fscope.find_type(self.session, &String::from("Self")).unwrap();
                        atype = expect_type(self.session, fscope.clone(), Some(atype), Some(stype), Check::Def)?;
                    }
                    self.session.update_type(fscope.clone(), arg_defid, atype.clone())?;
                    argtypes.push(atype);
                }

                let rettype = self.with_scope(fscope.clone(), |visitor| {
                    expect_type(visitor.session, fscope.clone(), Some(rtype.clone()), Some(self.check_node(body, Some(rtype.clone()))), Check::Def)
                })?;

                // Resolve type variables that can be
                for i in 0 .. argtypes.len() {
                    argtypes[i] = resolve_type(self.session, argtypes[i].clone(), false)?;
                    self.session.update_type(fscope.clone(), args[i].id, argtypes[i].clone())?;
                }

                let tupleargs = Type::Tuple(argtypes);
                let nftype = Type::Function(r(tupleargs.clone()), r(rettype), *abi);

                if let Ok(Def::Overload(ol)) = self.session.get_def_from_ref(defid) {
                    let (mut found, _) = ol.find_local_variants(self.session, scope.clone(), tupleargs.clone());
                    found = found.into_iter().filter(|(fid, _)| defid != *fid).collect();
                    if found.len() > 0 {
                        return Err(Error::new(format!("OverloadError: things {:?}\nvariants found [{}]", ident, found.iter().map(|(_, t)| format!("{}", t)).collect::<Vec<String>>().join(", "))));
                    }
                }

                debug!("FINISHED FUNCTION: {:?} -> {:?}", self.session.get_type(defid), nftype);
                self.session.update_type(scope.clone(), defid, nftype.clone())?;
                self.session.update_type(scope.clone(), node.id, nftype.clone())?;
                nftype
            },

            ExprKind::Invoke(fexpr, args, fid) => {
                let scope = self.stack.get_scope();
                let mut atypes = vec!();
                for ref mut value in args {
                    atypes.push(self.check_node(value, None));
                }
                let atypes = Type::Tuple(atypes);

                let tscope = Scope::new_ref(Some(scope.clone()));
                let dtype = self.session_find_variant(tscope.clone(), node.id, fexpr.as_ref(), &atypes)?;
                debug!("INVOKE TYPE: {:?} has type {:?}", node.id, dtype);
                let etype = match dtype {
                    Type::Variable(_, _, _) => dtype.clone(),
                    _ => tscope.map_all_typevars(self.session, dtype.clone()),
                };

                let ftype = match etype {
                    Type::Function(args, rettype, abi) => {
                        //let ftype = expect_type(self.session, tscope.clone(), Some(etype.clone()), Some(Type::Function(r(atypes), r(self.session.get_type(node.id).unwrap_or_else(|| tscope.new_typevar(self.session, false))), abi)), Check::Update)?;
                        let ftype = expect_type(self.session, tscope.clone(), Some(Type::Function(args, rettype.clone(), abi)), Some(Type::Function(r(atypes), r(*rettype), abi)), Check::Def)?;
                        // TODO should this actually be another expect, so type resolutions that occur in later args affect earlier args?  Might not be needed unless you add typevar constraints
                        let ftype = resolve_type(self.session, ftype, false)?;        // NOTE This ensures the early arguments are resolved despite typevars not being assigned until later in the signature

                        ftype
                    },
                    Type::Variable(_, vid, _) => {
                        let ftype = Type::Function(r(atypes), r(self.session.get_type(node.id).unwrap_or_else(|| tscope.new_typevar(self.session, false))), ABI::Unknown);
                        self.session.update_type(tscope, vid, ftype.clone())?;
                        ftype
                    },
                    _ => return Err(Error::new(format!("NotAFunction: {:?}", fexpr))),
                };

                //self.session.update_type(scope.clone(), node.id, ftype.clone())?;
                let rtype = ftype.get_rettype()?.clone();
                self.session.set_type(*fid, ftype);
                self.session.update_type(scope.clone(), node.id, rtype.clone());
                rtype
            },

            ExprKind::SideEffect(_, args) => {
                let scope = self.stack.get_scope();
                let mut ltype = None;
                for ref expr in args {
                    ltype = Some(expect_type(self.session, scope.clone(), ltype.clone(), Some(self.check_node(expr, ltype.clone())), Check::List)?);
                }
                ltype.unwrap()
            },

            ExprKind::Definition(_, _, _, body) => {
                let scope = self.stack.get_scope();
                let defid = self.session.get_ref(node.id)?;
                let dtype = self.session.get_type(defid);
                let btype = expect_type(self.session, scope.clone(), dtype.clone(), Some(self.check_node(body, dtype)), Check::Def)?;
                self.session.update_type(scope.clone(), defid, btype.clone())?;
                self.session.update_type(scope.clone(), node.id, btype.clone())?;
                btype
            },

            ExprKind::Declare(_, _, _) => {
                let scope = self.stack.get_scope();
                scope.make_obj(self.session, String::from("()"), vec!())?
            },

            ExprKind::Identifier(ident) => {
                let scope = self.stack.get_scope();
                if let Ok(ttype) = self.session.get_type_from_ref(node.id) {
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

            ExprKind::Block(body) => self.check_vec(body),

            ExprKind::If(cond, texpr, fexpr) => {
                let scope = self.stack.get_scope();
                // TODO should this require the cond type to be Bool?
                self.check_node(cond, None);
                let ttype = self.check_node(texpr, None);
                let ftype = self.check_node(fexpr, Some(ttype.clone()));
                expect_type(self.session, scope, Some(ttype), Some(ftype), Check::List)?
            },

            ExprKind::Match(cond, cases) => {
                let scope = self.stack.get_scope();
                let mut ctype = self.check_node(cond, None);

                let mut rtype = None;
                for ref case in cases {
                    let lscope = self.session.map.get(&case.id);
                    self.with_scope(lscope.clone(), |visitor| {
                        ctype = expect_type(self.session, lscope.clone(), Some(ctype.clone()), Some(self.check_pattern_expected(&case.pat, Some(ctype.clone()))?), Check::List)?;
                        rtype = Some(expect_type(self.session, lscope.clone(), rtype.clone(), Some(self.check_node(&case.body, rtype.clone())), Check::List)?);
                        Ok(())
                    })?;
                }

                rtype.unwrap()
            },

            ExprKind::Try(cond, cases) => {
                let scope = self.stack.get_scope();
                let btype = self.check_node(cond, None);
                let mut ctype = scope.make_obj(self.session, String::from("Exception"), vec!())?;

                let mut rtype = None;
                for ref case in cases {
                    let lscope = self.session.map.get(&case.id);
                    self.with_scope(lscope.clone(), |visitor| {
                        ctype = expect_type(self.session, lscope.clone(), Some(ctype.clone()), Some(self.check_pattern_expected(&case.pat, Some(ctype.clone()))?), Check::List)?;
                        rtype = Some(expect_type(self.session, lscope.clone(), rtype.clone(), Some(self.check_node(&case.body, rtype.clone())), Check::List)?);
                        Ok(())
                    })?;
                }

                expect_type(self.session, scope.clone(), Some(btype.clone()), rtype.clone(), Check::List)?;
                rtype.unwrap()
            },

            ExprKind::Raise(expr) => {
                let scope = self.stack.get_scope();
                // TODO should you check for a special error/exception type?
                let extype = scope.make_obj(self.session, String::from("Exception"), vec!())?;
                expect_type(self.session, scope.clone(), Some(extype.clone()), Some(self.check_node(expr, Some(extype))), Check::Def)?;
                scope.make_obj(self.session, String::from("()"), vec!())?
            },

            ExprKind::While(cond, body) => {
                let scope = self.stack.get_scope();
                // TODO should this require the cond type to be Bool?
                self.check_node(cond, None);
                self.check_node(body, None);
                scope.make_obj(self.session, String::from("()"), vec!())?
            },

            ExprKind::Nil => {
                let scope = self.stack.get_scope();
                let ttype = self.session.get_type(node.id).unwrap_or_else(|| scope.new_typevar(self.session, false));
                self.session.set_type(node.id, ttype.clone());
                ttype
            },

            ExprKind::Ref(expr) => {
                let scope = self.stack.get_scope();
                let ttype = self.check_node(expr, None);
                let rtype = Type::Ref(r(ttype));
                let rtype = expect_type(self.session, scope.clone(), self.session.get_type(node.id), Some(rtype), Check::Def)?;
                self.session.set_type(node.id, rtype.clone());
                rtype
            },

            ExprKind::Deref(expr) => {
                let scope = self.stack.get_scope();
                match self.check_node(expr, None) {
                    Type::Ref(etype) => {
                        let etype = expect_type(self.session, scope.clone(), self.session.get_type(node.id), Some(*etype), Check::Def)?;
                        self.session.set_type(node.id, etype.clone());
                        etype
                    },
                    Type::Variable(_, vid, _) => {
                        let etype = expect_type(self.session, scope.clone(), self.session.get_type(node.id), None, Check::Def)?;
                        self.session.update_type(scope.clone(), vid, Type::Ref(r(etype.clone())))?;
                        self.session.set_type(node.id, etype.clone());
                        etype
                    },
                    ttype @ _ => return Err(Error::new(format!("TypeError: attempting to dereference a non-reference: {:?}", ttype))),
                }
            },

            ExprKind::Tuple(items) => {
                let scope = self.stack.get_scope();
                // TODO would this not be a bug if the expected type was for some reason a variable?
                //let etypes = match self.session.get_type(node.id) {
                //    Some(ref e) => e.get_types()?.iter().map(|i| Some(i.clone())).collect(),
                //    None => vec![None; items.len()]
                //};
                let etypes = vec![None; items.len()];

                if etypes.len() != items.len() {
                    return Err(Error::new(format!("TypeError: number of tuple items don't match: expected {:?} with {} items but found {} items", self.session.get_type(node.id), etypes.len(), items.len())));
                }

                let mut types = vec!();
                for (ref expr, etype) in items.iter().zip(etypes.iter()) {
                    types.push(expect_type(self.session, scope.clone(), etype.clone(), Some(self.check_node(expr, etype.clone())), Check::List)?);
                }
                self.session.set_type(node.id, Type::Tuple(types.clone()));
                Type::Tuple(types)
            },

            ExprKind::Record(items) => {
                let mut types = vec!();
                for (ref ident, ref expr) in items {
                    types.push((ident.name.clone(), self.check_node(expr, None)));
                }
                self.session.set_type(node.id, Type::Record(types.clone()));
                Type::Record(types)
            },

            ExprKind::RecordUpdate(record, items) => {
                let scope = self.stack.get_scope();
                let rtype = self.check_node(record, None);

                for (ref ident, ref expr) in items {
                    let ftype = rtype.get_record_field(ident.name.as_str())?;
                    let itype = self.check_node(expr, None);
                    expect_type(self.session, scope.clone(), Some(ftype.clone()), Some(itype), Check::Def)?;
                }
                self.session.set_type(node.id, rtype.clone());
                rtype
            },

            ExprKind::Enum(_, _) |
            ExprKind::TypeAlias(_, _) => {
                let scope = self.stack.get_scope();
                scope.make_obj(self.session, String::from("()"), vec!())?
            },

            ExprKind::PtrCast(_, code) => {
                let scope = self.stack.get_scope();
                let ttype = self.session.get_type(node.id);
                let ctype = self.check_node(code, ttype.clone());
                debug!("PTRCAST: {:?} <- {:?}", ttype, ctype);
                expect_type(self.session, scope, ttype.clone(), Some(ctype), Check::List)?;
                ttype.unwrap()
            },

            ExprKind::New(_) => {
                let scope = self.stack.get_scope();
                let classtype = self.session.get_type(node.id).unwrap();
                let dtype = self.session.get_type_from_ref(node.id)?;
                let tscope = Scope::new_ref(Some(scope.clone()));
                let mtype = tscope.map_all_typevars(self.session, dtype.clone());
                check_type_params(self.session, scope.clone(), &mtype.get_params()?, &classtype.get_params()?, Check::Def, true)?;
                classtype
            },

            ExprKind::Class(_, _, body) => {
                let scope = self.stack.get_scope();
                let defid = self.session.get_ref(node.id)?;
                let tscope = self.session.map.get(&defid);
                self.with_scope(tscope.clone(), |visitor| {
                    Ok(self.check_vec(body))
                })?;
                scope.make_obj(self.session, String::from("()"), vec!())?
            },

            ExprKind::Resolver(_, _, _) |
            ExprKind::Accessor(_, _, _) => {
                let scope = self.stack.get_scope();
                let (refid, defid) = self.get_access_ids(scope.clone(), node)?.unwrap();
                self.session.set_ref(refid, defid);
                self.get_type_or_new_typevar(scope, defid, self.session.get_type(node.id))
            },

            ExprKind::Assignment(left, right, ty) => {
                let scope = self.stack.get_scope();
                // TODO this is duplicated in check_types_node(left)... can we avoid that
                match self.get_access_ids(scope.clone(), left)? {
                    Some((_refid, defid)) => {
                        if *ty == AssignType::Update && !self.session.get_def(defid).map(|d| d.is_mutable()).unwrap_or(false) {
                            return Err(Error::new(format!("MutableError: attempting to assign to an immutable variable")));
                        }
                    },
                    None => { }
                }

                let ltype = self.check_node(left, None);
                let rtype = self.check_node(right, Some(ltype.clone()));
                expect_type(self.session, scope, Some(ltype), Some(rtype), Check::Def)?
            },

            ExprKind::Import(_, decls) => {
                let scope = self.stack.get_scope();
                self.check_vec(decls);
                scope.make_obj(self.session, String::from("()"), vec!())?
            },
        };

        debug!("CHECK: {:?} {:?}", rtype, node);
        Ok(rtype)
    }

    pub fn check_literal(&self, literal: &Literal) -> Result<Type, Error> {
        let scope = self.stack.get_scope();
        match literal {
            Literal::Unit => scope.make_obj(self.session, String::from("()"), vec!()),
            Literal::Boolean(_) => scope.make_obj(self.session, String::from("Bool"), vec!()),
            Literal::Character(_) => scope.make_obj(self.session, String::from("Char"), vec!()),
            Literal::Integer(_) => scope.make_obj(self.session, String::from("Int"), vec!()),
            Literal::Real(_) => scope.make_obj(self.session, String::from("Real"), vec!()),
            Literal::String(_) => scope.make_obj(self.session, String::from("String"), vec!()),
        }
    }

    pub fn check_pattern_expected(&self, pat: &Pattern, expected: Option<Type>) -> Result<Type, Error> {
        let scope = self.stack.get_scope();
        if let Some(ttype) = expected {
            self.session.update_type(scope.clone(), pat.id, ttype)?;
        }
        self.check_pattern(pat)
    }

    pub fn check_pattern(&self, pat: &Pattern) -> Result<Type, Error> {
        match &pat.kind {
            PatKind::Wild => {
                let scope = self.stack.get_scope();
                Ok(self.session.get_type(pat.id).unwrap_or_else(|| scope.new_typevar(self.session, false)))
            },
            PatKind::Literal(lit) => {
                let scope = self.stack.get_scope();
                let ltype = self.check_literal(lit)?;
                self.link_comparison_func(scope.clone(), pat.id, &ltype)?;
                Ok(ltype)
            },
            PatKind::Binding(_) => {
                let scope = self.stack.get_scope();
                let defid = self.session.get_ref(pat.id)?;
                let btype = self.session.get_type(defid).unwrap_or_else(|| scope.new_typevar(self.session, false));
                self.session.update_type(scope.clone(), defid, btype.clone())?;
                self.session.update_type(scope.clone(), pat.id, btype.clone())?;
                Ok(btype)
            },
            PatKind::Annotation(_, subpat) => {
                let scope = self.stack.get_scope();
                let ttype = self.session.get_type(pat.id);
                let etype = self.check_pattern_expected(subpat, ttype.clone())?;
                expect_type(self.session, scope, ttype, Some(etype), Check::Def)
            },
            PatKind::Resolve(_left, field, oid) => {
                let scope = self.stack.get_scope();
                let ltype = self.session.get_type_from_ref(*oid).unwrap();

                let vars = self.session.get_def(ltype.get_id()?)?.get_vars()?;
                let defid = vars.get_var_def(&field.name).ok_or(Error::new(format!("VarError: definition not set for {:?}", field.name)))?;
                self.session.set_ref(pat.id, defid);
                let ttype = self.session.get_type(defid).ok_or(Error::new(format!("TypeError: no type set for id {:?}", defid)))?;
                expect_type(self.session, scope, Some(ttype), self.session.get_type(pat.id), Check::Def)
            },
            PatKind::EnumArgs(left, args) => {
                let scope = self.stack.get_scope();
                self.check_pattern(left)?;
                let variant_id = self.session.get_ref(left.get_id())?;
                self.session.set_ref(pat.id, variant_id);
                let enumdef = self.session.get_def(self.session.get_ref(variant_id)?)?.as_enum()?;
                match enumdef.get_variant_type_by_id(variant_id) {
                    None => return Err(Error::new(format!("TypeError: enum variant doesn't expect any arguments, but found {:?}", args))),
                    Some(ttype) => {
                        // Map the typevars from the enum type params into the enum variant's types, in order to use type hint from 'expected'
                        let tscope = Scope::new_ref(Some(scope.clone()));
                        let mut typevars = Scope::map_new();
                        let vtype = tscope.map_typevars(self.session, &mut typevars, enumdef.deftype.clone());
                        let ctype = expect_type(self.session, tscope.clone(), Some(vtype), self.session.get_type(pat.id), Check::Def)?;
                        let rtype = resolve_type(self.session, tscope.map_typevars(self.session, &mut typevars, ttype), false)?;
                        let types = rtype.as_vec();

                        if types.len() != args.len() {
                            return Err(Error::new(format!("TypeError: number of enum arguments expected doesn't match. Expected {:?}, found {:?}", types.len(), args.len())));
                        }

                        let mut argtypes = vec!();
                        for (arg, ttype) in args.iter().zip(types.iter()) {
                            argtypes.push(self.check_pattern_expected(&arg, Some(ttype.clone()))?);
                        }

                        expect_type(self.session, scope.clone(), Some(rtype), Some(Type::Tuple(argtypes)), Check::Def)?;
                        Ok(enumdef.deftype.clone())
                    },
                }
            },
            // TODO finish implementing these, and in transform as well
            //Pattern::Tuple(id, items) => { },
            //Pattern::Record(id, items) => { },
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


    pub fn get_access_ids(&self, scope: ScopeRef, node: &Expr) -> Result<Option<(NodeID, NodeID)>, Error> {
        match &node.kind {
            ExprKind::Identifier(_) => {
                Ok(Some((node.id, self.session.get_ref(node.id)?)))
            },
            ExprKind::Resolver(_, field, oid) => {
                let ltype = self.session.get_type_from_ref(*oid).unwrap();

                let vars = self.session.get_def(ltype.get_id()?)?.get_vars()?;
                Ok(Some((node.id, vars.get_var_def(&field.name).ok_or(Error::new(format!("VarError: definition not set for {:?}", field.name)))?)))
            },
            ExprKind::Accessor(left, field, oid) => {
                let ltype = resolve_type(self.session, self.check_node(left, None), false)?;
                self.session.set_type(*oid, ltype.clone());

                match ltype {
                    Type::Object(_, _, _) => {
                        let vars = self.session.get_def(ltype.get_id()?)?.get_vars()?;
                        Ok(Some((node.id, vars.get_var_def(&field.name).ok_or(Error::new(format!("VarError: definition not set for {:?}", field.name)))?)))
                    },
                    Type::Record(items) => {
                        let defid = NodeID::generate();
                        let index = items.iter().position(|(name, _)| *name == field.name).unwrap();
                        self.session.set_type(defid, items[index].1.clone());
                        Ok(Some((node.id, defid)))
                    },
                    Type::Tuple(items) => {
                        let defid = NodeID::generate();
                        let index = field.name.parse::<usize>().unwrap();
                        self.session.set_type(defid, items[index].clone());
                        Ok(Some((node.id, defid)))
                    },
                    _ => Err(Error::new(format!("TypeError: attempting to access within a non-accessible value: {:?}", ltype)))
                }
            },
            _ => { Ok(None) },
        }
    }

    pub fn session_find_variant(&self, scope: ScopeRef, invid: NodeID, fexpr: &Expr, argtypes: &Type) -> Result<Type, Error> {
        let (refid, defid) = match self.get_access_ids(scope.clone(), fexpr)? {
            Some(ids) => ids,
            None => { return self.check_node_or_error(fexpr); },
        };


        let (fid, ftype) = self.session_find_variant_id(scope.clone(), defid, argtypes)?;
        self.session.set_ref(refid, fid);
        self.session.set_ref(invid, fid);

        debug!("CHECK VARIANT: {:?} {:?}", ftype, fexpr);
        Ok(ftype)
    }

    pub fn session_find_variant_id(&self, scope: ScopeRef, defid: NodeID, argtypes: &Type) -> Result<(NodeID, Type), Error> {
        match self.session.get_def(defid) {
            Ok(Def::Overload(ol)) => ol.find_variant(self.session, scope.clone(), argtypes.clone()),
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


