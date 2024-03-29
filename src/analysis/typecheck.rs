

use crate::defs::Def;
use crate::session::{ Session, Error };
use crate::scope::ScopeRef;
use crate::types::{ Type, Check, ABI, expect_type, resolve_type };
use crate::misc::{ r, UniqueID };

use crate::analysis::hir::{ Visibility, Mutability, AssignType, Literal, MatchCase, EnumVariant, WhereClause, Function, Pattern, Expr, ExprKind };
use crate::analysis::visitor::{ self, TypeVisitor, Visitor, ScopeStack };


#[derive(Clone, Debug, PartialEq)]
pub struct TypeChecker<'sess> {
    pub session: &'sess Session,
    pub stack: ScopeStack,
}


impl<'sess> TypeChecker<'sess> {
    pub fn check(session: &'sess Session, scope: ScopeRef, code: &Vec<Expr>) -> Type {
        let mut typechecker = TypeChecker {
            session,
            stack: ScopeStack::new(),
        };

        typechecker.stack.push_scope(scope);
        let ttype = typechecker.visit_vec(code).unwrap();
        if session.errors.get() > 0 {
            panic!("Exiting due to previous errors");
        }
        ttype
    }
}

impl<'sess> TypeChecker<'sess> {
    fn visit_node_or_error(&mut self, node: &Expr) -> Type {
        self.visit_node(node).unwrap()
    }
}

impl<'sess> Visitor for TypeChecker<'sess> {
    type Return = Type;

    fn default_return(&self) -> Type {
        self.session.new_typevar()
    }

    fn get_scope_stack(&self) -> &ScopeStack {
        &self.stack
    }

    fn get_session(&self) -> &Session {
        self.session
    }

    fn handle_error(&mut self, node: &Expr, err: Error) -> Result<Self::Return, Error> {
        self.session.print_error(&err.add_pos(node.get_pos()));
        Ok(self.default_return())
    }

    fn visit_vec(&mut self, code: &Vec<Expr>) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        let mut last = scope.find_type(self.session, "()")?;
        for node in code {
            last = self.visit_node(node)?;
        }
        Ok(last)
    }

    fn visit_node(&mut self, node: &Expr) -> Result<Self::Return, Error> {
        match visitor::walk_node(self, node) {
            Ok(ttype) => {
                debug!("CHECK: {:?} {:?}", ttype, node);

                if let Err(err) = self.session.update_type(node.id, &ttype) {
                    self.session.print_error(&err.add_pos(node.get_pos()));
                }
                Ok(ttype)
            },
            Err(err) => self.handle_error(node, err),
        }
    }

    fn visit_pattern(&mut self, pat: &Pattern) -> Result<Self::Return, Error> {
        match visitor::walk_pattern(self, pat) {
            Ok(ttype) => {
                debug!("CHECK: PAT {:?} {:?}", ttype, pat);
                if let Err(err) = self.session.update_type(pat.id, &ttype) {
                    self.session.print_error(&err.add_pos(pat.pos));
                }
                Ok(ttype)
            },
            Err(err) => {
                self.session.print_error(&err.add_pos(pat.pos));
                Ok(self.default_return())
            },
        }
    }


    fn visit_literal(&mut self, _id: UniqueID, _lit: &Literal) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        match _lit {
            Literal::Unit => Ok(scope.find_type(self.session, "()")?),
            Literal::Boolean(_) => Ok(scope.find_type(self.session, "Bool")?),
            Literal::Character(_) => Ok(scope.find_type(self.session, "Char")?),
            Literal::Integer(_) => Ok(scope.find_type(self.session, "Int")?),
            Literal::Real(_) => Ok(scope.find_type(self.session, "Real")?),
            Literal::String(_) => Ok(scope.find_type(self.session, "String")?),
        }
    }

    fn visit_function(&mut self, refid: UniqueID, func: &Function) -> Result<Self::Return, Error> {
        let defid = self.session.get_ref(refid)?;
        let fscope = self.session.map.get(defid).unwrap();
        let dftype = self.session.get_type(defid).unwrap();
        let rtype = dftype.get_rettype()?;

        let mut argtypes = vec!();
        for arg in func.args.iter() {
            let arg_defid = self.session.get_ref(arg.id)?;
            let mut atype = self.session.get_type(arg_defid).unwrap();

            if &arg.name[..] == "self" {
                let stype = fscope.find_type(self.session, "Self")?;
                atype = expect_type(self.session, Some(&atype), Some(&stype), Check::Def)?;
            }
            self.session.update_type(arg_defid, &atype)?;
            argtypes.push(atype);
        }

        let rettype = self.with_scope(fscope.clone(), |visitor| {
            expect_type(visitor.session, Some(rtype), Some(&visitor.visit_vec(&func.body)?), Check::Def)
        })?;

        // Resolve type variables that can be
        for i in 0 .. argtypes.len() {
            argtypes[i] = resolve_type(self.session, argtypes[i].clone(), false)?;
            self.session.update_type(func.args[i].id, &argtypes[i])?;
        }

        let tupleargs = Type::Tuple(argtypes);
        let nftype = Type::Function(r(tupleargs.clone()), r(rettype), func.abi);

        debug!("FINISHED FUNCTION: {:?} -> {:?}", self.session.get_type(defid), nftype);
        self.session.update_type(defid, &nftype)?;
        Ok(nftype)
    }

    fn visit_invoke(&mut self, refid: UniqueID, fexpr: &Expr, args: &Vec<Expr>, fid: UniqueID) -> Result<Self::Return, Error> {
        let mut atypes = vec!();
        for value in args {
            atypes.push(self.visit_node_or_error(value));
        }
        let atypes = Type::Tuple(atypes);

        let dtype = self.session_find_variant(refid, fexpr, &atypes)?;
        debug!("INVOKE TYPE: {:?} has type {:?}", refid, dtype);
        let etype = Type::map_all_typevars(self.session, dtype.clone());

        let ftype = match etype {
            Type::Function(args, rettype, abi) => {
                let ftype = expect_type(self.session, Some(&Type::Function(args, rettype.clone(), abi)), Some(&Type::Function(r(atypes), r(*rettype), abi)), Check::Def)?;
                // TODO should this actually be another expect, so type resolutions that occur in later args affect earlier args?  Might not be needed unless you add typevar constraints
                let ftype = resolve_type(self.session, ftype, false)?;        // NOTE This ensures the early arguments are resolved despite typevars not being assigned until later in the signature

                ftype
            },
            Type::Variable(vid) => {
                let ftype = Type::Function(r(atypes), r(self.get_type_or_new_typevar(refid)), ABI::Unknown);
                self.session.update_type(vid, &ftype)?;
                ftype
            },
            _ => return Err(Error::new(format!("NotAFunction: {:?}", fexpr))),
        };

        let rtype = ftype.get_rettype()?;
        self.session.set_type(fid, dtype);
        Ok(rtype.clone())
    }

    fn visit_side_effect(&mut self, _id: UniqueID, _op: &str, args: &Vec<Expr>) -> Result<Self::Return, Error> {
        let mut ltype = None;
        for expr in args {
            ltype = Some(expect_type(self.session, ltype.as_ref(), Some(&self.visit_node_or_error(expr)), Check::List)?);
        }
        Ok(ltype.unwrap())
    }

    fn visit_definition(&mut self, refid: UniqueID, _mutable: Mutability, _name: &str, _ttype: &Option<Type>, expr: &Expr) -> Result<Self::Return, Error> {
        let defid = self.session.get_ref(refid)?;
        let dtype = self.session.get_type(defid);
        let btype = expect_type(self.session, dtype.as_ref(), Some(&self.visit_node_or_error(expr)), Check::Def)?;
        self.session.update_type(defid, &btype)?;
        Ok(btype)
    }

    fn visit_field(&mut self, refid: UniqueID, _mutable: Mutability, _name: &str, _ttype: &Option<Type>) -> Result<Self::Return, Error> {
        let defid = self.session.get_ref(refid)?;
        let dtype = self.get_type_or_new_typevar(defid);
        self.session.update_type(defid, &dtype)?;
        Ok(dtype)
    }

    fn visit_declare(&mut self, _refid: UniqueID, _vis: Visibility, _name: &str, _ttype: &Type, _whereclause: &WhereClause) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        Ok(scope.find_type(self.session, "()")?)
    }

    fn visit_identifier(&mut self, refid: UniqueID, name: &str) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        if let Ok(ttype) = self.session.get_type_from_ref(refid) {
            Ok(ttype.clone())
        } else {
            match scope.get_var_def(name) {
                Some(defid) => match self.session.get_type(defid) {
                    Some(ttype) => Ok(ttype.clone()),
                    None => return Err(Error::new(format!("TypeError: the reference {:?} has no type or has an ambiguous type", name))),
                }
                None => panic!("InternalError: name {:?} is undefined, but should have been caught in the name binding phase", name),
            }
        }
    }

    fn visit_if(&mut self, _refid: UniqueID, cond: &Expr, texpr: &Expr, fexpr: &Expr) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        let ctype = self.visit_node_or_error(cond);
        expect_type(self.session, Some(&scope.find_type(self.session, "Bool")?), Some(&ctype), Check::Def)?;
        let ttype = self.visit_node_or_error(texpr);
        let ftype = self.visit_node_or_error(fexpr);
        expect_type(self.session, Some(&ttype), Some(&ftype), Check::List)
    }

    fn visit_match(&mut self, _refid: UniqueID, cond: &Expr, cases: &Vec<MatchCase>) -> Result<Self::Return, Error> {
        let mut ctype = self.visit_node_or_error(cond);

        let mut rtype = None;
        for case in cases {
            let lscope = self.session.map.get(case.id).unwrap();
            self.with_scope(lscope.clone(), |visitor| {
                ctype = expect_type(visitor.session, Some(&ctype), Some(&visitor.visit_pattern(&case.pat)?), Check::List)?;
                rtype = Some(expect_type(visitor.session, rtype.as_ref(), Some(&visitor.visit_node_or_error(&case.body)), Check::List)?);
                Ok(rtype.clone().unwrap())
            })?;
        }

        Ok(rtype.unwrap())
    }

    fn visit_try(&mut self, _refid: UniqueID, cond: &Expr, cases: &Vec<MatchCase>) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        let btype = self.visit_node_or_error(cond);
        let mut ctype = scope.find_type(self.session, "Exception")?;

        let mut rtype = None;
        for case in cases {
            let lscope = self.session.map.get(case.id).unwrap();
            self.with_scope(lscope.clone(), |visitor| {
                ctype = expect_type(visitor.session, Some(&ctype), Some(&visitor.visit_pattern(&case.pat)?), Check::List)?;
                rtype = Some(expect_type(visitor.session, rtype.as_ref(), Some(&visitor.visit_node_or_error(&case.body)), Check::List)?);
                Ok(rtype.clone().unwrap())
            })?;
        }

        expect_type(self.session, Some(&btype), rtype.as_ref(), Check::List)?;
        Ok(rtype.unwrap())
    }

    fn visit_raise(&mut self, _refid: UniqueID, expr: &Expr) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        // TODO should you check for a special error/exception type?
        let extype = scope.find_type(self.session, "Exception")?;
        expect_type(self.session, Some(&extype), Some(&self.visit_node_or_error(expr)), Check::Def)?;
        Ok(self.session.new_typevar())
    }

    fn visit_while(&mut self, _refid: UniqueID, cond: &Expr, body: &Expr) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        let ctype = self.visit_node_or_error(cond);
        expect_type(self.session, Some(&scope.find_type(self.session, "Bool")?), Some(&ctype), Check::Def)?;
        self.visit_node_or_error(body);
        Ok(scope.find_type(self.session, "()")?)
    }

    fn visit_ref(&mut self, refid: UniqueID, expr: &Expr) -> Result<Self::Return, Error> {
        let ttype = self.visit_node_or_error(expr);
        let rtype = Type::Ref(r(ttype));
        let rtype = expect_type(self.session, self.session.get_type(refid).as_ref(), Some(&rtype), Check::Def)?;
        Ok(rtype)
    }

    fn visit_deref(&mut self, refid: UniqueID, expr: &Expr) -> Result<Self::Return, Error> {
        match self.visit_node_or_error(expr) {
            Type::Ref(etype) => {
                let etype = expect_type(self.session, self.session.get_type(refid).as_ref(), Some(&etype), Check::Def)?;
                Ok(etype)
            },
            Type::Variable(vid) => {
                let etype = expect_type(self.session, self.session.get_type(refid).as_ref(), None, Check::Def)?;
                self.session.update_type(vid, &Type::Ref(r(etype.clone())))?;
                Ok(etype)
            },
            ttype => return Err(Error::new(format!("TypeError: attempting to dereference a non-reference: {:?}", ttype))),
        }
    }

    fn visit_tuple(&mut self, refid: UniqueID, items: &Vec<Expr>) -> Result<Self::Return, Error> {
        // TODO would this not be a bug if the expected type was for some reason a variable?
        //let etypes = match self.session.get_type(refid) {
        //    Some(e) => e.get_types()?.iter().map(|i| Some(i.clone())).collect(),
        //    None => vec![None; items.len()]
        //};
        let etypes = vec![None; items.len()];

        if etypes.len() != items.len() {
            return Err(Error::new(format!("TypeError: number of tuple items don't match: expected {:?} with {} items but found {} items", self.session.get_type(refid), etypes.len(), items.len())));
        }

        let mut types = vec!();
        for (expr, etype) in items.iter().zip(etypes.iter()) {
            types.push(expect_type(self.session, etype.as_ref(), Some(&self.visit_node_or_error(expr)), Check::List)?);
        }
        Ok(Type::Tuple(types))
    }

    fn visit_record(&mut self, _refid: UniqueID, items: &Vec<(String, Expr)>) -> Result<Self::Return, Error> {
        let mut types = vec!();
        for (name, expr) in items {
            types.push((name.clone(), self.visit_node_or_error(expr)));
        }
        Ok(Type::Record(types))
    }

    fn visit_record_update(&mut self, _refid: UniqueID, record: &Expr, items: &Vec<(String, Expr)>) -> Result<Self::Return, Error> {
        let rtype = self.visit_node_or_error(record);

        for (name, expr) in items {
            let ftype = rtype.get_record_field(name)?;
            let itype = self.visit_node_or_error(expr);
            expect_type(self.session, Some(ftype), Some(&itype), Check::Def)?;
        }
        Ok(rtype)
    }

    fn visit_class(&mut self, refid: UniqueID, _classtype: &Type, _parenttype: &Option<Type>, _whereclause: &WhereClause, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        let defid = self.session.get_ref(refid)?;

        let scope = self.stack.get_scope();
        let tscope = self.session.map.get(defid).unwrap();
        self.with_scope(tscope.clone(), |visitor| {
            visitor.visit_vec(body)
        })?;
        Ok(scope.find_type(self.session, "()")?)
    }

    fn visit_enum(&mut self, _refid: UniqueID, _enumtype: &Type, _whereclause: &WhereClause, _variants: &Vec<EnumVariant>) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        Ok(scope.find_type(self.session, "()")?)
    }

    fn visit_type_alias(&mut self, _refid: UniqueID, _deftype: &Type, _ttype: &Type) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        Ok(scope.find_type(self.session, "()")?)
    }

    fn visit_trait_def(&mut self, _refid: UniqueID, _traitname: &str, _body: &Vec<Expr>) -> Result<Self::Return, Error> {
        // Nothing to do because only decls are allowed in the trait body, so all the type information is present
        let scope = self.stack.get_scope();
        Ok(scope.find_type(self.session, "()")?)
    }

    fn visit_trait_impl(&mut self, refid: UniqueID, _traitname: &str, _impltype: &Type, _whereclause: &WhereClause, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        let impl_id = self.session.get_ref(refid)?;

        // TODO this gets the impl-specific tscope and not the trait def tscope
        let tscope = self.session.map.get(impl_id).unwrap();
        self.with_scope(tscope.clone(), |visitor| {
            visitor.visit_vec(body)
        })?;

        let scope = self.stack.get_scope();
        Ok(scope.find_type(self.session, "()")?)
    }

    fn visit_methods(&mut self, refid: UniqueID, _ttype: &Type, _whereclause: &WhereClause, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        let defid = self.session.get_ref(refid)?;

        let tscope = self.session.map.get(defid).unwrap();
        self.with_scope(tscope.clone(), |visitor| {
            visitor.visit_vec(body)
        })?;
        Ok(tscope.find_type(self.session, "()")?)
    }

    fn visit_annotation(&mut self, refid: UniqueID, _ttype: &Type, code: &Expr) -> Result<Self::Return, Error> {
        let ttype = self.session.get_type(refid).unwrap();
        let ctype = self.visit_node_or_error(code);
        debug!("PTRCAST: {:?} <- {:?}", ttype, ctype);
        expect_type(self.session, Some(&ttype), Some(&ctype), Check::List)?;
        Ok(ttype.clone())
    }

    fn visit_alloc_object(&mut self, refid: UniqueID, _ttype: &Type) -> Result<Self::Return, Error> {
        let classtype = self.session.get_type(refid).unwrap();
        let dtype = self.session.get_type_from_ref(refid)?;
        let mtype = Type::map_all_typevars(self.session, dtype.clone());
        expect_type(self.session, Some(&mtype), Some(&classtype), Check::Def)?;
        Ok(classtype.clone())
    }

    fn visit_resolver(&mut self, refid: UniqueID, left: &Expr, field: &str, oid: UniqueID) -> Result<Self::Return, Error> {
        let defid = self.check_resolve_field(left, field, oid)?;
        self.session.set_ref(refid, defid);
        Ok(Type::map_all_typevars(self.session, self.get_type_or_new_typevar_and_set(defid)))
    }

    fn visit_accessor(&mut self, id: UniqueID, left: &Expr, field: &str, oid: UniqueID) -> Result<Self::Return, Error> {
        let defid = self.check_access_field(left, field, oid)?;
        self.session.set_ref(id, defid);
        Ok(self.get_type_or_new_typevar_and_set(defid))
    }

    fn visit_assignment(&mut self, _refid: UniqueID, left: &Expr, right: &Expr, ty: AssignType) -> Result<Self::Return, Error> {
        match self.get_access_target(left)? {
            Some(defid) => {
                if ty == AssignType::Update && !self.session.get_def(defid).map(|d| d.is_mutable()).unwrap_or(false) {
                    return Err(Error::new(format!("MutableError: attempting to assign to an immutable variable")));
                }

                if self.session.get_type(defid).map(|ttype| !self.check_universals_in_scope(&ttype)).unwrap_or(false) {
                    return Err(Error::new(format!("AssignError: attempting to assign to a universal type outside of it's scope")));
                }
            },
            None => { }
        }

        let ltype = self.visit_node_or_error(left);
        let rtype = self.visit_node_or_error(right);
        expect_type(self.session, Some(&ltype), Some(&rtype), Check::Def)
    }

    fn visit_import(&mut self, _refid: UniqueID, _ident: &str, decls: &Vec<Expr>) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        self.visit_vec(decls)?;
        scope.find_type(self.session, "()")
    }

    fn visit_module_decl(&mut self, _refid: UniqueID, _name: &str) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        scope.find_type(self.session, "()")
    }

    fn visit_pattern_wild(&mut self, refid: UniqueID) -> Result<Self::Return, Error> {
        Ok(self.get_type_or_new_typevar(refid))
    }

    fn visit_pattern_literal(&mut self, refid: UniqueID, lit: &Literal, fid: UniqueID) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        let ltype = self.visit_literal(refid, lit)?;
        self.link_comparison_func(scope, fid, &ltype)?;
        Ok(ltype)
    }

    fn visit_pattern_binding(&mut self, refid: UniqueID, _name: &str) -> Result<Self::Return, Error> {
        let defid = self.session.get_ref(refid)?;
        let btype = self.get_type_or_new_typevar(defid);

        self.session.update_type(defid, &btype)?;
        Ok(btype)
    }

    fn visit_pattern_annotation(&mut self, refid: UniqueID, _ttype: &Type, subpat: &Pattern) -> Result<Self::Return, Error> {
        let ttype = self.session.get_type(refid);
        let etype = self.visit_pattern(subpat)?;
        expect_type(self.session, ttype.as_ref(), Some(&etype), Check::Def)
    }

    fn visit_pattern_ref(&mut self, _refid: UniqueID, subpat: &Pattern) -> Result<Self::Return, Error> {
        let subtype = self.visit_pattern(subpat)?;
        Ok(Type::Ref(r(subtype)))
    }

    fn visit_pattern_tuple(&mut self, _refid: UniqueID, items: &Vec<Pattern>) -> Result<Self::Return, Error> {
        let mut itemtypes = vec!();
        for item in items {
            itemtypes.push(self.visit_pattern(item)?);
        }
        Ok(Type::Tuple(itemtypes))
    }

    fn visit_pattern_record(&mut self, _refid: UniqueID, items: &Vec<(String, Pattern)>) -> Result<Self::Return, Error> {
        let mut itemtypes = vec!();
        for (name, item) in items {
            itemtypes.push((name.clone(), self.visit_pattern(item)?));
        }
        Ok(Type::Record(itemtypes))
    }

    fn visit_pattern_enum_variant(&mut self, refid: UniqueID, _path: &Vec<String>, args: &Vec<Pattern>, eid: UniqueID) -> Result<Self::Return, Error> {
        let enumdef = self.session.get_def_from_ref(refid)?.as_enum()?;
        let variant_id = self.session.get_ref(eid)?;

        match enumdef.get_variant_type_by_id(variant_id) {
            None if args.len() > 0 => return Err(Error::new(format!("TypeError: enum variant doesn't expect any arguments, but found {:?}", args))),
            None => {
                self.session.set_type(eid, enumdef.deftype.clone());
                Ok(Type::map_all_typevars(self.session, enumdef.deftype.clone()))
            },
            Some(ttype) => {
                self.session.set_type(eid, ttype.clone());

                let mut typevars = Type::map_new();
                let def_type = Type::map_typevars(self.session, &mut typevars, enumdef.deftype.clone());
                let variant_type = Type::map_typevars(self.session, &mut typevars, resolve_type(self.session, ttype, false)?);

                let argtypes = args.iter().map(|pat| self.visit_pattern(pat)).collect::<Result<Vec<Type>, Error>>()?;

                expect_type(self.session, Some(&variant_type), Some(&Type::Tuple(argtypes)), Check::Def)?;
                Ok(def_type)
            },
        }
    }

    //fn visit_pattern_tuple(&mut self, id: UniqueID, items: &Vec<Pattern>) -> Result<Self::Return, Error> {
    //fn visit_pattern_record(&mut self, id: UniqueID, items: &Vec<(String, Pattern)>) -> Result<Self::Return, Error> {
}

impl<'sess> TypeChecker<'sess> {
    #[must_use]
    pub fn link_comparison_func(&self, scope: ScopeRef, refid: UniqueID, ctype: &Type) -> Result<(), Error> {
        match scope.get_var_def("==") {
            None => return Err(Error::new(format!("NameError: no \"==\" function defined for type {:?}", ctype))),
            Some(defid) => {
                let (fid, ftype) = self.session_find_variant_id(defid, &Type::Tuple(vec!(ctype.clone(), ctype.clone())))?;
                self.session.set_ref(refid, fid);
                self.session.set_type(refid, ftype);
            }
        }
        Ok(())
    }


    pub fn get_access_target(&mut self, node: &Expr) -> Result<Option<UniqueID>, Error> {
        match &node.kind {
            ExprKind::Identifier(_) => {
                Ok(Some(self.session.get_ref(node.id)?))
            },
            ExprKind::Resolver(left, field, oid) => {
                Ok(Some(self.check_resolve_field(left, field, *oid)?))
            },
            ExprKind::Accessor(left, field, oid) => {
                Ok(Some(self.check_access_field(left, field, *oid)?))
            },
            _ => { Ok(None) },
        }
    }

    pub fn check_resolve_field(&mut self, _left: &Expr, field: &str, oid: UniqueID) -> Result<UniqueID, Error> {
        let ltype = self.session.get_type_from_ref(oid).unwrap();
        let vars = self.session.get_def(ltype.get_id()?)?.get_vars()?;
        Ok(vars.get_var_def(field).ok_or(Error::new(format!("VarError: definition not set for {:?}", field)))?)
    }

    pub fn check_access_field(&mut self, left: &Expr, field: &str, oid: UniqueID) -> Result<UniqueID, Error> {
        let ltype = resolve_type(self.session, self.visit_node_or_error(left), false)?;
        self.session.set_type(oid, ltype.clone());

        match ltype {
            Type::Object(_, _, _) => {
                let vars = self.session.get_def(ltype.get_id()?)?.get_vars()?;
                Ok(vars.get_var_def(field).ok_or(Error::new(format!("VarError: definition not set for {:?}", field)))?)
            },
            Type::Record(items) => {
                let defid = UniqueID::generate();
                let index = items.iter().position(|(name, _)| name == field).unwrap();
                self.session.set_type(defid, items[index].1.clone());
                Ok(defid)
            },
            Type::Tuple(items) => {
                let defid = UniqueID::generate();
                let index = field.parse::<usize>().unwrap();
                self.session.set_type(defid, items[index].clone());
                Ok(defid)
            },
            Type::Universal(_, id) => {
                for trait_id in self.session.get_constraints(id) {
                    let vars = self.session.get_def(trait_id)?.get_vars()?;
                    if let Some(defid) = vars.get_var_def(field) {
                        self.session.set_ref(oid, trait_id);
                        return Ok(defid);
                    }
                }
                return Err(Error::new(format!("VarError: definition not set for {:?}", field)))
            },
            _ => Err(Error::new(format!("TypeError: attempting to access within a non-accessible value: {:?}", ltype)))
        }
    }

    pub fn session_find_variant(&mut self, invid: UniqueID, fexpr: &Expr, argtypes: &Type) -> Result<Type, Error> {
        let defid = match self.get_access_target(fexpr)? {
            Some(id) => id,
            None => { return self.visit_node(fexpr); },
        };

        let (func_id, ftype) = self.session_find_variant_id(defid, argtypes)?;
        self.session.set_ref(fexpr.id, func_id);
        self.session.set_ref(invid, func_id);

        debug!("CHECK VARIANT: {:?} {:?}", ftype, fexpr);
        Ok(ftype)
    }

    pub fn session_find_variant_id(&self, defid: UniqueID, argtypes: &Type) -> Result<(UniqueID, Type), Error> {
        match self.session.get_def(defid) {
            Ok(Def::Overload(ol)) => ol.find_variant(self.session, argtypes),
            _ => Ok((defid, self.get_type_or_new_typevar_and_set(defid)))
        }
    }

    pub fn get_type_or_new_typevar(&self, defid: UniqueID) -> Type {
        match self.session.get_type(defid) {
            Some(ttype) => ttype.clone(),
            None => self.session.new_typevar(),
        }
    }

    pub fn get_type_or_new_typevar_and_set(&self, defid: UniqueID) -> Type {
        let ttype = self.get_type_or_new_typevar(defid);
        self.session.set_type(defid, ttype.clone());
        ttype
    }

    pub fn check_universals_in_scope(&self, ttype: &Type) -> bool {
        let mut checker = UniversalChecker { scope: self.stack.get_scope() };
        return checker.visit_type(ttype).is_ok();
    }
}


struct UniversalChecker {
    pub scope: ScopeRef,
}

impl TypeVisitor for UniversalChecker {
    type Return = ();
    type Error = ();

    fn default_type_return(&mut self) -> Result<Self::Return, Self::Error> {
        Ok(())
    }

    fn visit_type_universal(&mut self, name: &str, id: UniqueID) -> Result<(), ()> {
        match self.scope.get_type_def(&name) == Some(id) {
            true => Ok(()),
            false => Err(()),
        }
    }
}

