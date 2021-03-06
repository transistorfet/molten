

use defs::Def;
use session::{ Session, Error };
use scope::{ ScopeRef };
use hir::{ NodeID, Visibility, Mutability, AssignType, Literal, Ident, Argument, ClassSpec, MatchCase, EnumVariant, Pattern, Expr, ExprKind };
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
        let mut typechecker = TypeChecker {
            session: session,
            stack: ScopeStack::new(),
            //context: RefCell::new(vec!()),
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
    fn visit_node_expected(&mut self, node: &Expr, expected: Option<Type>) -> Type {
        if let Some(ttype) = expected {
            if let Err(err) = self.session.update_type(node.id, ttype) {
                self.session.print_error(err.add_pos(&node.get_pos()));
            }
        }

        let ttype = self.visit_node(node).unwrap();

        debug!("Node {} has type {:?}", node.id, ttype);
        match self.session.update_type(node.id, ttype.clone()) {
            Ok(_) => ttype,
            Err(err) => {
                self.session.print_error(err.add_pos(&node.get_pos()));
                ttype
            },
        }
    }

    fn visit_pattern_expected(&mut self, pat: &Pattern, expected: Option<Type>) -> Result<Type, Error> {
        if let Some(ttype) = expected {
            self.session.update_type(pat.id, ttype)?;
        }
        self.visit_pattern(pat)
    }
}

impl<'sess> Visitor for TypeChecker<'sess> {
    type Return = Type;

    fn default_return(&self) -> Type {
        self.session.new_typevar()
    }

    fn get_scope_stack<'a>(&'a self) -> &'a ScopeStack {
        &self.stack
    }

    fn get_scope_by_id(&self, id: NodeID) -> ScopeRef {
        self.session.map.get(&id)
    }

    fn handle_error(&mut self, node: &Expr, err: Error) -> Result<Self::Return, Error> {
        self.session.print_error(err.add_pos(&node.get_pos()));
        Ok(self.default_return())
    }

    fn visit_vec(&mut self, code: &Vec<Expr>) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        let mut last: Type = scope.make_obj(self.session, "()", vec!())?;
        for node in code {
            last = self.visit_node(node)?;
        }
        Ok(last)
    }

    fn visit_node(&mut self, node: &Expr) -> Result<Self::Return, Error> {
        match visitor::walk_node(self, node) {
            Ok(ttype) => {
                debug!("CHECK: {:?} {:?}", ttype, node);
                Ok(ttype)
            },
            Err(err) => self.handle_error(node, err),
        }
    }

    fn visit_literal(&mut self, _id: NodeID, _lit: &Literal) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        match _lit {
            Literal::Unit => scope.make_obj(self.session, "()", vec!()),
            Literal::Boolean(_) => scope.make_obj(self.session, "Bool", vec!()),
            Literal::Character(_) => scope.make_obj(self.session, "Char", vec!()),
            Literal::Integer(_) => scope.make_obj(self.session, "Int", vec!()),
            Literal::Real(_) => scope.make_obj(self.session, "Real", vec!()),
            Literal::String(_) => scope.make_obj(self.session, "String", vec!()),
        }
    }

    fn visit_function(&mut self, refid: NodeID, _vis: Visibility, ident: &Option<Ident>, args: &Vec<Argument>, rettype: &Option<Type>, body: &Expr, abi: ABI) -> Result<Self::Return, Error> {
        let defid = self.session.get_ref(refid)?;
        let fscope = self.session.map.get(&defid);
        let dftype = self.session.get_type(defid).unwrap();
        let rtype = dftype.get_rettype()?;

        let mut argtypes = vec!();
        for ref arg in args.iter() {
            let arg_defid = self.session.get_ref(arg.id)?;
            let mut atype = self.session.get_type(arg_defid).unwrap_or_else(|| self.session.new_typevar());

            // TODO default args aren't used yet
            //let vtype = arg.default.clone().map(|ref vexpr| self.visit_node_expected(vexpr, ttype.clone()));
            //atype = expect_type(self.session, scope.clone(), atype, vtype, Check::Def)?;

            if &arg.ident.name[..] == "self" {
                let stype = fscope.find_type(self.session, "Self").ok_or(Error::new(format!("NameError: undefined type \"Self\" in the current scope\n")))?;
                atype = expect_type(self.session, Some(atype), Some(stype), Check::Def)?;
            }
            self.session.update_type(arg_defid, atype.clone())?;
            argtypes.push(atype);
        }

        let rettype = self.with_scope(fscope.clone(), |visitor| {
            expect_type(visitor.session, Some(rtype.clone()), Some(visitor.visit_node_expected(body, Some(rtype.clone()))), Check::Def)
        })?;

        // Resolve type variables that can be
        for i in 0 .. argtypes.len() {
            argtypes[i] = resolve_type(self.session, argtypes[i].clone(), false)?;
            self.session.update_type(args[i].id, argtypes[i].clone())?;
        }

        let tupleargs = Type::Tuple(argtypes);
        let nftype = Type::Function(r(tupleargs.clone()), r(rettype), abi);

        if let Ok(Def::Overload(ol)) = self.session.get_def_from_ref(defid) {
            let (mut found, _) = ol.find_local_variants(self.session, tupleargs.clone());
            found = found.into_iter().filter(|(fid, _)| defid != *fid).collect();
            if found.len() > 0 {
                return Err(Error::new(format!("OverloadError: things {:?}\nvariants found [{}]", ident, found.iter().map(|(_, t)| format!("{}", t)).collect::<Vec<String>>().join(", "))));
            }
        }

        debug!("FINISHED FUNCTION: {:?} -> {:?}", self.session.get_type(defid), nftype);
        self.session.update_type(defid, nftype.clone())?;
        self.session.update_type(refid, nftype.clone())?;
        Ok(nftype)
    }

    fn visit_invoke(&mut self, refid: NodeID, fexpr: &Expr, args: &Vec<Expr>, fid: NodeID) -> Result<Self::Return, Error> {
        let mut atypes = vec!();
        for ref mut value in args {
            atypes.push(self.visit_node_expected(value, None));
        }
        let atypes = Type::Tuple(atypes);

        let dtype = self.session_find_variant(refid, fexpr, &atypes)?;
        debug!("INVOKE TYPE: {:?} has type {:?}", refid, dtype);
        let etype = match dtype {
            Type::Variable(_, _, _) => dtype.clone(),
            _ => Type::map_all_typevars(self.session, dtype.clone()),
        };

        let ftype = match etype {
            Type::Function(args, rettype, abi) => {
                //let ftype = expect_type(self.session, Some(etype.clone()), Some(Type::Function(r(atypes), r(self.session.get_type(refid).unwrap_or_else(|| self.session.new_typevar())), abi)), Check::Update)?;
                let ftype = expect_type(self.session, Some(Type::Function(args, rettype.clone(), abi)), Some(Type::Function(r(atypes), r(*rettype), abi)), Check::Def)?;
                // TODO should this actually be another expect, so type resolutions that occur in later args affect earlier args?  Might not be needed unless you add typevar constraints
                let ftype = resolve_type(self.session, ftype, false)?;        // NOTE This ensures the early arguments are resolved despite typevars not being assigned until later in the signature

                ftype
            },
            Type::Variable(_, vid, _) => {
                let ftype = Type::Function(r(atypes), r(self.session.get_type(refid).unwrap_or_else(|| self.session.new_typevar())), ABI::Unknown);
                self.session.update_type(vid, ftype.clone())?;
                ftype
            },
            _ => return Err(Error::new(format!("NotAFunction: {:?}", fexpr))),
        };

        //self.session.update_type(refid, ftype.clone())?;
        let rtype = ftype.get_rettype()?.clone();
        self.session.set_type(fid, ftype);
        self.session.update_type(refid, rtype.clone())?;
        Ok(rtype)
    }

    fn visit_side_effect(&mut self, _id: NodeID, _op: &Ident, args: &Vec<Expr>) -> Result<Self::Return, Error> {
        let mut ltype = None;
        for ref expr in args {
            ltype = Some(expect_type(self.session, ltype.clone(), Some(self.visit_node_expected(expr, ltype.clone())), Check::List)?);
        }
        Ok(ltype.unwrap())
    }

    fn visit_definition(&mut self, refid: NodeID, _mutable: Mutability, _ident: &Ident, _ttype: &Option<Type>, expr: &Expr) -> Result<Self::Return, Error> {
        let defid = self.session.get_ref(refid)?;
        let dtype = self.session.get_type(defid);
        let btype = expect_type(self.session, dtype.clone(), Some(self.visit_node_expected(expr, dtype)), Check::Def)?;
        self.session.update_type(defid, btype.clone())?;
        self.session.update_type(refid, btype.clone())?;
        Ok(btype)
    }

    fn visit_declare(&mut self, _refid: NodeID, _vis: Visibility, _ident: &Ident, _ttype: &Type) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        scope.make_obj(self.session, "()", vec!())
    }

    fn visit_identifier(&mut self, refid: NodeID, ident: &Ident) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        if let Ok(ttype) = self.session.get_type_from_ref(refid) {
            Ok(ttype)
        } else {
            match scope.get_var_def(&ident.name) {
                Some(defid) => match self.session.get_type(defid) {
                    Some(ttype) => Ok(ttype),
                    None => return Err(Error::new(format!("TypeError: the reference {:?} has no type or has an ambiguous type", ident.name))),
                }
                None => panic!("InternalError: ident {:?} is undefined, but should have been caught in the name binding phase", ident.name),
            }
        }
    }

    fn visit_if(&mut self, _id: NodeID, cond: &Expr, texpr: &Expr, fexpr: &Expr) -> Result<Self::Return, Error> {
        // TODO should this require the cond type to be Bool?
        self.visit_node_expected(cond, None);
        let ttype = self.visit_node_expected(texpr, None);
        let ftype = self.visit_node_expected(fexpr, Some(ttype.clone()));
        expect_type(self.session, Some(ttype), Some(ftype), Check::List)
    }

    fn visit_match(&mut self, _id: NodeID, cond: &Expr, cases: &Vec<MatchCase>) -> Result<Self::Return, Error> {
        let mut ctype = self.visit_node_expected(cond, None);

        let mut rtype = None;
        for ref case in cases {
            let lscope = self.session.map.get(&case.id);
            self.with_scope(lscope.clone(), |visitor| {
                ctype = expect_type(visitor.session, Some(ctype.clone()), Some(visitor.visit_pattern_expected(&case.pat, Some(ctype.clone()))?), Check::List)?;
                rtype = Some(expect_type(visitor.session, rtype.clone(), Some(visitor.visit_node_expected(&case.body, rtype.clone())), Check::List)?);
                Ok(rtype.clone().unwrap())
            })?;
        }

        Ok(rtype.unwrap())
    }

    fn visit_try(&mut self, _id: NodeID, cond: &Expr, cases: &Vec<MatchCase>) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        let btype = self.visit_node_expected(cond, None);
        let mut ctype = scope.make_obj(self.session, "Exception", vec!())?;

        let mut rtype = None;
        for ref case in cases {
            let lscope = self.session.map.get(&case.id);
            self.with_scope(lscope.clone(), |visitor| {
                ctype = expect_type(visitor.session, Some(ctype.clone()), Some(visitor.visit_pattern_expected(&case.pat, Some(ctype.clone()))?), Check::List)?;
                rtype = Some(expect_type(visitor.session, rtype.clone(), Some(visitor.visit_node_expected(&case.body, rtype.clone())), Check::List)?);
                Ok(rtype.clone().unwrap())
            })?;
        }

        expect_type(self.session, Some(btype.clone()), rtype.clone(), Check::List)?;
        Ok(rtype.unwrap())
    }

    fn visit_raise(&mut self, _id: NodeID, expr: &Expr) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        // TODO should you check for a special error/exception type?
        let extype = scope.make_obj(self.session, "Exception", vec!())?;
        expect_type(self.session, Some(extype.clone()), Some(self.visit_node_expected(expr, Some(extype))), Check::Def)?;
        scope.make_obj(self.session, "()", vec!())
    }

    fn visit_while(&mut self, _id: NodeID, cond: &Expr, body: &Expr) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        // TODO should this require the cond type to be Bool?
        self.visit_node_expected(cond, None);
        self.visit_node_expected(body, None);
        scope.make_obj(self.session, "()", vec!())
    }

    fn visit_nil(&mut self, refid: NodeID) -> Result<Self::Return, Error> {
        let ttype = self.session.get_type(refid).unwrap_or_else(|| self.session.new_typevar());
        self.session.set_type(refid, ttype.clone());
        Ok(ttype)
    }

    fn visit_ref(&mut self, refid: NodeID, expr: &Expr) -> Result<Self::Return, Error> {
        let ttype = self.visit_node_expected(expr, None);
        let rtype = Type::Ref(r(ttype));
        let rtype = expect_type(self.session, self.session.get_type(refid), Some(rtype), Check::Def)?;
        self.session.set_type(refid, rtype.clone());
        Ok(rtype)
    }

    fn visit_deref(&mut self, refid: NodeID, expr: &Expr) -> Result<Self::Return, Error> {
        match self.visit_node_expected(expr, None) {
            Type::Ref(etype) => {
                let etype = expect_type(self.session, self.session.get_type(refid), Some(*etype), Check::Def)?;
                self.session.set_type(refid, etype.clone());
                Ok(etype)
            },
            Type::Variable(_, vid, _) => {
                let etype = expect_type(self.session, self.session.get_type(refid), None, Check::Def)?;
                self.session.update_type(vid, Type::Ref(r(etype.clone())))?;
                self.session.set_type(refid, etype.clone());
                Ok(etype)
            },
            ttype @ _ => return Err(Error::new(format!("TypeError: attempting to dereference a non-reference: {:?}", ttype))),
        }
    }

    fn visit_tuple(&mut self, refid: NodeID, items: &Vec<Expr>) -> Result<Self::Return, Error> {
        // TODO would this not be a bug if the expected type was for some reason a variable?
        //let etypes = match self.session.get_type(refid) {
        //    Some(ref e) => e.get_types()?.iter().map(|i| Some(i.clone())).collect(),
        //    None => vec![None; items.len()]
        //};
        let etypes = vec![None; items.len()];

        if etypes.len() != items.len() {
            return Err(Error::new(format!("TypeError: number of tuple items don't match: expected {:?} with {} items but found {} items", self.session.get_type(refid), etypes.len(), items.len())));
        }

        let mut types = vec!();
        for (ref expr, etype) in items.iter().zip(etypes.iter()) {
            types.push(expect_type(self.session, etype.clone(), Some(self.visit_node_expected(expr, etype.clone())), Check::List)?);
        }
        self.session.set_type(refid, Type::Tuple(types.clone()));
        Ok(Type::Tuple(types))
    }

    fn visit_record(&mut self, refid: NodeID, items: &Vec<(Ident, Expr)>) -> Result<Self::Return, Error> {
        let mut types = vec!();
        for (ref ident, ref expr) in items {
            types.push((ident.name.clone(), self.visit_node_expected(expr, None)));
        }
        self.session.set_type(refid, Type::Record(types.clone()));
        Ok(Type::Record(types))
    }

    fn visit_record_update(&mut self, refid: NodeID, record: &Expr, items: &Vec<(Ident, Expr)>) -> Result<Self::Return, Error> {
        let rtype = self.visit_node_expected(record, None);

        for (ref ident, ref expr) in items {
            let ftype = rtype.get_record_field(ident.name.as_str())?;
            let itype = self.visit_node_expected(expr, None);
            expect_type(self.session, Some(ftype.clone()), Some(itype), Check::Def)?;
        }
        self.session.set_type(refid, rtype.clone());
        Ok(rtype)
    }

    fn visit_enum(&mut self, _id: NodeID, _classspec: &ClassSpec, _variants: &Vec<EnumVariant>) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        scope.make_obj(self.session, "()", vec!())
    }

    fn visit_type_alias(&mut self, _id: NodeID, _classspec: &ClassSpec, _ttype: &Type) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        scope.make_obj(self.session, "()", vec!())
    }

    fn visit_ptr_cast(&mut self, refid: NodeID, _ttype: &Type, code: &Expr) -> Result<Self::Return, Error> {
        let ttype = self.session.get_type(refid);
        let ctype = self.visit_node_expected(code, None);
        debug!("PTRCAST: {:?} <- {:?}", ttype, ctype);
        expect_type(self.session, ttype.clone(), Some(ctype), Check::List)?;
        Ok(ttype.unwrap())
    }

    fn visit_new(&mut self, refid: NodeID, _classspec: &ClassSpec) -> Result<Self::Return, Error> {
        let classtype = self.session.get_type(refid).unwrap();
        let dtype = self.session.get_type_from_ref(refid)?;
        let mtype = Type::map_all_typevars(self.session, dtype.clone());
        check_type_params(self.session, &mtype.get_params()?, &classtype.get_params()?, Check::Def, true)?;
        Ok(classtype)
    }

    fn visit_class(&mut self, refid: NodeID, _classspec: &ClassSpec, _parentspec: &Option<ClassSpec>, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        let defid = self.session.get_ref(refid)?;
        let tscope = self.session.map.get(&defid);
        self.with_scope(tscope.clone(), |visitor| {
            visitor.visit_vec(body)
        })?;
        scope.make_obj(self.session, "()", vec!())
    }

    fn visit_resolver(&mut self, node: &Expr, left: &Expr, right: &Ident, oid: NodeID) -> Result<Self::Return, Error> {
        self.visit_accessor(node, left, right, oid)
    }

    fn visit_accessor(&mut self, node: &Expr, _left: &Expr, _right: &Ident, _oid: NodeID) -> Result<Self::Return, Error> {
        let (refid, defid) = self.get_access_ids(node)?.unwrap();
        self.session.set_ref(refid, defid);
        Ok(self.get_type_or_new_typevar(defid, self.session.get_type(refid)))
    }

    fn visit_assignment(&mut self, _id: NodeID, left: &Expr, right: &Expr, ty: AssignType) -> Result<Self::Return, Error> {
        // TODO this is duplicated in check_types_node(left)... can we avoid that
        match self.get_access_ids(left)? {
            Some((_refid, defid)) => {
                if ty == AssignType::Update && !self.session.get_def(defid).map(|d| d.is_mutable()).unwrap_or(false) {
                    return Err(Error::new(format!("MutableError: attempting to assign to an immutable variable")));
                }
            },
            None => { }
        }

        let ltype = self.visit_node_expected(left, None);
        let rtype = self.visit_node_expected(right, Some(ltype.clone()));
        expect_type(self.session, Some(ltype), Some(rtype), Check::Def)
    }

    fn visit_import(&mut self, _id: NodeID, _ident: &Ident, decls: &Vec<Expr>) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        self.visit_vec(decls)?;
        scope.make_obj(self.session, "()", vec!())
    }

    fn visit_pattern_wild(&mut self, id: NodeID) -> Result<Self::Return, Error> {
        Ok(self.session.get_type(id).unwrap_or_else(|| self.session.new_typevar()))
    }

    fn visit_pattern_literal(&mut self, id: NodeID, lit: &Literal) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        let ltype = self.visit_literal(id, lit)?;
        self.link_comparison_func(scope.clone(), id, &ltype)?;
        Ok(ltype)
    }

    fn visit_pattern_binding(&mut self, id: NodeID, _ident: &Ident) -> Result<Self::Return, Error> {
        let defid = self.session.get_ref(id)?;
        let btype = self.session.get_type(defid).unwrap_or_else(|| self.session.new_typevar());
        self.session.update_type(defid, btype.clone())?;
        self.session.update_type(id, btype.clone())?;
        Ok(btype)
    }

    fn visit_pattern_annotation(&mut self, id: NodeID, _ttype: &Type, subpat: &Pattern) -> Result<Self::Return, Error> {
        let ttype = self.session.get_type(id);
        let etype = self.visit_pattern_expected(subpat, ttype.clone())?;
        expect_type(self.session, ttype, Some(etype), Check::Def)
    }

    fn visit_pattern_resolve(&mut self, id: NodeID, _left: &Pattern, field: &Ident, oid: NodeID) -> Result<Self::Return, Error> {
        let ltype = self.session.get_type_from_ref(oid).unwrap();

        let vars = self.session.get_def(ltype.get_id()?)?.get_vars()?;
        let defid = vars.get_var_def(&field.name).ok_or(Error::new(format!("VarError: definition not set for {:?}", field.name)))?;
        self.session.set_ref(id, defid);
        let ttype = self.session.get_type(defid).ok_or(Error::new(format!("TypeError: no type set for id {:?}", defid)))?;
        expect_type(self.session, Some(ttype), self.session.get_type(id), Check::Def)
    }

    fn visit_pattern_enum_args(&mut self, id: NodeID, left: &Pattern, args: &Vec<Pattern>) -> Result<Self::Return, Error> {
        self.visit_pattern(left)?;
        let variant_id = self.session.get_ref(left.get_id())?;
        self.session.set_ref(id, variant_id);
        let enumdef = self.session.get_def(self.session.get_ref(variant_id)?)?.as_enum()?;
        match enumdef.get_variant_type_by_id(variant_id) {
            None => return Err(Error::new(format!("TypeError: enum variant doesn't expect any arguments, but found {:?}", args))),
            Some(ttype) => {
                // Map the typevars from the enum type params into the enum variant's types, in order to use type hint from 'expected'
                let mut typevars = Type::map_new();
                let vtype = Type::map_typevars(self.session, &mut typevars, enumdef.deftype.clone());
                let ctype = expect_type(self.session, Some(vtype), self.session.get_type(id), Check::Def)?;
                let rtype = resolve_type(self.session, Type::map_typevars(self.session, &mut typevars, ttype), false)?;
                let types = rtype.as_vec();

                if types.len() != args.len() {
                    return Err(Error::new(format!("TypeError: number of enum arguments expected doesn't match. Expected {:?}, found {:?}", types.len(), args.len())));
                }

                let mut argtypes = vec!();
                for (arg, ttype) in args.iter().zip(types.iter()) {
                    argtypes.push(self.visit_pattern_expected(&arg, Some(ttype.clone()))?);
                }

                expect_type(self.session, Some(rtype), Some(Type::Tuple(argtypes)), Check::Def)?;
                Ok(enumdef.deftype.clone())
            },
        }
    }

    //fn visit_pattern_tuple(&mut self, id: NodeID, items: &Vec<Pattern>) -> Result<Self::Return, Error> {
    //fn visit_pattern_record(&mut self, id: NodeID, items: &Vec<(Ident, Pattern)>) -> Result<Self::Return, Error> {
}

impl<'sess> TypeChecker<'sess> {
    #[must_use]
    pub fn link_comparison_func(&self, scope: ScopeRef, refid: NodeID, ctype: &Type) -> Result<(), Error> {
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


    pub fn get_access_ids(&mut self, node: &Expr) -> Result<Option<(NodeID, NodeID)>, Error> {
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
                let ltype = resolve_type(self.session, self.visit_node_expected(left, None), false)?;
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

    pub fn session_find_variant(&mut self, invid: NodeID, fexpr: &Expr, argtypes: &Type) -> Result<Type, Error> {
        let (refid, defid) = match self.get_access_ids(fexpr)? {
            Some(ids) => ids,
            None => { return self.visit_node(fexpr); },
        };


        let (fid, ftype) = self.session_find_variant_id(defid, argtypes)?;
        self.session.set_ref(refid, fid);
        self.session.set_ref(invid, fid);

        debug!("CHECK VARIANT: {:?} {:?}", ftype, fexpr);
        Ok(ftype)
    }

    pub fn session_find_variant_id(&self, defid: NodeID, argtypes: &Type) -> Result<(NodeID, Type), Error> {
        match self.session.get_def(defid) {
            Ok(Def::Overload(ol)) => ol.find_variant(self.session, argtypes.clone()),
            _ => Ok((defid, self.get_type_or_new_typevar(defid, None)))
        }
    }

    pub fn get_type_or_new_typevar(&self, defid: NodeID, expected: Option<Type>) -> Type {
        match self.session.get_type(defid) {
            Some(ttype) => ttype,
            None => {
                let ttype = expected.unwrap_or_else(|| self.session.new_typevar());
                self.session.set_type(defid, ttype.clone());
                ttype
            }
        }
    }
}


