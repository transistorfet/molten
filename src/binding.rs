
use abi::ABI;
use types::Type;
use session::{ Session, Error };
use scope::{ ScopeRef, Context };
use hir::{ NodeID, Visibility, Mutability, Ident, Argument, ClassSpec, MatchCase, EnumVariant, Pattern, PatKind, Expr, ExprKind };
use misc::{ UniqueID, r };

use defs::enums::EnumDef;
use defs::classes::ClassDef;
use defs::functions::{ AnyFunc, ClosureDef };
use defs::types::TypeAliasDef;
use defs::variables::{ AnyVar, VarDef, ArgDef };

use visitor::{ self, Visitor, ScopeStack };


#[derive(Clone, Debug, PartialEq)]
pub struct NameBinder<'sess> {
    pub session: &'sess Session,
    pub stack: ScopeStack,
}


impl<'sess> NameBinder<'sess> {
    pub fn bind_names(session: &'sess Session, scope: ScopeRef, code: &Vec<Expr>) {
        let mut namebinder = NameBinder {
            session: session,
            stack: ScopeStack::new(),
        };

        namebinder.stack.push_scope(scope);
        namebinder.visit(code).unwrap();
        if session.errors.get() > 0 {
            panic!("Exiting due to previous errors");
        }
    }
}


impl<'sess> Visitor for NameBinder<'sess> {
    type Return = ();

    fn default_return(&self) -> () {
        ()
    }

    fn get_scope_stack<'a>(&'a self) -> &'a ScopeStack {
        &self.stack
    }

    fn get_scope_by_id(&self, id: NodeID) -> ScopeRef {
        self.session.map.get(&id)
    }

    fn handle_error(&mut self, node: &Expr, err: Error) -> Result<Self::Return, Error> {
        self.session.print_error(err.add_pos(&node.get_pos()));
        Ok(())
    }

    fn visit_module(&mut self, id: NodeID, name: &str, code: &Expr, memo_id: NodeID) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();

        let memo_name = format!("memo.{}", name);
        VarDef::define(self.session, scope.clone(), memo_id, Mutability::Mutable, &memo_name, Some(scope.make_obj(self.session, "Bool", vec!())?))?;

        let defid = self.session.new_def_id(id);
        let ttype = Type::Function(r(Type::Tuple(vec!())), r(scope.make_obj(self.session, "Bool", vec!())?), ABI::Molten);
        self.session.map.set(defid, scope.clone());
        self.session.set_type(defid, ttype.clone());

        ClosureDef::define(self.session, scope.clone(), defid, Visibility::Public, Some(&format!("run_{}", name)), Some(ttype))?;
        self.visit_node(code)
    }

    fn visit_function(&mut self, id: NodeID, vis: Visibility, ident: &Option<Ident>, args: &Vec<Argument>, rettype: &Option<Type>, body: &Expr, abi: ABI) -> Result<Self::Return, Error> {
        let defid = self.session.new_def_id(id);

        let scope = self.stack.get_scope();
        let fscope = self.session.map.add(defid, Some(scope.clone()));
        fscope.set_basename(ident.as_ref().map_or(&format!("anon{}", defid), |ident| &ident.name));

        // Check for typevars in the type params
        let mut argtypes = vec!();
        for ref arg in args.iter() {
            //let arg_defid = self.session.new_def_id(arg.id);
            // TODO this is a hack because the arguments are being cloned in the class desugaring in refinery, and that means the id is duplicated rather
            //      when a class has a lambda style method defined and assigned to a class field, the refinery will include the class field initializer (the function)
            //      and then also build the __init__() method for the class, it duplicates the value initializer (the function) and assigns it to the class member during init
            let arg_defid = if let Ok(id) = self.session.get_ref(arg.id) {
                id
            } else {
                self.session.new_def_id(arg.id)
            };

            let mut ttype = arg.ttype.clone();
            bind_type_names(self.session, fscope.clone(), ttype.as_mut(), false)?;
            // TODO this is assumed to be always immutable, but maybe shouldn't be
            ArgDef::define(self.session, fscope.clone(), arg_defid, Mutability::Immutable, &arg.ident.name, ttype.clone())?;
            argtypes.push(ttype.unwrap_or_else(|| self.session.new_typevar()));
        }
        let mut rettype = rettype.clone();
        bind_type_names(self.session, fscope.clone(), rettype.as_mut(), false)?;

        // Build type according to the definition, using typevars for missing types
        let nftype = Type::Function(r(Type::Tuple(argtypes)), r(rettype.unwrap_or_else(|| self.session.new_typevar())), abi);

        // Define the function variable and it's arguments variables
        AnyFunc::define(self.session, scope.clone(), defid, vis, ident.as_ref().map(|ident| ident.as_str()), abi, Some(nftype))?;

        self.with_scope(fscope, |visitor| {
            visitor.visit_node(body)
        })
    }

    fn visit_definition(&mut self, id: NodeID, mutable: Mutability, ident: &Ident, ttype: &Option<Type>, expr: &Expr) -> Result<Self::Return, Error> {
        let defid = self.session.new_def_id(id);

        let scope = self.stack.get_scope();
        let mut ttype = ttype.clone();
        bind_type_names(self.session, scope.clone(), ttype.as_mut(), false)?;
        AnyVar::define(self.session, scope.clone(), defid, mutable, &ident.name, ttype)?;
        self.visit_node(expr)
    }

    fn visit_declare(&mut self, id: NodeID, vis: Visibility, ident: &Ident, ttype: &Type) -> Result<Self::Return, Error> {
        let defid = self.session.new_def_id(id);

        let scope = self.stack.get_scope();
        let mut ttype = ttype.clone();
        bind_type_names(self.session, scope.clone(), Some(&mut ttype), false)?;
        let abi = ttype.get_abi().unwrap_or(ABI::Molten);
        AnyFunc::define(self.session, scope.clone(), defid, vis, Some(ident.as_str()), abi, Some(ttype))?;
        Ok(())
    }

    fn visit_identifier(&mut self, id: NodeID, ident: &Ident) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        if self.session.get_ref(id).is_err() {
            // TODO you must check to make sure if we are accessing a variable or argument, that it is either local or we are in a closure...
            //      (the latter being more difficult to figure out; we need to some kind of context value)
            match scope.get_var_def(&ident.name) {
                Some(defid) => self.session.set_ref(id, defid),
                None => return Err(Error::new(format!("NameError: undefined identifier {:?}", ident.name)))
            }
        }
        Ok(())
    }

    fn visit_try(&mut self, id: NodeID, cond: &Expr, cases: &Vec<MatchCase>) -> Result<Self::Return, Error> {
        self.visit_match(id, cond, cases)
    }

    fn visit_match(&mut self, _id: NodeID, cond: &Expr, cases: &Vec<MatchCase>) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        self.visit_node(cond)?;
        // TODO check to make sure Pattern::Wild only occurs as the last case, if at all
        for ref mut case in cases {
            let lscope = self.session.map.add(case.id, Some(scope.clone()));
            lscope.set_context(Context::Block);
            visitor::walk_match_case(self, lscope, case)?;
        }
        Ok(())
    }


    fn visit_ptr_cast(&mut self, id: NodeID, ttype: &Type, code: &Expr) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        let mut ttype = ttype.clone();
        bind_type_names(self.session, scope.clone(), Some(&mut ttype), false)?;
        self.session.set_type(id, ttype);
        self.visit_node(code)
    }

    fn visit_new(&mut self, id: NodeID, classspec: &ClassSpec) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        let mut classspec = classspec.clone();
        bind_classspec_type_names(self.session, scope.clone(), &mut classspec, false)?;
        let classtype = scope.make_obj(self.session, classspec.ident.as_str(), classspec.types.clone())?;
        self.session.set_type(id, classtype);
        match scope.get_type_def(&classspec.ident.name) {
            Some(defid) => self.session.set_ref(id, defid),
            None => return Err(Error::new(format!("NameError: undefined identifier {:?}", classspec.ident.name)))
        }
        Ok(())
    }

    fn visit_class(&mut self, id: NodeID, classspec: &ClassSpec, parentspec: &Option<ClassSpec>, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        let defid = self.session.new_def_id(id);

        let scope = self.stack.get_scope();
        let tscope = self.session.map.get_or_add(defid, Some(scope.clone()));

        // Check for typevars in the type params
        let mut classspec = classspec.clone();
        let mut parentspec = parentspec.clone();
        bind_classspec_type_names(self.session, tscope.clone(), &mut classspec, true)?;
        if let &mut Some(ref mut pspec) = &mut parentspec {
            bind_classspec_type_names(self.session, tscope.clone(), pspec, false)?;
        }

        let classtype = Type::Object(classspec.ident.name, defid, classspec.types);
        let parenttype = match parentspec {
            Some(p) => Some(scope.make_obj(self.session, p.ident.as_str(), p.types.clone())?),
            None => None
        };
        //let classtype = Type::from_spec(classspec.clone());
        //let parenttype = parentspec.clone().map(|p| Type::from_spec(p));

        ClassDef::define(self.session, scope, defid, classtype, parenttype)?;

        self.with_scope(tscope, |visitor| {
            visitor.visit_vec(body)
        })
    }

    fn visit_type_alias(&mut self, id: NodeID, classspec: &ClassSpec, ttype: &Type) -> Result<Self::Return, Error> {
        let defid = self.session.new_def_id(id);

        let scope = self.stack.get_scope();
        let mut ttype = ttype.clone();
        let mut classspec = classspec.clone();
        bind_classspec_type_names(self.session, scope.clone(), &mut classspec, true)?;
        bind_type_names(self.session, scope.clone(), Some(&mut ttype), false)?;

        let deftype = Type::Object(classspec.ident.name, defid, classspec.types);
        TypeAliasDef::define(self.session, scope.clone(), defid, deftype, ttype)?;
        //scope.define_type(classspec.ident.name.clone(), Some(defid));
        //self.session.set_type(defid, ttype.clone());
        Ok(())
    }

    fn visit_enum(&mut self, id: NodeID, classspec: &ClassSpec, variants: &Vec<EnumVariant>) -> Result<Self::Return, Error> {
        let defid = self.session.new_def_id(id);

        let scope = self.stack.get_scope();
        let mut classspec = classspec.clone();
        bind_classspec_type_names(self.session, scope.clone(), &mut classspec, true)?;
        let deftype = Type::Object(classspec.ident.name, defid, classspec.types);
        let enumdef = EnumDef::define(self.session, scope.clone(), defid, deftype)?;

        for variant in variants.iter() {
            // TODO this still requires mut
            let mut variant = variant.clone();
            bind_type_names(self.session, scope.clone(), variant.ttype.as_mut(), false)?;
            check_recursive_type(&variant.ttype.as_ref(), defid)?;
            enumdef.add_variant(self.session, variant)?;
        }
        Ok(())
    }

    fn visit_resolver(&mut self, _node: &Expr, left: &Expr, _right: &Ident, oid: NodeID) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        // TODO should this always work on a type reference, or should classes be added as values as well as types?
        //self.visit_node(left);
        match left.kind {
            ExprKind::Identifier(ref ident) => {
                match scope.get_type_def(&ident.name) {
                    Some(defid) => self.session.set_ref(oid, defid),
                    None => return Err(Error::new(format!("NameError: undefined type {:?}", ident.name)))
                }
            },
            _ => { return Err(Error::new(format!("SyntaxError: left-hand side of scope resolver must be identifier"))); }
        }
        Ok(())
    }


    fn visit_pattern_binding(&mut self, id: NodeID, ident: &Ident) -> Result<Self::Return, Error> {
        let defid = self.session.new_def_id(id);

        let scope = self.stack.get_scope();
        VarDef::define(self.session, scope.clone(), defid, Mutability::Immutable, &ident.name, None)?;
        Ok(())
    }

    fn visit_pattern_annotation(&mut self, id: NodeID, ttype: &Type, pat: &Pattern) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        let mut ttype = ttype.clone();
        bind_type_names(self.session, scope.clone(), Some(&mut ttype), false)?;
        self.session.set_type(id, ttype);
        self.visit_pattern(pat)
    }

    fn visit_pattern_resolve(&mut self, _id: NodeID, left: &Pattern, _ident: &Ident, oid: NodeID) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        match &left.kind {
            PatKind::Identifier(ref ident) => {
                match scope.get_type_def(&ident.name) {
                    Some(defid) => self.session.set_ref(oid, defid),
                    None => return Err(Error::new(format!("NameError: undefined type {:?}", ident.name)))
                }
            },
            _ => { return Err(Error::new(format!("SyntaxError: left-hand side of scope resolver must be identifier"))); }
        }
        Ok(())
    }
}


#[must_use]
pub fn bind_type_names(session: &Session, scope: ScopeRef, ttype: Option<&mut Type>, always_new: bool) -> Result<(), Error> {
    match ttype {
        Some(ttype) => match ttype {
            &mut Type::Object(ref name, ref mut id, ref mut types) => {
                match scope.get_type_def(name) {
                    Some(defid) => *id = defid,
                    None => if !always_new {
                        panic!("UndefinedType: {:?}", name)
                    } else if *id == UniqueID(0) {
                        *id = UniqueID::generate();
                    },
                }

                for ttype in types.iter_mut() {
                    bind_type_names(session, scope.clone(), Some(ttype), always_new)?;
                }
            },
            &mut Type::Tuple(ref mut types) => {
                for ttype in types.iter_mut() {
                    bind_type_names(session, scope.clone(), Some(ttype), always_new)?;
                }
            },
            &mut Type::Record(ref mut types) => {
                types.sort_unstable_by(|a, b| a.0.cmp(&b.0));
                for (_, ttype) in types.iter_mut() {
                    bind_type_names(session, scope.clone(), Some(ttype), always_new)?;
                }
            },
            &mut Type::Function(ref mut args, ref mut ret, _) => {
                bind_type_names(session, scope.clone(), Some(args.as_mut()), always_new)?;
                bind_type_names(session, scope, Some(ret.as_mut()), always_new)?;
            },
            &mut Type::Variable(ref mut id) => {
                if *id == UniqueID(0) {
                    panic!("InternalError: Type::Variable with id 0");
                }
                session.set_type(*id, Type::Variable(*id));
            },
            &mut Type::Universal(ref name, ref mut id) => {
                match scope.find_type(session, name) {
                    Some(Type::Universal(_, ref eid)) => {
                        *id = *eid;
                    },
                    None => {
                        *id = UniqueID::generate();
                        let ttype = Type::Universal(name.clone(), *id);
                        scope.define_type(name, Some(*id))?;
                        session.set_type(*id, ttype);
                    }
                    Some(ttype) => return Err(Error::new(format!("NameError: expected Universal type with name {:?}, but found {:?}", name, ttype))),
                }
            },
            &mut Type::Ref(ref mut ttype) => {
                bind_type_names(session, scope.clone(), Some(ttype), always_new)?;
            },
            &mut Type::Variable(_) => { },
            &mut Type::Ambiguous(_) => { },
        },
        None => { },
    }
    Ok(())
}

#[must_use]
pub fn bind_classspec_type_names(session: &Session, scope: ScopeRef, classspec: &mut ClassSpec, always_new: bool) -> Result<(), Error> {
    let &mut ClassSpec { ref mut types, .. } = classspec;
    types.iter_mut().map(|ref mut ttype| bind_type_names(session, scope.clone(), Some(ttype), always_new)).count();
    Ok(())
}

#[must_use]
pub fn check_recursive_type(ttype: &Option<&Type>, forbidden_id: NodeID) -> Result<(), Error> {
    match ttype {
        Some(ttype) => match ttype {
            Type::Object(ref name, ref id, ref types) => {
                if *id == forbidden_id {
                    return Err(Error::new(format!("TypeError: an enum cannot contain itself in {:?}", name)));
                }

                for ttype in types.iter() {
                    check_recursive_type(&Some(ttype), forbidden_id)?;
                }
            },
            Type::Tuple(ref types) => {
                for ttype in types.iter() {
                    check_recursive_type(&Some(ttype), forbidden_id)?;
                }
            },
            Type::Record(ref types) => {
                for (_, ttype) in types.iter() {
                    check_recursive_type(&Some(ttype), forbidden_id)?;
                }
            },
            Type::Universal(_, ref _id) |
            Type::Variable(ref _id) => {
                // TODO this might be an error?
            },
            Type::Ref(_) |
            Type::Function(_, _, _) => {
                // Any reference or function containing the type is ok
            },
            Type::Ambiguous(_) => { },
        },
        None => { },
    }
    Ok(())
}

