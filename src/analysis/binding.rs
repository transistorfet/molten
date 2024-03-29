
use crate::abi::ABI;
use crate::session::{ Session, Error };
use crate::scope::{ ScopeRef, Context };
use crate::types::{ Type, resolve_type, CheckRecursiveType };
use crate::misc::{ UniqueID, r };

use crate::defs::Def;
use crate::defs::enums::EnumDef;
use crate::defs::classes::ClassDef;
use crate::defs::functions::AnyFunc;
use crate::defs::modules::ModuleDef;
use crate::defs::types::TypeAliasDef;
use crate::defs::variables::{ AnyVar, VarDef, ArgDef };
use crate::defs::traits::{ TraitDef, TraitImpl };

use crate::analysis::hir::{ Visibility, Mutability, MatchCase, EnumVariant, WhereClause, Function, Pattern, Expr, ExprKind };
use crate::analysis::visitor::{ self, Visitor, ScopeStack };


#[derive(Clone, Debug, PartialEq)]
pub struct NameBinder<'sess> {
    pub session: &'sess Session,
    pub stack: ScopeStack,
}


impl<'sess> NameBinder<'sess> {
    pub fn bind_names(session: &'sess Session, scope: ScopeRef, code: &Vec<Expr>) {
        let mut namebinder = NameBinder {
            session,
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

    fn get_scope_stack(&self) -> &ScopeStack {
        &self.stack
    }

    fn get_session(&self) -> &Session {
        self.session
    }

    fn handle_error(&mut self, node: &Expr, err: Error) -> Result<Self::Return, Error> {
        self.session.print_error(&err.add_pos(node.get_pos()));
        Ok(())
    }

    fn visit_module(&mut self, _id: UniqueID, name: &str, code: &Expr, memo_id: UniqueID) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();

        let memo_name = ModuleDef::get_memo_name(&ModuleDef::get_module_name(name));
        VarDef::define(self.session, scope.clone(), memo_id, Mutability::Mutable, &memo_name, Some(scope.find_type(self.session, "Bool")?.clone()))?;

        self.visit_node(code)?;

        // Set the basename of the scope of the module's closure to "" so that exported names don't include it
        let fscope = self.session.map.get(self.session.get_ref(code.id).unwrap()).unwrap();
        fscope.set_basename("");
        Ok(())
    }

    fn visit_function(&mut self, refid: UniqueID, func: &Function) -> Result<Self::Return, Error> {
        let defid = self.session.new_def_id(refid);

        let scope = self.stack.get_scope();
        let fscope = self.session.map.get_or_add(defid, &func.name, Context::Func(func.abi, defid), Some(scope.clone()));

        // Check for typevars in the type params
        let mut argtypes = vec!();
        for arg in func.args.iter() {
            let arg_defid = self.session.new_def_id(arg.id);
            let ttype = self.bind_type_option(fscope.clone(), arg.ttype.clone())?.unwrap_or_else(|| self.session.new_typevar());
            ArgDef::define(self.session, fscope.clone(), arg_defid, Mutability::Immutable, &arg.name, Some(ttype.clone()))?;
            argtypes.push(ttype);
        }
        let rettype = self.bind_type_option(fscope.clone(), func.rettype.clone())?;
        bind_where_constraints(self.session, fscope.clone(), &func.whereclause)?;

        // Build type according to the definition, using typevars for missing types
        let nftype = Type::Function(r(Type::Tuple(argtypes)), r(rettype.unwrap_or_else(|| self.session.new_typevar())), func.abi);

        // Define the function variable and it's arguments variables
        AnyFunc::define(self.session, scope.clone(), defid, func.vis, &func.name, func.abi, Some(nftype))?;

        self.with_scope(fscope, |visitor| {
            visitor.visit_vec(&func.body)
        })
    }

    fn visit_definition(&mut self, refid: UniqueID, mutable: Mutability, name: &str, ttype: &Option<Type>, expr: &Expr) -> Result<Self::Return, Error> {
        let defid = self.session.new_def_id(refid);

        let scope = self.stack.get_scope();
        let ttype = self.bind_type_option(scope.clone(), ttype.clone())?;
        AnyVar::define(self.session, scope.clone(), defid, mutable, name, ttype)?;
        self.visit_node(expr)
    }

    fn visit_field(&mut self, refid: UniqueID, mutable: Mutability, name: &str, ttype: &Option<Type>) -> Result<Self::Return, Error> {
        let defid = self.session.new_def_id(refid);

        let scope = self.stack.get_scope();
        let ttype = self.bind_type_option(scope.clone(), ttype.clone())?;
        AnyVar::define(self.session, scope.clone(), defid, mutable, name, ttype)?;
        Ok(())
    }

    fn visit_declare(&mut self, refid: UniqueID, vis: Visibility, name: &str, ttype: &Type, whereclause: &WhereClause) -> Result<Self::Return, Error> {
        let defid = self.session.new_def_id(refid);
        let scope = self.stack.get_scope();
        let abi = ttype.get_abi().unwrap_or(ABI::Molten);
        let tscope = self.session.map.get_or_add(defid, name, Context::Func(abi, defid), Some(scope.clone()));

        let ttype = self.bind_type(tscope.clone(), ttype.clone())?;
        bind_where_constraints(self.session, tscope.clone(), whereclause)?;

        // NOTE we resolve the type here to allow the resolution of aliases before this type becomes the cannonical type of this definition
        let ttype = resolve_type(self.session, ttype, false)?;
        AnyFunc::define(self.session, scope.clone(), defid, vis, name, abi, Some(ttype))?;
        Ok(())
    }

    fn visit_identifier(&mut self, refid: UniqueID, name: &str) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        match scope.get_var_def(name) {
            Some(defid) => self.session.set_ref(refid, defid),
            None => return Err(Error::new(format!("NameError: undefined identifier {:?}", name)))
        }
        Ok(())
    }

    fn visit_try(&mut self, refid: UniqueID, cond: &Expr, cases: &Vec<MatchCase>) -> Result<Self::Return, Error> {
        self.visit_match(refid, cond, cases)
    }

    fn visit_match(&mut self, _refid: UniqueID, cond: &Expr, cases: &Vec<MatchCase>) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        self.visit_node(cond)?;
        // TODO check to make sure Pattern::Wild only occurs as the last case, if at all
        for ref mut case in cases {
            let lscope = self.session.map.get_or_add(case.id, "", Context::Block, Some(scope.clone()));
            visitor::walk_match_case(self, lscope, case)?;
        }
        Ok(())
    }


    fn visit_annotation(&mut self, refid: UniqueID, ttype: &Type, code: &Expr) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        let ttype = self.bind_type(scope.clone(), ttype.clone())?;
        self.session.set_type(refid, ttype);
        self.visit_node(code)
    }

    fn visit_alloc_object(&mut self, refid: UniqueID, ttype: &Type) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        let name = ttype.get_name()?;
        let ttype = self.bind_type(scope.clone(), ttype.clone())?;
        self.session.set_type(refid, ttype);
        match scope.get_type_def(&name) {
            Some(defid) => self.session.set_ref(refid, defid),
            None => return Err(Error::new(format!("NameError: undefined identifier {:?}", name)))
        }
        Ok(())
    }

    fn visit_class(&mut self, refid: UniqueID, classtype: &Type, parenttype: &Option<Type>, whereclause: &WhereClause, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        let defid = self.session.new_def_id(refid);

        let scope = self.stack.get_scope();
        let tscope = self.session.map.get_or_add(defid, classtype.get_name()?, Context::Class(defid), Some(scope.clone()));

        let classtype = self.bind_type(tscope.clone(), classtype.clone().set_id(defid)?)?;
        let parenttype = self.bind_type_option(tscope.clone(), parenttype.clone())?;
        bind_where_constraints(self.session, tscope.clone(), whereclause)?;
        validate_object_definition(&classtype)?;

        ClassDef::define(self.session, scope, defid, classtype, parenttype)?;

        self.with_scope(tscope, |visitor| {
            visitor.visit_vec(body)
        })
    }

    fn visit_type_alias(&mut self, refid: UniqueID, deftype: &Type, ttype: &Type) -> Result<Self::Return, Error> {
        let defid = self.session.new_def_id(refid);
        let scope = self.stack.get_scope();

        let deftype = self.bind_type(scope.clone(), deftype.clone().set_id(defid)?)?;
        validate_object_definition(&deftype)?;
        let ttype = self.bind_type(scope.clone(), ttype.clone())?;

        TypeAliasDef::define(self.session, scope.clone(), defid, deftype, ttype)?;
        Ok(())
    }

    fn visit_enum(&mut self, refid: UniqueID, enumtype: &Type, whereclause: &WhereClause, variants: &Vec<EnumVariant>) -> Result<Self::Return, Error> {
        let defid = self.session.new_def_id(refid);

        let scope = self.stack.get_scope();
        let tscope = self.session.map.get_or_add(defid, enumtype.get_name()?, Context::Enum(defid), Some(scope.clone()));

        let enumtype = self.bind_type(tscope.clone(), enumtype.clone().set_id(defid)?)?;
        bind_where_constraints(self.session, tscope.clone(), whereclause)?;
        validate_object_definition(&enumtype)?;
        let enumdef = EnumDef::define(self.session, scope.clone(), defid, enumtype)?;

        for variant in variants.iter() {
            let mut variant = variant.clone();
            variant.ttype = self.bind_type_option(tscope.clone(), variant.ttype)?;
            CheckRecursiveType::check(variant.ttype.as_ref(), defid)?;
            enumdef.add_variant(self.session, variant)?;
        }
        Ok(())
    }

    fn visit_trait_def(&mut self, refid: UniqueID, traitname: &str, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        let defid = self.session.new_def_id(refid);
        let scope = self.stack.get_scope();
        let tscope = self.session.map.get_or_add(defid, traitname, Context::TraitDef(defid), Some(scope.clone()));

        TraitDef::define(self.session, scope.clone(), defid, traitname)?;

        self.with_scope(tscope, |visitor| {
            visitor.visit_vec(body)
        })
    }

    fn visit_trait_impl(&mut self, refid: UniqueID, traitname: &str, impltype: &Type, whereclause: &WhereClause, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        let impl_id = self.session.new_def_id(refid);
        let trait_id = scope.get_trait_def(traitname).ok_or_else(|| Error::new(format!("NameError: undefined type {:?}", traitname)))?;
        let tscope = self.session.map.get_or_add(impl_id, traitname, Context::TraitImpl(trait_id, impl_id), Some(scope.clone()));

        let impltype = self.bind_type(tscope.clone(), impltype.clone())?;
        bind_where_constraints(self.session, tscope.clone(), whereclause)?;
        TraitImpl::define(self.session, impl_id, trait_id, impltype)?;

        self.with_scope(tscope, |visitor| {
            visitor.visit_vec(body)
        })
    }

    fn visit_methods(&mut self, refid: UniqueID, ttype: &Type, whereclause: &WhereClause, body: &Vec<Expr>) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        let defid = scope.get_type_def(ttype.get_name()?).ok_or(Error::new(format!("NameError: undefined identifier {:?}", ttype.get_name()?)))?;
        let tscope = self.session.map.get(defid).unwrap();

        let ttype = self.bind_type(tscope.clone(), ttype.clone())?;
        bind_where_constraints(self.session, tscope.clone(), whereclause)?;
        validate_object_definition(&ttype)?;

        match self.session.get_def(defid) {
            // Only method blocks on enums are supported at the moment
            Ok(Def::Enum(_)) => { },
            _ => return Err(Error::new(format!("TypeError: cannot define methods for type {:?}", ttype))),
        }
        self.session.set_ref(refid, defid);

        self.with_scope(tscope, |visitor| {
            visitor.visit_vec(body)
        })
    }

    fn visit_resolver(&mut self, _id: UniqueID, left: &Expr, _right: &str, oid: UniqueID) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        // TODO should this always work on a type reference, or should classes be added as values as well as types?
        match &left.kind {
            ExprKind::Identifier(ident) => {
                match scope.get_type_def(&ident) {
                    Some(defid) => {
                        self.session.set_ref(left.id, defid);
                        self.session.set_ref(oid, defid);
                    },
                    None => return Err(Error::new(format!("NameError: undefined type {:?}", ident)))
                }
            },
            _ => { return Err(Error::new(format!("SyntaxError: left-hand side of scope resolver must be identifier"))); }
        }
        Ok(())
    }


    fn visit_pattern_binding(&mut self, refid: UniqueID, name: &str) -> Result<Self::Return, Error> {
        let defid = self.session.new_def_id(refid);

        let scope = self.stack.get_scope();
        VarDef::define(self.session, scope.clone(), defid, Mutability::Immutable, name, None)?;
        Ok(())
    }

    fn visit_pattern_annotation(&mut self, refid: UniqueID, ttype: &Type, pat: &Pattern) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();
        let ttype = self.bind_type(scope.clone(), ttype.clone())?;
        self.session.set_type(refid, ttype);
        self.visit_pattern(pat)
    }

    fn visit_pattern_enum_variant(&mut self, refid: UniqueID, path: &Vec<String>, args: &Vec<Pattern>, eid: UniqueID) -> Result<Self::Return, Error> {
        let scope = self.stack.get_scope();

        let enum_id = scope.get_type_def(&path[0]).ok_or(Error::new(format!("NameError: undefined type {:?}", path[0])))?;
        self.session.set_ref(refid, enum_id);

        let enumdef = self.session.get_def(enum_id)?.as_enum()?;
        let variant_id = enumdef.vars.get_var_def(&path[1]).ok_or(Error::new(format!("NameError: undefined enum variant {:?}", path[1])))?;
        self.session.set_ref(eid, variant_id);

        visitor::walk_pattern_vec(self, args)
    }
}


impl<'sess> NameBinder<'sess> {
    pub fn bind_type(&self, scope: ScopeRef, mut ttype: Type) -> Result<Type, Error> {
        bind_type_names(self.session, scope, Some(&mut ttype))?;
        Ok(ttype)
    }

    pub fn bind_type_option(&self, scope: ScopeRef, ttype: Option<Type>) -> Result<Option<Type>, Error> {
        ttype.map_or(Ok(None), |ttype| self.bind_type(scope.clone(), ttype).map(|result| Some(result)))
    }
}

pub fn bind_type_names(session: &Session, scope: ScopeRef, ttype: Option<&mut Type>) -> Result<(), Error> {
    match ttype {
        Some(ttype) => match ttype {
            &mut Type::Object(ref name, ref mut id, ref mut types) => {
                if *id == UniqueID(0) {
                    match scope.get_type_def(name) {
                        Some(defid) => *id = defid,
                        None => return Err(Error::new(format!("UndefinedType: {:?}", name))),
                    }
                }

                for ttype in types.iter_mut() {
                    bind_type_names(session, scope.clone(), Some(ttype))?;
                }
            },
            &mut Type::Tuple(ref mut types) => {
                for ttype in types.iter_mut() {
                    bind_type_names(session, scope.clone(), Some(ttype))?;
                }
            },
            &mut Type::Record(ref mut types) => {
                types.sort_unstable_by(|a, b| a.0.cmp(&b.0));
                for (_, ttype) in types.iter_mut() {
                    bind_type_names(session, scope.clone(), Some(ttype))?;
                }
            },
            &mut Type::Function(ref mut args, ref mut ret, _) => {
                bind_type_names(session, scope.clone(), Some(args.as_mut()))?;
                bind_type_names(session, scope, Some(ret.as_mut()))?;
            },
            &mut Type::Variable(ref mut id) => {
                if *id == UniqueID(0) {
                    let new_id = session.new_typevar().get_id()?;
                    *id = new_id;
                }
                session.set_type(*id, Type::Variable(*id));
            },
            &mut Type::Universal(ref name, ref mut id) => {
                match scope.find_type(session, name) {
                    Ok(Type::Universal(_, ref eid)) => {
                        *id = *eid;
                    },
                    Err(_) => {
                        *id = UniqueID::generate();
                        let ttype = Type::Universal(name.clone(), *id);
                        scope.define_type(name, *id)?;
                        session.set_type(*id, ttype);
                    }
                    Ok(ttype) => return Err(Error::new(format!("NameError: expected Universal type with name {:?}, but found {:?}", name, ttype))),
                }
            },
            &mut Type::Ref(ref mut ttype) => {
                bind_type_names(session, scope.clone(), Some(ttype))?;
            },
        },
        None => { },
    }
    Ok(())
}

fn bind_where_constraints(session: &Session, scope: ScopeRef, whereclause: &WhereClause) -> Result<(), Error> {
    for (varname, traitname) in &whereclause.constraints {
        let var_id = scope.get_type_def(&varname).ok_or_else(|| Error::new(format!("TypeError: definition not set for {:?}", varname)))?;
        let trait_id = scope.get_trait_def(&traitname).ok_or_else(|| Error::new(format!("TypeError: definition not set for {:?}", traitname)))?;
        let mut constraints = session.get_constraints(var_id);
        if !constraints.contains(&trait_id) {
            constraints.push(trait_id);
        }
        if constraints.len() > 1 {
            panic!("We're only allowing 1 trait constraint per universal at the moment");
        }
        session.set_constraints(var_id, constraints);
    }
    Ok(())
}

fn validate_object_definition(ttype: &Type) -> Result<(), Error> {
    match ttype {
        Type::Object(_, _, types) => {
            for ttype in types {
                match ttype {
                    Type::Universal(_, _) => { },
                    _ => return Err(Error::new(format!("TypeError: type parameter in definition must be a universal type variable, but found {}", ttype))),
                }
            }
            return Ok(());
        },
        _ => {
            return Err(Error::new(format!("TypeError: definition types must be an object type, but found {}", ttype)));
        },
    }
}

