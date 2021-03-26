
use std::cell::RefCell;

use abi::ABI;
use types::Type;
use session::Error;
use scope::ScopeRef;
use ast::{ NodeID, AST, Mutability, Visibility, AssignType, Ident, ClassSpec, Argument, MatchCase, EnumVariant, Pattern, Literal };


#[derive(Clone, Debug, PartialEq)]
pub struct ScopeStack {
    stack: RefCell<Vec<ScopeRef>>,
}

impl ScopeStack {
    pub fn new() -> Self {
        Self {
            stack: RefCell::new(Vec::new())
        }
    }

    pub fn push_scope(&self, scope: ScopeRef) {
        self.stack.borrow_mut().push(scope);
    }

    pub fn pop_scope(&self) {
        self.stack.borrow_mut().pop();
    }

    pub fn get_scope(&self) -> ScopeRef {
        self.stack.borrow().last().map(|s| s.clone()).unwrap()
    }
}


pub trait Visitor: Sized {
    type Return;

    fn default_return(&self) -> Self::Return;

    fn get_scope_stack<'a>(&'a self) -> &'a ScopeStack;
    fn get_scope_by_id(&self, id: NodeID) -> ScopeRef;

    fn with_scope<F>(&mut self, scope: ScopeRef, f: F) -> Result<Self::Return, Error> where F: FnOnce(&mut Self) -> Result<Self::Return, Error> {
        self.get_scope_stack().push_scope(scope);
        let ret = f(self);
        self.get_scope_stack().pop_scope();
        ret
    }


    fn visit_literal(&mut self, id: NodeID, lit: &Literal) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_nil(&mut self, id: NodeID) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_ptr_cast(&mut self, id: NodeID, ttype: &Type, code: &AST) -> Result<Self::Return, Error> {
        self.visit_node(code)
    }

    fn visit_ref(&mut self, id: NodeID, expr: &AST) -> Result<Self::Return, Error> {
        self.visit_node(expr)
    }

    fn visit_deref(&mut self, id: NodeID, expr: &AST) -> Result<Self::Return, Error> {
        self.visit_node(expr)
    }

    fn visit_list(&mut self, id: NodeID, items: &Vec<AST>) -> Result<Self::Return, Error> {
        self.visit_vec(items)
    }

    fn visit_tuple(&mut self, id: NodeID, items: &Vec<AST>) -> Result<Self::Return, Error> {
        self.visit_vec(items)
    }

    fn visit_record(&mut self, id: NodeID, items: &Vec<(Ident, AST)>) -> Result<Self::Return, Error> {
        walk_record(self, id, items)
    }

    fn visit_record_update(&mut self, id: NodeID, record: &AST, items: &Vec<(Ident, AST)>) -> Result<Self::Return, Error> {
        walk_record_update(self, id, record, items)
    }


    fn visit_get_value(&mut self, id: NodeID) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_identifier(&mut self, id: NodeID, ident: &Ident) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_resolver(&mut self, id: NodeID, left: &AST, right: &Ident, oid: NodeID) -> Result<Self::Return, Error> {
        self.visit_node(left)
    }

    fn visit_accessor(&mut self, id: NodeID, left: &AST, right: &Ident, oid: NodeID) -> Result<Self::Return, Error> {
        self.visit_node(left)
    }


    fn visit_block(&mut self, id: NodeID, code: &Vec<AST>) -> Result<Self::Return, Error> {
        self.visit_vec(code)
    }

    fn visit_invoke(&mut self, id: NodeID, func: &AST, args: &Vec<AST>) -> Result<Self::Return, Error> {
        walk_invoke(self, id, func, args)
    }


    fn visit_side_effect(&mut self, id: NodeID, op: &Ident, args: &Vec<AST>) -> Result<Self::Return, Error> {
        self.visit_vec(args)
    }


    fn visit_if(&mut self, id: NodeID, cond: &AST, texpr: &AST, fexpr: &AST) -> Result<Self::Return, Error> {
        walk_if(self, id, cond, texpr, fexpr)
    }

    fn visit_raise(&mut self, id: NodeID, expr: &AST) -> Result<Self::Return, Error> {
        self.visit_node(expr)
    }

    fn visit_try(&mut self, id: NodeID, cond: &AST, cases: &Vec<MatchCase>) -> Result<Self::Return, Error> {
        walk_try(self, id, cond, cases)
    }

    fn visit_match(&mut self, id: NodeID, cond: &AST, cases: &Vec<MatchCase>) -> Result<Self::Return, Error> {
        walk_match(self, id, cond, cases)
    }

    fn visit_while(&mut self, id: NodeID, cond: &AST, body: &AST) -> Result<Self::Return, Error> {
        walk_while(self, id, cond, body)
    }


    fn visit_declare(&mut self, id: NodeID, vis: Visibility, ident: &Ident, ttype: &Type) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_function(&mut self, id: NodeID, vis: Visibility, ident: &Option<Ident>, args: &Vec<Argument>, rettype: &Option<Type>, body: &AST, abi: ABI) -> Result<Self::Return, Error> {
        walk_function(self, id, vis, ident, args, rettype, body, abi)
    }

    fn visit_new(&mut self, id: NodeID, classspec: &ClassSpec) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_class(&mut self, id: NodeID, classspec: &ClassSpec, parentspec: &Option<ClassSpec>, body: &Vec<AST>) -> Result<Self::Return, Error> {
        walk_class(self, id, classspec, parentspec, body)
    }

    fn visit_type_alias(&mut self, id: NodeID, classspec: &ClassSpec, ttype: &Type) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_enum(&mut self, id: NodeID, classspec: &ClassSpec, variants: &Vec<EnumVariant>) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }


    fn visit_import(&mut self, id: NodeID, ident: &Ident, decls: &Vec<AST>) -> Result<Self::Return, Error> {
        self.visit_vec(decls)
    }

    fn visit_definition(&mut self, id: NodeID, mutable: Mutability, ident: &Ident, ttype: &Option<Type>, expr: &AST) -> Result<Self::Return, Error> {
        self.visit_node(expr)
    }

    fn visit_assignment(&mut self, id: NodeID, left: &AST, right: &AST, ty: AssignType) -> Result<Self::Return, Error> {
        walk_assignment(self, id, left, right, ty)
    }



    fn visit_pattern_binding(&mut self, id: NodeID, ident: &Ident) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_pattern_annotation(&mut self, id: NodeID, ttype: &Type, pat: &Pattern) -> Result<Self::Return, Error> {
        self.visit_pattern(pat)
    }

    fn visit_pattern_resolve(&mut self, id: NodeID, left: &Pattern, ident: &Ident, oid: NodeID) -> Result<Self::Return, Error> {
        self.visit_pattern(left)
    }

    fn visit_pattern_enum_args(&mut self, id: NodeID, left: &Pattern, args: &Vec<Pattern>) -> Result<Self::Return, Error> {
        walk_pattern_enum_args(self, id, left, args)
    }

    fn visit_pattern_tuple(&mut self, id: NodeID, items: &Vec<Pattern>) -> Result<Self::Return, Error> {
        walk_pattern_tuple(self, id, items)
    }

    fn visit_pattern_record(&mut self, id: NodeID, items: &Vec<(Ident, Pattern)>) -> Result<Self::Return, Error> {
        walk_pattern_record(self, id, items)
    }

    fn visit_pattern_wild(&mut self) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }

    fn visit_pattern_literal(&mut self, id: NodeID, lit: &AST) -> Result<Self::Return, Error> {
        self.visit_node(lit)
    }

    fn visit_pattern_identifier(&mut self, id: NodeID, ident: &Ident) -> Result<Self::Return, Error> {
        Ok(self.default_return())
    }



    fn handle_error(&mut self, node: &AST, err: Error) -> Result<Self::Return, Error> {
        Err(err)
    }


    fn visit_vec(&mut self, code: &Vec<AST>) -> Result<Self::Return, Error> {
        let mut last: Self::Return = self.default_return();
        for node in code {
            last = match self.visit_node(node) {
                Ok(v) => v,
                Err(err) => self.handle_error(node, err)?,
            };
        }
        Ok(last)
    }

    fn visit_node(&mut self, node: &AST) -> Result<Self::Return, Error> {
        match *node {
            AST::Literal(ref id, ref lit) => {
                self.visit_literal(*id, lit)
            },

            AST::Nil(ref id) => {
                self.visit_nil(*id)
            },

            AST::PtrCast(ref id, ref ttype, ref code) => {
                self.visit_ptr_cast(*id, ttype, code)
            },

            AST::Ref(ref id, _, ref expr) => {
                self.visit_ref(*id, expr)
            },

            AST::Deref(ref id, _, ref expr) => {
                self.visit_deref(*id, expr)
            },

            AST::List(ref id, _, ref items) => {
                self.visit_list(*id, items)
            },

            AST::Tuple(ref id, _, ref items) => {
                self.visit_tuple(*id, items)
            },

            AST::Record(ref id, _, ref items) => {
                self.visit_record(*id, items)
            },

            AST::RecordUpdate(ref id, _, ref record, ref items) => {
                self.visit_record_update(*id, record, items)
            },


            AST::GetValue(ref id) => {
                self.visit_get_value(*id)
            },

            AST::Identifier(ref id, _, ref ident) => {
                self.visit_identifier(*id, ident)
            },

            AST::Index(ref id, _, ref base, ref index) => {
                // TODO commented out because it should be desugared
                //self.visit_index(*id, base, index)
                Ok(self.default_return())
            },

            AST::Resolver(ref id, _, ref left, ref right, ref oid) => {
                self.visit_resolver(*id, left, right, *oid)
            },

            AST::Accessor(ref id, _, ref left, ref right, ref oid) => {
                self.visit_accessor(*id, left, right, *oid)
            },


            AST::Block(ref id, _, ref code) => {
                self.visit_block(*id, code)
            },

            AST::Invoke(ref id, _, ref func, ref args) => {
                self.visit_invoke(*id, func, args)
            },


            AST::SideEffect(ref id, _, ref op, ref args) => {
                self.visit_side_effect(*id, op, args)
            },

            AST::If(ref id, _, ref cond, ref texpr, ref fexpr) => {
                self.visit_if(*id, cond, texpr, fexpr)
            },

            AST::Raise(ref id, _, ref expr) => {
                self.visit_raise(*id, expr)
            },

            AST::Try(ref id, _, ref cond, ref cases) => {
                self.visit_try(*id, cond, cases)
            },

            AST::Match(ref id, _, ref cond, ref cases) => {
                self.visit_match(*id, cond, cases)
            },

            AST::For(ref id, _, ref index, ref cond, ref body) => {
                // TODO commented out because it should be desugared
                //self.visit_for(*id, index, cond, body)
                Ok(self.default_return())
            },

            AST::While(ref id, _, ref cond, ref body) => {
                self.visit_while(*id, cond, body)
            },


            AST::Declare(ref id, _, ref vis, ref ident, ref ttype) => {
                self.visit_declare(*id, *vis, ident, ttype)
            },

            AST::Function(ref id, _, ref vis, ref ident, ref args, ref rettype, ref body, ref abi) => {
                self.visit_function(*id, *vis, ident, args, rettype, body, *abi)
            },

            AST::New(ref id, _, ref classspec) => {
                self.visit_new(*id, classspec)
            },

            AST::Class(ref id, _, ref classspec, ref parentspec, ref body) => {
                self.visit_class(*id, classspec, parentspec, body)
            },

            AST::TypeAlias(ref id, _, ref classspec, ref ttype) => {
                self.visit_type_alias(*id, classspec, ttype)
            },

            AST::Enum(ref id, _, ref classspec, ref variants) => {
                self.visit_enum(*id, classspec, variants)
            },


            AST::Import(ref id, _, ref ident, ref decls) => {
                self.visit_import(*id, ident, decls)
            },

            AST::Definition(ref id, _, ref mutable, ref ident, ref ttype, ref expr) => {
                self.visit_definition(*id, *mutable, ident, ttype, expr)
            },

            AST::Assignment(ref id, _, ref left, ref right, ref ty) => {
                self.visit_assignment(*id, left, right, *ty)
            },
        }
    }

    fn visit_pattern(&mut self, pat: &Pattern) -> Result<Self::Return, Error> {
        match pat {
            Pattern::Binding(ref id, ref ident) => {
                self.visit_pattern_binding(*id, ident)
            },

            Pattern::Annotation(ref id, ref ttype, ref pat) => {
                self.visit_pattern_annotation(*id, ttype, pat)
            },

            Pattern::Resolve(ref id, ref left, ref ident, ref oid) => {
                self.visit_pattern_resolve(*id, left, ident, *oid)
            },

            Pattern::EnumArgs(ref id, ref left, ref args) => {
                self.visit_pattern_enum_args(*id, left, args)
            },

            Pattern::Tuple(ref id, ref items) => {
                self.visit_pattern_tuple(*id, items)
            },

            Pattern::Record(ref id, ref items) => {
                self.visit_pattern_record(*id, items)
            },

            Pattern::Wild => {
                self.visit_pattern_wild()
            },

            Pattern::Literal(ref id, ref lit) => {
                self.visit_pattern_literal(*id, lit)
            },

            Pattern::Identifier(ref id, ref ident) => {
                self.visit_pattern_identifier(*id, ident)
            },
        }
    }

}

pub fn walk_record<R, V: Visitor<Return = R>>(visitor: &mut V, id: NodeID, items: &Vec<(Ident, AST)>) -> Result<R, Error> {
    for item in items {
        visitor.visit_node(&item.1)?;
    }
    Ok(visitor.default_return())
}

pub fn walk_record_update<R, V: Visitor<Return = R>>(visitor: &mut V, id: NodeID, record: &AST, items: &Vec<(Ident, AST)>) -> Result<R, Error> {
    visitor.visit_node(record)?;
    for item in items {
        visitor.visit_node(&item.1)?;
    }
    Ok(visitor.default_return())
}

pub fn walk_invoke<R, V: Visitor<Return = R>>(visitor: &mut V, id: NodeID, func: &AST, args: &Vec<AST>) -> Result<R, Error> {
    visitor.visit_node(func)?;
    visitor.visit_vec(args)
}

pub fn walk_if<R, V: Visitor<Return = R>>(visitor: &mut V, id: NodeID, cond: &AST, texpr: &AST, fexpr: &AST) -> Result<R, Error> {
    visitor.visit_node(cond)?;
    visitor.visit_node(texpr)?;
    visitor.visit_node(fexpr)?;
    Ok(visitor.default_return())
}

pub fn walk_try<R, V: Visitor<Return = R>>(visitor: &mut V, id: NodeID, cond: &AST, cases: &Vec<MatchCase>) -> Result<R, Error> {
    visitor.visit_node(cond)?;
    for case in cases {
        let lscope = visitor.get_scope_by_id(case.id);
        walk_match_case(visitor, lscope, case)?;
    }
    Ok(visitor.default_return())
}

pub fn walk_match<R, V: Visitor<Return = R>>(visitor: &mut V, id: NodeID, cond: &AST, cases: &Vec<MatchCase>) -> Result<R, Error> {
    visitor.visit_node(cond)?;
    for case in cases {
        let lscope = visitor.get_scope_by_id(case.id);
        walk_match_case(visitor, lscope, case)?;
    }
    Ok(visitor.default_return())
}

pub fn walk_match_case<R, V: Visitor<Return = R>>(visitor: &mut V, lscope: ScopeRef, case: &MatchCase) -> Result<R, Error> {
    visitor.with_scope(lscope, |visitor| {
        visitor.visit_pattern(&case.pat)?;
        visitor.visit_node(&case.body)?;
        Ok(visitor.default_return())
    })
}

pub fn walk_while<R, V: Visitor<Return = R>>(visitor: &mut V, id: NodeID, cond: &AST, body: &AST) -> Result<R, Error> {
    visitor.visit_node(cond)?;
    visitor.visit_node(body)?;
    Ok(visitor.default_return())
}

pub fn walk_function<R, V: Visitor<Return = R>>(visitor: &mut V, id: NodeID, vis: Visibility, ident: &Option<Ident>, args: &Vec<Argument>, rettype: &Option<Type>, body: &AST, abi: ABI) -> Result<R, Error> {
    let fscope = visitor.get_scope_by_id(id);
    // TODO visit arguments
    visitor.with_scope(fscope, |visitor| {
        visitor.visit_node(body)
    })?;
    Ok(visitor.default_return())
}

pub fn walk_class<R, V: Visitor<Return = R>>(visitor: &mut V, id: NodeID, classspec: &ClassSpec, parentspec: &Option<ClassSpec>, body: &Vec<AST>) -> Result<R, Error> {
    let tscope = visitor.get_scope_by_id(id);
    visitor.with_scope(tscope, |visitor| {
        visitor.visit_vec(body)
    })
}

pub fn walk_assignment<R, V: Visitor<Return = R>>(visitor: &mut V, id: NodeID, left: &AST, right: &AST, ty: AssignType) -> Result<R, Error> {
    visitor.visit_node(left)?;
    visitor.visit_node(right)?;
    Ok(visitor.default_return())
}

pub fn walk_pattern_enum_args<R, V: Visitor<Return = R>>(visitor: &mut V, id: NodeID, left: &Pattern, args: &Vec<Pattern>) -> Result<R, Error> {
    visitor.visit_pattern(left)?;
    for arg in args {
        visitor.visit_pattern(arg)?;
    }
    Ok(visitor.default_return())
}

pub fn walk_pattern_tuple<R, V: Visitor<Return = R>>(visitor: &mut V, id: NodeID, items: &Vec<Pattern>) -> Result<R, Error> {
    for item in items {
        visitor.visit_pattern(item)?;
    }
    Ok(visitor.default_return())
}

pub fn walk_pattern_record<R, V: Visitor<Return = R>>(visitor: &mut V, id: NodeID, items: &Vec<(Ident, Pattern)>) -> Result<R, Error> {
    for (_, item) in items {
        visitor.visit_pattern(item)?;
    }
    Ok(visitor.default_return())
}

