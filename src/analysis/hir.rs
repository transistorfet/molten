
use crate::abi::ABI;
use crate::types::Type;
use crate::parsing::ast::{ Pos };
use crate::misc::{ r, R, UniqueID };

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Visibility {
    Anonymous,
    Private,
    Public,
    Global,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Mutability {
    Immutable,
    Mutable,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AssignType {
    Initialize,
    Update,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Unit,
    Boolean(bool),
    Character(char),
    Integer(isize),
    Real(f64),
    String(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Argument {
    pub id: UniqueID,
    pub pos: Pos,
    pub name: String,
    pub ttype: Option<Type>,
    //pub default: Option<Expr>
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchCase {
    pub id: UniqueID,
    pub pat: Pattern,
    pub body: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumVariant {
    pub id: UniqueID,
    pub pos: Pos,
    pub name: String,
    pub ttype: Option<Type>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhereClause {
    pub pos: Pos,
    pub constraints: Vec<(String, String)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub vis: Visibility,
    pub name: String,
    pub args: Vec<Argument>,
    pub rettype: Option<Type>,
    pub body: Vec<Expr>,
    pub abi: ABI,
    pub whereclause: WhereClause,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Pattern {
    pub id: UniqueID,
    pub pos: Pos,
    pub kind: PatKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum PatKind {
    Wild,
    Literal(Literal, UniqueID),
    Binding(String),
    Annotation(Type, R<Pattern>),
    PackUniversal(Type, R<Pattern>),
    UnpackUniversal(Type, R<Pattern>),
    EnumVariant(Vec<String>, Vec<Pattern>, UniqueID),
    Ref(R<Pattern>),
    Tuple(Vec<Pattern>),
    Record(Vec<(String, Pattern)>),
}


#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub id: UniqueID,
    pub pos: Pos,
    pub kind: ExprKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Literal(Literal),
    Ref(R<Expr>),
    Deref(R<Expr>),
    Annotation(Type, R<Expr>),
    PackUniversal(Type, R<Expr>),
    UnpackUniversal(Type, R<Expr>),

    Tuple(Vec<Expr>),
    Record(Vec<(String, Expr)>),
    RecordUpdate(R<Expr>, Vec<(String, Expr)>),

    Identifier(String),
    Resolver(R<Expr>, String, UniqueID),
    Accessor(R<Expr>, String, UniqueID),

    Block(Vec<Expr>),
    Invoke(R<Expr>, Vec<Expr>, UniqueID),

    SideEffect(String, Vec<Expr>),
    If(R<Expr>, R<Expr>, R<Expr>),
    Raise(R<Expr>),
    Try(R<Expr>, Vec<MatchCase>),
    Match(R<Expr>, Vec<MatchCase>),
    While(R<Expr>, R<Expr>),

    Declare(Visibility, String, Type, WhereClause),
    Function(Function),
    AllocObject(Type),
    Class(Type, Option<Type>, WhereClause, Vec<Expr>),
    TypeAlias(Type, Type),
    Enum(Type, WhereClause, Vec<EnumVariant>),
    TraitDef(String, Vec<Expr>),
    TraitImpl(String, Type, WhereClause, Vec<Expr>),
    Methods(Type, WhereClause, Vec<Expr>),

    Import(String, Vec<Expr>),
    Definition(Mutability, String, Option<Type>, R<Expr>),
    Field(Mutability, String, Option<Type>),
    Assignment(R<Expr>, R<Expr>, AssignType),

    Module(String, R<Expr>, UniqueID),
    ModuleDecl(String),
}

impl Argument {
    pub fn new(pos: Pos, name: String, ttype: Option<Type>) -> Self {
        Argument {
            id: UniqueID::generate(),
            pos,
            name,
            ttype,
            //default,
        }
    }
}

impl MatchCase {
    pub fn new(pat: Pattern, body: Expr) -> Self {
        Self {
            id: UniqueID::generate(),
            pat,
            body,
        }
    }
}


impl EnumVariant {
    pub fn new(pos: Pos, name: String, ttype: Option<Type>) -> Self {
        Self {
            id: UniqueID::generate(),
            pos,
            name,
            ttype,
        }
    }
}

impl WhereClause {
    pub fn new(pos: Pos, constraints: Vec<(String, String)>) -> Self {
        WhereClause {
            pos,
            constraints,
        }
    }

    pub fn empty() -> Self {
        WhereClause {
            pos: Pos::empty(),
            constraints: vec!(),
        }
    }
}

impl Function {
    pub fn new(vis: Visibility, name: String, args: Vec<Argument>, rettype: Option<Type>, body: Vec<Expr>, abi: ABI, whereclause: WhereClause) -> Function {
        Function {
            vis,
            name,
            args,
            rettype,
            body,
            abi,
            whereclause
        }
    }
}

impl Pattern {
    #[allow(dead_code)]
    pub fn new(pos: Pos, kind: PatKind) -> Self {
        Self {
            id: UniqueID::generate(),
            pos,
            kind,
        }
    }

    #[allow(dead_code)]
    pub fn make_wild() -> Pattern {
        Pattern { id: UniqueID::generate(), pos: Pos::empty(), kind: PatKind::Wild }
    }

    #[allow(dead_code)]
    pub fn make_lit(pos: Pos, literal: Literal) -> Pattern {
        Pattern { id: UniqueID::generate(), pos, kind: PatKind::Literal(literal, UniqueID::generate()) }
    }

    #[allow(dead_code)]
    pub fn make_binding(pos: Pos, name: String) -> Pattern {
        Pattern { id: UniqueID::generate(), pos, kind: PatKind::Binding(name) }
    }

    #[allow(dead_code)]
    pub fn make_annotation(pos: Pos, ttype: Type, pat: Pattern) -> Pattern {
        Pattern { id: UniqueID::generate(), pos, kind: PatKind::Annotation(ttype, r(pat)) }
    }

    #[allow(dead_code)]
    pub fn make_pack_universal(pos: Pos, ttype: Type, pat: Pattern) -> Pattern {
        Pattern { id: UniqueID::generate(), pos, kind: PatKind::PackUniversal(ttype, r(pat)) }
    }

    #[allow(dead_code)]
    pub fn make_unpack_universal(pos: Pos, ttype: Type, pat: Pattern) -> Pattern {
        Pattern { id: UniqueID::generate(), pos, kind: PatKind::UnpackUniversal(ttype, r(pat)) }
    }

    #[allow(dead_code)]
    pub fn make_enum_variant(pos: Pos, path: Vec<String>, args: Vec<Pattern>) -> Pattern {
        Pattern { id: UniqueID::generate(), pos, kind: PatKind::EnumVariant(path, args, UniqueID::generate()) }
    }

    #[allow(dead_code)]
    pub fn make_ref(pos: Pos, subpat: Pattern) -> Pattern {
        Pattern { id: UniqueID::generate(), pos, kind: PatKind::Ref(r(subpat)) }
    }

    #[allow(dead_code)]
    pub fn make_tuple(pos: Pos, items: Vec<Pattern>) -> Pattern {
        Pattern { id: UniqueID::generate(), pos, kind: PatKind::Tuple(items) }
    }

    #[allow(dead_code)]
    pub fn make_record(pos: Pos, items: Vec<(String, Pattern)>) -> Pattern {
        Pattern { id: UniqueID::generate(), pos, kind: PatKind::Record(items) }
    }
}

impl Expr {
    #[allow(dead_code)]
    pub fn new(pos: Pos, kind: ExprKind) -> Expr {
        Expr {
            id: UniqueID::generate(),
            pos,
            kind,
        }
    }

    #[allow(dead_code)]
    pub fn new_with_id(id: UniqueID, pos: Pos, kind: ExprKind) -> Expr {
        Expr {
            id,
            pos,
            kind,
        }
    }

    #[allow(dead_code)]
    pub fn get_pos(&self) -> Pos {
        self.pos
    }

    #[allow(dead_code)]
    pub fn set_pos(mut self, pos: Pos) -> Self {
        self.pos = pos;
        self
    }

    #[allow(dead_code)]
    pub fn make_annotation(ttype: Type, expr: Expr) -> Expr {
        Expr { id: UniqueID::generate(), pos: expr.pos.clone(), kind: ExprKind::Annotation(ttype, r(expr)) }
    }

    #[allow(dead_code)]
    pub fn make_pack_universal(utype: Type, expr: Expr) -> Expr {
        Expr { id: UniqueID::generate(), pos: expr.pos.clone(), kind: ExprKind::PackUniversal(utype, r(expr)) }
    }

    #[allow(dead_code)]
    pub fn make_unpack_universal(utype: Type, expr: Expr) -> Expr {
        Expr { id: UniqueID::generate(), pos: expr.pos.clone(), kind: ExprKind::UnpackUniversal(utype, r(expr)) }
    }

    #[allow(dead_code)]
    pub fn make_ref(pos: Pos, expr: Expr) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::Ref(r(expr)) }
    }

    #[allow(dead_code)]
    pub fn make_deref(pos: Pos, expr: Expr) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::Deref(r(expr)) }
    }

    #[allow(dead_code)]
    pub fn make_lit(literal: Literal) -> Expr {
        Expr { id: UniqueID::generate(), pos: Pos::empty(), kind: ExprKind::Literal(literal) }
    }

    #[allow(dead_code)]
    pub fn make_tuple(pos: Pos, items: Vec<Expr>) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::Tuple(items) }
    }

    #[allow(dead_code)]
    pub fn make_record(pos: Pos, items: Vec<(String, Expr)>) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::Record(items) }
    }

    #[allow(dead_code)]
    pub fn make_record_update(pos: Pos, record: Expr, items: Vec<(String, Expr)>) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::RecordUpdate(r(record), items) }
    }

    #[allow(dead_code)]
    pub fn make_ident(pos: Pos, ident: String) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::Identifier(ident) }
    }

    #[allow(dead_code)]
    pub fn make_ident_from_str(pos: Pos, name: &str) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::Identifier(name.to_string()) }
    }

    #[allow(dead_code)]
    pub fn make_resolve(pos: Pos, object: Expr, ident: String) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::Resolver(r(object), ident, UniqueID::generate()) }
    }

    #[allow(dead_code)]
    pub fn make_access(pos: Pos, object: Expr, ident: String) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::Accessor(r(object), ident, UniqueID::generate()) }
    }

    #[allow(dead_code)]
    pub fn make_invoke(pos: Pos, fexpr: Expr, args: Vec<Expr>) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::Invoke(r(fexpr), args, UniqueID::generate()) }
    }

    #[allow(dead_code)]
    pub fn make_side_effect(pos: Pos, ident: String, args: Vec<Expr>) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::SideEffect(ident, args) }
    }

    #[allow(dead_code)]
    pub fn make_block(pos: Pos, body: Vec<Expr>) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::Block(body) }
    }

    #[allow(dead_code)]
    pub fn make_if(pos: Pos, cond: Expr, texpr: Expr, fexpr: Expr) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::If(r(cond), r(texpr), r(fexpr)) }
    }

    #[allow(dead_code)]
    pub fn make_raise(pos: Pos, expr: Expr) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::Raise(r(expr)) }
    }

    #[allow(dead_code)]
    pub fn make_try(pos: Pos, cond: Expr, cases: Vec<MatchCase>) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::Try(r(cond), cases) }
    }

    #[allow(dead_code)]
    pub fn make_match(pos: Pos, cond: Expr, cases: Vec<MatchCase>) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::Match(r(cond), cases) }
    }

    #[allow(dead_code)]
    pub fn make_decl(pos: Pos, vis: Visibility, ident: String, ttype: Type, whereclause: WhereClause) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::Declare(vis, ident, ttype, whereclause) }
    }

    #[allow(dead_code)]
    pub fn make_func(pos: Pos, func: Function) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::Function(func) }
    }

    #[allow(dead_code)]
    pub fn make_alloc_object(pos: Pos, ttype: Type) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::AllocObject(ttype) }
    }

    #[allow(dead_code)]
    pub fn make_class(pos: Pos, classtype: Type, parenttype: Option<Type>, whereclause: WhereClause, body: Vec<Expr>) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::Class(classtype, parenttype, whereclause, body) }
    }

    #[allow(dead_code)]
    pub fn make_methods(pos: Pos, ttype: Type, whereclause: WhereClause, body: Vec<Expr>) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::Methods(ttype, whereclause, body) }
    }

    #[allow(dead_code)]
    pub fn make_import(pos: Pos, ident: String, decls: Vec<Expr>) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::Import(ident, decls) }
    }

    #[allow(dead_code)]
    pub fn make_def(pos: Pos, mutable: Mutability, ident: String, ttype: Option<Type>, value: Expr) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::Definition(mutable, ident, ttype, r(value)) }
    }

    #[allow(dead_code)]
    pub fn make_field(pos: Pos, mutable: Mutability, ident: String, ttype: Option<Type>) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::Field(mutable, ident, ttype) }
    }

    #[allow(dead_code)]
    pub fn make_assign(pos: Pos, left: Expr, right: Expr, ty: AssignType) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::Assignment(r(left), r(right), ty) }
    }

    #[allow(dead_code)]
    pub fn make_while(pos: Pos, cond: Expr, body: Expr) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::While(r(cond), r(body)) }
    }

    #[allow(dead_code)]
    pub fn make_type_alias(pos: Pos, deftype: Type, ttype: Type) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::TypeAlias(deftype, ttype) }
    }

    #[allow(dead_code)]
    pub fn make_enum(pos: Pos, enumtype: Type, whereclause: WhereClause, variants: Vec<EnumVariant>) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::Enum(enumtype, whereclause, variants) }
    }

    #[allow(dead_code)]
    pub fn make_trait_def(pos: Pos, traitname: String, body: Vec<Expr>) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::TraitDef(traitname, body) }
    }

    #[allow(dead_code)]
    pub fn make_trait_impl(pos: Pos, traitname: String, impltype: Type, whereclause: WhereClause, body: Vec<Expr>) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::TraitImpl(traitname, impltype, whereclause, body) }
    }

    #[allow(dead_code)]
    pub fn make_module(name: String, code: Expr) -> Expr {
        Expr { id: UniqueID::generate(), pos: Pos::empty(), kind: ExprKind::Module(name, r(code), UniqueID::generate()) }
    }

    #[allow(dead_code)]
    pub fn make_module_decl(pos: Pos, name: String) -> Expr {
        Expr { id: UniqueID::generate(), pos, kind: ExprKind::ModuleDecl(name) }
    }


    #[allow(dead_code)]
    pub fn make_resolve_ident(pos: Pos, object: &str, name: &str) -> Expr {
        Expr::make_resolve(pos, Expr::make_ident(pos, object.to_string()), name.to_string())
    }
}

