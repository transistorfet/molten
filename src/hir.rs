
use abi::ABI;
use types::Type;
use ast::{ Pos };
use misc::{ r, R, UniqueID };

pub type NodeID = UniqueID;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Visibility {
    Private,
    Public,
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
pub struct Ident {
    pub name: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Argument {
    pub id: NodeID,
    pub pos: Pos,
    pub ident: Ident,
    pub ttype: Option<Type>,
    pub default: Option<Expr>
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassSpec {
    pub pos: Pos,
    pub ident: Ident,
    pub types: Vec<Type>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchCase {
    pub id: NodeID,
    pub pat: Pattern,
    pub body: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumVariant {
    pub id: NodeID,
    pub pos: Pos,
    pub ident: Ident,
    pub ttype: Option<Type>,
}


#[derive(Clone, Debug, PartialEq)]
pub struct Pattern {
    pub id: NodeID,
    pub pos: Pos,
    pub kind: PatKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum PatKind {
    Wild,
    Literal(Literal),
    Binding(Ident),
    Annotation(Type, R<Pattern>),
    Identifier(Ident),
    Resolve(R<Pattern>, Ident, NodeID),
    EnumArgs(R<Pattern>, Vec<Pattern>),
    Tuple(Vec<Pattern>),
    Record(Vec<(Ident, Pattern)>),
}


#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub id: NodeID,
    pub pos: Pos,
    pub kind: ExprKind,
}

// TODO convert these to use struct variants instead of tuple variants
#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Literal(Literal),
    Nil,
    PtrCast(Type, R<Expr>),
    Ref(R<Expr>),
    Deref(R<Expr>),

    Tuple(Vec<Expr>),
    Record(Vec<(Ident, Expr)>),
    RecordUpdate(R<Expr>, Vec<(Ident, Expr)>),

    Identifier(Ident),
    Resolver(R<Expr>, Ident, NodeID),
    Accessor(R<Expr>, Ident, NodeID),

    Block(Vec<Expr>),
    Invoke(R<Expr>, Vec<Expr>, NodeID),

    SideEffect(Ident, Vec<Expr>),
    If(R<Expr>, R<Expr>, R<Expr>),
    Raise(R<Expr>),
    Try(R<Expr>, Vec<MatchCase>),
    Match(R<Expr>, Vec<MatchCase>),
    While(R<Expr>, R<Expr>),

    Declare(Visibility, Ident, Type),
    Function(Visibility, Option<Ident>, Vec<Argument>, Option<Type>, R<Expr>, ABI),
    New(ClassSpec),
    Class(ClassSpec, Option<ClassSpec>, Vec<Expr>),
    TypeAlias(ClassSpec, Type),
    Enum(ClassSpec, Vec<EnumVariant>),

    Import(Ident, Vec<Expr>),
    Definition(Mutability, Ident, Option<Type>, R<Expr>),
    Assignment(R<Expr>, R<Expr>, AssignType),
}


impl Ident {
    pub fn new(name: String) -> Self {
        Ident {
            name: name
        }
    }

    pub fn from_str(name: &str) -> Ident {
        Ident {
            name: String::from(name),
        }
    }

    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }
}


impl Argument {
    pub fn new(pos: Pos, ident: Ident, ttype: Option<Type>, default: Option<Expr>) -> Self {
        Argument {
            id: NodeID::generate(),
            pos: pos,
            ident: ident,
            ttype: ttype,
            default: default
        }
    }
}

impl ClassSpec {
    pub fn new(pos: Pos, ident: Ident, types: Vec<Type>) -> Self {
        ClassSpec {
            pos: pos,
            ident: ident,
            types: types
        }
    }
}

impl MatchCase {
    pub fn new(pat: Pattern, body: Expr) -> Self {
        Self {
            id: NodeID::generate(),
            pat: pat,
            body: body,
        }
    }
}


impl EnumVariant {
    pub fn new(pos: Pos, ident: Ident, ttype: Option<Type>) -> Self {
        Self {
            id: NodeID::generate(),
            pos: pos,
            ident: ident,
            ttype: ttype,
        }
    }
}


impl Pattern {
    pub fn new(pos: Pos, kind: PatKind) -> Self {
        Self {
            id: NodeID::generate(),
            pos: pos,
            kind: kind,
        }
    }

    pub fn get_id(&self) -> NodeID {
        self.id
    }
}

impl Expr {
    pub fn new(pos: Pos, kind: ExprKind) -> Expr {
        Expr {
            id: NodeID::generate(),
            pos: pos,
            kind: kind,
        }
    }

    pub fn new_with_id(id: NodeID, pos: Pos, kind: ExprKind) -> Expr {
        Expr {
            id: id,
            pos: pos,
            kind: kind,
        }
    }

    #[allow(dead_code)]
    pub fn get_id(&self) -> NodeID {
        self.id
    }

    #[allow(dead_code)]
    pub fn set_id(mut self, id: NodeID) -> Self {
        self.id = id;
        self
    }

    #[allow(dead_code)]
    pub fn get_pos(&self) -> Pos {
        self.pos.clone()
    }

    #[allow(dead_code)]
    pub fn set_pos(mut self, pos: Pos) -> Self {
        self.pos = pos;
        self
    }

    #[allow(dead_code)]
    pub fn make_ptr_cast(ttype: Type, expr: Expr) -> Expr {
        Expr { id: NodeID::generate(), pos: Pos::empty(), kind: ExprKind::PtrCast(ttype, r(expr)) }
    }

    #[allow(dead_code)]
    pub fn make_ref(pos: Pos, expr: Expr) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Ref(r(expr)) }
    }

    #[allow(dead_code)]
    pub fn make_deref(pos: Pos, expr: Expr) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Deref(r(expr)) }
    }

    #[allow(dead_code)]
    pub fn make_lit(literal: Literal) -> Expr {
        Expr { id: NodeID::generate(), pos: Pos::empty(), kind: ExprKind::Literal(literal) }
    }

    #[allow(dead_code)]
    pub fn make_nil() -> Expr {
        Expr { id: NodeID::generate(), pos: Pos::empty(), kind: ExprKind::Nil }
    }

    #[allow(dead_code)]
    pub fn make_tuple(pos: Pos, items: Vec<Expr>) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Tuple(items) }
    }

    #[allow(dead_code)]
    pub fn make_record(pos: Pos, items: Vec<(Ident, Expr)>) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Record(items) }
    }

    #[allow(dead_code)]
    pub fn make_record_update(pos: Pos, record: Expr, items: Vec<(Ident, Expr)>) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::RecordUpdate(r(record), items) }
    }

    #[allow(dead_code)]
    pub fn make_ident(pos: Pos, ident: Ident) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Identifier(ident) }
    }

    #[allow(dead_code)]
    pub fn make_ident_from_str(pos: Pos, name: &str) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Identifier(Ident::from_str(name)) }
    }

    #[allow(dead_code)]
    pub fn make_resolve(pos: Pos, object: Expr, ident: Ident) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Resolver(r(object), ident, NodeID::generate()) }
    }

    #[allow(dead_code)]
    pub fn make_access(pos: Pos, object: Expr, ident: Ident) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Accessor(r(object), ident, NodeID::generate()) }
    }

    #[allow(dead_code)]
    pub fn make_invoke(pos: Pos, fexpr: Expr, args: Vec<Expr>) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Invoke(r(fexpr), args, NodeID::generate()) }
    }

    #[allow(dead_code)]
    pub fn make_side_effect(pos: Pos, ident: Ident, args: Vec<Expr>) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::SideEffect(ident, args) }
    }

    #[allow(dead_code)]
    pub fn make_block(pos: Pos, body: Vec<Expr>) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Block(body) }
    }

    #[allow(dead_code)]
    pub fn make_if(pos: Pos, cond: Expr, texpr: Expr, fexpr: Expr) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::If(r(cond), r(texpr), r(fexpr)) }
    }

    #[allow(dead_code)]
    pub fn make_raise(pos: Pos, expr: Expr) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Raise(r(expr)) }
    }

    #[allow(dead_code)]
    pub fn make_try(pos: Pos, cond: Expr, cases: Vec<MatchCase>) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Try(r(cond), cases) }
    }

    #[allow(dead_code)]
    pub fn make_match(pos: Pos, cond: Expr, cases: Vec<MatchCase>) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Match(r(cond), cases) }
    }

    #[allow(dead_code)]
    pub fn make_decl(pos: Pos, vis: Visibility, ident: Ident, ttype: Type) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Declare(vis, ident, ttype) }
    }

    #[allow(dead_code)]
    pub fn make_func(pos: Pos, vis: Visibility, ident: Option<Ident>, args: Vec<Argument>, rtype: Option<Type>, body: Expr, abi: ABI) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Function(vis, ident, args, rtype, r(body), abi) }
    }

    #[allow(dead_code)]
    pub fn make_new(pos: Pos, classspec: ClassSpec) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::New(classspec) }
    }

    #[allow(dead_code)]
    pub fn make_class(pos: Pos, classspec: ClassSpec, parentspec: Option<ClassSpec>, body: Vec<Expr>) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Class(classspec, parentspec, body) }
    }

    #[allow(dead_code)]
    pub fn make_import(pos: Pos, ident: Ident, decls: Vec<Expr>) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Import(ident, decls) }
    }

    #[allow(dead_code)]
    pub fn make_def(pos: Pos, mutable: Mutability, ident: Ident, ttype: Option<Type>, value: Expr) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Definition(mutable, ident, ttype, r(value)) }
    }

    #[allow(dead_code)]
    pub fn make_assign(pos: Pos, left: Expr, right: Expr, ty: AssignType) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Assignment(r(left), r(right), ty) }
    }

    #[allow(dead_code)]
    pub fn make_while(pos: Pos, cond: Expr, body: Expr) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::While(r(cond), r(body)) }
    }

    #[allow(dead_code)]
    pub fn make_type_alias(pos: Pos, classspec: ClassSpec, ttype: Type) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::TypeAlias(classspec, ttype) }
    }

    #[allow(dead_code)]
    pub fn make_type_enum(pos: Pos, classspec: ClassSpec, variants: Vec<EnumVariant>) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Enum(classspec, variants) }
    }
}

