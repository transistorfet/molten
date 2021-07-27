
use abi::ABI;
use types::Type;
use ast::{ Pos };
use misc::{ r, R, UniqueID };

pub type NodeID = UniqueID;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Visibility {
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
    pub id: NodeID,
    pub pos: Pos,
    pub name: String,
    pub ttype: Option<Type>,
    pub default: Option<Expr>
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassSpec {
    pub pos: Pos,
    pub name: String,
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
    pub name: String,
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
    Binding(String),
    Annotation(Type, R<Pattern>),
    Identifier(String),
    Resolve(R<Pattern>, String, NodeID),
    EnumArgs(R<Pattern>, Vec<Pattern>),
    Tuple(Vec<Pattern>),
    Record(Vec<(String, Pattern)>),
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
    Ref(R<Expr>),
    Deref(R<Expr>),
    Annotation(Type, R<Expr>),

    Tuple(Vec<Expr>),
    Record(Vec<(String, Expr)>),
    RecordUpdate(R<Expr>, Vec<(String, Expr)>),

    Identifier(String),
    Resolver(R<Expr>, String, NodeID),
    Accessor(R<Expr>, String, NodeID),

    Block(Vec<Expr>),
    Invoke(R<Expr>, Vec<Expr>, NodeID),

    SideEffect(String, Vec<Expr>),
    If(R<Expr>, R<Expr>, R<Expr>),
    Raise(R<Expr>),
    Try(R<Expr>, Vec<MatchCase>),
    Match(R<Expr>, Vec<MatchCase>),
    While(R<Expr>, R<Expr>),

    Declare(Visibility, String, Type),
    Function(Visibility, Option<String>, Vec<Argument>, Option<Type>, Vec<Expr>, ABI),
    AllocObject(Type),
    Class(ClassSpec, Option<ClassSpec>, Vec<Expr>),
    TypeAlias(ClassSpec, Type),
    Enum(ClassSpec, Vec<EnumVariant>),
    TraitDef(ClassSpec, Vec<Expr>),
    TraitImpl(ClassSpec, Type, Vec<Expr>),
    UnpackTraitObject(Type, R<Expr>),

    Import(String, Vec<Expr>),
    Definition(Mutability, String, Option<Type>, R<Expr>),
    Assignment(R<Expr>, R<Expr>, AssignType),

    Module(String, Vec<Expr>, NodeID),
}

impl Argument {
    pub fn new(pos: Pos, name: String, ttype: Option<Type>, default: Option<Expr>) -> Self {
        Argument {
            id: NodeID::generate(),
            pos: pos,
            name: name,
            ttype: ttype,
            default: default
        }
    }
}

impl ClassSpec {
    pub fn new(pos: Pos, name: String, types: Vec<Type>) -> Self {
        ClassSpec {
            pos: pos,
            name: name,
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
    pub fn new(pos: Pos, name: String, ttype: Option<Type>) -> Self {
        Self {
            id: NodeID::generate(),
            pos: pos,
            name: name,
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

    #[allow(dead_code)]
    pub fn make_lit(literal: Literal) -> Pattern {
        Pattern { id: NodeID::generate(), pos: Pos::empty(), kind: PatKind::Literal(literal) }
    }
}

impl Expr {
    #[allow(dead_code)]
    pub fn new(pos: Pos, kind: ExprKind) -> Expr {
        Expr {
            id: NodeID::generate(),
            pos: pos,
            kind: kind,
        }
    }

    #[allow(dead_code)]
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
        self.pos
    }

    #[allow(dead_code)]
    pub fn set_pos(mut self, pos: Pos) -> Self {
        self.pos = pos;
        self
    }

    #[allow(dead_code)]
    pub fn make_annotation(ttype: Type, expr: Expr) -> Expr {
        Expr { id: NodeID::generate(), pos: Pos::empty(), kind: ExprKind::Annotation(ttype, r(expr)) }
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
    pub fn make_record(pos: Pos, items: Vec<(String, Expr)>) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Record(items) }
    }

    #[allow(dead_code)]
    pub fn make_record_update(pos: Pos, record: Expr, items: Vec<(String, Expr)>) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::RecordUpdate(r(record), items) }
    }

    #[allow(dead_code)]
    pub fn make_ident(pos: Pos, ident: String) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Identifier(ident) }
    }

    #[allow(dead_code)]
    pub fn make_ident_from_str(pos: Pos, name: &str) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Identifier(name.to_string()) }
    }

    #[allow(dead_code)]
    pub fn make_resolve(pos: Pos, object: Expr, ident: String) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Resolver(r(object), ident, NodeID::generate()) }
    }

    #[allow(dead_code)]
    pub fn make_access(pos: Pos, object: Expr, ident: String) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Accessor(r(object), ident, NodeID::generate()) }
    }

    #[allow(dead_code)]
    pub fn make_invoke(pos: Pos, fexpr: Expr, args: Vec<Expr>) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Invoke(r(fexpr), args, NodeID::generate()) }
    }

    #[allow(dead_code)]
    pub fn make_side_effect(pos: Pos, ident: String, args: Vec<Expr>) -> Expr {
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
    pub fn make_decl(pos: Pos, vis: Visibility, ident: String, ttype: Type) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Declare(vis, ident, ttype) }
    }

    #[allow(dead_code)]
    pub fn make_func(pos: Pos, vis: Visibility, ident: Option<String>, args: Vec<Argument>, rtype: Option<Type>, body: Vec<Expr>, abi: ABI) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Function(vis, ident, args, rtype, body, abi) }
    }

    #[allow(dead_code)]
    pub fn make_alloc_object(pos: Pos, ttype: Type) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::AllocObject(ttype) }
    }

    #[allow(dead_code)]
    pub fn make_class(pos: Pos, classspec: ClassSpec, parentspec: Option<ClassSpec>, body: Vec<Expr>) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Class(classspec, parentspec, body) }
    }

    #[allow(dead_code)]
    pub fn make_import(pos: Pos, ident: String, decls: Vec<Expr>) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Import(ident, decls) }
    }

    #[allow(dead_code)]
    pub fn make_def(pos: Pos, mutable: Mutability, ident: String, ttype: Option<Type>, value: Expr) -> Expr {
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
    pub fn make_enum(pos: Pos, classspec: ClassSpec, variants: Vec<EnumVariant>) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::Enum(classspec, variants) }
    }

    #[allow(dead_code)]
    pub fn make_trait_def(pos: Pos, traitspec: ClassSpec, body: Vec<Expr>) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::TraitDef(traitspec, body) }
    }

    #[allow(dead_code)]
    pub fn make_trait_impl(pos: Pos, traitspec: ClassSpec, impltype: Type, body: Vec<Expr>) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::TraitImpl(traitspec, impltype, body) }
    }

    #[allow(dead_code)]
    pub fn make_unpack_trait_obj(pos: Pos, impltype: Type, node: Expr) -> Expr {
        Expr { id: NodeID::generate(), pos: pos, kind: ExprKind::UnpackTraitObject(impltype, r(node)) }
    }

    #[allow(dead_code)]
    pub fn make_module(name: String, code: Vec<Expr>) -> Expr {
        Expr { id: NodeID::generate(), pos: Pos::empty(), kind: ExprKind::Module(name, code, NodeID::generate()) }
    }


    #[allow(dead_code)]
    pub fn make_resolve_ident(pos: Pos, object: &String, name: &str) -> Expr {
        Expr::make_resolve(pos, Expr::make_ident(pos, object.clone()), name.to_string())
    }
}

