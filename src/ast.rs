
use std::fmt;
use std::str;

use abi::ABI;
use types::Type;
use parser::Span;
use misc::{ R, r, UniqueID };


pub type NodeID = UniqueID;

#[derive(Clone, PartialEq)]
pub struct Pos {
    pub offset: usize,
    pub column: usize,
    pub line: u32,
    pub filenum: u16,
}

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

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Unit,
    Boolean(bool),
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
    pub default: Option<AST>
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassSpec {
    pub pos: Pos,
    pub ident: Ident,
    pub types: Vec<Type>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Field {
    pub id: NodeID,
    pub pos: Pos,
    pub ident: Ident,
    pub ttype: Option<Type>
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchCase {
    pub id: NodeID,
    pub pat: Pattern,
    pub body: AST,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Wild,
    Literal(NodeID, AST),
    Binding(NodeID, Ident),
}


#[derive(Clone, Debug, PartialEq)]
pub enum AST {
    Literal(NodeID, Literal),
    Nil(NodeID),
    PtrCast(Type, R<AST>),
    Ref(NodeID, Pos, R<AST>),
    Deref(NodeID, Pos, R<AST>),

    Tuple(NodeID, Pos, Vec<AST>),
    Record(NodeID, Pos, Vec<(Ident, AST)>),
    List(NodeID, Pos, Vec<AST>),

    GetValue(NodeID),
    Identifier(NodeID, Pos, Ident),
    Index(NodeID, Pos, R<AST>, R<AST>),
    Resolver(NodeID, Pos, R<AST>, Ident, NodeID),
    Accessor(NodeID, Pos, R<AST>, Ident, NodeID),

    Block(NodeID, Pos, Vec<AST>),
    Invoke(NodeID, Pos, R<AST>, Vec<AST>),
    //Prefix(NodeID, Pos, Ident, R<AST>),
    //Infix(NodeID, Pos, Ident, R<AST>, R<AST>),

    SideEffect(NodeID, Pos, Ident, Vec<AST>),
    If(NodeID, Pos, R<AST>, R<AST>, R<AST>),
    Raise(NodeID, Pos, R<AST>),
    Try(NodeID, Pos, R<AST>, Vec<MatchCase>),
    Match(NodeID, Pos, R<AST>, Vec<MatchCase>),
    For(NodeID, Pos, Ident, R<AST>, R<AST>),
    While(NodeID, Pos, R<AST>, R<AST>),

    Declare(NodeID, Pos, Visibility, Ident, Type),
    Function(NodeID, Pos, Visibility, Option<Ident>, Vec<Argument>, Option<Type>, R<AST>, ABI),
    New(NodeID, Pos, ClassSpec),
    Class(NodeID, Pos, ClassSpec, Option<ClassSpec>, Vec<AST>),
    TypeAlias(NodeID, Pos, ClassSpec, Type),

    Import(NodeID, Pos, Ident, Vec<AST>),
    Definition(NodeID, Pos, Mutability, Ident, Option<Type>, R<AST>),
    Assignment(NodeID, Pos, R<AST>, R<AST>),
}



impl Pos {
    pub fn new(span: Span) -> Pos {
        Pos {
            offset: span.offset,
            column: span.get_utf8_column(),
            line: span.line,
            filenum: 0,
        }
    }

    pub fn empty() -> Pos {
        Pos { offset: 0, column: 0, line: 0, filenum: 0 }
    }

    pub fn exerpt(&self, text: &[u8]) -> String {
        let mut end = self.offset + 1;
        for i in self.offset .. text.len() {
            if text[i] == b'\n' {
                end = i;
                break;
            }
        }
        String::from(str::from_utf8(&text[self.offset .. end]).unwrap())
    }
}

impl fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
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

    pub fn from_span(span: Span) -> Ident {
        Ident {
            name: String::from(str::from_utf8(&span.fragment).unwrap()),
        }
    }

    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }
}

impl Argument {
    pub fn new(pos: Pos, ident: Ident, ttype: Option<Type>, default: Option<AST>) -> Self {
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

    pub fn from_str(name: &str) -> Self {
        Self::new(Pos::empty(), Ident::from_str(name), vec!())
    }
}

impl Field {
    pub fn new(pos: Pos, ident: Ident, ttype: Option<Type>) -> Self {
        Self {
            id: NodeID::generate(),
            pos: pos,
            ident: ident,
            ttype: ttype
        }
    }
}

impl MatchCase {
    pub fn new(pat: Pattern, body: AST) -> Self {
        Self {
            id: NodeID::generate(),
            pat: pat,
            body: body,
        }
    }
}


impl AST {
    pub fn get_pos(&self) -> Pos {
        match *self {
            AST::Ref(_, ref pos, _) |
            AST::Deref(_, ref pos, _) |
            AST::Tuple(_, ref pos, _) |
            AST::Record(_, ref pos, _) |
            AST::List(_, ref pos, _) |
            AST::Identifier(_, ref pos, _) |
            AST::Index(_, ref pos, _, _) |
            AST::Resolver(_, ref pos, _, _, _) |
            AST::Accessor(_, ref pos, _, _, _) |
            AST::Invoke(_, ref pos, _, _) |
            AST::SideEffect(_, ref pos, _, _) |
            AST::Block(_, ref pos, _) |
            AST::If(_, ref pos, _, _, _) |
            AST::Raise(_, ref pos, _) |
            AST::Try(_, ref pos, _, _) |
            AST::Match(_, ref pos, _, _) |
            AST::For(_, ref pos, _, _, _) |
            AST::Declare(_, ref pos, _, _, _) |
            AST::Function(_, ref pos, _, _, _, _, _, _) |
            AST::New(_, ref pos, _) |
            AST::Class(_, ref pos, _, _, _) |
            AST::Import(_, ref pos, _, _) |
            AST::Definition(_, ref pos, _, _, _, _) |
            AST::Assignment(_, ref pos, _, _) |
            AST::While(_, ref pos, _, _) |
            AST::TypeAlias(_, ref pos, _, _) => { pos.clone() }
            _ => Pos::empty(),
        }
    }

    pub fn get_id(&self) -> NodeID {
        match *self {
            AST::Literal(ref id, _) |
            AST::Nil(ref id) |
            AST::Ref(ref id, _, _) |
            AST::Deref(ref id, _, _) |
            AST::Tuple(ref id, _, _) |
            AST::Record(ref id, _, _) |
            AST::List(ref id, _, _) |
            AST::Identifier(ref id, _, _) |
            AST::Index(ref id, _, _, _) |
            AST::Resolver(ref id, _, _, _, _) |
            AST::Accessor(ref id, _, _, _, _) |
            AST::Invoke(ref id, _, _, _) |
            AST::SideEffect(ref id, _, _, _) |
            AST::Block(ref id, _, _) |
            AST::If(ref id, _, _, _, _) |
            AST::Raise(ref id, _, _) |
            AST::Try(ref id, _, _, _) |
            AST::Match(ref id, _, _, _) |
            AST::For(ref id, _, _, _, _) |
            AST::Declare(ref id, _, _, _, _) |
            AST::Function(ref id, _, _, _, _, _, _, _) |
            AST::New(ref id, _, _) |
            AST::Class(ref id, _, _, _, _) |
            AST::Import(ref id, _, _, _) |
            AST::Definition(ref id, _, _, _, _, _) |
            AST::Assignment(ref id, _, _, _) |
            AST::While(ref id, _, _, _) |
            AST::TypeAlias(ref id, _, _, _) => { *id }
            _ => UniqueID(0),
        }
    }

    pub fn make_ref(pos: Pos, expr: AST) -> AST {
        AST::Ref(NodeID::generate(), pos, r(expr))
    }

    pub fn make_deref(pos: Pos, expr: AST) -> AST {
        AST::Deref(NodeID::generate(), pos, r(expr))
    }

    pub fn make_lit(literal: Literal) -> AST {
        AST::Literal(NodeID::generate(), literal)
    }

    pub fn make_nil() -> AST {
        AST::Nil(NodeID::generate())
    }

    pub fn make_tuple(pos: Pos, items: Vec<AST>) -> AST {
        AST::Tuple(NodeID::generate(), pos, items)
    }

    pub fn make_record(pos: Pos, items: Vec<(Ident, AST)>) -> AST {
        AST::Record(NodeID::generate(), pos, items)
    }

    pub fn make_list(pos: Pos, items: Vec<AST>) -> AST {
        AST::List(NodeID::generate(), pos, items)
    }

    pub fn make_ident(pos: Pos, ident: Ident) -> AST {
        AST::Identifier(NodeID::generate(), pos, ident)
    }

    pub fn make_ident_from_str(pos: Pos, name: &str) -> AST {
        AST::Identifier(NodeID::generate(), pos, Ident::from_str(name))
    }

    pub fn make_index(pos: Pos, list: AST, index: AST) -> AST {
        AST::Index(NodeID::generate(), pos, r(list), r(index))
    }

    pub fn make_resolve(pos: Pos, object: AST, ident: Ident) -> AST {
        AST::Resolver(NodeID::generate(), pos, r(object), ident, NodeID::generate())
    }

    pub fn make_access(pos: Pos, object: AST, ident: Ident) -> AST {
        AST::Accessor(NodeID::generate(), pos, r(object), ident, NodeID::generate())
    }

    pub fn make_invoke(pos: Pos, fexpr: AST, args: Vec<AST>) -> AST {
        AST::Invoke(NodeID::generate(), pos, r(fexpr), args)
    }

    pub fn make_side_effect(pos: Pos, ident: Ident, args: Vec<AST>) -> AST {
        AST::SideEffect(NodeID::generate(), pos, ident, args)
    }

    pub fn make_block(pos: Pos, body: Vec<AST>) -> AST {
        AST::Block(NodeID::generate(), pos, body)
    }

    pub fn make_if(pos: Pos, cond: AST, texpr: AST, fexpr: AST) -> AST {
        AST::If(NodeID::generate(), pos, r(cond), r(texpr), r(fexpr))
    }

    pub fn make_raise(pos: Pos, expr: AST) -> AST {
        AST::Raise(NodeID::generate(), pos, r(expr))
    }

    pub fn make_try(pos: Pos, cond: AST, cases: Vec<MatchCase>) -> AST {
        AST::Try(NodeID::generate(), pos, r(cond), cases)
    }

    pub fn make_match(pos: Pos, cond: AST, cases: Vec<MatchCase>) -> AST {
        AST::Match(NodeID::generate(), pos, r(cond), cases)
    }

    pub fn make_for(pos: Pos, ident: Ident, list: AST, body: AST) -> AST {
        AST::For(NodeID::generate(), pos, ident, r(list), r(body))
    }

    pub fn make_decl(pos: Pos, vis: Visibility, ident: Ident, ttype: Type) -> AST {
        AST::Declare(NodeID::generate(), pos, vis, ident, ttype)
    }

    pub fn make_func(pos: Pos, vis: Visibility, ident: Option<Ident>, args: Vec<Argument>, rtype: Option<Type>, body: AST, abi: ABI) -> AST {
        AST::Function(NodeID::generate(), pos, vis, ident, args, rtype, r(body), abi)
    }

    pub fn make_new(pos: Pos, classspec: ClassSpec) -> AST {
        AST::New(NodeID::generate(), pos, classspec)
    }

    pub fn make_class(pos: Pos, classspec: ClassSpec, parentspec: Option<ClassSpec>, body: Vec<AST>) -> AST {
        AST::Class(NodeID::generate(), pos, classspec, parentspec, body)
    }

    pub fn make_import(pos: Pos, ident: Ident, decls: Vec<AST>) -> AST {
        AST::Import(NodeID::generate(), pos, ident, decls)
    }

    pub fn make_def(pos: Pos, mutable: Mutability, ident: Ident, ttype: Option<Type>, value: AST) -> AST {
        AST::Definition(NodeID::generate(), pos, mutable, ident, ttype, r(value))
    }

    pub fn make_assign(pos: Pos, left: AST, right: AST) -> AST {
        AST::Assignment(NodeID::generate(), pos, r(left), r(right))
    }

    pub fn make_while(pos: Pos, cond: AST, body: AST) -> AST {
        AST::While(NodeID::generate(), pos, r(cond), r(body))
    }

    pub fn make_type_alias(pos: Pos, classspec: ClassSpec, ttype: Type) -> AST {
        AST::TypeAlias(NodeID::generate(), pos, classspec, ttype)
    }

    /*
    pub fn set_id(self, id: NodeID) -> Self {
        self.id = id;
        self
    }

    pub fn set_pos(self, pos: Pos) -> Self {
        self.pos = pos;
        self
    }

    pub fn set_span(self, span: Span) -> Self {
        self.pos = Pos::new(span);
        self
    }

    pub fn mk_ident(ident: Ident) -> Self {
        AST::Identifier(Pos::empty(), ident);
        AST {
            id: UniqueID::generate(),
            pos: Pos::empty(),
            kind: ASTKind::Ident(ident),
        }
    }
    */
}

