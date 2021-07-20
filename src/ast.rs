
use std::fmt;
use std::str;

use abi::ABI;
use types::Type;
use parser::Span;
use misc::{ R, r };
use hir::{ Visibility, Mutability, AssignType, Literal, Ident, Argument, ClassSpec, Pattern };


#[derive(Clone, PartialEq)]
pub struct Pos {
    pub offset: usize,
    pub column: usize,
    pub line: u32,
    pub filenum: u16,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AST {
    Literal(Literal),
    Nil,
    PtrCast(Type, R<AST>),
    Ref(Pos, R<AST>),
    Deref(Pos, R<AST>),

    List(Pos, Vec<AST>),
    Tuple(Pos, Vec<AST>),
    Record(Pos, Vec<(Ident, AST)>),
    RecordUpdate(Pos, R<AST>, Vec<(Ident, AST)>),

    Identifier(Pos, Ident),
    Index(Pos, R<AST>, R<AST>),
    Resolver(Pos, R<AST>, Ident),
    Accessor(Pos, R<AST>, Ident),

    Block(Pos, Vec<AST>),
    Invoke(Pos, R<AST>, Vec<AST>),
    //Prefix(Pos, Ident, R<AST>),
    //Infix(Pos, Ident, R<AST>, R<AST>),

    SideEffect(Pos, Ident, Vec<AST>),
    If(Pos, R<AST>, R<AST>, R<AST>),
    Raise(Pos, R<AST>),
    Try(Pos, R<AST>, Vec<(Pattern, AST)>),
    Match(Pos, R<AST>, Vec<(Pattern, AST)>),
    For(Pos, Ident, R<AST>, R<AST>),
    While(Pos, R<AST>, R<AST>),

    Declare(Pos, Visibility, Ident, Type),
    Function(Pos, Visibility, Option<Ident>, Vec<Argument>, Option<Type>, Vec<AST>, ABI),
    New(Pos, ClassSpec),
    Class(Pos, ClassSpec, Option<ClassSpec>, Vec<AST>),
    TypeAlias(Pos, ClassSpec, Type),
    Enum(Pos, ClassSpec, Vec<(Pos, Ident, Option<Type>)>),

    Import(Pos, Ident, Vec<AST>),
    Definition(Pos, Mutability, Ident, Option<Type>, R<AST>),
    Assignment(Pos, R<AST>, R<AST>, AssignType),
}


impl Pos {
    pub fn new(span: Span) -> Pos {
        Pos {
            offset: span.location_offset(),
            column: span.get_utf8_column(),
            line: span.location_line(),
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


impl AST {
    pub fn get_pos(&self) -> Pos {
        match *self {
            AST::Ref(ref pos, _) |
            AST::Deref(ref pos, _) |
            AST::List(ref pos, _) |
            AST::Tuple(ref pos, _) |
            AST::Record(ref pos, _) |
            AST::RecordUpdate(ref pos, _, _) |
            AST::Identifier(ref pos, _) |
            AST::Index(ref pos, _, _) |
            AST::Resolver(ref pos, _, _) |
            AST::Accessor(ref pos, _, _) |
            AST::Invoke(ref pos, _, _) |
            AST::SideEffect(ref pos, _, _) |
            AST::Block(ref pos, _) |
            AST::If(ref pos, _, _, _) |
            AST::Raise(ref pos, _) |
            AST::Try(ref pos, _, _) |
            AST::Match(ref pos, _, _) |
            AST::For(ref pos, _, _, _) |
            AST::Declare(ref pos, _, _, _) |
            AST::Function(ref pos, _, _, _, _, _, _) |
            AST::New(ref pos, _) |
            AST::Class(ref pos, _, _, _) |
            AST::Import(ref pos, _, _) |
            AST::Definition(ref pos, _, _, _, _) |
            AST::Assignment(ref pos, _, _, _) |
            AST::While(ref pos, _, _) |
            AST::TypeAlias(ref pos, _, _) |
            AST::Enum(ref pos, _, _) => { pos.clone() }
            _ => Pos::empty(),
        }
    }

    pub fn get_literal(self) -> Literal {
        match self {
            AST::Literal(lit) => lit,
            _ => panic!("Expected AST::Literal\n"),
        }
    }

    pub fn make_ident_from_str(pos: Pos, name: &str) -> AST {
        AST::Identifier(pos, Ident::from_str(name))
    }

    pub fn make_resolve_ident(pos: Pos, ident: Ident, field: &str) -> AST {
        AST::Resolver(pos.clone(), r(AST::Identifier(pos, ident)), Ident::from_str(field))
    }
}

