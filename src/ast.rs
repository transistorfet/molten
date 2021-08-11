
use std::fmt;
use std::str;

use crate::abi::ABI;
use crate::types::Type;
use crate::parser::Span;
use crate::misc::{ R };
use crate::hir::{ Visibility, Mutability, AssignType, Literal, WhereClause, Pattern };


#[derive(Copy, Clone, PartialEq)]
pub struct Pos {
    pub offset: usize,
    pub column: usize,
    pub line: u32,
    pub filenum: u16,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RawArgument {
    pub pos: Pos,
    pub name: String,
    pub ttype: Option<Type>,
    //pub default: Option<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AST {
    Literal(Literal),
    Nil,
    Annotation(Type, R<AST>),
    Ref(Pos, R<AST>),
    Deref(Pos, R<AST>),
    Bracketed(R<AST>),

    Array(Pos, Vec<AST>),
    Tuple(Pos, Vec<AST>),
    Record(Pos, Vec<(String, AST)>),
    RecordUpdate(Pos, R<AST>, Vec<(String, AST)>),

    Identifier(Pos, String),
    Index(Pos, R<AST>, R<AST>),
    Resolver(Pos, R<AST>, String),
    Accessor(Pos, R<AST>, String),

    Block(Pos, Vec<AST>),
    Invoke(Pos, R<AST>, Vec<AST>),
    //Prefix(Pos, String, R<AST>),
    //Infix(Pos, String, R<AST>, R<AST>),

    SideEffect(Pos, String, Vec<AST>),
    If(Pos, R<AST>, R<AST>, R<AST>),
    Raise(Pos, R<AST>),
    Try(Pos, R<AST>, Vec<(Pattern, AST)>),
    Match(Pos, R<AST>, Vec<(Pattern, AST)>),
    For(Pos, String, R<AST>, R<AST>),
    While(Pos, R<AST>, R<AST>),

    Declare(Pos, Visibility, String, Type, WhereClause),
    Function(Pos, Visibility, Option<String>, Vec<RawArgument>, Option<Type>, Vec<AST>, ABI, WhereClause),
    New(Pos, Type, Vec<AST>),
    Class(Pos, Type, Option<Type>, WhereClause, Vec<AST>),
    TypeAlias(Pos, Type, Type),
    Enum(Pos, Type, WhereClause, Vec<(Pos, String, Option<Type>)>),
    TraitDef(Pos, String, Vec<AST>),
    TraitImpl(Pos, String, Type, WhereClause, Vec<AST>),
    Methods(Pos, Type, WhereClause, Vec<AST>),

    ModuleDecl(Pos, String),
    Import(Pos, String, Vec<AST>),
    Definition(Pos, Mutability, String, Option<Type>, R<AST>),
    Field(Pos, Mutability, String, Option<Type>),
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


impl RawArgument {
    pub fn new(pos: Pos, name: String, ttype: Option<Type>) -> RawArgument {
        RawArgument {
            pos,
            name,
            ttype,
        }
    }
}

impl AST {
    pub fn get_pos(&self) -> Pos {
        match *self {
            AST::Ref(pos, _) |
            AST::Deref(pos, _) |
            AST::Array(pos, _) |
            AST::Tuple(pos, _) |
            AST::Record(pos, _) |
            AST::RecordUpdate(pos, _, _) |
            AST::Identifier(pos, _) |
            AST::Index(pos, _, _) |
            AST::Resolver(pos, _, _) |
            AST::Accessor(pos, _, _) |
            AST::Invoke(pos, _, _) |
            AST::SideEffect(pos, _, _) |
            AST::Block(pos, _) |
            AST::If(pos, _, _, _) |
            AST::Raise(pos, _) |
            AST::Try(pos, _, _) |
            AST::Match(pos, _, _) |
            AST::For(pos, _, _, _) |
            AST::Declare(pos, _, _, _, _) |
            AST::Function(pos, _, _, _, _, _, _, _) |
            AST::New(pos, _, _) |
            AST::Class(pos, _, _, _, _) |
            AST::Import(pos, _, _) |
            AST::Definition(pos, _, _, _, _) |
            AST::Assignment(pos, _, _, _) |
            AST::While(pos, _, _) |
            AST::TypeAlias(pos, _, _) |
            AST::Enum(pos, _, _, _) => { pos }
            _ => Pos::empty(),
        }
    }

    pub fn get_literal(self) -> Literal {
        match self {
            AST::Literal(lit) => lit,
            _ => panic!("Expected AST::Literal\n"),
        }
    }
}

