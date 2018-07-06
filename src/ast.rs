
use std::fmt;
use std::str;

use abi::ABI;
use types::Type;
use parser::Span;
use utils::UniqueID;


#[derive(Clone, PartialEq)]
pub struct Pos {
    pub offset: usize,
    pub column: usize,
    pub line: u32,
    pub filenum: u16,
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


#[derive(Clone, Debug, PartialEq)]
pub enum AST {
    Noop,
    //Comment(String),

    Underscore,
    Boolean(bool),
    Integer(isize),
    Real(f64),
    String(String),
    Nil(Option<Type>),
    PtrCast(Type, Box<AST>),
    List(Pos, Vec<AST>, Option<Type>),

    Recall(Pos, String),
    Identifier(Pos, String),
    Index(Pos, Box<AST>, Box<AST>, Option<Type>),
    Resolver(Pos, Box<AST>, String),
    Accessor(Pos, Box<AST>, String, Option<Type>),
    Invoke(Pos, Box<AST>, Vec<AST>, Option<Type>),
    SideEffect(Pos, String, Vec<AST>),
    //Prefix(Pos, String, Box<AST>),
    //Infix(Pos, String, Box<AST>, Box<AST>),
    Block(Pos, Vec<AST>),
    If(Pos, Box<AST>, Box<AST>, Box<AST>),
    Raise(Pos, Box<AST>),
    Try(Pos, Box<AST>, Vec<(AST, AST)>, Option<Type>),
    Match(Pos, Box<AST>, Vec<(AST, AST)>, Option<Type>),
    For(Pos, String, Box<AST>, Box<AST>, UniqueID),
    Declare(Pos, String, Type),
    Function(Pos, Option<String>, Vec<(String, Option<Type>, Option<AST>)>, Option<Type>, Box<AST>, UniqueID, ABI),
    New(Pos, (String, Vec<Type>)),
    Class(Pos, (String, Vec<Type>), Option<(String, Vec<Type>)>, Vec<AST>, UniqueID),

    Import(Pos, String, Vec<AST>),
    Definition(Pos, (String, Option<Type>), Box<AST>),
    Assignment(Pos, Box<AST>, Box<AST>),
    While(Pos, Box<AST>, Box<AST>),
    Type(Pos, String, Vec<(String, Option<Type>)>),
}

impl AST {
    pub fn get_pos(&self) -> Pos {
        match *self {
            AST::List(ref pos, _, _) |
            AST::Recall(ref pos, _) |
            AST::Identifier(ref pos, _) |
            AST::Index(ref pos, _, _, _) |
            AST::Resolver(ref pos, _, _) |
            AST::Accessor(ref pos, _, _, _) |
            AST::Invoke(ref pos, _, _, _) |
            AST::SideEffect(ref pos, _, _) |
            AST::Block(ref pos, _) |
            AST::If(ref pos, _, _, _) |
            AST::Raise(ref pos, _) |
            AST::Try(ref pos, _, _, _) |
            AST::Match(ref pos, _, _, _) |
            AST::For(ref pos, _, _, _, _) |
            AST::Declare(ref pos, _, _) |
            AST::Function(ref pos, _, _, _, _, _, _) |
            AST::New(ref pos, _) |
            AST::Class(ref pos, _, _, _, _) |
            AST::Import(ref pos, _, _) |
            AST::Definition(ref pos, _, _) |
            AST::Assignment(ref pos, _, _) |
            AST::While(ref pos, _, _) |
            AST::Type(ref pos, _, _) => { pos.clone() }
            _ => Pos::empty(),
        }
    }
} 

