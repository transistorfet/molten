
use std::fmt;
use std::str;

use abi::ABI;
use types::Type;
use parser::Span;
use misc::{ R, r };
use hir::{ Visibility, Mutability, AssignType, Literal, Argument, ClassSpec, Pattern };


#[derive(Copy, Clone, PartialEq)]
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
    Annotation(Type, R<AST>),
    Ref(Pos, R<AST>),
    Deref(Pos, R<AST>),

    List(Pos, Vec<AST>),
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

    Declare(Pos, Visibility, String, Type),
    Function(Pos, Visibility, Option<String>, Vec<Argument>, Option<Type>, Vec<AST>, ABI),
    New(Pos, ClassSpec, Vec<AST>),
    Class(Pos, ClassSpec, Option<ClassSpec>, Vec<AST>),
    TypeAlias(Pos, ClassSpec, Type),
    Enum(Pos, ClassSpec, Vec<(Pos, String, Option<Type>)>),
    TraitDef(Pos, ClassSpec, Vec<AST>),
    TraitImpl(Pos, ClassSpec, Type, Vec<AST>),

    Import(Pos, String, Vec<AST>),
    Definition(Pos, Mutability, String, Option<Type>, R<AST>),
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
            AST::Ref(pos, _) |
            AST::Deref(pos, _) |
            AST::List(pos, _) |
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
            AST::Declare(pos, _, _, _) |
            AST::Function(pos, _, _, _, _, _, _) |
            AST::New(pos, _, _) |
            AST::Class(pos, _, _, _) |
            AST::Import(pos, _, _) |
            AST::Definition(pos, _, _, _, _) |
            AST::Assignment(pos, _, _, _) |
            AST::While(pos, _, _) |
            AST::TypeAlias(pos, _, _) |
            AST::Enum(pos, _, _) => { pos }
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
        AST::Identifier(pos, name.to_string())
    }

    pub fn make_resolve_ident(pos: Pos, ident: String, field: &str) -> AST {
        AST::Resolver(pos, r(AST::Identifier(pos, ident)), field.to_string())
    }
}

