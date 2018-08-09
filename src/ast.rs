
use std::fmt;
use std::str;

use abi::ABI;
use types::Type;
use parser::Span;
use utils::UniqueID;


pub type NodeID = UniqueID;

#[derive(Clone, PartialEq)]
pub struct Pos {
    pub id: NodeID,
    pub offset: usize,
    pub column: usize,
    pub line: u32,
    pub filenum: u16,
}


#[derive(Clone, Debug, PartialEq)]
pub struct Ident {
    pub pos: Pos,
    pub name: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Argument {
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
    pub pos: Pos,
    pub ident: Ident,
    pub ttype: Option<Type>
}

#[derive(Clone, Debug, PartialEq)]
pub enum AST {
    //Comment(String),

    Underscore,
    Boolean(bool),
    Integer(isize),
    Real(f64),
    String(String),
    Nil(Option<Type>),
    PtrCast(Type, Box<AST>),
    Tuple(Pos, Vec<AST>, Option<Type>),
    List(Pos, Vec<AST>, Option<Type>),

    Recall(Pos, Ident),
    Identifier(Pos, Ident),
    Index(Pos, Box<AST>, Box<AST>, Option<Type>),
    Resolver(Pos, Box<AST>, Ident),
    Accessor(Pos, Box<AST>, Ident, Option<Type>),
    Invoke(Pos, Box<AST>, Vec<AST>, Option<Type>),
    SideEffect(Pos, Ident, Vec<AST>),
    //Prefix(Pos, Ident, Box<AST>),
    //Infix(Pos, Ident, Box<AST>, Box<AST>),
    Block(Pos, Vec<AST>),
    If(Pos, Box<AST>, Box<AST>, Box<AST>),
    Raise(Pos, Box<AST>),
    Try(Pos, Box<AST>, Vec<(AST, AST)>, Option<Type>),
    Match(Pos, Box<AST>, Vec<(AST, AST)>, Option<Type>),
    For(Pos, Ident, Box<AST>, Box<AST>, UniqueID),
    Declare(Pos, Ident, Type),
    Function(Pos, Option<Ident>, Vec<Argument>, Option<Type>, Box<AST>, UniqueID, ABI),
    New(Pos, ClassSpec),
    Class(Pos, ClassSpec, Option<ClassSpec>, Vec<AST>, UniqueID),

    Import(Pos, Ident, Vec<AST>),
    Definition(Pos, Ident, Option<Type>, Box<AST>),
    Assignment(Pos, Box<AST>, Box<AST>),
    While(Pos, Box<AST>, Box<AST>),
    TypeDef(Pos, ClassSpec, Vec<Field>),
}



impl Pos {
    pub fn new(span: Span) -> Pos {
        Pos {
            id: UniqueID::generate(),
            offset: span.offset,
            column: span.get_utf8_column(),
            line: span.line,
            filenum: 0,
        }
    }

    pub fn empty() -> Pos {
        Pos { id: UniqueID::generate(), offset: 0, column: 0, line: 0, filenum: 0 }
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
    pub fn new(pos: Pos, name: String) -> Self {
        Ident {
            pos: pos,
            name: name
        }
    }

    pub fn from_str(name: &str) -> Ident {
        Ident {
            pos: Pos::empty(),
            name: String::from(name),
        }
    }

    pub fn from_span(span: Span) -> Ident {
        Ident {
            pos: Pos::new(span),
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
            pos: pos,
            ident: ident,
            ttype: ttype
        }
    }
}

impl AST {
    pub fn get_pos(&self) -> Pos {
        match *self {
            AST::Tuple(ref pos, _, _) |
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
            AST::Definition(ref pos, _, _, _) |
            AST::Assignment(ref pos, _, _) |
            AST::While(ref pos, _, _) |
            AST::TypeDef(ref pos, _, _) => { pos.clone() }
            _ => Pos::empty(),
        }
    }

    pub fn make_


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




    /*
    AST::make_for(ident, list, body).set_span(span)
    AST::make_while(ident, body).set_span(span)
    AST::make_ident(ident).set_span(span)
    AST::make_def(ident).set_span(span)
    AST::make_decl(ident, ttype, value).set_span(span)
    AST::make_func(ident, args, rettype, body).set_span(span)
    */


    /*
    AST::mkident(ident).set_span(span)
    AST::mkdef(ident).set_span(span)


    AST::mk_ident(ident).set_span(span)
    AST::mk_def(ident).set_span(span)
    AST::make_for(ident, list, body).set_span(span)
    AST::make_while(ident, body).set_span(span)
    AST::make_identifer(ident).set_span(span)
    AST::make_definition(ident, ttype, value).set_span(span)
    AST::make_function(ident, args, rettype, body).set_span(span)


    AST::build_identifer(ident).set_span(span)
    AST::build_defintion(ident).set_span(span)

    AST::cons_for(ident).set_span(span)
    AST::cons_while(ident).set_span(span)
    AST::cons_identifer(ident).set_span(span)
    AST::cons_definition(ident).set_span(span)



    AST::mkdefinition(ident).set_span(span)
    AST::mkidentifer(ident).set_span(span)
    AST::mk_identifer(ident).set_span(span)
    AST::makeidentifer(ident).set_span(span)
    */
} 

