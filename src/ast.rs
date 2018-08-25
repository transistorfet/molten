
use std::fmt;
use std::str;

use abi::ABI;
use types::Type;
use parser::Span;
use utils::UniqueID;


pub type NodeID = UniqueID;

#[derive(Clone, PartialEq)]
pub struct Pos {
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

    Tuple(NodeID, Pos, Vec<AST>, Option<Type>),
    List(NodeID, Pos, Vec<AST>, Option<Type>),

    Recall(NodeID, Pos),
    Identifier(NodeID, Pos, Ident),
    Index(NodeID, Pos, Box<AST>, Box<AST>, Option<Type>),
    Resolver(NodeID, Pos, Box<AST>, Ident),
    Accessor(NodeID, Pos, Box<AST>, Ident, Option<Type>),
    Invoke(NodeID, Pos, Box<AST>, Vec<AST>, Option<Type>),
    SideEffect(NodeID, Pos, Ident, Vec<AST>),
    //Prefix(NodeID, Pos, Ident, Box<AST>),
    //Infix(NodeID, Pos, Ident, Box<AST>, Box<AST>),
    Block(NodeID, Pos, Vec<AST>),
    If(NodeID, Pos, Box<AST>, Box<AST>, Box<AST>),
    Raise(NodeID, Pos, Box<AST>),
    Try(NodeID, Pos, Box<AST>, Vec<(AST, AST)>, Option<Type>),
    Match(NodeID, Pos, Box<AST>, Vec<(AST, AST)>, Option<Type>),
    For(NodeID, Pos, Ident, Box<AST>, Box<AST>),
    Declare(NodeID, Pos, Ident, Type),
    Function(NodeID, Pos, Option<Ident>, Vec<Argument>, Option<Type>, Box<AST>, ABI),
    New(NodeID, Pos, ClassSpec),
    Class(NodeID, Pos, ClassSpec, Option<ClassSpec>, Vec<AST>),

    Import(NodeID, Pos, Ident, Vec<AST>),
    Definition(NodeID, Pos, Ident, Option<Type>, Box<AST>),
    Assignment(NodeID, Pos, Box<AST>, Box<AST>),
    While(NodeID, Pos, Box<AST>, Box<AST>),
    TypeDef(NodeID, Pos, ClassSpec, Vec<Field>),
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
            pos: pos,
            ident: ident,
            ttype: ttype
        }
    }
}

impl AST {
    pub fn get_pos(&self) -> Pos {
        match *self {
            AST::Tuple(_, ref pos, _, _) |
            AST::List(_, ref pos, _, _) |
            AST::Recall(_, ref pos) |
            AST::Identifier(_, ref pos, _) |
            AST::Index(_, ref pos, _, _, _) |
            AST::Resolver(_, ref pos, _, _) |
            AST::Accessor(_, ref pos, _, _, _) |
            AST::Invoke(_, ref pos, _, _, _) |
            AST::SideEffect(_, ref pos, _, _) |
            AST::Block(_, ref pos, _) |
            AST::If(_, ref pos, _, _, _) |
            AST::Raise(_, ref pos, _) |
            AST::Try(_, ref pos, _, _, _) |
            AST::Match(_, ref pos, _, _, _) |
            AST::For(_, ref pos, _, _, _) |
            AST::Declare(_, ref pos, _, _) |
            AST::Function(_, ref pos, _, _, _, _, _) |
            AST::New(_, ref pos, _) |
            AST::Class(_, ref pos, _, _, _) |
            AST::Import(_, ref pos, _, _) |
            AST::Definition(_, ref pos, _, _, _) |
            AST::Assignment(_, ref pos, _, _) |
            AST::While(_, ref pos, _, _) |
            AST::TypeDef(_, ref pos, _, _) => { pos.clone() }
            _ => Pos::empty(),
        }
    }

    pub fn make_tuple(pos: Pos, items: Vec<AST>, ttype: Option<Type>) -> AST {
        AST::Tuple(NodeID::generate(), pos, items, ttype)
    }

    pub fn make_list(pos: Pos, items: Vec<AST>, ttype: Option<Type>) -> AST {
        AST::List(NodeID::generate(), pos, items, ttype)
    }

    pub fn make_recall(pos: Pos) -> AST {
        AST::Recall(NodeID::generate(), pos)
    }

    pub fn make_ident(pos: Pos, ident: Ident) -> AST {
        AST::Identifier(NodeID::generate(), pos, ident)
    }

    pub fn make_index(pos: Pos, list: Box<AST>, index: Box<AST>, ttype: Option<Type>) -> AST {
        AST::Index(NodeID::generate(), pos, list, index, ttype)
    }

    pub fn make_resolve(pos: Pos, object: Box<AST>, ident: Ident) -> AST {
        AST::Resolver(NodeID::generate(), pos, object, ident)
    }

    pub fn make_access(pos: Pos, object: Box<AST>, ident: Ident, ttype: Option<Type>) -> AST {
        AST::Accessor(NodeID::generate(), pos, object, ident, ttype)
    }

    pub fn make_invoke(pos: Pos, fexpr: Box<AST>, args: Vec<AST>, ttype: Option<Type>) -> AST {
        AST::Invoke(NodeID::generate(), pos, fexpr, args, ttype)
    }

    pub fn make_side_effect(pos: Pos, ident: Ident, args: Vec<AST>) -> AST {
        AST::SideEffect(NodeID::generate(), pos, ident, args)
    }

    pub fn make_block(pos: Pos, body: Vec<AST>) -> AST {
        AST::Block(NodeID::generate(), pos, body)
    }

    pub fn make_if(pos: Pos, cond: Box<AST>, texpr: Box<AST>, fexpr: Box<AST>) -> AST {
        AST::If(NodeID::generate(), pos, cond, texpr, fexpr)
    }

    pub fn make_raise(pos: Pos, expr: Box<AST>) -> AST {
        AST::Raise(NodeID::generate(), pos, expr)
    }

    pub fn make_try(pos: Pos, cond: Box<AST>, cases: Vec<(AST, AST)>, ttype: Option<Type>) -> AST {
        AST::Try(NodeID::generate(), pos, cond, cases, ttype)
    }

    pub fn make_match(pos: Pos, cond: Box<AST>, cases: Vec<(AST, AST)>, ttype: Option<Type>) -> AST {
        AST::Match(NodeID::generate(), pos, cond, cases, ttype)
    }

    pub fn make_for(pos: Pos, ident: Ident, list: Box<AST>, body: Box<AST>) -> AST {
        AST::For(NodeID::generate(), pos, ident, list, body)
    }

    pub fn make_decl(pos: Pos, ident: Ident, ttype: Type) -> AST {
        AST::Declare(NodeID::generate(), pos, ident, ttype)
    }

    pub fn make_func(pos: Pos, ident: Option<Ident>, args: Vec<Argument>, rtype: Option<Type>, body: Box<AST>, abi: ABI) -> AST {
        AST::Function(NodeID::generate(), pos, ident, args, rtype, body, abi)
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

    pub fn make_def(pos: Pos, ident: Ident, ttype: Option<Type>, value: Box<AST>) -> AST {
        AST::Definition(NodeID::generate(), pos, ident, ttype, value)
    }

    pub fn make_assign(pos: Pos, left: Box<AST>, right: Box<AST>) -> AST {
        AST::Assignment(NodeID::generate(), pos, left, right)
    }

    pub fn make_while(pos: Pos, cond: Box<AST>, body: Box<AST>) -> AST {
        AST::While(NodeID::generate(), pos, cond, body)
    }

    pub fn make_typedef(pos: Pos, classspec: ClassSpec, fields: Vec<Field>) -> AST {
        AST::TypeDef(NodeID::generate(), pos, classspec, fields)
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

