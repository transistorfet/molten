
use ast::NodeID;
use misc::{ R, r };


#[derive(Clone, Debug, PartialEq)]
pub enum LLType {
    Void,
    I1,
    I8,
    I32,
    I64,
    F64,
    Var,
    Exception,
    Ptr(R<LLType>),
    Struct(Vec<LLType>),
    Function(Vec<LLType>, R<LLType>),
    Alias(NodeID),
}

impl LLType {
    pub fn argcount(&self) -> usize {
        match self {
            LLType::Function(args, _) => args.len(),
            _ => 0,
        }
    }

    pub fn get_rettype(&self) -> LLType {
        match self {
            LLType::Function(_, ret) => *ret.clone(),
            _ => LLType::Void,
        }
    }

    pub fn get_element(&self) -> LLType {
        match self {
            LLType::Ptr(element) => *element.clone(),
            _ => self.clone(),
        }
    }

    pub fn get_items(&self) -> Vec<LLType> {
        match self {
            LLType::Struct(items) => items.clone(),
            _ => panic!("InternalError: attempting to get items from a non-struct type {:?}", self),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum LLLit {
    I1(bool),
    I8(i8),
    I32(i32),
    I64(i64),
    F64(f64),
    Null(LLType),
    ConstStr(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum LLRef {
    Deref,
    Field(usize),
    Index(usize),
    //Ident(NodeID),
}

#[derive(Clone, Debug, PartialEq)]
pub enum LLCmpType {
    Equal,
    NotEqual,
}

pub type LLBlock = Vec<LLExpr>;

#[derive(Clone, Debug, PartialEq)]
pub enum LLExpr {
    Literal(LLLit),
    GetValue(NodeID),
    SetValue(NodeID, R<LLExpr>),
    GetNamed(String),
    Cast(LLType, R<LLExpr>),

    CallC(R<LLExpr>, Vec<LLExpr>),

    DefLocal(NodeID, String, LLType, R<LLExpr>),
    GetLocal(NodeID),
    SetLocal(NodeID, R<LLExpr>),

    GetGlobal(NodeID),
    SetGlobal(NodeID, R<LLExpr>),

    DefStruct(NodeID, LLType, Vec<LLExpr>),
    GetItem(R<LLExpr>, usize),
    SetItem(R<LLExpr>, usize, R<LLExpr>),

    AllocRef(NodeID, LLType, Option<R<LLExpr>>),
    AccessRef(R<LLExpr>, Vec<LLRef>),
    LoadRef(R<LLExpr>),
    StoreRef(R<LLExpr>, R<LLExpr>),

    Cmp(LLCmpType, R<LLExpr>, R<LLExpr>),
    Phi(Vec<LLBlock>, Vec<LLBlock>),
    Loop(LLBlock, LLBlock),
}

#[derive(Clone, Debug, PartialEq)]
pub enum LLGlobal {
    DefType(NodeID, String, LLType),

    DefGlobal(NodeID, String, LLType),
    DefCFunc(NodeID, String, LLType, Vec<(NodeID, String)>, Vec<LLExpr>),
    DeclCFunc(NodeID, String, LLType),

    DefNamedStruct(NodeID, String),
    SetStructBody(NodeID, Vec<LLType>),
}


