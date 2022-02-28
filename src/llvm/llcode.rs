
use crate::misc::{ R, UniqueID };


#[derive(Clone, Debug, PartialEq)]
pub enum LLType {
    #[allow(dead_code)]
    Void,
    I1,
    I8,
    I32,
    I64,
    F64,
    Var,
    ExceptionPoint,
    Ptr(R<LLType>),
    Struct(Vec<LLType>),
    Function(Vec<LLType>, R<LLType>),
    Alias(UniqueID),
    Largest(Vec<LLType>),
    #[allow(dead_code)]
    Array(R<LLType>, usize),
}

impl LLType {
    pub fn argcount(&self) -> usize {
        match self {
            LLType::Function(args, _) => args.len(),
            _ => 0,
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
    #[allow(dead_code)]
    Deref,
    Field(usize),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LLCmpType {
    Equal,
    #[allow(dead_code)]
    NotEqual,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LLLink {
    Private,
    Public,
    Once,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LLCC {
    CCC,
    FastCC,
}


pub type LLBlock = Vec<LLExpr>;

#[derive(Clone, Debug, PartialEq)]
pub enum LLExpr {
    Literal(LLLit),
    GetValue(UniqueID),
    SetValue(UniqueID, R<LLExpr>),
    GetNamed(String),

    Cast(LLType, R<LLExpr>),
    PackUniversal(LLType, R<LLExpr>),
    UnpackUniversal(LLType, R<LLExpr>),

    CallC(R<LLExpr>, Vec<LLExpr>, LLCC),

    DefLocal(UniqueID, String, LLType, R<LLExpr>),
    GetLocal(UniqueID),
    #[allow(dead_code)]
    SetLocal(UniqueID, R<LLExpr>),

    GetGlobal(UniqueID),
    SetGlobal(UniqueID, R<LLExpr>),

    DefStruct(UniqueID, LLType, Vec<LLExpr>),
    GetItem(R<LLExpr>, usize),
    #[allow(dead_code)]
    SetItem(R<LLExpr>, usize, R<LLExpr>),

    AllocRef(UniqueID, LLType, Option<R<LLExpr>>),
    AccessRef(R<LLExpr>, Vec<LLRef>),
    LoadRef(R<LLExpr>),
    StoreRef(R<LLExpr>, R<LLExpr>),

    And(R<LLExpr>, R<LLExpr>),
    Cmp(LLCmpType, R<LLExpr>, R<LLExpr>),
    Phi(Vec<LLBlock>, Vec<LLBlock>),
    Loop(LLBlock, LLBlock),
}

#[derive(Clone, Debug, PartialEq)]
pub enum LLGlobal {
    DefType(UniqueID, String, LLType),

    DefGlobal(UniqueID, LLLink, String, LLType, bool),
    DefCFunc(UniqueID, LLLink, String, LLType, Vec<(UniqueID, String)>, Vec<LLExpr>, LLCC),
    DeclCFunc(UniqueID, String, LLType, LLCC),

    DefNamedStruct(UniqueID, String, bool),
    SetStructBody(UniqueID, Vec<LLType>, bool),
}


