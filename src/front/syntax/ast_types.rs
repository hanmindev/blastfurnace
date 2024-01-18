use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub struct Reference<T, R> {
    pub raw: Option<T>,
    pub resolved: Option<R>,
}

impl<T, R> Reference<T, R> {
    pub fn new(raw: T) -> Reference<T, R> {
        Reference {
            raw: Some(raw),
            resolved: None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Void,
    Bool,
    Int,
    Float,
    Double,
    String,
    Struct(Reference<String, String>),
}

#[derive(Debug, PartialEq)]
pub enum UnOp {
    Neg,
    Not,
    Deref,
    Ref,
    PreInc,
    PreDec,
    PostInc,
    PostDec,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    And,
    Or,
}

#[derive(Debug, PartialEq, Clone)]
pub struct NamePath {
    pub name: Reference<String, String>,
    pub path: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub enum VarMod {
    Const,
    Static,
}

#[derive(Debug, PartialEq)]
pub struct VarDecl {
    pub name: Reference<String, String>,
    pub type_: Type,
    pub mods: Vec<VarMod>,
    pub expr: Option<Box<Expression>>,
}

#[derive(Debug, PartialEq)]
pub struct VarAssign {
    pub name_path: NamePath,
    pub expr: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct StructDef {
    pub name: Reference<String, String>,
    pub map: HashMap<String, Type>,
}

#[derive(Debug, PartialEq)]
pub struct StructDecl {
    pub name: Reference<String, String>,
    pub type_: Type,
    pub mods: Vec<VarMod>,
    pub expr: Option<Compound>,
}

pub type Compound = HashMap<String, CompoundValue>;

#[derive(Debug, PartialEq)]
pub enum CompoundValue {
    Expression(Box<Expression>),
    Compound(Box<Compound>),
}

#[derive(Debug, PartialEq)]
pub struct StructAssign {
    pub name_path: NamePath,
    pub compound: Compound,
}

#[derive(Debug, PartialEq)]
pub enum FnMod {
    Rec,
    Inline,
}

#[derive(Debug, PartialEq)]
pub struct FnDef {
    pub name: Reference<String, String>,
    pub args: Vec<(Vec<VarMod>, Type, String)>,
    pub body: Block,
    pub mods: Vec<FnMod>,
}

#[derive(Debug, PartialEq)]
pub struct FnCall {
    pub name_path: NamePath,
    pub args: Vec<Box<Expression>>,
}

#[derive(Debug, PartialEq)]
pub enum LiteralValue {
    Null,
    Bool(bool),
    Int(i32),
    Decimal(f64),
    String(String),
    Compound(Compound),
}

#[derive(Debug, PartialEq)]
pub enum AtomicExpression {
    Literal(LiteralValue),
    Variable(NamePath),
    FnCall(Box<FnCall>),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    AtomicExpression(AtomicExpression),
    Unary(UnOp, Box<Expression>),
    Binary(Box<Expression>, BinOp, Box<Expression>),
}

#[derive(Debug, PartialEq)]
pub struct If {
    pub cond: Box<Expression>,
    pub body: Box<Block>,
    pub else_: Option<Box<If>>,
}

#[derive(Debug, PartialEq)]
pub struct While {
    pub cond: Box<Expression>,
    pub body: Box<Block>,
}

#[derive(Debug, PartialEq)]
pub struct For {
    pub init: Option<Box<Statement>>,
    pub cond: Option<Box<Expression>>,
    pub step: Option<Box<Statement>>,
    pub body: Block,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    VarDecl(VarDecl),
    StructDecl(StructDecl),
    VarAssign(VarAssign),
    StructAssign(StructAssign),
    StructDef(StructDef),
    FnDef(FnDef),
    If(If),
    While(While),
    For(For),
    Return(Box<Expression>),
    Break,
    Continue,
    Expression(Box<Expression>),
}

#[derive(Debug, PartialEq)]
pub enum StatementBlock {
    Statement(Statement),
    Block(Block),
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub statements: Vec<StatementBlock>,
}
