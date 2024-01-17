use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum Type {
    Void,
    Bool,
    Int,
    Float,
    Double,
    String,
    Struct(String),
}

#[derive(Debug, PartialEq)]
pub enum UnOp {
    Plus,
    Neg,
    Not,
    Deref,
    Ref,
}

#[derive(Debug, PartialEq)]
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
}
pub type NamePath = Vec<String>;

#[derive(Debug, PartialEq)]
pub enum VarMod {
    Const,
    Static,
}

#[derive(Debug, PartialEq)]
pub struct VarDecl {
    pub name: String,
    pub type_: Type,
    pub mods: Vec<VarMod>,
    pub expr: Option<Box<Expression>>,
}

#[derive(Debug, PartialEq)]
pub struct VarAssign {
    pub path: NamePath,
    pub expr: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct StructDef {
    pub name: String,
    pub map: HashMap<String, Type>,
}

#[derive(Debug, PartialEq)]
pub struct StructDecl {
    pub name: String,
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
    pub path: NamePath,
    pub compound: Compound,
}

#[derive(Debug, PartialEq)]
pub enum FnMod {
    Rec,
    Inline,
}

#[derive(Debug, PartialEq)]
pub struct FnDef {
    pub name: String,
    pub args: Vec<(Vec<VarMod>, Type, String)>,
    pub body: Block,
    pub mods: Vec<FnMod>,
}

#[derive(Debug, PartialEq)]
pub struct FnCall {
    pub path: NamePath,
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
    FnDef(FnDef),
    FnCall(FnCall),
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
