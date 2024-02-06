use std::collections::HashMap;
use std::rc::Rc;

type StructName = String;
type VarName = String;
type FunctionName = String;


#[derive(Debug, PartialEq)]
pub enum IrType {
    Void,
    Bool,
    Int,
    Float,
    Double,
    String,
    Struct(StructName),
}

#[derive(Debug, PartialEq)]
pub enum IrUnOp {
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
pub enum IrBinOp {
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
pub struct IrNamePath {
    pub name: VarName,
    pub path: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub enum IrVarMod {
    Const,
}

#[derive(Debug, PartialEq)]
pub struct IrVarDecl {
    pub var_def: IrVarDef,
    pub expr: Option<Box<IrExpression>>,
}

#[derive(Debug, PartialEq)]
pub struct IrVarAssign {
    pub name_path: IrNamePath,
    pub expr: Box<IrExpression>,
}

pub type IrCompound = HashMap<String, IrCompoundValue>;

#[derive(Debug, PartialEq)]
pub enum IrCompoundValue {
    Expression(Box<IrExpression>),
    Compound(Box<IrCompound>),
}

#[derive(Debug, PartialEq)]
pub enum IrFnMod {
    Rec,
    Inline,
}

#[derive(Debug, PartialEq)]
pub enum IrStructMod {}

#[derive(Debug, PartialEq)]
pub struct IrStructDef {
    pub mods: Rc<Vec<IrStructMod>>,
    pub type_name: StructName,
    pub map: HashMap<String, IrType>,
}

#[derive(Debug, PartialEq)]
pub struct IrVarDef {
    pub mods: Rc<Vec<IrVarMod>>,
    pub name: VarName,
    pub type_: IrType,
}

#[derive(Debug, PartialEq)]
pub struct IrFnDef {
    pub return_type: IrType,
    pub mods: Rc<Vec<IrFnMod>>,
    pub name: FunctionName,
    pub args: Vec<IrVarDef>,
    pub body: IrBlock,
}

#[derive(Debug, PartialEq)]
pub struct IrFnCall {
    pub name: FunctionName,
    pub args: Vec<IrExpression>,
}

#[derive(Debug, PartialEq)]
pub enum IrLiteralValue {
    Null,
    Bool(bool),
    Int(i32),
    Decimal(f64),
    String(String),
    Compound(IrCompound),
}

#[derive(Debug, PartialEq)]
pub enum IrAtomicExpression {
    Literal(IrLiteralValue),
    Variable(IrNamePath),
    FnCall(Box<IrFnCall>),
}

#[derive(Debug, PartialEq)]
pub enum IrExpression {
    AtomicExpression(IrAtomicExpression),
    Unary(IrUnOp, Box<IrExpression>),
    Binary(Box<IrExpression>, IrBinOp, Box<IrExpression>),
}

#[derive(Debug, PartialEq)]
pub struct IrIf {
    pub cond: Box<IrExpression>,
    pub body: Box<IrBlock>,
    pub else_: Option<Box<IrIf>>,
}

#[derive(Debug, PartialEq)]
pub struct IrWhile {
    pub cond: Box<IrExpression>,
    pub body: Box<IrBlock>,
}

#[derive(Debug, PartialEq)]
pub struct IrFor {
    pub init: Option<Box<IrStatement>>,
    pub cond: Option<Box<IrExpression>>,
    pub step: Option<Box<IrStatement>>,
    pub body: IrBlock,
}

#[derive(Debug, PartialEq)]
pub enum IrStatement {
    VarDecl(IrVarDecl),
    VarAssign(IrVarAssign),
    If(IrIf),
    While(IrWhile),
    For(IrFor),
    Return(Box<IrExpression>),
    Break,
    Continue,
    Expression(Box<IrExpression>),
}

#[derive(Debug, PartialEq)]
pub enum IrStatementBlock {
    Statement(IrStatement),
    Block(IrBlock),
}

#[derive(Debug, PartialEq)]
pub struct IrBlock {
    pub statements: Vec<IrStatementBlock>,
}