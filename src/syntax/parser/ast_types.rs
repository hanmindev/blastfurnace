#[derive(Debug)]
pub enum Type {
    Void,
    Bool,
    Int,
    Float,
    Double,
    String,
    Struct(String),
}

#[derive(Debug)]
pub enum UnOp {
    Plus,
    Neg,
    Not,
    Deref,
    Ref,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum VarMod {
    Const,
    Static,
}


#[derive(Debug)]
pub struct VarDecl {
    pub name: String,
    pub type_: Type,
    pub mods: Vec<VarMod>,
}

#[derive(Debug)]
struct Assignment {
    pub path: NamePath,
    pub expr: Expression,
}

#[derive(Debug)]
pub enum FnMod {
    Rec,
    Inline,
}

#[derive(Debug)]
pub struct FnDecl {
    pub name: String,
    pub args: Vec<VarDecl>,
    pub body: Block,
    pub mods: Vec<FnMod>,
}

#[derive(Debug)]
pub struct FnCall {
    pub path: NamePath,
    pub args: Vec<Expression>,
}

#[derive(Debug)]
pub enum LiteralValue {
    Null,
    Bool(bool),
    Int(i32),
    Decimal(f64),
    String(String),
}

#[derive(Debug)]
pub enum AtomicExpression {
    Literal(LiteralValue),
    Variable(NamePath),
    FnCall(Box<FnCall>),
}

#[derive(Debug)]
pub enum Expression {
    AtomicExpression(AtomicExpression),
    Unary(UnOp, Box<Expression>),
    Binary(Box<Expression>, BinOp, Box<Expression>),
}

pub struct If {
    pub cond: Expression,
    pub body: Block,
    pub else_: Option<Box<If>>,
}

pub struct While {
    pub cond: Expression,
    pub body: Block,
}

pub struct For {
    pub init: Option<Statement>,
    pub cond: Option<Expression>,
    pub step: Option<Statement>,
    pub body: Block,
}

#[derive(Debug)]
pub enum Statement {
    VarDecl(VarDecl),
    Assignment(Assignment),
    FnDecl(FnDecl),
    FnCall(FnCall),
    Return(Expression),
}

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
}