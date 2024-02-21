pub mod visitor;

use std::collections::HashMap;
use std::rc::Rc;

pub type RawName = String;
pub type ResolvedName = String;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct GlobalResolvedName {
    pub package: Rc<str>,
    pub module: Rc<str>,
    pub name: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Reference {
    pub raw: RawName,
    pub module_resolved: Option<Rc<ResolvedName>>,
    pub global_resolved: Option<Rc<GlobalResolvedName>>,
}

impl Reference {
    pub fn new(raw: RawName) -> Reference {
        Reference {
            raw,
            module_resolved: None,
            global_resolved: None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Void,
    Int,
    Float,
    Double,
    String,
    Struct(Reference),
}

#[derive(Debug, PartialEq, Clone)]
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
    pub name: Reference,
    pub path: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub enum VarMod {
    Const,
}

#[derive(Debug, PartialEq)]
pub struct VarDecl {
    pub var_def: VarDef,
    pub expr: Option<Box<Expression>>,
}

#[derive(Debug, PartialEq)]
pub struct VarAssign {
    pub name_path: NamePath,
    pub expr: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub enum FnMod {
    Rec,
    Inline,
}

#[derive(Debug, PartialEq)]
pub enum StructMod {}

#[derive(Debug, PartialEq)]
pub struct StructDef {
    pub mods: Rc<Vec<StructMod>>,
    pub type_name: Reference,
    pub fields: HashMap<String, Type>,
}

#[derive(Debug, PartialEq)]
pub struct VarDef {
    pub mods: Rc<Vec<VarMod>>,
    pub name: Reference,
    pub type_: Option<Type>,
}

#[derive(Debug, PartialEq)]
pub struct FnDef {
    pub return_type: Type,
    pub mods: Rc<Vec<FnMod>>,
    pub name: Reference,
    pub args: Vec<VarDef>,
    pub body: Block,
}

#[derive(Debug, PartialEq)]
pub struct FnCall {
    pub name: Reference,
    pub args: Vec<Expression>,
}

#[derive(Debug, PartialEq)]
pub enum LiteralValue {
    Null,
    Int(i32),
    Float(f32),
    Double(f64),
    String(String),
}

#[derive(Debug, PartialEq)]
pub enum AtomicExpression {
    Literal(LiteralValue),
    Variable(NamePath),
    FnCall(Box<FnCall>),
    StructInit(StructInit),
}

#[derive(Debug, PartialEq)]
pub struct StructInit {
    pub type_: Reference,
    pub fields: HashMap<String, Expression>,
}

#[derive(Debug, PartialEq)]
pub enum ExpressionEnum {
    AtomicExpression(AtomicExpression),
    Unary(UnOp, Box<Expression>),
    Binary(Box<Expression>, BinOp, Box<Expression>),
}

#[derive(Debug, PartialEq)]
pub struct Expression {
    pub expr: ExpressionEnum,
    pub type_: Option<Type>,
}

#[derive(Debug, PartialEq)]
pub enum Else {
    If(Box<If>),
    Block(Box<Block>),
}

#[derive(Debug, PartialEq)]
pub struct If {
    pub cond: Box<Expression>,
    pub body: Box<Block>,
    pub else_: Option<Else>,
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
    VarAssign(VarAssign),
    If(If),
    While(While),
    For(For),
    Return(Box<Expression>),
    Break,
    Continue,
    Expression(Box<Expression>),
    Block(Block),
}

#[derive(Debug, PartialEq)]
pub enum Definition {
    VarDecl(VarDecl),
    StructDef(StructDef),
    FnDef(FnDef),
}

#[derive(Debug, PartialEq)]
pub struct DefinitionMap {
    pub functions: Vec<Definition>,
    pub structs: Vec<Definition>,
    pub vars: Vec<Definition>,
}

impl DefinitionMap {
    pub fn new() -> DefinitionMap {
        DefinitionMap {
            functions: Vec::new(),
            structs: Vec::new(),
            vars: Vec::new(),
        }
    }
    pub fn is_empty(&self) -> bool {
        self.functions.is_empty() && self.structs.is_empty() && self.vars.is_empty()
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item=&mut Definition> {
        self.functions.iter_mut().chain(self.structs.iter_mut()).chain(self.vars.iter_mut())
    }
    pub fn pop(&mut self) -> Option<Definition> {
        self.functions.pop().or_else(|| self.structs.pop()).or_else(|| self.vars.pop())
    }
}

pub fn module_definition_iter<'a>(public_definitions: &'a mut DefinitionMap, definitions: &'a mut DefinitionMap) -> impl Iterator<Item=&'a mut Definition> {
    public_definitions.structs.iter_mut()
        .chain(definitions.structs.iter_mut())
        .chain(public_definitions.vars.iter_mut())
        .chain(definitions.vars.iter_mut())
        .chain(public_definitions.functions.iter_mut())
        .chain(definitions.functions.iter_mut())
}

#[derive(Debug, PartialEq)]
pub struct Module {
    pub mods: Vec<ModuleImport>,
    pub uses: Vec<Use>,
    pub public_definitions: DefinitionMap,
    pub definitions: DefinitionMap,
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub definitions: DefinitionMap,
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub struct ModuleImport {
    pub public: bool,
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub struct UseElement {
    pub origin_name: String,
    pub imported_name: Reference,
}

#[derive(Debug, PartialEq)]
pub struct Use {
    pub path: Vec<String>,
    pub elements: Vec<UseElement>,
}
