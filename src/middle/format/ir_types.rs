#[derive(Debug, Clone)]
pub enum AddressOrigin {
    User(String),
    CtxGenerated(String, u32),
    If,
    Return
}


#[derive(Debug, Clone)]
pub struct Address {
    pub name: AddressOrigin,
    pub offset: i32,
}

pub type FunctionName = String;

#[derive(Debug, PartialEq)]
pub struct IrScoreSet {
    pub var_name: Address,
    pub value: i32,
}

#[derive(Debug, PartialEq)]
pub struct IrScoreAddI {
    pub var_name: Address,
    pub value: i32,
}

#[derive(Debug, PartialEq)]
pub enum IrScoreOperationType {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Assign,
}

#[derive(Debug, PartialEq)]
pub struct IrScoreOperation {
    pub left: Address,
    pub op: IrScoreOperationType,
    pub right: Address,
}

#[derive(Debug, PartialEq)]
pub struct IrFnDef {
    pub fn_name: FunctionName,
    pub body: IrBlock,
}

#[derive(Debug, PartialEq)]
pub struct IrFnCall {
    pub fn_name: FunctionName,
}

#[derive(Debug, PartialEq)]
pub struct CheckVal {
    pub var_name: Address,
    pub min: i32,
    pub max: i32,
}

#[derive(Debug, PartialEq)]
pub enum CompareOp {
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
}

#[derive(Debug, PartialEq)]
pub struct CompareVal {
    pub var_0: Address,
    pub op: CompareOp,
    pub var_1: Address,
}

#[derive(Debug, PartialEq)]
pub enum Cond {
    CheckVal(CheckVal),
}

#[derive(Debug, PartialEq)]
pub struct IrIf {
    pub cond: Cond,
    pub body: IrStatement,
}

#[derive(Debug, PartialEq)]
pub struct IrUnless {
    pub cond: Cond,
    pub body: IrStatement,
}

#[derive(Debug, PartialEq)]
pub enum IrStatement {
    ScoreSet(IrScoreSet),
    ScoreAddI(IrScoreAddI),
    ScoreOperation(IrScoreOperation),
    If(IrIf),
    Unless(IrUnless),
    FnCall(IrFnCall),
    Return,
    Block(IrBlock),
}

#[derive(Debug, PartialEq)]
pub struct IrBlock {
    pub root_fn_name: FunctionName,
    pub fn_block_index: usize,
    pub statements: Vec<IrStatement>,
}

impl IrBlock {
    pub fn get_fn_name(&mut self) -> String {
        format!("{}_{}", self.root_fn_name, self.fn_block_index)
    }
}
