#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub enum AddressOrigin {
    User(String),
    CtxGenerated(String, u32),
    If,
    Return,
    Const(i32),
}

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub struct Address {
    pub name: AddressOrigin,
    pub offset: i32,
}

pub type FunctionName = String;

#[derive(Debug, PartialEq)]
pub enum IrScoreOperationType {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Assign,

    Leq,
    Geq,
    Lt,
    Gt,
    Eq,
    Neq,

    And,
    Or,
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
    pub block_count: usize,
    pub statements: Vec<IrStatement>,
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
    CompareVal(CompareVal),
}

#[derive(Debug, PartialEq)]
pub struct IrIf {
    pub invert: bool,
    pub cond: Cond,
    pub body: Box<IrStatement>,
}

#[derive(Debug, PartialEq)]
pub enum IrStatement {
    ScoreOperation(IrScoreOperation),
    If(IrIf),
    FnCall(IrFnCall),
    Return,
    Block(IrBlock),
}

#[derive(Debug, PartialEq)]
pub struct IrBlock {
    pub can_embed: bool,
    pub root_fn_name: FunctionName,
    pub fn_block_index: usize,
    pub statements: Vec<IrStatement>,
}

impl IrBlock {
    pub fn get_fn_name(&self) -> String {
        format!("{}_{}", self.root_fn_name, self.fn_block_index)
    }
}

impl std::fmt::Display for IrBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "block {} {{\n",
            self.fn_block_index
        )?;
        for statement in &self.statements {
            write!(f, "    {:?}\n", statement)?;
        }
        write!(f, "}}")
    }
}

impl std::fmt::Display for IrFnDef {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "fn {} {{\n", self.fn_name,)?;
        for statement in &self.statements {
            match statement {
                IrStatement::Block(x) => {
                    write!(f, "{}\n", x)?;
                }
                _=> {
                    write!(f, "    {:?}\n", statement)?;
                }
            }
        }
        write!(f, "}}")
    }
}
