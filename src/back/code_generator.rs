use crate::middle::format::ir_types::{Cond, IrUnless};
use crate::middle::format::ir_types::{Address, IrBlock, IrIf, IrScoreSet, IrStatement};
use crate::middle::format::ir_types::{
    AddressOrigin, IrScoreAddI, IrScoreOperation, IrScoreOperationType,
};

static BLASTFURNACE_OBJECTIVE: &str = "blst";
static BLASTFURNACE_CONST: &str = "blst";

trait CodeGenerator {
    fn generate(&self) -> Vec<String>;
}

impl Address {
    fn to_score(&self) -> String {
        match &self.name {
            AddressOrigin::User(x) => format!("{x} {BLASTFURNACE_OBJECTIVE}"),
            AddressOrigin::CtxGenerated(x, y) => format!("{x}_{y} {BLASTFURNACE_OBJECTIVE}"),
            AddressOrigin::If => format!("!if {BLASTFURNACE_OBJECTIVE}"),
            AddressOrigin::Return => format!("!return {BLASTFURNACE_OBJECTIVE}"),
            AddressOrigin::Const(x) => {
                if *x >= 0 {
                    format!("{x} {BLASTFURNACE_CONST}")
                } else {
                    format!("n{x} {BLASTFURNACE_CONST}")
                }
            }
        }
    }
}

impl CodeGenerator for IrScoreSet {
    fn generate(&self) -> Vec<String> {
        vec![format!(
            "scoreboard players set {} {}",
            self.var_name.to_score(),
            self.value
        )]
    }
}

impl CodeGenerator for IrScoreAddI {
    fn generate(&self) -> Vec<String> {
        if self.value >= 0 {
            vec![format!(
                "scoreboard players add {} {}",
                self.var_name.to_score(),
                self.value
            )]
        } else {
            vec![format!(
                "scoreboard players remove {} {}",
                self.var_name.to_score(),
                -self.value
            )]
        }
    }
}

impl CodeGenerator for IrScoreOperation {
    fn generate(&self) -> Vec<String> {
        let op = match self.op {
            IrScoreOperationType::Add => "+=",
            IrScoreOperationType::Sub => "-=",
            IrScoreOperationType::Mul => "*=",
            IrScoreOperationType::Div => "/=",
            IrScoreOperationType::Mod => "%=",
            IrScoreOperationType::Assign => "=",
            IrScoreOperationType::Leq => "<=",
            IrScoreOperationType::Geq => "=>",
            IrScoreOperationType::Lt => "<",
            IrScoreOperationType::Gt => ">",
            IrScoreOperationType::Eq => "=",
            _ => "",
        };
        match self.op {
            IrScoreOperationType::Add
            | IrScoreOperationType::Sub
            | IrScoreOperationType::Mul
            | IrScoreOperationType::Div
            | IrScoreOperationType::Mod
            | IrScoreOperationType::Assign => {
                vec![format!(
                    "scoreboard players operation {} {} {}",
                    self.left.to_score(),
                    op,
                    self.right.to_score()
                )]
            }
            IrScoreOperationType::Leq |
            IrScoreOperationType::Geq |
            IrScoreOperationType::Lt |
            IrScoreOperationType::Gt |
            IrScoreOperationType::Eq => {
                vec![format!(
                    "execute store result score {} if score {} {op} {}",
                    self.left.to_score(),
                    self.left.to_score(),
                    self.right.to_score()
                )]
            }
            IrScoreOperationType::Neq => {
                vec![format!(
                    "execute store result score {} unless score {} = {}",
                    self.left.to_score(),
                    self.left.to_score(),
                    self.right.to_score()
                )]
            }
            IrScoreOperationType::And => {
                vec![format!(
                    "execute store result score {} unless score {} matches 0 unless score {} matches 0",
                    self.left.to_score(),
                    self.left.to_score(),
                    self.right.to_score()
                )]
            }
            IrScoreOperationType::Or => {
                vec![
                    format!(
                        "scoreboard players set {} 1",
                        self.left.to_score(),
                    ),
                    format!(
                        "execute if score {} matches 0 if score {} matches 0 scoreboard players set {} 0",
                        self.left.to_score(),
                        self.right.to_score(),
                        self.left.to_score(),
                    )]
            }
        }
    }
}

fn if_unless_helper(cond: &Cond, body: &Box<IrStatement>, type_: &str) -> Vec<String> {
    let mut result = vec![];
    result.push(match &cond {
        Cond::CheckVal(x) => {
            if x.min == x.max {
                format!("execute {type_} score {} matches {} run", x.var_name.to_score(), x.min)
            } else {
                format!(
                    "execute {type_} score {} matches {}..{} run",
                    x.var_name.to_score(),
                    x.min,
                    x.max
                )
            }
        }
        Cond::CompareVal(x) => {
            let op = match x.op {
                crate::middle::format::ir_types::CompareOp::Eq => "matches",
                crate::middle::format::ir_types::CompareOp::Neq => "matches",
                crate::middle::format::ir_types::CompareOp::Lt => "matches",
                crate::middle::format::ir_types::CompareOp::Gt => "matches",
                crate::middle::format::ir_types::CompareOp::Leq => "matches",
                crate::middle::format::ir_types::CompareOp::Geq => "matches",
            };
            format!(
                "execute {type_} score {} {} {} run",
                x.var_0.to_score(),
                op,
                x.var_1.to_score()
            )
        }
    });
    for statement in &body {
        result.append(&mut statement.generate());
    }
    result
}

impl CodeGenerator for IrIf {
    fn generate(&self) -> Vec<String> {
        if_unless_helper(&self.cond, &self.body, "if")
    }
}

impl CodeGenerator for IrUnless {
    fn generate(&self) -> Vec<String> {
        if_unless_helper(&self.cond, &self.body, "unless")
    }
}

impl CodeGenerator for IrStatement {
    fn generate(&self) -> Vec<String> {
        match self {
            IrStatement::ScoreSet(x) => x.generate(),
            IrStatement::ScoreAddI(x) => x.generate(),
            IrStatement::ScoreOperation(x) => x.generate(),
            IrStatement::If(x) => x.generate(),
            IrStatement::Unless(x) => x.generate(),
            IrStatement::FnCall(x) => x.generate(),
            IrStatement::Return => vec!["return".to_string()],
            IrStatement::Block(x) => x.generate()
        }
    }
}

impl CodeGenerator for IrBlock {
    fn generate(&self) -> Vec<String> {
        let mut result = vec![];
        for statement in &self.statements {
            result.append(&mut statement.generate());
        }
        result
    }
}