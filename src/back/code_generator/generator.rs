use crate::middle::format::ir_types::{CompareOp};
use crate::back::code_generator::{Context, GeneratedCode, MFunction};
use crate::middle::format::ir_types::{Cond, IrUnless};
use crate::middle::format::ir_types::{Address, IrBlock, IrIf, IrScoreSet, IrStatement};
use crate::middle::format::ir_types::{
    AddressOrigin, IrScoreAddI, IrScoreOperation, IrScoreOperationType,
};

static BLASTFURNACE_OBJECTIVE: &str = "blst";
static BLASTFURNACE_CONST: &str = "blst";

pub trait CodeGenerator {
    fn generate(&self, generated_code: &mut GeneratedCode, context: &mut Context) -> Vec<String>;
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
                    format!("c{x} {BLASTFURNACE_CONST}")
                } else {
                    format!("cn{x} {BLASTFURNACE_CONST}")
                }
            }
        }
    }
}

impl CodeGenerator for IrScoreSet {
    fn generate(&self, _generated_code: &mut GeneratedCode, _context: &mut Context) -> Vec<String> {
        vec![format!(
            "scoreboard players set {} {}",
            self.var_name.to_score(),
            self.value
        )]
    }
}

impl CodeGenerator for IrScoreAddI {
    fn generate(&self, _generated_code: &mut GeneratedCode, _context: &mut Context) -> Vec<String> {
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
    fn generate(&self, _generated_code: &mut GeneratedCode, _context: &mut Context) -> Vec<String> {
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
        match self.right.name {
            AddressOrigin::Const(x) => {
                match self.op {
                    IrScoreOperationType::Add
                    | IrScoreOperationType::Sub => {
                        let mut con = if self.op == IrScoreOperationType::Sub {
                            -x
                        } else {
                            x
                        };

                        return if con >= 0 {
                            vec![format!(
                                "scoreboard players add {} {}",
                                self.left.to_score(),
                                con
                            )]
                        } else {
                            vec![format!(
                                "scoreboard players remove {} {}",
                                self.left.to_score(),
                                -con
                            )]
                        };
                    }
                    _ => {}
                }
            }
            _ => {}
        }

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

fn if_unless_helper(generated_code: &mut GeneratedCode, context: &mut Context, cond: &Cond, body: &Box<IrStatement>, type_: &str) -> Vec<String> {
    let mut statements = body.generate(generated_code, context);

    let statement = if statements.len() == 1 {
        statements.remove(0)
    } else {
        wrap_in_function(statements, generated_code, context)
    };

    vec![format!("{} {}", match &cond {
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
                CompareOp::Eq => "matches",
                CompareOp::Neq => "matches",
                CompareOp::Lt => "matches",
                CompareOp::Gt => "matches",
                CompareOp::Leq => "matches",
                CompareOp::Geq => "matches",
            };
            format!(
                "execute {type_} score {} {} {} run",
                x.var_0.to_score(),
                op,
                x.var_1.to_score()
            )
        }
    }, statement)]
}

impl CodeGenerator for IrIf {
    fn generate(&self, generated_code: &mut GeneratedCode, context: &mut Context) -> Vec<String> {
        if_unless_helper(generated_code, context, &self.cond, &self.body, "if")
    }
}

impl CodeGenerator for IrUnless {
    fn generate(&self, generated_code: &mut GeneratedCode, context: &mut Context) -> Vec<String> {
        if_unless_helper(generated_code, context, &self.cond, &self.body, "unless")
    }
}

impl CodeGenerator for IrStatement {
    fn generate(&self, generated_code: &mut GeneratedCode, context: &mut Context) -> Vec<String> {
        match self {
            IrStatement::ScoreSet(x) => x.generate(generated_code, context),
            IrStatement::ScoreAddI(x) => x.generate(generated_code, context),
            IrStatement::ScoreOperation(x) => x.generate(generated_code, context),
            IrStatement::If(x) => x.generate(generated_code, context),
            IrStatement::Unless(x) => x.generate(generated_code, context),
            IrStatement::FnCall(x) => vec![format!("function {}", x.fn_name)],
            IrStatement::Return => vec!["return".to_string()],
            IrStatement::Block(x) => x.generate(generated_code, context)
        }
    }
}

impl CodeGenerator for IrBlock {
    fn generate(&self, generated_code: &mut GeneratedCode, context: &mut Context) -> Vec<String> {
        let mut result = vec![];
        for statement in &self.statements {
            result.append(&mut statement.generate(generated_code, context));
        }

        if result.len() == 1 && self.can_embed {
            result
        } else {
            vec![wrap_in_function(result, generated_code, context)]
        }
    }
}

fn wrap_in_function(statements: Vec<String>, generated_code: &mut GeneratedCode, context: &mut Context) -> String {
    let block_name = context.new_block();

    generated_code.add_function(MFunction {
        name: block_name.clone(),
        body: statements,
    });

    format!("function {}", block_name)
}