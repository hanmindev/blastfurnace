use crate::back::code_generator::{Context, GeneratedCode, MFunction};
use crate::middle::format::ir_types::AddressOrigin::Const;
use crate::middle::format::ir_types::CompareOp;
use crate::middle::format::ir_types::Cond;
use crate::middle::format::ir_types::{Address, IrBlock, IrIf, IrStatement};
use crate::middle::format::ir_types::{AddressOrigin, IrScoreOperation, IrScoreOperationType};

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
            AddressOrigin::Const(x) => match self.op {
                IrScoreOperationType::Add | IrScoreOperationType::Sub => {
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
                IrScoreOperationType::Assign => {
                    return vec![format!(
                        "scoreboard players set {} {}",
                        self.left.to_score(),
                        x
                    )];
                }
                _ => {}
            },
            _ => {}
        }

        match self.op {
            IrScoreOperationType::Add
            | IrScoreOperationType::Sub
            | IrScoreOperationType::Mul
            | IrScoreOperationType::Div
            | IrScoreOperationType::Mod
            | IrScoreOperationType::Assign => {
                if self.left != self.right {
                    vec![format!(
                        "scoreboard players operation {} {} {}",
                        self.left.to_score(),
                        op,
                        self.right.to_score()
                    )]
                } else {
                    vec![]
                }
            }
            IrScoreOperationType::Leq
            | IrScoreOperationType::Geq
            | IrScoreOperationType::Lt
            | IrScoreOperationType::Gt
            | IrScoreOperationType::Eq => {
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

fn if_unless_helper(
    generated_code: &mut GeneratedCode,
    context: &mut Context,
    cond: &Cond,
    body: &Box<IrStatement>,
    invert: bool,
) -> Vec<String> {
    let mut statements = body.generate(generated_code, context);

    let statement = if statements.len() == 1 {
        statements.remove(0)
    } else {
        wrap_in_function(statements, generated_code, context)
    };

    vec![format!(
        "{} {}",
        match &cond {
            Cond::CheckVal(x) => {
                let type_ = if invert { "unless" } else { "if" };
                if x.min == x.max {
                    format!(
                        "execute {type_} score {} matches {} run",
                        x.var_name.to_score(),
                        x.min
                    )
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
                let mut type_ = "if";

                let op = match x.op {
                    CompareOp::Eq => {
                        if invert {
                            type_ = "unless";
                        }
                        "="
                    }
                    CompareOp::Neq => {
                        if !invert {
                            type_ = "unless";
                        }
                        "="
                    }
                    CompareOp::Lt => {
                        if invert {
                            ">"
                        } else {
                            "<"
                        }
                    }
                    CompareOp::Gt => {
                        if invert {
                            "<"
                        } else {
                            ">"
                        }
                    }
                    CompareOp::Leq => {
                        if invert {
                            ">="
                        } else {
                            "<="
                        }
                    }
                    CompareOp::Geq => {
                        if invert {
                            "<="
                        } else {
                            ">="
                        }
                    }
                };

                // should be type_ var0 op var1

                if let Const(c1) = x.var_1.name {
                    if let Const(c0) = x.var_0.name {
                        return if match op {
                            "=" => (c0 == c1 && type_ == "if") || c0 != c1 && type_ == "unless",
                            "<" => c0 < c1,
                            ">" => c0 > c1,
                            "<=" => c0 <= c1,
                            ">=" => c0 >= c1,
                            _ => {
                                panic!("Invalid op, match arms must be insufficient")
                            }
                        } {
                            vec![statement]
                        } else {
                            vec![]
                        };
                    }

                    let range = match op {
                        "=" => c1.to_string(),
                        "<" => {
                            format!("..{}", c1 - 1)
                        }
                        ">" => {
                            format!("{}..", c1 + 1)
                        }
                        "<=" => {
                            format!("..{}", c1)
                        }
                        ">=" => {
                            format!("{}..", c1)
                        }
                        _ => {
                            panic!("Invalid op, match arms must be insufficient")
                        }
                    };
                    format!(
                        "execute {type_} score {} matches {} run",
                        x.var_0.to_score(),
                        range
                    )
                } else {
                    format!(
                        "execute {type_} score {} {} {} run",
                        x.var_0.to_score(),
                        op,
                        x.var_1.to_score()
                    )
                }
            }
        },
        statement
    )]
}

impl CodeGenerator for IrIf {
    fn generate(&self, generated_code: &mut GeneratedCode, context: &mut Context) -> Vec<String> {
        if_unless_helper(generated_code, context, &self.cond, &self.body, self.invert)
    }
}

impl CodeGenerator for IrStatement {
    fn generate(&self, generated_code: &mut GeneratedCode, context: &mut Context) -> Vec<String> {
        match self {
            IrStatement::ScoreOperation(x) => x.generate(generated_code, context),
            IrStatement::If(x) => x.generate(generated_code, context),
            IrStatement::FnCall(x) => vec![format!("function {}", x.fn_name)],
            IrStatement::Return => vec!["return".to_string()],
            IrStatement::Block(x) => x.generate(generated_code, context),
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

fn wrap_in_function(
    statements: Vec<String>,
    generated_code: &mut GeneratedCode,
    context: &mut Context,
) -> String {
    let block_name = context.new_block();

    generated_code.add_function(MFunction {
        name: block_name.clone(),
        body: statements,
    });

    format!("function {}", block_name)
}
