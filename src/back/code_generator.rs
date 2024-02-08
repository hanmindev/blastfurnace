use crate::middle::format::ir_types::{Address, IrScoreSet};
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