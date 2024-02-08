use crate::middle::format::ir_types::IrFnDef;
use std::collections::{HashMap, HashSet};

pub type GlobalName = String;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub public_functions: HashSet<GlobalName>,
    pub function_definitions: HashMap<GlobalName, IrFnDef>,
}
