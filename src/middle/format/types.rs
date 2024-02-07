use crate::middle::format::ir_types::{IrFnDef, IrStructDef, IrVarDecl};
use std::collections::{HashMap, HashSet};

pub type GlobalName = String;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub public_functions: HashSet<GlobalName>,
    pub function_definitions: HashMap<GlobalName, IrFnDef>,
    pub struct_definitions: HashMap<GlobalName, IrStructDef>,
    pub global_var_definitions: HashMap<GlobalName, IrVarDecl>,
}
