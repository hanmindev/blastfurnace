use crate::front::ast_types::{FnDef, StructDef, VarDecl};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct GlobalName {
    pub module: String,
    pub name: String,
}
#[derive(Debug, PartialEq)]
pub struct Program {
    pub function_definitions: HashMap<GlobalName, FnDef>,
    pub struct_definitions: HashMap<GlobalName, StructDef>,
    pub global_var_definitions: HashMap<GlobalName, VarDecl>,
}
