use std::collections::HashMap;
use crate::front::syntax::ast_types::{FnDef, StructDef, VarDecl};

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct GlobalName {
    pub module: String,
    pub name: String,
}
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Program {
    pub function_definitions: HashMap<GlobalName, FnDef>,
    pub struct_definitions: HashMap<GlobalName, StructDef>,
    pub global_var_definitions: HashMap<GlobalName, VarDecl>,
}