use crate::front::ast_types::{FnDef, StructDef, VarDecl};
use std::collections::HashMap;

#[derive(Debug)]
pub struct DefinitionTable<T> {
    pub function_definitions: HashMap<T, FnDef>,
    pub struct_definitions: HashMap<T, StructDef>,
    pub global_var_definitions: HashMap<T, VarDecl>,
}

impl<T> DefinitionTable<T> {
    pub fn new() -> DefinitionTable<T> {
        DefinitionTable {
            function_definitions: Default::default(),
            struct_definitions: Default::default(),
            global_var_definitions: Default::default(),
        }
    }
}
