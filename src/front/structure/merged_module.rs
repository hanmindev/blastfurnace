use crate::front::syntax::ast_types::{FnDef, GlobalResolvedName, StructDef, VarDef};
use std::collections::HashMap;

#[derive(Debug)]
pub struct MergedModule {
    pub function_definitions: HashMap<GlobalResolvedName, FnDef>,
    pub struct_definitions: HashMap<GlobalResolvedName, StructDef>,
    pub global_var_definitions: HashMap<GlobalResolvedName, VarDef>,
}

impl MergedModule {
    pub fn new() -> MergedModule {
        MergedModule {
            function_definitions: Default::default(),
            struct_definitions: Default::default(),
            global_var_definitions: Default::default(),
        }
    }
}
