use crate::front::syntax::ast_types::{FnDef, GlobalResolvedName, StructDef, VarDecl};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct MergedModule {
    pub public_definitions: DefinitionTable,
    pub private_definitions: DefinitionTable,
}

impl MergedModule {
    pub fn new() -> MergedModule {
        MergedModule {
            public_definitions: DefinitionTable::new(),
            private_definitions: DefinitionTable::new(),
        }
    }
}

#[derive(Debug)]
pub struct DefinitionTable {
    pub function_definitions: HashMap<Rc<GlobalResolvedName>, FnDef>,
    pub struct_definitions: HashMap<Rc<GlobalResolvedName>, StructDef>,
    pub global_var_definitions: HashMap<Rc<GlobalResolvedName>, VarDecl>,
}

impl DefinitionTable {
    pub fn new() -> DefinitionTable {
        DefinitionTable {
            function_definitions: Default::default(),
            struct_definitions: Default::default(),
            global_var_definitions: Default::default(),
        }
    }
}