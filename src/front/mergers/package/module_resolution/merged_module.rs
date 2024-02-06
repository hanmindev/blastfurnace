use crate::front::ast_types::{FnDef, GlobalResolvedName, StructDef, VarDecl};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct MergedModule {
    pub public_definitions: DefinitionTable<Rc<GlobalResolvedName>>,
    pub private_definitions: DefinitionTable<Rc<GlobalResolvedName>>,
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
