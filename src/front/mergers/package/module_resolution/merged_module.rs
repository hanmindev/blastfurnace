use crate::front::ast_types::{GlobalResolvedName};
use std::rc::Rc;
use crate::front::mergers::definition_table::DefinitionTable;

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