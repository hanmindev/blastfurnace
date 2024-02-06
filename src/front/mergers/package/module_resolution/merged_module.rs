use crate::front::mergers::definition_table::DefinitionTable;
use std::rc::Rc;
use crate::middle::format::types::GlobalName;

#[derive(Debug)]
pub struct MergedModule {
    pub public_definitions: DefinitionTable<Rc<GlobalName>>,
    pub private_definitions: DefinitionTable<Rc<GlobalName>>,
}

impl MergedModule {
    pub fn new() -> MergedModule {
        MergedModule {
            public_definitions: DefinitionTable::new(),
            private_definitions: DefinitionTable::new(),
        }
    }
}
