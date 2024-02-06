use crate::front::mergers::definition_table::DefinitionTable;
use crate::middle::format::types::GlobalName;
use std::rc::Rc;

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
