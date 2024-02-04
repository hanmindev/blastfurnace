use crate::front::module_resolution::definition_table::DefinitionTable;
use crate::front::syntax::ast_types::{GlobalResolvedName, ResolvedName};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct ModuleMerger {
    pub global_name_table: HashMap<Rc<ResolvedName>, Rc<GlobalResolvedName>>,
    pub definition_table: DefinitionTable,
}

impl ModuleMerger {
    pub fn new() -> ModuleMerger {
        ModuleMerger {
            global_name_table: HashMap::new(),
            definition_table: DefinitionTable::new(),
        }
    }
}
