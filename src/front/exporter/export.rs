use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use crate::front::ast_types::GlobalResolvedName;
use crate::middle::format::types::{Program};
use crate::front::exporter::convert::context::ConstGenerator;
use crate::front::exporter::convert::{convert_fn, global_name_updater};
use crate::front::mergers::definition_table::DefinitionTable;


#[derive(Debug)]
pub struct FrontProgram {
    pub public_functions: HashSet<Rc<GlobalResolvedName>>,
    pub definitions: DefinitionTable<Rc<GlobalResolvedName>>,
}

impl FrontProgram {
    pub fn export_program(&self) -> Program {
        let mut program = Program {
            public_functions: self.public_functions.iter().map(|x| global_name_updater(x)).collect(),
            function_definitions: HashMap::new(),
        };

        let mut const_generator = ConstGenerator::new();

        for public_function in &self.public_functions {
            if let Some(fn_) = self.definitions.function_definitions.get(public_function) {
                program.function_definitions.insert(
                    global_name_updater(&public_function),
                    convert_fn(fn_, &self.definitions, &mut const_generator),
                );
            }
        }

        program
    }

}