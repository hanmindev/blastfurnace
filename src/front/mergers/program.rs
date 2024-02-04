use crate::front::ast_retriever::retriever::FileRetriever;
use crate::front::file_system::fs::FileSystem;
use crate::front::mergers::package::Packager;
use crate::front::module_resolution::definition_table::DefinitionTable;
use std::collections::HashMap;
use std::marker::PhantomData;
use crate::middle::format::types::Program;

struct ProgramMerger<R> {
    packages: HashMap<String, DefinitionTable>,
    file_system_type: PhantomData<R>,
}

impl<R: FileSystem> ProgramMerger<R> {
    pub fn new() -> ProgramMerger<R> {
        ProgramMerger {
            packages: HashMap::new(),
            file_system_type: PhantomData,
        }
    }

    pub fn read_package(&mut self, package_name: &str, file_system: R) -> &mut DefinitionTable {
        let mut packager = Packager::new(package_name, FileRetriever::new(file_system));
        self.packages
            .insert(package_name.to_string(), packager.merge_modules());
        self.packages.get_mut(package_name).unwrap()
    }

    pub fn export_program(&mut self) -> Program {
        let mut p = Program {
            function_definitions: HashMap::new(),
            struct_definitions: HashMap::new(),
            global_var_definitions: HashMap::new(),
        };

        for (_, table) in self.packages.drain() {
            p.function_definitions.extend(table.function_definitions);
            p.struct_definitions.extend(table.struct_definitions);
            p.global_var_definitions.extend(table.global_var_definitions);
        }

        p
    }
}
