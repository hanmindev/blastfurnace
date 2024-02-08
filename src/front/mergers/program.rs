use crate::front::ast_retriever::retriever::FileRetriever;
use crate::front::file_system::fs::FileSystem;
use crate::front::mergers::convert::{convert_fn, global_name_updater};
use crate::front::mergers::definition_table::DefinitionTable;
use crate::front::mergers::package::{Package, Packager};
use crate::middle::format::types::Program;
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;
use std::rc::Rc;
use crate::front::mergers::convert::context::ConstGenerator;

pub struct ProgramMerger<R> {
    root_package: String,
    packages: HashMap<String, Package>,
    file_system_type: PhantomData<R>,
}

impl<R: FileSystem> ProgramMerger<R> {
    pub fn new(root: &str) -> ProgramMerger<R> {
        ProgramMerger {
            root_package: root.to_string(),
            packages: HashMap::new(),
            file_system_type: PhantomData,
        }
    }

    pub fn read_package(&mut self, package_name: &str, file_system: R) -> &mut Package {
        let mut packager = Packager::new(package_name, FileRetriever::new(file_system));
        self.packages
            .insert(package_name.to_string(), packager.merge_modules());
        self.packages.get_mut(package_name).unwrap()
    }

    pub fn export_program(&mut self) -> Program {
        let mut program = Program {
            public_functions: HashSet::new(),
            function_definitions: HashMap::new(),
        };

        let mut public_functions = HashSet::new();
        let mut def_table = DefinitionTable::new();

        for (package_name, mut table) in self.packages.drain() {
            if package_name == self.root_package {
                for def in &table.merged_module.public_definitions.function_definitions {
                    public_functions.insert(Rc::clone(def.0));
                    program.public_functions.insert(global_name_updater(&def.0));
                }
            }

            def_table.function_definitions.extend(
                table
                    .merged_module
                    .public_definitions
                    .function_definitions
                    .drain()
                    .chain(
                        table
                            .merged_module
                            .private_definitions
                            .function_definitions
                            .drain(),
                    ),
            );

            def_table.struct_definitions.extend(
                table
                    .merged_module
                    .public_definitions
                    .struct_definitions
                    .drain()
                    .chain(
                        table
                            .merged_module
                            .private_definitions
                            .struct_definitions
                            .drain(),
                    ),
            );

            def_table.global_var_definitions.extend(
                table
                    .merged_module
                    .public_definitions
                    .global_var_definitions
                    .drain()
                    .chain(
                        table
                            .merged_module
                            .private_definitions
                            .global_var_definitions
                            .drain(),
                    ),
            );
        }

        let mut const_generator = ConstGenerator::new();

        for public_function in public_functions {
            if let Some(fn_) = def_table.function_definitions.get(&public_function) {
                program.function_definitions.insert(
                    global_name_updater(&public_function),
                    convert_fn(fn_, &def_table, &mut const_generator),
                );
            }
        }

        program
    }
}
