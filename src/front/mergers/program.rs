use crate::front::ast_retriever::retriever::FileRetriever;
use crate::front::file_system::fs::FileSystem;
use crate::front::mergers::package::{Package, Packager};
use crate::middle::format::types::{GlobalName, Program};
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;
use std::rc::Rc;
use crate::front::ast_types::GlobalResolvedName;

fn global_name_updater(package_name: &str, global_resolved_name: Rc<GlobalResolvedName>) -> GlobalName {
    format!("{}/{}/{}", package_name, global_resolved_name.module, global_resolved_name.name)
}

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
            struct_definitions: HashMap::new(),
            global_var_definitions: HashMap::new(),
        };

        for (package_name, mut table) in self.packages.drain() {
            if package_name == self.root_package {
                for def in table
                    .merged_module
                    .public_definitions
                    .function_definitions
                    .drain()
                {
                    program.public_functions.insert(global_name_updater(&package_name, def.0));
                }
            }

            for def in table
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
                )
            {
                program.function_definitions.insert(global_name_updater(&package_name, def.0), def.1);
            }
            for def in table
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
                )
            {
                program.struct_definitions.insert(global_name_updater(&package_name, def.0), def.1);
            }
            for def in table
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
                )
            {
                program.global_var_definitions.insert(global_name_updater(&package_name, def.0), def.1);
            }
        }

        program
    }
}
