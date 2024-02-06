use crate::front::ast_retriever::retriever::FileRetriever;
use crate::front::file_system::fs::FileSystem;
use crate::front::mergers::package::{Package, Packager};
use crate::middle::format::types::{GlobalName, Program};
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;

pub struct ProgramMerger<R> {
    packages: HashMap<String, Package>,
    file_system_type: PhantomData<R>,
}

impl<R: FileSystem> ProgramMerger<R> {
    pub fn new() -> ProgramMerger<R> {
        ProgramMerger {
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
        let mut p = Program {
            public_functions: HashSet::new(),
            function_definitions: HashMap::new(),
            struct_definitions: HashMap::new(),
            global_var_definitions: HashMap::new(),
        };

        // for (_, mut table) in self.packages.drain() {
        //     for def in table.public_definitions.function_definitions.drain() {
        //         p.function_definitions.insert(
        //             GlobalName {
        //                 module: def.0.as_ref().module.clone(),
        //                 name: (*def.0.name).clone(),
        //             },
        //             def.1,
        //         );
        //     }
        // }

        p
    }
}
