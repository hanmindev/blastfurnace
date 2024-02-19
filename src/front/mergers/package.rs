use crate::front::ast_retriever::retriever::FileRetriever;
use crate::front::mergers::package::module_resolution::merged_module::MergedModule;
use crate::front::mergers::package::module_resolution::module_merger::ModuleMerger;

mod module_resolution;

#[derive(Debug)]
pub struct Package {
    pub merged_module: MergedModule,
}

#[derive(Debug)]
pub struct Packager<T> {
    package_name: String,
    retriever: Option<FileRetriever<T>>,
}

impl<T> Packager<T> {
    pub fn new(package_name: &str, retriever: FileRetriever<T>) -> Packager<T> {
        Packager {
            package_name: package_name.to_string(),
            retriever: Some(retriever),
        }
    }

    pub fn merge_modules(&mut self) -> Package {
        let mut module_merger = ModuleMerger::new(&self.package_name);
        let merged_module = module_merger
            .merge_modules(self.retriever.take().unwrap().modules) //TODO: don't use unwrap
            .unwrap();

        Package { merged_module }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::front::ast_types::Type::Void;
    use crate::front::ast_types::{
        AtomicExpression, Block, Expression, FnCall, FnDef, GlobalResolvedName, Reference,
        Statement, StatementBlock,
    };
    use crate::front::file_system::fs::FileSystem;
    use crate::front::file_system::mock_fs::MockFileSystem;
    use std::rc::Rc;
    use camino::Utf8PathBuf;

    #[test]
    fn test_parse_files() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(
            Utf8PathBuf::from("main.ing"), "mod test; fn main() {}");
        mock_file_system.insert_file(
            Utf8PathBuf::from("test.ing"), "pub mod example;");
        mock_file_system.insert_dir(
            Utf8PathBuf::from("test"));
        mock_file_system.insert_file(
            Utf8PathBuf::from("test/example.ing"), "pub fn a() {};");

        let mut program = Packager::new("pkg", FileRetriever::new(mock_file_system));

        let definition_table = program.merge_modules().merged_module.private_definitions;

        assert_eq!(definition_table.function_definitions.len(), 2);
        assert_eq!(definition_table.struct_definitions.len(), 0);
        assert_eq!(definition_table.global_var_definitions.len(), 0);

        let gr = Rc::from(GlobalResolvedName {
            package: Rc::from("pkg"),
            module: Rc::from("/root"),
            name: "0_main".to_string(),
        });

        assert_eq!(
            definition_table.function_definitions.get(&gr),
            Some(&FnDef {
                name: Reference {
                    raw: "main".to_string(),
                    module_resolved: Some(Rc::from("0_main".to_string())),
                    global_resolved: Some(gr),
                },
                return_type: Void,
                body: Some(Block {
                    definitions: vec![],
                    statements: vec![],
                }),
                mods: Rc::new(vec![]),
                args: vec![],
            })
        );

        let gr = Rc::from(GlobalResolvedName {
            package: Rc::from("pkg"),
            module: Rc::from("/root/test/example"),
            name: "0_a".to_string(),
        });

        assert_eq!(
            definition_table.function_definitions.get(&gr),
            Some(&FnDef {
                name: Reference {
                    raw: "a".to_string(),
                    module_resolved: Some(Rc::from("0_a".to_string())),
                    global_resolved: Some(gr),
                },
                return_type: Void,
                body: Some(Block {
                    definitions: vec![],
                    statements: vec![],
                }),
                mods: Rc::new(vec![]),
                args: vec![],
            })
        );
    }

    #[test]
    fn test_import_files() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(
            Utf8PathBuf::from("main.ing"),
            "mod test; use root::test::example::a; fn main() { a(); }",
        );
        mock_file_system.insert_file(
            Utf8PathBuf::from("test.ing"), "pub mod example;");
        mock_file_system.insert_dir(Utf8PathBuf::from("test"));
        mock_file_system.insert_file(Utf8PathBuf::from("test/example.ing"), "pub fn a() {};");

        let mut program = Packager::new("pkg", FileRetriever::new(mock_file_system));

        let definition_table = program.merge_modules().merged_module.private_definitions;

        assert_eq!(definition_table.function_definitions.len(), 2);
        assert_eq!(definition_table.struct_definitions.len(), 0);
        assert_eq!(definition_table.global_var_definitions.len(), 0);

        let gr = Rc::from(GlobalResolvedName {
            package: Rc::from("pkg"),
            module: Rc::from("/root"),
            name: "0_main".to_string(),
        });

        assert_eq!(
            definition_table.function_definitions.get(&gr),
            Some(&FnDef {
                name: Reference {
                    raw: "main".to_string(),
                    module_resolved: Some(Rc::from("0_main".to_string())),
                    global_resolved: Some(gr),
                },
                return_type: Void,
                body: Some(Block {
                    definitions: vec![],
                    statements: vec![StatementBlock::Statement(Statement::Expression(Box::new(
                        Expression::AtomicExpression(AtomicExpression::FnCall(Box::new(FnCall {
                            name: Reference {
                                raw: "a".to_string(),
                                module_resolved: Some(Rc::from("0_a".to_string())),
                                global_resolved: Some(Rc::from(GlobalResolvedName {
                                    package: Rc::from("pkg"),
                                    module: Rc::from("/test/example"),
                                    name: "0_a".to_string(),
                                })),
                            },
                            args: vec![],
                        }))),
                    )))],
                }),
                mods: Rc::new(vec![]),
                args: vec![],
            })
        );

        let gr = Rc::from(GlobalResolvedName {
            package: Rc::from("pkg"),
            module: Rc::from("/root/test/example"),
            name: "0_a".to_string(),
        });

        assert_eq!(
            definition_table.function_definitions.get(&gr),
            Some(&FnDef {
                name: Reference {
                    raw: "a".to_string(),
                    module_resolved: Some(Rc::from("0_a".to_string())),
                    global_resolved: Some(gr),
                },
                return_type: Void,
                body: Some(Block {
                    definitions: vec![],
                    statements: vec![],
                }),
                mods: Rc::new(vec![]),
                args: vec![],
            })
        );
    }

    #[test]
    fn test_import_cross_package() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(
            Utf8PathBuf::from("main.ing"),
            "mod test; use std::test::example::a; fn main() { a(); }",
        );
        mock_file_system.insert_file(Utf8PathBuf::from("test.ing"), "pub mod example;");
        mock_file_system.insert_dir(Utf8PathBuf::from("test/"));
        mock_file_system.insert_file(Utf8PathBuf::from("test/example.ing"), "pub fn a() {};");

        let mut program = Packager::new("pkg", FileRetriever::new(mock_file_system));

        let definition_table = program.merge_modules().merged_module.private_definitions;

        assert_eq!(definition_table.function_definitions.len(), 2);
        assert_eq!(definition_table.struct_definitions.len(), 0);
        assert_eq!(definition_table.global_var_definitions.len(), 0);

        let gr = Rc::from(GlobalResolvedName {
            package: Rc::from("pkg"),
            module: Rc::from("/root"),
            name: "0_main".to_string(),
        });

        assert_eq!(
            definition_table
                .function_definitions
                .get(&gr)
                .as_ref()
                .unwrap()
                .body
                .as_ref()
                .unwrap()
                .statements[0],
            StatementBlock::Statement(Statement::Expression(Box::new(
                Expression::AtomicExpression(AtomicExpression::FnCall(Box::new(FnCall {
                    name: Reference {
                        raw: "a".to_string(),
                        module_resolved: Some(Rc::from("0_a".to_string())),
                        global_resolved: Some(Rc::from(GlobalResolvedName {
                            package: Rc::from("pkg"),
                            module: Rc::from("std/test/example"),
                            name: "0_a".to_string(),
                        })),
                    },
                    args: vec![],
                }))),
            )))
        );
    }
}
