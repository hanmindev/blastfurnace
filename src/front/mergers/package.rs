use crate::front::ast_retriever::retriever::FileRetriever;
use crate::front::file_system::fs::FileSystem;
use crate::front::module_resolution::definition_table::DefinitionTable;
use crate::front::module_resolution::module_merger::ModuleMerger;

#[derive(Debug)]
pub struct Packager<T> {
    package_name: String,
    retriever: Option<FileRetriever<T>>,

    pub module_merger: Option<ModuleMerger>,
}

impl<T> Packager<T> {
    pub fn new(package_name: &str, retriever: FileRetriever<T>) -> Packager<T> {
        Packager {
            package_name: package_name.to_string(),
            retriever: Some(retriever),
            module_merger: Option::from(ModuleMerger::new(package_name)),
        }
    }

    fn globalize_names(&mut self) {
        let module_merger = self.module_merger.as_mut().unwrap();
        module_merger.merge_modules(self.retriever.take().unwrap().modules);
    }

    pub fn merge_modules(&mut self) -> DefinitionTable {
        self.globalize_names();

        return self.module_merger.take().unwrap().definition_table;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::front::file_system::mock_fs::MockFileSystem;
    use crate::front::syntax::ast_types::Type::Void;
    use crate::front::syntax::ast_types::{
        AtomicExpression, Block, Expression, FnCall, FnDef, GlobalResolvedName, Reference,
        Statement, StatementBlock,
    };
    use std::rc::Rc;

    #[test]
    fn test_parse_files() {
        let mut mock_file_system = MockFileSystem::new("/".to_string());
        mock_file_system.insert_file("/main.ing", "mod test; fn main() {}");
        mock_file_system.insert_file("/test.ing", "pub mod example;");
        mock_file_system.insert_dir("/test/");
        mock_file_system.insert_file("/test/example.ing", "pub fn a() {};");

        let mut program = Packager::new("pkg", FileRetriever::new(mock_file_system));
        program.globalize_names();

        let name_map = &program.module_merger.unwrap().definition_table;

        assert_eq!(name_map.function_definitions.len(), 2);
        assert_eq!(name_map.struct_definitions.len(), 0);
        assert_eq!(name_map.global_var_definitions.len(), 0);

        let gr = Rc::from(GlobalResolvedName {
            module: "/root".to_string(),
            name: Rc::from("0_main".to_string()),
        });

        assert_eq!(
            name_map.function_definitions.get(&gr),
            Some(&FnDef {
                name: Reference {
                    raw: "main".to_string(),
                    module_resolved: Some(Rc::from("0_main".to_string())),
                    global_resolved: Some(gr)
                },
                return_type: Void,
                body: Some(Block {
                    definitions: vec![],
                    statements: vec![]
                }),
                mods: Rc::new(vec![]),
                args: vec![],
            })
        );

        let gr = Rc::from(GlobalResolvedName {
            module: "/root/test/example".to_string(),
            name: Rc::from("0_a".to_string()),
        });

        assert_eq!(
            name_map.function_definitions.get(&gr),
            Some(&FnDef {
                name: Reference {
                    raw: "a".to_string(),
                    module_resolved: Some(Rc::from("0_a".to_string())),
                    global_resolved: Some(gr)
                },
                return_type: Void,
                body: Some(Block {
                    definitions: vec![],
                    statements: vec![]
                }),
                mods: Rc::new(vec![]),
                args: vec![],
            })
        );
    }

    #[test]
    fn test_import_files() {
        let mut mock_file_system = MockFileSystem::new("/".to_string());
        mock_file_system.insert_file(
            "/main.ing",
            "mod test; use root::test::example::a; fn main() { a(); }",
        );
        mock_file_system.insert_file("/test.ing", "pub mod example;");
        mock_file_system.insert_dir("/test/");
        mock_file_system.insert_file("/test/example.ing", "pub fn a() {};");

        let mut program = Packager::new("pkg", FileRetriever::new(mock_file_system));
        program.globalize_names();

        let name_map = &program.module_merger.unwrap().definition_table;

        assert_eq!(name_map.function_definitions.len(), 2);
        assert_eq!(name_map.struct_definitions.len(), 0);
        assert_eq!(name_map.global_var_definitions.len(), 0);

        let gr = Rc::from(GlobalResolvedName {
            module: "/root".to_string(),
            name: Rc::from("0_main".to_string()),
        });

        assert_eq!(
            name_map.function_definitions.get(&gr),
            Some(&FnDef {
                name: Reference {
                    raw: "main".to_string(),
                    module_resolved: Some(Rc::from("0_main".to_string())),
                    global_resolved: Some(gr)
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
                                    module: "/test/example".to_string(),
                                    name: Rc::from("0_a".to_string()),
                                })),
                            },
                            args: vec![],
                        }))),
                    )))]
                }),
                mods: Rc::new(vec![]),
                args: vec![],
            })
        );

        let gr = Rc::from(GlobalResolvedName {
            module: "/root/test/example".to_string(),
            name: Rc::from("0_a".to_string()),
        });

        assert_eq!(
            name_map.function_definitions.get(&gr),
            Some(&FnDef {
                name: Reference {
                    raw: "a".to_string(),
                    module_resolved: Some(Rc::from("0_a".to_string())),
                    global_resolved: Some(gr)
                },
                return_type: Void,
                body: Some(Block {
                    definitions: vec![],
                    statements: vec![]
                }),
                mods: Rc::new(vec![]),
                args: vec![],
            })
        );
    }

    #[test]
    fn test_import_cross_package() {
        let mut mock_file_system = MockFileSystem::new("/".to_string());
        mock_file_system.insert_file("/main.ing", "mod test; use std::test::example::a; fn main() { a(); }");
        mock_file_system.insert_file("/test.ing", "pub mod example;");
        mock_file_system.insert_dir("/test/");
        mock_file_system.insert_file("/test/example.ing", "pub fn a() {};");

        let mut program = Packager::new("pkg", FileRetriever::new(mock_file_system));
        program.globalize_names();

        let name_map = &program.module_merger.unwrap().definition_table;

        assert_eq!(name_map.function_definitions.len(), 2);
        assert_eq!(name_map.struct_definitions.len(), 0);
        assert_eq!(name_map.global_var_definitions.len(), 0);

        let gr = Rc::from(GlobalResolvedName {
            module: "/root".to_string(),
            name: Rc::from("0_main".to_string()),
        });

        assert_eq!(
            name_map
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
                            module: "std/test/example".to_string(),
                            name: Rc::from("0_a".to_string()),
                        })),
                    },
                    args: vec![],
                }))),
            )))
        );
    }
}
