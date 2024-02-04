use crate::front::file_system::fs::FileSystem;
use crate::front::lexical::lexer::Lexer;
use crate::front::module_resolution::definition_table::DefinitionTable;
use crate::front::module_resolution::module_merger::ModuleMerger;
use crate::front::name_resolution::name_resolver::resolve_module;
use crate::front::syntax::ast_types::Module;
use crate::front::syntax::parser::Parser;
use std::collections::HashMap;

type Source = String;
type Path = String;

#[derive(Debug, PartialEq)]
pub struct ModuleNode {
    pub source: Source,
    pub submodules: HashMap<Path, Option<bool>>,
    pub module: Option<Module>,
}

#[derive(Debug)]
pub struct Packager<T> {
    package_name: String,
    file_system: T,
    pub root: Option<Path>,
    pub modules: HashMap<Path, ModuleNode>,

    pub module_merger: Option<ModuleMerger>,
}

impl<T: FileSystem> Packager<T> {
    pub fn new(package_name: &str, file_system: T) -> Packager<T> {
        Packager {
            package_name: package_name.to_string(),
            file_system,
            root: None,
            modules: HashMap::new(),
            module_merger: Option::from(ModuleMerger::new(package_name)),
        }
    }

    fn read_nodes_rec(&mut self, parent_module: &mut ModuleNode) {
        let modules = self.file_system.ls_files_with_extension("ing");

        for full_file_name in modules {
            let file_name = full_file_name.strip_suffix(".ing").unwrap().to_string();
            let submodules = HashMap::new();

            let mut path = self.file_system.return_current_dir();
            path.push_str(&file_name);

            let mut module = ModuleNode {
                source: path.clone(),
                submodules,
                module: None,
            };

            if self.file_system.enter_dir(path.as_str()) {
                self.read_nodes_rec(&mut module);
                self.file_system.exit_dir();
            }

            parent_module.submodules.insert(path.clone(), None);
            self.modules.insert(path, module);
        }
    }

    fn read_nodes(&mut self) {
        let mut root = ModuleNode {
            source: self.file_system.return_current_dir(),
            submodules: HashMap::new(),
            module: None,
        };

        self.read_nodes_rec(&mut root);
        self.root = Some("main".to_string());
        if let Some(mut value) = self.modules.remove("/main") {
            assert_eq!(value.submodules.len(), 0);
            root.submodules
                .remove("/main")
                .expect("main module not found in submodules");
            value.submodules = root.submodules;
            self.modules.insert("/".to_string(), value);
        } else {
            panic!("main module not found");
        }
    }

    fn parse_files(&mut self, resolve_name: bool) {
        for module_node in self.modules.values_mut() {
            let mut source = module_node.source.clone();
            source.push_str(".ing");
            if let Ok(byte_stream) = self.file_system.read_file(&source) {
                let lexer = Lexer::new(byte_stream);
                let mut parser = Parser::new(lexer);
                let mut module = parser.parse_module().unwrap();

                if resolve_name {
                    resolve_module(&mut module);
                }
                module_node.module = Some(module);
            } else {
                panic!("File not found");
            }
        }
    }

    fn globalize_names(&mut self) {
        let merged_modules = self.module_merger.as_mut().unwrap();
        for (path, mut module_node) in self.modules.drain() {
            let module = module_node.module.as_mut().unwrap(); // if unwrap fails there's something wrong with the code
            merged_modules.switch_module(&path);
            merged_modules.merge_module(module);
        }
    }

    pub fn merge_modules(&mut self) -> DefinitionTable {
        self.read_nodes();
        self.parse_files(true);
        self.globalize_names();

        return self.module_merger.take().unwrap().definition_table;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::front::file_system::fs::MockFileSystem;
    use crate::front::syntax::ast_types::Type::Void;
    use crate::front::syntax::ast_types::{
        AtomicExpression, Block, Expression, FnCall, FnDef, GlobalResolvedName, Reference,
        Statement, StatementBlock,
    };
    use std::rc::Rc;

    #[test]
    fn test_read_rec() {
        let mut mock_file_system = MockFileSystem::new("/".to_string());
        mock_file_system.insert_file("/main.ing", "fn main() {}");
        mock_file_system.insert_file("/test.ing", "pub mod example;");
        mock_file_system.insert_dir("/test/");
        mock_file_system.insert_file("/test/example.ing", "pub fn a() {};");

        let mut program = Packager::new("pkg", mock_file_system);
        program.read_nodes();

        assert_eq!(program.modules.len(), 3);
        assert_eq!(
            program.modules.get("/"),
            Some(&ModuleNode {
                source: "/main".to_string(),
                submodules: HashMap::from([("/test".to_string(), None)]),
                module: None,
            })
        );
        assert_eq!(
            program.modules.get("/test"),
            Some(&ModuleNode {
                source: "/test".to_string(),
                submodules: HashMap::from([("/test/example".to_string(), None)]),
                module: None,
            })
        );
        assert_eq!(
            program.modules.get("/test/example"),
            Some(&ModuleNode {
                source: "/test/example".to_string(),
                submodules: HashMap::new(),
                module: None,
            })
        );
    }

    #[test]
    fn test_parse_files() {
        let mut mock_file_system = MockFileSystem::new("/".to_string());
        mock_file_system.insert_file("/main.ing", "fn main() {}");
        mock_file_system.insert_file("/test.ing", "pub mod example;");
        mock_file_system.insert_dir("/test/");
        mock_file_system.insert_file("/test/example.ing", "pub fn a() {};");

        let mut program = Packager::new("pkg", mock_file_system);
        program.read_nodes();
        program.parse_files(true);
        program.globalize_names();

        assert_eq!(program.modules.len(), 0);

        let name_map = &program.module_merger.unwrap().definition_table;

        assert_eq!(name_map.function_definitions.len(), 2);
        assert_eq!(name_map.struct_definitions.len(), 0);
        assert_eq!(name_map.global_var_definitions.len(), 0);

        let gr = Rc::from(GlobalResolvedName {
            module: "/".to_string(),
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
            module: "/test/example".to_string(),
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
            "use root::test::example::a; fn main() { a(); }",
        );
        mock_file_system.insert_file("/test.ing", "pub mod example;");
        mock_file_system.insert_dir("/test/");
        mock_file_system.insert_file("/test/example.ing", "pub fn a() {};");

        let mut program = Packager::new("pkg", mock_file_system);
        program.read_nodes();
        program.parse_files(true);
        program.globalize_names();

        assert_eq!(program.modules.len(), 0);

        let name_map = &program.module_merger.unwrap().definition_table;

        assert_eq!(name_map.function_definitions.len(), 2);
        assert_eq!(name_map.struct_definitions.len(), 0);
        assert_eq!(name_map.global_var_definitions.len(), 0);

        let gr = Rc::from(GlobalResolvedName {
            module: "/".to_string(),
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
            module: "/test/example".to_string(),
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
        mock_file_system.insert_file("/main.ing", "use std::test::example::a; fn main() { a(); }");
        mock_file_system.insert_file("/test.ing", "pub mod example;");
        mock_file_system.insert_dir("/test/");
        mock_file_system.insert_file("/test/example.ing", "pub fn a() {};");

        let mut program = Packager::new("pkg", mock_file_system);
        program.read_nodes();
        program.parse_files(true);
        program.globalize_names();

        assert_eq!(program.modules.len(), 0);

        let name_map = &program.module_merger.unwrap().definition_table;

        assert_eq!(name_map.function_definitions.len(), 2);
        assert_eq!(name_map.struct_definitions.len(), 0);
        assert_eq!(name_map.global_var_definitions.len(), 0);

        let gr = Rc::from(GlobalResolvedName {
            module: "/".to_string(),
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
