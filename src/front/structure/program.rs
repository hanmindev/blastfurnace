use crate::front::file_system::fs::FileSystem;
use crate::front::lexical::lexer::Lexer;
use crate::front::name_resolution::resolver::Resolvable;
use crate::front::name_resolution::scope_table::ScopeTable;
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
pub struct Program<T> {
    file_system: T,
    pub root: Option<Path>,
    pub modules: HashMap<Path, ModuleNode>,
}

impl<T: FileSystem> Program<T> {
    pub fn new(file_system: T) -> Program<T> {
        Program {
            file_system,
            root: None,
            modules: HashMap::new(),
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

    pub fn read_nodes(&mut self) {
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
            self.modules.insert("main".to_string(), value);
        } else {
            panic!("main module not found");
        }
    }

    fn parse_files_rec(&mut self, module_path: Path, resolve_name: bool) {
        if let Some(module_node) = self.modules.get_mut(&module_path) {
            let mut source = module_node.source.clone();
            source.push_str(".ing");
            if let Ok(byte_stream) = self.file_system.read_file(&source) {
                let lexer = Lexer::new(byte_stream);
                let mut parser = Parser::new(lexer);
                let mut module = parser.parse_module().unwrap();

                if resolve_name {
                    let mut scope_table = ScopeTable::new();
                    module.resolve(&mut scope_table).unwrap();
                }
                module_node.module = Some(module);
            } else {
                panic!("File not found");
            }
        }
    }

    pub fn parse_files(&mut self, resolve_name: bool) {
        let root = self.root.as_ref().unwrap().clone();
        self.parse_files_rec(root, resolve_name);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::front::file_system::fs::MockFileSystem;
    use crate::front::syntax::ast_types::Type::Void;
    use crate::front::syntax::ast_types::{Block, Definition, FnDef, Reference};
    use std::rc::Rc;

    #[test]
    fn test_read_rec() {
        let mut mock_file_system = MockFileSystem::new("/".to_string());
        mock_file_system.insert_file("/main.ing", "fn main() {}");
        mock_file_system.insert_file("/test.ing", "pub mod example;");
        mock_file_system.insert_dir("/test/");
        mock_file_system.insert_file("/test/example.ing", "pub fn a() {};");

        let mut program = Program::new(mock_file_system);
        program.read_nodes();

        assert_eq!(program.modules.len(), 3);
        assert_eq!(
            program.modules.get("main"),
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

        let mut program = Program::new(mock_file_system);
        program.read_nodes();
        program.parse_files(false);

        assert_eq!(program.modules.len(), 3);
        assert_eq!(
            program.modules.get("main").unwrap().module,
            Some(Module {
                block: Block {
                    definitions: vec![Definition::FnDef(FnDef {
                        name: Reference::new("main".to_string()),
                        args: vec![],
                        return_type: Void,
                        body: Some(Block {
                            definitions: vec![],
                            statements: vec![],
                        }),
                        mods: Rc::new(vec![]),
                    })],
                    statements: vec![],
                },
                public_definitions: vec![],
            })
        );
    }
}
