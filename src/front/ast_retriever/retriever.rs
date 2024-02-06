use crate::front::file_system::fs::FileSystem;
use crate::front::ast_retriever::lexical::lexer::Lexer;
use crate::front::name_resolution::name_resolver::resolve_module;
use crate::front::ast_retriever::syntax::ast_types::Module;
use crate::front::ast_retriever::syntax::parser::Parser;
use std::collections::HashMap;
pub type FilePath = String;
pub type ModuleSource = String;
#[derive(Debug, PartialEq)]
pub struct ModuleNode {
    pub file_path: FilePath,
    pub submodules: HashMap<ModuleSource, Option<bool>>,
    pub module: Option<Module>,
}
#[derive(Debug, PartialEq)]
pub struct FileRetriever<T> {
    file_system: T,
    root: Option<ModuleSource>,
    pub modules: HashMap<ModuleSource, ModuleNode>,
}

impl<T: FileSystem> FileRetriever<T> {
    pub fn new(file_system: T) -> FileRetriever<T> {
        let mut f = FileRetriever {
            file_system,
            root: None,
            modules: Default::default(),
        };
        f.read_nodes();
        f.parse_files();
        f
    }
    fn read_nodes_rec(&mut self, parent_module: &mut ModuleNode) {
        let modules = self.file_system.ls_files_with_extension("ing");

        for file_name_ext in modules {
            let file_name = file_name_ext.strip_suffix(".ing").unwrap().to_string();
            let submodules = HashMap::new();

            let mut path = self.file_system.return_current_dir();
            path.push_str(&file_name);

            let mut module = ModuleNode {
                file_path: path.clone(),
                submodules,
                module: None,
            };

            if self.file_system.enter_dir(path.as_str()) {
                self.read_nodes_rec(&mut module);
                self.file_system.exit_dir();
            }

            let mut module_source: ModuleSource = if path == "/main" {
                "/root".to_string()
            } else {
                let mut new_path = "/root".to_string();
                new_path.push_str(&path);
                new_path
            };

            parent_module.submodules.insert(module_source.clone(), None);
            self.modules.insert(module_source, module);
        }
    }

    fn read_nodes(&mut self) {
        let mut root = ModuleNode {
            file_path: self.file_system.return_current_dir(),
            submodules: HashMap::new(),
            module: None,
        };

        self.read_nodes_rec(&mut root);
        self.root = Some("/root".to_string());

        if let Some(mut value) = self.modules.get_mut("/root") {
            assert_eq!(value.submodules.len(), 0);
            root.submodules
                .remove("/root")
                .expect("main module not found in submodules");
            value.submodules = root.submodules;
        } else {
            panic!("main module not found");
        }
    }

    fn parse_files(&mut self) {
        for (mod_path, module_node) in self.modules.iter_mut() {
            let mut file_source = module_node.file_path.clone();

            // TODO: add option to read from cached object file
            file_source.push_str(".ing");
            if let Ok(byte_stream) = self.file_system.read_file(&file_source) {
                let lexer = Lexer::new(byte_stream);
                let mut parser = Parser::new(lexer);
                let mut module = parser.parse_module().unwrap();

                for import in &module.mods {
                    let mut path = mod_path.clone();
                    path.push_str("/");
                    path.push_str(&import.name);

                    if let Some(None) = module_node.submodules.remove(&path) {
                        module_node.submodules.insert(path, Some(import.public));
                    } else {
                        panic!("Submodule not found, or already resolved"); // TODO: error instead of panic
                    }
                }

                resolve_module(&mut module);
                module_node.module = Some(module);
            } else {
                panic!("File not found");
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::front::ast_retriever::retriever::ModuleNode;
    use crate::front::file_system::mock_fs::MockFileSystem;

    #[test]
    fn test_read_rec() {
        let mut mock_file_system = MockFileSystem::new("/".to_string());
        mock_file_system.insert_file("/main.ing", "fn main() {}");
        mock_file_system.insert_file("/test.ing", "pub mod example;");
        mock_file_system.insert_dir("/test/");
        mock_file_system.insert_file("/test/example.ing", "pub fn a() {};");

        let mut program = FileRetriever::new(mock_file_system);
        program.read_nodes();

        assert_eq!(program.modules.len(), 3);
        assert_eq!(
            program.modules.get("/root"),
            Some(&ModuleNode {
                file_path: "/main".to_string(),
                submodules: HashMap::from([("/root/test".to_string(), None)]),
                module: None,
            })
        );
        assert_eq!(
            program.modules.get("/root/test"),
            Some(&ModuleNode {
                file_path: "/test".to_string(),
                submodules: HashMap::from([("/root/test/example".to_string(), None)]),
                module: None,
            })
        );
        assert_eq!(
            program.modules.get("/root/test/example"),
            Some(&ModuleNode {
                file_path: "/test/example".to_string(),
                submodules: HashMap::new(),
                module: None,
            })
        );
    }
}
