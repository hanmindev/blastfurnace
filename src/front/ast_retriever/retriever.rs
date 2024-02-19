use crate::front::ast_retriever::name_resolution::resolve_module;
use crate::front::ast_retriever::reader::lexical::lexer::Lexer;
use crate::front::ast_retriever::reader::syntax::parser::Parser;
use crate::front::ast_types::Module;
use crate::front::file_system::fs::{FileSystem, RelUtf8PathBuf};
use std::collections::HashMap;

pub type ModuleSource = String;
#[derive(Debug, PartialEq)]
pub struct ModuleNode {
    pub file_path: RelUtf8PathBuf, // unrelated to module hierarchy
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
        let module_file_paths = self.file_system.ls_files_with_extension("ing");

        for file_path_ext in module_file_paths {
            let submodules = HashMap::new();

            let mut module = ModuleNode {
                file_path: file_path_ext.clone(),
                submodules,
                module: None,
            };

            let module_path = file_path_ext.with_extension("");

            if self
                .file_system
                .enter_dir(module_path.clone())
                .unwrap_or(false)
            {
                self.read_nodes_rec(&mut module);
                self.file_system.exit_dir();
            }

            let module_source: ModuleSource = if module_path == RelUtf8PathBuf::from("main") {
                String::from("/root")
            } else {
                let mut new_path = String::from("/root/");
                let it = module_path.iter();
                let module_path = it.collect::<Vec<&str>>().join("/");

                new_path.push_str(&module_path);
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

        if let Some(value) = self.modules.get_mut("/root") {
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
            let file_source = module_node.file_path.clone();

            // TODO: add option to read from cached object file
            if let Ok(byte_stream) = self.file_system.read_file(file_source) {
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
    use camino::Utf8PathBuf;

    #[test]
    fn test_read_rec() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(Utf8PathBuf::from("main.ing"), "fn main() {}");
        mock_file_system.insert_file(Utf8PathBuf::from("test.ing"), "pub mod example;");
        mock_file_system.insert_dir(Utf8PathBuf::from("test"));
        mock_file_system.insert_file(Utf8PathBuf::from("test/example.ing"), "pub fn a() {};");

        let mut file_retriever = FileRetriever::new(mock_file_system);
        file_retriever.read_nodes();

        assert_eq!(file_retriever.modules.len(), 3);
        assert_eq!(
            file_retriever.modules.get("/root"),
            Some(&ModuleNode {
                file_path: Utf8PathBuf::from("main.ing"),
                submodules: HashMap::from([("/root/test".to_string(), None)]),
                module: None,
            })
        );
        assert_eq!(
            file_retriever.modules.get("/root/test"),
            Some(&ModuleNode {
                file_path: Utf8PathBuf::from("test.ing"),
                submodules: HashMap::from([("/root/test/example".to_string(), None)]),
                module: None,
            })
        );
        assert_eq!(
            file_retriever.modules.get("/root/test/example"),
            Some(&ModuleNode {
                file_path: Utf8PathBuf::from("test/example.ing"),
                submodules: HashMap::new(),
                module: None,
            })
        );
    }
}
