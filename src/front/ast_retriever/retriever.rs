use crate::front::file_system::fs::FileSystem;
use crate::front::lexical::lexer::Lexer;
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
#[derive(Debug, PartialEq)]
pub struct FileRetriever<T> {
    file_system: T,
    root: Option<Path>,
    pub modules: HashMap<Path, ModuleNode>,
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

    fn parse_files(&mut self) {
        for module_node in self.modules.values_mut() {
            let mut source = module_node.source.clone();

            // TODO: add option to read from cached object file
            source.push_str(".ing");
            if let Ok(byte_stream) = self.file_system.read_file(&source) {
                let lexer = Lexer::new(byte_stream);
                let mut parser = Parser::new(lexer);
                let mut module = parser.parse_module().unwrap();

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
}
