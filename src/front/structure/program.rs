use crate::front::file_system::fs::FileSystem;
use crate::front::lexical::lexer::Lexer;
use crate::front::name_resolution::resolver::Resolvable;
use crate::front::name_resolution::scope_table::ScopeTable;
use crate::front::syntax::ast_types::Module;
use crate::front::syntax::parser::Parser;
use std::collections::HashMap;

type Source = String;

#[derive(Debug, PartialEq)]
pub struct ModuleNode {
    pub source: Source,
    pub submodules: HashMap<Source, Option<bool>>,
    pub module: Option<Module>,
}

#[derive(Debug)]
pub struct Program<T> {
    file_system: T,
    pub root: Option<Source>,
    pub modules: HashMap<Source, ModuleNode>,
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

            let mut module = ModuleNode {
                source: file_name.clone(),
                submodules,
                module: None,
            };

            if self.file_system.enter_dir(file_name.as_str()) {
                self.read_nodes_rec(&mut module);
                self.file_system.exit_dir();
            }

            parent_module.submodules.insert(file_name.clone(), None);

            let mut path = self.file_system.return_current_dir();
            path.push_str(&file_name);

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
        self.modules
            .get_mut("main")
            .unwrap()
            .submodules
            .extend(root.submodules);
    }

    fn parse_files_rec(&mut self, module_source: Source) {
        if let Some(module_node) = self.modules.get_mut(&module_source) {
            let mut scope_table = ScopeTable::new();
            if let Ok(byte_stream) = self.file_system.read_file(&module_source) {
                let lexer = Lexer::new(byte_stream);
                let mut parser = Parser::new(lexer);
                let mut module = parser.parse_module().unwrap();
                module.resolve(&mut scope_table).unwrap();
                module_node.module = Some(module);
            } else {
                panic!("File not found");
            }
        }
    }

    pub fn parse_files(&mut self) {
        let root = self.root.as_ref().unwrap().clone();
        self.parse_files_rec(root);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::front::file_system::fs::MockFileSystem;

    #[test]
    fn test_read_rec() {
        let mut mock_file_system = MockFileSystem::new("/".to_string());
        mock_file_system.insert_file("/main.ing", "fn main() {}");
        mock_file_system.insert_file("/test.ing", "pub mod example;");
        mock_file_system.insert_dir("/test");
        mock_file_system.insert_file("/test/example.ing", "pub fn a() {};");

        let mut program = Program::new(mock_file_system);
        program.read_nodes();

        assert_eq!(program.modules.len(), 2);
        assert_eq!(
            program.modules.get("main"),
            Some(&ModuleNode {
                source: "main".to_string(),
                submodules: HashMap::new(),
                module: None,
            })
        );
        assert_eq!(
            program.modules.get("test"),
            Some(&ModuleNode {
                source: "test".to_string(),
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
        mock_file_system.insert_dir("/test");
        mock_file_system.insert_file("/test/example.ing", "pub fn a() {};");

        let mut program = Program::new(mock_file_system);
        program.read_nodes();
    }
}
