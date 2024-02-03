use crate::front::structure::fs::FileSystem;
use crate::front::syntax::ast_types::Module;
use std::collections::HashMap;
use std::rc::Rc;

type Source = String;

#[derive(Debug, PartialEq)]
pub struct SubModule {
    pub public: Option<bool>,
    pub module_node: Rc<ModuleNode>,
}

#[derive(Debug, PartialEq)]
pub struct ModuleNode {
    pub source: Source,
    pub submodules: HashMap<Source, SubModule>,
    pub module: Option<Module>,
}

#[derive(Debug)]
pub struct Program<T> {
    file_system: T,

    pub root: Option<Rc<ModuleNode>>,

    pub modules: HashMap<Source, Rc<ModuleNode>>,
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

            let rc_module = Rc::new(module);

            parent_module.submodules.insert(
                file_name.clone(),
                SubModule {
                    public: None,
                    module_node: Rc::clone(&rc_module),
                },
            );

            self.modules.insert(file_name, rc_module);
        }
    }

    pub fn read_nodes(&mut self) {
        let mut root = ModuleNode {
            source: self.file_system.return_current_dir(),
            submodules: HashMap::new(),
            module: None,
        };

        self.read_nodes_rec(&mut root);
        if let Some(sub_module) = root.submodules.get_mut("main") {
            sub_module.public = Some(true);
        }
        self.root = Some(Rc::new(root));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::front::structure::fs::MockFileSystem;

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

        let root = program.root.unwrap();
        assert_eq!(root.submodules.len(), 2);
        assert_eq!(
            root.submodules.get("main"),
            Some(&SubModule {
                public: Some(true),
                module_node: program.modules.get("main").unwrap().clone(),
            })
        );
        assert_eq!(
            root.submodules.get("test"),
            Some(&SubModule {
                public: None,
                module_node: program.modules.get("test").unwrap().clone(),
            })
        );
    }
}
