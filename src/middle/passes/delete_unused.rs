use crate::middle::format::ir_types::{IrBlock, IrFnDef, IrIf, IrStatement};
use crate::middle::format::types::{GlobalName, Program};
use crate::middle::passes::Pass;
use std::collections::HashSet;

trait CheckUsed {
    fn add_used(&self, used: &mut Used, program: &mut Program);
}

impl CheckUsed for IrIf {
    fn add_used(&self, used: &mut Used, program: &mut Program) {
        self.body.add_used(used, program);
    }
}

impl CheckUsed for IrStatement {
    fn add_used(&self, used: &mut Used, program: &mut Program) {
        match self {
            IrStatement::If(x) => x.add_used(used, program),
            IrStatement::FnCall(x) => {
                used.functions.insert(x.fn_name.clone());
            }
            IrStatement::Block(x) => x.add_used(used, program),
            _ => {}
        }
    }
}

impl CheckUsed for IrBlock {
    fn add_used(&self, used: &mut Used, program: &mut Program) {
        for statement in &self.statements {
            statement.add_used(used, program);
        }
    }
}

impl CheckUsed for IrFnDef {
    fn add_used(&self, used: &mut Used, program: &mut Program) {
        used.functions.insert(self.fn_name.clone());
        for statement in &self.statements {
            statement.add_used(used, program);
        }
    }
}

struct Used {
    functions: HashSet<GlobalName>,
    structs: HashSet<GlobalName>,
    variables: HashSet<GlobalName>,
}

#[derive(Debug)]
pub struct DeleteUnused;

impl Pass for DeleteUnused {
    fn optimize(&mut self, program: &mut Program) {
        let mut used = Used {
            functions: program.public_functions.clone(),
            structs: HashSet::new(),
            variables: HashSet::new(),
        };

        for global_name in program.public_functions.clone() {
            if let Some(fn_def) = program.function_definitions.remove(&global_name) {
                fn_def.add_used(&mut used, program);
                program.function_definitions.insert(global_name, fn_def);
            }
        }

        program
            .function_definitions
            .retain(|x, _| used.functions.contains(x));
        // program.global_var_definitions.retain(|x, _| used.variables.contains(x));
    }
}

#[cfg(test)]
mod tests {
    use crate::front::file_system::fs::FileSystem;
    use crate::front::file_system::mock_fs::MockFileSystem;
    use crate::front::mergers::program::ProgramMerger;
    use crate::middle::passes::delete_unused::DeleteUnused;
    use crate::middle::passes::optimize;
    use camino::Utf8PathBuf;

    #[test]
    fn test_simple_deletion() {
        let mut mock_fs_0 = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_fs_0.insert_file(Utf8PathBuf::from("main.ing"), "mod test; pub fn main() {}");
        mock_fs_0.insert_file(
            Utf8PathBuf::from("test.ing"),
            "pub mod example; pub fn hello() {}",
        );
        mock_fs_0.insert_dir(Utf8PathBuf::from("test"));
        mock_fs_0.insert_file(Utf8PathBuf::from("test/example.ing"), "pub fn a() {};");

        let mut mock_fs_1 = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_fs_1.insert_file(Utf8PathBuf::from("main.ing"), "pub fn libfunc() {}");

        let mut program_merger = ProgramMerger::new("test");
        program_merger.read_package("test", mock_fs_0);
        program_merger.read_package("library", mock_fs_1);

        let mut program = program_merger.export_program();

        optimize(&mut program, &mut vec![Box::new(DeleteUnused)]);

        assert_eq!(program.function_definitions.len(), 1);
        assert_eq!(
            program
                .function_definitions
                .contains_key("test/root/0_main"),
            true
        );
    }
}
