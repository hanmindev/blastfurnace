mod first_assignment_graph;
mod type_expression;
mod var_def_table;

use crate::front::exporter::export::FrontProgram;
use crate::front::passes::{Pass, PassResult};

use crate::front::ast_types::visitor::{Visitable, Visitor};
use crate::front::passes::types::first_assignment_graph::create_first_assignment_graph;

#[derive(Debug)]
pub struct AnnotateTypes;

impl Pass for AnnotateTypes {
    fn pass(&mut self, program: &mut FrontProgram) -> PassResult {
        let mut table = create_first_assignment_graph(program);



        // topologically sort first assignment graph

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::front::file_system::fs::FileSystem;
    use crate::front::file_system::mock_fs::MockFileSystem;
    use crate::front::mergers::program::ProgramMerger;
    use crate::front::passes::pass;
    use crate::front::passes::types::AnnotateTypes;
    use camino::Utf8PathBuf;

    #[test]
    fn test_type_annotation_simple() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(
            Utf8PathBuf::from("main.ing"),
            "pub fn main() { let a = 5; }",
        );

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut front_program = program_merger.return_merged();

        pass(&mut front_program, &mut vec![Box::new(AnnotateTypes)]);

        println!("{:?}", front_program);
    }
}
