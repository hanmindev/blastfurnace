pub mod front;
pub mod middle;
pub mod back;

#[cfg(test)]
mod tests {
    use crate::front::file_system::fs::FileSystem;
    use crate::front::file_system::mock_fs::MockFileSystem;
    use crate::front::mergers::program::ProgramMerger;
    use crate::middle::passes::delete_unused::DeleteUnused;
    use crate::middle::passes::optimize;

    #[test]
    fn test_compile_simple() {
        let mut mock_file_system = MockFileSystem::new("/".to_string());
        mock_file_system.insert_file("/main.ing", "mod test; fn main() {}");
        mock_file_system.insert_file("/test.ing", "pub mod example;");
        mock_file_system.insert_dir("/test/");
        mock_file_system.insert_file("/test/example.ing", "pub fn a() {};");

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut program = program_merger.export_program();

        optimize(&mut program, &mut vec![Box::new(DeleteUnused {})]);
    }
}
