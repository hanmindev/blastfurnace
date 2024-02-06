pub mod front;
pub mod middle;

#[cfg(test)]
mod tests {
    use crate::front::file_system::mock_fs::MockFileSystem;
    use crate::front::file_system::fs::FileSystem;
    use crate::front::mergers::program::ProgramMerger;

    #[test]
    fn test_compile_simple() {
        let mut mock_file_system = MockFileSystem::new("/".to_string());
        mock_file_system.insert_file("/main.ing", "mod test; fn main() {}");
        mock_file_system.insert_file("/test.ing", "pub mod example;");
        mock_file_system.insert_dir("/test/");
        mock_file_system.insert_file("/test/example.ing", "pub fn a() {};");

        let mut program_merger = ProgramMerger::new();

        program_merger.read_package("pkg", mock_file_system);

        let program = program_merger.export_program();


    }
}