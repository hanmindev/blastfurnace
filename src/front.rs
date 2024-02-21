pub mod ast_retriever;
mod ast_types;
mod exporter;
pub mod file_system;
pub mod mergers;
mod passes;

#[cfg(test)]
mod tests {
    use crate::front::file_system::fs::FileSystem;
    use crate::front::file_system::mock_fs::MockFileSystem;
    use crate::front::mergers::program::ProgramMerger;
    use camino::Utf8PathBuf;

    #[test]
    fn test_simple_program() {
        let mut mock_fs_0 = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_fs_0.insert_file(Utf8PathBuf::from("main.ing"), "mod test; pub fn main() {}");
        mock_fs_0.insert_file(Utf8PathBuf::from("test.ing"), "pub mod example;");
        mock_fs_0.insert_dir(Utf8PathBuf::from("test"));
        mock_fs_0.insert_file(Utf8PathBuf::from("test/example.ing"), "pub fn a() {};");

        let mut mock_fs_1 = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_fs_1.insert_file(Utf8PathBuf::from("main.ing"), "pub fn libfunc() {}");

        let mut program_merger = ProgramMerger::new("test");
        program_merger.read_package("test", mock_fs_0);
        program_merger.read_package("library", mock_fs_1);

        let front_program = program_merger.return_merged();
        let program = front_program.export_program();

        assert_eq!(program.public_functions.len(), 1);
    }
}
