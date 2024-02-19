pub mod ast_retriever;
mod ast_types;
pub mod file_system;
pub mod mergers;

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;
    use crate::front::file_system::fs::FileSystem;
    use crate::front::file_system::mock_fs::MockFileSystem;
    use crate::front::mergers::program::ProgramMerger;

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

        let program = program_merger.export_program();

        assert_eq!(program.public_functions.len(), 1);
    }
}
