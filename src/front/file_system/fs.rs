use crate::front::file_system::byte_stream::ByteStream;
use camino::Utf8PathBuf;

pub type AbsUtf8PathBuf = Utf8PathBuf;
pub type RelUtf8PathBuf = Utf8PathBuf;

#[derive(Debug)]
pub enum FileSystemError {
    FileNotFound,
    DirectoryNotFound,
    NotRelative,
    NotAbsolute,
}

pub type FileSystemResult<T> = Result<T, FileSystemError>;

pub trait FileSystem {
    // paths must be relative to root
    // the absolute root should never be returned
    fn new(root: AbsUtf8PathBuf) -> FileSystemResult<Self>
    where
        Self: Sized;
    fn ls_files_with_extension(&self, extension: &str) -> Vec<RelUtf8PathBuf>;
    fn read_file(&self, file_path: RelUtf8PathBuf) -> FileSystemResult<ByteStream>;
    fn check_dir(&self, path: RelUtf8PathBuf) -> FileSystemResult<bool>;
    fn enter_dir(&mut self, path: RelUtf8PathBuf) -> FileSystemResult<bool>;
    fn exit_dir(&mut self);
    fn return_current_dir(&self) -> RelUtf8PathBuf;
}
