use crate::front::file_system::byte_stream::{ByteStream, StringReader};
use std::collections::{HashMap, HashSet};

pub enum FileSystemError {
    FileNotFound,
    DirectoryNotFound,
}

pub type FileSystemResult<T> = Result<T, FileSystemError>;

pub trait FileSystem {
    // paths must be relative to root
    // the absolute root should never be returned
    fn new(root: String) -> Self; // must be absolute path
    fn ls_files_with_extension(&self, extension: &str) -> Vec<String>; // must return only the file name
    fn read_file(&self, file_path: &str) -> FileSystemResult<ByteStream>;
    fn check_dir(&self, path: &str) -> bool;
    fn enter_dir(&mut self, path: &str) -> bool;
    fn exit_dir(&mut self);
    fn return_current_dir(&self) -> String;
}
