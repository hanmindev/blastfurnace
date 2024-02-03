use crate::front::file_system::byte_stream::{ByteStream, ByteStreamable, StringReader};
use std::collections::{HashMap, HashSet};

pub enum FileSystemError {
    FileNotFound,
    DirectoryNotFound,
}

pub type FileSystemResult<T> = Result<T, FileSystemError>;

pub trait FileSystem {
    fn new(root: String) -> Self;
    fn ls_files_with_extension(&self, extension: &str) -> Vec<String>;
    fn read_file(&self, path: &str) -> FileSystemResult<ByteStream>;
    fn check_dir(&self, path: &str) -> bool;
    fn enter_dir(&mut self, path: &str) -> bool;
    fn exit_dir(&mut self);
    fn return_current_dir(&self) -> String;
}

pub struct MockFileSystem {
    current_dir: String,
    files: HashMap<String, String>,
    dirs: HashSet<String>,
}

impl FileSystem for MockFileSystem {
    fn new(root: String) -> MockFileSystem {
        MockFileSystem {
            current_dir: root,
            files: Default::default(),
            dirs: Default::default(),
        }
    }

    fn ls_files_with_extension(&self, extension: &str) -> Vec<String> {
        let mut files = Vec::new();
        for (path, _) in self.files.iter() {
            match path.strip_prefix(&self.current_dir) {
                Some(stripped_path) => {
                    if !stripped_path.contains('/') && stripped_path.ends_with(extension) {
                        files.push(stripped_path.to_string());
                    }
                }
                None => {
                    continue;
                }
            }
        }
        files
    }

    fn read_file(&self, path: &str) -> FileSystemResult<ByteStream> {
        match self.files.get(path) {
            Some(content) => Ok(ByteStream::new(Box::new(StringReader::new(
                content.to_string(),
            )))),
            None => Err(FileSystemError::FileNotFound),
        }
    }

    fn check_dir(&self, path: &str) -> bool {
        self.dirs.contains(path)
    }

    fn enter_dir(&mut self, path: &str) -> bool {
        if self.dirs.contains(path) {
            self.current_dir = path.to_string();
            true
        } else {
            false
        }
    }

    fn exit_dir(&mut self) {
        if self.current_dir != "/" {
            self.current_dir = self.current_dir.split('/').collect::<Vec<&str>>()
                [..self.current_dir.split('/').count() - 1]
                .join("/");
        }
    }

    fn return_current_dir(&self) -> String {
        self.current_dir.clone()
    }
}

impl MockFileSystem {
    pub fn insert_file(&mut self, path: &str, content: &str) {
        self.files.insert(path.to_string(), content.to_string());
    }

    pub fn insert_dir(&mut self, path: &str) {
        self.dirs.insert(path.to_string());
    }
}
