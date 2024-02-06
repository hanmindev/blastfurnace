use std::collections::{HashMap, HashSet};
use crate::front::ast_retriever::retriever::{FilePath};
use crate::front::file_system::byte_stream::{ByteStream, StringReader};
use crate::front::file_system::fs::{FileSystem, FileSystemError, FileSystemResult};

pub struct MockFileSystem {
    current_dir: FilePath,
    files: HashMap<FilePath, String>,
    dirs: HashSet<FilePath>,
}

impl FileSystem for MockFileSystem {
    fn new(root: FilePath) -> MockFileSystem {
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
                    if !stripped_path.contains('/')
                        && stripped_path.ends_with(extension)
                        && stripped_path.len() != extension.len() + 1
                    {
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
        assert!(path.starts_with('/'));
        let path = if path.ends_with('/') {
            path.to_string()
        } else {
            format!("{}/", path)
        };

        if self.dirs.contains(&path) {
            self.current_dir = path;
            true
        } else {
            false
        }
    }

    fn exit_dir(&mut self) {
        if self.current_dir != "/" {
            self.current_dir = self.current_dir.split('/').collect::<Vec<&str>>()
                [..self.current_dir.split('/').count() - 2]
                .join("/");
            self.current_dir.push('/');
        }
    }

    fn return_current_dir(&self) -> FilePath {
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