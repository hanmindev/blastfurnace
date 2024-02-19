use crate::front::file_system::byte_stream::{ByteStream, StringReader};
use crate::front::file_system::fs::{AbsUtf8PathBuf, FileSystem, FileSystemError, FileSystemResult, RelUtf8PathBuf};
use std::collections::{HashMap, HashSet};
use camino::Utf8PathBuf;

pub struct MockFileSystem {
    rel_dir: RelUtf8PathBuf,
    files: HashMap<RelUtf8PathBuf, String>,
    dirs: HashSet<RelUtf8PathBuf>,
}

impl FileSystem for MockFileSystem {
    fn new(_root: AbsUtf8PathBuf) -> FileSystemResult<MockFileSystem> {
        Ok(MockFileSystem {
            rel_dir: Utf8PathBuf::new(),
            files: Default::default(),
            dirs: Default::default(),
        })
    }

    fn ls_files_with_extension(&self, extension: &str) -> Vec<RelUtf8PathBuf> {
        let mut files = Vec::new();
        for (path, _) in self.files.iter() {
            match path.strip_prefix(&self.rel_dir) {
                Ok(stripped_path) => {
                    if stripped_path.extension() == Some(extension) && stripped_path.parent() == Some(&Utf8PathBuf::new())
                    {
                        files.push(path.clone());
                    }
                }
                Err(_) => {
                    continue;
                }
            }
        }
        files
    }

    fn read_file(&self, file_path: RelUtf8PathBuf) -> FileSystemResult<ByteStream> {
        if !file_path.is_relative() { return Err(FileSystemError::NotRelative); }

        match self.files.get(&file_path) {
            Some(content) => Ok(ByteStream::new(Box::new(StringReader::new(
                content.to_string(),
            )))),
            None => Err(FileSystemError::FileNotFound),
        }
    }

    fn check_dir(&self, path: RelUtf8PathBuf) -> FileSystemResult<bool> {
        if !path.is_relative() { return Err(FileSystemError::NotRelative); }

        Ok(self.dirs.contains(&path))
    }

    fn enter_dir(&mut self, path: RelUtf8PathBuf) -> FileSystemResult<bool> {
        if !path.is_relative() { return Err(FileSystemError::NotRelative); }

        Ok(if self.dirs.contains(&path) {
            self.rel_dir = path;
            true
        } else {
            false
        })
    }

    fn exit_dir(&mut self) {
        self.rel_dir.pop();
    }

    fn return_current_dir(&self) -> RelUtf8PathBuf {
        self.rel_dir.clone()
    }
}

impl MockFileSystem {
    pub fn insert_file(&mut self, path: Utf8PathBuf, content: &str) {
        self.files.insert(path, content.to_string());
    }

    pub fn insert_dir(&mut self, path: Utf8PathBuf) {
        self.dirs.insert(path);
    }
}
