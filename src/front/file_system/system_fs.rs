use crate::front::file_system::byte_stream::{ByteStream, ByteStreamable};
use crate::front::file_system::fs::{FileSystem, FileSystemError, FileSystemResult, AbsUtf8PathBuf, RelUtf8PathBuf};
use std::fs;
use std::fs::File;
use std::io::Read;

pub struct SystemFs {
    root_dir: AbsUtf8PathBuf,
    current_dir: RelUtf8PathBuf,
}

pub struct FileReader {
    // TODO: make this more efficient instead of reading the whole file into memory
    file: File,
    str: String,
    index: usize,
}

impl FileReader {
    pub fn new(file: File) -> FileReader {
        let mut file_reader = FileReader {
            file,
            str: String::new(),
            index: 0,
        };
        file_reader
            .file
            .read_to_string(&mut file_reader.str)
            .expect("Unable to read file");
        file_reader
    }
}

impl ByteStreamable for FileReader {
    fn next(&mut self) -> char {
        if self.index >= self.str.len() {
            return '\0';
        }

        let c = self.str.as_bytes()[self.index] as char;
        self.index += 1;
        c
    }
}

impl SystemFs {
    fn get_abs_path(&self) -> AbsUtf8PathBuf {
        self.root_dir.join(&self.current_dir)
    }
}
impl FileSystem for SystemFs {
    fn new(root: AbsUtf8PathBuf) -> FileSystemResult<SystemFs> {
        if !root.is_absolute() { return Err(FileSystemError::NotAbsolute); }

        Ok(SystemFs { root_dir: root, current_dir: RelUtf8PathBuf::new() })
    }

    fn ls_files_with_extension(&self, extension: &str) -> Vec<RelUtf8PathBuf> {
        let mut files = Vec::new();
        if let Ok(paths) = fs::read_dir(self.get_abs_path()) {
            for dir_entry_res in paths {
                if let Ok(dir_entry) = dir_entry_res {
                    if let Ok(path) = RelUtf8PathBuf::try_from(dir_entry.path()) {
                        if path.extension() == Some(extension) {
                            files.push(path.strip_prefix(&self.root_dir).unwrap().to_path_buf());
                        }
                    }
                }
            }
        }
        files
    }

    fn read_file(&self, file_path: RelUtf8PathBuf) -> FileSystemResult<ByteStream> {
        match File::open(self.root_dir.join(file_path)) {
            Ok(file) => Ok(ByteStream::new(Box::new(FileReader::new(file)))),
            Err(_) => Err(FileSystemError::FileNotFound),
        }
    }

    fn check_dir(&self, path: RelUtf8PathBuf) -> FileSystemResult<bool> {
        if !path.is_relative() { return Err(FileSystemError::NotRelative); }

        Ok(self.root_dir.join(&path).is_dir())
    }

    fn enter_dir(&mut self, path: RelUtf8PathBuf) -> FileSystemResult<bool> {
        if !path.is_relative() { return Err(FileSystemError::NotRelative); }

        Ok(if self.check_dir(path.clone())? {
            self.current_dir = path;
            true
        } else {
            false
        })
    }

    fn exit_dir(&mut self) {
        if let Some(parent) = self.current_dir.parent() {
            self.current_dir = parent.to_path_buf();
        }
    }

    fn return_current_dir(&self) -> RelUtf8PathBuf {
        self.current_dir.clone()
    }
}
