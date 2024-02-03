use crate::front::file_system::byte_stream::{ByteStream, ByteStreamable};
use crate::front::file_system::fs::{FileSystem, FileSystemError, FileSystemResult};
use std::fs;
use std::fs::File;
use std::io::Read;

pub struct SystemFs {
    current_dir: String,
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

impl FileSystem for SystemFs {
    fn new(root: String) -> SystemFs {
        SystemFs { current_dir: root }
    }

    fn ls_files_with_extension(&self, extension: &str) -> Vec<String> {
        let mut files = Vec::new();
        let paths = fs::read_dir(&self.current_dir).unwrap();
        for path in paths {
            let path = path.unwrap().path();
            let path = path.to_str().unwrap();
            if path.ends_with(extension) {
                files.push(path.to_string());
            }
        }
        files
    }

    fn read_file(&self, path: &str) -> FileSystemResult<ByteStream> {
        match File::open(path) {
            Ok(file) => Ok(ByteStream::new(Box::new(FileReader::new(file)))),
            Err(_) => Err(FileSystemError::FileNotFound),
        }
    }

    fn check_dir(&self, path: &str) -> bool {
        if let Ok(metadata) = fs::metadata(path) {
            metadata.is_dir()
        } else {
            false
        }
    }

    fn enter_dir(&mut self, path: &str) -> bool {
        if self.check_dir(path) {
            self.current_dir = path.to_string();
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

    fn return_current_dir(&self) -> String {
        self.current_dir.clone()
    }
}
