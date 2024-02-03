pub struct ByteStream {
    stream: Box<dyn ByteStreamable>,
}

impl ByteStream {
    pub fn new(stream: Box<dyn ByteStreamable>) -> ByteStream {
        ByteStream { stream }
    }
    pub fn next(&mut self) -> char {
        self.stream.next()
    }
}

pub trait ByteStreamable {
    fn next(&mut self) -> char;
}

pub struct StringReader {
    string: String,
    index: usize,
}

impl StringReader {
    pub fn new(string: String) -> StringReader {
        StringReader { string, index: 0 }
    }
}

impl ByteStreamable for StringReader {
    fn next(&mut self) -> char {
        if self.index >= self.string.len() {
            return '\0';
        }

        let c = self.string.as_bytes()[self.index] as char;
        self.index += 1;
        c
    }
}
