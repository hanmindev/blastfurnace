use crate::syntax::token::lexer::ByteStream;

pub struct StringReader {
    string: String,
    index: usize,
}

impl StringReader {
    pub fn new(string: String) -> StringReader {
        StringReader { string, index: 0 }
    }
}

impl ByteStream for StringReader {
    fn next(&mut self) -> char {
        if (self.index >= self.string.len()) {
            return '\0';
        }

        let c = self.string.as_bytes()[self.index] as char;
        self.index += 1;
        c
    }
}
