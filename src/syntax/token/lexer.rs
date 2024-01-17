use crate::syntax::parse::parser::TokenStream;
use crate::syntax::token::token_types::Token;
use crate::syntax::token::token_types::Token::Invalid;

pub trait ByteStream {
    fn next(&mut self) -> char;
}

pub struct Lexer<T: ByteStream> {
    reader: T,
    curr: char,
}

impl<T: ByteStream> Lexer<T> {
    pub fn new(reader: T) -> Lexer<T> {
        let mut lexer = Lexer { reader, curr: '\0' };
        lexer.eat();
        lexer
    }
    fn read_char(&mut self) -> char {
        self.reader.next()
    }

    fn eat(&mut self) -> char {
        let prev = self.curr;
        self.curr = self.read_char();
        prev
    }

    fn get_token(&mut self) -> Token {
        // check for EOF
        if self.curr == '\0' {
            return Token::EOF;
        }

        while self.curr.is_whitespace() || self.curr == '/' {
            // skip whitespace
            while self.curr.is_whitespace() {
                self.eat();
            }

            // skip comments
            if self.curr == '/' {
                match self.read_char() {
                    '/' => {
                        // comment until end of line
                        loop {
                            self.eat();
                            if self.curr == '\n' || self.curr == '\r' {
                                self.eat();
                                break;
                            }
                        }
                    }

                    _ => {
                        if self.curr == '=' {
                            self.eat();
                            return Token::SlashAssign;
                        }
                        return Token::Slash;
                    }
                }
            }
        }

        // identifiers
        if self.curr.is_alphabetic() {
            let mut ident = String::new();

            // read word and set to ident
            while self.curr.is_alphanumeric() {
                ident.push(self.eat());
            }

            match ident.as_str() {
                "fn" => return Token::Fn,
                "rec" => return Token::Rec,

                "if" => return Token::If,
                "else" => return Token::Else,
                "while" => return Token::While,
                "for" => return Token::For,
                "return" => return Token::Return,
                "break" => return Token::Break,
                "continue" => return Token::Continue,

                "true" => return Token::Bool(true),
                "false" => return Token::Bool(false),

                "void" => return Token::VoidType,
                "int" => return Token::IntType,
                "float" => return Token::FloatType,
                "double" => return Token::DoubleType,
                "bool" => return Token::BoolType,
                "string" => return Token::StringType,
                "struct" => return Token::StructType,

                "impl" => return Token::Impl,
                "const" => return Token::Const,

                _ => {}
            }

            return Token::Ident(ident);
        }

        // numbers
        if self.curr.is_ascii_digit() || self.curr == '.' {
            let mut number = String::new();
            let mut dec = self.curr == '.';

            while self.curr.is_ascii_digit() || (self.curr == '.') {
                if dec {
                    if self.curr == '.' {
                        panic!("Error reading number: multiple decimal points")
                    } else {
                        dec = true;
                    }
                }

                number.push(self.curr);
                self.eat();
            }

            return if dec {
                Token::Decimal(number.parse().unwrap())
            } else {
                Token::Int(number.parse().unwrap())
            };
        }

        let prev = self.eat();
        if self.curr == '=' {
            return match prev {
                '<' => Token::Leq,
                '>' => Token::Geq,

                '=' => Token::Equal,
                '!' => Token::NotEqual,
                '+' => Token::PlusAssign,
                '-' => Token::MinusAssign,
                '*' => Token::StarAssign,
                '/' => Token::SlashAssign,
                '%' => Token::PercentAssign,
                _ => Invalid(format!("{prev}=")),
            };
        }

        // match singletons
        match prev {
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Star,
            '/' => Token::Slash,
            '%' => Token::Percent,
            '!' => Token::Exclamation,
            '&' => Token::Ampersand,
            '=' => Token::Assign,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            ':' => Token::Colon,
            '.' => Token::Dot,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            '<' => Token::LAngle,
            '>' => Token::RAngle,
            _ => Invalid(prev.to_string()),
        }
    }
}

impl<T: ByteStream> TokenStream for Lexer<T> {
    fn next(&mut self) -> Token {
        self.get_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct StringReader {
        string: String,
        index: usize,
    }

    impl StringReader {
        fn new(string: String) -> StringReader {
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

    #[test]
    fn simple_test() {
        let statement = "fn main() { return 0; }";
        let mut lexer = Lexer::new(StringReader::new(statement.to_string()));

        assert_eq!(lexer.next(), Token::Fn);
        assert_eq!(lexer.next(), Token::Ident("main".to_string()));
        assert_eq!(lexer.next(), Token::LParen);
        assert_eq!(lexer.next(), Token::RParen);
        assert_eq!(lexer.next(), Token::LBrace);
        assert_eq!(lexer.next(), Token::Return);
        assert_eq!(lexer.next(), Token::Int(0));
        assert_eq!(lexer.next(), Token::Semicolon);
        assert_eq!(lexer.next(), Token::RBrace);
        assert_eq!(lexer.next(), Token::EOF);
    }

    #[test]
    fn number_comprehension() {
        let statement = "643214 3243.24321 .432432 2342.342315.321534";
        let mut lexer = Lexer::new(StringReader::new(statement.to_string()));

        assert_eq!(lexer.next(), Token::Int(643214));
        assert_eq!(lexer.next(), Token::Decimal(3243.24321));
        assert_eq!(lexer.next(), Token::Decimal(0.432432));
        assert_eq!(lexer.next(), Token::Decimal(2342.342315));
    }
}
