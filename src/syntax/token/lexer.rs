use crate::syntax::parse::parser::TokenStream;
use crate::syntax::token::token_types::Token;

pub trait ByteStream {
    fn next(&mut self) -> char;
}

pub struct Lexer<T: ByteStream> {
    reader: T,
    curr: char,
}

#[derive(Debug, PartialEq)]
pub enum TokenError {
    InvalidToken(String),
    MultipleDecimals,
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

    fn get_token(&mut self) -> Result<Token, TokenError> {
        // check for EOF
        if self.curr == '\0' {
            return Ok(Token::EOF);
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
                            return Ok(Token::SlashAssign);
                        }
                        return Ok(Token::Slash);
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

            return Ok(match ident.as_str() {
                "fn" => Token::Fn,
                "rec" => Token::Rec,

                "if" => Token::If,
                "else" => Token::Else,
                "while" => Token::While,
                "for" => Token::For,
                "return" => Token::Return,
                "break" => Token::Break,
                "continue" => Token::Continue,

                "true" => Token::Bool(true),
                "false" => Token::Bool(false),

                "void" => Token::VoidType,
                "int" => Token::IntType,
                "float" => Token::FloatType,
                "double" => Token::DoubleType,
                "bool" => Token::BoolType,
                "string" => Token::StringType,
                "struct" => Token::StructType,

                "impl" => Token::Impl,
                "const" => Token::Const,

                _ => Token::Ident(ident),
            });
        }

        // numbers
        if self.curr.is_ascii_digit() || self.curr == '.' {
            let mut number = String::new();
            let mut dec = false;

            while self.curr.is_ascii_digit() || (self.curr == '.') {
                if self.curr == '.' {
                    if dec {
                        return Err(TokenError::MultipleDecimals);
                    } else {
                        dec = true;
                    }
                }

                number.push(self.curr);
                self.eat();
            }

            return Ok(if dec {
                match number.parse() {
                    Ok(n) => Token::Decimal(n),
                    Err(_) => return Err(TokenError::InvalidToken(number)),
                }
            } else {
                match number.parse() {
                    Ok(n) => Token::Int(n),
                    Err(_) => return Err(TokenError::InvalidToken(number)),
                }
            });
        }

        let prev = self.eat();
        if self.curr == '=' {
            return Ok(match prev {
                '<' => Token::Leq,
                '>' => Token::Geq,

                '=' => Token::Equal,
                '!' => Token::NotEqual,
                '+' => Token::PlusAssign,
                '-' => Token::MinusAssign,
                '*' => Token::StarAssign,
                '/' => Token::SlashAssign,
                '%' => Token::PercentAssign,
                _ => return Err(TokenError::InvalidToken(format!("{}", prev))),
            });
        }

        // match singletons
        Ok(match prev {
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
            _ => return Err(TokenError::InvalidToken(format!("{}", prev))),
        })
    }
}

impl<T: ByteStream> TokenStream for Lexer<T> {
    fn next(&mut self) -> Result<Token, TokenError> {
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

        assert_eq!(lexer.next().unwrap(), Token::Int(643214));
        assert_eq!(lexer.next().unwrap(), Token::Decimal(3243.24321));
        assert_eq!(lexer.next().unwrap(), Token::Decimal(0.432432));
        assert_eq!(lexer.next().err().unwrap(), TokenError::MultipleDecimals);
    }
}
