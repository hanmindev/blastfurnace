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
                self.eat();
                match self.curr {
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

                    '=' => {
                        self.eat();
                        return Ok(Token::SlashAssign);
                    }
                    _ => {
                        return Ok(Token::Slash);
                    }
                }
            }
        }

        // read string
        if self.curr == '"' {
            let mut string = String::new();
            self.eat();
            let mut escaped = false;

            while self.curr != '"' && !escaped {
                escaped = false;
                string.push(self.curr);
                if self.eat() == '\\' {
                    escaped = true;
                }
            }

            self.eat();
            return Ok(Token::String(string));
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
                "static" => Token::Static,
                "inline" => Token::Inline,

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
            let assign = match prev {
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
            };

            self.eat();
            return Ok(assign);
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
    use crate::syntax::token::lexer_string_reader::StringReader;

    #[test]
    fn simple_test() {
        let statement = "fn main() { return 0; }";
        let mut lexer = Lexer::new(StringReader::new(statement.to_string()));

        assert_eq!(lexer.next().unwrap(), Token::Fn);
        assert_eq!(lexer.next().unwrap(), Token::Ident("main".to_string()));
        assert_eq!(lexer.next().unwrap(), Token::LParen);
        assert_eq!(lexer.next().unwrap(), Token::RParen);
        assert_eq!(lexer.next().unwrap(), Token::LBrace);
        assert_eq!(lexer.next().unwrap(), Token::Return);
        assert_eq!(lexer.next().unwrap(), Token::Int(0));
        assert_eq!(lexer.next().unwrap(), Token::Semicolon);
        assert_eq!(lexer.next().unwrap(), Token::RBrace);
        assert_eq!(lexer.next().unwrap(), Token::EOF);
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

    #[test]
    fn comment_test() {
        let statement = "fn main() { // return 0; \n return 1; }";
        let mut lexer = Lexer::new(StringReader::new(statement.to_string()));

        assert_eq!(lexer.next().unwrap(), Token::Fn);
        assert_eq!(lexer.next().unwrap(), Token::Ident("main".to_string()));
        assert_eq!(lexer.next().unwrap(), Token::LParen);
        assert_eq!(lexer.next().unwrap(), Token::RParen);
        assert_eq!(lexer.next().unwrap(), Token::LBrace);
        assert_eq!(lexer.next().unwrap(), Token::Return);
        assert_eq!(lexer.next().unwrap(), Token::Int(1));
        assert_eq!(lexer.next().unwrap(), Token::Semicolon);
        assert_eq!(lexer.next().unwrap(), Token::RBrace);
        assert_eq!(lexer.next().unwrap(), Token::EOF);
    }

    #[test]
    fn whitespace_test() {
        let statement = "fn main()                   {       return 0; }";
        let mut lexer = Lexer::new(StringReader::new(statement.to_string()));

        assert_eq!(lexer.next().unwrap(), Token::Fn);
        assert_eq!(lexer.next().unwrap(), Token::Ident("main".to_string()));
        assert_eq!(lexer.next().unwrap(), Token::LParen);
        assert_eq!(lexer.next().unwrap(), Token::RParen);
        assert_eq!(lexer.next().unwrap(), Token::LBrace);
        assert_eq!(lexer.next().unwrap(), Token::Return);
        assert_eq!(lexer.next().unwrap(), Token::Int(0));
        assert_eq!(lexer.next().unwrap(), Token::Semicolon);
        assert_eq!(lexer.next().unwrap(), Token::RBrace);
        assert_eq!(lexer.next().unwrap(), Token::EOF);
    }

    #[test]
    fn operator_test() {
        let statement = "fn main() { return 0 + 1 - 2 * 3 / 4 % 5; }";
        let mut lexer = Lexer::new(StringReader::new(statement.to_string()));

        assert_eq!(lexer.next().unwrap(), Token::Fn);
        assert_eq!(lexer.next().unwrap(), Token::Ident("main".to_string()));
        assert_eq!(lexer.next().unwrap(), Token::LParen);
        assert_eq!(lexer.next().unwrap(), Token::RParen);
        assert_eq!(lexer.next().unwrap(), Token::LBrace);
        assert_eq!(lexer.next().unwrap(), Token::Return);
        assert_eq!(lexer.next().unwrap(), Token::Int(0));
        assert_eq!(lexer.next().unwrap(), Token::Plus);
        assert_eq!(lexer.next().unwrap(), Token::Int(1));
        assert_eq!(lexer.next().unwrap(), Token::Minus);
        assert_eq!(lexer.next().unwrap(), Token::Int(2));
        assert_eq!(lexer.next().unwrap(), Token::Star);
        assert_eq!(lexer.next().unwrap(), Token::Int(3));
        assert_eq!(lexer.next().unwrap(), Token::Slash);
        assert_eq!(lexer.next().unwrap(), Token::Int(4));
        assert_eq!(lexer.next().unwrap(), Token::Percent);
        assert_eq!(lexer.next().unwrap(), Token::Int(5));
        assert_eq!(lexer.next().unwrap(), Token::Semicolon);
        assert_eq!(lexer.next().unwrap(), Token::RBrace);
        assert_eq!(lexer.next().unwrap(), Token::EOF);
    }

    #[test]
    fn singleton_symbol_test() {
        let statement = "=,;:(){}[]<>+-*/%!&";
        let mut lexer = Lexer::new(StringReader::new(statement.to_string()));

        assert_eq!(lexer.next().unwrap(), Token::Assign);
        assert_eq!(lexer.next().unwrap(), Token::Comma);
        assert_eq!(lexer.next().unwrap(), Token::Semicolon);
        assert_eq!(lexer.next().unwrap(), Token::Colon);
        assert_eq!(lexer.next().unwrap(), Token::LParen);
        assert_eq!(lexer.next().unwrap(), Token::RParen);
        assert_eq!(lexer.next().unwrap(), Token::LBrace);
        assert_eq!(lexer.next().unwrap(), Token::RBrace);
        assert_eq!(lexer.next().unwrap(), Token::LBracket);
        assert_eq!(lexer.next().unwrap(), Token::RBracket);
        assert_eq!(lexer.next().unwrap(), Token::LAngle);
        assert_eq!(lexer.next().unwrap(), Token::RAngle);
        assert_eq!(lexer.next().unwrap(), Token::Plus);
        assert_eq!(lexer.next().unwrap(), Token::Minus);
        assert_eq!(lexer.next().unwrap(), Token::Star);
        assert_eq!(lexer.next().unwrap(), Token::Slash);
        assert_eq!(lexer.next().unwrap(), Token::Percent);
        assert_eq!(lexer.next().unwrap(), Token::Exclamation);
        assert_eq!(lexer.next().unwrap(), Token::Ampersand);
    }

    #[test]
    fn symbol_equals_test() {
        let statement = "== != <= >= += -= *= /= %=";
        let mut lexer = Lexer::new(StringReader::new(statement.to_string()));

        assert_eq!(lexer.next().unwrap(), Token::Equal);
        assert_eq!(lexer.next().unwrap(), Token::NotEqual);
        assert_eq!(lexer.next().unwrap(), Token::Leq);
        assert_eq!(lexer.next().unwrap(), Token::Geq);
        assert_eq!(lexer.next().unwrap(), Token::PlusAssign);
        assert_eq!(lexer.next().unwrap(), Token::MinusAssign);
        assert_eq!(lexer.next().unwrap(), Token::StarAssign);
        assert_eq!(lexer.next().unwrap(), Token::SlashAssign);
        assert_eq!(lexer.next().unwrap(), Token::PercentAssign);
    }

    #[test]
    fn key_word_test() {
        let statement = "const static void int float double bool string struct impl fn rec inline if else while for return break continue true false";
        let mut lexer = Lexer::new(StringReader::new(statement.to_string()));

        assert_eq!(lexer.next().unwrap(), Token::Const);
        assert_eq!(lexer.next().unwrap(), Token::Static);
        assert_eq!(lexer.next().unwrap(), Token::VoidType);
        assert_eq!(lexer.next().unwrap(), Token::IntType);
        assert_eq!(lexer.next().unwrap(), Token::FloatType);
        assert_eq!(lexer.next().unwrap(), Token::DoubleType);
        assert_eq!(lexer.next().unwrap(), Token::BoolType);
        assert_eq!(lexer.next().unwrap(), Token::StringType);
        assert_eq!(lexer.next().unwrap(), Token::StructType);
        assert_eq!(lexer.next().unwrap(), Token::Impl);
        assert_eq!(lexer.next().unwrap(), Token::Fn);
        assert_eq!(lexer.next().unwrap(), Token::Rec);
        assert_eq!(lexer.next().unwrap(), Token::Inline);
        assert_eq!(lexer.next().unwrap(), Token::If);
        assert_eq!(lexer.next().unwrap(), Token::Else);
        assert_eq!(lexer.next().unwrap(), Token::While);
        assert_eq!(lexer.next().unwrap(), Token::For);
        assert_eq!(lexer.next().unwrap(), Token::Return);
        assert_eq!(lexer.next().unwrap(), Token::Break);
        assert_eq!(lexer.next().unwrap(), Token::Continue);
        assert_eq!(lexer.next().unwrap(), Token::Bool(true));
        assert_eq!(lexer.next().unwrap(), Token::Bool(false));
    }
}
