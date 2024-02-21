use crate::front::ast_retriever::reader::lexical::token_types::Token;
use crate::front::ast_retriever::reader::syntax::parser::TokenStream;
use crate::front::file_system::byte_stream::ByteStream;

pub struct Lexer {
    reader: ByteStream,
    curr: char,
    index: u64,
    return_index: u64,
}

#[derive(Debug, PartialEq)]
pub enum TokenError {
    InvalidToken(String),
    MultipleDecimals,
}

pub type TokenInfo = (Token, u64);

impl Lexer {
    pub fn new(reader: ByteStream) -> Lexer {
        let mut lexer = Lexer {
            reader,
            curr: '\0',
            index: 0,
            return_index: 0,
        };
        lexer.eat();
        lexer.index = 0;
        lexer
    }
    fn read_char(&mut self) -> char {
        self.index += 1;
        self.reader.next()
    }

    fn eat(&mut self) -> char {
        let prev = self.curr;
        self.curr = self.read_char();
        prev
    }

    fn get_token(&mut self) -> Result<TokenInfo, TokenError> {
        return Ok((self.parse_token()?, self.return_index));
    }

    fn parse_token(&mut self) -> Result<Token, TokenError> {
        // check for EOF
        if self.curr == '\0' {
            self.return_index = self.index;
            return Ok(Token::Eof);
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
        self.return_index = self.index;

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
            while self.curr.is_alphanumeric() || self.curr == '_' || self.curr == '-' {
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

                "null" => Token::Null,

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
                "let" => Token::Let,
                "const" => Token::Const,
                "inline" => Token::Inline,

                "use" => Token::Use,
                "as" => Token::As,
                "mod" => Token::Mod,
                "pub" => Token::Pub,

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
                    Ok(n) => {
                        if self.curr == 'd' {
                            self.eat();
                            Token::Double(n)
                        } else {
                            Token::Float(n as f32)
                        }
                    }
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

        match (prev, self.curr) {
            ('&', '&') => {
                self.eat();
                return Ok(Token::And);
            }
            ('|', '|') => {
                self.eat();
                return Ok(Token::Or);
            }
            ('+', '+') => {
                self.eat();
                return Ok(Token::PlusPlus);
            }
            ('-', '-') => {
                self.eat();
                return Ok(Token::MinusMinus);
            }
            ('-', '>') => {
                self.eat();
                return Ok(Token::Arrow);
            }
            _ => {}
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

impl TokenStream for Lexer {
    fn next(&mut self) -> Result<TokenInfo, TokenError> {
        self.get_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::front::file_system::byte_stream::StringReader;

    #[test]
    fn simple_test() {
        let statement = "fn main() { return 0; }";
        let mut lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));

        assert_eq!(lexer.next().unwrap(), (Token::Fn, 0));
        assert_eq!(lexer.next().unwrap(), (Token::Ident("main".to_string()), 3));
        assert_eq!(lexer.next().unwrap(), (Token::LParen, 7));
        assert_eq!(lexer.next().unwrap(), (Token::RParen, 8));
        assert_eq!(lexer.next().unwrap(), (Token::LBrace, 10));
        assert_eq!(lexer.next().unwrap(), (Token::Return, 12));
        assert_eq!(lexer.next().unwrap(), (Token::Int(0), 19));
        assert_eq!(lexer.next().unwrap(), (Token::Semicolon, 20));
        assert_eq!(lexer.next().unwrap(), (Token::RBrace, 22));
        assert_eq!(lexer.next().unwrap(), (Token::Eof, 23));
    }

    #[test]
    fn number_comprehension() {
        let statement = "643214 3243.24321 .432432 2342.342315.321534";
        let mut lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));

        assert_eq!(lexer.next().unwrap().0, Token::Int(643214));
        assert_eq!(lexer.next().unwrap().0, Token::Double(3243.24321));
        assert_eq!(lexer.next().unwrap().0, Token::Double(0.432432));
        assert_eq!(lexer.next().err().unwrap(), TokenError::MultipleDecimals);
    }

    #[test]
    fn comment_test() {
        let statement = "fn main() { // return 0; \n return 1; }";
        let mut lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));

        assert_eq!(lexer.next().unwrap().0, Token::Fn);
        assert_eq!(lexer.next().unwrap().0, Token::Ident("main".to_string()));
        assert_eq!(lexer.next().unwrap().0, Token::LParen);
        assert_eq!(lexer.next().unwrap().0, Token::RParen);
        assert_eq!(lexer.next().unwrap().0, Token::LBrace);
        assert_eq!(lexer.next().unwrap().0, Token::Return);
        assert_eq!(lexer.next().unwrap().0, Token::Int(1));
        assert_eq!(lexer.next().unwrap().0, Token::Semicolon);
        assert_eq!(lexer.next().unwrap().0, Token::RBrace);
        assert_eq!(lexer.next().unwrap().0, Token::Eof);
    }

    #[test]
    fn whitespace_test() {
        let statement = "fn main()                   {       return 0; }";
        let mut lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));

        assert_eq!(lexer.next().unwrap().0, Token::Fn);
        assert_eq!(lexer.next().unwrap().0, Token::Ident("main".to_string()));
        assert_eq!(lexer.next().unwrap().0, Token::LParen);
        assert_eq!(lexer.next().unwrap().0, Token::RParen);
        assert_eq!(lexer.next().unwrap().0, Token::LBrace);
        assert_eq!(lexer.next().unwrap().0, Token::Return);
        assert_eq!(lexer.next().unwrap().0, Token::Int(0));
        assert_eq!(lexer.next().unwrap().0, Token::Semicolon);
        assert_eq!(lexer.next().unwrap().0, Token::RBrace);
        assert_eq!(lexer.next().unwrap().0, Token::Eof);
    }

    #[test]
    fn operator_test() {
        let statement = "fn main() { return 0 + 1 - 2 * 3 / 4 % 5; }";
        let mut lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));

        assert_eq!(lexer.next().unwrap().0, Token::Fn);
        assert_eq!(lexer.next().unwrap().0, Token::Ident("main".to_string()));
        assert_eq!(lexer.next().unwrap().0, Token::LParen);
        assert_eq!(lexer.next().unwrap().0, Token::RParen);
        assert_eq!(lexer.next().unwrap().0, Token::LBrace);
        assert_eq!(lexer.next().unwrap().0, Token::Return);
        assert_eq!(lexer.next().unwrap().0, Token::Int(0));
        assert_eq!(lexer.next().unwrap().0, Token::Plus);
        assert_eq!(lexer.next().unwrap().0, Token::Int(1));
        assert_eq!(lexer.next().unwrap().0, Token::Minus);
        assert_eq!(lexer.next().unwrap().0, Token::Int(2));
        assert_eq!(lexer.next().unwrap().0, Token::Star);
        assert_eq!(lexer.next().unwrap().0, Token::Int(3));
        assert_eq!(lexer.next().unwrap().0, Token::Slash);
        assert_eq!(lexer.next().unwrap().0, Token::Int(4));
        assert_eq!(lexer.next().unwrap().0, Token::Percent);
        assert_eq!(lexer.next().unwrap().0, Token::Int(5));
        assert_eq!(lexer.next().unwrap().0, Token::Semicolon);
        assert_eq!(lexer.next().unwrap().0, Token::RBrace);
        assert_eq!(lexer.next().unwrap().0, Token::Eof);
    }

    #[test]
    fn singleton_symbol_test() {
        let statement = "=,;:(){}[]<>+-*/%!&";
        let mut lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));

        assert_eq!(lexer.next().unwrap().0, Token::Assign);
        assert_eq!(lexer.next().unwrap().0, Token::Comma);
        assert_eq!(lexer.next().unwrap().0, Token::Semicolon);
        assert_eq!(lexer.next().unwrap().0, Token::Colon);
        assert_eq!(lexer.next().unwrap().0, Token::LParen);
        assert_eq!(lexer.next().unwrap().0, Token::RParen);
        assert_eq!(lexer.next().unwrap().0, Token::LBrace);
        assert_eq!(lexer.next().unwrap().0, Token::RBrace);
        assert_eq!(lexer.next().unwrap().0, Token::LBracket);
        assert_eq!(lexer.next().unwrap().0, Token::RBracket);
        assert_eq!(lexer.next().unwrap().0, Token::LAngle);
        assert_eq!(lexer.next().unwrap().0, Token::RAngle);
        assert_eq!(lexer.next().unwrap().0, Token::Plus);
        assert_eq!(lexer.next().unwrap().0, Token::Minus);
        assert_eq!(lexer.next().unwrap().0, Token::Star);
        assert_eq!(lexer.next().unwrap().0, Token::Slash);
        assert_eq!(lexer.next().unwrap().0, Token::Percent);
        assert_eq!(lexer.next().unwrap().0, Token::Exclamation);
        assert_eq!(lexer.next().unwrap().0, Token::Ampersand);
    }

    #[test]
    fn symbol_equals_test() {
        let statement = "== != <= >= += -= *= /= %=";
        let mut lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));

        assert_eq!(lexer.next().unwrap().0, Token::Equal);
        assert_eq!(lexer.next().unwrap().0, Token::NotEqual);
        assert_eq!(lexer.next().unwrap().0, Token::Leq);
        assert_eq!(lexer.next().unwrap().0, Token::Geq);
        assert_eq!(lexer.next().unwrap().0, Token::PlusAssign);
        assert_eq!(lexer.next().unwrap().0, Token::MinusAssign);
        assert_eq!(lexer.next().unwrap().0, Token::StarAssign);
        assert_eq!(lexer.next().unwrap().0, Token::SlashAssign);
        assert_eq!(lexer.next().unwrap().0, Token::PercentAssign);
    }

    #[test]
    fn other_symbols_test() {
        let statement = "&& || ++ --";
        let mut lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));

        assert_eq!(lexer.next().unwrap().0, Token::And);
        assert_eq!(lexer.next().unwrap().0, Token::Or);
        assert_eq!(lexer.next().unwrap().0, Token::PlusPlus);
        assert_eq!(lexer.next().unwrap().0, Token::MinusMinus);
    }

    #[test]
    fn key_word_test() {
        let statement = "const void int float double bool string struct impl fn rec inline if else while for return break continue true false use as mod pub";
        let mut lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));

        assert_eq!(lexer.next().unwrap().0, Token::Const);
        assert_eq!(lexer.next().unwrap().0, Token::VoidType);
        assert_eq!(lexer.next().unwrap().0, Token::IntType);
        assert_eq!(lexer.next().unwrap().0, Token::FloatType);
        assert_eq!(lexer.next().unwrap().0, Token::DoubleType);
        assert_eq!(lexer.next().unwrap().0, Token::BoolType);
        assert_eq!(lexer.next().unwrap().0, Token::StringType);
        assert_eq!(lexer.next().unwrap().0, Token::StructType);
        assert_eq!(lexer.next().unwrap().0, Token::Impl);
        assert_eq!(lexer.next().unwrap().0, Token::Fn);
        assert_eq!(lexer.next().unwrap().0, Token::Rec);
        assert_eq!(lexer.next().unwrap().0, Token::Inline);
        assert_eq!(lexer.next().unwrap().0, Token::If);
        assert_eq!(lexer.next().unwrap().0, Token::Else);
        assert_eq!(lexer.next().unwrap().0, Token::While);
        assert_eq!(lexer.next().unwrap().0, Token::For);
        assert_eq!(lexer.next().unwrap().0, Token::Return);
        assert_eq!(lexer.next().unwrap().0, Token::Break);
        assert_eq!(lexer.next().unwrap().0, Token::Continue);
        assert_eq!(lexer.next().unwrap().0, Token::Bool(true));
        assert_eq!(lexer.next().unwrap().0, Token::Bool(false));
        assert_eq!(lexer.next().unwrap().0, Token::Use);
        assert_eq!(lexer.next().unwrap().0, Token::As);
        assert_eq!(lexer.next().unwrap().0, Token::Mod);
        assert_eq!(lexer.next().unwrap().0, Token::Pub);
    }
}
