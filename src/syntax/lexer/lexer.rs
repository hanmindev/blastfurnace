use std::io;
use std::io::Read;
use crate::syntax::lexer::token_types::Token;
use crate::syntax::lexer::token_types::Token::Invalid;

pub struct Lexer {
    curr: char,
}

impl Lexer {
    fn read_char(&self) -> char {
        let mut buf = [0; 1];

        match io::stdin().read(&mut buf) {
            Ok(n) => {
                if n == 1 {
                    buf[0] as char
                } else {
                    '\0'
                }
            }
            Err(_e) => {
                panic!("Error reading from stdin: {_e}");
            }
        }
    }

    fn eat(&mut self) -> char {
        let prev = self.curr;
        self.curr = self.read_char();
        prev
    }

    pub fn get_token(&mut self) -> Token {
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