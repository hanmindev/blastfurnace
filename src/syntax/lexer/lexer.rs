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
        return prev;
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

            return Token::Ident(String::from(ident));
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
                _ => Invalid(String::from(format!("{prev}="))),
            };
        }

        // match singletons
        return match prev {
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
        };
    }
}