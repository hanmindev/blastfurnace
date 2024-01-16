use std::mem;
use crate::syntax::lexer::lexer::Lexer;
use crate::syntax::lexer::token_types::Token;
use crate::syntax::parser::ast_types::{AtomicExpression, BinOp, Expression, FnCall, LiteralValue, NamePath, UnOp};

#[derive(Debug)]
pub enum ParseError {
    Unexpected(Token),
}

pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
    next_token: Token,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            curr_token: Token::EOF,
            next_token: Token::EOF,
        };
        let tok = Token::EOF;
        parser.eat(&tok).unwrap();
        parser
    }

    fn eat(&mut self, type_: &Token) -> ParseResult<Token> {
        // return old token, set new token, set one buffer of next token
        if mem::discriminant(&self.curr_token) == mem::discriminant(type_) || matches!(self.curr_token, Token::Any) {
            Ok(mem::replace(&mut self.curr_token, mem::replace(&mut self.next_token, self.lexer.get_token())))
        } else {
            Err(ParseError::Unexpected(self.curr_token.clone()))
        }
    }

    fn string_to_namepath(&mut self, s: String) -> NamePath {
        let mut path = Vec::new();
        let mut curr = String::new();
        for c in s.chars() {
            if c == '.' {
                path.push(curr);
                curr = String::new();
            } else {
                curr.push(c);
            }
        }
        path.push(curr);
        path
    }

    fn parse_atomic_expression(&mut self) -> ParseResult<AtomicExpression> {
        match self.eat(&Token::Any)? {
            Token::Bool(b) => {
                Ok(AtomicExpression::Literal(LiteralValue::Bool(b)))
            }
            Token::Int(i) => {
                Ok(AtomicExpression::Literal(LiteralValue::Int(i)))
            }
            Token::Decimal(f) => {
                Ok(AtomicExpression::Literal(LiteralValue::Decimal(f)))
            }
            Token::String(s) => {
                Ok(AtomicExpression::Literal(LiteralValue::String(s)))
            }
            Token::Ident(s) => {
                if matches!(self.next_token, Token::LParen) {
                    let mut fn_call = Box::from(FnCall { path: self.string_to_namepath(s), args: Vec::new() });

                    self.eat(&Token::LParen)?;

                    while !matches!(self.curr_token, Token::RParen) {
                        fn_call.args.push(self.parse_expression()?);
                        self.eat(&Token::Comma)?;
                    }
                    self.eat(&Token::RParen)?; // eat RParen

                    // function call
                    Ok(AtomicExpression::FnCall(fn_call))
                } else {
                    // variable
                    Ok(AtomicExpression::Variable(self.string_to_namepath(s)))
                }
            }
            _ => Err(ParseError::Unexpected(self.curr_token.clone())),
        }
    }

    fn get_bin_op_prec(&self, binop: &BinOp) -> i32 {
        match binop {
            BinOp::Add => 10,
            BinOp::Sub => 10,
            BinOp::Mul => 20,
            BinOp::Div => 20,
            BinOp::Mod => 20,
            BinOp::Eq => 5,
            BinOp::Neq => 5,
            BinOp::Lt => 5,
            BinOp::Gt => 5,
            BinOp::Leq => 5,
            BinOp::Geq => 5,
        }
    }

    fn parse_bin_op_rhs(&mut self, expr_prec: i32, mut lhs: Expression) -> ParseResult<Expression> {
        loop {
            let binop = match self.curr_token {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Sub,
                Token::Star => BinOp::Mul,
                Token::Slash => BinOp::Div,
                Token::Percent => BinOp::Mod,
                Token::Equal => BinOp::Eq,
                Token::NotEqual => BinOp::Neq,
                Token::LAngle => BinOp::Lt,
                Token::RAngle => BinOp::Gt,
                Token::Leq => BinOp::Leq,
                Token::Geq => BinOp::Geq,
                _ => {
                    break;
                }
            };
            self.eat(&Token::Any)?; // eat binop

            let mut rhs = self.parse_expression()?;

            let next_prec = self.get_bin_op_prec(&binop);

            if expr_prec < next_prec && expr_prec != 0 {
                rhs = self.parse_bin_op_rhs(next_prec, rhs)?;
            }

            lhs = Expression::Binary(Box::from(lhs), binop, Box::from(rhs));
        }
        Ok(lhs)
    }

    fn parse_expression(&mut self) -> ParseResult<Expression> {
        match self.curr_token {
            Token::LParen => {
                self.eat(&Token::LParen)?;
                let expr = self.parse_expression()?;
                self.eat(&Token::RParen)?;
                Ok(self.parse_bin_op_rhs(0, expr)?)
            }
            _ => {
                // check if unary
                if matches!(self.curr_token, Token::Plus | Token::Minus | Token::Exclamation | Token::Star | Token::Ampersand) {
                    let op = self.eat(&Token::Any)?;
                    let right = self.parse_expression()?;

                    let unop = match op {
                        Token::Plus => {
                            return Ok(right);
                        }
                        Token::Minus => UnOp::Neg,
                        Token::Exclamation => UnOp::Not,
                        Token::Star => UnOp::Deref,
                        Token::Ampersand => UnOp::Ref,
                        _ => {
                            panic!("Invalid unary operator. Match arms in parse_expression() are not exhaustive.");
                        }
                    };

                    Ok(Expression::Unary(unop, Box::from(right)))
                } else {
                    let expr = Expression::AtomicExpression(self.parse_atomic_expression()?);
                    Ok(self.parse_bin_op_rhs(0, expr)?)
                }
            }
        }
    }
}