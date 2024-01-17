use crate::syntax::parse::ast_types::{
    AtomicExpression, BinOp, Block, Compound, CompoundValue, Expression, FnCall, FnDef, FnMod, For,
    If, LiteralValue, NamePath, Statement, StatementBlock, StructAssign, StructDecl, Type, UnOp,
    VarAssign, VarDecl, VarMod, While,
};
use crate::syntax::token::lexer::TokenError;
use crate::syntax::token::token_types::Token;
use crate::syntax::token::token_types::Token::Any;
use std::collections::HashMap;
use std::mem;

#[derive(Debug)]
pub enum ParseError {
    Unexpected(Token),
}

pub type ParseResult<T> = Result<T, ParseError>;

pub trait TokenStream {
    fn next(&mut self) -> Result<Token, TokenError>;
}

pub struct Parser<T: TokenStream> {
    lexer: T,
    curr_token: Token,
    next_token: Token,
}

impl<T: TokenStream> Parser<T> {
    pub fn new(lexer: T) -> Parser<T> {
        let mut parser = Parser {
            lexer,
            curr_token: Token::EOF,
            next_token: Token::EOF,
        };
        parser.eat(&Token::EOF).unwrap();
        parser.eat(&Token::EOF).unwrap();
        parser
    }

    fn next(&mut self) -> Token {
        self.lexer.next().unwrap()
    }

    fn eat(&mut self, type_: &Token) -> ParseResult<Token> {
        // return old token, set new token, set one buffer of next token
        if mem::discriminant(&self.curr_token) == mem::discriminant(type_) || matches!(type_, Any) {
            let old_curr = self.curr_token.clone();
            self.curr_token = self.next_token.clone();
            self.next_token = self.next();

            Ok(old_curr)
        } else {
            Err(ParseError::Unexpected(self.curr_token.clone()))
        }
    }

    fn string_to_namepath(&mut self, s: &str) -> NamePath {
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
        match self.eat(&Any)? {
            Token::Bool(b) => Ok(AtomicExpression::Literal(LiteralValue::Bool(b))),
            Token::Int(i) => Ok(AtomicExpression::Literal(LiteralValue::Int(i))),
            Token::Decimal(f) => Ok(AtomicExpression::Literal(LiteralValue::Decimal(f))),
            Token::String(s) => Ok(AtomicExpression::Literal(LiteralValue::String(s))),
            Token::Ident(s) => {
                if matches!(self.next_token, Token::LParen) {
                    let mut fn_call = Box::from(FnCall {
                        path: self.string_to_namepath(&s),
                        args: Vec::new(),
                    });
                    self.eat(&Token::LParen)?;

                    loop {
                        fn_call.args.push(self.parse_expression()?);
                        match self.eat(&Token::Comma) {
                            Ok(_) => {}
                            Err(_) => {
                                break;
                            }
                        }
                    }
                    self.eat(&Token::RParen)?; // eat RParen

                    // function call
                    Ok(AtomicExpression::FnCall(fn_call))
                } else {
                    // variable
                    Ok(AtomicExpression::Variable(self.string_to_namepath(&s)))
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

    fn parse_bin_op_rhs(
        &mut self,
        expr_prec: i32,
        mut lhs: Box<Expression>,
    ) -> ParseResult<Box<Expression>> {
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
            self.eat(&Any)?; // eat binop

            let mut rhs = self.parse_expression()?;

            let next_prec = self.get_bin_op_prec(&binop);

            if expr_prec < next_prec && expr_prec != 0 {
                rhs = self.parse_bin_op_rhs(next_prec, rhs)?;
            }

            lhs = Box::from(Expression::Binary(lhs, binop, rhs));
        }
        Ok(lhs)
    }

    fn parse_expression(&mut self) -> ParseResult<Box<Expression>> {
        match self.curr_token {
            Token::LParen => {
                self.eat(&Token::LParen)?;
                let expr = self.parse_expression()?;
                self.eat(&Token::RParen)?;
                Ok(self.parse_bin_op_rhs(0, expr)?)
            }
            _ => {
                // check if prefixed unary
                let unop = match self.curr_token {
                    Token::Plus => {
                        let right = self.parse_expression()?;
                        return Ok(right);
                    }
                    Token::Minus => UnOp::Neg,
                    Token::Exclamation => UnOp::Not,
                    Token::Star => UnOp::Deref,
                    Token::Ampersand => UnOp::Ref,
                    _ => {
                        // not prefixed unary
                        let expr = Box::from(Expression::AtomicExpression(
                            self.parse_atomic_expression()?,
                        ));
                        return Ok(self.parse_bin_op_rhs(0, expr)?);
                    }
                };

                let right = self.parse_expression()?;
                Ok(Box::from(Expression::Unary(unop, right)))
            }
        }
    }

    fn parse_if_statement(&mut self) -> ParseResult<If> {
        self.eat(&Token::If)?;
        self.eat(&Token::LParen)?;
        let cond = self.parse_expression()?;
        self.eat(&Token::RParen)?;
        let body = Box::from(self.parse_block()?);
        let mut else_ = None;
        if self.eat(&Token::Else).is_ok() {
            if self.eat(&Token::If).is_ok() {
                else_ = Some(Box::from(self.parse_if_statement()?));
            } else {
                else_ = Some(Box::from(If {
                    cond: Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Bool(true),
                    ))),
                    body: Box::from(self.parse_block()?),
                    else_: None,
                }));
            }
        }

        Ok(If { cond, body, else_ })
    }

    fn parse_for_statement(&mut self) -> ParseResult<For> {
        self.eat(&Token::For)?;
        self.eat(&Token::LParen)?;
        let init = Box::from(self.parse_statement()?);
        let cond = self.parse_expression()?;
        self.eat(&Token::Semicolon)?;
        let inc = Box::from(self.parse_statement()?);
        self.eat(&Token::RParen)?;
        let body = self.parse_block()?;

        Ok(For {
            init: Some(init),
            cond: Some(cond),
            step: Some(inc),
            body,
        })
    }

    fn parse_while_statement(&mut self) -> ParseResult<While> {
        self.eat(&Token::While)?;
        self.eat(&Token::LParen)?;
        let cond = self.parse_expression()?;
        self.eat(&Token::RParen)?;
        let body = Box::from(self.parse_block()?);

        Ok(While { cond, body })
    }

    fn parse_assignment(&mut self) -> ParseResult<Statement> {
        match &self.eat(&Any)? {
            Token::String(var_name) => {
                let path = self.string_to_namepath(var_name);

                let assign_op = match self.eat(&Any)? {
                    Token::Assign => {
                        return if matches!(self.curr_token, Token::LBrace) {
                            Ok(Statement::StructAssign(StructAssign {
                                path,
                                compound: self.parse_compound()?,
                            }))
                        } else {
                            Ok(Statement::VarAssign(VarAssign {
                                path,
                                expr: self.parse_expression()?,
                            }))
                        };
                    }
                    Token::PlusAssign => BinOp::Add,
                    Token::MinusAssign => BinOp::Sub,
                    Token::StarAssign => BinOp::Mul,
                    Token::SlashAssign => BinOp::Div,
                    Token::PercentAssign => BinOp::Mod,
                    _ => Err(ParseError::Unexpected(self.curr_token.clone()))?,
                };

                if matches!(self.curr_token, Token::LBrace) {
                    Err(ParseError::Unexpected(self.curr_token.clone()))?
                }

                let rhs = self.parse_expression()?;

                let expr = Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        path.clone(),
                    ))),
                    assign_op,
                    rhs,
                );

                Ok(Statement::VarAssign(VarAssign {
                    path,
                    expr: Box::from(expr),
                }))
            }
            _ => Err(ParseError::Unexpected(self.curr_token.clone())),
        }
    }

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        match &self.curr_token {
            Token::VoidType
            | Token::IntType
            | Token::FloatType
            | Token::DoubleType
            | Token::BoolType
            | Token::StringType
            | Token::Const
            | Token::Static => {
                // variable declaration
                let decl = self.parse_struct_or_var_decl()?;
                self.eat(&Token::Semicolon)?;
                Ok(decl)
            }

            Token::Ident(_) => {
                match &self.next_token {
                    Token::String(_) => {
                        // struct declaration
                        let decl = self.parse_struct_or_var_decl()?;
                        self.eat(&Token::Semicolon)?;
                        Ok(decl)
                    }

                    // variable / struct assignment
                    Token::Assign
                    | Token::PlusAssign
                    | Token::MinusAssign
                    | Token::StarAssign
                    | Token::SlashAssign
                    | Token::PercentAssign => {
                        let statement = self.parse_assignment();
                        self.eat(&Token::Semicolon)?;
                        statement
                    }
                    _ => {
                        let expr = self.parse_expression()?;
                        self.eat(&Token::Semicolon)?;
                        Ok(Statement::Expression(expr))
                    }
                }
            }

            Token::If => Ok(Statement::If(self.parse_if_statement()?)),
            Token::While => Ok(Statement::While(self.parse_while_statement()?)),
            Token::For => Ok(Statement::For(self.parse_for_statement()?)),
            Token::Return => {
                self.eat(&Token::Return)?;
                let expr = self.parse_expression()?;
                self.eat(&Token::Semicolon)?;
                Ok(Statement::Return(expr))
            }
            Token::Break => {
                self.eat(&Token::Break)?;
                Ok(Statement::Break)
            }
            Token::Continue => {
                self.eat(&Token::Continue)?;
                Ok(Statement::Continue)
            }
            Token::Fn => Ok(Statement::FnDef(self.parse_fn_def()?)),
            _ => Err(ParseError::Unexpected(self.curr_token.clone())),
        }
    }

    fn parse_block(&mut self) -> ParseResult<Block> {
        self.eat(&Token::LBrace)?;
        let mut statements = Vec::new();
        while !matches!(self.curr_token, Token::RBrace) {
            match self.curr_token {
                Token::LBrace => statements.push(StatementBlock::Block(self.parse_block()?)),
                _ => statements.push(StatementBlock::Statement(self.parse_statement()?)),
            }
        }
        self.eat(&Token::RBrace)?;

        Ok(Block { statements })
    }

    fn parse_compound_value(&mut self) -> ParseResult<CompoundValue> {
        match self.curr_token {
            Token::LBrace => Ok(CompoundValue::Compound(Box::from(self.parse_compound()?))),
            _ => Ok(CompoundValue::Expression(self.parse_expression()?)),
        }
    }

    fn parse_compound(&mut self) -> ParseResult<Compound> {
        let mut compound: Compound = HashMap::new();

        self.eat(&Token::LBrace)?;

        while let Token::String(key) = &mut self.curr_token {
            let key = key.clone();

            self.eat(&Any)?;
            self.eat(&Token::Colon)?;
            compound.insert(key, self.parse_compound_value()?);

            if matches!(self.curr_token, Token::RBrace) {
                break;
            } else {
                self.eat(&Token::Comma)?;
            }
        }

        self.eat(&Token::RBrace)?;

        Ok(compound)
    }

    fn parse_struct_decl(&mut self, mods: Vec<VarMod>) -> ParseResult<StructDecl> {
        match self.eat(&Any)? {
            Token::Ident(s_type) => {
                let type_ = Type::Struct(s_type.clone());

                match &self.curr_token {
                    Token::Ident(s) => {
                        if matches!(self.next_token, Token::Assign) {
                            // struct assignment

                            match self.parse_assignment()? {
                                Statement::StructAssign(struct_assign) => {
                                    if struct_assign.path.len() != 1 {
                                        Err(ParseError::Unexpected(self.curr_token.clone()))?
                                    }

                                    Ok(StructDecl {
                                        type_,
                                        name: struct_assign.path[0].clone(),
                                        mods,
                                        expr: Some(struct_assign.compound),
                                    })
                                }
                                _ => Err(ParseError::Unexpected(self.curr_token.clone())),
                            }
                        } else {
                            // just struct declaration
                            Ok(StructDecl {
                                type_,
                                name: String::from(s),
                                mods,
                                expr: None,
                            })
                        }
                    }
                    _ => Err(ParseError::Unexpected(self.curr_token.clone()))?,
                }
            }
            _ => Err(ParseError::Unexpected(self.curr_token.clone())),
        }
    }

    fn parse_var_decl(&mut self, mods: Vec<VarMod>) -> ParseResult<VarDecl> {
        let type_ = match self.eat(&Any)? {
            Token::VoidType => Type::Void,
            Token::IntType => Type::Int,
            Token::FloatType => Type::Float,
            Token::DoubleType => Type::Double,
            Token::BoolType => Type::Bool,
            Token::StringType => Type::String,
            _ => Err(ParseError::Unexpected(self.curr_token.clone()))?,
        };

        match self.parse_assignment()? {
            Statement::VarAssign(var_assign) => {
                if var_assign.path.len() != 1 {
                    Err(ParseError::Unexpected(self.curr_token.clone()))?
                }

                Ok(VarDecl {
                    type_,
                    name: var_assign.path[0].clone(),
                    mods,
                    expr: Some(var_assign.expr),
                })
            }
            _ => Err(ParseError::Unexpected(self.curr_token.clone())),
        }
    }

    fn parse_struct_or_var_decl(&mut self) -> ParseResult<Statement> {
        let mut mods: Vec<VarMod> = Vec::new();

        if self.eat(&Token::Const).is_ok() {
            mods.push(VarMod::Const);
        }

        if self.eat(&Token::Static).is_ok() {
            mods.push(VarMod::Static);
        }

        match self.curr_token {
            Token::Ident(_) => Ok(Statement::StructDecl(self.parse_struct_decl(mods)?)),
            _ => Ok(Statement::VarDecl(self.parse_var_decl(mods)?)),
        }
    }

    fn parse_fn_def(&mut self) -> ParseResult<FnDef> {
        let mut mods = Vec::new();

        if self.eat(&Token::Rec).is_ok() {
            mods.push(FnMod::Rec);
        }

        if self.eat(&Token::Inline).is_ok() {
            mods.push(FnMod::Inline);
        }

        self.eat(&Token::Fn)?;
        let name = match self.eat(&Any)? {
            Token::Ident(s) => s,
            _ => Err(ParseError::Unexpected(self.curr_token.clone()))?,
        };

        self.eat(&Token::LParen)?;
        let mut args = Vec::new();
        loop {
            if self.eat(&Token::Comma).is_err() {
                break;
            }

            let mut mods = Vec::new();
            if self.eat(&Token::Const).is_ok() {
                mods.push(VarMod::Const);
            }

            let type_ = match self.eat(&Any)? {
                Token::VoidType => Type::Void,
                Token::IntType => Type::Int,
                Token::FloatType => Type::Float,
                Token::DoubleType => Type::Double,
                Token::BoolType => Type::Bool,
                Token::StringType => Type::String,
                Token::Ident(s) => Type::Struct(s),
                _ => {
                    return Err(ParseError::Unexpected(self.curr_token.clone()));
                }
            };

            let name = match self.eat(&Any)? {
                Token::Ident(s) => s,
                _ => {
                    return Err(ParseError::Unexpected(self.curr_token.clone()));
                }
            };

            args.push((mods, type_, name));

            if self.eat(&Token::Comma).is_err() {
                break;
            }
        }

        self.eat(&Token::RParen)?;

        let body = self.parse_block()?;

        Ok(FnDef {
            name,
            args,
            body,
            mods,
        })
    }

    pub fn parse(&mut self) -> ParseResult<Block> {
        let mut statements = Vec::new();
        while !matches!(self.curr_token, Token::EOF) {
            statements.push(StatementBlock::Statement(self.parse_statement()?));
        }
        self.eat(&Token::EOF)?;

        Ok(Block { statements })
    }
}
