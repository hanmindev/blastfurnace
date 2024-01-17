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
    Unknown,
    Unexpected(Token, String),
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
            Err(ParseError::Unexpected(
                self.curr_token.clone(),
                format!("Tried to eat {:?}, ate {:?}", type_, self.curr_token),
            ))
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
                if matches!(self.curr_token, Token::LParen) {
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
            tok => Err(ParseError::Unexpected(
                tok,
                "Expected type for atomic expression parsing".to_string(),
            )),
        }
    }

    fn get_bin_op_prec(&self, binop: &BinOp) -> i32 {
        match binop {
            BinOp::Add => 10,
            BinOp::Sub => 10,
            BinOp::Mul => 20,
            BinOp::Div => 20,
            BinOp::Mod => 20,
            BinOp::Eq => 30,
            BinOp::Neq => 30,
            BinOp::Lt => 30,
            BinOp::Gt => 30,
            BinOp::Leq => 30,
            BinOp::Geq => 30,
        }
    }

    fn parse_bin_op_rhs(
        &mut self,
        mut prev_binop: BinOp,
        mut lhs: Box<Expression>,
    ) -> ParseResult<Box<Expression>> {
        // start on rhs
        loop {
            let mut rhs = self.parse_expression_single()?;

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
                    return Ok(Box::from(Expression::Binary(lhs, prev_binop, rhs)));
                }
            };

            let expr_prec = self.get_bin_op_prec(&prev_binop);
            let next_prec = self.get_bin_op_prec(&binop);

            if expr_prec < next_prec {
                self.eat(&Any)?;
                rhs = self.parse_bin_op_rhs(binop.clone(), rhs)?;
            } else if expr_prec > next_prec {
                return Ok(Box::from(Expression::Binary(lhs, prev_binop, rhs)));
            }

            lhs = Box::from(Expression::Binary(lhs, prev_binop, rhs));

            prev_binop = match self.curr_token {
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
                    return Ok(lhs);
                }
            };

            self.eat(&Any)?;
        }
    }

    fn parse_expression_single(&mut self) -> ParseResult<Box<Expression>> {
        match self.curr_token {
            Token::LParen => {
                self.eat(&Token::LParen)?;
                let expr = self.parse_expression()?;
                self.eat(&Token::RParen)?;
                Ok(expr)
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
                        return Ok(expr);
                    }
                };

                // eat unop
                self.eat(&Any)?;

                let right = self.parse_expression_single()?;
                Ok(Box::from(Expression::Unary(unop, right)))
            }
        }
    }

    fn parse_expression(&mut self) -> ParseResult<Box<Expression>> {
        // not prefixed unary
        let lhs = Box::from(self.parse_expression_single()?);
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
                return Ok(lhs);
            }
        };
        self.eat(&Any)?;
        return Ok(self.parse_bin_op_rhs(binop, lhs)?);
    }

    fn parse_if_statement(&mut self) -> ParseResult<If> {
        self.eat(&Token::If)?;
        self.eat(&Token::LParen)?;
        let cond = self.parse_expression()?;
        self.eat(&Token::RParen)?;
        let body = Box::from(self.parse_block()?);
        let mut else_ = None;
        if self.eat(&Token::Else).is_ok() {
            if matches!(self.curr_token, Token::If) {
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
        self.eat(&Token::Semicolon)?;
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
            Token::Ident(var_name) => {
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
                    tok => Err(ParseError::Unexpected(
                        tok.clone(),
                        "Expected binary operator".to_string(),
                    ))?,
                };

                if matches!(self.curr_token, Token::LBrace) {
                    Err(ParseError::Unexpected(
                        self.curr_token.clone(),
                        "Cannot use for struct assignment".to_string(),
                    ))?
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
            tok => Err(ParseError::Unexpected(
                tok.clone(),
                "Expected identifier for variable name".to_string(),
            )),
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
                Ok(self.parse_struct_or_var_decl()?)
            }

            Token::Ident(_) => {
                match &self.next_token {
                    Token::Ident(_) => {
                        // struct declaration
                        Ok(self.parse_struct_or_var_decl()?)
                    }

                    // variable / struct assignment
                    Token::Assign
                    | Token::PlusAssign
                    | Token::MinusAssign
                    | Token::StarAssign
                    | Token::SlashAssign
                    | Token::PercentAssign => self.parse_assignment(),
                    _ => Ok(Statement::Expression(self.parse_expression()?)),
                }
            }

            Token::If => Ok(Statement::If(self.parse_if_statement()?)),
            Token::While => Ok(Statement::While(self.parse_while_statement()?)),
            Token::For => Ok(Statement::For(self.parse_for_statement()?)),
            Token::Return => {
                self.eat(&Token::Return)?;
                Ok(Statement::Return(self.parse_expression()?))
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
            tok => Err(ParseError::Unexpected(
                tok.clone(),
                "Expected valid token for statement beginning".to_string(),
            )),
        }
    }

    fn parse_block(&mut self) -> ParseResult<Block> {
        self.eat(&Token::LBrace)?;
        let mut statements = Vec::new();
        while !matches!(self.curr_token, Token::RBrace) {
            match self.curr_token {
                Token::LBrace => statements.push(StatementBlock::Block(self.parse_block()?)),
                _ => {
                    statements.push(StatementBlock::Statement(self.parse_statement()?));
                    self.eat(&Token::Semicolon)?;
                }
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

        while let Token::Ident(key) = &mut self.curr_token {
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
                                        Err(ParseError::Unexpected(
                                            self.curr_token.clone(),
                                            "Struct assignment path must be length 1".to_string(),
                                        ))?
                                    }

                                    Ok(StructDecl {
                                        type_,
                                        name: struct_assign.path[0].clone(),
                                        mods,
                                        expr: Some(struct_assign.compound),
                                    })
                                }
                                _ => Err(ParseError::Unknown),
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
                    tok => Err(ParseError::Unexpected(
                        tok.clone(),
                        "Expected struct name for struct declaration".to_string(),
                    ))?,
                }
            }
            tok => Err(ParseError::Unexpected(
                tok.clone(),
                "Expected struct type for struct declaration".to_string(),
            )),
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
            tok => Err(ParseError::Unexpected(
                tok,
                "Expected variable type annotation for variable declaration".to_string(),
            ))?,
        };

        match self.parse_assignment()? {
            Statement::VarAssign(var_assign) => {
                if var_assign.path.len() != 1 {
                    Err(ParseError::Unexpected(
                        self.curr_token.clone(),
                        "Variable assignment path must be length 1".to_string(),
                    ))?
                }

                Ok(VarDecl {
                    type_,
                    name: var_assign.path[0].clone(),
                    mods,
                    expr: Some(var_assign.expr),
                })
            }
            _ => Err(ParseError::Unknown),
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
            tok => Err(ParseError::Unexpected(
                tok,
                "Expected identifier for function name".to_string(),
            ))?,
        };

        self.eat(&Token::LParen)?;
        let mut args = Vec::new();

        if self.eat(&Token::RParen).is_err() {
            loop {
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
                    tok => {
                        return Err(ParseError::Unexpected(
                            tok,
                            "Expected type for function argument".to_string(),
                        ));
                    }
                };

                let name = match self.eat(&Any)? {
                    Token::Ident(s) => s,
                    tok => {
                        return Err(ParseError::Unexpected(
                            tok,
                            "Expected identifier for function argument name".to_string(),
                        ));
                    }
                };

                args.push((mods, type_, name));

                if self.eat(&Token::Comma).is_err() {
                    break;
                }
            }

            self.eat(&Token::RParen)?;
        }

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
            let statement = self.parse_statement()?;

            if !matches!(
                statement,
                Statement::FnDef(_) | Statement::If(_) | Statement::For(_) | Statement::While(_)
            ) {
                self.eat(&Token::Semicolon)?;
            }

            statements.push(StatementBlock::Statement(statement));
        }
        self.eat(&Token::EOF)?;

        Ok(Block { statements })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::token::lexer::Lexer;
    use crate::syntax::token::lexer_string_reader::StringReader;

    #[test]
    fn simple_test() {
        let statement = "fn main() { return 0; }";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let block = parser.parse().unwrap();

        assert_eq!(block.statements.len(), 1);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::FnDef(FnDef {
                name: "main".to_string(),
                args: Vec::new(),
                body: Block {
                    statements: vec![StatementBlock::Statement(Statement::Return(Box::from(
                        Expression::AtomicExpression(AtomicExpression::Literal(LiteralValue::Int(
                            0
                        )))
                    )))],
                },
                mods: Vec::new(),
            }))
        );
    }

    #[test]
    fn variable_declarations_test() {
        let statement = "const int a = 0; static int b = 1; int c = 2; float d = 3.0; double e = 4.0; bool f = true; string g = \"hello\";";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let block = parser.parse().unwrap();

        assert_eq!(block.statements.len(), 7);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::VarDecl(VarDecl {
                type_: Type::Int,
                name: "a".to_string(),
                mods: vec![VarMod::Const],
                expr: Some(Box::from(Expression::AtomicExpression(
                    AtomicExpression::Literal(LiteralValue::Int(0))
                ))),
            }))
        );
        assert_eq!(
            block.statements[1],
            StatementBlock::Statement(Statement::VarDecl(VarDecl {
                type_: Type::Int,
                name: "b".to_string(),
                mods: vec![VarMod::Static],
                expr: Some(Box::from(Expression::AtomicExpression(
                    AtomicExpression::Literal(LiteralValue::Int(1))
                ))),
            }))
        );
        assert_eq!(
            block.statements[2],
            StatementBlock::Statement(Statement::VarDecl(VarDecl {
                type_: Type::Int,
                name: "c".to_string(),
                mods: Vec::new(),
                expr: Some(Box::from(Expression::AtomicExpression(
                    AtomicExpression::Literal(LiteralValue::Int(2))
                ))),
            }))
        );
        assert_eq!(
            block.statements[3],
            StatementBlock::Statement(Statement::VarDecl(VarDecl {
                type_: Type::Float,
                name: "d".to_string(),
                mods: Vec::new(),
                expr: Some(Box::from(Expression::AtomicExpression(
                    AtomicExpression::Literal(LiteralValue::Decimal(3.0))
                ))),
            }))
        );
        assert_eq!(
            block.statements[4],
            StatementBlock::Statement(Statement::VarDecl(VarDecl {
                type_: Type::Double,
                name: "e".to_string(),
                mods: Vec::new(),
                expr: Some(Box::from(Expression::AtomicExpression(
                    AtomicExpression::Literal(LiteralValue::Decimal(4.0))
                ))),
            }))
        );
        assert_eq!(
            block.statements[5],
            StatementBlock::Statement(Statement::VarDecl(VarDecl {
                type_: Type::Bool,
                name: "f".to_string(),
                mods: Vec::new(),
                expr: Some(Box::from(Expression::AtomicExpression(
                    AtomicExpression::Literal(LiteralValue::Bool(true))
                ))),
            }))
        );
        assert_eq!(
            block.statements[6],
            StatementBlock::Statement(Statement::VarDecl(VarDecl {
                type_: Type::String,
                name: "g".to_string(),
                mods: Vec::new(),
                expr: Some(Box::from(Expression::AtomicExpression(
                    AtomicExpression::Literal(LiteralValue::String("hello".to_string()))
                ))),
            }))
        );
    }

    #[test]
    fn struct_declarations_test() {
        let statement = "A a = { a: 0, b: 1, c: 2 }; B b = { a: 0, b: \"hello\", c: 2.54 };";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let block = parser.parse().unwrap();

        assert_eq!(block.statements.len(), 2);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::StructDecl(StructDecl {
                type_: Type::Struct("A".to_string()),
                name: "a".to_string(),
                mods: Vec::new(),
                expr: Some(
                    vec![
                        (
                            "a".to_string(),
                            CompoundValue::Expression(Box::from(Expression::AtomicExpression(
                                AtomicExpression::Literal(LiteralValue::Int(0))
                            )))
                        ),
                        (
                            "b".to_string(),
                            CompoundValue::Expression(Box::from(Expression::AtomicExpression(
                                AtomicExpression::Literal(LiteralValue::Int(1))
                            )))
                        ),
                        (
                            "c".to_string(),
                            CompoundValue::Expression(Box::from(Expression::AtomicExpression(
                                AtomicExpression::Literal(LiteralValue::Int(2))
                            )))
                        ),
                    ]
                    .into_iter()
                    .collect()
                ),
            }))
        );
    }

    #[test]
    fn variable_assignment_tests() {
        let statement = "a = 0; b = 2.4; a += 2; a -= 3; a *= 4; a /= 5; a %= 6;";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let block = parser.parse().unwrap();

        assert_eq!(block.statements.len(), 7);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::VarAssign(VarAssign {
                path: vec!["a".to_string()],
                expr: Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                    LiteralValue::Int(0)
                ))),
            }))
        );
        assert_eq!(
            block.statements[1],
            StatementBlock::Statement(Statement::VarAssign(VarAssign {
                path: vec!["b".to_string()],
                expr: Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                    LiteralValue::Decimal(2.4)
                ))),
            }))
        );
        assert_eq!(
            block.statements[2],
            StatementBlock::Statement(Statement::VarAssign(VarAssign {
                path: vec!["a".to_string()],
                expr: Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        vec!["a".to_string()]
                    ))),
                    BinOp::Add,
                    Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(2)
                    ))),
                )),
            }))
        );
        assert_eq!(
            block.statements[3],
            StatementBlock::Statement(Statement::VarAssign(VarAssign {
                path: vec!["a".to_string()],
                expr: Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        vec!["a".to_string()]
                    ))),
                    BinOp::Sub,
                    Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(3)
                    ))),
                )),
            }))
        );
        assert_eq!(
            block.statements[4],
            StatementBlock::Statement(Statement::VarAssign(VarAssign {
                path: vec!["a".to_string()],
                expr: Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        vec!["a".to_string()]
                    ))),
                    BinOp::Mul,
                    Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(4)
                    ))),
                )),
            }))
        );
        assert_eq!(
            block.statements[5],
            StatementBlock::Statement(Statement::VarAssign(VarAssign {
                path: vec!["a".to_string()],
                expr: Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        vec!["a".to_string()]
                    ))),
                    BinOp::Div,
                    Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(5)
                    ))),
                )),
            }))
        );
        assert_eq!(
            block.statements[6],
            StatementBlock::Statement(Statement::VarAssign(VarAssign {
                path: vec!["a".to_string()],
                expr: Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        vec!["a".to_string()]
                    ))),
                    BinOp::Mod,
                    Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(6)
                    ))),
                )),
            }))
        );
    }

    #[test]
    fn struct_assignment_tests() {
        let statement = "a = { a: 0, b: 1, c: 2 }; b = { a: 0, b: \"hello\", c: 2.54 };";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let block = parser.parse().unwrap();

        assert_eq!(block.statements.len(), 2);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::StructAssign(StructAssign {
                path: vec!["a".to_string()],
                compound: vec![
                    (
                        "a".to_string(),
                        CompoundValue::Expression(Box::from(Expression::AtomicExpression(
                            AtomicExpression::Literal(LiteralValue::Int(0))
                        )))
                    ),
                    (
                        "b".to_string(),
                        CompoundValue::Expression(Box::from(Expression::AtomicExpression(
                            AtomicExpression::Literal(LiteralValue::Int(1))
                        )))
                    ),
                    (
                        "c".to_string(),
                        CompoundValue::Expression(Box::from(Expression::AtomicExpression(
                            AtomicExpression::Literal(LiteralValue::Int(2))
                        )))
                    ),
                ]
                .into_iter()
                .collect(),
            }))
        );
    }

    #[test]
    fn function_definition_tests() {
        let statement = "fn add(int a, B b) { return a + b; }";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let block = parser.parse().unwrap();

        assert_eq!(block.statements.len(), 1);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::FnDef(FnDef {
                name: "add".to_string(),
                args: vec![
                    (Vec::new(), Type::Int, "a".to_string(),),
                    (Vec::new(), Type::Struct("B".to_string()), "b".to_string(),),
                ],
                body: Block {
                    statements: vec![StatementBlock::Statement(Statement::Return(Box::from(
                        Expression::Binary(
                            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                                vec!["a".to_string()]
                            ))),
                            BinOp::Add,
                            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                                vec!["b".to_string()]
                            ))),
                        )
                    )))],
                },
                mods: Vec::new(),
            }))
        );
    }

    #[test]
    fn function_call_tests() {
        let statement = "add(1, 2, 3);";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let block = parser.parse().unwrap();

        assert_eq!(block.statements.len(), 1);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::Expression(Box::from(
                Expression::AtomicExpression(AtomicExpression::FnCall(Box::from(FnCall {
                    path: vec!["add".to_string()],
                    args: vec![
                        Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                            LiteralValue::Int(1)
                        ))),
                        Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                            LiteralValue::Int(2)
                        ))),
                        Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                            LiteralValue::Int(3)
                        ))),
                    ],
                })))
            )))
        );
    }

    #[test]
    fn if_simple_test() {
        let statement = "if (a == 0) { return 0; }";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let block = parser.parse().unwrap();

        assert_eq!(block.statements.len(), 1);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::If(If {
                cond: Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        vec!["a".to_string()]
                    ))),
                    BinOp::Eq,
                    Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(0)
                    ))),
                )),
                body: Box::from(Block {
                    statements: vec![StatementBlock::Statement(Statement::Return(Box::from(
                        Expression::AtomicExpression(AtomicExpression::Literal(LiteralValue::Int(
                            0
                        )))
                    )))],
                }),
                else_: None,
            }))
        );
    }

    #[test]
    fn if_statement_complex_test() {
        let statement =
            "if (a == 0) { return 0; } else if (a == 1) { return 1; } else { return 2; }";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let block = parser.parse().unwrap();

        assert_eq!(block.statements.len(), 1);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::If(If {
                cond: Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        vec!["a".to_string()]
                    ))),
                    BinOp::Eq,
                    Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(0)
                    ))),
                )),
                body: Box::from(Block {
                    statements: vec![StatementBlock::Statement(Statement::Return(Box::from(
                        Expression::AtomicExpression(AtomicExpression::Literal(LiteralValue::Int(
                            0
                        )))
                    )))],
                }),
                else_: Some(Box::from(If {
                    cond: Box::from(Expression::Binary(
                        Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                            vec!["a".to_string()]
                        ))),
                        BinOp::Eq,
                        Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                            LiteralValue::Int(1)
                        ))),
                    )),
                    body: Box::from(Block {
                        statements: vec![StatementBlock::Statement(Statement::Return(Box::from(
                            Expression::AtomicExpression(AtomicExpression::Literal(
                                LiteralValue::Int(1)
                            ))
                        )))],
                    }),
                    else_: Some(Box::from(If {
                        cond: Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                            LiteralValue::Bool(true)
                        ))),
                        body: Box::from(Block {
                            statements: vec![StatementBlock::Statement(Statement::Return(
                                Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                                    LiteralValue::Int(2)
                                )))
                            ))],
                        }),
                        else_: None,
                    })),
                })),
            }))
        );
    }

    #[test]
    fn while_test() {
        let statement = "while (a < 10) { a += 1; }";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let block = parser.parse().unwrap();

        assert_eq!(block.statements.len(), 1);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::While(While {
                cond: Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        vec!["a".to_string()]
                    ))),
                    BinOp::Lt,
                    Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(10)
                    ))),
                )),
                body: Box::from(Block {
                    statements: vec![StatementBlock::Statement(Statement::VarAssign(VarAssign {
                        path: vec!["a".to_string()],
                        expr: Box::from(Expression::Binary(
                            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                                vec!["a".to_string()]
                            ))),
                            BinOp::Add,
                            Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                                LiteralValue::Int(1)
                            ))),
                        )),
                    }))],
                }),
            }))
        );
    }

    #[test]
    fn for_test() {
        let statement = "for (int i = 0; i < 10; i += 1) { a += 1; }";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let block = parser.parse().unwrap();

        assert_eq!(block.statements.len(), 1);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::For(For {
                init: Some(Box::from(Statement::VarDecl(VarDecl {
                    type_: Type::Int,
                    name: "i".to_string(),
                    mods: Vec::new(),
                    expr: Some(Box::from(Expression::AtomicExpression(
                        AtomicExpression::Literal(LiteralValue::Int(0))
                    ))),
                }))),
                cond: Some(Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        vec!["i".to_string()]
                    ))),
                    BinOp::Lt,
                    Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(10)
                    ))),
                ))),
                step: Some(Box::from(Statement::VarAssign(VarAssign {
                    path: vec!["i".to_string()],
                    expr: Box::from(Expression::Binary(
                        Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                            vec!["i".to_string()]
                        ))),
                        BinOp::Add,
                        Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                            LiteralValue::Int(1)
                        ))),
                    )),
                }))),
                body: Block {
                    statements: vec![StatementBlock::Statement(Statement::VarAssign(VarAssign {
                        path: vec!["a".to_string()],
                        expr: Box::from(Expression::Binary(
                            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                                vec!["a".to_string()]
                            ))),
                            BinOp::Add,
                            Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                                LiteralValue::Int(1)
                            ))),
                        )),
                    }))],
                },
            }))
        );
    }

    #[test]
    fn loop_break_continue_test() {
        let statement = "while (true) { break; continue; }";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let block = parser.parse().unwrap();

        assert_eq!(block.statements.len(), 1);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::While(While {
                cond: Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                    LiteralValue::Bool(true)
                ))),
                body: Box::from(Block {
                    statements: vec![
                        StatementBlock::Statement(Statement::Break),
                        StatementBlock::Statement(Statement::Continue),
                    ],
                }),
            }))
        );
    }

    #[test]
    fn simple_expression_order_test() {
        let statement = "a + b + c + d - e - f";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let expr = parser.parse_expression().unwrap();

        assert_eq!(
            expr,
            Box::from(Expression::Binary(
                Box::from(Expression::Binary(
                    Box::from(Expression::Binary(
                        Box::from(Expression::Binary(
                            Box::from(Expression::Binary(
                                Box::from(Expression::AtomicExpression(
                                    AtomicExpression::Variable(vec!["a".to_string()],)
                                )),
                                BinOp::Add,
                                Box::from(Expression::AtomicExpression(
                                    AtomicExpression::Variable(vec!["b".to_string()],)
                                ))
                            )),
                            BinOp::Add,
                            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                                vec!["c".to_string()],
                            )))
                        )),
                        BinOp::Add,
                        Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                            vec!["d".to_string()],
                        )))
                    )),
                    BinOp::Sub,
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        vec!["e".to_string()],
                    )))
                )),
                BinOp::Sub,
                Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                    vec!["f".to_string()],
                ))),
            ))
        );
    }

    #[test]
    fn expression_order_test() {
        let statement = "a + b * c - d / e % f";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let expr = parser.parse_expression().unwrap();

        // should result in tree
        //          -
        //      /       \
        //     +         %
        //    / \       / \
        //   a   *     /   f
        //      / \   / \
        //     b  c  d   e

        // (a + (b * c)) - ((d / e) % f)

        // (a + (b * c))

        let b_times_c = Box::from(Expression::Binary(
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                vec!["b".to_string()],
            ))),
            BinOp::Mul,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                vec!["c".to_string()],
            ))),
        ));

        let a_plus = Box::from(Expression::Binary(
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                vec!["a".to_string()],
            ))),
            BinOp::Add,
            b_times_c,
        ));

        let d_div_e = Box::from(Expression::Binary(
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                vec!["d".to_string()],
            ))),
            BinOp::Div,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                vec!["e".to_string()],
            ))),
        ));

        let mod_f = Box::from(Expression::Binary(
            d_div_e,
            BinOp::Mod,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                vec!["f".to_string()],
            ))),
        ));

        assert_eq!(
            expr,
            Box::from(Expression::Binary(a_plus, BinOp::Sub, mod_f))
        );
    }
}
