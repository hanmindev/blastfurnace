use crate::front::ast_retriever::reader::lexical::lexer::{TokenError, TokenInfo};
use crate::front::ast_retriever::reader::lexical::token_types::Token;
use crate::front::ast_retriever::reader::lexical::token_types::Token::Any;
use crate::front::ast_types::{
    AtomicExpression, BinOp, Block, Compound, CompoundValue, Definition, Else, Expression,
    ExpressionEnum, FnCall, FnDef, FnMod, For, If, LiteralValue, Module, ModuleImport, NamePath,
    Reference, Statement, StructDef, Type, UnOp, Use, UseElement, VarAssign, VarDecl, VarDef,
    VarMod, While,
};
use std::collections::{HashMap, VecDeque};
use std::mem;
use std::rc::Rc;

#[derive(Debug)]
pub enum ParseError {
    Unknown,
    Unexpected(TokenInfo, String),
}

pub type ParseResult<T> = Result<T, ParseError>;

pub trait TokenStream {
    fn next(&mut self) -> Result<TokenInfo, TokenError>;
}

pub struct Parser<T: TokenStream> {
    lexer: T,
    token_index: u64,
    curr_token: Token,
    future_tokens: VecDeque<TokenInfo>,
}

impl<T: TokenStream> Parser<T> {
    pub fn new(lexer: T) -> Parser<T> {
        let mut parser = Parser {
            lexer,
            token_index: 0,
            curr_token: Token::Eof,
            future_tokens: VecDeque::new(),
        };
        parser.eat(&Token::Eof).unwrap();
        parser
    }

    fn curr_token_info(&self) -> TokenInfo {
        (self.curr_token.clone(), self.token_index)
    }

    fn next(&mut self) -> TokenInfo {
        match self.future_tokens.pop_front() {
            None => self.lexer.next().unwrap(),
            Some(front) => front,
        }
    }

    fn eat(&mut self, type_: &Token) -> ParseResult<TokenInfo> {
        // return old lexical, set new lexical, set one buffer of next lexical
        if mem::discriminant(&self.curr_token) == mem::discriminant(type_) || matches!(type_, Any) {
            let old_curr = self.curr_token.clone();
            let old_index = self.token_index;
            (self.curr_token, self.token_index) = self.next();

            Ok((old_curr, old_index))
        } else {
            Err(ParseError::Unexpected(
                (self.curr_token.clone(), self.token_index),
                format!("Tried to eat {:?}, ate {:?}", type_, self.curr_token),
            ))
        }
    }

    fn peek(&mut self, count: i32) -> &Token {
        if count == 0 {
            return &self.curr_token;
        }

        while self.future_tokens.len() < count as usize {
            let next = self.next();
            self.future_tokens.push_back(next);
        }

        &self.future_tokens[count as usize - 1].0
    }

    pub fn string_to_namepath(s: &str) -> NamePath {
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
        let name = Reference::new(path.remove(0));

        NamePath { name, path }
    }

    fn parse_atomic_expression(&mut self) -> ParseResult<Expression> {
        if matches!(self.curr_token, Token::LBrace) {
            let compound = self.parse_compound()?;
            return Ok(Expression {
                type_: None,
                expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                    LiteralValue::Compound(compound),
                )),
            });
        }

        match self.eat(&Any)? {
            (Token::Null, _) => Ok(Expression {
                type_: None,
                expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                    LiteralValue::Null,
                )),
            }),
            (Token::Bool(b), _) => Ok(Expression {
                type_: None,
                expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                    LiteralValue::Int(b as i32),
                )),
            }),
            (Token::Int(i), _) => Ok(Expression {
                type_: None,
                expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                    LiteralValue::Int(i),
                )),
            }),
            (Token::Float(f), _) => Ok(Expression {
                type_: None,
                expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                    LiteralValue::Float(f),
                )),
            }),
            (Token::Double(f), _) => Ok(Expression {
                type_: None,
                expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                    LiteralValue::Double(f),
                )),
            }),
            (Token::String(s), _) => Ok(Expression {
                type_: None,
                expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                    LiteralValue::String(s),
                )),
            }),
            (Token::Ident(s), _) => {
                if matches!(self.curr_token, Token::LParen) {
                    let mut fn_call = Box::from(FnCall {
                        name: Reference::new(s),
                        args: Vec::new(),
                    });
                    self.eat(&Token::LParen)?;

                    if self.eat(&Token::RParen).is_err() {
                        loop {
                            fn_call.args.push(*self.parse_expression()?);
                            match self.eat(&Token::Comma) {
                                Ok(_) => {}
                                Err(_) => {
                                    break;
                                }
                            }
                        }
                        self.eat(&Token::RParen)?; // eat RParen
                    }

                    // function call
                    Ok(Expression {
                        expr: ExpressionEnum::AtomicExpression(AtomicExpression::FnCall(fn_call)),
                        type_: None,
                    })
                } else {
                    let var = Expression {
                        expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                            Parser::<T>::string_to_namepath(&s),
                        )),
                        type_: None,
                    };
                    match self.curr_token {
                        Token::PlusPlus => {
                            self.eat(&Token::PlusPlus)?;
                            Ok(Expression {
                                expr: ExpressionEnum::Unary(UnOp::PostInc, Box::from(var)),
                                type_: None,
                            })
                        }
                        Token::MinusMinus => {
                            self.eat(&Token::MinusMinus)?;
                            Ok(Expression {
                                expr: ExpressionEnum::Unary(UnOp::PostDec, Box::from(var)),
                                type_: None,
                            })
                        }
                        _ => Ok(var),
                    }
                }
            }
            tok => Err(ParseError::Unexpected(
                tok,
                "Expected type for atomic expression parsing".to_string(),
            )),
        }
    }

    fn get_bin_op_prec(&self, binop: &BinOp) -> i32 {
        // taken from https://en.cppreference.com/w/c/language/operator_precedence

        160 - (match binop {
            BinOp::Mul => 3,
            BinOp::Div => 3,
            BinOp::Mod => 3,

            BinOp::Add => 4,
            BinOp::Sub => 4,

            BinOp::Lt => 6,
            BinOp::Gt => 6,
            BinOp::Leq => 6,
            BinOp::Geq => 6,

            BinOp::Eq => 7,
            BinOp::Neq => 7,

            BinOp::And => 11,
            BinOp::Or => 12,
        } * 10)
    }

    fn token_to_binop(&self, tok: &Token) -> Option<BinOp> {
        Some(match tok {
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
            Token::And => BinOp::And,
            Token::Or => BinOp::Or,
            _ => {
                return None;
            }
        })
    }

    fn parse_bin_op_rhs(
        &mut self,
        expr_prec: i32,
        mut lhs: Box<Expression>,
    ) -> ParseResult<Box<Expression>> {
        // start on op before rhs
        loop {
            let binop = match self.token_to_binop(&self.curr_token) {
                Some(op) => op,
                None => {
                    return Ok(lhs);
                }
            };

            let tok_prec = self.get_bin_op_prec(&binop);

            if tok_prec < expr_prec {
                return Ok(lhs);
            }

            self.eat(&Any)?; // we know this is a binop

            let mut rhs = self.parse_expression_single()?;

            let next_prec = match self.token_to_binop(&self.curr_token) {
                Some(op) => self.get_bin_op_prec(&op),
                None => {
                    return Ok(Box::from(Expression {
                        expr: ExpressionEnum::Binary(lhs, binop, rhs),
                        type_: None,
                    }));
                }
            };

            if tok_prec < next_prec {
                rhs = self.parse_bin_op_rhs(tok_prec + 1, rhs)?;
            }

            lhs = Box::from(Expression {
                expr: ExpressionEnum::Binary(lhs, binop, rhs),
                type_: None,
            });
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
                        self.eat(&Token::Plus)?;
                        let right = self.parse_expression()?;
                        return Ok(right);
                    }
                    Token::Minus => UnOp::Neg,
                    Token::Exclamation => UnOp::Not,
                    Token::Star => UnOp::Deref,
                    Token::Ampersand => UnOp::Ref,
                    Token::PlusPlus => UnOp::PreInc,
                    Token::MinusMinus => UnOp::PreDec,

                    _ => {
                        // not prefixed unary
                        let expr = Box::from(self.parse_atomic_expression()?);
                        return Ok(expr);
                    }
                };

                // eat unop
                self.eat(&Any)?;

                let right = self.parse_expression_single()?;
                Ok(Box::from(Expression {
                    expr: ExpressionEnum::Unary(unop, right),
                    type_: None,
                }))
            }
        }
    }

    fn parse_expression(&mut self) -> ParseResult<Box<Expression>> {
        // not prefixed unary
        let lhs = self.parse_expression_single()?;
        self.parse_bin_op_rhs(0, lhs)
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
                else_ = Some(Else::If(Box::from(self.parse_if_statement()?)));
            } else {
                else_ = Some(Else::Block(Box::from(self.parse_block()?)));
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
            (Token::Ident(var_name), _) => {
                let name_path = Parser::<T>::string_to_namepath(var_name);

                let assign_op = match self.eat(&Any)? {
                    (Token::Assign, _) => {
                        return Ok(Statement::VarAssign(VarAssign {
                            name_path,
                            expr: self.parse_expression()?,
                        }));
                    }
                    (Token::PlusAssign, _) => BinOp::Add,
                    (Token::MinusAssign, _) => BinOp::Sub,
                    (Token::StarAssign, _) => BinOp::Mul,
                    (Token::SlashAssign, _) => BinOp::Div,
                    (Token::PercentAssign, _) => BinOp::Mod,
                    tok => {
                        return Err(ParseError::Unexpected(
                            tok.clone(),
                            "Expected binary operator".to_string(),
                        ))?;
                    }
                };

                let rhs = self.parse_expression()?;

                let expr = Expression {
                    expr: ExpressionEnum::Binary(
                        Box::from(Expression {
                            expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                                name_path.clone(),
                            )),
                            type_: None,
                        }),
                        assign_op,
                        rhs,
                    ),
                    type_: None,
                };

                Ok(Statement::VarAssign(VarAssign {
                    name_path,
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
            Token::Const | Token::Let => {
                // variable declaration
                Ok(Statement::VarDecl(self.parse_var_decl()?))
            }

            Token::Ident(_) => {
                match self.peek(1) {
                    Token::Assign
                    | Token::PlusAssign
                    | Token::MinusAssign
                    | Token::StarAssign
                    | Token::SlashAssign
                    | Token::PercentAssign => {
                        // variable / struct assignment
                        self.parse_assignment()
                    }
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
            _ => Ok(Statement::Expression(self.parse_expression()?)),
        }
    }

    fn peek_def_type(&mut self) -> Option<Token> {
        match self.curr_token {
            Token::Fn | Token::Rec | Token::Inline => Some(Token::Fn),
            Token::StructType => Some(Token::StructType),
            Token::Const | Token::Let => Some(Token::Const),

            Token::Pub => match self.peek(1) {
                Token::Fn | Token::Rec | Token::Inline => Some(Token::Fn),
                Token::StructType => Some(Token::StructType),
                Token::Const | Token::Let => Some(Token::Let),
                _ => None,
            },

            _ => None,
        }
    }

    fn parse_definition(&mut self) -> ParseResult<Definition> {
        if let Some(type_) = self.peek_def_type() {
            match type_ {
                Token::Fn => return Ok(Definition::FnDef(self.parse_fn_def()?)),
                Token::StructType => return Ok(Definition::StructDef(self.parse_struct_def()?)),
                Token::Let => return Ok(Definition::VarDecl(self.parse_var_decl()?)),
                _ => {}
            }
        }

        Err(ParseError::Unexpected(
            self.curr_token_info(),
            "Expected definition".to_string(),
        ))
    }

    fn parse_module_no_brace(&mut self, global: bool) -> ParseResult<Module> {
        let mut mods = Vec::new();
        let mut uses = Vec::new();

        let mut definitions = Vec::new();
        let mut struct_var_definitions = Vec::new();
        let mut fn_definitions = Vec::new();

        let mut pub_struct_var_definitions = Vec::new();
        let mut pub_fn_definitions = Vec::new();

        let mut statements = Vec::new();
        loop {
            let _ = self.eat(&Token::Semicolon).is_err();

            if matches!(self.curr_token, Token::RBrace | Token::Eof) {
                break;
            }

            if !global && self.curr_token == Token::Pub {
                panic!("pub in local scope")
            }

            let pub_ = self.curr_token == Token::Pub;

            if let Some(type_) = self.peek_def_type() {
                match type_ {
                    Token::Fn => {
                        if pub_ {
                            pub_fn_definitions.push(Definition::FnDef(self.parse_fn_def()?));
                        } else {
                            fn_definitions.push(Definition::FnDef(self.parse_fn_def()?));
                        }
                        continue;
                    }
                    Token::StructType => {
                        if pub_ {
                            pub_struct_var_definitions
                                .push(Definition::StructDef(self.parse_struct_def()?));
                        } else {
                            struct_var_definitions
                                .push(Definition::StructDef(self.parse_struct_def()?));
                        }
                        continue;
                    }
                    Token::Let => {
                        if global {
                            if pub_ {
                                pub_struct_var_definitions
                                    .push(Definition::VarDecl(self.parse_var_decl()?));
                            } else {
                                struct_var_definitions
                                    .push(Definition::VarDecl(self.parse_var_decl()?));
                            }
                        } else {
                            statements.push(Statement::VarDecl(self.parse_var_decl()?));
                        }
                        continue;
                    }
                    _ => {}
                }
            }

            if pub_ && matches!(self.peek(1), Token::Mod) {
                mods.push(self.parse_module_import()?);
                continue;
            }

            match self.curr_token {
                Token::LBrace => statements.push(Statement::Block(self.parse_block()?)),
                Token::Mod => mods.push(self.parse_module_import()?),
                Token::Use => uses.push(self.parse_use_import()?),
                _ => {
                    if global {
                        Err(ParseError::Unexpected(
                            self.curr_token_info(),
                            "Cannot be used in global scope".to_string(),
                        ))?
                    }
                    let statement = self.parse_statement()?;
                    match statement {
                        Statement::If(_) | Statement::For(_) | Statement::While(_) => {}
                        _ => {
                            self.eat(&Token::Semicolon)?;
                        }
                    }
                    statements.push(statement);
                }
            }
        }

        definitions.append(&mut struct_var_definitions);
        definitions.append(&mut fn_definitions);

        pub_struct_var_definitions.append(&mut pub_fn_definitions);

        Ok(Module {
            uses,
            mods,
            public_definitions: pub_struct_var_definitions,
            block: Block {
                definitions,
                statements,
            },
        })
    }

    fn parse_block(&mut self) -> ParseResult<Block> {
        self.eat(&Token::LBrace)?;
        let module = self.parse_module_no_brace(false)?;
        self.eat(&Token::RBrace)?;

        if !module.public_definitions.is_empty() {
            panic!("public definitions in block")
        }

        Ok(module.block)
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

    fn parse_var_decl(&mut self) -> ParseResult<VarDecl> {
        let mut mods: Vec<VarMod> = Vec::new();
        let _ = self.eat(&Token::Pub);

        if self.eat(&Token::Const).is_ok() {
            mods.push(VarMod::Const);
        } else {
            self.eat(&Token::Let)?;
        }

        let var_name = match self.eat(&Any)? {
            (Token::Ident(s), _) => s,
            tok => Err(ParseError::Unexpected(
                tok,
                "Expected identifier for variable name".to_string(),
            ))?,
        };

        let type_ = if self.eat(&Token::Colon).is_ok() {
            Some(match self.eat(&Any)? {
                (Token::VoidType, _) => Type::Void,
                (Token::IntType, _) => Type::Int,
                (Token::FloatType, _) => Type::Float,
                (Token::DoubleType, _) => Type::Double,
                (Token::BoolType, _) => Type::Int,
                (Token::StringType, _) => Type::String,
                (Token::Ident(s), _) => Type::Struct(Reference::new(s)),
                tok => Err(ParseError::Unexpected(
                    tok,
                    "Expected variable type annotation for variable declaration".to_string(),
                ))?,
            })
        } else {
            None
        };

        let expr = if self.eat(&Token::Assign).is_ok() {
            Some(self.parse_expression()?)
        } else {
            None
        };

        Ok(VarDecl {
            var_def: VarDef {
                type_,
                name: Reference::new(var_name),
                mods: Rc::new(mods),
            },
            expr,
        })
    }

    fn parse_fn_def(&mut self) -> ParseResult<FnDef> {
        let mut mods = Vec::new();
        let _ = self.eat(&Token::Pub);

        let mut rec = false;

        if self.eat(&Token::Rec).is_ok() {
            mods.push(FnMod::Rec);
            rec = true;
        }

        let tok_r = self.eat(&Token::Inline);
        if let Ok(tok) = tok_r {
            mods.push(FnMod::Inline);
            if rec {
                Err(ParseError::Unexpected(
                    tok,
                    "Inline functions cannot be recursive".to_string(),
                ))?;
            }
        }

        self.eat(&Token::Fn)?;
        let name = match self.eat(&Any)? {
            (Token::Ident(s), _) => s,
            tok => Err(ParseError::Unexpected(
                tok,
                "Expected identifier for function name".to_string(),
            ))?,
        };

        self.eat(&Token::LParen)?;
        let mut args: Vec<VarDef> = Vec::new();

        if self.eat(&Token::RParen).is_err() {
            loop {
                let mut mods = Vec::new();
                if self.eat(&Token::Const).is_ok() {
                    mods.push(VarMod::Const);
                }

                let name = Reference::new(match self.eat(&Any)? {
                    (Token::Ident(s), _) => s,
                    tok => {
                        return Err(ParseError::Unexpected(
                            tok,
                            "Expected identifier for function argument name".to_string(),
                        ));
                    }
                });

                let type_ = if self.eat(&Token::Colon).is_ok() {
                    Some(match self.eat(&Any)? {
                        (Token::VoidType, _) => Type::Void,
                        (Token::IntType, _) => Type::Int,
                        (Token::FloatType, _) => Type::Float,
                        (Token::DoubleType, _) => Type::Double,
                        (Token::BoolType, _) => Type::Int,
                        (Token::StringType, _) => Type::String,
                        (Token::Ident(s), _) => Type::Struct(Reference::new(s)),
                        tok => Err(ParseError::Unexpected(
                            tok,
                            "Expected variable type annotation for variable declaration"
                                .to_string(),
                        ))?,
                    })
                } else {
                    None
                };

                args.push(VarDef {
                    mods: Rc::new(mods),
                    type_,
                    name,
                });

                if self.eat(&Token::Comma).is_err() {
                    break;
                }
            }

            self.eat(&Token::RParen)?;
        }

        let return_type = if self.eat(&Token::Arrow).is_ok() {
            match self.eat(&Any)? {
                (Token::VoidType, _) => Type::Void,
                (Token::IntType, _) => Type::Int,
                (Token::FloatType, _) => Type::Float,
                (Token::DoubleType, _) => Type::Double,
                (Token::BoolType, _) => Type::Int,
                (Token::StringType, _) => Type::String,
                (Token::Ident(s), _) => Type::Struct(Reference::new(s)),
                tok => {
                    return Err(ParseError::Unexpected(
                        tok,
                        "Expected type for function return type".to_string(),
                    ));
                }
            }
        } else {
            Type::Void
        };

        let body = self.parse_block()?;

        Ok(FnDef {
            return_type,
            name: Reference::new(name),
            args,
            body,
            mods: Rc::new(mods),
        })
    }

    fn parse_struct_def(&mut self) -> ParseResult<StructDef> {
        let mods = Vec::new();
        let _ = self.eat(&Token::Pub);

        self.eat(&Token::StructType)?;

        let struct_name = match self.eat(&Any)? {
            (Token::Ident(s), _) => s,
            tok => {
                return Err(ParseError::Unexpected(
                    tok,
                    "Expected identifier for struct name".to_string(),
                ));
            }
        };

        let mut map = HashMap::new();

        self.eat(&Token::LBrace)?;

        while !matches!(self.curr_token, Token::RBrace) {
            let name = match self.eat(&Any)? {
                (Token::Ident(s), _) => s,
                tok => {
                    return Err(ParseError::Unexpected(
                        tok,
                        "Expected identifier for struct field name".to_string(),
                    ));
                }
            };

            self.eat(&Token::Colon)?; // required for now because we don't have type inference

            let type_ = match self.eat(&Any)? {
                (Token::VoidType, _) => Type::Void,
                (Token::IntType, _) => Type::Int,
                (Token::FloatType, _) => Type::Float,
                (Token::DoubleType, _) => Type::Double,
                (Token::BoolType, _) => Type::Int,
                (Token::StringType, _) => Type::String,
                (Token::Ident(s), _) => Type::Struct(Reference::new(s)),
                tok => {
                    return Err(ParseError::Unexpected(
                        tok,
                        "Expected type for struct field".to_string(),
                    ));
                }
            };

            map.insert(name, type_);

            if self.eat(&Token::Comma).is_err() {
                break;
            }
        }

        self.eat(&Token::RBrace)?;

        Ok(StructDef {
            mods: Rc::new(mods),
            type_name: Reference::new(struct_name),
            map,
        })
    }

    pub fn parse_module(&mut self) -> ParseResult<Module> {
        let module = self.parse_module_no_brace(true)?;
        self.eat(&Token::Eof)?;

        Ok(module)
    }
    fn parse_module_import(&mut self) -> ParseResult<ModuleImport> {
        let public = self.eat(&Token::Pub).is_ok();
        self.eat(&Token::Mod)?;

        let tok = self.eat(&Any)?;

        if let (Token::Ident(s), _) = tok {
            self.eat(&Token::Semicolon)?;
            Ok(ModuleImport { public, name: s })
        } else {
            Err(ParseError::Unexpected(
                tok,
                "Expected identifier for module name".to_string(),
            ))
        }
    }
    fn parse_use_import(&mut self) -> ParseResult<Use> {
        self.eat(&Token::Use)?;

        let mut use_ = Use {
            path: Vec::new(),
            elements: Vec::new(),
        };

        loop {
            match self.eat(&Any)? {
                (Token::Ident(s), _) => match self.eat(&Any)? {
                    (Token::Colon, _) => {
                        self.eat(&Token::Colon)?;
                        use_.path.push(s);
                    }
                    (Token::Semicolon, _) | (Token::As, _) => {
                        use_.elements.push(UseElement {
                            origin_name: s.clone(),
                            imported_name: Reference::new({
                                if self.eat(&Token::As).is_ok() {
                                    let tok = self.eat(&Any)?;
                                    if let Token::Ident(s) = tok.0 {
                                        s
                                    } else {
                                        return Err(ParseError::Unexpected(
                                            tok,
                                            "Expected identifier for imported name alias"
                                                .to_string(),
                                        ));
                                    }
                                } else {
                                    s
                                }
                            }),
                        });

                        return Ok(use_);
                    }
                    tok => {
                        return Err(ParseError::Unexpected(
                            tok.clone(),
                            "Expected identifier for use path".to_string(),
                        ));
                    }
                },
                (Token::LBrace, _) => {
                    loop {
                        match self.eat(&Any)? {
                            (Token::RBrace, _) => {
                                self.eat(&Token::Semicolon)?;
                                break;
                            }
                            (Token::Ident(s), _) => {
                                use_.elements.push(UseElement {
                                    origin_name: s.clone(),
                                    imported_name: Reference::new({
                                        if self.eat(&Token::As).is_ok() {
                                            let tok = self.eat(&Any)?;
                                            if let Token::Ident(s) = tok.0 {
                                                s
                                            } else {
                                                return Err(ParseError::Unexpected(
                                                    tok,
                                                    "Expected identifier for imported name alias"
                                                        .to_string(),
                                                ));
                                            }
                                        } else {
                                            s
                                        }
                                    }),
                                });
                                if self.eat(&Token::Comma).is_err() {
                                    break;
                                }
                            }
                            tok => {
                                return Err(ParseError::Unexpected(
                                    tok.clone(),
                                    "Expected identifier for use path".to_string(),
                                ));
                            }
                        }
                    }
                    self.eat(&Token::RBrace)?;
                    self.eat(&Token::Semicolon)?;
                    break;
                }
                tok => {
                    return Err(ParseError::Unexpected(
                        tok.clone(),
                        "Expected identifier for use path".to_string(),
                    ));
                }
            }
        }
        Ok(use_)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::front::ast_retriever::reader::lexical::lexer::Lexer;
    use crate::front::file_system::byte_stream::{ByteStream, StringReader};

    #[test]
    fn simple_test() {
        let statement = "fn main() { return 0; }";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let block = parser.parse_module_no_brace(false).unwrap().block;

        assert_eq!(block.definitions.len(), 1);
        assert_eq!(
            block.definitions[0],
            Definition::FnDef(FnDef {
                return_type: Type::Void,
                name: Reference::new("main".to_string()),
                args: Vec::new(),
                body: Block {
                    definitions: Vec::new(),
                    statements: vec![Statement::Return(Box::from(Expression {
                        expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                            LiteralValue::Int(0)
                        )),
                        type_: None,
                    }))],
                },
                mods: Rc::new(Vec::new()),
            })
        );
    }

    #[test]
    fn variable_declarations_test() {
        let statement = "const a: int = 0; const b: int = 1; let c: int = 2; let d: float = 3.0; let e: double = 4.0d; let f: bool = true; let g: string = \"hello\";";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let block = parser.parse_module_no_brace(false).unwrap().block;

        assert_eq!(block.statements.len(), 7);
        assert_eq!(
            block.statements[0],
            (Statement::VarDecl(VarDecl {
                var_def: VarDef {
                    type_: Some(Type::Int),
                    name: Reference::new("a".to_string()),
                    mods: Rc::new(vec![VarMod::Const]),
                },
                expr: Some(Box::from(Expression {
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(0)
                    )),
                    type_: None,
                })),
            }))
        );
        assert_eq!(
            block.statements[1],
            (Statement::VarDecl(VarDecl {
                var_def: VarDef {
                    type_: Some(Type::Int),
                    name: Reference::new("b".to_string()),
                    mods: Rc::new(vec![VarMod::Const]),
                },
                expr: Some(Box::from(Expression {
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(1)
                    )),
                    type_: None,
                })),
            }))
        );
        assert_eq!(
            block.statements[2],
            (Statement::VarDecl(VarDecl {
                var_def: VarDef {
                    type_: Some(Type::Int),
                    name: Reference::new("c".to_string()),
                    mods: Rc::new(Vec::new()),
                },
                expr: Some(Box::from(Expression {
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(2)
                    )),
                    type_: None,
                })),
            }))
        );
        assert_eq!(
            block.statements[3],
            (Statement::VarDecl(VarDecl {
                var_def: VarDef {
                    type_: Some(Type::Float),
                    name: Reference::new("d".to_string()),
                    mods: Rc::new(Vec::new()),
                },
                expr: Some(Box::from(Expression {
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Float(3.0)
                    )),
                    type_: None,
                })),
            }))
        );
        assert_eq!(
            block.statements[4],
            (Statement::VarDecl(VarDecl {
                var_def: VarDef {
                    type_: Some(Type::Double),
                    name: Reference::new("e".to_string()),
                    mods: Rc::new(Vec::new()),
                },
                expr: Some(Box::from(Expression {
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Double(4.0)
                    )),
                    type_: None,
                })),
            }))
        );
        assert_eq!(
            block.statements[5],
            (Statement::VarDecl(VarDecl {
                var_def: VarDef {
                    type_: Some(Type::Int),
                    name: Reference::new("f".to_string()),
                    mods: Rc::new(Vec::new()),
                },
                expr: Some(Box::from(Expression {
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(1)
                    )),
                    type_: None,
                })),
            }))
        );
        assert_eq!(
            block.statements[6],
            (Statement::VarDecl(VarDecl {
                var_def: VarDef {
                    type_: Some(Type::String),
                    name: Reference::new("g".to_string()),
                    mods: Rc::new(Vec::new()),
                },
                expr: Some(Box::from(Expression {
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::String("hello".to_string())
                    )),
                    type_: None,
                })),
            }))
        );
    }

    #[test]
    fn struct_declarations_test() {
        let statement =
            "let a: A = { a: 0, b: 1, c: 2 }; let b: B = { a: 0, b: \"hello\", c: 2.54 };";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let block = parser.parse_module_no_brace(false).unwrap().block;

        let compound = {
            let mut map = HashMap::new();
            map.insert(
                "a".to_string(),
                CompoundValue::Expression(Box::from(Expression {
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(0),
                    )),
                    type_: None,
                })),
            );
            map.insert(
                "b".to_string(),
                CompoundValue::Expression(Box::from(Expression {
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(1),
                    )),
                    type_: None,
                })),
            );
            map.insert(
                "c".to_string(),
                CompoundValue::Expression(Box::from(Expression {
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(2),
                    )),
                    type_: None,
                })),
            );
            map
        };

        assert_eq!(block.statements.len(), 2);
        assert_eq!(
            block.statements[0],
            (Statement::VarDecl(VarDecl {
                var_def: VarDef {
                    type_: Some(Type::Struct(Reference::new("A".to_string()))),
                    name: Reference::new("a".to_string()),
                    mods: Rc::new(Vec::new()),
                },
                expr: Some(Box::from(Expression {
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Compound(compound)
                    )),
                    type_: None,
                })),
            }))
        );
    }

    #[test]
    fn variable_assignment_tests() {
        let statement = "a = 0; b = 2.4; a += 2; a -= 3; a *= 4; a /= 5; a %= 6;";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let block = parser.parse_module_no_brace(false).unwrap().block;

        assert_eq!(block.statements.len(), 7);
        assert_eq!(
            block.statements[0],
            (Statement::VarAssign(VarAssign {
                name_path: Parser::<Lexer>::string_to_namepath("a"),
                expr: Box::from(Expression {
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(0)
                    )),
                    type_: None,
                }),
            }))
        );
        assert_eq!(
            block.statements[1],
            (Statement::VarAssign(VarAssign {
                name_path: Parser::<Lexer>::string_to_namepath("b"),
                expr: Box::from(Expression {
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Float(2.4)
                    )),
                    type_: None,
                }),
            }))
        );
        assert_eq!(
            block.statements[2],
            (Statement::VarAssign(VarAssign {
                name_path: Parser::<Lexer>::string_to_namepath("a"),
                expr: Box::from(Expression {
                    expr: ExpressionEnum::Binary(
                        Box::from(Expression {
                            expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                                Parser::<Lexer>::string_to_namepath("a")
                            )),
                            type_: None,
                        }),
                        BinOp::Add,
                        Box::from(Expression {
                            expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                                LiteralValue::Int(2)
                            )),
                            type_: None,
                        }),
                    ),
                    type_: None,
                }),
            }))
        );
        assert_eq!(
            block.statements[3],
            (Statement::VarAssign(VarAssign {
                name_path: Parser::<Lexer>::string_to_namepath("a"),
                expr: Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::Binary(
                        Box::from(Expression {
                            type_: None,
                            expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                                Parser::<Lexer>::string_to_namepath("a")
                            )),
                        }),
                        BinOp::Sub,
                        Box::from(Expression {
                            type_: None,
                            expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                                LiteralValue::Int(3)
                            )),
                        }),
                    ),
                }),
            }))
        );
        assert_eq!(
            block.statements[4],
            (Statement::VarAssign(VarAssign {
                name_path: Parser::<Lexer>::string_to_namepath("a"),
                expr: Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::Binary(
                        Box::from(Expression {
                            type_: None,
                            expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                                Parser::<Lexer>::string_to_namepath("a")
                            )),
                        }),
                        BinOp::Mul,
                        Box::from(Expression {
                            type_: None,
                            expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                                LiteralValue::Int(4)
                            )),
                        }),
                    ),
                }),
            }))
        );
        assert_eq!(
            block.statements[5],
            (Statement::VarAssign(VarAssign {
                name_path: Parser::<Lexer>::string_to_namepath("a"),
                expr: Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::Binary(
                        Box::from(Expression {
                            type_: None,
                            expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                                Parser::<Lexer>::string_to_namepath("a")
                            )),
                        }),
                        BinOp::Div,
                        Box::from(Expression {
                            type_: None,
                            expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                                LiteralValue::Int(5)
                            )),
                        }),
                    ),
                }),
            }))
        );
        assert_eq!(
            block.statements[6],
            (Statement::VarAssign(VarAssign {
                name_path: Parser::<Lexer>::string_to_namepath("a"),
                expr: Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::Binary(
                        Box::from(Expression {
                            type_: None,
                            expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                                Parser::<Lexer>::string_to_namepath("a")
                            )),
                        }),
                        BinOp::Mod,
                        Box::from(Expression {
                            type_: None,
                            expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                                LiteralValue::Int(6)
                            )),
                        }),
                    ),
                }),
            }))
        );
    }

    #[test]
    fn struct_assignment_tests() {
        let statement = "a = { a: 0, b: 1, c: 2 }; b = { a: 0, b: \"hello\", c: 2.54 };";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let block = parser.parse_module_no_brace(false).unwrap().block;

        assert_eq!(block.statements.len(), 2);
        assert_eq!(
            block.statements[0],
            (Statement::VarAssign(VarAssign {
                name_path: Parser::<Lexer>::string_to_namepath("a"),
                expr: Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Compound({
                            let mut map = HashMap::new();
                            map.insert(
                                "a".to_string(),
                                CompoundValue::Expression(Box::from(Expression {
                                    type_: None,
                                    expr: ExpressionEnum::AtomicExpression(
                                        AtomicExpression::Literal(LiteralValue::Int(0)),
                                    ),
                                })),
                            );
                            map.insert(
                                "b".to_string(),
                                CompoundValue::Expression(Box::from(Expression {
                                    type_: None,
                                    expr: ExpressionEnum::AtomicExpression(
                                        AtomicExpression::Literal(LiteralValue::Int(1)),
                                    ),
                                })),
                            );
                            map.insert(
                                "c".to_string(),
                                CompoundValue::Expression(Box::from(Expression {
                                    type_: None,
                                    expr: ExpressionEnum::AtomicExpression(
                                        AtomicExpression::Literal(LiteralValue::Int(2)),
                                    ),
                                })),
                            );

                            map
                        })
                    )),
                }),
            }))
        );
    }

    #[test]
    fn function_definition_tests() {
        let statement = "fn add(a: int, b: B) -> int { return a + b; }";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let block = parser.parse_module_no_brace(false).unwrap().block;

        assert_eq!(block.definitions.len(), 1);
        assert_eq!(
            block.definitions[0],
            (Definition::FnDef(FnDef {
                return_type: Type::Int,
                name: Reference::new("add".to_string()),
                args: vec![
                    VarDef {
                        mods: Rc::new(Vec::new()),
                        type_: Some(Type::Int),
                        name: Reference::new("a".to_string()),
                    },
                    VarDef {
                        mods: Rc::new(Vec::new()),
                        type_: Some(Type::Struct(Reference::new("B".to_string()))),
                        name: Reference::new("b".to_string()),
                    },
                ],
                body: (Block {
                    definitions: vec![],
                    statements: vec![
                        (Statement::Return(Box::from(Expression {
                            type_: None,
                            expr: ExpressionEnum::Binary(
                                Box::from(Expression {
                                    type_: None,
                                    expr: ExpressionEnum::AtomicExpression(
                                        AtomicExpression::Variable(
                                            Parser::<Lexer>::string_to_namepath("a")
                                        )
                                    ),
                                }),
                                BinOp::Add,
                                Box::from(Expression {
                                    type_: None,
                                    expr: ExpressionEnum::AtomicExpression(
                                        AtomicExpression::Variable(
                                            Parser::<Lexer>::string_to_namepath("b")
                                        )
                                    ),
                                }),
                            ),
                        })))
                    ],
                }),
                mods: Rc::new(Vec::new()),
            }))
        );
    }

    #[test]
    fn function_call_tests() {
        let statement = "add(1, 2, 3);";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let block = parser.parse_module_no_brace(false).unwrap().block;

        assert_eq!(block.statements.len(), 1);
        assert_eq!(
            block.statements[0],
            (Statement::Expression(Box::from(Expression {
                type_: None,
                expr: ExpressionEnum::AtomicExpression(AtomicExpression::FnCall(Box::from(
                    FnCall {
                        name: Reference::new("add".to_string()),
                        args: vec![
                            Expression {
                                type_: None,
                                expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                                    LiteralValue::Int(1)
                                )),
                            },
                            Expression {
                                type_: None,
                                expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                                    LiteralValue::Int(2)
                                )),
                            },
                            Expression {
                                type_: None,
                                expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                                    LiteralValue::Int(3)
                                )),
                            },
                        ],
                    }
                ))),
            })))
        );
    }

    #[test]
    fn if_simple_test() {
        let statement = "if (a == 0) { return 0; }";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let block = parser.parse_module_no_brace(false).unwrap().block;

        assert_eq!(block.statements.len(), 1);
        assert_eq!(
            block.statements[0],
            (Statement::If(If {
                cond: Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::Binary(
                        Box::from(Expression {
                            type_: None,
                            expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                                Parser::<Lexer>::string_to_namepath("a")
                            )),
                        }),
                        BinOp::Eq,
                        Box::from(Expression {
                            type_: None,
                            expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                                LiteralValue::Int(0)
                            )),
                        }),
                    ),
                }),
                body: Box::from(Block {
                    definitions: vec![],
                    statements: vec![
                        (Statement::Return(Box::from(Expression {
                            type_: None,
                            expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                                LiteralValue::Int(0)
                            )),
                        })))
                    ],
                }),
                else_: None,
            }))
        );
    }

    #[test]
    fn if_statement_complex_test() {
        let statement =
            "if (a == 0) { return 0; } else if (a == 1) { return 1; } else { return 2; }";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let block = parser.parse_module_no_brace(false).unwrap().block;

        assert_eq!(block.statements.len(), 1);
        assert_eq!(
            block.statements[0],
            (Statement::If(If {
                cond: Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::Binary(
                        Box::from(Expression {
                            type_: None,
                            expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                                Parser::<Lexer>::string_to_namepath("a")
                            )),
                        }),
                        BinOp::Eq,
                        Box::from(Expression {
                            type_: None,
                            expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                                LiteralValue::Int(0)
                            )),
                        }),
                    ),
                }),
                body: Box::from(Block {
                    definitions: vec![],
                    statements: vec![
                        (Statement::Return(Box::from(Expression {
                            type_: None,
                            expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                                LiteralValue::Int(0)
                            )),
                        })))
                    ],
                }),
                else_: Some(Else::If(Box::from(If {
                    cond: Box::from(Expression {
                        type_: None,
                        expr: ExpressionEnum::Binary(
                            Box::from(Expression {
                                type_: None,
                                expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                                    Parser::<Lexer>::string_to_namepath("a")
                                )),
                            }),
                            BinOp::Eq,
                            Box::from(Expression {
                                type_: None,
                                expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                                    LiteralValue::Int(1)
                                )),
                            }),
                        ),
                    }),
                    body: Box::from(Block {
                        definitions: vec![],
                        statements: vec![
                            (Statement::Return(Box::from(Expression {
                                type_: None,
                                expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                                    LiteralValue::Int(1)
                                )),
                            })))
                        ],
                    }),
                    else_: Some(Else::Block(Box::from(Block {
                        definitions: vec![],
                        statements: vec![
                            (Statement::Return(Box::from(Expression {
                                type_: None,
                                expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                                    LiteralValue::Int(2)
                                )),
                            })))
                        ],
                    }))),
                }))),
            }))
        );
    }

    #[test]
    fn while_test() {
        let statement = "while (a < 10) { a += 1; }";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let block = parser.parse_module_no_brace(false).unwrap().block;

        assert_eq!(block.statements.len(), 1);
        assert_eq!(
            block.statements[0],
            (Statement::While(While {
                cond: Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::Binary(
                        Box::from(Expression {
                            type_: None,
                            expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                                Parser::<Lexer>::string_to_namepath("a")
                            )),
                        }),
                        BinOp::Lt,
                        Box::from(Expression {
                            type_: None,
                            expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                                LiteralValue::Int(10)
                            )),
                        }),
                    ),
                }),
                body: Box::from(Block {
                    definitions: vec![],
                    statements: vec![
                        (Statement::VarAssign(VarAssign {
                            name_path: Parser::<Lexer>::string_to_namepath("a"),
                            expr: Box::from(Expression {
                                type_: None,
                                expr: ExpressionEnum::Binary(
                                    Box::from(Expression {
                                        type_: None,
                                        expr: ExpressionEnum::AtomicExpression(
                                            AtomicExpression::Variable(
                                                Parser::<Lexer>::string_to_namepath("a")
                                            )
                                        ),
                                    }),
                                    BinOp::Add,
                                    Box::from(Expression {
                                        type_: None,
                                        expr: ExpressionEnum::AtomicExpression(
                                            AtomicExpression::Literal(LiteralValue::Int(1))
                                        ),
                                    }),
                                ),
                            }),
                        }))
                    ],
                }),
            }))
        );
    }

    #[test]
    fn for_test() {
        let statement = "for (let i: int = 0; i < 10; i += 1) { a += 1; }";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let block = parser.parse_module_no_brace(false).unwrap().block;

        assert_eq!(block.statements.len(), 1);
        assert_eq!(
            block.statements[0],
            (Statement::For(For {
                init: Some(Box::from(Statement::VarDecl(VarDecl {
                    var_def: VarDef {
                        type_: Some(Type::Int),
                        name: Reference::new("i".to_string()),
                        mods: Rc::new(Vec::new()),
                    },
                    expr: Some(Box::from(Expression {
                        type_: None,
                        expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                            LiteralValue::Int(0)
                        )),
                    })),
                }))),
                cond: Some(Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::Binary(
                        Box::from(Expression {
                            type_: None,
                            expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                                Parser::<Lexer>::string_to_namepath("i")
                            )),
                        }),
                        BinOp::Lt,
                        Box::from(Expression {
                            type_: None,
                            expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                                LiteralValue::Int(10)
                            )),
                        }),
                    ),
                })),
                step: Some(Box::from(Statement::VarAssign(VarAssign {
                    name_path: Parser::<Lexer>::string_to_namepath("i"),
                    expr: Box::from(Expression {
                        type_: None,
                        expr: ExpressionEnum::Binary(
                            Box::from(Expression {
                                type_: None,
                                expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                                    Parser::<Lexer>::string_to_namepath("i")
                                )),
                            }),
                            BinOp::Add,
                            Box::from(Expression {
                                type_: None,
                                expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                                    LiteralValue::Int(1)
                                )),
                            }),
                        ),
                    }),
                }))),
                body: Block {
                    definitions: vec![],
                    statements: vec![
                        (Statement::VarAssign(VarAssign {
                            name_path: Parser::<Lexer>::string_to_namepath("a"),
                            expr: Box::from(Expression {
                                type_: None,
                                expr: ExpressionEnum::Binary(
                                    Box::from(Expression {
                                        type_: None,
                                        expr: ExpressionEnum::AtomicExpression(
                                            AtomicExpression::Variable(
                                                Parser::<Lexer>::string_to_namepath("a")
                                            )
                                        ),
                                    }),
                                    BinOp::Add,
                                    Box::from(Expression {
                                        type_: None,
                                        expr: ExpressionEnum::AtomicExpression(
                                            AtomicExpression::Literal(LiteralValue::Int(1))
                                        ),
                                    }),
                                ),
                            }),
                        }))
                    ],
                },
            }))
        );
    }

    #[test]
    fn loop_break_continue_test() {
        let statement = "while (true) { break; continue; }";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let block = parser.parse_module_no_brace(false).unwrap().block;

        assert_eq!(block.statements.len(), 1);
        assert_eq!(
            block.statements[0],
            (Statement::While(While {
                cond: Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(1)
                    )),
                }),
                body: Box::from(Block {
                    definitions: vec![],
                    statements: vec![(Statement::Break), (Statement::Continue),],
                }),
            }))
        );
    }

    #[test]
    fn simple_expression_order_test() {
        let statement = "a + b + c + d - e - f";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let expr = parser.parse_expression().unwrap();

        assert_eq!(
            expr,
            Box::from(Expression {
                type_: None,
                expr: ExpressionEnum::Binary(
                    Box::from(Expression {
                        type_: None,
                        expr: ExpressionEnum::Binary(
                            Box::from(Expression {
                                type_: None,
                                expr: ExpressionEnum::Binary(
                                    Box::from(Expression {
                                        type_: None,
                                        expr: ExpressionEnum::Binary(
                                            Box::from(Expression {
                                                type_: None,
                                                expr: ExpressionEnum::Binary(
                                                    Box::from(Expression {
                                                        type_: None,
                                                        expr: ExpressionEnum::AtomicExpression(
                                                            AtomicExpression::Variable(
                                                                Parser::<Lexer>::string_to_namepath(
                                                                    "a"
                                                                ),
                                                            )
                                                        ),
                                                    }),
                                                    BinOp::Add,
                                                    Box::from(Expression {
                                                        type_: None,
                                                        expr: ExpressionEnum::AtomicExpression(
                                                            AtomicExpression::Variable(
                                                                Parser::<Lexer>::string_to_namepath(
                                                                    "b"
                                                                ),
                                                            )
                                                        ),
                                                    }),
                                                ),
                                            }),
                                            BinOp::Add,
                                            Box::from(Expression {
                                                type_: None,
                                                expr: ExpressionEnum::AtomicExpression(
                                                    AtomicExpression::Variable(
                                                        Parser::<Lexer>::string_to_namepath("c"),
                                                    )
                                                ),
                                            }),
                                        ),
                                    }),
                                    BinOp::Add,
                                    Box::from(Expression {
                                        type_: None,
                                        expr: ExpressionEnum::AtomicExpression(
                                            AtomicExpression::Variable(
                                                Parser::<Lexer>::string_to_namepath("d"),
                                            )
                                        ),
                                    }),
                                ),
                            }),
                            BinOp::Sub,
                            Box::from(Expression {
                                type_: None,
                                expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                                    Parser::<Lexer>::string_to_namepath("e"),
                                )),
                            }),
                        ),
                    }),
                    BinOp::Sub,
                    Box::from(Expression {
                        type_: None,
                        expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                            Parser::<Lexer>::string_to_namepath("f"),
                        )),
                    }),
                ),
            })
        );
    }

    #[test]
    fn expression_order_test() {
        let statement = "a + b * c - d / e % f";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
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

        let b_times_c = Box::from(Expression {
            type_: None,
            expr: ExpressionEnum::Binary(
                Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("b"),
                    )),
                }),
                BinOp::Mul,
                Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("c"),
                    )),
                }),
            ),
        });

        let a_plus = Box::from(Expression {
            type_: None,
            expr: ExpressionEnum::Binary(
                Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("a"),
                    )),
                }),
                BinOp::Add,
                b_times_c,
            ),
        });

        let d_div_e = Box::from(Expression {
            type_: None,
            expr: ExpressionEnum::Binary(
                Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("d"),
                    )),
                }),
                BinOp::Div,
                Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("e"),
                    )),
                }),
            ),
        });

        let mod_f = Box::from(Expression {
            type_: None,
            expr: ExpressionEnum::Binary(
                d_div_e,
                BinOp::Mod,
                Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("f"),
                    )),
                }),
            ),
        });

        assert_eq!(
            expr,
            Box::from(Expression {
                type_: None,
                expr: ExpressionEnum::Binary(a_plus, BinOp::Sub, mod_f),
            })
        );
    }

    #[test]
    fn expression_order_bracket_test() {
        let statement = "a + b * (c - d) / e % f";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let expr = parser.parse_expression().unwrap();

        // should result in tree
        //                    %
        //                /      \
        //              /         f
        //          /        \
        //         +          e
        //      /       \
        //     a         *
        //           /       \
        //          b         -
        //                  /   \
        //                 c     d

        // (a + (((b * (c - d)) / e) % f))

        let c_minus_d = Box::from(Expression {
            type_: None,
            expr: ExpressionEnum::Binary(
                Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("c"),
                    )),
                }),
                BinOp::Sub,
                Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("d"),
                    )),
                }),
            ),
        });

        let b_times_c_minus_d = Box::from(Expression {
            type_: None,
            expr: ExpressionEnum::Binary(
                Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("b"),
                    )),
                }),
                BinOp::Mul,
                c_minus_d,
            ),
        });

        let b_times_c_minus_d_div_e = Box::from(Expression {
            type_: None,
            expr: ExpressionEnum::Binary(
                b_times_c_minus_d,
                BinOp::Div,
                Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("e"),
                    )),
                }),
            ),
        });

        let b_times_c_minus_d_div_e_mod_f = Box::from(Expression {
            type_: None,
            expr: ExpressionEnum::Binary(
                b_times_c_minus_d_div_e,
                BinOp::Mod,
                Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("f"),
                    )),
                }),
            ),
        });

        let a_plus_b_times_c_minus_d_div_e_mod_f = Box::from(Expression {
            type_: None,
            expr: ExpressionEnum::Binary(
                Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("a"),
                    )),
                }),
                BinOp::Add,
                b_times_c_minus_d_div_e_mod_f,
            ),
        });

        assert_eq!(expr, a_plus_b_times_c_minus_d_div_e_mod_f);
    }

    #[test]
    fn complex_expression_order_test() {
        let statement = "a == b && c || d != e";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let expr = parser.parse_expression().unwrap();

        // should result in tree
        //                             ||
        //                       /            \
        //                      &&            !=
        //                     /  \          /   \
        //                    ==   c        d     e
        //                   /  \
        //                  a    b
        //

        // ((a == b) && c) || ((d != e))

        let a_eq_b = Box::from(Expression {
            type_: None,
            expr: ExpressionEnum::Binary(
                Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("a"),
                    )),
                }),
                BinOp::Eq,
                Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("b"),
                    )),
                }),
            ),
        });

        let a_eq_b_and_c = Box::from(Expression {
            type_: None,
            expr: ExpressionEnum::Binary(
                a_eq_b,
                BinOp::And,
                Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("c"),
                    )),
                }),
            ),
        });

        let d_neq_e = Box::from(Expression {
            type_: None,
            expr: ExpressionEnum::Binary(
                Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("d"),
                    )),
                }),
                BinOp::Neq,
                Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("e"),
                    )),
                }),
            ),
        });

        let a_eq_b_and_c_or_d_neq_e = Box::from(Expression {
            type_: None,
            expr: ExpressionEnum::Binary(a_eq_b_and_c, BinOp::Or, d_neq_e),
        });

        assert_eq!(expr, a_eq_b_and_c_or_d_neq_e);
    }

    #[test]
    fn expression_deref_test() {
        let statement = "*a + b * *d";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let expr = parser.parse_expression().unwrap();

        // should result in tree
        //          +
        //      /       \
        //    *a         *
        //              / \
        //             b   *d

        // (*a + (b * (*d)))

        let deref_a = Box::from(Expression {
            type_: None,
            expr: ExpressionEnum::Unary(
                UnOp::Deref,
                Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("a"),
                    )),
                }),
            ),
        });

        let deref_d = Box::from(Expression {
            type_: None,
            expr: ExpressionEnum::Unary(
                UnOp::Deref,
                Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("d"),
                    )),
                }),
            ),
        });

        let b_times_deref_d = Box::from(Expression {
            type_: None,
            expr: ExpressionEnum::Binary(
                Box::from(Expression {
                    type_: None,
                    expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("b"),
                    )),
                }),
                BinOp::Mul,
                deref_d,
            ),
        });

        let deref_a_plus_b_times_deref_d = Box::from(Expression {
            type_: None,
            expr: ExpressionEnum::Binary(deref_a, BinOp::Add, b_times_deref_d),
        });

        assert_eq!(expr, deref_a_plus_b_times_deref_d);
    }

    #[test]
    fn plusplus_check_test() {
        let statement = "a++ + ++b";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let expr = parser.parse_expression().unwrap();

        assert_eq!(
            expr,
            Box::from(Expression {
                type_: None,
                expr: ExpressionEnum::Binary(
                    Box::from(Expression {
                        type_: None,
                        expr: ExpressionEnum::Unary(
                            UnOp::PostInc,
                            Box::from(Expression {
                                type_: None,
                                expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                                    Parser::<Lexer>::string_to_namepath("a")
                                )),
                            }),
                        ),
                    }),
                    BinOp::Add,
                    Box::from(Expression {
                        type_: None,
                        expr: ExpressionEnum::Unary(
                            UnOp::PreInc,
                            Box::from(Expression {
                                type_: None,
                                expr: ExpressionEnum::AtomicExpression(AtomicExpression::Variable(
                                    Parser::<Lexer>::string_to_namepath("b")
                                )),
                            }),
                        ),
                    }),
                ),
            })
        );
    }

    #[test]
    fn struct_definition_test() {
        let statement = "struct A { a: int, b: float, c: double, d: C, }";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let block = parser.parse_module_no_brace(false).unwrap().block;

        assert_eq!(block.definitions.len(), 1);
        assert_eq!(
            block.definitions[0],
            Definition::StructDef(StructDef {
                mods: Rc::new(Vec::new()),
                type_name: Reference::new("A".to_string()),
                map: {
                    let mut map = HashMap::new();
                    map.insert("a".to_string(), Type::Int);
                    map.insert("b".to_string(), Type::Float);
                    map.insert("c".to_string(), Type::Double);
                    map.insert(
                        "d".to_string(),
                        Type::Struct(Reference::new("C".to_string())),
                    );
                    map
                },
            })
        );
    }

    #[test]
    fn multiple_declaration_test() {
        let statement = "let a: int; fn main(a: int) { let a: int; a + 1; return 0; }";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let block = parser.parse_module_no_brace(false).unwrap().block;

        assert_eq!(block.definitions.len(), 1);
        assert_eq!(block.statements.len(), 1);
        assert_eq!(
            block.statements[0],
            (Statement::VarDecl(VarDecl {
                var_def: VarDef {
                    type_: Some(Type::Int),
                    name: Reference::new("a".to_string()),
                    mods: Rc::new(Vec::new()),
                },
                expr: None,
            }))
        );
        assert_eq!(
            block.definitions[0],
            (Definition::FnDef(FnDef {
                return_type: Type::Void,
                name: Reference::new("main".to_string()),
                args: vec![VarDef {
                    mods: Rc::new(Vec::new()),
                    type_: Some(Type::Int),
                    name: Reference::new("a".to_string()),
                }],
                body: (Block {
                    definitions: vec![],
                    statements: vec![
                        (Statement::VarDecl(VarDecl {
                            var_def: VarDef {
                                type_: Some(Type::Int),
                                name: Reference::new("a".to_string()),
                                mods: Rc::new(Vec::new()),
                            },
                            expr: None,
                        })),
                        (Statement::Expression(Box::from(Expression {
                            type_: None,
                            expr: ExpressionEnum::Binary(
                                Box::from(Expression {
                                    type_: None,
                                    expr: ExpressionEnum::AtomicExpression(
                                        AtomicExpression::Variable(
                                            Parser::<Lexer>::string_to_namepath("a")
                                        )
                                    ),
                                }),
                                BinOp::Add,
                                Box::from(Expression {
                                    type_: None,
                                    expr: ExpressionEnum::AtomicExpression(
                                        AtomicExpression::Literal(LiteralValue::Int(1))
                                    ),
                                }),
                            ),
                        }))),
                        (Statement::Return(Box::from(Expression {
                            type_: None,
                            expr: ExpressionEnum::AtomicExpression(AtomicExpression::Literal(
                                LiteralValue::Int(0)
                            )),
                        }))),
                    ],
                }),
                mods: Rc::new(Vec::new()),
            }))
        );
    }

    #[test]
    fn multiple_use() {
        let statement = "use root::test::{foo, bar};";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let module = parser.parse_module_no_brace(false).unwrap();

        assert_eq!(module.uses.len(), 1);
        assert_eq!(
            module.uses[0],
            Use {
                path: vec!["root".to_string(), "test".to_string()],
                elements: vec![
                    UseElement {
                        origin_name: "foo".to_string(),
                        imported_name: Reference {
                            raw: "foo".to_string(),
                            module_resolved: None,
                            global_resolved: None,
                        },
                    },
                    UseElement {
                        origin_name: "bar".to_string(),
                        imported_name: Reference {
                            raw: "bar".to_string(),
                            module_resolved: None,
                            global_resolved: None,
                        },
                    },
                ],
            }
        );

        println!("{:?}", module);
    }
}
