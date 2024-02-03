use crate::front::lexical::lexer::TokenError;
use crate::front::lexical::token_types::Token;
use crate::front::lexical::token_types::Token::Any;
use crate::front::syntax::ast_types::{
    AtomicExpression, BinOp, Block, Compound, CompoundValue, Definition, Expression, FnCall, FnDef,
    FnMod, For, If, LiteralValue, Module, ModuleImport, NamePath, Reference, Statement,
    StatementBlock, StructDef, StructMod, Type, UnOp, Use, UseElement, VarAssign, VarDecl, VarDef,
    VarMod, While,
};
use std::collections::{HashMap, VecDeque};
use std::mem;
use std::rc::Rc;

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
    future_tokens: VecDeque<Token>,
}

impl<T: TokenStream> Parser<T> {
    pub fn new(lexer: T) -> Parser<T> {
        let mut parser = Parser {
            lexer,
            curr_token: Token::Eof,
            future_tokens: VecDeque::new(),
        };
        parser.eat(&Token::Eof).unwrap();
        parser
    }

    fn next(&mut self) -> Token {
        match self.future_tokens.pop_front() {
            None => self.lexer.next().unwrap(),
            Some(front) => front,
        }
    }

    fn eat(&mut self, type_: &Token) -> ParseResult<Token> {
        // return old lexical, set new lexical, set one buffer of next lexical
        if mem::discriminant(&self.curr_token) == mem::discriminant(type_) || matches!(type_, Any) {
            let old_curr = self.curr_token.clone();
            self.curr_token = self.next();

            Ok(old_curr)
        } else {
            Err(ParseError::Unexpected(
                self.curr_token.clone(),
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

        &self.future_tokens[count as usize - 1]
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
        let name: Reference<String, String> = Reference::new(path.remove(0));

        NamePath { name, path }
    }

    fn parse_atomic_expression(&mut self) -> ParseResult<Expression> {
        if matches!(self.curr_token, Token::LBrace) {
            let compound = self.parse_compound()?;
            return Ok(Expression::AtomicExpression(AtomicExpression::Literal(
                LiteralValue::Compound(compound),
            )));
        }

        match self.eat(&Any)? {
            Token::Null => Ok(Expression::AtomicExpression(AtomicExpression::Literal(
                LiteralValue::Null,
            ))),
            Token::Bool(b) => Ok(Expression::AtomicExpression(AtomicExpression::Literal(
                LiteralValue::Bool(b),
            ))),
            Token::Int(i) => Ok(Expression::AtomicExpression(AtomicExpression::Literal(
                LiteralValue::Int(i),
            ))),
            Token::Decimal(f) => Ok(Expression::AtomicExpression(AtomicExpression::Literal(
                LiteralValue::Decimal(f),
            ))),
            Token::String(s) => Ok(Expression::AtomicExpression(AtomicExpression::Literal(
                LiteralValue::String(s),
            ))),
            Token::Ident(s) => {
                if matches!(self.curr_token, Token::LParen) {
                    let mut fn_call = Box::from(FnCall {
                        name: Reference::new(s),
                        args: Vec::new(),
                    });
                    self.eat(&Token::LParen)?;

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

                    // function call
                    Ok(Expression::AtomicExpression(AtomicExpression::FnCall(
                        fn_call,
                    )))
                } else {
                    let var = Expression::AtomicExpression(AtomicExpression::Variable(
                        Parser::<T>::string_to_namepath(&s),
                    ));
                    match self.curr_token {
                        Token::PlusPlus => {
                            self.eat(&Token::PlusPlus)?;
                            Ok(Expression::Unary(UnOp::PostInc, Box::from(var)))
                        }
                        Token::MinusMinus => {
                            self.eat(&Token::MinusMinus)?;
                            Ok(Expression::Unary(UnOp::PostDec, Box::from(var)))
                        }
                        _ => Ok(var),
                    }
                }
            }
            tok => Err(ParseError::Unexpected(
                tok.clone(),
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
                    return Ok(Box::from(Expression::Binary(lhs, binop, rhs)));
                }
            };

            if tok_prec < next_prec {
                rhs = self.parse_bin_op_rhs(tok_prec + 1, rhs)?;
            }

            lhs = Box::from(Expression::Binary(lhs, binop, rhs));
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
                Ok(Box::from(Expression::Unary(unop, right)))
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
                let name_path = Parser::<T>::string_to_namepath(var_name);

                let assign_op = match self.eat(&Any)? {
                    Token::Assign => {
                        return Ok(Statement::VarAssign(VarAssign {
                            name_path,
                            expr: self.parse_expression()?,
                        }));
                    }
                    Token::PlusAssign => BinOp::Add,
                    Token::MinusAssign => BinOp::Sub,
                    Token::StarAssign => BinOp::Mul,
                    Token::SlashAssign => BinOp::Div,
                    Token::PercentAssign => BinOp::Mod,
                    tok => {
                        return Err(ParseError::Unexpected(
                            tok.clone(),
                            "Expected binary operator".to_string(),
                        ))?;
                    }
                };

                let rhs = self.parse_expression()?;

                let expr = Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        name_path.clone(),
                    ))),
                    assign_op,
                    rhs,
                );

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

        match self.curr_token {
            Token::Mod => Ok(Definition::ModuleImport(self.parse_module_import()?)),
            Token::Use => Ok(Definition::Use(self.parse_use_import()?)),
            _ => Err(ParseError::Unexpected(
                self.curr_token.clone(),
                "Expected definition".to_string(),
            )),
        }
    }

    fn parse_module_no_brace(&mut self, global: bool) -> ParseResult<Module> {
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
                return Err(ParseError::Unexpected(
                    self.curr_token.clone(),
                    "Cannot use pub in local scope".to_string(),
                ));
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
                            statements.push(StatementBlock::Statement(Statement::VarDecl(
                                self.parse_var_decl()?,
                            )));
                        }
                        continue;
                    }
                    _ => {}
                }
            }

            match self.curr_token {
                Token::LBrace => statements.push(StatementBlock::Block(self.parse_block()?)),
                _ => {
                    if global {
                        Err(ParseError::Unexpected(
                            self.curr_token.clone(),
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
                    statements.push(StatementBlock::Statement(statement));
                }
            }
        }

        struct_var_definitions.append(&mut fn_definitions);
        pub_struct_var_definitions.append(&mut pub_fn_definitions);

        Ok(Module {
            public_definitions: pub_struct_var_definitions,
            block: Block {
                definitions: struct_var_definitions,
                statements,
            },
        })
    }

    fn parse_block(&mut self) -> ParseResult<Block> {
        self.eat(&Token::LBrace)?;
        let module = self.parse_module_no_brace(false)?;
        self.eat(&Token::RBrace)?;

        if !module.public_definitions.is_empty() {
            Err(ParseError::Unexpected(
                Token::Pub,
                "Cannot use pub in local scope".to_string(),
            ))?;
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
        if self.eat(&Token::Pub).is_ok() {
            mods.push(VarMod::Pub);
        }

        if self.eat(&Token::Const).is_ok() {
            mods.push(VarMod::Const);
        } else {
            self.eat(&Token::Let)?;
        }

        let var_name = match self.eat(&Any)? {
            Token::Ident(s) => s,
            tok => Err(ParseError::Unexpected(
                tok,
                "Expected identifier for variable name".to_string(),
            ))?,
        };

        self.eat(&Token::Colon)?; // required for now because we don't have type inference

        let type_ = match self.eat(&Any)? {
            Token::VoidType => Type::Void,
            Token::IntType => Type::Int,
            Token::FloatType => Type::Float,
            Token::DoubleType => Type::Double,
            Token::BoolType => Type::Bool,
            Token::StringType => Type::String,
            Token::Ident(s) => Type::Struct(Reference::new(s)),
            tok => Err(ParseError::Unexpected(
                tok,
                "Expected variable type annotation for variable declaration".to_string(),
            ))?,
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

        if self.eat(&Token::Pub).is_ok() {
            mods.push(FnMod::Pub);
        }

        let mut rec = false;

        if self.eat(&Token::Rec).is_ok() {
            mods.push(FnMod::Rec);
            rec = true;
        }

        if self.eat(&Token::Inline).is_ok() {
            mods.push(FnMod::Inline);
            if rec {
                Err(ParseError::Unexpected(
                    Token::Inline,
                    "Inline functions cannot be recursive".to_string(),
                ))?;
            }
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
        let mut args: Vec<VarDef> = Vec::new();

        if self.eat(&Token::RParen).is_err() {
            loop {
                let mut mods = Vec::new();
                if self.eat(&Token::Const).is_ok() {
                    mods.push(VarMod::Const);
                }

                let name = Reference::new(match self.eat(&Any)? {
                    Token::Ident(s) => s,
                    tok => {
                        return Err(ParseError::Unexpected(
                            tok,
                            "Expected identifier for function argument name".to_string(),
                        ));
                    }
                });

                self.eat(&Token::Colon)?; // required for now because we don't have type inference

                let type_ = match self.eat(&Any)? {
                    Token::VoidType => Type::Void,
                    Token::IntType => Type::Int,
                    Token::FloatType => Type::Float,
                    Token::DoubleType => Type::Double,
                    Token::BoolType => Type::Bool,
                    Token::StringType => Type::String,
                    Token::Ident(s) => Type::Struct(Reference::new(s)),
                    tok => {
                        return Err(ParseError::Unexpected(
                            tok,
                            "Expected type for function argument".to_string(),
                        ));
                    }
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
                Token::VoidType => Type::Void,
                Token::IntType => Type::Int,
                Token::FloatType => Type::Float,
                Token::DoubleType => Type::Double,
                Token::BoolType => Type::Bool,
                Token::StringType => Type::String,
                Token::Ident(s) => Type::Struct(Reference::new(s)),
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
        let mut mods = Vec::new();

        if self.eat(&Token::Pub).is_ok() {
            mods.push(StructMod::Pub);
        }

        self.eat(&Token::StructType)?;

        let struct_name = match self.eat(&Any)? {
            Token::Ident(s) => s,
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
                Token::Ident(s) => s,
                tok => {
                    return Err(ParseError::Unexpected(
                        tok,
                        "Expected identifier for struct field name".to_string(),
                    ));
                }
            };

            self.eat(&Token::Colon)?; // required for now because we don't have type inference

            let type_ = match self.eat(&Any)? {
                Token::VoidType => Type::Void,
                Token::IntType => Type::Int,
                Token::FloatType => Type::Float,
                Token::DoubleType => Type::Double,
                Token::BoolType => Type::Bool,
                Token::StringType => Type::String,
                Token::Ident(s) => Type::Struct(Reference::new(s)),
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

        if let Token::Ident(s) = self.eat(&Any)? {
            Ok(ModuleImport { public, name: s })
        } else {
            Err(ParseError::Unexpected(
                self.curr_token.clone(),
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
            if let Token::Ident(s) = self.eat(&Any)? {
                match self.eat(&Any)? {
                    Token::Colon => {
                        self.eat(&Token::Colon)?;
                        use_.path.push(s);
                    }
                    Token::LBrace => {
                        use_.path.push(s);
                        break;
                    }
                    Token::Semicolon | Token::As => {
                        use_.elements.push(UseElement {
                            origin_name: s.clone(),
                            imported_name: Reference::new((|| {
                                if self.eat(&Token::As).is_ok() {
                                    return if let Token::Ident(s) = self.eat(&Any)? {
                                        Ok(s)
                                    } else {
                                        Err(ParseError::Unexpected(
                                            self.curr_token.clone(),
                                            "Expected identifier for imported name alias"
                                                .to_string(),
                                        ))
                                    };
                                }
                                Ok(s)
                            })()?),
                        });

                        return Ok(use_);
                    }
                    tok => {
                        return Err(ParseError::Unexpected(
                            tok.clone(),
                            "Expected identifier for use path".to_string(),
                        ));
                    }
                }
            }
        }
        Ok(use_)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::front::file_system::byte_stream::{ByteStream, StringReader};
    use crate::front::lexical::lexer::Lexer;

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
                    statements: vec![StatementBlock::Statement(Statement::Return(Box::from(
                        Expression::AtomicExpression(AtomicExpression::Literal(LiteralValue::Int(
                            0
                        )))
                    )))],
                },
                mods: Rc::new(Vec::new()),
            })
        );
    }

    #[test]
    fn variable_declarations_test() {
        let statement = "const a: int = 0; const b: int = 1; let c: int = 2; let d: float = 3.0; let e: double = 4.0; let f: bool = true; let g: string = \"hello\";";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let block = parser.parse_module_no_brace(false).unwrap().block;

        assert_eq!(block.statements.len(), 7);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::VarDecl(VarDecl {
                var_def: VarDef {
                    type_: Type::Int,
                    name: Reference::new("a".to_string()),
                    mods: Rc::new(vec![VarMod::Const]),
                },
                expr: Some(Box::from(Expression::AtomicExpression(
                    AtomicExpression::Literal(LiteralValue::Int(0))
                ))),
            }))
        );
        assert_eq!(
            block.statements[1],
            StatementBlock::Statement(Statement::VarDecl(VarDecl {
                var_def: VarDef {
                    type_: Type::Int,
                    name: Reference::new("b".to_string()),
                    mods: Rc::new(vec![VarMod::Const]),
                },
                expr: Some(Box::from(Expression::AtomicExpression(
                    AtomicExpression::Literal(LiteralValue::Int(1))
                ))),
            }))
        );
        assert_eq!(
            block.statements[2],
            StatementBlock::Statement(Statement::VarDecl(VarDecl {
                var_def: VarDef {
                    type_: Type::Int,
                    name: Reference::new("c".to_string()),
                    mods: Rc::new(Vec::new()),
                },
                expr: Some(Box::from(Expression::AtomicExpression(
                    AtomicExpression::Literal(LiteralValue::Int(2))
                ))),
            }))
        );
        assert_eq!(
            block.statements[3],
            StatementBlock::Statement(Statement::VarDecl(VarDecl {
                var_def: VarDef {
                    type_: Type::Float,
                    name: Reference::new("d".to_string()),
                    mods: Rc::new(Vec::new()),
                },
                expr: Some(Box::from(Expression::AtomicExpression(
                    AtomicExpression::Literal(LiteralValue::Decimal(3.0))
                ))),
            }))
        );
        assert_eq!(
            block.statements[4],
            StatementBlock::Statement(Statement::VarDecl(VarDecl {
                var_def: VarDef {
                    type_: Type::Double,
                    name: Reference::new("e".to_string()),
                    mods: Rc::new(Vec::new()),
                },
                expr: Some(Box::from(Expression::AtomicExpression(
                    AtomicExpression::Literal(LiteralValue::Decimal(4.0))
                ))),
            }))
        );
        assert_eq!(
            block.statements[5],
            StatementBlock::Statement(Statement::VarDecl(VarDecl {
                var_def: VarDef {
                    type_: Type::Bool,
                    name: Reference::new("f".to_string()),
                    mods: Rc::new(Vec::new()),
                },
                expr: Some(Box::from(Expression::AtomicExpression(
                    AtomicExpression::Literal(LiteralValue::Bool(true))
                ))),
            }))
        );
        assert_eq!(
            block.statements[6],
            StatementBlock::Statement(Statement::VarDecl(VarDecl {
                var_def: VarDef {
                    type_: Type::String,
                    name: Reference::new("g".to_string()),
                    mods: Rc::new(Vec::new()),
                },
                expr: Some(Box::from(Expression::AtomicExpression(
                    AtomicExpression::Literal(LiteralValue::String("hello".to_string()))
                ))),
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
                CompoundValue::Expression(Box::from(Expression::AtomicExpression(
                    AtomicExpression::Literal(LiteralValue::Int(0)),
                ))),
            );
            map.insert(
                "b".to_string(),
                CompoundValue::Expression(Box::from(Expression::AtomicExpression(
                    AtomicExpression::Literal(LiteralValue::Int(1)),
                ))),
            );
            map.insert(
                "c".to_string(),
                CompoundValue::Expression(Box::from(Expression::AtomicExpression(
                    AtomicExpression::Literal(LiteralValue::Int(2)),
                ))),
            );
            map
        };

        assert_eq!(block.statements.len(), 2);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::VarDecl(VarDecl {
                var_def: VarDef {
                    type_: Type::Struct(Reference::new("A".to_string())),
                    name: Reference::new("a".to_string()),
                    mods: Rc::new(Vec::new()),
                },
                expr: Some(Box::from(Expression::AtomicExpression(
                    AtomicExpression::Literal(LiteralValue::Compound(compound))
                ))),
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
            StatementBlock::Statement(Statement::VarAssign(VarAssign {
                name_path: Parser::<Lexer>::string_to_namepath("a"),
                expr: Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                    LiteralValue::Int(0)
                ))),
            }))
        );
        assert_eq!(
            block.statements[1],
            StatementBlock::Statement(Statement::VarAssign(VarAssign {
                name_path: Parser::<Lexer>::string_to_namepath("b"),
                expr: Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                    LiteralValue::Decimal(2.4)
                ))),
            }))
        );
        assert_eq!(
            block.statements[2],
            StatementBlock::Statement(Statement::VarAssign(VarAssign {
                name_path: Parser::<Lexer>::string_to_namepath("a"),
                expr: Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("a")
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
                name_path: Parser::<Lexer>::string_to_namepath("a"),
                expr: Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("a")
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
                name_path: Parser::<Lexer>::string_to_namepath("a"),
                expr: Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("a")
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
                name_path: Parser::<Lexer>::string_to_namepath("a"),
                expr: Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("a")
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
                name_path: Parser::<Lexer>::string_to_namepath("a"),
                expr: Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("a")
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
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let block = parser.parse_module_no_brace(false).unwrap().block;

        assert_eq!(block.statements.len(), 2);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::VarAssign(VarAssign {
                name_path: Parser::<Lexer>::string_to_namepath("a"),
                expr: Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                    LiteralValue::Compound({
                        let mut map = HashMap::new();
                        map.insert(
                            "a".to_string(),
                            CompoundValue::Expression(Box::from(Expression::AtomicExpression(
                                AtomicExpression::Literal(LiteralValue::Int(0)),
                            ))),
                        );
                        map.insert(
                            "b".to_string(),
                            CompoundValue::Expression(Box::from(Expression::AtomicExpression(
                                AtomicExpression::Literal(LiteralValue::Int(1)),
                            ))),
                        );
                        map.insert(
                            "c".to_string(),
                            CompoundValue::Expression(Box::from(Expression::AtomicExpression(
                                AtomicExpression::Literal(LiteralValue::Int(2)),
                            ))),
                        );

                        map
                    })
                ))),
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
                        type_: Type::Int,
                        name: Reference::new("a".to_string()),
                    },
                    VarDef {
                        mods: Rc::new(Vec::new()),
                        type_: Type::Struct(Reference::new("B".to_string())),
                        name: Reference::new("b".to_string()),
                    },
                ],
                body: Block {
                    definitions: vec![],
                    statements: vec![StatementBlock::Statement(Statement::Return(Box::from(
                        Expression::Binary(
                            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                                Parser::<Lexer>::string_to_namepath("a")
                            ))),
                            BinOp::Add,
                            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                                Parser::<Lexer>::string_to_namepath("b")
                            ))),
                        )
                    )))],
                },
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
            StatementBlock::Statement(Statement::Expression(Box::from(
                Expression::AtomicExpression(AtomicExpression::FnCall(Box::from(FnCall {
                    name: Reference::new("add".to_string()),
                    args: vec![
                        Expression::AtomicExpression(AtomicExpression::Literal(LiteralValue::Int(
                            1
                        ))),
                        Expression::AtomicExpression(AtomicExpression::Literal(LiteralValue::Int(
                            2
                        ))),
                        Expression::AtomicExpression(AtomicExpression::Literal(LiteralValue::Int(
                            3
                        ))),
                    ],
                })))
            )))
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
            StatementBlock::Statement(Statement::If(If {
                cond: Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("a")
                    ))),
                    BinOp::Eq,
                    Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(0)
                    ))),
                )),
                body: Box::from(Block {
                    definitions: vec![],
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
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let block = parser.parse_module_no_brace(false).unwrap().block;

        assert_eq!(block.statements.len(), 1);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::If(If {
                cond: Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("a")
                    ))),
                    BinOp::Eq,
                    Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(0)
                    ))),
                )),
                body: Box::from(Block {
                    definitions: vec![],
                    statements: vec![StatementBlock::Statement(Statement::Return(Box::from(
                        Expression::AtomicExpression(AtomicExpression::Literal(LiteralValue::Int(
                            0
                        )))
                    )))],
                }),
                else_: Some(Box::from(If {
                    cond: Box::from(Expression::Binary(
                        Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                            Parser::<Lexer>::string_to_namepath("a")
                        ))),
                        BinOp::Eq,
                        Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                            LiteralValue::Int(1)
                        ))),
                    )),
                    body: Box::from(Block {
                        definitions: vec![],
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
                            definitions: vec![],
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
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let block = parser.parse_module_no_brace(false).unwrap().block;

        assert_eq!(block.statements.len(), 1);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::While(While {
                cond: Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("a")
                    ))),
                    BinOp::Lt,
                    Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(10)
                    ))),
                )),
                body: Box::from(Block {
                    definitions: vec![],
                    statements: vec![StatementBlock::Statement(Statement::VarAssign(VarAssign {
                        name_path: Parser::<Lexer>::string_to_namepath("a"),
                        expr: Box::from(Expression::Binary(
                            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                                Parser::<Lexer>::string_to_namepath("a")
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
        let statement = "for (let i: int = 0; i < 10; i += 1) { a += 1; }";
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let block = parser.parse_module_no_brace(false).unwrap().block;

        assert_eq!(block.statements.len(), 1);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::For(For {
                init: Some(Box::from(Statement::VarDecl(VarDecl {
                    var_def: VarDef {
                        type_: Type::Int,
                        name: Reference::new("i".to_string()),
                        mods: Rc::new(Vec::new()),
                    },
                    expr: Some(Box::from(Expression::AtomicExpression(
                        AtomicExpression::Literal(LiteralValue::Int(0))
                    ))),
                }))),
                cond: Some(Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("i")
                    ))),
                    BinOp::Lt,
                    Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(10)
                    ))),
                ))),
                step: Some(Box::from(Statement::VarAssign(VarAssign {
                    name_path: Parser::<Lexer>::string_to_namepath("i"),
                    expr: Box::from(Expression::Binary(
                        Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                            Parser::<Lexer>::string_to_namepath("i")
                        ))),
                        BinOp::Add,
                        Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                            LiteralValue::Int(1)
                        ))),
                    )),
                }))),
                body: Block {
                    definitions: vec![],
                    statements: vec![StatementBlock::Statement(Statement::VarAssign(VarAssign {
                        name_path: Parser::<Lexer>::string_to_namepath("a"),
                        expr: Box::from(Expression::Binary(
                            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                                Parser::<Lexer>::string_to_namepath("a")
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
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
        let mut parser = Parser::new(lexer);

        let block = parser.parse_module_no_brace(false).unwrap().block;

        assert_eq!(block.statements.len(), 1);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::While(While {
                cond: Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                    LiteralValue::Bool(true)
                ))),
                body: Box::from(Block {
                    definitions: vec![],
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
        let lexer = Lexer::new(ByteStream::new(Box::from(StringReader::new(
            statement.to_string(),
        ))));
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
                                    AtomicExpression::Variable(
                                        Parser::<Lexer>::string_to_namepath("a"),
                                    )
                                )),
                                BinOp::Add,
                                Box::from(Expression::AtomicExpression(
                                    AtomicExpression::Variable(
                                        Parser::<Lexer>::string_to_namepath("b"),
                                    )
                                )),
                            )),
                            BinOp::Add,
                            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                                Parser::<Lexer>::string_to_namepath("c"),
                            ))),
                        )),
                        BinOp::Add,
                        Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                            Parser::<Lexer>::string_to_namepath("d"),
                        ))),
                    )),
                    BinOp::Sub,
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("e"),
                    ))),
                )),
                BinOp::Sub,
                Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                    Parser::<Lexer>::string_to_namepath("f"),
                ))),
            ))
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

        let b_times_c = Box::from(Expression::Binary(
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer>::string_to_namepath("b"),
            ))),
            BinOp::Mul,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer>::string_to_namepath("c"),
            ))),
        ));

        let a_plus = Box::from(Expression::Binary(
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer>::string_to_namepath("a"),
            ))),
            BinOp::Add,
            b_times_c,
        ));

        let d_div_e = Box::from(Expression::Binary(
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer>::string_to_namepath("d"),
            ))),
            BinOp::Div,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer>::string_to_namepath("e"),
            ))),
        ));

        let mod_f = Box::from(Expression::Binary(
            d_div_e,
            BinOp::Mod,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer>::string_to_namepath("f"),
            ))),
        ));

        assert_eq!(
            expr,
            Box::from(Expression::Binary(a_plus, BinOp::Sub, mod_f))
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

        let c_minus_d = Box::from(Expression::Binary(
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer>::string_to_namepath("c"),
            ))),
            BinOp::Sub,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer>::string_to_namepath("d"),
            ))),
        ));

        let b_times_c_minus_d = Box::from(Expression::Binary(
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer>::string_to_namepath("b"),
            ))),
            BinOp::Mul,
            c_minus_d,
        ));

        let b_times_c_minus_d_div_e = Box::from(Expression::Binary(
            b_times_c_minus_d,
            BinOp::Div,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer>::string_to_namepath("e"),
            ))),
        ));

        let b_times_c_minus_d_div_e_mod_f = Box::from(Expression::Binary(
            b_times_c_minus_d_div_e,
            BinOp::Mod,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer>::string_to_namepath("f"),
            ))),
        ));

        let a_plus_b_times_c_minus_d_div_e_mod_f = Box::from(Expression::Binary(
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer>::string_to_namepath("a"),
            ))),
            BinOp::Add,
            b_times_c_minus_d_div_e_mod_f,
        ));

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

        let a_eq_b = Box::from(Expression::Binary(
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer>::string_to_namepath("a"),
            ))),
            BinOp::Eq,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer>::string_to_namepath("b"),
            ))),
        ));

        let a_eq_b_and_c = Box::from(Expression::Binary(
            a_eq_b,
            BinOp::And,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer>::string_to_namepath("c"),
            ))),
        ));

        let d_neq_e = Box::from(Expression::Binary(
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer>::string_to_namepath("d"),
            ))),
            BinOp::Neq,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer>::string_to_namepath("e"),
            ))),
        ));

        let a_eq_b_and_c_or_d_neq_e =
            Box::from(Expression::Binary(a_eq_b_and_c, BinOp::Or, d_neq_e));

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

        let deref_a = Box::from(Expression::Unary(
            UnOp::Deref,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer>::string_to_namepath("a"),
            ))),
        ));

        let deref_d = Box::from(Expression::Unary(
            UnOp::Deref,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer>::string_to_namepath("d"),
            ))),
        ));

        let b_times_deref_d = Box::from(Expression::Binary(
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer>::string_to_namepath("b"),
            ))),
            BinOp::Mul,
            deref_d,
        ));

        let deref_a_plus_b_times_deref_d =
            Box::from(Expression::Binary(deref_a, BinOp::Add, b_times_deref_d));

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
            Box::from(Expression::Binary(
                Box::from(Expression::Unary(
                    UnOp::PostInc,
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("a")
                    ))),
                )),
                BinOp::Add,
                Box::from(Expression::Unary(
                    UnOp::PreInc,
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer>::string_to_namepath("b")
                    ))),
                )),
            ))
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
            StatementBlock::Statement(Statement::VarDecl(VarDecl {
                var_def: VarDef {
                    type_: Type::Int,
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
                    type_: Type::Int,
                    name: Reference::new("a".to_string()),
                }],
                body: Block {
                    definitions: vec![],
                    statements: vec![
                        StatementBlock::Statement(Statement::VarDecl(VarDecl {
                            var_def: VarDef {
                                type_: Type::Int,
                                name: Reference::new("a".to_string()),
                                mods: Rc::new(Vec::new()),
                            },
                            expr: None,
                        })),
                        StatementBlock::Statement(Statement::Expression(Box::from(
                            Expression::Binary(
                                Box::from(Expression::AtomicExpression(
                                    AtomicExpression::Variable(
                                        Parser::<Lexer>::string_to_namepath("a")
                                    )
                                )),
                                BinOp::Add,
                                Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                                    LiteralValue::Int(1)
                                ))),
                            )
                        ))),
                        StatementBlock::Statement(Statement::Return(Box::from(
                            Expression::AtomicExpression(AtomicExpression::Literal(
                                LiteralValue::Int(0)
                            ))
                        ))),
                    ],
                },
                mods: Rc::new(Vec::new()),
            }))
        );
    }
}
