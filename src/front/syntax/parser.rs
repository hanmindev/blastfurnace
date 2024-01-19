use crate::front::lexical::lexer::TokenError;
use crate::front::lexical::token_types::Token;
use crate::front::lexical::token_types::Token::Any;
use crate::front::syntax::ast_types::{
    AtomicExpression, BinOp, Block, Compound, CompoundValue, Expression, FnCall, FnDef, FnMod, For,
    If, LiteralValue, ModuleImport, NamePath, Reference, Statement, StatementBlock, StructDef,
    Type, UnOp, Use, UseElement, VarAssign, VarDecl, VarDef, VarMod, While,
};
use std::collections::HashMap;
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
    next_token: Token,
}

impl<T: TokenStream> Parser<T> {
    pub fn new(lexer: T) -> Parser<T> {
        let mut parser = Parser {
            lexer,
            curr_token: Token::Eof,
            next_token: Token::Eof,
        };
        parser.eat(&Token::Eof).unwrap();
        parser.eat(&Token::Eof).unwrap();
        parser
    }

    fn next(&mut self) -> Token {
        self.lexer.next().unwrap()
    }

    fn eat(&mut self, type_: &Token) -> ParseResult<Token> {
        // return old lexical, set new lexical, set one buffer of next lexical
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
                        name_path: Parser::<T>::string_to_namepath(&s),
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
                        }))
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
                        ))?
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
            Token::VoidType
            | Token::IntType
            | Token::FloatType
            | Token::DoubleType
            | Token::BoolType
            | Token::StringType
            | Token::Const => {
                // variable declaration
                Ok(Statement::VarDecl(self.parse_var_decl()?))
            }

            Token::Ident(_) => {
                match &self.next_token {
                    Token::Ident(_) => {
                        // struct declaration
                        Ok(Statement::VarDecl(self.parse_var_decl()?))
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
            Token::StructType => Ok(Statement::StructDef(self.parse_struct_def()?)),
            Token::Mod => Ok(Statement::ModuleImport(self.parse_module_import()?)),
            Token::Use => Ok(Statement::Use(self.parse_use_import()?)),
            _ => Ok(Statement::Expression(self.parse_expression()?)),
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

    fn parse_var_decl(&mut self) -> ParseResult<VarDecl> {
        let mut mods: Vec<VarMod> = Vec::new();

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
            Token::Ident(s) => Type::Struct(Reference::new(s)),
            tok => Err(ParseError::Unexpected(
                tok,
                "Expected variable type annotation for variable declaration".to_string(),
            ))?,
        };

        if matches!(self.next_token, Token::Assign) {
            match self.parse_assignment()? {
                Statement::VarAssign(var_assign) => Ok(VarDecl {
                    var_def: VarDef {
                        type_,
                        name: var_assign.name_path.name,
                        mods: Rc::new(mods),
                    },
                    expr: Some(var_assign.expr),
                }),
                _ => Err(ParseError::Unknown),
            }
        } else {
            match self.eat(&Any)? {
                Token::Ident(s) => Ok(VarDecl {
                    var_def: VarDef {
                        type_,
                        name: Reference::new(s),
                        mods: Rc::new(mods),
                    },
                    expr: None,
                }),
                tok => Err(ParseError::Unexpected(
                    tok,
                    "Expected identifier for variable name".to_string(),
                )),
            }
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
        let mut args: Vec<VarDef> = Vec::new();

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
                    Token::Ident(s) => Type::Struct(Reference::new(s)),
                    tok => {
                        return Err(ParseError::Unexpected(
                            tok,
                            "Expected type for function argument".to_string(),
                        ));
                    }
                };

                let name = Reference::new(match self.eat(&Any)? {
                    Token::Ident(s) => s,
                    tok => {
                        return Err(ParseError::Unexpected(
                            tok,
                            "Expected identifier for function argument name".to_string(),
                        ));
                    }
                });

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

        let body = self.parse_block()?;

        Ok(FnDef {
            name: Reference::new(name),
            args,
            body,
            mods: Rc::new(mods),
        })
    }

    fn parse_struct_def(&mut self) -> ParseResult<StructDef> {
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

            let name = match self.eat(&Any)? {
                Token::Ident(s) => s,
                tok => {
                    return Err(ParseError::Unexpected(
                        tok,
                        "Expected identifier for struct field name".to_string(),
                    ));
                }
            };

            map.insert(name, type_);

            self.eat(&Token::Semicolon)?;
        }

        self.eat(&Token::RBrace)?;

        Ok(StructDef {
            name: Reference::new(struct_name),
            map,
        })
    }

    pub fn parse(&mut self) -> ParseResult<Block> {
        let mut statements = Vec::new();
        while !matches!(self.curr_token, Token::Eof) {
            let statement = self.parse_statement()?;

            if !matches!(
                statement,
                Statement::FnDef(_)
                    | Statement::If(_)
                    | Statement::For(_)
                    | Statement::While(_)
                    | Statement::StructDef(_)
            ) {
                self.eat(&Token::Semicolon)?;
            }

            statements.push(StatementBlock::Statement(statement));
        }
        self.eat(&Token::Eof)?;

        Ok(Block { statements })
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
    use crate::front::lexical::lexer::Lexer;
    use crate::front::lexical::lexer_string_reader::StringReader;

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
                name: Reference::new("main".to_string()),
                args: Vec::new(),
                body: Block {
                    statements: vec![StatementBlock::Statement(Statement::Return(Box::from(
                        Expression::AtomicExpression(AtomicExpression::Literal(LiteralValue::Int(
                            0
                        )))
                    )))],
                },
                mods: Rc::new(Vec::new()),
            }))
        );
    }

    #[test]
    fn variable_declarations_test() {
        let statement = "const int a = 0; const int b = 1; int c = 2; float d = 3.0; double e = 4.0; bool f = true; string g = \"hello\";";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let block = parser.parse().unwrap();

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
        let statement = "A a = { a: 0, b: 1, c: 2 }; B b = { a: 0, b: \"hello\", c: 2.54 };";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let block = parser.parse().unwrap();

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
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let block = parser.parse().unwrap();

        assert_eq!(block.statements.len(), 7);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::VarAssign(VarAssign {
                name_path: Parser::<Lexer<StringReader>>::string_to_namepath("a"),
                expr: Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                    LiteralValue::Int(0)
                ))),
            }))
        );
        assert_eq!(
            block.statements[1],
            StatementBlock::Statement(Statement::VarAssign(VarAssign {
                name_path: Parser::<Lexer<StringReader>>::string_to_namepath("b"),
                expr: Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                    LiteralValue::Decimal(2.4)
                ))),
            }))
        );
        assert_eq!(
            block.statements[2],
            StatementBlock::Statement(Statement::VarAssign(VarAssign {
                name_path: Parser::<Lexer<StringReader>>::string_to_namepath("a"),
                expr: Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer<StringReader>>::string_to_namepath("a")
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
                name_path: Parser::<Lexer<StringReader>>::string_to_namepath("a"),
                expr: Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer<StringReader>>::string_to_namepath("a")
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
                name_path: Parser::<Lexer<StringReader>>::string_to_namepath("a"),
                expr: Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer<StringReader>>::string_to_namepath("a")
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
                name_path: Parser::<Lexer<StringReader>>::string_to_namepath("a"),
                expr: Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer<StringReader>>::string_to_namepath("a")
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
                name_path: Parser::<Lexer<StringReader>>::string_to_namepath("a"),
                expr: Box::from(Expression::Binary(
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer<StringReader>>::string_to_namepath("a")
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
            StatementBlock::Statement(Statement::VarAssign(VarAssign {
                name_path: Parser::<Lexer<StringReader>>::string_to_namepath("a"),
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
        let statement = "fn add(int a, B b) { return a + b; }";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let block = parser.parse().unwrap();

        assert_eq!(block.statements.len(), 1);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::FnDef(FnDef {
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
                    statements: vec![StatementBlock::Statement(Statement::Return(Box::from(
                        Expression::Binary(
                            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                                Parser::<Lexer<StringReader>>::string_to_namepath("a")
                            ))),
                            BinOp::Add,
                            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                                Parser::<Lexer<StringReader>>::string_to_namepath("b")
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
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let block = parser.parse().unwrap();

        assert_eq!(block.statements.len(), 1);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::Expression(Box::from(
                Expression::AtomicExpression(AtomicExpression::FnCall(Box::from(FnCall {
                    name_path: Parser::<Lexer<StringReader>>::string_to_namepath("add"),
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
                        Parser::<Lexer<StringReader>>::string_to_namepath("a")
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
                        Parser::<Lexer<StringReader>>::string_to_namepath("a")
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
                            Parser::<Lexer<StringReader>>::string_to_namepath("a")
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
                        Parser::<Lexer<StringReader>>::string_to_namepath("a")
                    ))),
                    BinOp::Lt,
                    Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(10)
                    ))),
                )),
                body: Box::from(Block {
                    statements: vec![StatementBlock::Statement(Statement::VarAssign(VarAssign {
                        name_path: Parser::<Lexer<StringReader>>::string_to_namepath("a"),
                        expr: Box::from(Expression::Binary(
                            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                                Parser::<Lexer<StringReader>>::string_to_namepath("a")
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
                        Parser::<Lexer<StringReader>>::string_to_namepath("i")
                    ))),
                    BinOp::Lt,
                    Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                        LiteralValue::Int(10)
                    ))),
                ))),
                step: Some(Box::from(Statement::VarAssign(VarAssign {
                    name_path: Parser::<Lexer<StringReader>>::string_to_namepath("i"),
                    expr: Box::from(Expression::Binary(
                        Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                            Parser::<Lexer<StringReader>>::string_to_namepath("i")
                        ))),
                        BinOp::Add,
                        Box::from(Expression::AtomicExpression(AtomicExpression::Literal(
                            LiteralValue::Int(1)
                        ))),
                    )),
                }))),
                body: Block {
                    statements: vec![StatementBlock::Statement(Statement::VarAssign(VarAssign {
                        name_path: Parser::<Lexer<StringReader>>::string_to_namepath("a"),
                        expr: Box::from(Expression::Binary(
                            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                                Parser::<Lexer<StringReader>>::string_to_namepath("a")
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
                                    AtomicExpression::Variable(
                                        Parser::<Lexer<StringReader>>::string_to_namepath("a"),
                                    )
                                )),
                                BinOp::Add,
                                Box::from(Expression::AtomicExpression(
                                    AtomicExpression::Variable(
                                        Parser::<Lexer<StringReader>>::string_to_namepath("b"),
                                    )
                                ))
                            )),
                            BinOp::Add,
                            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                                Parser::<Lexer<StringReader>>::string_to_namepath("c"),
                            )))
                        )),
                        BinOp::Add,
                        Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                            Parser::<Lexer<StringReader>>::string_to_namepath("d"),
                        )))
                    )),
                    BinOp::Sub,
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer<StringReader>>::string_to_namepath("e"),
                    )))
                )),
                BinOp::Sub,
                Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                    Parser::<Lexer<StringReader>>::string_to_namepath("f"),
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

        let b_times_c = Box::from(Expression::Binary(
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer<StringReader>>::string_to_namepath("b"),
            ))),
            BinOp::Mul,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer<StringReader>>::string_to_namepath("c"),
            ))),
        ));

        let a_plus = Box::from(Expression::Binary(
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer<StringReader>>::string_to_namepath("a"),
            ))),
            BinOp::Add,
            b_times_c,
        ));

        let d_div_e = Box::from(Expression::Binary(
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer<StringReader>>::string_to_namepath("d"),
            ))),
            BinOp::Div,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer<StringReader>>::string_to_namepath("e"),
            ))),
        ));

        let mod_f = Box::from(Expression::Binary(
            d_div_e,
            BinOp::Mod,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer<StringReader>>::string_to_namepath("f"),
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
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
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
                Parser::<Lexer<StringReader>>::string_to_namepath("c"),
            ))),
            BinOp::Sub,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer<StringReader>>::string_to_namepath("d"),
            ))),
        ));

        let b_times_c_minus_d = Box::from(Expression::Binary(
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer<StringReader>>::string_to_namepath("b"),
            ))),
            BinOp::Mul,
            c_minus_d,
        ));

        let b_times_c_minus_d_div_e = Box::from(Expression::Binary(
            b_times_c_minus_d,
            BinOp::Div,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer<StringReader>>::string_to_namepath("e"),
            ))),
        ));

        let b_times_c_minus_d_div_e_mod_f = Box::from(Expression::Binary(
            b_times_c_minus_d_div_e,
            BinOp::Mod,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer<StringReader>>::string_to_namepath("f"),
            ))),
        ));

        let a_plus_b_times_c_minus_d_div_e_mod_f = Box::from(Expression::Binary(
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer<StringReader>>::string_to_namepath("a"),
            ))),
            BinOp::Add,
            b_times_c_minus_d_div_e_mod_f,
        ));

        assert_eq!(expr, a_plus_b_times_c_minus_d_div_e_mod_f);
    }

    #[test]
    fn complex_expression_order_test() {
        let statement = "a == b && c || d != e";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
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
                Parser::<Lexer<StringReader>>::string_to_namepath("a"),
            ))),
            BinOp::Eq,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer<StringReader>>::string_to_namepath("b"),
            ))),
        ));

        let a_eq_b_and_c = Box::from(Expression::Binary(
            a_eq_b,
            BinOp::And,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer<StringReader>>::string_to_namepath("c"),
            ))),
        ));

        let d_neq_e = Box::from(Expression::Binary(
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer<StringReader>>::string_to_namepath("d"),
            ))),
            BinOp::Neq,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer<StringReader>>::string_to_namepath("e"),
            ))),
        ));

        let a_eq_b_and_c_or_d_neq_e =
            Box::from(Expression::Binary(a_eq_b_and_c, BinOp::Or, d_neq_e));

        assert_eq!(expr, a_eq_b_and_c_or_d_neq_e);
    }

    #[test]
    fn expression_deref_test() {
        let statement = "*a + b * *d";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
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
                Parser::<Lexer<StringReader>>::string_to_namepath("a"),
            ))),
        ));

        let deref_d = Box::from(Expression::Unary(
            UnOp::Deref,
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer<StringReader>>::string_to_namepath("d"),
            ))),
        ));

        let b_times_deref_d = Box::from(Expression::Binary(
            Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                Parser::<Lexer<StringReader>>::string_to_namepath("b"),
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
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let expr = parser.parse_expression().unwrap();

        assert_eq!(
            expr,
            Box::from(Expression::Binary(
                Box::from(Expression::Unary(
                    UnOp::PostInc,
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer<StringReader>>::string_to_namepath("a")
                    )))
                )),
                BinOp::Add,
                Box::from(Expression::Unary(
                    UnOp::PreInc,
                    Box::from(Expression::AtomicExpression(AtomicExpression::Variable(
                        Parser::<Lexer<StringReader>>::string_to_namepath("b")
                    )))
                )),
            ))
        );
    }

    #[test]
    fn struct_definition_test() {
        let statement = "struct A { int a; float b; double c; C d; }";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let block = parser.parse().unwrap();

        assert_eq!(block.statements.len(), 1);
        assert_eq!(
            block.statements[0],
            StatementBlock::Statement(Statement::StructDef(StructDef {
                name: Reference::new("A".to_string()),
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
                }
            }))
        );
    }

    #[test]
    fn multiple_declaration_test() {
        let statement = "int a; fn main(int a) { int a; a + 1; return 0; }";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let block = parser.parse().unwrap();

        assert_eq!(block.statements.len(), 2);
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
            block.statements[1],
            StatementBlock::Statement(Statement::FnDef(FnDef {
                name: Reference::new("main".to_string()),
                args: vec![VarDef {
                    mods: Rc::new(Vec::new()),
                    type_: Type::Int,
                    name: Reference::new("a".to_string()),
                }],
                body: Block {
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
                                        Parser::<Lexer<StringReader>>::string_to_namepath("a")
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
