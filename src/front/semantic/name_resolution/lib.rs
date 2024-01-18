#[cfg(test)]
mod tests {
    use crate::front::lexical::lexer::Lexer;
    use crate::front::lexical::lexer_string_reader::StringReader;
    use crate::front::semantic::name_resolution::resolver::Resolvable;
    use crate::front::semantic::name_resolution::scope_table::{ScopeTable, SymbolInfo};
    use crate::front::syntax::ast_types::{
        AtomicExpression, Expression, Reference, Statement, StatementBlock, Type, VarDecl,
    };
    use crate::front::syntax::parser::Parser;

    #[test]
    fn simple_scope() {
        let mut scope_table = ScopeTable::new();

        let statement = "int a; fn main(int a, int b) { a + 1; int a = a; return 0; }";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let mut block = parser.parse().unwrap();

        block.resolve(&mut scope_table).unwrap();

        match &block.statements[0] {
            StatementBlock::Statement(Statement::VarDecl(var_decl)) => {
                assert_eq!(
                    var_decl.var_def.name.clone(),
                    Reference {
                        raw: Some("a".to_string()),
                        resolved: Some("a".to_string()),
                    }
                );
            }
            _ => {
                panic!("Expected VarDecl");
            }
        };

        match &block.statements[1] {
            StatementBlock::Statement(Statement::FnDef(fn_def)) => {
                assert_eq!(
                    fn_def.name.clone(),
                    Reference {
                        raw: Some("main".to_string()),
                        resolved: Some("main".to_string()),
                    }
                );

                assert_eq!(
                    fn_def.args[0].name.clone(),
                    Reference {
                        raw: Some("a".to_string()),
                        resolved: Some("1_a".to_string()),
                    }
                );

                assert_eq!(
                    fn_def.args[1].name.clone(),
                    Reference {
                        raw: Some("b".to_string()),
                        resolved: Some("b".to_string()),
                    }
                );

                match &fn_def.body.statements[0] {
                    StatementBlock::Statement(Statement::Expression(bx)) => match bx.as_ref() {
                        Expression::Binary(e0, b, e1) => match e0.as_ref() {
                            Expression::AtomicExpression(AtomicExpression::Variable(name_path)) => {
                                assert_eq!(
                                    name_path.name.clone(),
                                    Reference {
                                        raw: Some("a".to_string()),
                                        resolved: Some("1_a".to_string()),
                                    }
                                );
                            }
                            _ => {}
                        },
                        _ => {}
                    },
                    _ => {
                        panic!("Expected Expression");
                    }
                };
                match &fn_def.body.statements[1] {
                    StatementBlock::Statement(Statement::VarDecl(var_decl)) => {
                        assert_eq!(
                            var_decl.var_def.name.clone(),
                            Reference {
                                raw: Some("a".to_string()),
                                resolved: Some("2_a".to_string()),
                            }
                        );
                        match var_decl.expr.as_ref().unwrap().as_ref() {
                            Expression::AtomicExpression(AtomicExpression::Variable(name_path)) => {
                                assert_eq!(
                                    name_path.name.clone(),
                                    Reference {
                                        raw: Some("a".to_string()),
                                        resolved: Some("1_a".to_string()),
                                    }
                                );
                            }
                            _ => {
                                panic!("Expected Expression");
                            }
                        }
                    }
                    _ => {
                        panic!("Expected VarDecl");
                    }
                };
            }
            _ => {
                panic!("Expected FnDef");
            }
        }
    }
}
