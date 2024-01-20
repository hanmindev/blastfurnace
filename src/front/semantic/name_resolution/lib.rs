#[cfg(test)]
mod tests {
    use crate::front::lexical::lexer::Lexer;
    use crate::front::lexical::lexer_string_reader::StringReader;
    use crate::front::semantic::name_resolution::resolver::Resolvable;
    use crate::front::semantic::name_resolution::scope_table::ScopeTable;
    use crate::front::syntax::ast_types::{
        AtomicExpression, Definition, Expression, Reference, Statement, StatementBlock, Type,
    };
    use crate::front::syntax::parser::Parser;
    use std::rc::Rc;

    #[test]
    fn simple_scope() {
        let mut scope_table = ScopeTable::new();

        let statement =
            "pub let a: int; fn main(a: int, b: int) -> int { a + 1; let a: int = a; return 0; }";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let mut block = parser.parse().unwrap();

        block.resolve(&mut scope_table).unwrap();

        match &block.definitions[0] {
            Definition::VarDecl(var_decl) => {
                assert_eq!(
                    var_decl.var_def.name.clone(),
                    Reference {
                        raw: "a".to_string(),
                        resolved: Some(Rc::new("0_a".to_string())),
                    }
                );
            }
            _ => {
                panic!("Expected VarDecl");
            }
        };

        match &block.definitions[1] {
            Definition::FnDef(fn_def) => {
                assert_eq!(
                    fn_def.name.clone(),
                    Reference {
                        raw: "main".to_string(),
                        resolved: Some(Rc::new("0_main".to_string())),
                    }
                );

                assert_eq!(
                    fn_def.args[0].name.clone(),
                    Reference {
                        raw: "a".to_string(),
                        resolved: Some(Rc::new("1_a".to_string())),
                    }
                );

                assert_eq!(
                    fn_def.args[1].name.clone(),
                    Reference {
                        raw: "b".to_string(),
                        resolved: Some(Rc::new("0_b".to_string())),
                    }
                );

                match &fn_def.body.statements[0] {
                    StatementBlock::Statement(Statement::Expression(bx)) => match bx.as_ref() {
                        Expression::Binary(e0, b, e1) => match e0.as_ref() {
                            Expression::AtomicExpression(AtomicExpression::Variable(name_path)) => {
                                assert_eq!(
                                    name_path.name.clone(),
                                    Reference {
                                        raw: "a".to_string(),
                                        resolved: Some(Rc::new("1_a".to_string())),
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
                                raw: "a".to_string(),
                                resolved: Some(Rc::new("2_a".to_string())),
                            }
                        );
                        match var_decl.expr.as_ref().unwrap().as_ref() {
                            Expression::AtomicExpression(AtomicExpression::Variable(name_path)) => {
                                assert_eq!(
                                    name_path.name.clone(),
                                    Reference {
                                        raw: "a".to_string(),
                                        resolved: Some(Rc::new("1_a".to_string())),
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

    #[test]
    fn struct_defn_after() {
        let mut scope_table = ScopeTable::new();

        let statement = "pub let a: A; pub let b: A; pub let c: A; pub struct A { }";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let mut block = parser.parse().unwrap();

        block.resolve(&mut scope_table).unwrap();

        match &block.definitions[0] {
            Definition::VarDecl(var_decl) => {
                assert_eq!(
                    var_decl.var_def.type_,
                    Type::Struct(Reference {
                        raw: "A".to_string(),
                        resolved: Some(Rc::new("0_A".to_string())),
                    })
                );

                assert_eq!(
                    var_decl.var_def.name.clone(),
                    Reference {
                        raw: "a".to_string(),
                        resolved: Some(Rc::new("0_a".to_string())),
                    }
                );
            }
            _ => {
                panic!("Expected VarDecl");
            }
        };
        match &block.definitions[1] {
            Definition::VarDecl(var_decl) => {
                assert_eq!(
                    var_decl.var_def.type_,
                    Type::Struct(Reference {
                        raw: "A".to_string(),
                        resolved: Some(Rc::new("0_A".to_string())),
                    })
                );

                assert_eq!(
                    var_decl.var_def.name.clone(),
                    Reference {
                        raw: "b".to_string(),
                        resolved: Some(Rc::new("0_b".to_string())),
                    }
                );
            }
            _ => {
                panic!("Expected VarDecl");
            }
        };
        match &block.definitions[2] {
            Definition::VarDecl(var_decl) => {
                assert_eq!(
                    var_decl.var_def.type_,
                    Type::Struct(Reference {
                        raw: "A".to_string(),
                        resolved: Some(Rc::new("0_A".to_string())),
                    })
                );

                assert_eq!(
                    var_decl.var_def.name.clone(),
                    Reference {
                        raw: "c".to_string(),
                        resolved: Some(Rc::new("0_c".to_string())),
                    }
                );
            }
            _ => {
                panic!("Expected VarDecl");
            }
        };

        match &block.definitions[3] {
            Definition::StructDef(struct_def) => {
                assert_eq!(
                    struct_def.type_name.clone(),
                    Reference {
                        raw: "A".to_string(),
                        resolved: Some(Rc::new("0_A".to_string())),
                    }
                );
            }
            _ => {
                panic!("Expected StructDef");
            }
        };
    }

    #[test]
    fn struct_defn_rec() {
        let mut scope_table = ScopeTable::new();

        let statement = "struct A { b: B } struct B { a: A }";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let mut block = parser.parse_local().unwrap();

        block.resolve(&mut scope_table).unwrap();

        match &block.definitions[0] {
            Definition::StructDef(struct_def) => {
                assert_eq!(
                    struct_def.type_name.clone(),
                    Reference {
                        raw: "A".to_string(),
                        resolved: Some(Rc::new("0_A".to_string())),
                    }
                );
                match &struct_def.map.get("b").unwrap() {
                    Type::Struct(struct_name) => {
                        assert_eq!(
                            struct_name.clone(),
                            Reference {
                                raw: "B".to_string(),
                                resolved: Some(Rc::new("0_B".to_string())),
                            }
                        );
                    }
                    _ => {
                        panic!("Expected Struct");
                    }
                }
            }
            _ => {
                panic!("Expected StructDef");
            }
        }

        match &block.definitions[1] {
            Definition::StructDef(struct_def) => {
                assert_eq!(
                    struct_def.type_name.clone(),
                    Reference {
                        raw: "B".to_string(),
                        resolved: Some(Rc::new("0_B".to_string())),
                    }
                );
                match &struct_def.map.get("a").unwrap() {
                    Type::Struct(struct_name) => {
                        assert_eq!(
                            struct_name.clone(),
                            Reference {
                                raw: "A".to_string(),
                                resolved: Some(Rc::new("0_A".to_string())),
                            }
                        );
                    }
                    _ => {
                        panic!("Expected Struct");
                    }
                }
            }
            _ => {
                panic!("Expected StructDef");
            }
        }
    }

    #[test]
    fn struct_defn_rec_scope() {
        let mut scope_table = ScopeTable::new();

        let statement = "fn main() { let a: A; let b: B; } struct A { b: B } struct B { a: A }";
        let lexer = Lexer::new(StringReader::new(statement.to_string()));
        let mut parser = Parser::new(lexer);

        let mut block = parser.parse_local().unwrap();

        block.resolve(&mut scope_table).unwrap();
        match &block.definitions[2] {
            Definition::FnDef(fn_def) => {
                assert_eq!(
                    fn_def.name.clone(),
                    Reference {
                        raw: "main".to_string(),
                        resolved: Some(Rc::new("0_main".to_string())),
                    }
                );

                match &fn_def.body.statements[0] {
                    StatementBlock::Statement(Statement::VarDecl(var_decl)) => {
                        assert_eq!(
                            var_decl.var_def.type_,
                            Type::Struct(Reference {
                                raw: "A".to_string(),
                                resolved: Some(Rc::new("0_A".to_string())),
                            })
                        );

                        assert_eq!(
                            var_decl.var_def.name.clone(),
                            Reference {
                                raw: "a".to_string(),
                                resolved: Some(Rc::new("0_a".to_string())),
                            }
                        );
                    }
                    _ => {
                        panic!("Expected VarDecl");
                    }
                };
                match &fn_def.body.statements[1] {
                    StatementBlock::Statement(Statement::VarDecl(var_decl)) => {
                        assert_eq!(
                            var_decl.var_def.type_,
                            Type::Struct(Reference {
                                raw: "B".to_string(),
                                resolved: Some(Rc::new("0_B".to_string())),
                            })
                        );

                        assert_eq!(
                            var_decl.var_def.name.clone(),
                            Reference {
                                raw: "b".to_string(),
                                resolved: Some(Rc::new("0_b".to_string())),
                            }
                        );
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

        match &block.definitions[0] {
            Definition::StructDef(struct_def) => {
                assert_eq!(
                    struct_def.type_name.clone(),
                    Reference {
                        raw: "A".to_string(),
                        resolved: Some(Rc::new("0_A".to_string())),
                    }
                );
                match &struct_def.map.get("b").unwrap() {
                    Type::Struct(struct_name) => {
                        assert_eq!(
                            struct_name.clone(),
                            Reference {
                                raw: "B".to_string(),
                                resolved: Some(Rc::new("0_B".to_string())),
                            }
                        );
                    }
                    _ => {
                        panic!("Expected Struct");
                    }
                }
            }
            _ => {
                panic!("Expected StructDef");
            }
        }

        match &block.definitions[1] {
            Definition::StructDef(struct_def) => {
                assert_eq!(
                    struct_def.type_name.clone(),
                    Reference {
                        raw: "B".to_string(),
                        resolved: Some(Rc::new("0_B".to_string())),
                    }
                );
                match &struct_def.map.get("a").unwrap() {
                    Type::Struct(struct_name) => {
                        assert_eq!(
                            struct_name.clone(),
                            Reference {
                                raw: "A".to_string(),
                                resolved: Some(Rc::new("0_A".to_string())),
                            }
                        );
                    }
                    _ => {
                        panic!("Expected Struct");
                    }
                }
            }
            _ => {
                panic!("Expected StructDef");
            }
        }
    }
}
