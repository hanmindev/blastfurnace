#[cfg(test)]
mod tests {
    use crate::front::name_resolution::resolvers::Resolvable;
    use crate::front::name_resolution::resolvers::ResolverError::Redefinition;
    use crate::front::name_resolution::scope_table::ScopeTable;
    use crate::front::ast_types::{
        AtomicExpression, Definition, Expression, Reference, Statement, StatementBlock, Type,
    };
    use std::rc::Rc;
    use crate::front::ast_retriever::string_to_module;

    #[test]
    fn simple_scope() {
        let mut scope_table = ScopeTable::new();

        let statement =
            "pub let a: int; pub fn main(a: int, b: int) -> int { a + 1; let a: int = a; return 0; }";
        let mut module = string_to_module(statement).unwrap();

        module.resolve_name(&mut scope_table).unwrap();

        match &module.public_definitions[0] {
            Definition::VarDecl(var_decl) => {
                assert_eq!(
                    var_decl.var_def.name.clone(),
                    Reference {
                        raw: "a".to_string(),
                        module_resolved: Some(Rc::new("0_a".to_string())),
                        global_resolved: None
                    }
                );
            }
            _ => {
                panic!("Expected VarDecl");
            }
        };

        match &module.public_definitions[1] {
            Definition::FnDef(fn_def) => {
                assert_eq!(
                    fn_def.name.clone(),
                    Reference {
                        raw: "main".to_string(),
                        module_resolved: Some(Rc::new("0_main".to_string())),
                        global_resolved: None
                    }
                );

                assert_eq!(
                    fn_def.args[0].name.clone(),
                    Reference {
                        raw: "a".to_string(),
                        module_resolved: Some(Rc::new("1_a".to_string())),
                        global_resolved: None
                    }
                );

                assert_eq!(
                    fn_def.args[1].name.clone(),
                    Reference {
                        raw: "b".to_string(),
                        module_resolved: Some(Rc::new("0_b".to_string())),
                        global_resolved: None
                    }
                );

                match &fn_def.body.as_ref().unwrap().statements[0] {
                    StatementBlock::Statement(Statement::Expression(bx)) => match bx.as_ref() {
                        Expression::Binary(e0, _, _) => match e0.as_ref() {
                            Expression::AtomicExpression(AtomicExpression::Variable(name_path)) => {
                                assert_eq!(
                                    name_path.name.clone(),
                                    Reference {
                                        raw: "a".to_string(),
                                        module_resolved: Some(Rc::new("1_a".to_string())),
                                        global_resolved: None
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
                match &fn_def.body.as_ref().unwrap().statements[1] {
                    StatementBlock::Statement(Statement::VarDecl(var_decl)) => {
                        assert_eq!(
                            var_decl.var_def.name.clone(),
                            Reference {
                                raw: "a".to_string(),
                                module_resolved: Some(Rc::new("2_a".to_string())),
                                global_resolved: None
                            }
                        );
                        match var_decl.expr.as_ref().unwrap().as_ref() {
                            Expression::AtomicExpression(AtomicExpression::Variable(name_path)) => {
                                assert_eq!(
                                    name_path.name.clone(),
                                    Reference {
                                        raw: "a".to_string(),
                                        module_resolved: Some(Rc::new("1_a".to_string())),
                                        global_resolved: None
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
        let mut module = string_to_module(statement).unwrap();

        module.resolve_name(&mut scope_table).unwrap();

        match &module.public_definitions[0] {
            Definition::VarDecl(var_decl) => {
                assert_eq!(
                    var_decl.var_def.type_,
                    Type::Struct(Reference {
                        raw: "A".to_string(),
                        module_resolved: Some(Rc::new("0_A".to_string())),
                        global_resolved: None
                    })
                );

                assert_eq!(
                    var_decl.var_def.name.clone(),
                    Reference {
                        raw: "a".to_string(),
                        module_resolved: Some(Rc::new("0_a".to_string())),
                        global_resolved: None
                    }
                );
            }
            _ => {
                panic!("Expected VarDecl");
            }
        };
        match &module.public_definitions[1] {
            Definition::VarDecl(var_decl) => {
                assert_eq!(
                    var_decl.var_def.type_,
                    Type::Struct(Reference {
                        raw: "A".to_string(),
                        module_resolved: Some(Rc::new("0_A".to_string())),
                        global_resolved: None
                    })
                );

                assert_eq!(
                    var_decl.var_def.name.clone(),
                    Reference {
                        raw: "b".to_string(),
                        module_resolved: Some(Rc::new("0_b".to_string())),
                        global_resolved: None
                    }
                );
            }
            _ => {
                panic!("Expected VarDecl");
            }
        };
        match &module.public_definitions[2] {
            Definition::VarDecl(var_decl) => {
                assert_eq!(
                    var_decl.var_def.type_,
                    Type::Struct(Reference {
                        raw: "A".to_string(),
                        module_resolved: Some(Rc::new("0_A".to_string())),
                        global_resolved: None
                    })
                );

                assert_eq!(
                    var_decl.var_def.name.clone(),
                    Reference {
                        raw: "c".to_string(),
                        module_resolved: Some(Rc::new("0_c".to_string())),
                        global_resolved: None
                    }
                );
            }
            _ => {
                panic!("Expected VarDecl");
            }
        };

        match &module.public_definitions[3] {
            Definition::StructDef(struct_def) => {
                assert_eq!(
                    struct_def.type_name.clone(),
                    Reference {
                        raw: "A".to_string(),
                        module_resolved: Some(Rc::new("0_A".to_string())),
                        global_resolved: None
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
        let mut block = string_to_module(statement).unwrap().block;

        block.resolve_name(&mut scope_table).unwrap();

        match &block.definitions[0] {
            Definition::StructDef(struct_def) => {
                assert_eq!(
                    struct_def.type_name.clone(),
                    Reference {
                        raw: "A".to_string(),
                        module_resolved: Some(Rc::new("0_A".to_string())),
                        global_resolved: None
                    }
                );
                match &struct_def.map.get("b").unwrap() {
                    Type::Struct(struct_name) => {
                        assert_eq!(
                            struct_name.clone(),
                            Reference {
                                raw: "B".to_string(),
                                module_resolved: Some(Rc::new("0_B".to_string())),
                                global_resolved: None
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
                        module_resolved: Some(Rc::new("0_B".to_string())),
                        global_resolved: None
                    }
                );
                match &struct_def.map.get("a").unwrap() {
                    Type::Struct(struct_name) => {
                        assert_eq!(
                            struct_name.clone(),
                            Reference {
                                raw: "A".to_string(),
                                module_resolved: Some(Rc::new("0_A".to_string())),
                                global_resolved: None
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
    fn struct_defn_dupe() {
        let mut scope_table = ScopeTable::new();

        let statement = "struct A { b: int } struct A { a: int }";
        let mut block = string_to_module(statement).unwrap().block;

        if let Err(error) = block.resolve_name(&mut scope_table) {
            assert_eq!(error, Redefinition("A".to_string()));
        } else {
            panic!("Expected error");
        }
    }

    #[test]
    fn struct_defn_rec_scope() {
        let mut scope_table = ScopeTable::new();

        let statement = "fn main() { let a: A; let b: B; } struct A { b: B } struct B { a: A }";
        let mut block = string_to_module(statement).unwrap().block;

        block.resolve_name(&mut scope_table).unwrap();
        match &block.definitions[2] {
            Definition::FnDef(fn_def) => {
                assert_eq!(
                    fn_def.name.clone(),
                    Reference {
                        raw: "main".to_string(),
                        module_resolved: Some(Rc::new("0_main".to_string())),
                        global_resolved: None
                    }
                );

                match &fn_def.body.as_ref().unwrap().statements[0] {
                    StatementBlock::Statement(Statement::VarDecl(var_decl)) => {
                        assert_eq!(
                            var_decl.var_def.type_,
                            Type::Struct(Reference {
                                raw: "A".to_string(),
                                module_resolved: Some(Rc::new("0_A".to_string())),
                                global_resolved: None
                            })
                        );

                        assert_eq!(
                            var_decl.var_def.name.clone(),
                            Reference {
                                raw: "a".to_string(),
                                module_resolved: Some(Rc::new("0_a".to_string())),
                                global_resolved: None
                            }
                        );
                    }
                    _ => {
                        panic!("Expected VarDecl");
                    }
                };
                match &fn_def.body.as_ref().unwrap().statements[1] {
                    StatementBlock::Statement(Statement::VarDecl(var_decl)) => {
                        assert_eq!(
                            var_decl.var_def.type_,
                            Type::Struct(Reference {
                                raw: "B".to_string(),
                                module_resolved: Some(Rc::new("0_B".to_string())),
                                global_resolved: None
                            })
                        );

                        assert_eq!(
                            var_decl.var_def.name.clone(),
                            Reference {
                                raw: "b".to_string(),
                                module_resolved: Some(Rc::new("0_b".to_string())),
                                global_resolved: None
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
                        module_resolved: Some(Rc::new("0_A".to_string())),
                        global_resolved: None
                    }
                );
                match &struct_def.map.get("b").unwrap() {
                    Type::Struct(struct_name) => {
                        assert_eq!(
                            struct_name.clone(),
                            Reference {
                                raw: "B".to_string(),
                                module_resolved: Some(Rc::new("0_B".to_string())),
                                global_resolved: None
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
                        module_resolved: Some(Rc::new("0_B".to_string())),
                        global_resolved: None
                    }
                );
                match &struct_def.map.get("a").unwrap() {
                    Type::Struct(struct_name) => {
                        assert_eq!(
                            struct_name.clone(),
                            Reference {
                                raw: "A".to_string(),
                                module_resolved: Some(Rc::new("0_A".to_string())),
                                global_resolved: None
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
