use crate::front::ast_retriever::name_resolution::scope_table::{ScopeTable, SymbolType};
use crate::front::ast_types::{AtomicExpression, Block, Compound, CompoundValue, Definition, Else, Expression, ExpressionEnum, FnCall, FnDef, For, If, LiteralValue, Module, NamePath, Reference, Statement, StructDef, Type, Use, VarAssign, VarDecl, VarDef, While};
use crate::front::ast_types::visitor::{ASTNodeEnum, Visitable, Visitor};

#[derive(Debug, PartialEq)]
pub enum ResolverError {
    UndefinedVariable(String),
    Redefinition(String),
}

pub type ResolveResult<T> = Result<T, ResolverError>;

fn name_path_lookup(scope_table: &ScopeTable, name_path: &mut NamePath) -> ResolveResult<()> {
    match scope_table.scope_lookup(&name_path.name.raw, SymbolType::Var) {
        Some(name) => {
            name_path.name.module_resolved = Some(name.clone());
            Ok(())
        }
        None => Err(ResolverError::UndefinedVariable(name_path.name.raw.clone())),
    }
}

impl Visitor<ResolverError> for ScopeTable {
    fn apply(&mut self, ast_node: &mut ASTNodeEnum) -> ResolveResult<bool> {
        match ast_node {
            ASTNodeEnum::NamePath(name_path) => {
                panic!("NamePath should not be visited directly")
            }
            ASTNodeEnum::Reference(reference) => {
                panic!("Reference should not be visited directly")
            }
            ASTNodeEnum::AtomicExpression(atomic_expression) => {
                match atomic_expression {
                    AtomicExpression::Variable(x) => {
                        match self.scope_lookup(&x.name.raw, SymbolType::Var) {
                            Some(name) => {
                                x.name.module_resolved = Some(name.clone());
                            }
                            None => Err(ResolverError::UndefinedVariable(x.name.raw.clone()))?,
                        }
                    }
                    _=> { return Ok(true); }
                }
            }

            ASTNodeEnum::VarDef(var_def) => {
                if let Some(Type::Struct(struct_name)) = &mut var_def.type_ {
                    struct_name.module_resolved =
                        Some(self.scope_lookup_force(&struct_name.raw, SymbolType::Struct));
                }

                var_def.name.module_resolved = Some(self.scope_bind(&var_def.name.raw, SymbolType::Var)?);
            }
            ASTNodeEnum::VarAssign(var_assign) => {
                var_assign.expr.visit(self)?;
                name_path_lookup(&self, &mut var_assign.name_path)?;
            }
            ASTNodeEnum::FnDef(fn_def) => {
                self.scope_enter();

                fn_def.name.module_resolved = Some(self.scope_bind(&fn_def.name.raw, SymbolType::Fn)?);
                for arg in &mut fn_def.args {
                    arg.visit(self)?;
                }

                fn_def.body.visit(self)?;
                self.scope_exit();
            }
            ASTNodeEnum::FnCall(fn_call) => {
                match self.scope_lookup(&fn_call.name.raw, SymbolType::Fn) {
                    Some(name) => {
                        fn_call.name.module_resolved = Some(name.clone());
                    }
                    None => return Err(ResolverError::UndefinedVariable(fn_call.name.raw.clone())),
                }

                for arg in &mut fn_call.args {
                    arg.visit(self)?;
                }
            }
            ASTNodeEnum::Block(block) => {
                self.scope_enter();
                for definitions in &mut block.definitions {
                    definitions.visit(self)?;
                }
                for statement in &mut block.statements {
                    statement.visit(self)?;
                }
                self.scope_exit();
            }
            ASTNodeEnum::Module(module) => {
                self.scope_enter();
                for use_ in &mut module.uses {
                    use_.visit(self)?;
                }

                for definitions in &mut module.public_definitions {
                    definitions.visit(self)?;
                }

                module.block.visit(self)?;
                self.scope_exit();
            }
            ASTNodeEnum::StructDef(struct_def) => {
                struct_def.type_name.module_resolved =
                    Some(self.scope_bind(&struct_def.type_name.raw, SymbolType::Struct)?);

                for v in &mut struct_def.map.values_mut() {
                    if let Type::Struct(struct_name) = v {
                        struct_name.module_resolved =
                            Some(self.scope_lookup_force(&struct_name.raw, SymbolType::Struct));
                    }
                }
            }
            ASTNodeEnum::Use(use_) => {
                for element in &mut use_.elements {
                    let struct_name =
                        self.scope_bind(&element.imported_name.raw, SymbolType::Struct)?;
                    let fn_name = self.scope_bind(&element.imported_name.raw, SymbolType::Fn)?;
                    let var_name = self.scope_bind(&element.imported_name.raw, SymbolType::Var)?;

                    if struct_name == fn_name && fn_name == var_name {
                        element.imported_name.module_resolved = Some(struct_name);
                    } else {
                        return Err(ResolverError::Redefinition(
                            element.imported_name.raw.clone(),
                        ));
                    }
                }
            }
            _ => { return Ok(true); }
        };
        return Ok(false);
    }
}