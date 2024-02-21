use crate::front::ast_types::visitor::{ASTNodeEnum, GenericResolveResult, Visitable, Visitor};
use crate::front::ast_types::{AtomicExpression, ExpressionEnum, GlobalResolvedName, Type};
use crate::front::exporter::export::FrontProgram;
use crate::front::passes::types::type_expression::{
    binop_type_resolver, literal_types, unop_type_resolver, TypeError,
};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum ResolverError {
    TypeError(TypeError),
}

pub type ResolveResult<T> = GenericResolveResult<T, ResolverError>;

impl Visitor<Type, ResolverError> for ResolvedVarDefTable {
    fn apply(&mut self, ast_node: &mut ASTNodeEnum) -> ResolveResult<Type> {
        match ast_node {
            ASTNodeEnum::VarDef(&mut ref mut x) => {
                x.type_ = Some(
                    self.var_types
                        .get(x.name.global_resolved.as_ref().unwrap())
                        .unwrap()
                        .clone(),
                );
            }

            ASTNodeEnum::VarDecl(&mut ref mut x) => {
                x.var_def.visit(self)?;
                if let Some(expr) = &mut x.expr {
                    if &expr.visit(self)?.unwrap() != x.var_def.type_.as_ref().unwrap() {
                        return Err(ResolverError::TypeError(TypeError::MultipleTypes));
                    }
                }
            }

            ASTNodeEnum::VarAssign(&mut ref mut x) => {
                if self
                    .var_types
                    .get(x.name_path.name.global_resolved.as_ref().unwrap())
                    .unwrap()
                    .clone()
                    != x.expr.visit(self)?.unwrap()
                {
                    return Err(ResolverError::TypeError(TypeError::MultipleTypes));
                }
            }

            ASTNodeEnum::Expression(&mut ref mut x) => {
                x.type_ = Some(match &mut x.expr {
                    ExpressionEnum::AtomicExpression(atomic) => match atomic {
                        AtomicExpression::Variable(name_path) => self
                            .var_types
                            .get(name_path.name.global_resolved.as_ref().unwrap())
                            .unwrap()
                            .clone(),
                        AtomicExpression::FnCall(fn_call) => self
                            .var_types
                            .get(fn_call.name.global_resolved.as_ref().unwrap())
                            .unwrap()
                            .clone(),
                        AtomicExpression::Literal(literal) => literal_types(literal),
                    },
                    ExpressionEnum::Unary(unop, x) => {
                        match unop_type_resolver(unop, &x.visit(self)?.unwrap()) {
                            Ok(type_) => type_,
                            Err(type_error) => {
                                return Err(ResolverError::TypeError(type_error));
                            }
                        }
                    }
                    ExpressionEnum::Binary(e0, binop, e1) => {
                        let t0 = e0.visit(self)?.unwrap();
                        let t1 = e1.visit(self)?.unwrap();

                        match binop_type_resolver(binop, &t0, &t1) {
                            Ok(type_) => type_,
                            Err(type_error) => {
                                return Err(ResolverError::TypeError(type_error));
                            }
                        }
                    }
                });
                return Ok((false, x.type_.clone()));
            }

            ASTNodeEnum::If(_)
            | ASTNodeEnum::Else(_)
            | ASTNodeEnum::While(_)
            | ASTNodeEnum::For(_)
            | ASTNodeEnum::Statement(_)
            | ASTNodeEnum::Block(_)
            | ASTNodeEnum::FnDef(_)
            | ASTNodeEnum::FnCall(_)
            | ASTNodeEnum::AtomicExpression(_) => return Ok((true, None)),

            ASTNodeEnum::NamePath(_)
            | ASTNodeEnum::Reference(_)
            | ASTNodeEnum::StructDef(_)
            | ASTNodeEnum::LiteralValue(_)
            | ASTNodeEnum::Definition(_)
            | ASTNodeEnum::Module(_)
            | ASTNodeEnum::Use(_) => return Ok((false, None)),
        };
        return Ok((false, None));
    }
}

pub struct ResolvedVarDefTable {
    pub var_types: HashMap<Rc<GlobalResolvedName>, Type>,
}

pub fn insert_types(program: &mut FrontProgram, table: &mut ResolvedVarDefTable) {
    for v in program.definitions.function_definitions.values_mut() {
        for statement in &mut v.body.statements {
            statement.visit(table).unwrap();
        }
    }
}
