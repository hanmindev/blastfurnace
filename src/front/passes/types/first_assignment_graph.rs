use std::rc::Rc;
use either::Either;
use crate::front::ast_types::visitor::{ASTNodeEnum, GenericResolveResult, Visitable, Visitor};
use crate::front::exporter::export::FrontProgram;
use crate::front::passes::types::type_expression::TypeDependency;
use crate::front::passes::types::var_def_table::{VarDefTable, VarTypeNode};

#[derive(Debug, PartialEq)]
pub enum ResolverError {
    Unknown,
}

pub type ResolveResult<T> = GenericResolveResult<T, ResolverError>;

impl Visitor<(), ResolverError> for VarDefTable<'_> {
    fn apply(&mut self, ast_node: &mut ASTNodeEnum) -> ResolveResult<()> {
        match ast_node {
            ASTNodeEnum::VarDef(x) => {
                self.register_variable_type(x.name.global_resolved.as_ref().unwrap(), &x.type_);
            }
            ASTNodeEnum::VarAssign(x) => {
                x.expr.visit(self)?;
                self.register_variable_expr(
                    x.name_path.name.global_resolved.as_ref().unwrap(),
                    &x.expr,
                );
            }
            ASTNodeEnum::VarDecl(x) => {
                x.var_def.visit(self)?;
                if let Some(expr) = &mut x.expr {
                    expr.visit(self)?;
                    self.register_variable_expr(
                        x.var_def.name.global_resolved.as_ref().unwrap(),
                        &expr,
                    );
                }
            }
            ASTNodeEnum::StructInit(x) => {
                for (_, expr) in &mut x.fields {
                    expr.visit(self)?;
                }
            }

            ASTNodeEnum::If(_)
            | ASTNodeEnum::Else(_)
            | ASTNodeEnum::While(_)
            | ASTNodeEnum::For(_)
            | ASTNodeEnum::Statement(_)
            | ASTNodeEnum::Block(_)
            | ASTNodeEnum::FnDef(_)
            | ASTNodeEnum::FnCall(_) => return Ok((true, None)),

            ASTNodeEnum::AtomicExpression(_)
            | ASTNodeEnum::Expression(_)
            | ASTNodeEnum::NamePath(_)
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

pub fn create_first_assignment_graph(program: &mut FrontProgram) -> VarDefTable {
    let mut var_types = program
        .definitions
        .global_var_definitions
        .iter()
        .map(|(k, v)| {
            (Rc::clone(k), {
                if let Some(type_) = &v.var_def.type_ {
                    VarTypeNode {
                        types_: Either::Left(type_.clone()),
                    }
                } else {
                    VarTypeNode {
                        types_: Either::Right(TypeDependency::new(
                            &v.expr.as_ref().unwrap(),
                        )),
                    }
                }
            })
        })
        .collect();

    for fn_name in program.definitions.function_definitions.keys().map(|x| x.clone()).collect::<Vec<_>>() {
        let fn_body = program.definitions.function_definitions.get_mut(&fn_name).unwrap();
        let mut statements = fn_body.body.statements.drain(..).collect::<Vec<_>>();

        for statement in &mut statements {
            let mut table = VarDefTable::new(var_types);

            table.program = Some(program);
            statement.visit(&mut table).unwrap();
            table.program = None;

            var_types = table.var_types;
        }

        let fn_body = program.definitions.function_definitions.get_mut(&fn_name).unwrap();
        fn_body.body.statements = statements;
    }
    let mut table = VarDefTable::new(var_types);
    table.program = Some(program);
    table
}
