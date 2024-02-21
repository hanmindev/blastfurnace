use crate::front::ast_types::visitor::{ASTNodeEnum, GenericResolveResult, Visitable, Visitor};
use crate::front::exporter::export::FrontProgram;
use crate::front::passes::types::var_def_table::VarDefTable;

#[derive(Debug, PartialEq)]
pub enum ResolverError {
    Unknown,
}

pub type ResolveResult<T> = GenericResolveResult<T, ResolverError>;

impl Visitor<(), ResolverError> for VarDefTable {
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
    let mut table = VarDefTable::new(program);

    for v in program.definitions.function_definitions.values_mut() {
        for statement in &mut v.body.statements {
            statement.visit(&mut table).unwrap();
        }
    }
    return table;
}
