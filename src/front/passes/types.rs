mod type_expression;

use crate::front::ast_types::{Expression, GlobalResolvedName, Type};
use crate::front::exporter::export::FrontProgram;
use crate::front::passes::types::type_expression::TypeDependency;
use crate::front::passes::{Pass, PassResult};
use either::Either;
use std::collections::HashMap;

use crate::front::ast_types::visitor::{ASTNodeEnum, GenericResolveResult, Visitable, Visitor};
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum ResolverError {
    Unknown,
}

pub type ResolveResult<T> = GenericResolveResult<T, ResolverError>;

impl Visitor<ResolverError, ()> for VarDefTable<'_> {
    fn apply(&mut self, ast_node: &mut ASTNodeEnum) -> ResolveResult<()> {
        match ast_node {
            ASTNodeEnum::FnCall(x) => {
                for arg in &mut x.args {
                    arg.visit(self)?;
                }
                if let Some(_fn_def) = self
                    .program
                    .definitions
                    .function_definitions
                    .get(x.name.global_resolved.as_ref().unwrap().as_ref())
                {
                } else {
                    panic!("Function not found")
                }
            }
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
            | ASTNodeEnum::FnDef(_) => return Ok((true, None)),

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

struct VarTypeNode {
    types_: Either<Type, TypeDependency>,
}

struct VarDefTable<'a> {
    program: &'a FrontProgram,
    var_types: HashMap<Rc<GlobalResolvedName>, VarTypeNode>,
}

impl VarDefTable<'_> {
    fn new(program: &'_ FrontProgram) -> VarDefTable<'_> {
        VarDefTable {
            program,
            var_types: program
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
                .collect(),
        }
    }

    fn register_variable_type(&mut self, name: &Rc<GlobalResolvedName>, type_: &Option<Type>) {
        if let Some(type_) = type_ {
            self.var_types.insert(
                Rc::clone(name),
                VarTypeNode {
                    types_: Either::Left(type_.clone()),
                },
            );
        }
    }

    fn register_variable_expr(&mut self, name: &Rc<GlobalResolvedName>, expr: &Expression) {
        if let Some(_) = &expr.type_ {
            self.register_variable_type(name, &expr.type_);
        } else {
            self.var_types.insert(
                Rc::clone(name),
                VarTypeNode {
                    types_: Either::Right(TypeDependency::new(expr)),
                },
            );
        }
    }
}

#[derive(Debug)]
pub struct AnnotateTypes;

impl Pass for AnnotateTypes {
    fn pass(&mut self, program: &mut FrontProgram) -> PassResult {
        let _table = VarDefTable::new(program);

        // for (_, v) in &mut program.definitions.function_definitions {
        //     let mut statements = v.body.statements.drain(..).collect::<Vec<Statement>>();
        //     statements.iter().for_each(|s| { s.annotate(v, &mut table); });
        //
        //     v.body.statements = statements;
        // };
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::front::file_system::fs::FileSystem;
    use crate::front::file_system::mock_fs::MockFileSystem;
    use crate::front::mergers::program::ProgramMerger;
    use crate::front::passes::pass;
    use crate::front::passes::types::AnnotateTypes;
    use camino::Utf8PathBuf;

    #[test]
    fn test_type_annotation_simple() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(
            Utf8PathBuf::from("main.ing"),
            "pub fn main() { let a = 5; }",
        );

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut front_program = program_merger.return_merged();

        pass(&mut front_program, &mut vec![Box::new(AnnotateTypes)]);

        println!("{:?}", front_program);
    }
}
