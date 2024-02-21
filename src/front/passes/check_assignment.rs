mod null_check;

use crate::front::ast_types::visitor::{ASTNodeEnum, GenericResolveResult, Visitable, Visitor};
use crate::front::ast_types::{AtomicExpression, Else, ExpressionEnum, Statement};
use crate::front::exporter::export::FrontProgram;
use crate::front::passes::check_assignment::null_check::NullCheck;
use crate::front::passes::{Pass, PassError, PassResult};
use std::collections::HashSet;

use std::ops::DerefMut;

#[derive(Debug, PartialEq)]
pub enum ResolverError {
    Unknown,
}

pub type ResolveResult<T> = GenericResolveResult<T, ResolverError>;

impl Visitor<bool, ResolverError> for Option<NullCheck> {
    fn apply(&mut self, ast_node: &mut ASTNodeEnum) -> ResolveResult<bool> {
        match ast_node {
            ASTNodeEnum::AtomicExpression(atomic) => {
                return match atomic {
                    AtomicExpression::Variable(x) => Ok((
                        false,
                        Some(
                            self.as_mut()
                                .unwrap()
                                .used_as_null(x.name.global_resolved.as_ref().unwrap().clone()),
                        ),
                    )),
                    AtomicExpression::FnCall(x) => {
                        for arg in &mut x.args {
                            arg.visit(self)?;
                        }
                        Ok((false, Some(false)))
                    }
                    AtomicExpression::Literal(_) => Ok((true, Some(false))),
                }
            }
            ASTNodeEnum::If(if_) => {
                if_.cond.visit(self)?;

                let mut ifs = vec![];

                let mut nc = Some(NullCheck::new_with_parent(self.take().unwrap()));
                if_.body.visit(&mut nc)?;
                ifs.push(nc.as_mut().unwrap().take_not_null());
                *self = Some(nc.unwrap().take_parent());

                let mut always_run_one = false;

                let mut cur = if_.deref_mut();
                while let Some(ref mut else_) = &mut cur.else_ {
                    let mut nc = Some(NullCheck::new_with_parent(self.take().unwrap()));
                    match else_ {
                        Else::If(ref mut x) => {
                            x.cond.visit(self)?;
                            x.body.visit(&mut nc)?;
                            ifs.push(nc.as_mut().unwrap().take_not_null());
                            *self = Some(nc.unwrap().take_parent());

                            cur = x.as_mut();
                        }
                        Else::Block(ref mut x) => {
                            x.visit(&mut nc)?;
                            ifs.push(nc.as_mut().unwrap().take_not_null());
                            *self = Some(nc.unwrap().take_parent());

                            always_run_one = true;
                            break;
                        }
                    }
                }

                // body may not necessarily run, so any "unnulled" variables in the body are not necessarily unnulled
                // should add check to see if it can be determined whether the body runs at least once aside from
                // existence of else block
                if always_run_one {
                    self.as_mut().unwrap().merge_children(ifs);
                }
            }

            ASTNodeEnum::While(while_) => {
                while_.cond.visit(self)?;

                // body may not necessarily run, so any "unnulled" variables in the body are not necessarily unnulled
                // should add check to see if it can be determined whether the body runs at least once

                let mut body_null_check = Some(NullCheck::new_with_parent(self.take().unwrap()));
                while_.body.visit(&mut body_null_check)?;
                *self = Some(body_null_check.unwrap().take_parent());
            }

            ASTNodeEnum::For(for_) => {
                if let Some(ref mut init) = &mut for_.init {
                    init.visit(self)?;
                }
                if let Some(ref mut cond) = &mut for_.cond {
                    cond.visit(self)?;
                }

                // body may not necessarily run, so any "unnulled" variables in the body are not necessarily unnulled
                // should add check to see if it can be determined whether the body runs at least once
                let mut body_null_check = Some(NullCheck::new_with_parent(self.take().unwrap()));
                for_.body.visit(&mut body_null_check)?;
                *self = Some(body_null_check.unwrap().take_parent());
            }
            ASTNodeEnum::VarAssign(var_assign) => {
                if !self
                    .apply(&mut ASTNodeEnum::Expression(&mut var_assign.expr))?
                    .1
                    .unwrap()
                {
                    self.as_mut().unwrap().confirm_not_null(
                        var_assign
                            .name_path
                            .name
                            .global_resolved
                            .as_ref()
                            .unwrap()
                            .clone(),
                    );
                }
            }

            ASTNodeEnum::VarDecl(var_decl) => {
                var_decl.var_def.visit(self)?;
                if let Some(expr) = &mut var_decl.expr {
                    if !self.apply(&mut ASTNodeEnum::Expression(expr))?.1.unwrap() {
                        self.as_mut().unwrap().confirm_not_null(
                            var_decl
                                .var_def
                                .name
                                .global_resolved
                                .as_ref()
                                .unwrap()
                                .clone(),
                        );
                    }
                }
            }
            ASTNodeEnum::Expression(expr) => {
                return match &mut expr.expr {
                    ExpressionEnum::AtomicExpression(ref mut e) => {
                        Ok((false, self.apply(&mut ASTNodeEnum::AtomicExpression(e))?.1))
                    }
                    ExpressionEnum::Unary(_, ref mut e) => Ok((
                        false,
                        self.apply(&mut ASTNodeEnum::Expression(e.as_mut()))?.1,
                    )),
                    ExpressionEnum::Binary(ref mut e0, _, ref mut e1) => {
                        let t0 = self.apply(&mut ASTNodeEnum::Expression(e0))?.1.unwrap();
                        let t1 = self.apply(&mut ASTNodeEnum::Expression(e1))?.1.unwrap();
                        Ok((false, Some(t0 || t1)))
                    }
                };
            }

            ASTNodeEnum::FnCall(_)
            | ASTNodeEnum::VarDef(_)
            | ASTNodeEnum::Statement(_)
            | ASTNodeEnum::Block(_)
            | ASTNodeEnum::NamePath(_)
            | ASTNodeEnum::Reference(_)
            | ASTNodeEnum::FnDef(_)
            | ASTNodeEnum::StructDef(_)
            | ASTNodeEnum::LiteralValue(_)
            | ASTNodeEnum::Else(_)
            | ASTNodeEnum::Definition(_)
            | ASTNodeEnum::Module(_)
            | ASTNodeEnum::Use(_) => return Ok((true, None)),
        };
        return Ok((false, None));
    }
}

#[derive(Debug)]
pub struct DisallowNullAssignment;

impl Pass for DisallowNullAssignment {
    fn pass(&mut self, program: &mut FrontProgram) -> PassResult {
        let mut all_null_accesses = HashSet::new();

        for (_, v) in &mut program.definitions.function_definitions {
            let mut null_check = Some(NullCheck::new());

            let mut statements = v.body.statements.drain(..).collect::<Vec<Statement>>();
            statements.iter_mut().for_each(|s| {
                s.visit(&mut null_check).expect("Null check failed");
            });

            v.body.statements = statements;

            all_null_accesses.extend(null_check.unwrap().force_take_used_while_null());
        }

        if !all_null_accesses.is_empty() {
            return Err(PassError::Generic(format!(
                "Null access detected: {:?}",
                all_null_accesses
            )));
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::front::file_system::fs::FileSystem;
    use crate::front::file_system::mock_fs::MockFileSystem;
    use crate::front::mergers::program::ProgramMerger;
    use crate::front::passes::check_assignment::DisallowNullAssignment;
    use crate::front::passes::pass;
    use camino::Utf8PathBuf;

    #[test]
    fn test_simple_pass() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(
            Utf8PathBuf::from("main.ing"),
            "pub fn main() { let a = 5; let b = 2; a = b; }",
        );

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut front_program = program_merger.return_merged();

        assert!(pass(
            &mut front_program,
            &mut vec![Box::new(DisallowNullAssignment)],
        )
        .is_ok());
    }

    #[test]
    fn test_simple_null_assign() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(
            Utf8PathBuf::from("main.ing"),
            "pub fn main() { let a = 5; let b; a = b; }",
        );

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut front_program = program_merger.return_merged();

        assert!(pass(
            &mut front_program,
            &mut vec![Box::new(DisallowNullAssignment)],
        )
        .is_err());
    }

    #[test]
    fn test_complex_null_assign() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(
            Utf8PathBuf::from("main.ing"),
            "pub fn main() { let a = 5; let b; a = 2 + b * 5 / 9; }",
        );

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut front_program = program_merger.return_merged();

        assert!(pass(
            &mut front_program,
            &mut vec![Box::new(DisallowNullAssignment)],
        )
        .is_err());
    }

    #[test]
    fn test_comprehensive_if_null_assign() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(
            Utf8PathBuf::from("main.ing"),
            "pub fn main() { let c = 0; let a; if (c == 0) { a = 5; } else { a = 6; } let b = a; }",
        );

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut front_program = program_merger.return_merged();

        assert!(pass(
            &mut front_program,
            &mut vec![Box::new(DisallowNullAssignment)],
        )
        .is_ok());
    }

    #[test]
    fn test_non_comprehensive_if_null_assign() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(
            Utf8PathBuf::from("main.ing"),
            "pub fn main() { let c = 0; let a; if (c == 0) { a = 5; } let b = a; }",
        );

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut front_program = program_merger.return_merged();

        assert!(pass(
            &mut front_program,
            &mut vec![Box::new(DisallowNullAssignment)],
        )
        .is_err());
    }

    #[test]
    fn test_null_fn_argument() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(
            Utf8PathBuf::from("main.ing"),
            "pub fn main(b: int) { let a; main(a); }",
        );

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut front_program = program_merger.return_merged();

        assert!(pass(
            &mut front_program,
            &mut vec![Box::new(DisallowNullAssignment)],
        )
        .is_err());
    }
}
