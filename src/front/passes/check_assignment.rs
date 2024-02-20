mod null_check;

use std::collections::HashSet;
use crate::front::ast_types::{AtomicExpression, Block, Else, Expression, ExpressionEnum, FnCall, FnDef, For, If, Statement, VarAssign, VarDecl, VarDef, While};
use crate::front::exporter::export::FrontProgram;
use crate::front::passes::{Pass, PassError, PassResult};
use std::hash::Hash;
use std::ops::DerefMut;
use crate::front::passes::check_assignment::null_check::NullCheck;

trait CheckNull {
    fn check_null(&mut self, fn_def: &FnDef, null_check: &mut Option<NullCheck>) -> bool;
}

impl CheckNull for FnCall {
    fn check_null(&mut self, fn_def: &FnDef, null_check: &mut Option<NullCheck>) -> bool {
        for mut arg in &mut self.args {
            arg.check_null(fn_def, null_check);
        }
        return false;
    }
}

impl CheckNull for AtomicExpression {
    fn check_null(&mut self, fn_def: &FnDef, null_check: &mut Option<NullCheck>) -> bool {
        match self {
            AtomicExpression::Variable(x) =>
                null_check.as_mut().unwrap().used_as_null(x.name.global_resolved.as_ref().unwrap().clone()),
            AtomicExpression::FnCall(x) => x.check_null(fn_def, null_check),
            AtomicExpression::Literal(_) => false
        }
    }
}

impl CheckNull for Expression {
    fn check_null(&mut self, fn_def: &FnDef, null_check: &mut Option<NullCheck>) -> bool {
        return match &mut self.expr {
            ExpressionEnum::AtomicExpression(e) => e.check_null(fn_def, null_check),
            ExpressionEnum::Unary(_, e) => e.check_null(fn_def, null_check),
            ExpressionEnum::Binary(e0, _, e1) => {
                let t0 = e0.check_null(fn_def, null_check);
                let t1 = e1.check_null(fn_def, null_check);
                t0 || t1
            }
        };
    }
}

impl CheckNull for If {
    fn check_null(&mut self, fn_def: &FnDef, null_check: &mut Option<NullCheck>) -> bool {
        self.cond.check_null(fn_def, null_check);


        let mut ifs = vec![];

        let mut nc = Some(NullCheck::new_with_parent(null_check.take().unwrap()));
        self.body.check_null(fn_def, &mut nc);
        ifs.push(nc.as_mut().unwrap().take_not_null());
        *null_check = Some(nc.unwrap().take_parent());

        let mut always_run_one = false;

        let mut cur = self;
        while let Some(ref mut else_) = &mut cur.else_ {
            let mut nc = Some(NullCheck::new_with_parent(null_check.take().unwrap()));
            match else_ {
                Else::If(ref mut x) => {
                    x.cond.check_null(fn_def, null_check);

                    x.body.check_null(fn_def, &mut nc);
                    ifs.push(nc.as_mut().unwrap().take_not_null());
                    *null_check = Some(nc.unwrap().take_parent());

                    cur = x.as_mut();
                }
                Else::Block(ref mut x) => {
                    x.check_null(fn_def, &mut nc);
                    ifs.push(nc.as_mut().unwrap().take_not_null());
                    *null_check = Some(nc.unwrap().take_parent());

                    always_run_one = true;
                    break;
                }
            }
        }

        // body may not necessarily run, so any "unnulled" variables in the body are not necessarily unnulled
        // should add check to see if it can be determined whether the body runs at least once aside from
        // existence of else block
        if always_run_one {
            null_check.as_mut().unwrap().merge_children(ifs);
        }

        return true;
    }
}

impl CheckNull for While {
    fn check_null(&mut self, fn_def: &FnDef, null_check: &mut Option<NullCheck>) -> bool {
        self.cond.check_null(fn_def, null_check);

        // body may not necessarily run, so any "unnulled" variables in the body are not necessarily unnulled
        // should add check to see if it can be determined whether the body runs at least once

        let mut body_null_check = Some(NullCheck::new_with_parent(null_check.take().unwrap()));
        self.body.check_null(fn_def, &mut body_null_check);
        *null_check = Some(body_null_check.unwrap().take_parent());
        return true;
    }
}

impl CheckNull for For {
    fn check_null(&mut self, fn_def: &FnDef, null_check: &mut Option<NullCheck>) -> bool {
        if let Some(ref mut init) = &mut self.init {
            init.check_null(fn_def, null_check);
        }
        if let Some(ref mut cond) = &mut self.cond {
            cond.check_null(fn_def, null_check);
        }

        // body may not necessarily run, so any "unnulled" variables in the body are not necessarily unnulled
        // should add check to see if it can be determined whether the body runs at least once
        let mut body_null_check = Some(NullCheck::new_with_parent(null_check.take().unwrap()));
        self.body.check_null(fn_def, &mut body_null_check);
        *null_check = Some(body_null_check.unwrap().take_parent());

        return true;
    }
}

impl CheckNull for VarDef {
    fn check_null(&mut self, _fn_def: &FnDef, _null_check: &mut Option<NullCheck>) -> bool {
        return true;
    }
}

impl CheckNull for VarAssign {
    fn check_null(&mut self, fn_def: &FnDef, null_check: &mut Option<NullCheck>) -> bool {
        if !self.expr.check_null(fn_def, null_check) {
            null_check.as_mut().unwrap().confirm_not_null(self.name_path.name.global_resolved.as_ref().unwrap().clone());
        }
        return true;
    }
}

impl CheckNull for VarDecl {
    fn check_null(&mut self, fn_def: &FnDef, null_check: &mut Option<NullCheck>) -> bool {
        self.var_def.check_null(fn_def, null_check);
        if let Some(expr) = &mut self.expr {
            if !expr.check_null(fn_def, null_check) {
                null_check.as_mut().unwrap().confirm_not_null(self.var_def.name.global_resolved.as_ref().unwrap().clone());
            }
        }
        return true;
    }
}

impl CheckNull for Statement {
    fn check_null(&mut self, fn_def: &FnDef, null_check: &mut Option<NullCheck>) -> bool {
        match self {
            Statement::VarDecl(x) => x.check_null(fn_def, null_check),
            Statement::VarAssign(x) => x.check_null(fn_def, null_check),
            Statement::If(x) => x.check_null(fn_def, null_check),
            Statement::While(x) => x.check_null(fn_def, null_check),
            Statement::For(x) => x.check_null(fn_def, null_check),
            Statement::Return(x) => {
                x.check_null(fn_def, null_check);
                true
            }
            Statement::Expression(x) => x.check_null(fn_def, null_check),
            Statement::Block(x) => x.check_null(fn_def, null_check),
            Statement::Continue | Statement::Break => true
        }
    }
}

impl CheckNull for Block {
    fn check_null(&mut self, fn_def: &FnDef, null_check: &mut Option<NullCheck>) -> bool {
        for mut statement in &mut self.statements {
            statement.check_null(fn_def, null_check);
        }
        return true;
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
            statements.iter_mut().for_each(|s| { s.check_null(v, &mut null_check); });

            v.body.statements = statements;

            all_null_accesses.extend(null_check.unwrap().force_take_used_while_null());
        };

        if !all_null_accesses.is_empty() {
            return Err(PassError::Generic(format!("Null access detected: {:?}", all_null_accesses)));
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::front::file_system::fs::FileSystem;
    use crate::front::file_system::mock_fs::MockFileSystem;
    use crate::front::mergers::program::ProgramMerger;
    use crate::front::passes::pass;
    use camino::Utf8PathBuf;
    use crate::front::passes::check_assignment::DisallowNullAssignment;

    #[test]
    fn test_simple_pass() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(Utf8PathBuf::from("main.ing"), "pub fn main() { let a = 5; let b = 2; a = b; }");

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut front_program = program_merger.return_merged();

        assert!(pass(&mut front_program, &mut vec![Box::new(DisallowNullAssignment)]).is_ok());
    }

    #[test]
    fn test_simple_null_assign() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(Utf8PathBuf::from("main.ing"), "pub fn main() { let a = 5; let b; a = b; }");

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut front_program = program_merger.return_merged();

        assert!(pass(&mut front_program, &mut vec![Box::new(DisallowNullAssignment)]).is_err());
    }

    #[test]
    fn test_complex_null_assign() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(Utf8PathBuf::from("main.ing"), "pub fn main() { let a = 5; let b; a = 2 + b * 5 / 9; }");

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut front_program = program_merger.return_merged();

        assert!(pass(&mut front_program, &mut vec![Box::new(DisallowNullAssignment)]).is_err());
    }

    #[test]
    fn test_comprehensive_if_null_assign() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(Utf8PathBuf::from("main.ing"), "pub fn main() { let c = 0; let a; if (c == 0) { a = 5; } else { a = 6; } let b = a; }");

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut front_program = program_merger.return_merged();

        assert!(pass(&mut front_program, &mut vec![Box::new(DisallowNullAssignment)]).is_ok());
    }

    #[test]
    fn test_non_comprehensive_if_null_assign() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(Utf8PathBuf::from("main.ing"), "pub fn main() { let c = 0; let a; if (c == 0) { a = 5; } let b = a; }");

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut front_program = program_merger.return_merged();

        assert!(pass(&mut front_program, &mut vec![Box::new(DisallowNullAssignment)]).is_err());
    }
}
