use crate::middle::format::ir_types::{IrAtomicExpression, IrLiteralValue, IrVarDecl};
use crate::middle::format::ir_types::{
    IrBlock, IrCompound, IrCompoundValue, IrExpression, IrFnDef, IrIf, IrStatement,
    IrStatementBlock,
};
use crate::middle::format::types::{GlobalName, Program};
use crate::middle::passes::Pass;
use std::collections::HashSet;

trait CheckUsed {
    fn add_used(&self, used: &mut Used, program: &mut Program);
}

impl CheckUsed for IrCompound {
    fn add_used(&self, used: &mut Used, program: &mut Program) {
        for (_, value) in self {
            match value {
                IrCompoundValue::Expression(x) => x.add_used(used, program),
                IrCompoundValue::Compound(x) => x.add_used(used, program),
            }
        }
    }
}

impl CheckUsed for IrExpression {
    fn add_used(&self, used: &mut Used, program: &mut Program) {
        match self {
            IrExpression::AtomicExpression(x) => match x {
                IrAtomicExpression::Literal(y) => match y {
                    IrLiteralValue::Compound(c) => {
                        c.add_used(used, program);
                    }
                    _ => {}
                },
                IrAtomicExpression::Variable(y) => {
                    used.variables.insert(y.name.clone());
                }
                IrAtomicExpression::FnCall(y) => {
                    if let None = used.functions.get(&y.name) {
                        used.functions.insert(y.name.clone());
                        if let Some(fn_def) = program.function_definitions.remove(&y.name) {
                            fn_def.add_used(used, program);
                            program.function_definitions.insert(y.name.clone(), fn_def);
                        }
                    }
                }
            },
            IrExpression::Unary(_, x) => {
                x.add_used(used, program);
            }
            IrExpression::Binary(x0, _, x1) => {
                x0.add_used(used, program);
                x1.add_used(used, program);
            }
        }
    }
}

impl CheckUsed for IrIf {
    fn add_used(&self, used: &mut Used, program: &mut Program) {
        self.cond.add_used(used, program);
        self.body.add_used(used, program);
        if let Some(else_) = &self.else_ {
            else_.add_used(used, program);
        }
    }
}

impl CheckUsed for IrStatement {
    fn add_used(&self, used: &mut Used, program: &mut Program) {
        match self {
            IrStatement::VarAssign(x) => {
                used.variables.insert(x.name_path.name.clone());
            }
            IrStatement::If(x) => x.add_used(used, program),
            IrStatement::While(x) => {
                x.cond.add_used(used, program);
                x.body.add_used(used, program);
            }
            IrStatement::For(x) => {
                if let Some(init) = &x.init {
                    init.add_used(used, program);
                }
                if let Some(cond) = &x.cond {
                    cond.add_used(used, program);
                }
                if let Some(step) = &x.step {
                    step.add_used(used, program);
                }
                x.body.add_used(used, program);
            }
            IrStatement::Return(x) => {
                x.add_used(used, program);
            }
            IrStatement::Expression(x) => x.add_used(used, program),
            IrStatement::VarDecl(x) => x.add_used(used, program),
            _ => {}
        }
    }
}

impl CheckUsed for IrVarDecl {
    fn add_used(&self, used: &mut Used, program: &mut Program) {
        if let Some(expr) = &self.expr {
            expr.add_used(used, program);
        }
    }
}

impl CheckUsed for IrStatementBlock {
    fn add_used(&self, used: &mut Used, program: &mut Program) {
        match self {
            IrStatementBlock::Statement(x) => x.add_used(used, program),
            IrStatementBlock::Block(x) => x.add_used(used, program),
        }
    }
}

impl CheckUsed for IrBlock {
    fn add_used(&self, used: &mut Used, program: &mut Program) {
        for statement_block in &self.statements {
            statement_block.add_used(used, program);
        }
    }
}

impl CheckUsed for IrFnDef {
    fn add_used(&self, used: &mut Used, program: &mut Program) {
        used.functions.insert(self.name.clone());
        self.body.add_used(used, program);
    }
}

struct Used {
    functions: HashSet<GlobalName>,
    structs: HashSet<GlobalName>,
    variables: HashSet<GlobalName>,
}

#[derive(Debug)]
pub struct DeleteUnused {}

impl Pass for DeleteUnused {
    fn optimize(&mut self, program: &mut Program) {
        let mut used = Used {
            functions: program.public_functions.clone(),
            structs: HashSet::new(),
            variables: HashSet::new(),
        };

        for global_name in program.public_functions.clone() {
            if let Some(fn_def) = program.function_definitions.remove(&global_name) {
                fn_def.add_used(&mut used, program);
                program.function_definitions.insert(global_name, fn_def);
            }
        }

        program
            .function_definitions
            .retain(|x, _| used.functions.contains(x));
        program
            .struct_definitions
            .retain(|x, _| used.structs.contains(x));
        // program.global_var_definitions.retain(|x, _| used.variables.contains(x));
    }
}

#[cfg(test)]
mod tests {
    use crate::front::file_system::fs::FileSystem;
    use crate::front::file_system::mock_fs::MockFileSystem;
    use crate::front::mergers::program::ProgramMerger;
    use crate::middle::passes::delete_unused::DeleteUnused;
    use crate::middle::passes::optimize;

    #[test]
    fn test_simple_deletion() {
        let mut mock_fs_0 = MockFileSystem::new("/".to_string());
        mock_fs_0.insert_file("/main.ing", "mod test; pub fn main() {}");
        mock_fs_0.insert_file("/test.ing", "pub mod example; pub fn hello() {}");
        mock_fs_0.insert_dir("/test/");
        mock_fs_0.insert_file("/test/example.ing", "pub fn a() {};");

        let mut mock_fs_1 = MockFileSystem::new("/".to_string());
        mock_fs_1.insert_file("/main.ing", "pub fn libfunc() {}");

        let mut program_merger = ProgramMerger::new("test");
        program_merger.read_package("test", mock_fs_0);
        program_merger.read_package("library", mock_fs_1);

        let mut program = program_merger.export_program();

        optimize(&mut program, &mut vec![Box::new(DeleteUnused {})]);

        assert_eq!(program.function_definitions.len(), 1);
        assert_eq!(
            program
                .function_definitions
                .contains_key("test/root/0_main"),
            true
        );
    }
}
