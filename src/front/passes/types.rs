mod type_expression;

use crate::front::ast_types::{
    AtomicExpression, Block, Else, Expression, FnCall, FnDef, For, GlobalResolvedName, If,
    Statement, Type, VarAssign, VarDecl, VarDef, While,
};
use crate::front::exporter::export::FrontProgram;
use crate::front::passes::types::type_expression::TypeDependency;
use crate::front::passes::{Pass, PassResult};
use either::Either;
use std::collections::HashMap;

use std::rc::Rc;

trait CheckUsed {
    fn annotate(&self, fn_def: &FnDef, table: &mut VarDefTable);
}

impl CheckUsed for FnCall {
    fn annotate(&self, fn_def: &FnDef, table: &mut VarDefTable) {
        for arg in &self.args {
            arg.annotate(fn_def, table);
        }
        if let Some(_fn_def) = table
            .program
            .definitions
            .function_definitions
            .get(self.name.global_resolved.as_ref().unwrap().as_ref())
        {
            // return Some(fn_def.return_type.clone());
        } else {
            panic!("Function not found")
        }
    }
}

impl CheckUsed for AtomicExpression {
    fn annotate(&self, _fn_def: &FnDef, _table: &mut VarDefTable) {}
}

impl CheckUsed for Expression {
    fn annotate(&self, _fn_def: &FnDef, _table: &mut VarDefTable) {}
}

impl CheckUsed for Else {
    fn annotate(&self, fn_def: &FnDef, table: &mut VarDefTable) {
        match self {
            Else::If(x) => {
                x.annotate(fn_def, table);
            }
            Else::Block(x) => {
                x.annotate(fn_def, table);
            }
        }
    }
}

impl CheckUsed for If {
    fn annotate(&self, fn_def: &FnDef, table: &mut VarDefTable) {
        self.cond.annotate(fn_def, table);
        self.body.annotate(fn_def, table);
        if let Some(else_) = &self.else_ {
            else_.annotate(fn_def, table);
        }
    }
}

impl CheckUsed for While {
    fn annotate(&self, fn_def: &FnDef, table: &mut VarDefTable) {
        self.cond.annotate(fn_def, table);
        self.body.annotate(fn_def, table);
    }
}

impl CheckUsed for For {
    fn annotate(&self, fn_def: &FnDef, table: &mut VarDefTable) {
        if let Some(init) = &self.init {
            init.annotate(fn_def, table);
        }
        if let Some(cond) = &self.cond {
            cond.annotate(fn_def, table);
        }
        self.body.annotate(fn_def, table);
    }
}

impl CheckUsed for VarDef {
    fn annotate(&self, _fn_def: &FnDef, table: &mut VarDefTable) {
        table.register_variable_type(self.name.global_resolved.as_ref().unwrap(), &self.type_);
    }
}

impl CheckUsed for VarAssign {
    fn annotate(&self, fn_def: &FnDef, table: &mut VarDefTable) {
        self.expr.annotate(fn_def, table);
        table.register_variable_expr(
            self.name_path.name.global_resolved.as_ref().unwrap(),
            &self.expr,
        );
    }
}

impl CheckUsed for VarDecl {
    fn annotate(&self, fn_def: &FnDef, table: &mut VarDefTable) {
        self.var_def.annotate(fn_def, table);
        if let Some(expr) = &self.expr {
            expr.annotate(fn_def, table);
            table
                .register_variable_expr(self.var_def.name.global_resolved.as_ref().unwrap(), &expr);
        }
    }
}

impl CheckUsed for Statement {
    fn annotate(&self, fn_def: &FnDef, table: &mut VarDefTable) {
        match self {
            Statement::VarDecl(x) => x.annotate(fn_def, table),
            Statement::VarAssign(x) => x.annotate(fn_def, table),
            Statement::If(x) => x.annotate(fn_def, table),
            Statement::While(x) => x.annotate(fn_def, table),
            Statement::For(x) => x.annotate(fn_def, table),
            Statement::Return(x) => x.annotate(fn_def, table),
            Statement::Expression(x) => x.annotate(fn_def, table),
            Statement::Block(x) => x.annotate(fn_def, table),
            Statement::Continue | Statement::Break => {}
        }
    }
}

impl CheckUsed for Block {
    fn annotate(&self, fn_def: &FnDef, table: &mut VarDefTable) {
        for statement in &self.statements {
            statement.annotate(fn_def, table);
        }
    }
}

impl CheckUsed for FnDef {
    fn annotate(&self, fn_def: &FnDef, table: &mut VarDefTable) {
        self.body.annotate(fn_def, table);
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
