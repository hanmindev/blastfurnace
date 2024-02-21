mod first_assignment_graph;
mod insert_types;
mod topological_sort;
mod type_expression;
mod var_def_table;

use crate::front::exporter::export::FrontProgram;
use crate::front::passes::{Pass, PassError, PassResult};
use either::Either;
use std::collections::HashMap;

use crate::front::passes::types::first_assignment_graph::create_first_assignment_graph;
use crate::front::passes::types::insert_types::{insert_types, ResolvedVarDefTable};
use crate::front::passes::types::topological_sort::topological_sort;
use crate::front::passes::types::var_def_table::VarTypeNode;

#[derive(Debug, PartialEq)]
pub enum TypeError {
    TypeMismatch,
    MultipleTypes,
    NotEnoughInformation
}

#[derive(Debug)]
pub struct AnnotateTypes;

impl Pass for AnnotateTypes {
    fn pass(&mut self, program: &mut FrontProgram) -> PassResult {
        let mut table = create_first_assignment_graph(program);

        // topologically sort first assignment graph
        let sorted = topological_sort(&table);

        let mut new_table = ResolvedVarDefTable {
            var_types: HashMap::new(),
        };

        // evaluate types
        for value in sorted {
            let types = &table.var_types.get(&value).unwrap().types_;
            let type_ = match &types {
                Either::Left(l) => l.clone(),
                Either::Right(r) => {
                    if let Ok(type_) = r.tree.resolve_type(program, &table.var_types) {
                        type_
                    } else {
                        return Err(PassError::Types(TypeError::NotEnoughInformation));
                    }
                }
            };

            table.var_types.insert(
                value.clone(),
                VarTypeNode {
                    types_: Either::Left(type_.clone()),
                },
            );
            new_table.var_types.insert(value, type_);
        }

        if let Err(e) = insert_types(program, &mut new_table) {
            return Err(PassError::Types(e));
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::front::ast_types::Type;
    use crate::front::ast_types::{GlobalResolvedName, Statement};
    use crate::front::file_system::fs::FileSystem;
    use crate::front::file_system::mock_fs::MockFileSystem;
    use crate::front::mergers::program::ProgramMerger;
    use crate::front::passes::pass;
    use crate::front::passes::types::AnnotateTypes;
    use camino::Utf8PathBuf;
    use std::rc::Rc;

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

        if let Statement::VarDecl(x) = &front_program
            .definitions
            .function_definitions
            .get(&Rc::from(GlobalResolvedName {
                package: Rc::from("pkg"),
                module: Rc::from("/root"),
                name: "0_main".to_string(),
            }))
            .unwrap()
            .body
            .statements[0]
        {
            assert_eq!(x.var_def.type_.as_ref().unwrap(), &Type::Int);
        } else {
            panic!("Expected VarDecl");
        }
    }

    #[test]
    fn test_type_annotation_complex() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(
            Utf8PathBuf::from("main.ing"),
            "pub fn main() { let a; let b = 5; a = 5 * b; }",
        );

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut front_program = program_merger.return_merged();

        pass(&mut front_program, &mut vec![Box::new(AnnotateTypes)]);

        if let Statement::VarDecl(x) = &front_program
            .definitions
            .function_definitions
            .get(&Rc::from(GlobalResolvedName {
                package: Rc::from("pkg"),
                module: Rc::from("/root"),
                name: "0_main".to_string(),
            }))
            .unwrap()
            .body
            .statements[0]
        {
            assert_eq!(x.var_def.type_.as_ref().unwrap(), &Type::Int);
        } else {
            panic!("Expected VarDecl");
        }
    }


    #[test]
    fn test_illegal_struct_assignment() {
        let mut mock_file_system = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_file_system.insert_file(
            Utf8PathBuf::from("main.ing"),
            "struct A { a: int, b: int } pub fn main() { let a: A = A { a: 1, b: 2 }; a = 5; }",
        );

        let mut program_merger = ProgramMerger::new("pkg");

        program_merger.read_package("pkg", mock_file_system);

        let mut front_program = program_merger.return_merged();
        assert!(pass(&mut front_program, &mut vec![Box::new(AnnotateTypes)]).is_err());
    }
}
