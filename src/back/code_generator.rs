use crate::back::code_generator::generator::CodeGenerator;
use crate::middle::format::ir_types::{fn_name_from_block, FunctionName};
use crate::middle::format::types::Program;

mod generator;

pub struct MFunction {
    pub name: String,
    pub body: Vec<String>,
}

pub struct GeneratedCode {
    pub functions: Vec<MFunction>,
}

impl GeneratedCode {
    pub fn add_function(&mut self, function: MFunction) {
        self.functions.push(function);
    }
}

pub struct Context {
    pub fn_name: FunctionName,
    pub block_count: usize,
}

impl Context {
    pub fn new_block(&mut self) -> String {
        let fn_name = fn_name_from_block(&self.fn_name, self.block_count);
        self.block_count += 1;
        fn_name
    }
}

pub fn generate_code(program: &Program) -> GeneratedCode {
    let mut generated_code = GeneratedCode { functions: vec![] };
    for (name, def) in &program.function_definitions {
        let mut context = Context {
            fn_name: name.clone(),
            block_count: 0,
        };

        let mut body = vec![];
        for statement in &def.statements {
            body.extend(statement.generate(&mut generated_code, &mut context));
        }

        let mcf = MFunction {
            name: name.clone(),
            body,
        };

        generated_code.add_function(mcf);
    }
    generated_code
}

pub fn flatten_to_hmasm(generated_code: &GeneratedCode) -> String {
    let mut hmasm = String::new();
    for function in &generated_code.functions {
        hmasm.push_str(&format!("{}:\n", function.name));
        for line in &function.body {
            hmasm.push_str(&format!("    {}\n", line));
        }
    }
    hmasm
}

#[cfg(test)]
mod tests {
    use crate::back::code_generator::{flatten_to_hmasm, generate_code};
    use crate::front::file_system::fs::FileSystem;
    use crate::front::file_system::mock_fs::MockFileSystem;
    use crate::front::mergers::program::ProgramMerger;
    use camino::Utf8PathBuf;

    // TODO: these tests don't do anything at the moment, you should review it and make sure the output is correct.
    // TODO: should add a mcfunction interpreter to test the output of the code generator
    #[test]
    fn test_generate_code() {
        let mut mock_fs = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_fs.insert_file(
            Utf8PathBuf::from("main.ing"),
            "pub fn main() { let a: int = 5; if (a - 5 == 0) { a = 7; }; }",
        );

        let mut program_merger = ProgramMerger::new("test");
        program_merger.read_package("test", mock_fs);

        let front_program = program_merger.return_merged();
        let program = front_program.export_program();

        let hmasm = flatten_to_hmasm(&generate_code(&program));

        println!("{}", hmasm);
    }
    #[test]
    fn test_if_else() {
        let mut mock_fs = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_fs.insert_file(
            Utf8PathBuf::from("main.ing"),
            "pub fn main() { let a: int = 0; if (a == 0) { a = 0; } else if (a == 0) { a = 1; } else { a = 2; }; }",
        );

        let mut program_merger = ProgramMerger::new("test");
        program_merger.read_package("test", mock_fs);

        let front_program = program_merger.return_merged();
        let program = front_program.export_program();

        let hmasm = flatten_to_hmasm(&generate_code(&program));

        println!("{}", hmasm);
    }

    #[test]
    fn test_generate_for_code() {
        let mut mock_fs = MockFileSystem::new(Utf8PathBuf::new()).unwrap();
        mock_fs.insert_file(
            Utf8PathBuf::from("main.ing"),
            "pub fn main() { let a: int = 0; for (let i: int = 0; i < 9; i += 1) { a = 5; }; }",
        );

        let mut program_merger = ProgramMerger::new("test");
        program_merger.read_package("test", mock_fs);

        let front_program = program_merger.return_merged();
        let program = front_program.export_program();

        let hmasm = flatten_to_hmasm(&generate_code(&program));

        println!("{}", hmasm);
    }
}
