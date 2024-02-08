use crate::middle::format::ir_types::FunctionName;
use crate::back::code_generator::generator::CodeGenerator;
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
        self.block_count += 1;
        format!("{}_{}", self.fn_name, self.block_count)
    }
}


pub fn generate_code(program: &Program) -> GeneratedCode {
    let mut generated_code = GeneratedCode { functions: vec![] };
    for (name, def) in &program.function_definitions {
        let mut context = Context {
            fn_name: name.clone(),
            block_count: 0,
        };

        let mcf = MFunction {
            name: name.clone(),
            body: def.body.generate(&mut generated_code, &mut context),
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
    use crate::front::file_system::mock_fs::MockFileSystem;
    use crate::front::mergers::program::ProgramMerger;
    use crate::front::file_system::fs::FileSystem;
    use crate::back::code_generator::{flatten_to_hmasm, generate_code};

    #[test]
    fn test_generate_code() {
        let mut mock_fs = MockFileSystem::new("/".to_string());
        mock_fs.insert_file("/main.ing", "pub fn main() { let a: int = 5; } pub fn test() { let b: int = 8; }");

        let mut program_merger = ProgramMerger::new("test");
        program_merger.read_package("test", mock_fs);

        let program = program_merger.export_program();

        let hmasm = flatten_to_hmasm(&generate_code(&program));

        println!("{}", hmasm);
    }
}