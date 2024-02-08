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