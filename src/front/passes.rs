mod check_assignment;
mod types;

use crate::front::exporter::export::FrontProgram;

pub enum PassError {
    Unimplemented,
    Generic(String),
}

pub type PassResult = Result<(), PassError>;

pub trait Pass {
    fn pass(&mut self, program: &mut FrontProgram) -> PassResult;
}

pub fn pass(program: &mut FrontProgram, passes: &mut Vec<Box<dyn Pass>>) -> PassResult {
    for pass in passes {
        pass.pass(program)?;
    }
    Ok(())
}
