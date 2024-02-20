mod check_assignment;

use crate::front::exporter::export::FrontProgram;

pub enum PassError {
    Unimplemented,
    Generic(String),
}

pub type PassResult = Result<(), PassError>;

pub trait Pass {
    fn optimize(&mut self, program: &mut FrontProgram) -> PassResult;
}

pub fn optimize(program: &mut FrontProgram, passes: &mut Vec<Box<dyn Pass>>) -> PassResult {
    for pass in passes {
        pass.optimize(program)?;
    }
    Ok(())
}
