use crate::middle::format::types::Program;

pub trait Pass {
    fn optimize(&mut self, program: &mut Program);
}

pub fn optimize(program: &mut Program, passes: &mut Vec<Box<dyn Pass>>) {
    for pass in passes {
        pass.optimize(program);
    }
}
