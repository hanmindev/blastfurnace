use crate::front::exporter::export::FrontProgram;

pub trait Pass {
    fn optimize(&mut self, program: &mut FrontProgram);
}

pub fn optimize(program: &mut FrontProgram, passes: &mut Vec<Box<dyn Pass>>) {
    for pass in passes {
        pass.optimize(program);
    }
}
