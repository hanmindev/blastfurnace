use clap::Args;
use crate::cli::arg_runner::ArgRunner;

#[derive(Debug, Args)]
pub struct BuildArgs {
    /// Should output HMASM instead
    #[clap(long, short = 'h')]
    hmasm: Option<bool>,
}

impl ArgRunner for BuildArgs {
    fn run(&self) -> String {
        format!("{:?}", self)
    }
}