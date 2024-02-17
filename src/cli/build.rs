use crate::cli::arg_runner::{ArgRunner, CliMessage};
use clap::Args;

#[derive(Debug, Args)]
pub struct BuildArgs {
    /// Should output HMASM instead
    #[clap(long, short = 'o')]
    hmasm: bool,
}

impl ArgRunner for BuildArgs {
    fn run(&self) -> CliMessage {
        CliMessage::Message(format!("{:?}", self))
    }
}
