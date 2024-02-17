use camino::Utf8PathBuf;
use clap::Args;
use crate::cli::arg_runner::ArgRunner;
use crate::cli::build::BuildArgs;

#[derive(Debug, Args)]
pub struct NewArgs {
    /// The path to the project.
    path: Utf8PathBuf,

    #[clap(long)]
    /// The name of the project.
    name: Option<String>,
}

impl ArgRunner for BuildArgs {
    fn run(&self) -> String {
        format!("{:?}", self)
    }
}