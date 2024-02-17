use clap::{Args, Parser, Subcommand};
use crate::cli::build::BuildArgs;
use crate::cli::new::NewArgs;
use crate::cli::arg_runner::ArgRunner;

mod back;
mod front;
mod middle;
mod cli;

#[derive(Debug, Parser)]
#[clap(name = "BlastFurnace", version)]
pub struct App {
    #[clap(subcommand)]
    command: Command,
}
#[derive(Debug, Subcommand)]
enum Command {
    /// Help message for creating a new project.
    New(NewArgs),
    /// Help message for building a project.
    Build(BuildArgs),
}

fn main() {
    let app = App::parse();
    match app.command {
        Command::New(args) => {
            args.run();
        }
        Command::Build(args) => {
            args.run();
        }
    }
}
