use crate::back::code_generator::flatten_to_hmasm;
use crate::back::code_generator::generate_code;
use crate::cli::arg_runner::{ArgRunner, CliMessage};
use crate::front::file_system::fs::FileSystem;
use crate::front::file_system::system_fs::SystemFs;
use crate::front::mergers::program::ProgramMerger;
use crate::middle::passes::delete_unused::DeleteUnused;
use crate::middle::passes::optimize;
use camino::Utf8PathBuf;
use clap::Args;
use std::fs::File;
use std::io::{Read, Write};
use std::{fs, io};
use toml::{Table, Value};

#[derive(Debug, Args)]
pub struct BuildArgs {
    /// The path to the project.
    path: Option<Utf8PathBuf>,
    /// Should output HMASM instead
    #[clap(long, short = 'o')]
    hmasm: bool,
}

impl ArgRunner for BuildArgs {
    fn run(&self) -> CliMessage {
        let env_path = match Utf8PathBuf::try_from(std::env::current_dir().unwrap()) {
            Ok(p) => p,
            Err(e) => {
                return CliMessage::Error(format!(
                    "Could not parse current directory, perhaps there is a non-Utf8 character?: {}",
                    e
                ));
            }
        };

        let abs_path = {
            if let Some(given_path) = &self.path {
                if given_path.is_absolute() {
                    given_path.clone()
                } else {
                    env_path.join(given_path.clone())
                }
            } else {
                env_path
            }
        };

        let config = abs_path.join("blastf.toml");

        let mut buf = String::new();
        if let Ok(mut fs) = File::open(config) {
            if let Ok(read) = fs.read_to_string(&mut buf) {
                if read == 0 {
                    return CliMessage::Error("The blastf.toml file is empty.".to_string());
                }
            } else {
                return CliMessage::Error("Could not read the blastf.toml file.".to_string());
            }
        } else {
            return CliMessage::Error(
                "Could not find blastf.toml file, are you in the right directory?".to_string(),
            );
        }

        let package_name = if let Ok(toml) = buf.parse::<Table>() {
            if let Some(package) = toml.get("package") {
                if let Some(name) = package.get("name") {
                    if let Value::String(name) = name {
                        name.clone()
                    } else {
                        return CliMessage::Error("The name field in the package section of the blastf.toml file is not a string.".to_string());
                    }
                } else {
                    return CliMessage::Error(
                        "The package section of the blastf.toml file does not have a name field."
                            .to_string(),
                    );
                }
            } else {
                return CliMessage::Error(
                    "The blastf.toml file does not have a package section.".to_string(),
                );
            }
        } else {
            return CliMessage::Error("The blastf.toml file is not a valid toml file.".to_string());
        };

        let real_fs = SystemFs::new(abs_path.join("src")).unwrap();

        let mut program_merger = ProgramMerger::new(&package_name);

        program_merger.read_package(&package_name, real_fs);

        let front_program = program_merger.return_merged();
        let mut program = front_program.export_program();

        optimize(&mut program, &mut vec![Box::new(DeleteUnused)]);

        if let Err(e) = fs::create_dir(&abs_path.join("target")) {
            match e.kind() {
                io::ErrorKind::AlreadyExists => {}
                _ => {
                    return CliMessage::Error(format!(
                        "Could not create directory 'target'. Error: {}",
                        e
                    ))
                }
            }
        }

        let target = abs_path.join(format!("target/{package_name}.hmasm"));

        return if let Ok(mut fs) = File::create(&target) {
            if let Ok(_) = fs.write_all(flatten_to_hmasm(&generate_code(&program)).as_ref()) {
                CliMessage::Message(format!("Wrote the HMASM file to: {:?}", target))
            } else {
                CliMessage::Error("Could not write the HMASM file.".to_string())
            }
        } else {
            CliMessage::Error("Could not find the target directory.".to_string())
        };
    }
}
