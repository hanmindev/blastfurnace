use crate::cli::arg_runner::{ArgRunner, CliMessage};
use camino::Utf8PathBuf;
use clap::Args;
use std::fs::File;
use std::io::Write;

#[derive(Debug, Args)]
pub struct NewArgs {
    /// The path to the project.
    path: Utf8PathBuf,

    #[clap(long)]
    /// The name of the project.
    name: Option<String>,
}

impl ArgRunner for NewArgs {
    fn run(&self) -> CliMessage {
        let path = match Utf8PathBuf::try_from(std::env::current_dir().unwrap()) {
            Ok(p) => p,
            Err(e) => {
                return CliMessage::Error(format!(
                    "Could not parse current directory, perhaps there is a non-Utf8 character?: {}",
                    e
                ));
            }
        };

        if self.path.extension() != None {
            return CliMessage::Error(format!(
                "The path specified is not a directory: {:?}",
                self.path
            ));
        }

        let abs_path = if self.path.is_relative() {
            path.join(self.path.clone())
        } else {
            self.path.clone()
        };

        let name = if let Some(n) = &self.name {
            n.clone()
        } else {
            if let Some(n) = abs_path.file_name() {
                n.to_string()
            } else {
                return CliMessage::Error(format!("Could not get the name of the project from the path. Specify a project name using --name if this was intentional: {:?}", abs_path));
            }
        };

        if abs_path.try_exists().unwrap_or(false) {
            if let Ok(files) = abs_path.read_dir() {
                if files.count() > 0 {
                    return CliMessage::Error(format!(
                        "The directory specified is not empty: {:?}",
                        abs_path
                    ));
                }
            } else {
                return CliMessage::Error(format!(
                    "Could not read the directory specified: {:?}",
                    abs_path
                ));
            }
        } else {
            if let Err(_) = std::fs::create_dir(&abs_path) {
                return CliMessage::Error(String::from(
                    "Could not create the directory specified. Check permissions and try again.",
                ));
            }
        }

        let mut file = File::create(abs_path.join("blastf.toml")).unwrap();
        if let Err(_) =
            file.write_all(format!("[package]\nname = \"{name}\"\nversion = \"0.1.0\"").as_ref())
        {
            return CliMessage::Error(String::from("[ERROR]: Could not write blastf.toml."));
        }
        if let Err(_) = std::fs::create_dir(&abs_path.join("./src")) {
            return CliMessage::Error(String::from(
                "Could not create the directory specified. Check permissions and try again.",
            ));
        }

        let mut file = File::create(abs_path.join("./src/main.ing")).unwrap();
        if let Err(_) = file.write(
            "pub fn main() {\n\tlet a: int = 5;\n\tprintln!(a);\n}"
                .to_string()
                .as_ref(),
        ) {
            return CliMessage::Error(String::from(
                "Could not write to file. Check permissions and try again.",
            ));
        }

        CliMessage::Message(format!("Created new project {:?} at: {:?}", name, abs_path))
    }
}
