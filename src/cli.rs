use std::path::PathBuf;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Cli {
    /// The path to the source file
    #[arg(value_name = "FILE")]
    pub source_path: PathBuf,
}
