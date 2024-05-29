#[cfg(unix)]
mod fake_symbols;
mod nif;

use std::path::PathBuf;

use clap::{Parser, Subcommand};

use crate::nif::NifLibrary;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// does testing things
    Nif { path: PathBuf },
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::Nif { path }) => {
            println!("Extracting nifs from {:?}", path);

            let lib = NifLibrary::load(&path).unwrap();

            println!("Found library {} with nifs", lib.name);
            for nif in lib.nifs {
                println!("  {}/{}", nif.name, nif.arity);
            }
        }
        None => {
            panic!("No command given")
        }
    }
}
