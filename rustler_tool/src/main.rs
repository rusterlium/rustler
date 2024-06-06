#[cfg(unix)]
mod fake_symbols;
mod nif;
mod nif_elixir;
mod nif_erlang;

use std::path::PathBuf;

use clap::{Parser, Subcommand};

use crate::nif::NifLibrary;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(clap::ValueEnum, Clone, Default, Debug)]
enum OutputFormat {
    #[default]
    Bare,
    Erlang,
    Elixir,
}

#[derive(Subcommand)]
enum Commands {
    /// does testing things
    Nif {
        path: PathBuf,
        #[arg(short, long, default_value_t, value_enum)]
        format: OutputFormat,
    },
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::Nif { path, format }) => {
            let lib = NifLibrary::load(path).unwrap();

            match format {
                OutputFormat::Bare => {
                    println!("{}", lib.name);
                    for nif in lib.nifs {
                        println!("  {}/{}", nif.name, nif.arity);
                    }
                }
                OutputFormat::Erlang => {
                    println!("{}", nif_erlang::LibAsErlang(lib))
                }
                OutputFormat::Elixir => {
                    println!("{}", nif_elixir::LibAsElixir(lib))
                }
            }
        }
        None => {
            panic!("No command given")
        }
    }
}
