mod erl_build;
#[cfg(unix)]
mod fake_symbols;
mod nif;
mod nif_elixir;
mod nif_erlang;
mod nif_types;
mod rust_build;

use std::path::PathBuf;

use clap::{Parser, Subcommand};
use rust_build::BuildArgs;

use crate::nif::NifLibrary;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
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

    Build(BuildArgs),
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Nif { path, format } => {
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
        Commands::Build(args) => {
            println!("Building NIFs in {:?}", args.path);
            let paths = rust_build::build(args);

            println!("Got NIFs: {paths:?}");

            for path in paths {
                let lib = NifLibrary::load(&path).unwrap();
                let erlang = nif_erlang::LibAsErlang(lib);
                let module = format!("{erlang}");
                let module_name = erlang.0.name;

                erl_build::build(&module_name, &module, &args.out);
            }
        }
    }
}
