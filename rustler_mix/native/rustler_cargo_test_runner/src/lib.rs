#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate rustler;

extern crate cargo;

use rustler::schedule::SchedulerFlags;
use rustler::{Encoder, Env, NifResult, Term};

use cargo::core::shell;
use cargo::core::Workspace;
use cargo::ops;
use cargo::util::config;

use std::path::PathBuf;

mod atoms {
    rustler_atoms! {
        atom ok;
        atom failed;
    }
}

rustler_export_nifs! {
    "Elixir.Rustler.CargoTestRunner",
    [("run_tests", 3, run_tests, SchedulerFlags::DirtyCpu)],
    None
}

// NOTE: this might take a long time if Rust needs to compile first
fn run_tests<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let cwd: String = args[0].decode()?;
    let homedir: String = args[1].decode()?;
    let manifest_path: String = args[2].decode()?;
    let manifest_path: PathBuf = manifest_path.into();

    let shell = shell::Shell::new();
    let config = config::Config::new(shell, cwd.into(), homedir.into());
    let ws = match Workspace::new(manifest_path.as_path(), &config) {
        Ok(ws) => ws,
        Err(err) => {
            eprintln!("Failed to create Workspace: {:?}", err);
            return Ok(atoms::failed().encode(env));
        }
    };

    let compile_opts = ops::CompileOptions::default(&config, ops::CompileMode::Test);
    let options = ops::TestOptions {
        compile_opts: compile_opts,
        no_run: false,
        no_fail_fast: false,
        only_doc: false,
    };

    // TODO test_args ignored for now
    let test_args: Vec<String> = vec![];

    match ops::run_tests(&ws, &options, &test_args) {
        Ok(None) => Ok(atoms::ok().encode(env)),
        Ok(Some(_err)) => {
            return Ok(atoms::failed().encode(env));
        }
        Err(err) => {
            eprintln!("An unexpected error:\n{:?}", err);
            return Ok(atoms::failed().encode(env));
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn success() {
        assert!(true);
    }
}
