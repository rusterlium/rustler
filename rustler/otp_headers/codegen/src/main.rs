mod generator;
mod overrides;
mod parser;

use generator::{Emit, GenerateOptions, generate};
use std::fs;
use std::io::{self, Read};
use std::path::Path;

fn parse_ulong_size_arg(value: &str) -> u8 {
    match value {
        "4" => 4,
        "8" => 8,
        _ => panic!("Invalid --ulong-size value `{value}`; expected 4 or 8"),
    }
}

fn parse_emit_arg(value: &str) -> Emit {
    match value {
        "main" => Emit::Main,
        "direct" => Emit::Direct,
        _ => panic!("Invalid --emit value `{value}`; expected main or direct"),
    }
}

fn main() {
    let args = std::env::args().skip(1);
    let mut header_path: Option<String> = None;
    let mut ulong_size: Option<u8> = None;
    let mut emit: Option<Emit> = None;

    for arg in args {
        if let Some(value) = arg.strip_prefix("--ulong-size=") {
            let value = parse_ulong_size_arg(value);
            if ulong_size.replace(value).is_some() {
                panic!("--ulong-size provided more than once");
            }
            continue;
        }

        if let Some(value) = arg.strip_prefix("--emit=") {
            let value = parse_emit_arg(value);
            if emit.replace(value).is_some() {
                panic!("--emit provided more than once");
            }
            continue;
        }

        if arg.starts_with('-') {
            panic!("Unknown option `{arg}`");
        }

        if header_path.replace(arg).is_some() {
            panic!("Expected at most one header-file argument");
        }
    }

    let declarations_source = if let Some(header_path) = header_path.as_ref() {
        let path = Path::new(&header_path);
        fs::read_to_string(path)
            .unwrap_or_else(|err| panic!("Failed to read `{}`: {err}", path.display()))
    } else {
        let mut declarations_source = String::new();
        io::stdin()
            .read_to_string(&mut declarations_source)
            .unwrap_or_else(|err| panic!("Failed to read stdin: {err}"));
        declarations_source
    };

    let ulong_size = ulong_size.expect("Missing --ulong-size=<4|8>");
    let emit = emit.unwrap_or(Emit::Main);

    let opts = GenerateOptions {
        declarations_source,
        ulong_size: ulong_size as usize,
        emit,
    };

    let mut out = std::io::stdout();
    generate(&mut out, &opts).unwrap_or_else(|err| panic!("Failed to write to output: {err}"));
}
