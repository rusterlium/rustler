use crate::NifLibrary;
use std::fmt;

pub struct LibAsErlang(pub NifLibrary);

impl fmt::Display for LibAsErlang {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let nif_lib = &self.0;

        let module_name = string_to_erlang_atom(&nif_lib.name);

        let nifs: Vec<_> = nif_lib
            .nifs
            .iter()
            .enumerate()
            .map(|(n, nif)| (n, string_to_erlang_atom(&nif.name), nif.arity))
            .collect();

        let nif_name = nif_lib
            .path
            .with_extension("")
            .file_name()
            .unwrap()
            .to_string_lossy()
            .to_string();

        writeln!(f, "-module({module_name}).\n")?;
        writeln!(f, "-export([")?;
        let count = nifs.len();
        for (n, name, arity) in &nifs {
            write!(f, "    {name}/{arity}")?;
            if *n != count - 1 {
                write!(f, ",")?;
            }
            writeln!(f)?;
        }
        write!(f, "]).\n\n")?;

        writeln!(f, "-nifs([")?;
        for (n, name, arity) in &nifs {
            write!(f, "    {name}/{arity}")?;
            if *n != count - 1 {
                write!(f, ",")?;
            }
            writeln!(f)?;
        }
        write!(f, "]).\n\n")?;

        writeln!(f, "-define(NIF_LOAD_INFO, 0).")?;
        writeln!(f, "-define(NIF_NAME, \"{}\").", &nif_name)?;
        write!(f, "{}", include_str!("../snippets/on_load.erl"))?;

        for (_, name, arity) in &nifs {
            write!(f, "{name}(")?;
            for i in 0..*arity {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "_")?;
            }
            write!(f, ") ->\n    erlang:nif_error(not_loaded).\n\n")?;
        }

        Ok(())
    }
}

fn string_to_erlang_atom(input: &str) -> String {
    let mut output = String::with_capacity(input.len());
    let mut needs_quotes = false;

    let mut first = true;

    for c in input.chars() {
        match c {
            'A'..='Z' if first => {
                needs_quotes = true;
                output.push(c);
            }
            'a'..='z' | 'A'..='Z' | '0'..='9' | '@' | '_' => output.push(c),
            '\'' => {
                needs_quotes = true;
                output.push_str(r"\'");
            }
            '\\' => {
                needs_quotes = true;
                output.push_str(r"\\");
            }
            _ => {
                needs_quotes = true;
                output.push(c);
            }
        }

        first = false;
    }

    if needs_quotes {
        format!("'{output}'").to_string()
    } else {
        output
    }
}
