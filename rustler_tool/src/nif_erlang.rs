use crate::NifLibrary;
use std::fmt;

pub struct LibAsErlang(pub NifLibrary);

impl fmt::Display for LibAsErlang {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "-module({}).\n\n", string_to_erlang_atom(&self.0.name))?;
        writeln!(f, "-export([")?;
        let count = self.0.nifs.len();
        for (n, nif) in self.0.nifs.iter().enumerate() {
            write!(f, "    {}/{}", string_to_erlang_atom(&nif.name), nif.arity)?;
            if n == count - 1 {
                write!(f, ",")?;
            }
            writeln!(f)?;
        }
        write!(f, "]).\n\n")?;

        // TODO: On Load function

        for nif in &self.0.nifs {
            write!(f, "{}(", string_to_erlang_atom(&nif.name))?;
            for i in 0..nif.arity {
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
        format!("'{}'", output).to_string()
    } else {
        output
    }
}
