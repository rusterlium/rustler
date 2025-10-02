use crate::NifLibrary;
use std::fmt;

pub struct LibAsElixir(pub NifLibrary);

impl fmt::Display for LibAsElixir {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            f,
            "defmodule {} do",
            string_to_elixir_atom(&self.0.module_name, false)
        )?;

        for nif in &self.0.nifs {
            write!(f, "  def {}(", string_to_elixir_atom(&nif.name, true))?;
            for i in 0..nif.arity {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "_")?;
            }
            writeln!(f, "), do: :erlang.nif_error(not_loaded)")?;
        }
        writeln!(f, "end")
    }
}

fn string_to_elixir_atom(s: &str, func: bool) -> String {
    match s {
        "false" | "true" | "nil" => s.to_string(),
        _ if s.starts_with("Elixir.") => s[7..].to_string(),
        _ => {
            let mut output = String::new();
            let mut needs_quotes = false;
            for c in s.chars() {
                match c {
                    'a'..='z' | 'A'..='Z' | '0'..='9' | '@' | '_' => output.push(c),
                    '"' => {
                        needs_quotes = true;
                        output.push_str("\\\"");
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
            }

            if needs_quotes {
                format!(":\"{output}\"").to_string()
            } else if !func {
                format!(":{output}").to_string()
            } else {
                output
            }
        }
    }
}
