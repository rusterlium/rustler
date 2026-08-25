use crate::{
    generator::DONE,
    parser::{ApiArg, CType},
};
use std::io::Write;

use super::{ApiBuilder, Res, render_type_args, write_ret, write_variadic_fn_type};

/// Builds the `extern "C"` block declaring direct links against the real
/// NIF API symbols, under their original names.
pub(super) struct DirectSymbolsApiBuilder<'a, W: Write>(pub(super) &'a mut W);

impl<W: Write> ApiBuilder for DirectSymbolsApiBuilder<'_, W> {
    fn init(&mut self) -> Res {
        writeln!(self.0, "extern \"C\" {{")
    }

    fn finish(&mut self) -> Res {
        writeln!(self.0, "}}\n")
    }

    fn func(&mut self, ret: &CType, name: &str, args: &[ApiArg]) -> Res {
        let args = render_type_args(args);
        write!(self.0, "    pub fn {name}({args})")?;
        write_ret(self.0, ret)?;
        writeln!(self.0, ";")
    }

    fn variadic_func(&mut self, ret: &CType, name: &str, args: &[ApiArg]) -> Res {
        let args = render_type_args(args);
        // Variadic functions also get a `{name}!` call macro (see
        // `VariadicMacroApiBuilder`), which needs the bare `{name}` value
        // namespace slot for its own `pub use {name};` self re-export.
        // Declare this one under a different Rust identifier (keeping the
        // real linked symbol name via `#[link_name]`) to avoid clashing.
        writeln!(self.0, "    #[link_name = \"{name}\"]")?;
        write!(self.0, "    fn __variadic_{name}({args}, ...)")?;
        write_ret(self.0, ret)?;
        writeln!(self.0, ";")
    }

    fn dummy(&mut self, _name: &str) -> Res {
        DONE
    }
}

/// Builds `get_{name}` accessors (outside of the `extern "C"` block) and
/// call macros for variadic functions: a variadic C function pointer can't
/// be named directly as a value, only called, so this wraps it in a plain
/// function that returns its address.
pub(super) struct DirectVariadicApiBuilder<'a, W: Write>(pub(super) &'a mut W);

impl<W: Write> ApiBuilder for DirectVariadicApiBuilder<'_, W> {
    fn func(&mut self, _ret: &CType, _name: &str, _args: &[ApiArg]) -> Res {
        DONE
    }

    fn variadic_func(&mut self, ret: &CType, name: &str, args: &[ApiArg]) -> Res {
        let args_sig = render_type_args(args);
        writeln!(self.0, "#[macro_export] macro_rules! {name} {{")?;
        writeln!(
            self.0,
            "    ( $( $arg:expr ),* ) => {{ $crate::sys::get_{name}()($($arg),*) }};"
        )?;
        writeln!(
            self.0,
            "    ( $( $arg:expr ),+, ) => {{ {name}!($($arg),*) }};"
        )?;
        writeln!(self.0, "}}\n")?;
        writeln!(self.0, "pub use {name};\n")?;

        write!(self.0, "pub unsafe fn get_{name}() -> ")?;
        write_variadic_fn_type(self.0, &args_sig, ret)?;
        writeln!(self.0, " {{")?;
        writeln!(
            self.0,
            "    std::mem::transmute(__variadic_{name} as *const ())"
        )?;
        writeln!(self.0, "}}\n")
    }

    fn dummy(&mut self, _name: &str) -> Res {
        DONE
    }
}
