use std::io::Write;

use super::{ApiArg, ApiBuilder, CType, Res};

/// Builds call macros for variadic functions: a variadic C function pointer
/// can't be named directly as a value, only called, so this wraps it in a plain
/// function that returns its address.
pub(super) struct VariadicApiBuilder<'a, W: Write>(pub(super) &'a mut W);

impl<W: Write> ApiBuilder for VariadicApiBuilder<'_, W> {
    fn variadic_func(&mut self, _: &CType, name: &str, _: &[ApiArg]) -> Res {
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
        writeln!(self.0, "pub use {name};\n")
    }
}
