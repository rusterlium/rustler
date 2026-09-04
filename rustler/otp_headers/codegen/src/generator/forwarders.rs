use std::io::Write;

use super::{
    ApiArg, ApiBuilder, CType, Res, render_arg_names, render_type_args, write_ret,
    write_variadic_fn_type,
};

/// Builds the callable forwarder functions used when the direct-symbols
/// code path isn't selected: each one calls through `DYN_NIF_CALLBACKS`,
/// which is filled in at runtime.
pub(super) struct ForwardersApiBuilder<'a, W: Write>(pub(super) &'a mut W);

impl<W: Write> ApiBuilder for ForwardersApiBuilder<'_, W> {
    fn func(&mut self, ret: &CType, name: &str, args: &[ApiArg]) -> Res {
        let args_sig = render_type_args(args);
        let args_names = render_arg_names(args);

        writeln!(
            self.0,
            "/// See [{name}](http://www.erlang.org/doc/man/erl_nif.html#{name}) in the Erlang docs."
        )?;
        writeln!(self.0, "#[inline]")?;
        writeln!(self.0, "pub unsafe extern \"C\" fn {name}({args_sig})")?;
        write_ret(self.0, ret)?;
        writeln!(self.0, "{{")?;
        writeln!(
            self.0,
            "    (DYN_NIF_CALLBACKS.{name}.unwrap_unchecked())({args_names})"
        )?;
        writeln!(self.0, "}}\n")
    }

    fn variadic_func(&mut self, ret: &CType, name: &str, args: &[ApiArg]) -> Res {
        let args_sig = render_type_args(args);

        write!(self.0, "pub unsafe fn get_{name}() -> ")?;
        write_variadic_fn_type(self.0, &args_sig, ret)?;
        writeln!(self.0, " {{")?;
        writeln!(self.0, "    DYN_NIF_CALLBACKS.{name}.unwrap_unchecked()")?;
        writeln!(self.0, "}}\n")
    }
}
