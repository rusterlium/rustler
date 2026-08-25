use crate::parser::{ApiArg, CType};
use std::io::Write;

use super::{ApiBuilder, DONE, Res};

/// Builds `DynNifCallbacks::write_symbols`, which fills in the callback
/// table at runtime (e.g. via `dlsym`). Part of the `Main` emit, used only
/// when the direct-symbols code path isn't selected (see `sys/functions.rs`).
pub(super) struct WriterBuilder<'a, W: Write>(pub(super) &'a mut W);

impl<W: Write> ApiBuilder for WriterBuilder<'_, W> {
    fn init(&mut self) -> Res {
        write!(
            self.0,
            "impl DynNifCallbacks {{\n    fn write_symbols<T: DynNifFiller>(&mut self, filler: T) {{\n"
        )
    }

    fn finish(&mut self) -> Res {
        writeln!(self.0, "    }}\n}}")
    }

    fn func(&mut self, _ret: &CType, name: &str, _args: &[ApiArg]) -> Res {
        writeln!(
            self.0,
            "        filler.write(&mut self.{name}, \"{name}\0\");"
        )
    }
    fn variadic_func(&mut self, ret: &CType, name: &str, args: &[ApiArg]) -> Res {
        self.func(ret, name, args)
    }
    fn dummy(&mut self, _name: &str) -> Res {
        DONE
    }
}
