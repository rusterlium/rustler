use crate::parser::{ApiArg, CBaseType, CPrimitiveType, CType};
use std::io::Write;

use super::{ApiBuilder, Res, render_type_args, write_fn_type, write_variadic_fn_type};

pub(super) struct CallbacksApiBuilder<'a, W: Write>(pub(super) &'a mut W);

impl<W: Write> ApiBuilder for CallbacksApiBuilder<'_, W> {
    fn init(&mut self) -> Res {
        writeln!(self.0, "#[allow(dead_code)]")?;
        writeln!(self.0, "#[derive(Default, Copy, Clone)]")?;
        writeln!(self.0, "pub struct DynNifCallbacks {{")
    }

    fn finish(&mut self) -> Res {
        writeln!(self.0, "}}")
    }

    fn func(&mut self, ret: &CType, name: &str, args: &[ApiArg]) -> Res {
        let args = render_type_args(args);
        write!(self.0, "    {name}: Option<")?;
        write_fn_type(self.0, &args, ret)?;
        writeln!(self.0, ">,")
    }

    fn variadic_func(&mut self, ret: &CType, name: &str, args: &[ApiArg]) -> Res {
        let args = render_type_args(args);
        write!(self.0, "    {name}: Option<")?;
        write_variadic_fn_type(self.0, &args, ret)?;
        writeln!(self.0, ">,")
    }
    fn dummy(&mut self, name: &str) -> Res {
        write!(self.0, "    {name}: Option<")?;
        write_fn_type(
            self.0,
            "",
            &CType::Base {
                base: CBaseType::Primitive(CPrimitiveType::Void),
                is_const: false,
            },
        )?;
        writeln!(self.0, ">,")
    }
}
