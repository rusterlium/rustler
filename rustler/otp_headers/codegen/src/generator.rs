mod callbacks;
mod direct;
mod forwarders;
mod writer;

use crate::overrides::apply_api_signature_fixes;
use crate::parser::{
    ApiArg, CBaseType, CParam, CPrimitiveType, CType, parse_api_declarations_source,
};
use callbacks::CallbacksApiBuilder;
use direct::{DirectSymbolsApiBuilder, DirectVariadicApiBuilder};
use forwarders::ForwardersApiBuilder;
use std::fmt::Write as _;
use std::io::Write;
use writer::WriterBuilder;

/// Which complete API implementation to emit: `Main` is the default,
/// callback-table-based implementation (symbols filled in at runtime, e.g.
/// via `dlsym`); `Direct` links directly against the real NIF API symbols
/// under their original names. Both are self-contained (each includes the
/// shared consts/macros/aliases), so `sys` can pick whichever one it needs
/// via `cfg` with a single `include!` and no extra module wrapping.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Emit {
    Main,
    Direct,
}

pub struct GenerateOptions {
    pub declarations_source: String,
    pub ulong_size: usize,
    pub emit: Emit,
}

type Res = std::io::Result<()>;
const DONE: Res = Res::Ok(());

trait ApiBuilder {
    fn init(&mut self) -> Res {
        DONE
    }
    fn finish(&mut self) -> Res {
        DONE
    }

    fn func(&mut self, ret: &CType, name: &str, args: &[ApiArg]) -> Res;
    fn variadic_func(&mut self, ret: &CType, name: &str, args: &[ApiArg]) -> Res;
    fn dummy(&mut self, name: &str) -> Res;
}

fn render_arg_name(name: &str) -> String {
    // Overrides for reserved keywords
    match name {
        "ref" | "type" | "mod" => format!("{name}_"),
        _ => name.to_string(),
    }
}

fn render_arg_names(args: &[ApiArg]) -> String {
    args.iter()
        .map(|arg| render_arg_name(&arg.name))
        .collect::<Vec<_>>()
        .join(", ")
}

fn render_type_args(args: &[ApiArg]) -> String {
    args.iter()
        .map(|arg| {
            format!(
                "{}: {}",
                render_arg_name(&arg.name),
                render_rust_type(&arg.ty)
            )
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn render_return_type(ret: &CType) -> String {
    let ret = render_rust_type(ret);
    if ret == "c_void" { String::new() } else { ret }
}

fn write_ret<W: Write>(out: &mut W, ret: &CType) -> Res {
    let ret = render_return_type(ret);
    if !ret.is_empty() {
        write!(out, " -> {ret}")?;
    }
    DONE
}

fn write_fn_type<W: Write>(out: &mut W, args: &str, ret: &CType) -> Res {
    write!(out, "extern \"C\" fn ({args})")?;
    write_ret(out, ret)
}

fn write_variadic_fn_type<W: Write>(out: &mut W, args: &str, ret: &CType) -> Res {
    write!(out, "extern \"C\" fn ({args}, ...)")?;
    write_ret(out, ret)
}

fn render_rust_type(ty: &CType) -> String {
    match ty {
        CType::Base { base, .. } => render_rust_base_type(base),
        CType::Pointer { pointee, .. } => {
            if let CType::Function {
                ret,
                params,
                variadic,
            } = pointee.as_ref()
            {
                return format!(
                    "Option<{}>",
                    render_function_type(ret, params, *variadic, true)
                );
            }
            format!(
                "{} {}",
                if pointee.is_const_qualified() {
                    "*const"
                } else {
                    "*mut"
                },
                render_rust_type(pointee)
            )
        }
        CType::Function {
            ret,
            params,
            variadic,
        } => render_function_type(ret, params, *variadic, false),
    }
}

fn render_rust_base_type(base: &CBaseType) -> String {
    match base {
        CBaseType::Primitive(primitive) => match primitive {
            CPrimitiveType::Void => "c_void".into(),
            CPrimitiveType::Char => "c_char".into(),
            CPrimitiveType::UnsignedChar => "c_uchar".into(),
            CPrimitiveType::Int => "c_int".into(),
            CPrimitiveType::UnsignedInt => "c_uint".into(),
            CPrimitiveType::Long => "c_long".into(),
            CPrimitiveType::UnsignedLong => "c_ulong".into(),
            CPrimitiveType::Double => "c_double".into(),
        },
        CBaseType::Named(name) => match name.as_str() {
            "FILE" => "c_void".into(),
            "ErlNifSInt64" => "i64".into(),
            "ErlNifUInt64" => "u64".into(),
            _ => name.clone(),
        },
        CBaseType::Enum(name) => name.clone(),
    }
}

fn render_function_type(ret: &CType, params: &[CParam], variadic: bool, spaced: bool) -> String {
    let mut out = format!(
        "unsafe extern \"C\" fn{}({})",
        if spaced { " " } else { "" },
        params
            .iter()
            .map(|param| render_rust_type(&param.ty))
            .collect::<Vec<_>>()
            .join(", ")
    );
    if variadic {
        if !params.is_empty() {
            out.push_str(", ");
        }
        out.push_str("...");
    }
    let ret = render_return_type(ret);
    if !ret.is_empty() {
        write!(out, " -> {ret}").unwrap();
    }
    out
}

pub fn generate<W: Write>(out: &mut W, opts: &GenerateOptions) -> Res {
    writeln!(
        out,
        "pub const ERL_NIF_ENTRY_OPTIONS: c_uint = ERL_NIF_DIRTY_NIF_OPTION;"
    )?;

    match opts.emit {
        Emit::Main => {
            build_api(&mut CallbacksApiBuilder(out), opts)?;
            build_api(&mut ForwardersApiBuilder(out), opts)?;
            build_api(&mut WriterBuilder(out), opts)?;
        }
        Emit::Direct => {
            build_api(&mut DirectSymbolsApiBuilder(out), opts)?;
            build_api(&mut DirectVariadicApiBuilder(out), opts)?;
        }
    }

    if opts.ulong_size != 4 {
        write!(
            out,
            r#"
/// See [enif_make_int64](http://www.erlang.org/doc/man/erl_nif.html#enif_make_int64) at erlang.org
#[inline]
pub unsafe fn enif_make_int64(env: *mut ErlNifEnv, i: i64) -> ERL_NIF_TERM
    {{ enif_make_long(env, i) }}

/// See [enif_make_uint64](http://www.erlang.org/doc/man/erl_nif.html#enif_make_uint64) at erlang.org
#[inline]
pub unsafe fn enif_make_uint64(env: *mut ErlNifEnv, i: u64) -> ERL_NIF_TERM
    {{ enif_make_ulong(env, i) }}

/// See [enif_get_int64](http://www.erlang.org/doc/man/erl_nif.html#enif_get_int64) at erlang.org
#[inline]
pub unsafe fn enif_get_int64(env: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut i64) -> c_int
    {{ enif_get_long(env, term, ip) }}

/// See [enif_get_uint64](http://www.erlang.org/doc/man/erl_nif.html#enif_get_uint64) at erlang.org
#[inline]
pub unsafe fn enif_get_uint64(env: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut u64) -> c_int
    {{ enif_get_ulong(env, term, ip) }}
        "#
        )?;
    }

    DONE
}

fn build_api(b: &mut dyn ApiBuilder, opts: &GenerateOptions) -> Res {
    b.init()?;
    for mut decl in parse_api_declarations_source(&opts.declarations_source) {
        apply_api_signature_fixes(&mut decl);
        if is_skipped_api(&decl.name) {
            b.dummy(&decl.name)?;
        } else if decl.variadic {
            b.variadic_func(&decl.ret, &decl.name, &decl.args)?;
        } else {
            b.func(&decl.ret, &decl.name, &decl.args)?;
        }
    }
    b.finish()
}

fn is_skipped_api(name: &str) -> bool {
    name.starts_with("enif_mutex_")
        || name.starts_with("enif_cond_")
        || name.starts_with("enif_rwlock_")
        || name.starts_with("enif_tsd_")
        || (name.starts_with("enif_thread_") && name != "enif_thread_type")
        || name == "enif_equal_tids"
        || name == "enif_vsnprintf"
        || name == "enif_vfprintf"
}
