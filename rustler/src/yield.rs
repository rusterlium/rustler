use crate::{
    codegen_runtime::{NifReturnable, NifReturned},
    nif::RawFunc,
    schedule::SchedulerFlags,
    Env, Nif, Term,
};

/// If a function needs to break up long-running work, it can return
/// a `Yield`. The runtime will resume execution by calling the provided
/// NIF.
///
/// ```rust
/// use rustler::{Encoder, Env, Yield};
///
/// #[rustler::nif]
/// fn start_expensive_work(env: Env) -> Yield {
///     let args = vec![42_i32.encode(env)];
///     Yield::to(perform_expensive_work, args)
/// }
///
/// #[rustler::nif]
/// fn perform_expensive_work(env: Env, value: i32) {
///     assert_eq!(value, 42);
/// }
/// ```
pub struct Yield<'a> {
    fun_name: *const i8,
    fun: RawFunc,
    flags: SchedulerFlags,
    args: Vec<Term<'a>>,
}

impl<'a> Yield<'a> {
    /// Yield back to the runtime with the provided nif.
    pub fn to<N: Nif>(nif: N, args: Vec<Term<'a>>) -> Self {
        Self::to_with_flags(nif, SchedulerFlags::Normal, args)
    }

    /// Yield back to the runtime with the provided function and scheduler flags.
    pub fn to_with_flags<N: Nif>(nif: N, flags: SchedulerFlags, args: Vec<Term<'a>>) -> Self {
        let _ = nif;
        Self {
            fun_name: N::NAME,
            fun: N::RAW_FUNC,
            flags,
            args,
        }
    }
}

unsafe impl<'a> NifReturnable for Yield<'a> {
    unsafe fn into_returned(self, _env: Env) -> NifReturned {
        NifReturned::Reschedule {
            fun_name: self.fun_name,
            fun: self.fun,
            flags: self.flags,
            args: self.args.into_iter().map(|term| term.as_c_arg()).collect(),
        }
    }
}
