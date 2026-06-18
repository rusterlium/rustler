#![allow(clippy::type_complexity)]

use super::{
    nif_filler::{self, DynNifFiller},
    types::*,
};

static mut DYN_NIF_CALLBACKS: DynNifCallbacks =
    unsafe { std::mem::MaybeUninit::zeroed().assume_init() };

pub unsafe fn internal_set_symbols(callbacks: DynNifCallbacks) {
    DYN_NIF_CALLBACKS = callbacks;
}

#[allow(static_mut_refs)]
pub unsafe fn internal_write_symbols() {
    let filler = nif_filler::new();
    DYN_NIF_CALLBACKS.write_symbols(filler);
}

/// See [enif_make_pid](http://erlang.org/doc/man/erl_nif.html#enif_make_pid) in the Erlang docs
pub unsafe fn enif_make_pid(_env: *mut ErlNifEnv, pid: ErlNifPid) -> ERL_NIF_TERM {
    pid.pid
}

/// See [enif_compare_pids](http://erlang.org/doc/man/erl_nif.html#enif_compare_pids) in the Erlang docs
pub unsafe fn enif_compare_pids(pid1: *const ErlNifPid, pid2: *const ErlNifPid) -> c_int {
    // Mimics the implementation of the enif_compare_pids macro
    enif_compare((*pid1).pid, (*pid2).pid)
}

macro_rules! use_snippet {
    ($version:expr) => {
        #[cfg(all(
                                            feature = $version,
                                            any(windows, all(unix, target_pointer_width = "32"))
                                        ))]
        use_snippet! {include, $version, "4"}

        #[cfg(all(
                                            feature = $version,
                                            all(unix, target_pointer_width = "64")
                                        ))]
        use_snippet! {include, $version, "8"}
    };

    (include, $version:expr, $sizeof_long:expr) => {
        include!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/otp_headers/",
            $version,
            "/",
            "api.",
            $sizeof_long,
            ".rs"
        ));
    };
}

use_snippet!("nif_version_2_18");

#[cfg(not(feature = "nif_version_2_18"))]
use_snippet!("nif_version_2_17");

#[cfg(not(feature = "nif_version_2_17"))]
use_snippet!("nif_version_2_16");

#[cfg(not(feature = "nif_version_2_16"))]
use_snippet!("nif_version_2_15");

#[cfg(not(feature = "nif_version_2_15"))]
use_snippet!("nif_version_2_14");

#[cfg(not(any(unix, windows)))]
compile_error!("rustler only supports unix and windows targets");

#[cfg(all(
    unix,
    not(any(target_pointer_width = "32", target_pointer_width = "64"))
))]
compile_error!("rustler only supports 32-bit or 64-bit unix targets");
