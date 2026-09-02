#![allow(unused)]

#[allow(dead_code)]
pub(crate) trait DynNifFiller {
    fn write<T: Copy>(&self, field: &mut Option<T>, name: &str);
}

#[cfg(not(windows))]
use libc::{RTLD_GLOBAL, RTLD_NOLOAD, RTLD_NOW};
#[cfg(not(windows))]
use libloading::os::unix::Library;

#[cfg(not(windows))]
const FLAGS: i32 = RTLD_GLOBAL | RTLD_NOLOAD | RTLD_NOW;

// Path to the shared object that contains the BEAM
#[cfg(not(windows))]
const BEAM_LOC: &str = "RUSTLER_BEAM_LIBRARY_PATH";

#[cfg(not(windows))]
pub(crate) struct DlsymNifFiller {
    lib: libloading::Library,
}

#[cfg(not(windows))]
impl DlsymNifFiller {
    pub(crate) fn new() -> Self {
        let beam_location = match std::env::var(BEAM_LOC) {
            Ok(val) if !val.is_empty() => Some(val),
            _ => None,
        };
        let lib = unsafe { Library::open(beam_location, FLAGS) };
        DlsymNifFiller {
            lib: lib.unwrap().into(),
        }
    }
}

#[cfg(not(windows))]
impl DynNifFiller for DlsymNifFiller {
    fn write<T: Copy>(&self, field: &mut Option<T>, name: &str) {
        let symbol = unsafe { self.lib.get::<T>(name.as_bytes()).unwrap() };
        *field = Some(*symbol);
    }
}

// On Windows the callback table is always supplied directly by the caller
// via `internal_set_symbols`, so no dlsym-based (or otherwise) filler is
// ever actually needed there.
#[cfg(not(windows))]
pub(crate) fn new() -> impl DynNifFiller {
    DlsymNifFiller::new()
}
