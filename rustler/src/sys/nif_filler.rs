#![allow(unused)]

#[allow(dead_code)]
pub(crate) trait DynNifFiller {
    fn write<T: Copy>(&self, field: &mut Option<T>, name: &str);
}

pub(crate) struct NoopNifFiller;

impl DynNifFiller for NoopNifFiller {
    fn write<T: Copy>(&self, field: &mut Option<T>, name: &str) {}
}

#[cfg(not(windows))]
mod dlsym_filler {
    use libc::{RTLD_GLOBAL, RTLD_NOLOAD, RTLD_NOW};
    use libloading::os::unix::Library;

    const FLAGS: i32 = RTLD_GLOBAL | RTLD_NOLOAD | RTLD_NOW;

    // Path to the shared object that contains the BEAM
    const BEAM_LOC: &str = "RUSTLER_BEAM_LIBRARY_PATH";

    pub(crate) struct DlsymNifFiller {
        lib: libloading::Library,
    }

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

    impl super::DynNifFiller for DlsymNifFiller {
        fn write<T: Copy>(&self, field: &mut Option<T>, name: &str) {
            let symbol = unsafe { self.lib.get::<T>(name.as_bytes()).unwrap() };
            *field = Some(*symbol);
        }
    }
}

// On Windows the callback table is always supplied directly by the caller
// via `internal_set_symbols`, so no dlsym-based (or otherwise) filler is
// ever actually needed there.
#[cfg(windows)]
pub(crate) fn new() -> impl DynNifFiller {
    NoopNifFiller
}

#[cfg(not(windows))]
pub(crate) fn new() -> impl DynNifFiller {
    dlsym_filler::DlsymNifFiller::new()
}
