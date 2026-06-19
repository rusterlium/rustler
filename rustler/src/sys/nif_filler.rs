pub(crate) trait DynNifFiller {
    fn write<T: Copy>(&self, field: &mut Option<T>, name: &[u8]);
}

#[cfg(not(target_os = "windows"))]
mod internal {
    use super::DynNifFiller;
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

    impl DynNifFiller for DlsymNifFiller {
        fn write<T: Copy>(&self, field: &mut Option<T>, name: &[u8]) {
            let symbol = unsafe { self.lib.get::<T>(name).unwrap() };
            *field = Some(*symbol);
        }
    }

    pub(crate) fn new() -> impl DynNifFiller {
        DlsymNifFiller::new()
    }
}

#[cfg(target_os = "windows")]
mod internal {
    use super::*;

    pub struct NullNifFiller;
    impl DynNifFiller for NullNifFiller {
        fn write<T: Copy>(&self, _field: &mut Option<T>, _name: &str) {}
    }

    pub fn new() -> impl DynNifFiller {
        NullNifFiller
    }
}

pub(crate) use internal::new;
