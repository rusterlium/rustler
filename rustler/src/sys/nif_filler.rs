pub(crate) trait DynNifFiller {
    fn write<T: Copy>(&self, field: &mut Option<T>, name: &[u8]);
}

pub struct NullNifFiller;
impl DynNifFiller for NullNifFiller {
    fn write<T: Copy>(&self, _field: &mut Option<T>, _name: &[u8]) {}
}

#[cfg(not(target_os = "windows"))]
mod internal {
    use std::ffi::OsStr;

    use super::DynNifFiller;
    use libloading::os::unix::{Library, RTLD_GLOBAL, RTLD_NOW};

    pub(crate) struct DlsymNifFiller {
        lib: libloading::Library,
    }

    impl DlsymNifFiller {
        pub(crate) fn new() -> Self {
            let lib = unsafe { Library::open(None::<&OsStr>, RTLD_NOW | RTLD_GLOBAL) };
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

    pub fn new() -> impl DynNifFiller {
        NullNifFiller
    }
}

pub(crate) use internal::new;
