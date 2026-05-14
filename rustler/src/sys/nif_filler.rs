pub(crate) trait DynNifFiller {
    fn write<T: Copy>(&self, field: &mut Option<T>, name: &str);
}

#[cfg(all(not(target_os = "windows"), not(target_os = "android")))]
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
        fn write<T: Copy>(&self, field: &mut Option<T>, name: &str) {
            let symbol = unsafe { self.lib.get::<T>(name.as_bytes()).unwrap() };
            *field = Some(*symbol);
        }
    }

    pub(crate) fn new() -> impl DynNifFiller {
        DlsymNifFiller::new()
    }
}

// Android: on Bionic, neither `dlopen(NULL, ...)` nor the `RTLD_DEFAULT`
// pseudo-handle traverses RTLD_GLOBAL-promoted application libraries.
// `dlopen(NULL)` returns a handle to the main executable namespace
// (typically `app_process` for JVM-hosted apps); `RTLD_DEFAULT` walks
// the loader's default search order but does not include siblings that
// were loaded RTLD_LOCAL by `System.loadLibrary` and only later
// upgraded via a host-side `dlopen(self, RTLD_GLOBAL)` trick (which
// Bionic doesn't honour for symbol resolution).
//
// The path that *does* work: find the .so file containing this rustler
// code (via `dladdr` of one of our own functions), then dlopen that
// path with RTLD_NOLOAD so we get an explicit handle to it. dlsym on
// that handle DOES find the statically-linked-in BEAM exports.
#[cfg(target_os = "android")]
mod internal {

    use std::os::raw::{c_char, c_int, c_void};

    use super::DynNifFiller;

    const RTLD_NOW: c_int = 0x00002;
    const RTLD_NOLOAD: c_int = 0x00004;

    #[repr(C)]
    struct DlInfo {
        dli_fname: *const c_char,
        dli_fbase: *mut c_void,
        dli_sname: *const c_char,
        dli_saddr: *mut c_void,
    }

    extern "C" {
        fn dladdr(addr: *const c_void, info: *mut DlInfo) -> c_int;
        fn dlopen(filename: *const c_char, flag: c_int) -> *mut c_void;
        fn dlsym(handle: *mut c_void, symbol: *const c_char) -> *mut c_void;
        fn dlerror() -> *mut c_char;
    }

    pub(crate) struct DlsymNifFiller {
        handle: *mut c_void,
    }

    // Safe to share — the handle is opened once and only used for dlsym
    // reads, which are thread-safe per POSIX.
    unsafe impl Send for DlsymNifFiller {}
    unsafe impl Sync for DlsymNifFiller {}

    impl DlsymNifFiller {
        pub(crate) fn new() -> Self {
            // Get the file path of the shared object we're inside. `Self::new`
            // is statically linked into the same .so as the rest of rustler
            // (and as libbeam.a's enif_* exports — that's why this works).
            let mut info = DlInfo {
                dli_fname: std::ptr::null(),
                dli_fbase: std::ptr::null_mut(),
                dli_sname: std::ptr::null(),
                dli_saddr: std::ptr::null_mut(),
            };
            let probe_addr = Self::new as *const c_void;
            let rc = unsafe { dladdr(probe_addr, &mut info) };
            if rc == 0 || info.dli_fname.is_null() {
                panic!(
                    "rustler: dladdr() failed to identify the calling .so on \
                     Android. Cannot open a self-handle for enif_* lookups."
                );
            }

            // dlopen with RTLD_NOLOAD: don't reload the library (we're
            // already running in it), just return a handle to the existing
            // load. The handle's dlsym() scope is *this specific library*,
            // which is exactly what we want for enif_* lookups.
            let handle = unsafe { dlopen(info.dli_fname, RTLD_NOW | RTLD_NOLOAD) };
            if handle.is_null() {
                let err = unsafe { dlerror() };
                let err_str = if err.is_null() {
                    String::from("unknown")
                } else {
                    unsafe { std::ffi::CStr::from_ptr(err).to_string_lossy().into_owned() }
                };
                let fname = unsafe {
                    std::ffi::CStr::from_ptr(info.dli_fname)
                        .to_string_lossy()
                        .into_owned()
                };
                panic!(
                    "rustler: dlopen({:?}, RTLD_NOW | RTLD_NOLOAD) failed: {}",
                    fname, err_str
                );
            }

            DlsymNifFiller { handle }
        }
    }

    impl DynNifFiller for DlsymNifFiller {
        fn write<T: Copy>(&self, field: &mut Option<T>, name: &str) {
            // rustler's caller passes `name` with a trailing NUL byte
            // already in it (so the same string can be handed to dlsym
            // without a copy). `CString::new` would reject that as
            // "interior nul" — convert as already-C-string instead.
            let bytes = name.as_bytes();
            debug_assert!(
                bytes.ends_with(b"\0"),
                "rustler: expected name with trailing NUL; got {:?}",
                name
            );
            let raw = unsafe { dlsym(self.handle, bytes.as_ptr() as *const c_char) };
            if raw.is_null() {
                panic!(
                    "rustler: dlsym({:?}) on self-handle returned null. \
                     The BEAM's enif_* exports are not visible inside the \
                     calling .so. Has the host process linked libbeam.a \
                     into the .so containing rustler?",
                    name
                );
            }
            let sym: T = unsafe { *(&raw as *const _ as *const T) };
            *field = Some(sym);
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
