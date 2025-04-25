use crate::nif_types::ErlNifEntry;
use libloading::{Library, Symbol};
use std::ffi::CStr;
use std::path::{Path, PathBuf};

pub struct Nif {
    pub name: String,
    pub arity: usize,
    pub _flags: usize,
}

pub struct NifLibrary {
    pub path: PathBuf,
    pub name: String,
    pub nifs: Vec<Nif>,
}

#[cfg(unix)]
unsafe fn maybe_call_nif_init(
    lib: &Library,
) -> Result<*const ErlNifEntry, Box<dyn std::error::Error>> {
    let func: Symbol<unsafe extern "C" fn() -> *const ErlNifEntry> = lib.get(b"nif_init")?;

    Ok(func())
}

#[cfg(windows)]
unsafe fn maybe_call_nif_init(
    lib: &Library,
) -> Result<*const ErlNifEntry, Box<dyn std::error::Error>> {
    static mut NULL_CALLBACKS: [usize; 1024] = [0; 1024];
    // enif_priv_data
    NULL_CALLBACKS[0] = 0;
    // enif_alloc
    NULL_CALLBACKS[1] = crate::fake_symbols::enif_alloc as *mut c_void as usize;
    // enif_free
    NULL_CALLBACKS[2] = crate::fake_symbols::enif_free as *mut c_void as usize;

    let func: Symbol<unsafe extern "C" fn(*mut [usize; 1024]) -> *const ErlNifEntry> =
        lib.get(b"nif_init")?;

    Ok(func(std::ptr::addr_of_mut!(NULL_CALLBACKS)))
}

impl NifLibrary {
    pub fn load(path: &Path) -> Result<NifLibrary, Box<dyn std::error::Error>> {
        unsafe {
            let lib = Library::new(path)?;
            let entry = maybe_call_nif_init(&lib)?;

            let name = CStr::from_ptr((*entry).name).to_str()?.to_string();
            let nif_array =
                std::slice::from_raw_parts((*entry).funcs, (*entry).num_of_funcs as usize);

            let mut nifs: Vec<_> = nif_array
                .iter()
                .filter_map(|f| {
                    Some(Nif {
                        name: CStr::from_ptr(f.name).to_str().ok()?.to_string(),
                        arity: f.arity as usize,
                        _flags: f.flags as usize,
                    })
                })
                .collect();

            nifs.sort_by_key(|x| x.name.clone());

            Ok(NifLibrary {
                path: path.to_path_buf(),
                name,
                nifs,
            })
        }
    }
}
