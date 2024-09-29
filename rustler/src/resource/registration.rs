use super::traits;
use super::util::align_alloced_mem_for_struct;
use super::ResourceInitError;
use crate::env::EnvKind;
use crate::sys::{
    c_char, c_void, ErlNifEnv, ErlNifMonitor, ErlNifPid, ErlNifResourceDown, ErlNifResourceDtor,
    ErlNifResourceFlags, ErlNifResourceType, ErlNifResourceTypeInit,
};
use crate::{Env, LocalPid, Monitor, Resource};
use std::any::TypeId;
use std::ffi::CString;
use std::mem::MaybeUninit;
use std::ptr;

#[derive(Debug)]
pub struct Registration {
    get_type_id: fn() -> TypeId,
    get_type_name: fn() -> &'static str,
    type_name: Option<&'static str>,
    init: ErlNifResourceTypeInit,
}

unsafe impl Sync for Registration {}

inventory::collect!(Registration);

impl<'a> Env<'a> {
    /// Register a resource type, see `Registration::register`.
    pub fn register<T: Resource>(&self) -> Result<(), ResourceInitError> {
        Registration::new::<T>().register(*self)
    }
}

/// Resource registration
///
/// The type name is derived using Rust's `std::any::type_name` and the callbacks are registered
/// according to the `IMPLEMENTS_...` associated constants on the `Resource` trait implementation.
/// A destructor is always registered if the type requires explicit `drop`ping (checked using
/// `std::mem::needs_drop`). All other callbacks are only registered if `IMPLEMENTS_...` is set to
/// `true`.
impl Registration {
    /// Register all resource types that have been submitted to the inventory.
    pub fn register_all_collected(env: Env) -> Result<(), ResourceInitError> {
        for reg in inventory::iter::<Registration>() {
            reg.register(env)?;
        }

        Ok(())
    }

    /// Generate a new (pending) resource type registration.
    pub const fn new<T: Resource>() -> Self {
        Self {
            init: ErlNifResourceTypeInit {
                dtor: ptr::null(),
                stop: ptr::null(),
                down: ptr::null(),
                members: 0,
                dyncall: ptr::null(),
            },
            get_type_name: std::any::type_name::<T>,
            get_type_id: TypeId::of::<T>,
            type_name: None,
        }
        .maybe_add_destructor_callback::<T>()
        .maybe_add_down_callback::<T>()
        .maybe_add_dyncall_callback::<T>()
    }

    pub const fn with_name(self, name: &'static str) -> Self {
        Self {
            type_name: Some(name),
            ..self
        }
    }

    const fn maybe_add_destructor_callback<T: Resource>(self) -> Self {
        if T::IMPLEMENTS_DESTRUCTOR || std::mem::needs_drop::<T>() {
            Self {
                init: ErlNifResourceTypeInit {
                    dtor: resource_destructor::<T> as *const ErlNifResourceDtor,
                    members: max(self.init.members, 1),
                    ..self.init
                },
                ..self
            }
        } else {
            self
        }
    }

    const fn maybe_add_down_callback<T: Resource>(self) -> Self {
        if T::IMPLEMENTS_DOWN {
            Self {
                init: ErlNifResourceTypeInit {
                    down: resource_down::<T> as *const ErlNifResourceDown,
                    members: max(self.init.members, 3),
                    ..self.init
                },
                ..self
            }
        } else {
            self
        }
    }

    #[cfg(not(feature = "nif_version_2_16"))]
    #[allow(clippy::extra_unused_type_parameters)]
    const fn maybe_add_dyncall_callback<T: Resource>(self) -> Self {
        self
    }

    #[cfg(feature = "nif_version_2_16")]
    const fn maybe_add_dyncall_callback<T: Resource>(self) -> Self {
        if T::IMPLEMENTS_DYNCALL {
            Self {
                init: ErlNifResourceTypeInit {
                    dyncall: resource_dyncall::<T> as *const crate::sys::ErlNifResourceDynCall,
                    members: max(self.init.members, 4),
                    ..self.init
                },
                ..self
            }
        } else {
            self
        }
    }

    /// Try to register the resource type for which this registration was created. This function
    /// will only succeed when called from the `load` callback and if this type has not yet been
    /// registered.
    pub fn register(&self, env: Env) -> Result<(), ResourceInitError> {
        if env.kind != EnvKind::Init {
            return Err(ResourceInitError);
        }

        let type_id = (self.get_type_id)();
        let type_name = self.type_name.unwrap_or_else(self.get_type_name);

        let res: Option<*const ErlNifResourceType> = unsafe {
            open_resource_type(
                env.as_c_arg(),
                CString::new(type_name).unwrap().as_bytes_with_nul(),
                self.init,
                ErlNifResourceFlags::ERL_NIF_RT_CREATE,
            )
        };
        if let Some(ptr) = res {
            unsafe { traits::register_resource_type(type_id, ptr) };
            Ok(())
        } else {
            Err(ResourceInitError)
        }
    }
}

/// Drop a T that lives in an Erlang resource
unsafe extern "C" fn resource_destructor<T>(_env: *mut ErlNifEnv, handle: *mut c_void)
where
    T: Resource,
{
    let env = Env::new_internal(&_env, _env, EnvKind::Callback);
    let aligned = align_alloced_mem_for_struct::<T>(handle);
    // Destructor takes ownership, thus the resource object will be dropped after the function has
    // run.
    let obj = ptr::read::<T>(aligned as *mut T);
    if T::IMPLEMENTS_DESTRUCTOR {
        obj.destructor(env);
    }
}

unsafe extern "C" fn resource_down<T: Resource>(
    env: *mut ErlNifEnv,
    obj: *mut c_void,
    pid: *const ErlNifPid,
    mon: *const ErlNifMonitor,
) {
    let env = Env::new_internal(&env, env, EnvKind::Callback);
    let aligned = align_alloced_mem_for_struct::<T>(obj);
    let res = &*(aligned as *const T);
    let pid = LocalPid::from_c_arg(*pid);
    let mon = Monitor::from_c_arg(*mon);

    res.down(env, pid, mon);
}

#[cfg(feature = "nif_version_2_16")]
unsafe extern "C" fn resource_dyncall<T: Resource>(
    env: *mut ErlNifEnv,
    obj: *mut c_void,
    call_data: *mut c_void,
) {
    let env = Env::new_internal(&env, env, EnvKind::Callback);
    let aligned = align_alloced_mem_for_struct::<T>(obj);
    let res = &*(aligned as *const T);

    res.dyncall(env, call_data);
}

pub unsafe fn open_resource_type(
    env: *mut ErlNifEnv,
    name: &[u8],
    init: ErlNifResourceTypeInit,
    flags: ErlNifResourceFlags,
) -> Option<*const ErlNifResourceType> {
    // Panic if name is not null-terminated.
    assert_eq!(name.last().cloned(), Some(0u8));

    let name_p = name.as_ptr() as *const c_char;

    let res = {
        let mut tried = MaybeUninit::uninit();
        OPEN_RESOURCE_TYPE(env, name_p, &init, flags, tried.as_mut_ptr())
    };

    if res.is_null() {
        None
    } else {
        Some(res)
    }
}

type OpenResourceTypeFn = unsafe extern "C" fn(
    *mut ErlNifEnv,
    *const c_char,
    *const ErlNifResourceTypeInit,
    ErlNifResourceFlags,
    *mut ErlNifResourceFlags,
) -> *const ErlNifResourceType;

#[cfg(feature = "nif_version_2_16")]
static OPEN_RESOURCE_TYPE: OpenResourceTypeFn = crate::sys::enif_init_resource_type;

#[cfg(not(feature = "nif_version_2_16"))]
static OPEN_RESOURCE_TYPE: OpenResourceTypeFn = crate::sys::enif_open_resource_type_x;

const fn max(i: i32, j: i32) -> i32 {
    if i > j {
        i
    } else {
        j
    }
}
