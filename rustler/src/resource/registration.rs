use super::util::align_alloced_mem_for_struct;
use super::{traits, ResourceInitError};
use crate::{Env, LocalPid, Monitor, MonitorResource, Resource};
use rustler_sys::{
    c_char, c_void, ErlNifEnv, ErlNifMonitor, ErlNifPid, ErlNifResourceDown, ErlNifResourceDtor,
    ErlNifResourceFlags, ErlNifResourceType, ErlNifResourceTypeInit,
};
use std::any::TypeId;
use std::ffi::CString;
use std::mem::MaybeUninit;
use std::ptr;

#[derive(Debug)]
pub struct Registration {
    get_type_id: fn() -> TypeId,
    get_type_name: fn() -> &'static str,
    init: ErlNifResourceTypeInit,
}

impl Registration {
    pub const fn new<T: Resource>() -> Self {
        let init = ErlNifResourceTypeInit {
            dtor: resource_destructor::<T> as *const ErlNifResourceDtor,
            stop: ptr::null(),
            down: ptr::null(),
            members: 1,
            dyncall: ptr::null(),
        };
        Self {
            init,
            get_type_name: std::any::type_name::<T>,
            get_type_id: TypeId::of::<T>,
        }
    }

    pub const fn add_down_callback<T: MonitorResource>(self) -> Self {
        Self {
            init: ErlNifResourceTypeInit {
                down: resource_down::<T> as *const ErlNifResourceDown,
                ..self.init
            },
            ..self
        }
    }

    pub fn register(&self, env: Env) -> Result<(), ResourceInitError> {
        let type_id = (self.get_type_id)();
        let type_name = (self.get_type_name)();

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
    let env = Env::new(&_env, _env);
    let aligned = align_alloced_mem_for_struct::<T>(handle);
    // Destructor takes ownership, thus the resource object will be dropped after the function has
    // run.
    ptr::read::<T>(aligned as *mut T).destructor(env);
}

unsafe extern "C" fn resource_down<T: MonitorResource>(
    env: *mut ErlNifEnv,
    obj: *mut c_void,
    pid: *const ErlNifPid,
    mon: *const ErlNifMonitor,
) {
    let env = Env::new(&env, env);
    let aligned = align_alloced_mem_for_struct::<T>(obj);
    let res = &*(aligned as *const T);
    let pid = LocalPid::from_c_arg(*pid);
    let mon = Monitor::from_c_arg(*mon);

    res.down(env, pid, mon);
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
        rustler_sys::enif_open_resource_type_x(env, name_p, &init, flags, tried.as_mut_ptr())
    };

    if res.is_null() {
        None
    } else {
        Some(res)
    }
}
