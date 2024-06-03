use std::any::TypeId;
use std::collections::HashMap;
use std::sync::OnceLock;

use crate::{Env, LocalPid, Monitor};

type NifResourcePtr = *const rustler_sys::ErlNifResourceType;

static mut RESOURCE_TYPES: OnceLock<HashMap<TypeId, usize>> = OnceLock::new();

pub(crate) unsafe fn register_resource_type(type_id: TypeId, resource_type: NifResourcePtr) {
    RESOURCE_TYPES.get_or_init(Default::default);
    RESOURCE_TYPES
        .get_mut()
        .unwrap()
        .insert(type_id, resource_type as usize);
}

pub trait Resource: Sized + Send + Sync + 'static {
    const IMPLEMENTS_DESTRUCTOR: bool = false;
    const IMPLEMENTS_DOWN: bool = false;

    /// Callback function that is executed right before dropping a resource object.
    #[allow(unused_mut, unused)]
    fn destructor(mut self, env: Env<'_>) {}

    #[allow(unused)]
    fn down<'a>(&'a self, env: Env<'a>, pid: LocalPid, monitor: Monitor) {}
}

#[doc(hidden)]
pub(crate) trait ResourceExt: 'static {
    fn get_resource_type() -> Option<NifResourcePtr> {
        let map = unsafe { RESOURCE_TYPES.get()? };
        map.get(&TypeId::of::<Self>())
            .map(|ptr| *ptr as NifResourcePtr)
    }
}

impl<T: Resource> ResourceExt for T {}
