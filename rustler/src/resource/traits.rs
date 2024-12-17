use std::any::TypeId;
use std::collections::HashMap;
use std::sync::OnceLock;

use crate::sys::ErlNifResourceType;
use crate::{Env, LocalPid, Monitor};

type NifResourcePtr = *const ErlNifResourceType;

/// Map from `TypeId` to the `NifResourcePtr`. To be able to store this in a `OnceLock`, the
/// pointer is type-erased and stored as a `usize`.
static mut RESOURCE_TYPES: OnceLock<HashMap<TypeId, usize>> = OnceLock::new();

/// Register an Erlang resource type handle for a particular type given by its `TypeId`
#[allow(static_mut_refs)]
pub(crate) unsafe fn register_resource_type(type_id: TypeId, resource_type: NifResourcePtr) {
    RESOURCE_TYPES.get_or_init(Default::default);
    RESOURCE_TYPES
        .get_mut()
        .unwrap()
        .insert(type_id, resource_type as usize);
}

/// Trait that needs to be implemented to use a type as a NIF resource type.
///
/// The Erlang runtime provides the following guarantees:
/// - An object will be valid as long as the associated environment is valid
/// - `destructor` is the last function that is run on an object before it is freed
///
/// In particular, the type needs to handle all synchronization itself (thus we require it to
/// implement `Sync`) and callbacks or NIFs can run on arbitrary threads (thus we require `Send`).
///
/// Currently only `destructor` and `down` callbacks are possible. If a callback is implemented,
/// the respective associated constant `IMPLEMENTS_...` must be set to `true` for the registration
/// to take it into account. All callbacks provide (empty) default implementations.
pub trait Resource: Sized + Send + Sync + 'static {
    const IMPLEMENTS_DESTRUCTOR: bool = false;
    const IMPLEMENTS_DOWN: bool = false;

    #[cfg(feature = "nif_version_2_16")]
    const IMPLEMENTS_DYNCALL: bool = false;

    /// Callback function that is executed right before dropping a resource object.
    ///
    /// This callback does not have to be implemented to release associated resources or run
    /// constructors. For that it is enough to implement `Drop` or rely on the generated `Drop`
    /// implementation which will be called in any case. The function is useful when the cleanup
    /// requires access to a NIF env, e.g. to send messages.
    #[allow(unused_mut, unused)]
    fn destructor(mut self, env: Env<'_>) {}

    /// Callback function to handle process monitoring.
    ///
    /// This callback is called when a process monitored using `Env::monitor` terminates
    /// and receives the `pid` of that process as well as the `Monitor` instance that was returned
    /// by `ResourceArc<T>::monitor`.
    #[allow(unused)]
    fn down<'a>(&'a self, env: Env<'a>, pid: LocalPid, monitor: Monitor) {}

    #[cfg(feature = "nif_version_2_16")]
    #[allow(unused)]
    unsafe fn dyncall<'a>(&'a self, env: Env<'a>, call_data: *mut crate::sys::c_void) {}
}

#[doc(hidden)]
pub(crate) trait ResourceExt: 'static {
    /// Get the NIF resource type handle for this type if it had been registered before
    #[allow(static_mut_refs)]
    fn get_resource_type() -> Option<NifResourcePtr> {
        let map = unsafe { RESOURCE_TYPES.get()? };
        map.get(&TypeId::of::<Self>())
            .map(|ptr| *ptr as NifResourcePtr)
    }
}

impl<T: Resource> ResourceExt for T {}
