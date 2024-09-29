use std::mem::MaybeUninit;
use std::ops::Deref;
use std::ptr;

use crate::sys::{
    c_void, enif_alloc_resource, enif_demonitor_process, enif_keep_resource, enif_make_resource,
    enif_make_resource_binary, enif_monitor_process, enif_release_resource, ErlNifEnv,
};

use crate::thread::is_scheduler_thread;
use crate::{Binary, Decoder, Encoder, Env, Error, LocalPid, Monitor, NifResult, OwnedEnv, Term};

use super::traits::{Resource, ResourceExt};
use super::util::{align_alloced_mem_for_struct, get_alloc_size_struct};

/// A reference to a resource of type `T`.
///
/// This type is like `std::sync::Arc`: it provides thread-safe, reference-counted storage for Rust
/// data that can be shared across threads. Data stored this way is immutable by default. If you
/// need to modify data in a resource, use a `std::sync::Mutex` or `RwLock`.
///
/// Rust code and Erlang code can both have references to the same resource at the same time.  Rust
/// code uses `ResourceArc`; in Erlang, a reference to a resource is a kind of term.  You can
/// convert back and forth between the two using `Encoder` and `Decoder`.
pub struct ResourceArc<T>
where
    T: Resource,
{
    raw: *const c_void,
    inner: *mut T,
}

// Safe because T is `Sync` and `Send`.
unsafe impl<T> Send for ResourceArc<T> where T: Resource {}
unsafe impl<T> Sync for ResourceArc<T> where T: Resource {}

impl<T> ResourceArc<T>
where
    T: Resource,
{
    /// Makes a new ResourceArc from the given type. Note that the type must have Resource
    /// implemented for it. See module documentation for info on this.
    pub fn new(data: T) -> Self {
        let alloc_size = get_alloc_size_struct::<T>();
        let resource_type = T::get_resource_type().unwrap();
        let mem_raw = unsafe { enif_alloc_resource(resource_type, alloc_size) };
        let aligned_mem = unsafe { align_alloced_mem_for_struct::<T>(mem_raw) as *mut T };

        unsafe { ptr::write(aligned_mem, data) };

        ResourceArc {
            raw: mem_raw,
            inner: aligned_mem,
        }
    }

    /// Make a resource binary associated with the given resource
    ///
    /// The closure `f` is called with the referenced object and must return a slice with the same
    /// lifetime as the object. This means that the slice either has to be derived directly from
    /// the instance or that it has to have static lifetime.
    pub fn make_binary<'env, 'a, F>(&self, env: Env<'env>, f: F) -> Binary<'env>
    where
        F: FnOnce(&'a T) -> &'a [u8],
    {
        // This call is safe because `f` can only return a slice that lives at least as long as
        // the given instance of `T`.
        unsafe { self.make_binary_unsafe(env, f) }
    }

    /// Make a resource binary without strict lifetime checking
    ///
    /// The user *must* ensure that the lifetime of the returned slice is at least as long as the
    /// lifetime of the referenced instance.
    ///
    /// # Safety
    ///
    /// This function is only safe if the slice that is returned from the closure is guaranteed to
    /// live at least as long as the `ResourceArc` instance. If in doubt, use the safe version
    /// `ResourceArc::make_binary` which enforces this bound through its signature.
    pub unsafe fn make_binary_unsafe<'env, 'a, 'b, F>(&self, env: Env<'env>, f: F) -> Binary<'env>
    where
        F: FnOnce(&'a T) -> &'b [u8],
    {
        let bin = f(&*self.inner);
        let binary = enif_make_resource_binary(
            env.as_c_arg(),
            self.raw,
            bin.as_ptr() as *const c_void,
            bin.len(),
        );

        let term = Term::new(env, binary);
        Binary::from_term_and_slice(term, bin)
    }

    fn from_term(term: Term) -> Result<Self, Error> {
        let (raw, inner) = unsafe { term.try_get_resource_ptrs::<T>() }.ok_or(Error::BadArg)?;
        unsafe { enif_keep_resource(raw) };
        Ok(ResourceArc { raw, inner })
    }

    fn as_term<'a>(&self, env: Env<'a>) -> Term<'a> {
        unsafe { Term::new(env, enif_make_resource(env.as_c_arg(), self.raw)) }
    }

    fn as_c_arg(&mut self) -> *const c_void {
        self.raw
    }

    fn inner(&self) -> &T {
        unsafe { &*self.inner }
    }
}

impl<T> ResourceArc<T>
where
    T: Resource,
{
    pub fn monitor(&self, caller_env: Option<Env>, pid: &LocalPid) -> Option<Monitor> {
        if !T::IMPLEMENTS_DOWN {
            return None;
        }

        let env = maybe_env(caller_env);
        let mut mon = MaybeUninit::uninit();
        let res =
            unsafe { enif_monitor_process(env, self.raw, pid.as_c_arg(), mon.as_mut_ptr()) == 0 };
        if res {
            Some(unsafe { Monitor::new(mon.assume_init()) })
        } else {
            None
        }
    }

    pub fn demonitor(&self, caller_env: Option<Env>, mon: &Monitor) -> bool {
        if !T::IMPLEMENTS_DOWN {
            return false;
        }

        let env = maybe_env(caller_env);
        unsafe { enif_demonitor_process(env, self.raw, mon.as_c_arg()) == 0 }
    }
}

impl OwnedEnv {
    pub fn monitor<T: Resource>(
        &self,
        resource: &ResourceArc<T>,
        pid: &LocalPid,
    ) -> Option<Monitor> {
        resource.monitor(None, pid)
    }

    pub fn demonitor<T: Resource>(&self, resource: &ResourceArc<T>, mon: &Monitor) -> bool {
        resource.demonitor(None, mon)
    }
}

impl<'a> Env<'a> {
    pub fn monitor<T: Resource>(
        &self,
        resource: &ResourceArc<T>,
        pid: &LocalPid,
    ) -> Option<Monitor> {
        resource.monitor(Some(*self), pid)
    }

    pub fn demonitor<T: Resource>(&self, resource: &ResourceArc<T>, mon: &Monitor) -> bool {
        resource.demonitor(Some(*self), mon)
    }

    #[cfg(feature = "nif_version_2_16")]
    pub unsafe fn dynamic_resource_call(
        self,
        module: crate::Atom,
        name: crate::Atom,
        resource: Term<'a>,
        call_data: *mut c_void,
    ) -> Result<(), super::DynamicResourceCallError> {
        use crate::sys::enif_dynamic_resource_call;

        let res = enif_dynamic_resource_call(
            self.as_c_arg(),
            module.as_c_arg(),
            name.as_c_arg(),
            resource.as_c_arg(),
            call_data,
        );

        if res == 0 {
            Ok(())
        } else {
            Err(super::DynamicResourceCallError)
        }
    }
}

impl<T> Deref for ResourceArc<T>
where
    T: Resource,
{
    type Target = T;

    fn deref(&self) -> &T {
        self.inner()
    }
}

impl<T> Clone for ResourceArc<T>
where
    T: Resource,
{
    /// Cloning a `ResourceArc` simply increments the reference count for the
    /// resource. The `T` value is not cloned.
    fn clone(&self) -> Self {
        unsafe { enif_keep_resource(self.raw) };
        ResourceArc {
            raw: self.raw,
            inner: self.inner,
        }
    }
}

impl<T> Drop for ResourceArc<T>
where
    T: Resource,
{
    /// When a `ResourceArc` is dropped, the reference count is decremented. If
    /// there are no other references to the resource, the `T` value is dropped.
    ///
    /// However, note that in general, the Rust value in a resource is dropped
    /// at an unpredictable time: whenever the VM decides to do garbage
    /// collection.
    fn drop(&mut self) {
        unsafe { enif_release_resource(self.as_c_arg()) };
    }
}

impl<T: Resource> From<T> for ResourceArc<T> {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

impl<T> Encoder for ResourceArc<T>
where
    T: Resource,
{
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        self.as_term(env)
    }
}
impl<'a, T> Decoder<'a> for ResourceArc<T>
where
    T: Resource + 'a,
{
    fn decode(term: Term<'a>) -> NifResult<Self> {
        ResourceArc::from_term(term)
    }
}

fn maybe_env(env: Option<Env>) -> *mut ErlNifEnv {
    if is_scheduler_thread() {
        let env = env.expect("Env required when calling from a scheduler thread");
        // Panic if `env` is not the environment of the calling process.
        env.pid();
        env.as_c_arg()
    } else {
        assert!(
            env.is_none(),
            "Env provided when not calling from a scheduler thread"
        );
        ptr::null_mut()
    }
}
