//! Support for storing Rust data in Erlang terms.
//!
//! A NIF resource allows you to safely store Rust structs in a term, and therefore keep it across
//! NIF calls. The struct will be automatically dropped when the BEAM GC decides that there are no
//! more references to the resource.

use std::any::TypeId;
use std::collections::HashMap;
use std::ops::Deref;
use std::ptr;
use std::sync::OnceLock;
use std::{ffi::CString, mem};

use super::{Binary, Decoder, Encoder, Env, Error, NifResult, Term};
use crate::resource::resource::open_resource_type;
pub use crate::wrapper::{
    c_void, resource, NifResourceFlags, MUTABLE_NIF_RESOURCE_HANDLE, NIF_ENV, NIF_RESOURCE_TYPE,
};

#[derive(Debug)]
pub struct ResourceRegistration {
    name: &'static str,
    get_type_id: fn() -> TypeId,
    destructor: unsafe extern "C" fn(_env: NIF_ENV, handle: MUTABLE_NIF_RESOURCE_HANDLE),
}
inventory::collect!(ResourceRegistration);

static mut RESOURCE_TYPES: OnceLock<HashMap<TypeId, usize>> = OnceLock::new();

fn get_resource_type<T: 'static>() -> Option<NIF_RESOURCE_TYPE> {
    let map = unsafe { RESOURCE_TYPES.get()? };
    map.get(&TypeId::of::<T>())
        .map(|ptr| *ptr as NIF_RESOURCE_TYPE)
}

impl ResourceRegistration {
    pub const fn new<T: ResourceType>(name: &'static str) -> Self {
        Self {
            name,
            destructor: resource_destructor::<T>,
            get_type_id: TypeId::of::<T>,
        }
    }

    pub fn initialize(env: Env) {
        for reg in inventory::iter::<Self>() {
            reg.register(env);
        }
    }

    pub fn register(&self, env: Env) {
        let res: Option<NIF_RESOURCE_TYPE> = unsafe {
            open_resource_type(
                env.as_c_arg(),
                CString::new(self.name).unwrap().as_bytes_with_nul(),
                Some(self.destructor),
                NIF_RESOURCE_FLAGS::ERL_NIF_RT_CREATE,
            )
        };

        let type_id = (self.get_type_id)();

        unsafe {
            RESOURCE_TYPES.get_or_init(Default::default);
            RESOURCE_TYPES
                .get_mut()
                .unwrap()
                .insert(type_id, res.unwrap() as usize);
        }
    }
}

/// Re-export a type used by the `resource!` macro.
#[doc(hidden)]
pub use crate::wrapper::NIF_RESOURCE_FLAGS;

#[doc(hidden)]
pub trait ResourceType: Sized + Send + Sync + 'static {
    fn get_resource_type() -> Option<NIF_RESOURCE_TYPE> {
        get_resource_type::<Self>()
    }
}

impl<'a> Term<'a> {
    unsafe fn get_resource_ptrs<T: ResourceType>(&self) -> Option<(*const c_void, *mut T)> {
        let typ = T::get_resource_type()?;
        let res = resource::get_resource(self.get_env().as_c_arg(), self.as_c_arg(), typ)?;
        Some((res, align_alloced_mem_for_struct::<T>(res) as *mut T))
    }

    pub fn get_resource<T: ResourceType>(&self) -> Option<&'a T> {
        unsafe { self.get_resource_ptrs().map(|(_, ptr)| &*ptr) }
    }

    pub unsafe fn get_mut_resource<T: ResourceType>(&self) -> Option<&'a mut T> {
        self.get_resource_ptrs().map(|(_, ptr)| &mut *ptr)
    }
}

impl<T> Encoder for ResourceArc<T>
where
    T: ResourceType,
{
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        self.as_term(env)
    }
}
impl<'a, T> Decoder<'a> for ResourceArc<T>
where
    T: ResourceType + 'a,
{
    fn decode(term: Term<'a>) -> NifResult<Self> {
        ResourceArc::from_term(term)
    }
}

impl<'a, T> Decoder<'a> for &'a T
where
    T: ResourceType + 'a,
{
    fn decode(term: Term<'a>) -> NifResult<Self> {
        term.get_resource().ok_or(Error::BadArg)
    }
}

/// Drop a T that lives in an Erlang resource. (erlang_nif-sys requires us to declare this
/// function safe, but it is of course thoroughly unsafe!)
pub unsafe extern "C" fn resource_destructor<T>(
    _env: NIF_ENV,
    handle: MUTABLE_NIF_RESOURCE_HANDLE,
) {
    unsafe {
        let aligned = align_alloced_mem_for_struct::<T>(handle);
        let res = aligned as *mut T;
        ptr::read(res);
    }
}

fn get_alloc_size_struct<T>() -> usize {
    mem::size_of::<T>() + mem::align_of::<T>()
}

/// Given a pointer `ptr` to an allocation of `get_alloc_size_struct::<T>()` bytes, return the
/// first aligned pointer within the allocation where a `T` may be stored.
/// Unsafe: `ptr` must point to a large enough allocation and not be null.
unsafe fn align_alloced_mem_for_struct<T>(ptr: *const c_void) -> *const c_void {
    let offset = mem::align_of::<T>() - ((ptr as usize) % mem::align_of::<T>());
    ptr.add(offset)
}

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
    T: ResourceType,
{
    raw: *const c_void,
    inner: *mut T,
}

// Safe because T is `Sync` and `Send`.
unsafe impl<T> Send for ResourceArc<T> where T: ResourceType {}
unsafe impl<T> Sync for ResourceArc<T> where T: ResourceType {}

impl<T> ResourceArc<T>
where
    T: ResourceType,
{
    /// Makes a new ResourceArc from the given type. Note that the type must have
    /// ResourceType implemented for it. See module documentation for info on this.
    pub fn new(data: T) -> Self {
        let alloc_size = get_alloc_size_struct::<T>();
        let resource_type = T::get_resource_type().unwrap();
        let mem_raw = unsafe { rustler_sys::enif_alloc_resource(resource_type, alloc_size) };
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
        let binary = rustler_sys::enif_make_resource_binary(
            env.as_c_arg(),
            self.raw,
            bin.as_ptr() as *const c_void,
            bin.len(),
        );

        let term = Term::new(env, binary);
        Binary::from_term_and_slice(term, bin)
    }

    fn from_term(term: Term) -> Result<Self, Error> {
        let (raw, inner) = unsafe { term.get_resource_ptrs::<T>() }.ok_or(Error::BadArg)?;
        unsafe { rustler_sys::enif_keep_resource(raw) };
        Ok(ResourceArc { raw, inner })
    }

    fn as_term<'a>(&self, env: Env<'a>) -> Term<'a> {
        unsafe {
            Term::new(
                env,
                rustler_sys::enif_make_resource(env.as_c_arg(), self.raw),
            )
        }
    }

    fn as_c_arg(&mut self) -> *const c_void {
        self.raw
    }

    fn inner(&self) -> &T {
        unsafe { &*self.inner }
    }
}

impl<T> Deref for ResourceArc<T>
where
    T: ResourceType,
{
    type Target = T;

    fn deref(&self) -> &T {
        self.inner()
    }
}

impl<T> Clone for ResourceArc<T>
where
    T: ResourceType,
{
    /// Cloning a `ResourceArc` simply increments the reference count for the
    /// resource. The `T` value is not cloned.
    fn clone(&self) -> Self {
        unsafe { rustler_sys::enif_keep_resource(self.raw) };
        ResourceArc {
            raw: self.raw,
            inner: self.inner,
        }
    }
}

impl<T> Drop for ResourceArc<T>
where
    T: ResourceType,
{
    /// When a `ResourceArc` is dropped, the reference count is decremented. If
    /// there are no other references to the resource, the `T` value is dropped.
    ///
    /// However, note that in general, the Rust value in a resource is dropped
    /// at an unpredictable time: whenever the VM decides to do garbage
    /// collection.
    fn drop(&mut self) {
        unsafe { rustler_sys::enif_release_resource(self.as_c_arg()) };
    }
}

#[macro_export]
macro_rules! resource {
    ($struct_name:ty, $env: ident) => {{
        impl $crate::resource::ResourceType for $struct_name {}

        let tuple = rustler::resource::ResourceRegistration::new::<$struct_name>(
            stringify!(#name)
        ).register($env);
    }};
}
