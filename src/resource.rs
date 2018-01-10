//! Support for storing Rust data in Erlang terms.
//!
//! A NIF resource allows you to safely store Rust structs in a term, and therefore keep it across
//! NIF calls. The struct will be automatically dropped when the BEAM GC decides that there are no
//! more references to the resource.

use std::mem;
use std::ptr;
use std::ops::Deref;
use std::marker::PhantomData;

use super::{ NifTerm, NifEnv, NifError, Encoder, Decoder, NifResult };
use ::wrapper::nif_interface::{ NIF_RESOURCE_TYPE, MUTABLE_NIF_RESOURCE_HANDLE, NIF_ENV, NifResourceFlags };
use ::wrapper::nif_interface::{ c_void };

/// Re-export a type used by the `resource_struct_init!` macro.
#[doc(hidden)]
pub use ::wrapper::nif_interface::NIF_RESOURCE_FLAGS;

/// The NifResourceType struct contains a  NIF_RESOURCE_TYPE and a phantom reference to the type it
/// is for. It serves as a holder for the information needed to interact with the Erlang VM about
/// the resource type.
/// 
/// This is usually stored in an implementation of NifResourceTypeProvider.
#[doc(hidden)]
pub struct NifResourceType<T> {
    pub res: NIF_RESOURCE_TYPE,
    pub struct_type: PhantomData<T>,
}

/// This trait gets implemented for the type we want to put into a resource when
/// resource_struct_init! is called on it. It provides the NifResourceType.
///
/// In most cases the user should not have to worry about this.
#[doc(hidden)]
pub trait NifResourceTypeProvider: Sized + Send + Sync + 'static {
    fn get_type() -> &'static NifResourceType<Self>;
}

impl<T> Encoder for ResourceArc<T> where T: NifResourceTypeProvider {
    fn encode<'a>(&self, env: NifEnv<'a>) -> NifTerm<'a> {
        self.as_term(env)
    }
}
impl<'a, T> Decoder<'a> for ResourceArc<T> where T: NifResourceTypeProvider + 'a {
    fn decode(term: NifTerm<'a>) -> NifResult<Self> {
        ResourceArc::from_term(term)
    }
}

/// Drop a T that lives in an Erlang resource. (erlang_nif-sys requires us to declare this
/// function safe, but it is of course thoroughly unsafe!)
extern "C" fn resource_destructor<T>(_env: NIF_ENV, handle: MUTABLE_NIF_RESOURCE_HANDLE) {
    unsafe {
        let aligned = align_alloced_mem_for_struct::<T>(handle);
        let res = aligned as *mut T;
        ptr::read(res);
    }
}

/// This is the function that gets called from resource_struct_init! in on_load to create a new
/// resource type.
///
/// # Panics
///
/// Panics if `name` isn't null-terminated.
#[doc(hidden)]
pub fn open_struct_resource_type<'a, T: NifResourceTypeProvider>(env: NifEnv<'a>, name: &str,
                                 flags: NifResourceFlags) -> Option<NifResourceType<T>> {
    let res: Option<NIF_RESOURCE_TYPE> = unsafe {
        ::wrapper::resource::open_resource_type(env.as_c_arg(), name.as_bytes(), Some(resource_destructor::<T>), flags)
    };
    if res.is_some() {
        Some(NifResourceType {
            res: res.unwrap(),
            struct_type: PhantomData,
        })
    } else {
        None
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
    ptr.offset(offset as isize)
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
pub struct ResourceArc<T> where T: NifResourceTypeProvider {
    raw: *const c_void,
    inner: *mut T,
}

// Safe because T is `Sync` and `Send`.
unsafe impl<T> Send for ResourceArc<T> where T: NifResourceTypeProvider {}
unsafe impl<T> Sync for ResourceArc<T> where T: NifResourceTypeProvider {}

impl<T> ResourceArc<T> where T: NifResourceTypeProvider {
    /// Makes a new ResourceArc from the given type. Note that the type must have
    /// NifResourceTypeProvider implemented for it. See module documentation for info on this.
    pub fn new(data: T) -> Self {
        let alloc_size = get_alloc_size_struct::<T>();
        let mem_raw = unsafe { ::wrapper::resource::alloc_resource(T::get_type().res, alloc_size) };
        let aligned_mem = unsafe { align_alloced_mem_for_struct::<T>(mem_raw) as *mut T };

        unsafe { ptr::write(aligned_mem, data) };

        ResourceArc {
            raw: mem_raw,
            inner: aligned_mem,
        }
    }

    fn from_term(term: NifTerm) -> Result<Self, NifError> {
        let res_resource = match unsafe { ::wrapper::resource::get_resource(term.get_env().as_c_arg(), term.as_c_arg(), T::get_type().res) } {
            Some(res) => res,
            None => return Err(NifError::BadArg),
        };
        unsafe { ::wrapper::resource::keep_resource(res_resource); }
        let casted_ptr = unsafe { align_alloced_mem_for_struct::<T>(res_resource) as *mut T };
        Ok(ResourceArc {
            raw: res_resource,
            inner: casted_ptr,
        })
    }

    fn as_term<'a>(&self, env: NifEnv<'a>) -> NifTerm<'a> {
        unsafe { NifTerm::new(env, ::wrapper::resource::make_resource(env.as_c_arg(), self.raw)) }
    }

    fn as_c_arg(&mut self) -> *const c_void {
        self.raw
    }

    fn inner(&self) -> &T {
        unsafe { &*self.inner }
    }
}

impl<T> Deref for ResourceArc<T> where T: NifResourceTypeProvider {
    type Target = T;

    fn deref(&self) -> &T {
        self.inner()
    }
}

impl<T> Clone for ResourceArc<T> where T: NifResourceTypeProvider {
    /// Cloning a `ResourceArc` simply increments the reference count for the
    /// resource. The `T` value is not cloned.
    fn clone(&self) -> Self {
        unsafe { ::wrapper::resource::keep_resource(self.raw); }
        ResourceArc {
            raw: self.raw,
            inner: self.inner,
        }
    }
}

impl<T> Drop for ResourceArc<T> where T: NifResourceTypeProvider {
    /// When a `ResourceArc` is dropped, the reference count is decremented. If
    /// there are no other references to the resource, the `T` value is dropped.
    ///
    /// However, note that in general, the Rust value in a resource is dropped
    /// at an unpredictable time: whenever the VM decides to do garbage
    /// collection.
    fn drop(&mut self) {
        unsafe { ::wrapper::nif_interface::enif_release_resource(self.as_c_arg()) };
    }
}

#[macro_export]
macro_rules! resource_struct_init {
    ($struct_name:ty, $env: ident) => {
        {
            static mut STRUCT_TYPE: Option<$crate::resource::NifResourceType<$struct_name>> = None;

            let temp_struct_type =
                match $crate::resource::open_struct_resource_type::<$struct_name>(
                    $env,
                    concat!(stringify!($struct_name), "\x00"),
                    $crate::resource::NIF_RESOURCE_FLAGS::ERL_NIF_RT_CREATE
                    ) {
                    Some(inner) => inner,
                    None => {
                        println!("Failure in creating resource type");
                        return false;
                    }
                };
            unsafe { STRUCT_TYPE = Some(temp_struct_type) };

            impl $crate::resource::NifResourceTypeProvider for $struct_name {
                fn get_type() -> &'static $crate::resource::NifResourceType<Self> {
                    unsafe { &STRUCT_TYPE }.as_ref()
                        .expect("The resource type hasn't been inited. Did you remember to call the function where you used the `resource_struct_init!` macro?")
                }
            }
        }
    }
}
