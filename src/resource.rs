
//! A NIF resource allows you to safely store rust structs in a term, and therefore keep it across
//! NIF calls. The struct will be automatically dropped when the BEAM GC decides that there are no
//! more references to the resource.

use std::mem;
use std::ptr;
use std::ops::Deref;
use std::marker::PhantomData;

use super::{ NifTerm, NifEnv, NifError, NifEncoder, NifDecoder, NifResult };
use ::wrapper::nif_interface::{ NIF_RESOURCE_TYPE, MUTABLE_NIF_RESOURCE_HANDLE, NIF_ENV, NifResourceFlags };
use ::wrapper::nif_interface::{ c_void };

/// Re-export a type used by the `resource_struct_init!` macro.
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
/// resource_struct_init! is called on it. It provides the destructor and the
/// NifResourceType.
///
/// In most cases the user should not have to worry about this.
#[doc(hidden)]
pub trait NifResourceTypeProvider: Sized {
    //fn get_dtor() -> extern "C" fn(env: NIF_ENV, handle: MUTABLE_NIF_RESOURCE_HANDLE);
    extern "C" fn destructor(env: NIF_ENV, handle: MUTABLE_NIF_RESOURCE_HANDLE);
    fn get_type<'a>() -> &'a NifResourceType<Self>;
    //unsafe fn set_type(typ: NifResourceType<Self>);
}

impl<T> NifEncoder for ResourceCell<T> where T: NifResourceTypeProvider + Sync {
    fn encode<'a>(&self, env: NifEnv<'a>) -> NifTerm<'a> {
        self.as_term(env)
    }
}
impl<'a, T> NifDecoder<'a> for ResourceCell<T> where T: NifResourceTypeProvider + Sync + 'a {
    fn decode(term: NifTerm<'a>) -> NifResult<Self> {
        ResourceCell::from_term(term)
    }
}

/// This is the function that gets called from resource_struct_init! in on_load to create a new
/// resource type.
#[doc(hidden)]
pub fn open_struct_resource_type<'a, T: NifResourceTypeProvider>(env: NifEnv<'a>, name: &str,
                                 flags: NifResourceFlags) -> Option<NifResourceType<T>> {
    let res: Option<NIF_RESOURCE_TYPE> = ::wrapper::resource::open_resource_type(env.as_c_arg(), name, 
                                                                                 Some(T::destructor), flags);
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

/// Exported for use by codegen_runtime
/// This is unsafe as it allows us to do nasty things with pointers
#[doc(hidden)]
pub unsafe fn align_alloced_mem_for_struct<T>(ptr: *const c_void) -> *const c_void {
    let offset = mem::align_of::<T>() - ((ptr as usize) % mem::align_of::<T>());
    ptr.offset(offset as isize)
}

/// This is the struct that holds a reference to a resource. It increments the refcounter for the
/// resource instance on creation, and decrements when dropped.
pub struct ResourceCell<T> where T: NifResourceTypeProvider + Sync {
    raw: *const c_void,
    inner: *mut Box<T>,
}

impl<T> ResourceCell<T> where T: NifResourceTypeProvider + Sync {
    /// Makes a new ResourceCell from the given type. Note that the type must have
    /// NifResourceTypeProvider implemented for it. See module documentation for info on this.
    pub fn new(data: T) -> Self {
        let alloc_size = get_alloc_size_struct::<Box<T>>();
        let mem_raw = ::wrapper::resource::alloc_resource(T::get_type().res, alloc_size);
        let aligned_mem = unsafe { align_alloced_mem_for_struct::<Box<T>>(mem_raw) } as *mut Box<T>;

        unsafe { ptr::write(aligned_mem, Box::new(data)) };

        ResourceCell {
            raw: mem_raw,
            inner: aligned_mem,
        }
    }

    fn from_term(term: NifTerm) -> Result<Self, NifError> {
        let res_resource = match ::wrapper::resource::get_resource(term.get_env().as_c_arg(), term.as_c_arg(), T::get_type().res) {
            Some(res) => res,
            None => return Err(NifError::BadArg),
        };
        ::wrapper::resource::keep_resource(res_resource);
        let casted_ptr = unsafe { align_alloced_mem_for_struct::<Box<T>>(res_resource) } as *mut Box<T>;
        Ok(ResourceCell {
            raw: res_resource,
            inner: casted_ptr,
        })
    }

    fn as_term<'a>(&self, env: NifEnv<'a>) -> NifTerm<'a> {
        let raw_term = ::wrapper::resource::make_resource(env.as_c_arg(), self.raw);
        NifTerm::new(env, raw_term)
    }

    fn as_c_arg(&mut self) -> *const c_void {
        self.raw
    }

    fn inner(&self) -> &T {
        unsafe { &*self.inner }
    }
}

impl<T> Deref for ResourceCell<T> where T: NifResourceTypeProvider + Sync {
    type Target = T;

    fn deref(&self) -> &T {
        self.inner()
    }
}

impl<T> Clone for ResourceCell<T> where T: NifResourceTypeProvider + Sync {
    /// For a ResourceCell this will simply increment the refcounter for the resource instance and
    /// perform a fairly standard clone.
    fn clone(&self) -> Self {
        ::wrapper::resource::keep_resource(self.raw);
        ResourceCell {
            raw: self.raw,
            inner: self.inner,
        }
    }
}

impl<T> Drop for ResourceCell<T> where T: NifResourceTypeProvider + Sync {
    /// When drop is called for a ResourceCell, the reference held to the resource by the Cell is
    /// released.
    fn drop(&mut self) {
        unsafe { ::wrapper::nif_interface::enif_release_resource(self.as_c_arg()) };
    }
}

#[macro_export]
macro_rules! resource_struct_init {
    ($struct_name:ident, $env: ident) => {
        {
            static mut STRUCT_TYPE: Option<$crate::resource::NifResourceType<$struct_name>> = None;

            let temp_struct_type =
                match $crate::resource::open_struct_resource_type::<$struct_name>(
                    $env,
                    stringify!($struct_name),
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
                extern "C" fn destructor(
                    env: $crate::codegen_runtime::NIF_ENV,
                    obj: $crate::codegen_runtime::MUTABLE_NIF_RESOURCE_HANDLE) {
                    unsafe { $crate::codegen_runtime::handle_drop_resource_struct_handle::<$struct_name>(env, obj) };
                }
                fn get_type<'a>() -> &'a $crate::resource::NifResourceType<Self> {
                    unsafe { &STRUCT_TYPE }.as_ref().unwrap()
                }
            }
        }
    }
}
