extern crate ruster_unsafe;
use self::ruster_unsafe::{ ErlNifResourceFlags, ErlNifResourceType };

extern crate libc;
use self::libc::{ c_void, size_t };

use std::ffi::CString;
use std::mem;
use std::ptr;
use std::marker::PhantomData;

use super::{ NifTerm, NifEnv, NifError, NifEncoder, NifDecoder };
use ::wrapper::nif_interface::{ NIF_RESOURCE_TYPE, NIF_RESOURCE_HANDLE, NIF_ENV };

pub struct NifStructResourceType<T> {
    pub res: NIF_RESOURCE_TYPE,
    pub struct_type: PhantomData<T>,
}

// Resources
/*pub fn open_resource_type_raw(env: &NifEnv, module: &str, name: &str, 
                         flags: ErlNifResourceFlags) -> Result<NifResourceType, &'static str> {
    let module_p = CString::new(module).unwrap().as_bytes_with_nul().as_ptr();
    let name_p = CString::new(name).unwrap().as_bytes_with_nul().as_ptr();
    unsafe {
        let mut tried: ErlNifResourceFlags = mem::uninitialized();
        let res = ruster_unsafe::enif_open_resource_type(env.env, module_p, name_p, None, flags, 
                                                         (&mut tried as *mut ErlNifResourceFlags));
        if !res.is_null() {
            return Ok(NifResourceType { res: res });
        }
    }
    Err("Error when opening resource type")
}
pub unsafe fn alloc_resource_raw(res_type: &NifResourceType, size: usize) -> *mut c_void {
    ruster_unsafe::enif_alloc_resource((res_type.res as *mut ErlNifResourceType), size as size_t)
}*/
// End Resources

// Resource Structs
// TODO: Provide synchronization. THIS IS IMPORTANT
// TODO: Handle destruction. This is important, heap allocated objects will leak!
pub trait NifResourceStruct: Sized {
    fn get_dtor() -> extern "C" fn(env: NIF_ENV, handle: NIF_RESOURCE_HANDLE);
    fn get_type<'a>() -> &'a NifStructResourceType<Self>;
}

impl<'b, T> NifEncoder for ResourceTypeHolder<'b, T> where T: NifResourceStruct+'b {
    fn encode<'a>(&self, env: &'a NifEnv) -> NifTerm<'a> {
        self.as_term(env)
    }
}
impl<'a, T> NifDecoder<'a> for ResourceTypeHolder<'a, T> where T: NifResourceStruct+'a {
    fn decode(term: NifTerm, env: &'a NifEnv) -> Result<Self, NifError> {
        ResourceTypeHolder::from_term(env, term)
    }
}

pub fn open_struct_resource_type<T: NifResourceStruct>(env: &NifEnv, name: &str,
                                 flags: ErlNifResourceFlags) -> Option<NifStructResourceType<T>> {
    let res: Option<NIF_RESOURCE_TYPE> = ::wrapper::resource::open_resource_type(env.as_c_arg(), name, 
                                                                                 Some(T::get_dtor()), flags);
    if res.is_some() {
        Some(NifStructResourceType {
            res: res.unwrap(),
            struct_type: PhantomData,
        })
    } else {
        None
    }
}

/*pub fn alloc_struct_resource<'a, T>(env: &'a NifEnv, res_type: &NifStructResourceType<T>) -> (&'a mut T, NifTerm<'a>) {
    let res = unsafe { 
        let buf: NIF_RESOURCE_HANDLE = 
            ::wrapper::resource::alloc_resource(res_type.res as NIF_RESOURCE_TYPE, mem::size_of::<T>());
        &mut *(buf as *mut T)
    };
    let res_ptr = (res as *mut T) as *mut c_void;
    let term = NifTerm::new(env, unsafe { ruster_unsafe::enif_make_resource(env.env, res_ptr) });
    unsafe { ruster_unsafe::enif_release_resource(res_ptr) };
    (res, term)
}
pub fn get_struct_resource<'a, T>(env: &'a NifEnv, 
                                  res_type: &NifStructResourceType<T>, term: NifTerm) -> Result<&'a mut T, NifError> {
    let res: &mut T = unsafe { mem::uninitialized() };
    if unsafe { ruster_unsafe::enif_get_resource(env.env, term.term, res_type.res, 
                                     &mut ((res as *mut T) as *mut c_void) as *mut *mut c_void ) } == 0 {
        return Err(NifError::BadArg);
    }
    Ok(res)
}*/
// End Resource Structs

pub fn get_alloc_size_struct<T>() -> usize {
    mem::size_of::<T>() + mem::align_of::<T>()
}

// This is unsafe as it allows us to do nasty things with pointers
pub unsafe fn align_alloced_mem_for_struct<T>(ptr: *mut c_void) -> *mut c_void {
    let offset = mem::align_of::<T>() - ((ptr as usize) % mem::align_of::<T>());
    unsafe { ptr.offset(offset as isize) }
}

use std::sync::RwLock;

pub struct ResourceTypeHolder<'a, T> where T: NifResourceStruct {
    raw: *mut c_void,
    inner: *mut RwLock<T>,
    env_life: PhantomData<&'a NifEnv>
}

use std::sync::{LockResult, RwLockReadGuard, RwLockWriteGuard};
impl<'a, T> ResourceTypeHolder<'a, T> where T: NifResourceStruct {
    pub fn new(_env: &'a NifEnv, data: T) -> Self {
        let alloc_size = get_alloc_size_struct::<RwLock<T>>();
        let mem_raw = ::wrapper::resource::alloc_resource(T::get_type().res, alloc_size);
        let aligned_mem = unsafe { align_alloced_mem_for_struct::<RwLock<T>>(mem_raw) } as *mut RwLock<T>;
        unsafe { ptr::write(aligned_mem, RwLock::new(data)) };
        ResourceTypeHolder {
            raw: mem_raw,
            inner: aligned_mem,
            env_life: PhantomData,
        }
    }
    fn from_term(env: &'a NifEnv, term: NifTerm) -> Result<Self, NifError> {
        let res_resource = match ::wrapper::resource::get_resource(env.as_c_arg(), term.as_c_arg(), T::get_type().res) {
            Some(res) => res,
            None => return Err(NifError::BadArg),
        };
        ::wrapper::resource::keep_resource(res_resource);
        let casted_ptr = unsafe { align_alloced_mem_for_struct::<RwLock<T>>(res_resource) } as *mut RwLock<T>;
        Ok(ResourceTypeHolder {
            raw: res_resource,
            inner: casted_ptr,
            env_life: PhantomData,
        })
    }

    fn as_term<'b>(&self, env: &'b NifEnv) -> NifTerm<'b> {
        NifTerm::new(env, ::wrapper::resource::make_resource(env.as_c_arg(), self.raw))
    }

    fn as_c_arg(&mut self) -> *mut c_void {
        self.raw
    }

    pub fn read(&self) -> LockResult<RwLockReadGuard<T>> {
        self.get_rwlock().read()
    }
    pub fn write(&self) -> LockResult<RwLockWriteGuard<T>> {
        self.get_rwlock().write()
    }

    pub fn get_rwlock(&self) -> &RwLock<T> {
        unsafe { &*self.inner }
    }
}

impl<'a, T> Drop for ResourceTypeHolder<'a, T> where T: NifResourceStruct {
    fn drop(&mut self) {
        unsafe { ::wrapper::nif_interface::enif_release_resource(self.as_c_arg()) };
    }
}
