extern crate ruster_unsafe;
use self::ruster_unsafe::{ ErlNifResourceFlags, ErlNifResourceType };

extern crate libc;
use self::libc::{ c_void, size_t };

use std::ffi::CString;
use std::mem;
use std::marker::PhantomData;

use super::{ NifTerm, NifEnv, NifError, NifResourceType, NifStructResourceType };

// Resources
pub fn open_resource_type_raw(env: &NifEnv, module: &str, name: &str, 
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
}
// End Resources

// Resource Structs
// TODO: Provide synchronization. THIS IS IMPORTANT
// TODO: Handle destruction. This is important, heap allocated objects will leak!
pub fn open_struct_resource_type<T>(env: &NifEnv, module: &str, name: &str,
                                 flags: ErlNifResourceFlags) -> Result<NifStructResourceType<T>, &'static str> {
    let res: NifResourceType = try!(open_resource_type_raw(env, module, name, flags));
    Ok(NifStructResourceType {
        res: res,
        struct_type: PhantomData,
    })
}
pub fn alloc_struct_resource<'a, T>(env: &'a NifEnv, res_type: &NifStructResourceType<T>) -> (&'a mut T, NifTerm<'a>) {
    let res = unsafe { 
        let buf: *mut c_void = alloc_resource_raw(&res_type.res, mem::size_of::<T>());
        &mut *(buf as *mut T)
    };
    let res_ptr = (res as *mut T) as *mut c_void;
    let term = NifTerm::new(env, unsafe { ruster_unsafe::enif_make_resource(env.env, res_ptr) });
    unsafe { ruster_unsafe::enif_release_resource(res_ptr) };
    (res, term)
}
pub fn get_struct_resource<'a, T>(env: &'a NifEnv, 
                                  res_type: &NifStructResourceType<T>, term: NifTerm)-> Result<&'a mut T, NifError> {
    let res: &mut T = unsafe { mem::uninitialized() };
    if unsafe { ruster_unsafe::enif_get_resource(env.env, term.term, res_type.res.res, 
                                     &mut ((res as *mut T) as *mut c_void) as *mut *mut c_void ) } == 0 {
        return Err(NifError::BadArg);
    }
    Ok(res)
}
// End Resource Structs
