//! Support for storing Rust data in Erlang terms.
//!
//! A NIF resource allows you to safely store Rust structs in a term, and therefore keep it across
//! NIF calls. The struct will be automatically dropped when the BEAM GC decides that there are no
//! more references to the resource.

mod handle;
mod monitor;
mod registration;
mod traits;
mod util;

use std::mem::MaybeUninit;

use super::{Decoder, Error, NifResult, Term};

pub use handle::ResourceArc;
pub use monitor::Monitor;
use rustler_sys::c_void;
pub use traits::Resource;
use traits::ResourceExt;
use util::align_alloced_mem_for_struct;

impl<'a> Term<'a> {
    unsafe fn get_resource_ptrs<T: Resource>(&self) -> Option<(*const c_void, *mut T)> {
        let typ = T::get_resource_type()?;
        let mut ret_obj = MaybeUninit::uninit();
        let res = rustler_sys::enif_get_resource(
            self.get_env().as_c_arg(),
            self.as_c_arg(),
            typ,
            ret_obj.as_mut_ptr(),
        );

        if res == 0 {
            None
        } else {
            let res = ret_obj.assume_init();
            Some((res, align_alloced_mem_for_struct::<T>(res) as *mut T))
        }
    }

    pub fn get_resource<T: Resource>(&self) -> Option<&'a T> {
        unsafe { self.get_resource_ptrs().map(|(_, ptr)| &*ptr) }
    }

    pub unsafe fn get_mut_resource<T: Resource>(&self) -> Option<&'a mut T> {
        self.get_resource_ptrs().map(|(_, ptr)| &mut *ptr)
    }
}

impl<'a, T> Decoder<'a> for &'a T
where
    T: Resource + 'a,
{
    fn decode(term: Term<'a>) -> NifResult<Self> {
        term.get_resource().ok_or(Error::BadArg)
    }
}

/// Indicates that a resource has not been registered successfully
#[derive(Clone, Copy, Debug)]
pub struct ResourceInitError;

#[macro_export]
macro_rules! resource {
    ($struct_name:ty, $env: ident) => {{
        impl $crate::Resource for $struct_name {}
        $env.register::<$struct_name>().is_ok()
    }};
}
