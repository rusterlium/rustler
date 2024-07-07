use super::util::align_alloced_mem_for_struct;
use super::{Resource, ResourceExt};
use crate::{Decoder, Error, NifResult, Term};
use rustler_sys::c_void;
use std::mem::MaybeUninit;

impl<'a> Term<'a> {
    /// Internal method to retrieve both the "real" resource pointer as well as a pointer to the
    /// `T`-aligned region.
    pub(super) unsafe fn try_get_resource_ptrs<T: Resource>(
        &self,
    ) -> Option<(*const c_void, *mut T)> {
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

    /// Try to retrieve a reference to a resource object of type `T` from this term.
    pub fn try_get_resource<T: Resource>(&self) -> Option<&'a T> {
        unsafe { self.try_get_resource_ptrs().map(|(_, ptr)| &*ptr) }
    }
}

impl<'a, T> Decoder<'a> for &'a T
where
    T: Resource + 'a,
{
    fn decode(term: Term<'a>) -> NifResult<Self> {
        term.try_get_resource().ok_or(Error::BadArg)
    }
}
