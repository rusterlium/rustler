use std::alloc::{GlobalAlloc, Layout};

const SIZEOF_USIZE: usize = std::mem::size_of::<usize>();
const MAX_ALIGN: usize = 8;

#[cfg(feature = "allocator")]
#[global_allocator]
static ALLOCATOR: EnifAllocator = EnifAllocator;

/// Allocator implementation that forwards all allocation calls to Erlang's allocator. Allows the
/// memory usage to be tracked by the BEAM.
pub struct EnifAllocator;

unsafe impl GlobalAlloc for EnifAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        if layout.align() > MAX_ALIGN {
            // Overallocate and store the original pointer in memory immediately before the aligned
            // section.
            //
            // The requested size is chosen such that we can always get an aligned buffer of size
            // `layout.size()`: Ignoring `SIZEOF_USIZE`, there must always be an aligned pointer in
            // the interval `[ptr, layout.align())`, so in the worst case, we have to pad with
            // `layout.align() - 1`. The requirement for an additional `usize` just shifts the
            // problem without changing the padding requirement.
            let total_size = SIZEOF_USIZE + layout.size() + layout.align() - 1;
            let ptr = rustler_sys::enif_alloc(total_size) as *mut u8;

            // Shift the returned pointer to make space for the original pointer
            let ptr1 = ptr.wrapping_add(SIZEOF_USIZE);

            // Align the result to the requested alignment
            let aligned_ptr = ptr1.wrapping_add(ptr1.align_offset(layout.align()));

            // Write the original pointer immediately in front of the aligned pointer
            let header = aligned_ptr.wrapping_sub(SIZEOF_USIZE);
            *(header as *mut usize) = ptr as usize;

            aligned_ptr
        } else {
            rustler_sys::enif_alloc(layout.size()) as *mut u8
        }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        let ptr = if layout.align() > MAX_ALIGN {
            // Retrieve the original pointer
            let header = ptr.wrapping_sub(SIZEOF_USIZE);
            let ptr = *(header as *mut usize);
            ptr as *mut rustler_sys::c_void
        } else {
            ptr as *mut rustler_sys::c_void
        };
        rustler_sys::enif_free(ptr);
    }
}
