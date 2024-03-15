use std::alloc::{GlobalAlloc, Layout};

#[cfg(feature = "allocator")]
#[global_allocator]
static ALLOCATOR: EnifAllocator = EnifAllocator;

/// Allocator implementation that forwards all allocation calls to Erlang's allocator. Allows the
/// memory usage to be tracked by the BEAM.
pub struct EnifAllocator;

unsafe impl GlobalAlloc for EnifAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        // TODO: Check enif_alloc's real alignment
        rustler_sys::enif_alloc(layout.size().max(layout.align())) as *mut u8
    }

    unsafe fn dealloc(&self, ptr: *mut u8, _layout: Layout) {
        rustler_sys::enif_free(ptr as *mut rustler_sys::c_void);
    }
}
