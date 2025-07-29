use std::alloc::{alloc, dealloc, Layout};

const HEADER: usize = 8;
const ALIGNMENT: usize = 8;

#[no_mangle]
pub unsafe extern "C" fn enif_alloc(size: usize) -> *mut u8 {
    if let Ok(layout) = Layout::from_size_align(size + HEADER, ALIGNMENT) {
        let ptr = alloc(layout);
        *(ptr as *mut usize) = size;
        return ptr.wrapping_add(HEADER);
    }

    std::ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn enif_free(ptr: *mut u8) {
    let real_ptr = ptr.wrapping_sub(HEADER);
    let size = *(real_ptr as *const usize);
    if let Ok(layout) = Layout::from_size_align(size + HEADER, ALIGNMENT) {
        dealloc(real_ptr, layout);
    }
}

#[no_mangle]
pub static enif_priv_data: usize = 0;
#[no_mangle]
pub static enif_is_atom: usize = 0;
#[no_mangle]
pub static enif_is_binary: usize = 0;
#[no_mangle]
pub static enif_is_ref: usize = 0;
#[no_mangle]
pub static enif_inspect_binary: usize = 0;
#[no_mangle]
pub static enif_alloc_binary: usize = 0;
#[no_mangle]
pub static enif_realloc_binary: usize = 0;
#[no_mangle]
pub static enif_release_binary: usize = 0;
#[no_mangle]
pub static enif_get_int: usize = 0;
#[no_mangle]
pub static enif_get_ulong: usize = 0;
#[no_mangle]
pub static enif_get_double: usize = 0;
#[no_mangle]
pub static enif_get_list_cell: usize = 0;
#[no_mangle]
pub static enif_get_tuple: usize = 0;
#[no_mangle]
pub static enif_is_identical: usize = 0;
#[no_mangle]
pub static enif_compare: usize = 0;
#[no_mangle]
pub static enif_make_binary: usize = 0;
#[no_mangle]
pub static enif_make_badarg: usize = 0;
#[no_mangle]
pub static enif_make_int: usize = 0;
#[no_mangle]
pub static enif_make_ulong: usize = 0;
#[no_mangle]
pub static enif_make_double: usize = 0;
#[no_mangle]
pub static enif_make_atom: usize = 0;
#[no_mangle]
pub static enif_make_existing_atom: usize = 0;
#[no_mangle]
pub static enif_make_tuple: usize = 0;
#[no_mangle]
pub static enif_make_list: usize = 0;
#[no_mangle]
pub static enif_make_list_cell: usize = 0;
#[no_mangle]
pub static enif_make_string: usize = 0;
#[no_mangle]
pub static enif_make_ref: usize = 0;
#[no_mangle]
pub static enif_realloc: usize = 0;
#[no_mangle]
pub static enif_system_info: usize = 0;
#[no_mangle]
pub static enif_fprintf: usize = 0;
#[no_mangle]
pub static enif_inspect_iolist_as_binary: usize = 0;
#[no_mangle]
pub static enif_make_sub_binary: usize = 0;
#[no_mangle]
pub static enif_get_string: usize = 0;
#[no_mangle]
pub static enif_get_atom: usize = 0;
#[no_mangle]
pub static enif_is_fun: usize = 0;
#[no_mangle]
pub static enif_is_pid: usize = 0;
#[no_mangle]
pub static enif_is_port: usize = 0;
#[no_mangle]
pub static enif_get_uint: usize = 0;
#[no_mangle]
pub static enif_get_long: usize = 0;
#[no_mangle]
pub static enif_make_uint: usize = 0;
#[no_mangle]
pub static enif_make_long: usize = 0;
#[no_mangle]
pub static enif_make_tuple_from_array: usize = 0;
#[no_mangle]
pub static enif_make_list_from_array: usize = 0;
#[no_mangle]
pub static enif_is_empty_list: usize = 0;
#[no_mangle]
pub static enif_open_resource_type: usize = 0;
#[no_mangle]
pub static enif_alloc_resource: usize = 0;
#[no_mangle]
pub static enif_release_resource: usize = 0;
#[no_mangle]
pub static enif_make_resource: usize = 0;
#[no_mangle]
pub static enif_get_resource: usize = 0;
#[no_mangle]
pub static enif_sizeof_resource: usize = 0;
#[no_mangle]
pub static enif_make_new_binary: usize = 0;
#[no_mangle]
pub static enif_is_list: usize = 0;
#[no_mangle]
pub static enif_is_tuple: usize = 0;
#[no_mangle]
pub static enif_get_atom_length: usize = 0;
#[no_mangle]
pub static enif_get_list_length: usize = 0;
#[no_mangle]
pub static enif_make_atom_len: usize = 0;
#[no_mangle]
pub static enif_make_existing_atom_len: usize = 0;
#[no_mangle]
pub static enif_make_string_len: usize = 0;
#[no_mangle]
pub static enif_alloc_env: usize = 0;
#[no_mangle]
pub static enif_free_env: usize = 0;
#[no_mangle]
pub static enif_clear_env: usize = 0;
#[no_mangle]
pub static enif_send: usize = 0;
#[no_mangle]
pub static enif_make_copy: usize = 0;
#[no_mangle]
pub static enif_self: usize = 0;
#[no_mangle]
pub static enif_get_local_pid: usize = 0;
#[no_mangle]
pub static enif_keep_resource: usize = 0;
#[no_mangle]
pub static enif_make_resource_binary: usize = 0;
#[no_mangle]
pub static enif_is_exception: usize = 0;
#[no_mangle]
pub static enif_make_reverse_list: usize = 0;
#[no_mangle]
pub static enif_is_number: usize = 0;
#[no_mangle]
pub static enif_dlopen: usize = 0;
#[no_mangle]
pub static enif_dlsym: usize = 0;
#[no_mangle]
pub static enif_consume_timeslice: usize = 0;
#[no_mangle]
pub static enif_is_map: usize = 0;
#[no_mangle]
pub static enif_get_map_size: usize = 0;
#[no_mangle]
pub static enif_make_new_map: usize = 0;
#[no_mangle]
pub static enif_make_map_put: usize = 0;
#[no_mangle]
pub static enif_get_map_value: usize = 0;
#[no_mangle]
pub static enif_make_map_update: usize = 0;
#[no_mangle]
pub static enif_make_map_remove: usize = 0;
#[no_mangle]
pub static enif_map_iterator_create: usize = 0;
#[no_mangle]
pub static enif_map_iterator_destroy: usize = 0;
#[no_mangle]
pub static enif_map_iterator_is_head: usize = 0;
#[no_mangle]
pub static enif_map_iterator_is_tail: usize = 0;
#[no_mangle]
pub static enif_map_iterator_next: usize = 0;
#[no_mangle]
pub static enif_map_iterator_prev: usize = 0;
#[no_mangle]
pub static enif_map_iterator_get_pair: usize = 0;
#[no_mangle]
pub static enif_schedule_nif: usize = 0;
#[no_mangle]
pub static enif_has_pending_exception: usize = 0;
#[no_mangle]
pub static enif_raise_exception: usize = 0;
#[no_mangle]
pub static enif_getenv: usize = 0;
#[no_mangle]
pub static enif_monotonic_time: usize = 0;
#[no_mangle]
pub static enif_time_offset: usize = 0;
#[no_mangle]
pub static enif_convert_time_unit: usize = 0;
#[no_mangle]
pub static enif_now_time: usize = 0;
#[no_mangle]
pub static enif_cpu_time: usize = 0;
#[no_mangle]
pub static enif_make_unique_integer: usize = 0;
#[no_mangle]
pub static enif_is_current_process_alive: usize = 0;
#[no_mangle]
pub static enif_is_process_alive: usize = 0;
#[no_mangle]
pub static enif_is_port_alive: usize = 0;
#[no_mangle]
pub static enif_get_local_port: usize = 0;
#[no_mangle]
pub static enif_term_to_binary: usize = 0;
#[no_mangle]
pub static enif_binary_to_term: usize = 0;
#[no_mangle]
pub static enif_port_command: usize = 0;
#[no_mangle]
pub static enif_thread_type: usize = 0;
#[no_mangle]
pub static enif_snprintf: usize = 0;
#[no_mangle]
pub static enif_select: usize = 0;
#[no_mangle]
pub static enif_open_resource_type_x: usize = 0;
#[no_mangle]
pub static enif_monitor_process: usize = 0;
#[no_mangle]
pub static enif_demonitor_process: usize = 0;
#[no_mangle]
pub static enif_compare_monitors: usize = 0;
#[no_mangle]
pub static enif_hash: usize = 0;
#[no_mangle]
pub static enif_whereis_pid: usize = 0;
#[no_mangle]
pub static enif_whereis_port: usize = 0;
#[no_mangle]
pub static enif_make_map_from_arrays: usize = 0;
#[no_mangle]
pub static enif_make_monitor_term: usize = 0;
#[no_mangle]
pub static enif_is_pid_undefined: usize = 0;
#[no_mangle]
pub static enif_set_pid_undefined: usize = 0;
#[no_mangle]
pub static enif_term_type: usize = 0;
