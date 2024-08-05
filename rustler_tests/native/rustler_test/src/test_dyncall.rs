use rustler::{Env, LocalPid, Term};

rustler::atoms! {
    module = "Elixir.ResourceDyncall",
    resource_name = "resource_with_dyncall",
}

#[repr(C)]
struct Params {
    a: i64,
    b: i64,
    c: i64,
    sent_to: Option<LocalPid>,
}

#[rustler::nif]
pub fn perform_dyncall<'a>(
    env: Env<'a>,
    resource: Term<'a>,
    a: i64,
    b: i64,
    c: i64,
) -> Option<LocalPid> {
    let mut params = Params {
        a,
        b,
        c,
        sent_to: None,
    };

    unsafe {
        let params_ptr = std::ptr::addr_of_mut!(params) as *mut rustler::sys::c_void;
        let _ = env.dynamic_resource_call(module(), resource_name(), resource, params_ptr);
    }

    params.sent_to
}
