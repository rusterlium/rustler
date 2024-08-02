use rustler::{Env, LocalPid, Resource, ResourceArc};

#[repr(C)]
struct Params {
    a: i64,
    b: i64,
    c: i64,
    sent_to: Option<LocalPid>,
}

struct ResourceWithDyncall {
    pid: LocalPid,
}

#[rustler::resource_impl(name = "resource_with_dyncall")]
impl Resource for ResourceWithDyncall {
    unsafe fn dyncall<'a>(&'a self, env: Env<'a>, call_data: *mut rustler::sys::c_void) {
        let p = &mut *(call_data as *mut Params);
        if let Ok(()) = env.send(&self.pid, (p.a, p.b, p.c)) {
            p.sent_to = Some(self.pid);
        } else {
            p.sent_to = None
        }
    }
}

#[rustler::nif]
fn new(pid: LocalPid) -> ResourceArc<ResourceWithDyncall> {
    ResourceWithDyncall { pid }.into()
}

rustler::init!("Elixir.ResourceDyncall");
