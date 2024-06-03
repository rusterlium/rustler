use rustler::{Binary, Env, LocalPid, Monitor, Resource, ResourceArc};
use std::sync::{Mutex, OnceLock, RwLock};

pub struct TestResource {
    test_field: RwLock<i32>,
}

struct TestMonitorResourceInner {
    mon: Option<Monitor>,
    down_called: bool,
}

pub struct TestMonitorResource {
    inner: Mutex<TestMonitorResourceInner>,
}

impl Resource for TestMonitorResource {
    const IMPLEMENTS_DOWN: bool = true;

    fn down<'a>(&'a self, _env: Env<'a>, _pid: LocalPid, mon: Monitor) {
        let mut inner = self.inner.lock().unwrap();
        assert!(Some(mon) == inner.mon);
        inner.down_called = true;
    }
}

/// This one is designed to look more like pointer data, to increase the
/// chance of segfaults if the implementation is wrong.
#[derive(Debug)]
pub struct ImmutableResource {
    a: u32,
    b: u32,
}

impl Resource for ImmutableResource {}

pub struct WithBinaries {
    a: [u8; 10],
    b: Vec<u8>,
}

pub fn on_load(env: Env) -> bool {
    rustler::resource!(TestResource, env)
        && env.register::<ImmutableResource>().is_ok()
        && env.register::<TestMonitorResource>().is_ok()
        && rustler::resource!(WithBinaries, env)
}

#[rustler::nif]
pub fn resource_make() -> ResourceArc<TestResource> {
    ResourceArc::new(TestResource {
        test_field: RwLock::new(0),
    })
}

#[rustler::nif]
pub fn resource_set_integer_field(resource: ResourceArc<TestResource>, n: i32) -> &'static str {
    let mut test_field = resource.test_field.write().unwrap();
    *test_field = n;

    "ok"
}

#[rustler::nif]
pub fn resource_get_integer_field(resource: ResourceArc<TestResource>) -> i32 {
    *resource.test_field.read().unwrap()
}

use std::sync::atomic::{AtomicUsize, Ordering};

fn get_count() -> &'static AtomicUsize {
    static COUNT: OnceLock<AtomicUsize> = OnceLock::new();
    COUNT.get_or_init(|| AtomicUsize::new(0))
}

fn get_static_bin() -> &'static [u8; 10] {
    static STATIC_BIN: OnceLock<[u8; 10]> = OnceLock::new();
    STATIC_BIN.get_or_init(|| [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
}

impl ImmutableResource {
    fn new(u: u32) -> ImmutableResource {
        get_count().fetch_add(1, Ordering::SeqCst);
        ImmutableResource { a: u, b: !u }
    }
}

impl Drop for ImmutableResource {
    fn drop(&mut self) {
        assert_eq!(self.a, !self.b);
        self.b = self.a;
        get_count().fetch_sub(1, Ordering::SeqCst);
    }
}

#[rustler::nif]
pub fn resource_make_immutable(u: u32) -> ResourceArc<ImmutableResource> {
    ResourceArc::new(ImmutableResource::new(u))
}

// Count how many instances of `ImmutableResource` are currently alive globally.
#[rustler::nif]
pub fn resource_immutable_count() -> u32 {
    get_count().load(Ordering::SeqCst) as u32
}

#[rustler::nif]
pub fn monitor_resource_make() -> ResourceArc<TestMonitorResource> {
    TestMonitorResource {
        inner: Mutex::new(TestMonitorResourceInner {
            mon: None,
            down_called: false,
        }),
    }
    .into()
}

#[rustler::nif]
pub fn resource_make_with_binaries() -> ResourceArc<WithBinaries> {
    WithBinaries {
        a: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
        b: vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
    }
    .into()
}

#[rustler::nif]
pub fn resource_make_binaries(
    env: Env,
    resource: ResourceArc<WithBinaries>,
) -> (Binary, Binary, Binary) {
    // This won't compile:
    // let temp = [1,2,3];
    // resource.make_binary(env, |_w| &temp)

    (
        // From slice (embedded in T)
        resource.make_binary(env, |w| &w.a),
        // From vec (on the heap)
        resource.make_binary(env, |w| &w.b),
        // From static
        resource.make_binary(env, |_| get_static_bin()),
    )
}

#[rustler::nif]
pub fn monitor_resource_monitor(
    env: Env,
    resource: ResourceArc<TestMonitorResource>,
    pid: LocalPid,
) {
    let mut inner = resource.inner.lock().unwrap();
    inner.mon = env.monitor(&resource, &pid);
    assert!(inner.mon.is_some());
    inner.down_called = false;
}

#[rustler::nif]
pub fn monitor_resource_down_called(resource: ResourceArc<TestMonitorResource>) -> bool {
    resource.inner.lock().unwrap().down_called
}

#[rustler::nif]
pub fn monitor_resource_demonitor(env: Env, resource: ResourceArc<TestMonitorResource>) -> bool {
    let inner = resource.inner.lock().unwrap();
    env.demonitor(&resource, inner.mon.as_ref().unwrap())
}
