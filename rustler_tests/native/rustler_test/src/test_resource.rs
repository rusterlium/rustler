use rustler::{Binary, Env, ResourceArc};
use std::sync::RwLock;

pub struct TestResource {
    test_field: RwLock<i32>,
}

/// This one is designed to look more like pointer data, to increase the
/// chance of segfaults if the implementation is wrong.
pub struct ImmutableResource {
    a: u32,
    b: u32,
}

pub struct WithBinaries {
    a: [u8; 10],
    b: Vec<u8>,
}

pub fn on_load(env: Env) -> bool {
    rustler::resource!(TestResource, env);
    rustler::resource!(ImmutableResource, env);
    rustler::resource!(WithBinaries, env);
    true
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

lazy_static::lazy_static! {
    static ref COUNT: AtomicUsize = AtomicUsize::new(0);
    static ref STATIC_BIN: [u8; 10] = [0,1,2,3,4,5,6,7,8,9];
}

impl ImmutableResource {
    fn new(u: u32) -> ImmutableResource {
        COUNT.fetch_add(1, Ordering::SeqCst);
        ImmutableResource { a: u, b: !u }
    }
}

impl Drop for ImmutableResource {
    fn drop(&mut self) {
        assert_eq!(self.a, !self.b);
        self.b = self.a;
        COUNT.fetch_sub(1, Ordering::SeqCst);
    }
}

#[rustler::nif]
pub fn resource_make_immutable(u: u32) -> ResourceArc<ImmutableResource> {
    ResourceArc::new(ImmutableResource::new(u))
}

// Count how many instances of `ImmutableResource` are currently alive globally.
#[rustler::nif]
pub fn resource_immutable_count() -> u32 {
    COUNT.load(Ordering::SeqCst) as u32
}

#[rustler::nif]
pub fn resource_make_with_binaries() -> ResourceArc<WithBinaries> {
    ResourceArc::new(WithBinaries {
        a: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
        b: vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
    })
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
        resource.make_binary(env, |_| &*STATIC_BIN),
    )
}

#[rustler::nif]
pub fn resource_make_binary_from_vec(env: Env, resource: ResourceArc<WithBinaries>) -> Binary {
    resource.make_binary(env, |w| &w.b)
}
