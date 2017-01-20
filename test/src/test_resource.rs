use rustler::NifEncoder;
use rustler::{NifEnv, NifTerm, NifResult};
use rustler::resource::ResourceArc;
use std::sync::RwLock;

struct TestResource {
    test_field: RwLock<i32>,
}

/// This one is designed to look more like pointer data, to increase the
/// chance of segfaults if the implementation is wrong.
struct ImmutableResource {
    a: u32,
    b: u32
}

pub fn on_load<'a>(env: NifEnv<'a>) -> bool {
    resource_struct_init!(TestResource, env);
    resource_struct_init!(ImmutableResource, env);
    true
}

pub fn resource_make<'a>(env: NifEnv<'a>, _args: &Vec<NifTerm<'a>>) -> NifResult<NifTerm<'a>> {
    let data = TestResource {
        test_field: RwLock::new(0),
    };
    let resource = ResourceArc::new(data);

    Ok(resource.encode(env))
}

pub fn resource_set_integer_field<'a>(env: NifEnv<'a>, args: &Vec<NifTerm<'a>>) -> NifResult<NifTerm<'a>> {
    let resource: ResourceArc<TestResource> = try!(args[0].decode());
    let mut test_field = resource.test_field.write().unwrap();
    *test_field = try!(args[1].decode());

    Ok("ok".encode(env))
}

pub fn resource_get_integer_field<'a>(env: NifEnv<'a>, args: &Vec<NifTerm<'a>>) ->  NifResult<NifTerm<'a>> {
    let resource: ResourceArc<TestResource> = try!(args[0].decode());
    let test_field = resource.test_field.read().unwrap();
    Ok(test_field.encode(env))
}


use std::sync::atomic::{ AtomicUsize, Ordering };

lazy_static! {
    static ref COUNT: AtomicUsize = AtomicUsize::new(0);
}

impl ImmutableResource {
    fn new(u: u32) -> ImmutableResource {
        COUNT.fetch_add(1, Ordering::SeqCst);
        ImmutableResource {
            a: u,
            b: !u
        }
    }
}

impl Drop for ImmutableResource {
    fn drop(&mut self) {
        assert_eq!(self.a, !self.b);
        self.b = self.a;
        COUNT.fetch_sub(1, Ordering::SeqCst);
    }
}

pub fn resource_make_immutable<'a>(env: NifEnv<'a>, args: &Vec<NifTerm<'a>>) -> NifResult<NifTerm<'a>> {
    let u: u32 = try!(args[0].decode());
    Ok(ResourceArc::new(ImmutableResource::new(u)).encode(env))
}

/// Count how many instances of `ImmutableResource` are currently alive globally.
pub fn resource_immutable_count<'a>(env: NifEnv<'a>, _args: &Vec<NifTerm<'a>>) -> NifResult<NifTerm<'a>> {
    let n = COUNT.load(Ordering::SeqCst) as u32;
    Ok(n.encode(env))
}
