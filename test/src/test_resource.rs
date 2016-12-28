use rustler::NifEncoder;
use rustler::{NifEnv, NifTerm, NifResult};
use rustler::resource::ResourceCell;
use std::sync::RwLock;

struct TestResourceOuter(RwLock<TestResource>);
struct TestResource {
    test_field: i32
}

pub fn on_load(env: &NifEnv) -> bool {
    resource_struct_init!(TestResourceOuter, env);
    true
}

pub fn resource_make<'a>(env: &'a NifEnv, _args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let data = TestResource {
        test_field: 0,
    };
    let resource = ResourceCell::new(TestResourceOuter(RwLock::new(data)));

    Ok(resource.encode(env))
}

pub fn resource_set_integer_field<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let resource: ResourceCell<TestResourceOuter> = try!(args[0].decode());
    let mut data = resource.0.write().unwrap();
    data.test_field = try!(args[1].decode());

    Ok("ok".encode(env))
}

pub fn resource_get_integer_field<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) ->  NifResult<NifTerm<'a>> {
    let resource: ResourceCell<TestResourceOuter> = try!(args[0].decode());
    let data = resource.0.read().unwrap();
    Ok(data.test_field.encode(env))
}
