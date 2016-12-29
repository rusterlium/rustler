use rustler::NifEncoder;
use rustler::{NifEnv, NifTerm, NifResult};
use rustler::resource::ResourceCell;
use std::sync::RwLock;

struct TestResource {
    test_field: RwLock<i32>,
}

pub fn on_load(env: &NifEnv) -> bool {
    resource_struct_init!(TestResource, env);
    true
}

pub fn resource_make<'a>(env: &'a NifEnv, _args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let data = TestResource {
        test_field: RwLock::new(0),
    };
    let resource = ResourceCell::new(data);

    Ok(resource.encode(env))
}

pub fn resource_set_integer_field<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let resource: ResourceCell<TestResource> = try!(args[0].decode());
    let mut test_field = resource.test_field.write().unwrap();
    *test_field = try!(args[1].decode());

    Ok("ok".encode(env))
}

pub fn resource_get_integer_field<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) ->  NifResult<NifTerm<'a>> {
    let resource: ResourceCell<TestResource> = try!(args[0].decode());
    let test_field = resource.test_field.read().unwrap();
    Ok(test_field.encode(env))
}
