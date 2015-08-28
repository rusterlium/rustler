#[macro_use]
extern crate rustler;

nif_init!(b"Elixir.Rustler.NativeTest\0", Some(load), None, None, None,
        nif_init_func!(b"test_call\0", 0, test_call));

nif_func_args!(test_call, (), |env, ()| {

});
