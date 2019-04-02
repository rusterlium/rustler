/*!
Low level Rust bindings to the [Erlang NIF API](http://www.erlang.org/doc/man/erl_nif.html).

# NIF Crate

A NIF module is built by creating a new crate that uses `erlang_nif-sys` as a dependency.
(more)

# NIF Functions

All NIF functions must have the following signature:

```
#[macro_use]
extern crate erl_nif_sys;
use erl_nif_sys::*;
# fn main(){} //0
fn my_nif(env: *mut ErlNifEnv,
          argc: c_int,
          args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    // ...
#   unsafe{enif_make_badarg(env)}
}

```

# NIF Module Initialization

## For the Impatient
```
#[macro_use]
extern crate erl_nif_sys;
use erl_nif_sys::*;

nif_init!("my_nif_module",[
        ("my_nif_fun1", 1, my_nif_fun1),
        ("my_dirty_fun2", 1, my_dirty_fun2, ERL_NIF_DIRTY_JOB_CPU_BOUND)
    ],
    {load: my_load}
);
# fn main(){} //1
# fn my_load(env: *mut ErlNifEnv, priv_data: *mut *mut c_void, load_info: ERL_NIF_TERM)-> c_int { 0 }
# fn my_nif_fun1(_: *mut ErlNifEnv,_: c_int,args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {unsafe{*args}}
# fn my_dirty_fun2(_: *mut ErlNifEnv,_: c_int,args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {unsafe{*args}}
```

## Details

The `erlang_nif-sys` analog of [`ERL_NIF_INIT()`](http://www.erlang.org/doc/man/erl_nif_init.html) is `nif_init!` which has the following form:

`nif_init!(module_name, [nif_funcs], {options})`

`module_name` must be a string literal, for example `"mynifmodule"`.


`nif_funcs` declares all the exported NIF functions for this module.  Each entry is declared as

`(name, arity, function, flags)`

`name` is a string literal indicating the name of the function as seen from Erlang code.
`arity` is an integer indicating how many parameter this function takes as seen from Erlang code.
`function` is the Rust implentation of the NIF and must be of the form
`Fn(env: *mut ErlNifEnv, argc: c_int, args: *const ERL_NIF_TERM) -> ERL_NIF_TERM`.  This is usually a plain
Rust function, but closures are permitted.
`flags` is optional and allows you to specify if this NIF is to run on a dirty scheduler.  See [dirty NIFs](http://www.erlang.org/doc/man/erl_nif.html#dirty_nifs)
in the Erlang docs.

The `options` are the NIF module intialization functions [`load`](http://www.erlang.org/doc/man/erl_nif.html#load), [`reload`](http://www.erlang.org/doc/man/erl_nif.html#reload),
[`upgrade`](http://www.erlang.org/doc/man/erl_nif.html#upgrade), and [`unload`](http://www.erlang.org/doc/man/erl_nif.html#unload).
Each is optional and is specified in struct-init style if present.  If no options are needed,
the curly braces may be elided.  Stub implementation of all these functions looks something like:

```
#[macro_use]
extern crate erl_nif_sys;
use erl_nif_sys::*;

nif_init!("mymod", [], {load: load, reload: reload, upgrade: upgrade, unload: unload});

fn load(env: *mut ErlNifEnv,
        priv_data: *mut *mut c_void,
        load_info: ERL_NIF_TERM)-> c_int { 0 }

fn reload(env: *mut ErlNifEnv,
          priv_data: *mut *mut c_void,
          load_info: ERL_NIF_TERM) -> c_int { 0 }

fn upgrade(env: *mut ErlNifEnv,
           priv_data: *mut *mut c_void,
           old_priv_data: *mut *mut c_void,
                      load_info: ERL_NIF_TERM) -> c_int { 0 }

fn unload(env: *mut ErlNifEnv,
          priv_data: *mut c_void) {}

# fn main(){} //2
```

# Invoking NIF API

As with any Rust FFI call, NIF API calls must be wrapped in `unsafe` blocks.
Below is an example of invoking NIF APIs along with an approach for dealing with
the the `args` parameter.

```
extern crate erl_nif_sys;
use erl_nif_sys::*;
use std::mem;
fn native_add(env: *mut ErlNifEnv,
              argc: c_int,
              args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    unsafe {
        let mut a: c_int = mem::uninitialized();
        let mut b: c_int = mem::uninitialized();
        if argc == 2 &&
           0 != enif_get_int(env, *args, &mut a) &&
           0 != enif_get_int(env, *args.offset(1), &mut b) {
            enif_make_int(env, a+b)
         }
         else {
            enif_make_badarg(env)
         }
    }
}
# fn main(){} //3
```

*/

// Don't throw warnings on NIF naming conventions
#![allow(non_camel_case_types)]

#[cfg(windows)]
extern crate unreachable;

#[macro_use]
mod initmacro;

pub mod erl_nif_sys_api;

pub use erl_nif_sys_api::*;
