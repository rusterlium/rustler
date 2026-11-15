//! Ten cases of `rustler_tests`, where each takes NIFs in
//! `native/rustler_test`, their stubs in `lib/rustler_test.ex`, and an ExUnit
//! test. Here each is one Rust test, named after the ExUnit test.

use std::collections::HashMap;
use std::sync::RwLock;
use std::thread;

use rustler::env::{OwnedEnv, SavedTerm};
use rustler::types::atom::{error, nil, ok};
use rustler::{Atom, Encoder, Env, ListIterator, NifResult, Resource, ResourceArc, Term};
use rustler_beam_test::{receive, Receive};

rustler::atoms! { great, not_a_number, spaniel, ring_of_power, rope }

// primitives_test.exs

#[rustler_beam_test::test]
fn number_decoding_and_encoding(env: Env) {
    assert_eq!(3u32.encode(env).decode::<u32>().unwrap(), 3);
    assert_eq!((-3i32).encode(env).decode::<i32>().unwrap(), -3);
    assert_eq!(3u8.encode(env).decode::<u8>().unwrap(), 3);
    assert_eq!(2.0f64.encode(env).decode::<f64>().unwrap(), 2.0);
    assert_eq!(3.encode(env).decode::<f32>().unwrap(), 3.0);
    assert_eq!((-1).encode(env).decode::<f64>().unwrap(), -1.0);
}

#[rustler_beam_test::test]
fn number_decoding_should_fail_on_invalid_terms(env: Env) {
    assert!((-1).encode(env).decode::<u32>().is_err());
    assert!("1".encode(env).decode::<u32>().is_err());
    assert!(2_147_483_648i64.encode(env).decode::<i32>().is_err());
}

#[rustler_beam_test::test]
fn narrow_integer_decoding_rejects_values_out_of_range_instead_of_truncating(env: Env) {
    assert_eq!(255.encode(env).decode::<u8>().unwrap(), 255);
    assert!(256.encode(env).decode::<u8>().is_err());
    assert!((-1).encode(env).decode::<u8>().is_err());

    assert_eq!((-128).encode(env).decode::<i8>().unwrap(), -128);
    assert!(128.encode(env).decode::<i8>().is_err());

    assert_eq!(65_535.encode(env).decode::<u16>().unwrap(), 65_535);
    assert!(65_536.encode(env).decode::<u16>().is_err());

    assert_eq!((-32_768).encode(env).decode::<i16>().unwrap(), -32_768);
    assert!(32_768.encode(env).decode::<i16>().is_err());
}

#[rustler_beam_test::test]
fn option_decoding_and_encoding(env: Env) {
    assert_eq!(
        32.0.encode(env).decode::<Option<f64>>().unwrap(),
        Some(32.0)
    );
    assert_eq!(nil().encode(env).decode::<Option<f64>>().unwrap(), None);
    assert!("hello".encode(env).decode::<Option<f64>>().is_err());

    assert_eq!(Some(33.0).encode(env).decode::<f64>().unwrap(), 33.0);
    assert_eq!(None::<f64>.encode(env).decode::<Atom>().unwrap(), nil());
}

#[rustler_beam_test::test]
fn result_decoding_and_encoding(env: Env) {
    type Decoded<'a> = Result<bool, &'a str>;
    assert_eq!(
        (ok(), true).encode(env).decode::<Decoded>().unwrap(),
        Ok(true)
    );
    assert_eq!(
        (error(), "wat").encode(env).decode::<Decoded>().unwrap(),
        Err("wat")
    );
    assert!((great(), true).encode(env).decode::<Decoded>().is_err());

    let encoded = Ok::<usize, String>(1).encode(env);
    assert_eq!(encoded.decode::<(Atom, usize)>().unwrap(), (ok(), 1));
    let encoded = Err::<usize, String>("watwat".into()).encode(env);
    assert_eq!(
        encoded.decode::<(Atom, &str)>().unwrap(),
        (error(), "watwat")
    );
}

// map_test.exs

#[rustler_beam_test::test]
fn generic_maps(env: Env) {
    let map = HashMap::from([(1, "hello".to_string()), (2, "world".to_string())]);
    assert_eq!(
        map.encode(env).decode::<HashMap<i64, String>>().unwrap(),
        map
    );

    let pairs = [
        (1.encode(env), "hello"),
        (not_a_number().encode(env), "world"),
    ];
    let mixed = Term::map_from_pairs(env, &pairs).unwrap();
    assert!(mixed.decode::<HashMap<i64, String>>().is_err());
}

// atom_test.exs

#[rustler_beam_test::test]
fn binary_to_existing_atom(env: Env) {
    assert!(Atom::existing_from_bytes(env, b"test_atom_nonexisting").is_err());
    assert!(Atom::from_bytes(env, b"test_atom_nonexisting").is_ok());
    assert!(Atom::existing_from_bytes(env, b"test_atom_nonexisting").is_ok());
}

// resource_test.exs

struct TestResource {
    test_field: RwLock<i32>,
}

#[rustler::resource_impl]
impl Resource for TestResource {}

#[rustler_beam_test::test]
fn resource_creation_and_interaction(env: Env) {
    let resource = ResourceArc::new(TestResource {
        test_field: RwLock::new(0),
    });
    let term = resource.encode(env);
    assert!(term.is_ref());

    let decoded: ResourceArc<TestResource> = term.decode().unwrap();
    assert_eq!(*decoded.test_field.read().unwrap(), 0);
    *decoded.test_field.write().unwrap() = 10;
    assert_eq!(*resource.test_field.read().unwrap(), 10);
}

struct WithBinaries {
    a: [u8; 10],
    b: Vec<u8>,
}

#[rustler::resource_impl]
impl Resource for WithBinaries {}

static STATIC_BIN: [u8; 10] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];

#[rustler_beam_test::test]
fn resource_binaries(env: Env) {
    let resource = ResourceArc::new(WithBinaries {
        a: STATIC_BIN,
        b: STATIC_BIN.to_vec(),
    });
    // From slice (embedded in T)
    let slice = resource.make_binary(env, |w| &w.a);
    // From vec (on the heap)
    let vec = resource.make_binary(env, |w| &w.b);
    // From static
    let from_static = resource.make_binary(env, |_| &STATIC_BIN);

    assert_eq!(slice.as_slice(), vec.as_slice());
    assert_eq!(vec.as_slice(), from_static.as_slice());
}

// env_test.exs

/// A "threaded NIF": it spawns a thread that sends all sublists of `list` to
/// the calling process later. The body is the one of `sublists` in
/// `rustler_test`.
fn sublists<'a>(env: Env<'a>, list: Term<'a>) -> NifResult<Atom> {
    let pid = env.pid();
    let mut owned_env = OwnedEnv::new();

    let saved_reversed_list = owned_env.run(|env| -> NifResult<SavedTerm> {
        let list_arg = list.in_env(env);
        Ok(owned_env.save(list_arg.list_reverse()?))
    })?;

    thread::spawn(move || {
        let _ = owned_env.send_and_clear(&pid, |env| {
            let result: NifResult<Term> = (|| {
                let reversed_list = saved_reversed_list.load(env);
                let iter: ListIterator = reversed_list.decode()?;

                let empty_list = Vec::<Term>::new().encode(env);
                let mut all_sublists = vec![empty_list];

                for element in iter {
                    for i in 0..all_sublists.len() {
                        let new_list = all_sublists[i].list_prepend(element);
                        all_sublists.push(new_list);
                    }
                }

                Ok(all_sublists.encode(env))
            })();

            match result {
                Err(_err) => env.error_tuple("test failed".encode(env)),
                Ok(term) => term,
            }
        });
    });

    Ok(ok())
}

#[rustler_beam_test::test]
fn owned_environments(env: Env) -> NifResult<Receive> {
    let [spaniel, ring, rope] = [(spaniel(), 34), (ring_of_power(), 75), (rope(), 4)];
    sublists(env, vec![spaniel, ring, rope].encode(env))?;

    Ok(receive(move |_env, message| {
        assert_eq!(
            message.decode::<Vec<Vec<(Atom, i64)>>>().unwrap(),
            [
                vec![],
                vec![rope],
                vec![ring],
                vec![ring, rope],
                vec![spaniel],
                vec![spaniel, rope],
                vec![spaniel, ring],
                vec![spaniel, ring, rope],
            ]
        );
    }))
}
