use std::collections::HashMap;
use std::sync::atomic::{AtomicI64, Ordering};

use rustler::{Atom, Binary, Encoder, Env, NifMap, ResourceArc};
use rustler_beam_test::{receive, Receive};

use crate::{atoms, send_factorial, Counter};

/// A test that needs no BEAM runs there like the others.
#[rustler_beam_test::test]
fn factorial_of_five() {
    assert_eq!(crate::factorial(5), 120);
}

/// A name that the attribute used for an item of its own.
const NAME: &str = "Ada";

#[rustler_beam_test::test]
fn a_test_sees_the_items_of_its_module() {
    assert_eq!(NAME, "Ada");
}

#[rustler_beam_test::test]
fn integers_round_trip(env: Env) {
    for n in [i64::MIN, -1, 0, 1, i64::MAX] {
        assert_eq!(n.encode(env).decode::<i64>().unwrap(), n);
    }
    assert_eq!(u128::MAX.encode(env).decode::<u128>().unwrap(), u128::MAX);
    assert!(300.encode(env).decode::<u8>().is_err());
    assert!((-1).encode(env).decode::<u64>().is_err());
}

#[rustler_beam_test::test]
fn floats_round_trip(env: Env) {
    assert_eq!(1.5f64.encode(env).decode::<f64>().unwrap(), 1.5);
    // An integer decodes as a float, but a float does not decode as an integer.
    assert_eq!(3.encode(env).decode::<f64>().unwrap(), 3.0);
    assert!(3.0.encode(env).decode::<i64>().is_err());
}

#[rustler_beam_test::test]
fn strings_and_binaries_round_trip(env: Env) {
    let term = "Grüezi".encode(env);
    assert!(term.is_binary());
    assert_eq!(term.decode::<String>().unwrap(), "Grüezi");
    assert_eq!(
        term.decode::<Binary>().unwrap().as_slice(),
        "Grüezi".as_bytes()
    );
}

#[rustler_beam_test::test]
fn atoms_round_trip(env: Env) {
    let term = atoms::hello().encode(env);
    assert_eq!(term.atom_to_string().unwrap(), "hello");
    assert_eq!(term.decode::<Atom>().unwrap(), atoms::hello());
    assert_eq!(Atom::from_str(env, "hello").unwrap(), atoms::hello());
    assert_eq!(
        true.encode(env).decode::<Atom>().unwrap(),
        rustler::types::atom::true_()
    );
}

#[rustler_beam_test::test]
fn tuples_lists_and_options_round_trip(env: Env) {
    let value = vec![(1u8, Some("one".to_string())), (2, None)];
    let term = value.encode(env);
    assert!(term.is_list());
    assert_eq!(term.decode::<Vec<(u8, Option<String>)>>().unwrap(), value);
    assert_eq!(term.list_length().unwrap(), 2);
}

#[derive(NifMap, Debug, PartialEq)]
struct Point {
    x: i64,
    y: i64,
}

#[rustler_beam_test::test]
fn maps_round_trip(env: Env) {
    let ages = HashMap::from([("ada".to_string(), 36), ("joe".to_string(), 68)]);
    let term = ages.encode(env);
    assert_eq!(term.map_size().unwrap(), 2);
    assert_eq!(term.map_get("joe").unwrap().decode::<i64>().unwrap(), 68);
    assert_eq!(term.decode::<HashMap<String, i64>>().unwrap(), ages);

    let point = Point { x: 3, y: -4 };
    let term = point.encode(env);
    assert_eq!(
        term.map_get(Atom::from_str(env, "y").unwrap())
            .unwrap()
            .decode::<i64>()
            .unwrap(),
        -4
    );
    assert_eq!(term.decode::<Point>().unwrap(), point);
}

#[rustler_beam_test::test]
fn a_resource_reads_back_from_its_term(env: Env) {
    let counter = ResourceArc::new(Counter(AtomicI64::new(40)));
    let term = counter.encode(env);
    assert!(term.is_ref());

    let read_back: ResourceArc<Counter> = term.decode().unwrap();
    assert_eq!(read_back.add(2), 42);
    assert_eq!(counter.0.load(Ordering::Relaxed), 42);
    assert!(term.decode::<ResourceArc<Counter>>().is_ok());
    assert!(1.encode(env).decode::<ResourceArc<Counter>>().is_err());
}

#[rustler_beam_test::test]
fn a_message_to_self_arrives(env: Env) -> Receive {
    let pid = env.pid();
    assert!(env.is_process_alive(pid));
    env.send(&pid, (atoms::hello(), 42)).unwrap();

    receive(|_env, message| {
        assert_eq!(
            message.decode::<(Atom, i64)>().unwrap(),
            (atoms::hello(), 42)
        );
    })
}

#[rustler_beam_test::test]
fn a_thread_sends_its_result_to_the_caller(env: Env) -> Receive {
    send_factorial(env.pid(), 20);

    receive(|_env, message| {
        let (tag, result): (Atom, u64) = message.decode().unwrap();
        assert_eq!(tag, atoms::factorial());
        assert_eq!(result, 2_432_902_008_176_640_000);
    })
}

#[rustler_beam_test::test]
fn messages_arrive_in_the_order_sent(env: Env) -> Receive {
    let pid = env.pid();
    env.send(&pid, 1).unwrap();
    env.send(&pid, 2).unwrap();

    receive(|_env, first| {
        assert_eq!(first.decode::<i64>().unwrap(), 1);
        receive(|_env, second| assert_eq!(second.decode::<i64>().unwrap(), 2))
    })
}

#[rustler_beam_test::test]
#[should_panic(expected = "the BEAM lost our number")]
fn a_panic_in_the_beam_fails_the_test(env: Env) {
    assert_eq!(
        1.encode(env).decode::<i64>().unwrap(),
        2,
        "the BEAM lost our number"
    );
}

#[rustler_beam_test::test]
#[should_panic = "lost our number"]
fn should_panic_also_takes_the_text_after_an_equals_sign() {
    panic!("the BEAM lost our number");
}

/// `cargo test -p beam_test_demo -- --ignored fails_on_purpose` shows the
/// report of a failure.
#[rustler_beam_test::test]
#[ignore = "fails on purpose"]
fn fails_on_purpose(env: Env) {
    assert_eq!(
        1.encode(env).decode::<i64>().unwrap(),
        2,
        "the BEAM lost our number"
    );
}

/// `cargo test -p beam_test_demo -- --ignored crashes_on_purpose` shows the
/// report of a test that kills the BEAM.
#[rustler_beam_test::test]
#[ignore = "kills the BEAM on purpose"]
fn crashes_on_purpose() {
    unsafe { std::ptr::null_mut::<u8>().write_volatile(1) }
}
