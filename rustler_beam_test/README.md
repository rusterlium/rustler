# rustler_beam_test

Write the tests of a Rustler NIF library in Rust, and run them inside a real
BEAM with `cargo test`.

A test binary has no BEAM, so a test that calls into the NIF API cannot run in
it. `#[rustler_beam_test::test]` compiles the test into the NIF library
instead. A test target then builds the library, loads it into an `erl` node,
and runs each test there as a NIF. `cargo test` lists, filters, and reports
them like any other test.

```rust,ignore
#[cfg(feature = "beam_test")]
mod tests {
    use rustler::{Encoder, Env};
    use rustler_beam_test::{receive, Receive};

    #[rustler_beam_test::test]
    fn an_integer_round_trips(env: Env) {
        assert_eq!(42.encode(env).decode::<i64>().unwrap(), 42);
    }

    #[rustler_beam_test::test]
    fn a_message_to_self_arrives(env: Env) -> Receive {
        env.send(&env.pid(), "hello").unwrap();
        receive(|_env, message| assert_eq!(message.decode::<&str>().unwrap(), "hello"))
    }
}
```

## Setup

It works on Linux so far, and needs `erl` on the `PATH`.

```toml
[lib]
crate-type = ["cdylib"]
# Without a BEAM, the test binary of a NIF library does not link on Linux.
test = false

[[test]]
name = "beam"
harness = false

[features]
beam_test = ["dep:rustler_beam_test"]

[dependencies]
rustler = "..."
rustler_beam_test = { version = "...", optional = true }

[dev-dependencies]
rustler_beam_test_runner = "..."
```

The feature must have the name `beam_test`. The test target is one line, in
`tests/beam.rs`:

```rust,ignore
rustler_beam_test_runner::main!();
```

## Tests

A test takes no argument or a `rustler::Env`. It fails when it panics, or when
it returns an `Err`. `#[ignore]`, `#[should_panic]`, and
`#[should_panic(expected = "...")]` work as on a `#[test]`.

Each test runs as one NIF call in a new process, on a normal scheduler, so
the `Env` is bound to a process, as in a NIF. The tests share the node and run
in parallel.

A NIF cannot read the mailbox of its process. To check a message, a test
returns `receive(...)`: the process then takes its next message and passes it
to the closure, in a second NIF call. The test fails when no message arrives
within 5 seconds.

`#[rustler::nif]` hides the function that it wraps, so a test cannot call it.
Test the function that the NIF calls.

## How it works

The attribute registers the test as one more NIF of the library, like
`#[rustler::nif]` does, behind the `beam_test` feature. The test target is
free of Rustler, so it links without a BEAM. It:

1. builds the library with `cargo build --lib --features beam_test`, in the
   profile of the test target, into a directory of its own in the target
   directory,
2. starts `erl` with a small driver, which connects back to the test target
   on the loopback interface and proves with a random token that it is the
   node that the test target started,
3. has the driver load the library and send back the names of its NIFs,
   among them the tests, and then run each test that `cargo test` selects.

`erlang:load_nif/2` accepts a library only from the module that
`rustler::init!` names, and only when that module defines every NIF. The
driver knows neither. `rustler_beam_test` turns on the `nif_manifest` feature
of Rustler, with which the library writes both to a file when the BEAM asks it
for them. The BEAM does that before it checks them, so the driver calls
`erlang:load_nif/2` once from its own module, which the BEAM refuses, reads
the file, compiles the right module with a stub for each NIF, and loads the
library from it. So the library loads as it does in production, with all its
NIFs, its `load` function, and its resource types.

A failed test reports the panic message and its location. Set
`RUST_BACKTRACE=1` to get the backtrace too.

When a test kills the node, for example with a segmentation fault, that test
fails with the exit status of the node, and the next test starts a new node.
When several tests were running at that moment, each of them runs again
alone, so that only the one that kills the node fails.

Output that a test prints inside the node goes to the terminal directly.
`cargo test` does not capture it.
