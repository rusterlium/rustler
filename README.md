# Rustler

[Documentation](https://docs.rs/rustler/latest/rustler) | [Getting Started](https://github.com/rusterlium/rustler/blob/master/README.md#getting-started) | [Example](https://github.com/rusterlium/NifIo)

[![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/rusterlium/rustler/main.yml)](https://github.com/rusterlium/rustler/actions/workflows/main.yml)
[![Hex.pm package version](https://img.shields.io/hexpm/v/rustler.svg)](https://hex.pm/packages/rustler)
[![Crates.io package version](https://img.shields.io/crates/v/rustler.svg)](https://crates.io/crates/rustler)
[![Last Updated](https://img.shields.io/github/last-commit/rusterlium/rustler.svg)](https://github.com/rusterlium/rustler/commits/master)

Rustler is a library for writing Erlang NIFs in safe Rust code. That means
there should be no ways to crash the BEAM (Erlang VM). The library provides
facilities for generating the boilerplate for interacting with the BEAM,
handles encoding and decoding of Erlang terms, and catches rust panics before
they unwind into C.

The library provides functionality for both Erlang and Elixir, however Elixir
is favored as of now.

#### Features

**Safety**  
The code you write in a Rust NIF should never be able to crash the BEAM.

**Interop**  
Decoding and encoding rust values into Erlang terms is as easy as a function call.

**Type composition**  
Making a Rust struct encodable and decodable to Erlang or Elixir can be done with a single attribute.

**Resource objects**  
Enables you to safely pass a reference to a Rust struct into Erlang code. The struct will be automatically dropped when it's no longer referenced.

#### Getting started

The easiest way of getting started is the [rustler Elixir library](https://hex.pm/packages/rustler).

- Add the [rustler Elixir library](https://hex.pm/packages/rustler) as a
  dependency of your project.
- Run `mix rustler.new` to generate a new NIF in your project. Follow the
  instructions.
- If you are already using [`serde`](https://serde.rs) and/or have been using
  `serde_rustler` before, please enable the `serde` feature in your NIF crate's
  `Cargo.toml` on the `rustler` dependency.

#### What it looks like

This is the code for a minimal NIF that adds two numbers and returns the result.

```rust
#[rustler::nif]
fn add(a: i64, b: i64) -> i64 {
    a + b
}

rustler::init!("Elixir.Math");
```

#### Async NIFs

Async NIFs are unstable and require `RUSTFLAGS="--cfg rustler_unstable"`.
Cooperative NIFs do not require a runtime backend; `#[rustler::task]` uses
the configured runtime and requires a backend such as `tokio-rt`.

Async NIFs can yield cooperatively without an external executor:

```rust
#[rustler::nif]
async fn echo<'a>(
    env: rustler::Env<'a, rustler::ProcessIndependent>,
    term: rustler::Term<'a>,
) -> rustler::Term<'a> {
    let result = env.encode((term, "echo"));
    rustler::yield_now().await;
    result
}
```

Arguments are copied into an `OwnedEnv` that survives every yield. The result
is copied back into the process environment. The suspended future is retained
in a BEAM resource and dropped if the calling process terminates.

Futures must be `Send`, because `enif_schedule_nif` can resume on another
scheduler thread. Async environment handles are not copyable; use
`env.encode(value)` or a temporary `env.as_env()` for existing encoding APIs.
The environment is process-independent, so it cannot provide the caller's
pid or send messages from a scheduler thread. Existing synchronous NIFs are
unchanged.

Cooperative yielding is recognized through an explicit, private poll-local
signal, not ordinary waker notifications. A future returning `Pending` without
`yield_now()` or an enabled executor handoff raises
`async_nif_pending_without_yield`, even if it wakes itself. This is a runtime
scheduling policy, not a compile-time restriction on all nested futures.

#### External executors

Opt in with `#[rustler::nif(executor)]` and submit owned work using
`rustler::on_executor(&adapter, future).await`. The adapter implements
`rustler::Executor`; Rustler itself has no dependency on a particular runtime.
For example, an adapter for an existing Tokio runtime:

```rust
struct TokioExecutor(tokio::runtime::Handle);

impl rustler::Executor for TokioExecutor {
    fn spawn(&self, task: rustler::ExecutorTask) -> Result<(), rustler::ExecutorError> {
        self.0.spawn(task);
        Ok(())
    }
}
```

Keep the runtime alive and supply its adapter to the NIF, for example through
application-owned static state:

```rust
#[rustler::nif(executor)]
async fn delayed_echo(text: String) -> rustler::NifResult<String> {
    let text = rustler::on_executor(executor(), async move {
        tokio::time::sleep(std::time::Duration::from_millis(10)).await;
        text
    })
    .await?;
    rustler::yield_now().await;
    Ok(text)
}
```

Here `executor()` is your application's accessor for its adapter. External work
and its output must be `Send + 'static`; copy borrowed input into owned Rust
values before handing it off. I/O and timers are constructed/polled inside the
submitted async block, under the external runtime's context. The outer NIF
future resumes on the BEAM and retains its owned environment throughout.

Enable the matching Elixir wrappers:

```elixir
use Rustler,
  otp_app: :my_app,
  async_nifs: [delayed_echo: 1]
```

Do not write NIF stubs for functions in `async_nifs`: Rustler generates their
public wrappers and internal stubs. Use the exported name when the NIF has a
`name` override. Cooperative-only NIFs do not need these wrappers.

When external work is pending, the wrapper waits for a correlated message,
without blocking or busy-polling a scheduler, then calls a continuation NIF.
Completion before the wrapper starts waiting is handled without losing wakeups.
Unrelated mailbox messages are left untouched, and results and exceptions retain
their usual public shape.

Process termination drops the outer future and requests cancellation of
external work. Cancellation wakes the task for cleanup on its executor; it
cannot interrupt blocking code or an executor that stops polling. Adapter
rejection, executor shutdown and task panics are reported as `ExecutorError`,
which can be propagated with `?` from a `NifResult`.

For Erlang callers, implement the same protocol: call
`'__rustler_async_<name>'/Arity`, unwrap `{rustler_async_done, Result}`, or receive
`{rustler_async_ready, Ref}` after `{rustler_async_wait, Ref, Resource}` and call
`'__rustler_async_continue'/1` with that resource. Repeat until complete. The
continuation belongs to the originating process. Names prefixed
`__rustler_async_` are reserved for this protocol.

#### Minimal Supported Rust Version (MSRV)

Rustler currently has a minimal supported Rust version (MSRV) of 1.91. This
is the configured version in `.clippy.toml`.

#### Supported OTP and Elixir Versions

Rustler aims to support the newest three major OTP versions as well as newest three minor Elixir versions.

#### Supported NIF version

The minimal supported NIF version for a library should be defined via Cargo
features. The default is currently `2.15` (Erlang/OTP 22). To use features from
NIF version `2.16` (Erlang/OTP 24) or `2.17` (Erlang/OTP 26), the respective
feature flag has to be enabled on the dependency:

```toml
[dependencies]
rustler = { version = "...", features = ["nif_version_2_16"] }
```

#### Community

You can find us in the `#rustler:matrix.org` channel on [Matrix](https://matrix.to/#/#rustler:matrix.org)
or in the `#rustler` channel in [the Elixir lang Slack](https://elixir-slackin.herokuapp.com/).

#### License

Licensed under either of

- Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

##### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
