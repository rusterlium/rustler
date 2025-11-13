mod async_runtime;

pub use async_runtime::AsyncRuntime;

#[cfg(feature = "tokio-rt")]
pub mod tokio;

#[cfg(feature = "tokio-rt")]
pub use tokio::TokioRuntime;

#[cfg(feature = "tokio-rt")]
pub use tokio::{ConfigError, RuntimeConfig};

#[cfg(feature = "async-rt")]
pub mod yielding;

#[cfg(feature = "async-rt")]
pub use yielding::{yield_now, yielding_nif_run, YieldingTaskState};

#[cfg(rustler_unstable)]
pub mod channel;

#[cfg(rustler_unstable)]
pub use channel::{Channel, ChannelSender, ResponseSender};

/// Configure the global async runtime from Elixir configuration.
///
/// This is the recommended way to configure the runtime, allowing Elixir application
/// developers to tune the runtime without recompiling the NIF.
///
/// # Example
///
/// ```ignore
/// use rustler::{Env, Term};
///
/// fn load(_env: Env, load_info: Term) -> bool {
///     if let Ok(config) = load_info.decode::<rustler::runtime::RuntimeConfig>() {
///         rustler::runtime::configure(config)
///             .expect("Failed to configure runtime");
///     }
///     true
/// }
/// ```
#[cfg(feature = "tokio-rt")]
pub fn configure(config: RuntimeConfig) -> Result<(), ConfigError> {
    tokio::configure(config)
}

/// Configure the global async runtime with a builder function.
///
/// This provides a runtime-agnostic API. The builder type is determined
/// by the enabled runtime feature.
///
/// # Example
///
/// ```ignore
/// use rustler::{Env, Term};
///
/// fn load(_env: Env, _: Term) -> bool {
///     rustler::runtime::builder(|builder| {
///         builder
///             .worker_threads(4)
///             .thread_name("myapp-runtime")
///             .thread_stack_size(3 * 1024 * 1024);
///     }).expect("Failed to configure runtime");
///
///     true
/// }
/// ```
#[cfg(feature = "tokio-rt")]
pub fn builder<F>(config_fn: F) -> Result<(), ConfigError>
where
    F: FnOnce(&mut ::tokio::runtime::Builder),
{
    self::tokio::configure_runtime(config_fn)
}

/// Get a handle to the global async runtime.
///
/// This provides a runtime-agnostic API. The handle type is determined
/// by the enabled runtime feature.
///
/// Returns a handle to the current runtime if already inside one, otherwise
/// returns a handle to the global runtime (initializing it with defaults if needed).
///
/// # Example
///
/// ```ignore
/// let handle = rustler::runtime::handle();
/// handle.spawn(async {
///     // Your async code
/// });
/// ```
#[cfg(feature = "tokio-rt")]
pub fn handle() -> ::tokio::runtime::Handle {
    self::tokio::runtime_handle()
}
