use crate::{Decoder, NifResult, Term};
use once_cell::sync::OnceCell;
use std::sync::Arc;
use tokio::runtime::Runtime;

/// Global tokio runtime for async NIFs.
///
/// This runtime can be configured via `configure_runtime()` in your NIF's `load` callback,
/// or will be lazily initialized with default settings on first use.
static TOKIO_RUNTIME: OnceCell<Arc<Runtime>> = OnceCell::new();

/// Error type for runtime configuration failures.
#[derive(Debug)]
pub enum ConfigError {
    /// The runtime has already been initialized (either by configuration or first use).
    AlreadyInitialized,
    /// Failed to build the Tokio runtime.
    BuildFailed(std::io::Error),
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConfigError::AlreadyInitialized => {
                write!(f, "Tokio runtime already initialized")
            }
            ConfigError::BuildFailed(e) => {
                write!(f, "Failed to build Tokio runtime: {}", e)
            }
        }
    }
}

impl std::error::Error for ConfigError {}

/// Configuration options for the Tokio runtime.
///
/// These can be passed from Elixir via the `load_data` option:
///
/// ```elixir
/// use Rustler,
///   otp_app: :my_app,
///   crate: :my_nif,
///   load_data: [
///     worker_threads: 4,
///     thread_name: "my-runtime"
///   ]
/// ```
#[derive(Debug, Clone)]
pub struct RuntimeConfig {
    /// Number of worker threads for the runtime.
    /// If not specified, uses Tokio's default (number of CPU cores).
    pub worker_threads: Option<usize>,

    /// Thread name prefix for worker threads.
    /// If not specified, uses "rustler-tokio".
    pub thread_name: Option<String>,

    /// Stack size for worker threads in bytes.
    /// If not specified, uses Tokio's default.
    pub thread_stack_size: Option<usize>,
}

impl Default for RuntimeConfig {
    fn default() -> Self {
        RuntimeConfig {
            worker_threads: None,
            thread_name: Some("rustler-tokio".to_string()),
            thread_stack_size: None,
        }
    }
}

impl<'a> Decoder<'a> for RuntimeConfig {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        use crate::types::map::MapIterator;
        use crate::Error;

        let mut config = RuntimeConfig::default();

        // Try to decode as a map/keyword list
        let map_iter = MapIterator::new(term).ok_or(Error::BadArg)?;

        for (key, value) in map_iter {
            let key_str: String = key.decode()?;

            match key_str.as_str() {
                "worker_threads" => {
                    config.worker_threads = Some(value.decode()?);
                }
                "thread_name" => {
                    config.thread_name = Some(value.decode()?);
                }
                "thread_stack_size" => {
                    config.thread_stack_size = Some(value.decode()?);
                }
                _ => {
                    // Ignore unknown options for forward compatibility
                }
            }
        }

        Ok(config)
    }
}

/// Configure the global Tokio runtime from Elixir load_data.
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
///     // Try to decode runtime config from load_info
///     if let Ok(config) = load_info.decode::<rustler::tokio::RuntimeConfig>() {
///         rustler::tokio::configure(config)
///             .expect("Failed to configure Tokio runtime");
///     }
///     true
/// }
/// ```
///
/// In your Elixir config:
///
/// ```elixir
/// # config/config.exs
/// config :my_app, MyNif,
///   load_data: [
///     worker_threads: 4,
///     thread_name: "my-runtime"
///   ]
/// ```
pub fn configure(config: RuntimeConfig) -> Result<(), ConfigError> {
    let mut builder = tokio::runtime::Builder::new_multi_thread();
    builder.enable_all();

    // Apply configuration
    if let Some(threads) = config.worker_threads {
        builder.worker_threads(threads);
    }

    if let Some(name) = config.thread_name {
        builder.thread_name(name);
    }

    if let Some(stack_size) = config.thread_stack_size {
        builder.thread_stack_size(stack_size);
    }

    let runtime = builder.build().map_err(ConfigError::BuildFailed)?;

    TOKIO_RUNTIME
        .set(Arc::new(runtime))
        .map_err(|_| ConfigError::AlreadyInitialized)
}

/// Configure the global Tokio runtime programmatically.
///
/// This provides direct access to the Tokio Builder API for advanced use cases.
/// For most applications, prefer `configure_runtime_from_term` which allows
/// configuration from Elixir.
///
/// # Example
///
/// ```ignore
/// use rustler::{Env, Term};
///
/// fn load(_env: Env, _: Term) -> bool {
///     rustler::tokio::configure_runtime(|builder| {
///         builder
///             .worker_threads(4)
///             .thread_name("myapp-tokio")
///             .thread_stack_size(3 * 1024 * 1024);
///     }).expect("Failed to configure Tokio runtime");
///
///     true
/// }
/// ```
pub fn configure_runtime<F>(config_fn: F) -> Result<(), ConfigError>
where
    F: FnOnce(&mut tokio::runtime::Builder),
{
    let mut builder = tokio::runtime::Builder::new_multi_thread();
    builder.enable_all();

    // Allow user to customize
    config_fn(&mut builder);

    let runtime = builder.build().map_err(ConfigError::BuildFailed)?;

    TOKIO_RUNTIME
        .set(Arc::new(runtime))
        .map_err(|_| ConfigError::AlreadyInitialized)
}

/// Get a handle to the global tokio runtime, or the current runtime if already inside one.
pub fn runtime_handle() -> tokio::runtime::Handle {
    // Try to get the current runtime handle first (if already in a tokio context)
    tokio::runtime::Handle::try_current().unwrap_or_else(|_| {
        // Get or initialize with default configuration
        TOKIO_RUNTIME
            .get_or_init(|| {
                Arc::new(
                    tokio::runtime::Builder::new_multi_thread()
                        .enable_all()
                        .thread_name("rustler-tokio")
                        .build()
                        .expect("Failed to create default tokio runtime for async NIFs"),
                )
            })
            .handle()
            .clone()
    })
}
