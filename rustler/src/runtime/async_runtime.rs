use std::future::Future;
use std::pin::Pin;

/// Trait for pluggable async runtimes.
///
/// This allows users to provide their own async runtime implementation
/// instead of being locked into Tokio.
///
/// # Example
///
/// ```ignore
/// use rustler::runtime::AsyncRuntime;
/// use std::future::Future;
/// use std::pin::Pin;
///
/// struct MyRuntime;
///
/// impl AsyncRuntime for MyRuntime {
///     fn spawn(&self, future: Pin<Box<dyn Future<Output = ()> + Send + 'static>>) {
///         // Spawn on your custom runtime
///     }
/// }
/// ```
pub trait AsyncRuntime: Send + Sync + 'static {
    /// Spawn a future onto the runtime.
    ///
    /// The future should be executed to completion, and the runtime
    /// is responsible for driving it.
    fn spawn(&self, future: Pin<Box<dyn Future<Output = ()> + Send + 'static>>);
}
