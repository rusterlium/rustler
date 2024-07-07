//! Support for storing Rust data in Erlang terms.
//!
//! A NIF resource allows you to safely store Rust structs in a term, and therefore keep it across
//! NIF calls. The struct will be automatically dropped when the BEAM GC decides that there are no
//! more references to the resource.

mod arc;
mod error;
mod monitor;
mod registration;
mod term;
mod traits;
mod util;

pub use arc::ResourceArc;
pub use error::ResourceInitError;
pub use monitor::Monitor;
pub use traits::Resource;
use traits::ResourceExt;

#[macro_export]
macro_rules! resource {
    ($struct_name:ty, $env: ident) => {{
        impl $crate::Resource for $struct_name {}
        $env.register::<$struct_name>().is_ok()
    }};
}
