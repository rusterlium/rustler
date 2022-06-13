//! [Github](https://github.com/rusterlium/rustler)
//!
//! [BigInt](crate::BigInt) is a helper type to support using large integers in Rust nifs
//!
//! BigInt type is a wrapper around [num-bigint](num_bigint::BigInt)
//! this crate is also exposed via `rustler_bigint::num_bigint`
//!
//! ## Example
//!
//! ```rust
//! use rustler::{NifResult, Error};
//! use rustler_bigint::BigInt;
//!
//! #[rustler::nif]
//! pub fn add(left: BigInt, right: BigInt) -> NifResult<BigInt> {
//!     // BigInt implements deref so we can just use num-bigint methods directly
//!     if let Some(result) = left.checked_add(&right) {
//!         Ok(result.into())
//!     } else {
//!         Err(Error::RaiseAtom("number_overflow"))
//!     }
//! }
//! ```

pub mod big_int;

pub use big_int::BigInt;
pub use num_bigint;
