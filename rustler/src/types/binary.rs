//! Safe wrappers around Erlang binaries.
//!
//! Rustler provides three binary types: [`Binary`], [`NewBinary`] and
//! [`OwnedBinary`]. All represent a contiguous region `u8`s, and they all use
//! the Erlang allocator. The primary difference between them is their ownership
//! semantics.
//!
//! The _owned_ in `OwnedBinary` refers to the fact that it owns the binary it
//! wraps. The _owner_ of an `OwnedBinary` is free to modify its contents. Ownership
//! lasts until it is dropped or consumed by converting it into a regular
//! `Binary`. An `OwnedBinary` cannot be copied or cloned and is thus always moved.
//!
//! The `Binary` type is an immutable shared-reference to a binary. `Binary`s are
//! cheap to copy: all copies of a `Binary` point to the original `Binary`'s
//! data. Additionally, a `Binary`'s lifetime is tied to that of the NIF's [`Env`],
//! preventing outstanding references to the data after a NIF returns.
//!
//! `NewBinary` is a way of creating a `Binary` without going via `OwnedBinary`.
//! This can improve performance, since `NewBinary`s can be allocated on the
//! heap if they are small. Unlike `OwnedBinary`, `NewBinary`s lifetime is tied
//! to that of the NIF's [`Env`]. `NewBinary` must be converted to a `Binary`
//! or directly to a `Term` before it can be passed to Erlang.
//!
//! # Examples
//!
//! Constructing an `OwnedBinary`:
//!
//! ```no_run
//! # use rustler::OwnedBinary;
//! {
//!     let mut bin = OwnedBinary::new(5).expect("allocation failed");
//!     bin.as_mut_slice().copy_from_slice("hello".as_bytes());
//! } // <- `bin` is dropped here
//! ```
//!
//! The following NIF takes a binary as its only parameter and returns a new binary
//! where each element is exclusive-or'ed with a constant:
//!
//! ```no_run
//! # use rustler::{Env, OwnedBinary, Binary, NifResult, Error};
//! #[rustler::nif]
//! fn xor_example<'a>(env: Env<'a>, bin: Binary<'a>) -> NifResult<Binary<'a>> {
//!     let mut owned: OwnedBinary = bin.to_owned().ok_or(Error::Term(Box::new("no mem")))?;
//!     for byte in owned.as_mut_slice() {
//!         *byte ^= 0xAA;
//!     }
//!
//!     // Ownership of `owned`'s data is transferred to `env` on the
//!     // following line, so no additional heap allocations are incurred.
//!     Ok(Binary::from_owned(owned, env))
//! }
//! ```
//!
//! The contents of a newly-allocated `OwnedBinary` is not initialized to any
//! particular value. If your usage of the binary requires the it's data to be
//! zeroed, for example, then you must explicit zero it. In this example, we
//! manually zeroize the binary before passing it as slice to a third party
//! function.
//!
//! ```no_run
//! # fn some_third_party_api(buf: &mut [u8]) {
//! #     for elem in buf {
//! #         if *elem == 0 { *elem = 1 } else { panic!("Not a zero!") }
//! #     }
//! # }
//! # use rustler::{Env, OwnedBinary, Binary, NifResult, Error};
//! #[rustler::nif]
//! fn wrapper_for_some_<'a>(env: Env<'a>) -> NifResult<Binary<'a>> {
//!     let mut owned = OwnedBinary::new(100).ok_or(Error::Term(Box::new("no mem")))?;
//!     for byte in owned.as_mut_slice() {
//!         *byte = 0;
//!     }
//!
//!     // Some third party API which requires the slice to be all zeros on entry.
//!     some_third_party_api(owned.as_mut_slice());
//!
//!     // The imaginary API call presumedly filled in our binary with meaningful
//!     // data, so let's return it.
//!     Ok(Binary::from_owned(owned, env))
//! }
//!
//! ```
//!
//! [`Binary`]: struct.Binary.html
//! [`Env`]: ../../env/struct.Env.html
//! [`OwnedBinary`]: struct.OwnedBinary.html

use crate::{
    sys::{
        enif_inspect_binary, enif_inspect_iolist_as_binary, enif_make_binary, enif_make_sub_binary,
        enif_release_binary,
    },
    wrapper::binary::{alloc, new_binary, realloc, ErlNifBinary},
    Decoder, Encoder, Env, Error, NifResult, Term,
};
use std::{
    borrow::{Borrow, BorrowMut},
    hash::{Hash, Hasher},
    io::Write,
    mem::MaybeUninit,
    ops::{Deref, DerefMut},
};

/// An mutable smart-pointer to an Erlang binary.
///
/// See [module-level doc](index.html) for more information.
pub struct OwnedBinary(ErlNifBinary);

impl OwnedBinary {
    pub unsafe fn from_raw(inner: ErlNifBinary) -> OwnedBinary {
        OwnedBinary(inner)
    }

    /// Allocates a new `OwnedBinary` with size `size`.
    ///
    /// Memory is not initialized. If uninitialized memory is undesirable, set it
    /// manually.
    ///
    /// # Errors
    ///
    /// If allocation fails, `None` is returned.
    pub fn new(size: usize) -> Option<OwnedBinary> {
        unsafe { alloc(size) }.map(OwnedBinary)
    }

    /// Copies `src`'s data into a new `OwnedBinary`.
    ///
    /// # Errors
    ///
    /// If allocation fails, `None` is returned.
    pub fn from_unowned(src: &Binary) -> Option<OwnedBinary> {
        OwnedBinary::new(src.len()).map(|mut b| {
            b.as_mut_slice().copy_from_slice(src);
            b
        })
    }

    /// Attempts to reallocate `self` with the new size.
    ///
    /// Memory outside the range of the original binary will not be initialized. If
    /// uninitialized memory is undesirable, set it manually.
    ///
    /// # Errors
    ///
    /// If reallocation fails, `false` is returned. Data remains intact on error.
    #[must_use]
    pub fn realloc(&mut self, size: usize) -> bool {
        unsafe { realloc(&mut self.0, size) }
    }

    /// Attempts to reallocate `self` with the new size.
    ///
    /// If reallocation fails, it will perform a copy instead.
    ///
    /// Memory outside the range of the original binary will not be initialized. If
    /// uninitialized memory is undesirable, set it manually.
    pub fn realloc_or_copy(&mut self, size: usize) {
        if !self.realloc(size) {
            let mut new = OwnedBinary::new(size).unwrap();
            if let Ok(num_written) = new.as_mut_slice().write(self.as_slice()) {
                if !(num_written == self.len() || num_written == new.len()) {
                    panic!("Could not copy binary");
                }
                ::std::mem::swap(&mut self.0, &mut new.0);
            } else {
                panic!("Could not copy binary");
            }
        }
    }

    /// Extracts a slice containing the entire binary.
    pub fn as_slice(&self) -> &[u8] {
        unsafe { ::std::slice::from_raw_parts(self.0.data, self.0.size) }
    }

    /// Extracts a mutable slice of the entire binary.
    pub fn as_mut_slice(&mut self) -> &mut [u8] {
        unsafe { ::std::slice::from_raw_parts_mut(self.0.data, self.0.size) }
    }

    /// Consumes `self` and returns an immutable `Binary`.
    ///
    /// This method is the mirror of [`Binary::from_owned`], and they can be used
    /// interchangeably.
    ///
    /// [`Binary::from_owned`]: struct.Binary.html#method.from_owned
    pub fn release(self, env: Env) -> Binary {
        Binary::from_owned(self, env)
    }
}

impl Borrow<[u8]> for OwnedBinary {
    fn borrow(&self) -> &[u8] {
        self.as_slice()
    }
}
impl BorrowMut<[u8]> for OwnedBinary {
    fn borrow_mut(&mut self) -> &mut [u8] {
        self.as_mut_slice()
    }
}
impl Deref for OwnedBinary {
    type Target = [u8];
    fn deref(&self) -> &[u8] {
        self.as_slice()
    }
}
impl DerefMut for OwnedBinary {
    fn deref_mut(&mut self) -> &mut [u8] {
        self.as_mut_slice()
    }
}
impl Hash for OwnedBinary {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_slice().hash(state);
    }
}
impl PartialEq for OwnedBinary {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}
impl Eq for OwnedBinary {}
impl PartialEq<Binary<'_>> for OwnedBinary {
    fn eq(&self, other: &Binary) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl Drop for OwnedBinary {
    fn drop(&mut self) {
        unsafe { enif_release_binary(&mut self.0) };
    }
}

unsafe impl Send for OwnedBinary {}
unsafe impl Sync for OwnedBinary {}

/// An immutable smart-pointer to an Erlang binary.
///
/// See [module-level doc](index.html) for more information.
#[derive(Copy, Clone)]
pub struct Binary<'a> {
    buf: *const u8,
    size: usize,
    term: Term<'a>,
}

impl<'a> Binary<'a> {
    /// Consumes `owned` and returns an immutable `Binary`.
    #[inline]
    pub fn from_owned(owned: OwnedBinary, env: Env<'a>) -> Self {
        // We are transferring ownership of `owned`'s data to the
        // environment. Therefore, we need to prevent `owned`'s destructor being
        // called at the end of this scope. The least error-prone solution (compared
        // to `mem::forget()`) is to wrap `owned` in a `ManuallyDrop` and EXPLICITLY
        // NOT CALL `ManuallyDrop::drop()`.
        let mut owned = std::mem::ManuallyDrop::new(owned);
        let term = unsafe { Term::new(env, enif_make_binary(env.as_c_arg(), &mut owned.0)) };
        Binary {
            buf: owned.0.data,
            size: owned.0.size,
            term,
        }
    }

    /// Copies `self`'s data into a new `OwnedBinary`.
    ///
    /// # Errors
    ///
    /// If allocation fails, an error will be returned.
    #[allow(clippy::wrong_self_convention)]
    #[inline]
    pub fn to_owned(&self) -> Option<OwnedBinary> {
        OwnedBinary::from_unowned(self)
    }

    /// Creates a `Binary` from `term`.
    ///
    /// # Errors
    ///
    /// If `term` is not a binary, an error will be returned.
    #[inline]
    pub fn from_term(term: Term<'a>) -> Result<Self, Error> {
        let mut binary = MaybeUninit::uninit();
        if unsafe {
            enif_inspect_binary(
                term.get_env().as_c_arg(),
                term.as_c_arg(),
                binary.as_mut_ptr(),
            )
        } == 0
        {
            return Err(Error::BadArg);
        }

        let binary = unsafe { binary.assume_init() };
        Ok(Binary {
            buf: binary.data,
            size: binary.size,
            term,
        })
    }

    /// Creates a Binary from a `term` and the associated slice
    ///
    /// The `term` *must* be constructed from the given slice, it is not checked.
    pub(crate) unsafe fn from_term_and_slice(term: Term<'a>, binary: &[u8]) -> Self {
        Binary {
            term,
            buf: binary.as_ptr(),
            size: binary.len(),
        }
    }

    /// Creates a `Binary` from `term`.
    ///
    /// # Errors
    ///
    /// If `term` is not an `iolist`, an error will be returned.
    #[inline]
    pub fn from_iolist(term: Term<'a>) -> Result<Self, Error> {
        let mut binary = MaybeUninit::uninit();
        if unsafe {
            enif_inspect_iolist_as_binary(
                term.get_env().as_c_arg(),
                term.as_c_arg(),
                binary.as_mut_ptr(),
            )
        } == 0
        {
            return Err(Error::BadArg);
        }

        let binary = unsafe { binary.assume_init() };
        Ok(Binary {
            buf: binary.data,
            size: binary.size,
            term,
        })
    }

    /// Returns an Erlang term representation of `self`.
    #[allow(clippy::wrong_self_convention)]
    #[inline]
    pub fn to_term<'b>(&self, env: Env<'b>) -> Term<'b> {
        self.term.in_env(env)
    }

    /// Extracts a slice containing the entire binary.
    #[inline]
    pub fn as_slice(&self) -> &'a [u8] {
        unsafe { ::std::slice::from_raw_parts(self.buf, self.size) }
    }

    /// Returns a new view into the same binary.
    ///
    /// This method is analogous to subslicing (e.g. `some_data[offset..length]`) in
    /// that it does not copy nor allocate data.
    ///
    /// # Errors
    ///
    /// If `offset + length` is out of bounds, an error will be returned.
    #[inline]
    pub fn make_subbinary(&self, offset: usize, length: usize) -> NifResult<Binary<'a>> {
        let min_len = length.checked_add(offset);
        if min_len.ok_or(Error::BadArg)? > self.size {
            return Err(Error::BadArg);
        }

        Ok(unsafe { self.make_subbinary_unchecked(offset, length) })
    }

    /// Returns a new view into the same binary.
    ///
    /// This method is an unsafe variant of `Binary::make_subbinary` in that it does not check for
    /// `offset + length < self.len()` and always returns a `Binary`.
    ///
    /// # Safety
    ///
    /// If `offset + length` is out of bounds, this call results in *undefined behavior*. The
    /// caller has to ensure that `offset + length < self.len()`.
    #[allow(unused_unsafe)]
    pub unsafe fn make_subbinary_unchecked(&self, offset: usize, length: usize) -> Binary<'a> {
        let raw_term = unsafe {
            enif_make_sub_binary(
                self.term.get_env().as_c_arg(),
                self.term.as_c_arg(),
                offset,
                length,
            )
        };
        let term = unsafe { Term::new(self.term.get_env(), raw_term) };

        Binary {
            buf: unsafe { self.buf.add(offset) },
            size: length,
            term,
        }
    }
}

impl Borrow<[u8]> for Binary<'_> {
    fn borrow(&self) -> &[u8] {
        self.as_slice()
    }
}
impl Deref for Binary<'_> {
    type Target = [u8];
    fn deref(&self) -> &[u8] {
        self.as_slice()
    }
}

impl<'a> Decoder<'a> for Binary<'a> {
    fn decode(term: Term<'a>) -> Result<Self, Error> {
        Binary::from_term(term)
    }
}
impl Encoder for Binary<'_> {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        self.to_term(env)
    }
}
impl Hash for Binary<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_slice().hash(state);
    }
}
impl PartialEq for Binary<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}
impl Eq for Binary<'_> {}
impl PartialEq<OwnedBinary> for Binary<'_> {
    fn eq(&self, other: &OwnedBinary) -> bool {
        self.as_slice() == other.as_slice()
    }
}

/// ## Binary terms
impl<'a> Term<'a> {
    pub fn into_binary(self) -> NifResult<Binary<'a>> {
        Binary::from_term(self)
    }
}

/// An newly-created, mutable Erlang binary.
///
/// See [module-level doc](index.html) for more information.
pub struct NewBinary<'a> {
    buf: *mut u8,
    size: usize,
    // safety: we must not expose `term` until it is no longer possible to get a
    // &mut ref to `buf`.
    term: Term<'a>,
}

impl<'a> NewBinary<'a> {
    /// Allocates a new `NewBinary`
    #[inline]
    pub fn new(env: Env<'a>, size: usize) -> Self {
        let (buf, term) = unsafe { new_binary(env, size) };
        NewBinary { buf, term, size }
    }
    /// Extracts a slice containing the entire binary.
    #[inline]
    pub fn as_slice(&self) -> &[u8] {
        unsafe { ::std::slice::from_raw_parts(self.buf, self.size) }
    }

    /// Extracts a mutable slice of the entire binary.
    #[inline]
    pub fn as_mut_slice(&mut self) -> &mut [u8] {
        unsafe { ::std::slice::from_raw_parts_mut(self.buf, self.size) }
    }
}

impl<'a> From<NewBinary<'a>> for Binary<'a> {
    #[inline]
    fn from(new_binary: NewBinary<'a>) -> Self {
        Binary::from_term(new_binary.term).unwrap()
    }
}

impl<'a> From<NewBinary<'a>> for Term<'a> {
    #[inline]
    fn from(new_binary: NewBinary<'a>) -> Self {
        new_binary.term
    }
}

impl Deref for NewBinary<'_> {
    type Target = [u8];
    #[inline]
    fn deref(&self) -> &[u8] {
        self.as_slice()
    }
}
impl DerefMut for NewBinary<'_> {
    #[inline]
    fn deref_mut(&mut self) -> &mut [u8] {
        self.as_mut_slice()
    }
}
