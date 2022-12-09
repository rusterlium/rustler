use super::atom;
use crate::{Decoder, Encoder, Env, Error, NifResult, Term};

use std::ops::{Deref, DerefMut};

/// A wrapper type for [`Option<T>`][option] to provide Erlang style encoding. It
/// uses `undefined` atom instead of `nil` when the enclosing value is `None`.
///
/// Useful for interacting with Erlang libraries as `undefined` is commonly used in
/// Erlang to represent the absence of a value.
///
/// [option]: https://doc.rust-lang.org/stable/core/option/enum.Option.html
///
/// # Examples
///
/// `ErlOption<T>` provides methods to convert to/from `Option<T>`.
///
/// ```rust
/// use rustler::ErlOption;
///
/// // Create new `ErlOption<i32>` values via convenient functions.
/// let _ = ErlOption::some(1); // Wraps `Some(1)`.
/// let _ = ErlOption::<i32>::none();
///
/// // Convert Option<i32> values to ErlOption<i32> values.
/// let _ = ErlOption::from(Some(2));
/// let _: ErlOption<_> = Some(3).into();
/// let _: ErlOption<i32> = None.into();
///
/// // Get a reference of enclosing Option<T> from an ErlOption<T>.
/// let _: &Option<i32> = ErlOption::some(4).as_ref();
///
/// // Get a mutable reference of enclosing Option<T> from an ErlOption<T>.
/// let _: &mut Option<i32> = ErlOption::some(5).as_mut();
///
/// // Convert an ErlOption<i32> value to an Option<i32> value.
/// let _: Option<i32> = ErlOption::some(6).into();
///
/// // Compare ErlOption<T> with Option<T>.
/// assert_eq!(ErlOption::some(7), Some(7));
/// assert!(ErlOption::some(8) > Some(7));
///
/// // Call Option<T>'s methods on an ErlOption<T> via Deref and DerefMut.
/// assert!(ErlOption::some(9).is_some());
/// assert_eq!(ErlOption::some(10).unwrap(), 10);
/// assert_eq!(ErlOption::some(12).map(|v| v + 1), ErlOption::some(13));
/// ```
///
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ErlOption<T>(Option<T>);

impl<T> ErlOption<T> {
    /// A convenience function to create an `ErlOption<T>` from a `Some(T)` value.
    pub fn some(v: T) -> Self {
        Some(v).into()
    }

    /// A convenience function to create an `ErlOption<T>` enclosing the `None`
    /// value.
    pub fn none() -> Self {
        Default::default()
    }
}

// NOTE: Manually implement the Default instead of deriving it. This is because
// deriving requires `T` to be `Default` as well, but we do not need that.
impl<T> Default for ErlOption<T> {
    fn default() -> Self {
        Self(None)
    }
}

impl<T> From<Option<T>> for ErlOption<T> {
    fn from(v: Option<T>) -> Self {
        ErlOption(v)
    }
}

impl<T> From<ErlOption<T>> for Option<T> {
    fn from(v: ErlOption<T>) -> Self {
        v.0
    }
}

impl<T> AsMut<Option<T>> for ErlOption<T> {
    fn as_mut(&mut self) -> &mut Option<T> {
        &mut self.0
    }
}

impl<T> AsRef<Option<T>> for ErlOption<T> {
    fn as_ref(&self) -> &Option<T> {
        &self.0
    }
}

impl<T> Deref for ErlOption<T> {
    type Target = Option<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for ErlOption<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> PartialEq<Option<T>> for ErlOption<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Option<T>) -> bool {
        &self.0 == other
    }
}

impl<T> PartialEq<ErlOption<T>> for Option<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &ErlOption<T>) -> bool {
        self == &other.0
    }
}

impl<T> PartialOrd<Option<T>> for ErlOption<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Option<T>) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(other)
    }
}

impl<T> PartialOrd<ErlOption<T>> for Option<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &ErlOption<T>) -> Option<std::cmp::Ordering> {
        self.partial_cmp(&other.0)
    }
}

impl<T> Encoder for ErlOption<T>
where
    T: Encoder,
{
    fn encode<'c>(&self, env: Env<'c>) -> Term<'c> {
        match self.0 {
            Some(ref value) => value.encode(env),
            None => atom::undefined().encode(env),
        }
    }
}

impl<'a, T> Decoder<'a> for ErlOption<T>
where
    T: Decoder<'a>,
{
    fn decode(term: Term<'a>) -> NifResult<Self> {
        if let Ok(term) = term.decode::<T>() {
            Ok(Self(Some(term)))
        } else {
            let decoded_atom: atom::Atom = term.decode()?;
            if decoded_atom == atom::undefined() {
                Ok(Self(None))
            } else {
                Err(Error::BadArg)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::ErlOption;

    #[test]
    fn test_creations() {
        assert_eq!(ErlOption::some(1).as_ref(), &Some(1));
        assert_eq!(ErlOption::<i32>::none().as_ref(), &None as &Option<i32>);
    }

    #[test]
    fn test_conversions() {
        // Convert Option<i32> values to ErlOption<i32> values.
        assert_eq!(ErlOption::from(Some(2)), ErlOption::some(2));
        assert_eq!(Into::<ErlOption<i32>>::into(Some(3)), ErlOption::some(3));
        assert_eq!(Into::<ErlOption<i32>>::into(None), ErlOption::none());

        // Convert an ErlOption<i32> value to an Option<i32> value.
        assert_eq!(Into::<Option<i32>>::into(ErlOption::some(6)), Some(6));
    }

    #[test]
    fn test_as_ref() {
        assert_eq!(ErlOption::some(4).as_ref(), &Some(4));
        assert_eq!(ErlOption::some(5).as_mut(), &mut Some(5));
    }

    #[test]
    fn test_compare() {
        assert_eq!(ErlOption::some(7), Some(7));
        assert!(ErlOption::some(8) > Some(7));
    }

    #[test]
    fn test_deref() {
        assert!(ErlOption::some(9).is_some());
        assert_eq!(ErlOption::some(10).unwrap(), 10);
        assert_eq!(ErlOption::some(12).map(|v| v + 1), ErlOption::some(13));
    }
}
