use rustler::{Decoder, Encoder, Env, Error, NifResult, Term};

use num_bigint::Sign;

// From https://www.erlang.org/doc/apps/erts/erl_ext_dist.html
const EXTERNAL_TERM_FORMAT_VERSION: u8 = 131;
const SMALL_INTEGER: u8 = 97;
const INTEGER: u8 = 98;
const SMALL_BIG_EXT: u8 = 110;
const LARGE_BIG_EXT: u8 = 111;

rustler::atoms! {
    big_int_encoder_invalid_bytes
}

/// Wrapper around num-bigint that implements [Decoder](rustler::Decoder) and [Encoder](rustler::Encoder) traits
///
/// ```rust
/// use rustler_bigint::{BigInt, num_bigint};
///
/// // convert from and to the bigint wrapper
/// let number = num_bigint::BigInt::from(12);
/// let implements_decode_encode = BigInt::from(number.clone());
/// let bigint = num_bigint::BigInt::from(implements_decode_encode);
/// assert_eq!(number, bigint);
/// ```
///
/// ## Examples
///
/// ```rust
/// use rustler_bigint::BigInt;
///
/// #[rustler::nif]
/// pub fn pow(base: BigInt, exponent: u32) -> BigInt {
///   base.pow(exponent).into()
/// }
/// ```
///
/// ```rust
/// use rustler::Binary;
/// use rustler_bigint::{BigInt, num_bigint};
///
/// #[rustler::nif]
/// pub fn binary_to_integer(binary: Binary) -> BigInt {
///   num_bigint::BigInt::from_signed_bytes_be(binary.as_slice()).into()
/// }
/// ```
///
/// ```rust
/// use rustler::{Binary, Env, NewBinary};
/// use rustler_bigint::BigInt;
///
/// #[rustler::nif]
/// pub fn integer_to_binary<'a>(env: Env<'a>, integer: BigInt) -> Binary<'a> {
///   let bytes = integer.to_signed_bytes_be();
///   let mut output = NewBinary::new(env, bytes.len());
///   output.as_mut_slice().copy_from_slice(bytes.as_slice());
///   output.into()
/// }
/// ```
///
#[derive(Debug, PartialEq, PartialOrd, Eq)]
pub struct BigInt(num_bigint::BigInt);

impl std::convert::From<num_bigint::BigInt> for BigInt {
    fn from(big_int: num_bigint::BigInt) -> BigInt {
        BigInt(big_int)
    }
}

impl std::convert::From<BigInt> for num_bigint::BigInt {
    fn from(big_int: BigInt) -> num_bigint::BigInt {
        big_int.0
    }
}

impl std::ops::Deref for BigInt {
    type Target = num_bigint::BigInt;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for BigInt {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

fn decode_big_integer(input: &[u8]) -> NifResult<num_bigint::BigInt> {
    if Some(&EXTERNAL_TERM_FORMAT_VERSION) != input.first() {
        return Err(Error::BadArg);
    }

    match input[1] {
        SMALL_INTEGER => Ok(num_bigint::BigInt::from(input[2])),

        INTEGER => Ok(num_bigint::BigInt::from_signed_bytes_be(&input[2..6])),

        SMALL_BIG_EXT => {
            let n = input[2] as usize;
            let sign = if input[3] == 0 {
                Sign::Plus
            } else {
                Sign::Minus
            };

            Ok(num_bigint::BigInt::from_bytes_le(sign, &input[4..n + 4]))
        }

        LARGE_BIG_EXT => {
            let n = u32::from_be_bytes([input[2], input[3], input[4], input[5]]) as usize;
            let sign = if input[6] == 0 {
                Sign::Plus
            } else {
                Sign::Minus
            };

            Ok(num_bigint::BigInt::from_bytes_le(sign, &input[7..n + 7]))
        }

        _ => Err(Error::BadArg),
    }
}

fn encode_big_integer(big_int: &num_bigint::BigInt) -> Vec<u8> {
    if let Ok(integer) = i32::try_from(big_int) {
        let mut out = vec![EXTERNAL_TERM_FORMAT_VERSION, INTEGER];
        out.extend(integer.to_be_bytes());
        out
    } else {
        let (sign, data) = big_int.to_bytes_le();
        let sign = if sign == Sign::Minus { 1 } else { 0 };

        let mut out = vec![EXTERNAL_TERM_FORMAT_VERSION];
        if data.len() < 256 {
            // small big integer
            let n = data.len() as u8;
            out.push(SMALL_BIG_EXT);
            out.push(n);
        } else {
            // large big integer
            let n = (data.len() as u32).to_be_bytes();
            out.push(LARGE_BIG_EXT);
            out.extend(n);
        };
        out.push(sign);
        out.extend(data);

        out
    }
}

impl<'a> Decoder<'a> for BigInt {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        decode_big_integer(term.to_binary().as_slice()).map(BigInt)
    }
}

impl Encoder for BigInt {
    fn encode<'c>(&self, env: Env<'c>) -> Term<'c> {
        // Returns error tuple if the encode_big_integer returns invalid ETF bytes
        let binary = encode_big_integer(&self.0);
        match env.binary_to_term(&binary) {
            Some((term, _)) => term,
            None => env.error_tuple(big_int_encoder_invalid_bytes()),
        }
    }
}

#[test]
fn from_test() {
    let right = BigInt::from(num_bigint::BigInt::from(123));
    let left = BigInt(num_bigint::BigInt::from(123));
    assert_eq!(left, right)
}

#[test]
fn into_test() {
    let right = BigInt(num_bigint::BigInt::from(123));
    let left = num_bigint::BigInt::from(123);
    assert_eq!(left, right.into())
}

#[test]
fn deref_test() {
    let input = BigInt(num_bigint::BigInt::from(123));
    assert_eq!(vec![123], input.to_signed_bytes_be())
}

#[test]
fn deref_mut_test() {
    let expected = BigInt(num_bigint::BigInt::from(127));
    let mut input = BigInt(num_bigint::BigInt::from(123));
    input.set_bit(2, true);

    assert_eq!(expected, input)
}

#[test]
fn decode_small_int() {
    // :erlang.term_to_binary(3)
    let data = [131, 97, 3];

    let expected = num_bigint::BigInt::from(3);

    assert_eq!(expected, decode_big_integer(&data).unwrap());
}

#[test]
fn decode_small_negative_int() {
    // :erlang.term_to_binary(-5)
    let data = [131, 98, 255, 255, 255, 251];

    let expected = num_bigint::BigInt::from(-5);

    assert_eq!(expected, decode_big_integer(&data).unwrap());
}

#[test]
fn decode_normal_int() {
    // :erlang.term_to_binary(12345)
    let data = [131, 98, 0, 0, 48, 57];

    let expected = num_bigint::BigInt::from(12345);

    assert_eq!(expected, decode_big_integer(&data).unwrap());
}

#[test]
fn decode_small_big_int() {
    // :erlang.term_to_binary(24**120)
    let data = [
        131, 110, 69, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 97, 68, 238, 176, 235, 252,
        240, 96, 176, 91, 142, 219, 66, 30, 177, 137, 199, 21, 191, 153, 182, 169, 73, 73,
    ];

    let expected = num_bigint::BigInt::from(24).pow(120);

    assert_eq!(expected, decode_big_integer(&data).unwrap());
}

#[test]
fn decode_negative_small_big_int() {
    // :erlang.term_to_binary(-17**121)
    let data = [
        131, 110, 62, 1, 145, 35, 189, 92, 121, 166, 54, 134, 249, 187, 212, 168, 99, 15, 199, 18,
        199, 144, 202, 125, 24, 74, 178, 7, 108, 123, 248, 46, 241, 16, 48, 69, 1, 158, 117, 115,
        239, 213, 166, 221, 100, 3, 60, 67, 137, 0, 113, 26, 119, 247, 227, 213, 91, 112, 34, 59,
        186, 211, 12, 168, 222, 95,
    ];

    let expected = num_bigint::BigInt::from(-17).pow(121);

    assert_eq!(expected, decode_big_integer(&data).unwrap());
}

#[test]
fn decode_large_big_int() {
    // :erlang.term_to_binary(5**923)
    let data = [
        131, 111, 0, 0, 1, 12, 0, 157, 49, 35, 72, 82, 113, 71, 89, 143, 96, 245, 181, 222, 60,
        192, 44, 126, 212, 89, 42, 70, 17, 126, 228, 141, 27, 155, 90, 226, 45, 64, 172, 156, 24,
        197, 15, 30, 187, 15, 9, 134, 118, 177, 56, 98, 1, 252, 238, 113, 38, 174, 187, 181, 161,
        128, 55, 122, 131, 27, 44, 224, 26, 96, 20, 60, 68, 84, 188, 139, 50, 27, 35, 46, 111, 179,
        103, 3, 199, 16, 76, 202, 27, 108, 124, 189, 232, 63, 190, 120, 107, 231, 233, 67, 46, 164,
        146, 45, 0, 180, 109, 22, 20, 201, 153, 9, 199, 7, 75, 91, 29, 48, 143, 191, 229, 182, 108,
        161, 177, 245, 218, 54, 70, 12, 89, 196, 140, 138, 73, 115, 163, 249, 101, 13, 116, 117,
        122, 72, 14, 71, 112, 6, 243, 124, 189, 51, 36, 199, 143, 80, 129, 8, 175, 54, 95, 104, 6,
        40, 35, 244, 36, 219, 112, 129, 114, 96, 207, 37, 61, 113, 250, 82, 185, 101, 176, 68, 250,
        128, 104, 151, 141, 54, 167, 30, 231, 244, 173, 120, 162, 209, 54, 193, 170, 19, 160, 192,
        6, 233, 134, 70, 73, 142, 77, 174, 157, 177, 249, 210, 65, 164, 1, 27, 205, 204, 128, 251,
        195, 234, 183, 185, 159, 191, 32, 252, 61, 28, 157, 38, 61, 130, 198, 248, 203, 196, 150,
        186, 42, 43, 70, 136, 200, 78, 114, 229, 221, 198, 252, 187, 38, 162, 107, 52, 143, 177,
        133, 185, 168, 30, 64, 0, 151, 20, 186, 82, 147, 226, 1, 2, 141,
    ];

    let expected = num_bigint::BigInt::from(5).pow(923);

    assert_eq!(expected, decode_big_integer(&data).unwrap());
}

#[test]
fn decode_negative_large_big_int() {
    // :erlang.term_to_binary(-17**613)
    let data = [
        131, 111, 0, 0, 1, 58, 1, 81, 128, 217, 64, 63, 89, 203, 140, 179, 117, 112, 210, 237, 68,
        55, 214, 150, 63, 212, 77, 220, 75, 154, 120, 104, 18, 185, 178, 140, 7, 210, 84, 65, 183,
        69, 128, 159, 165, 220, 21, 229, 65, 80, 122, 240, 162, 174, 250, 62, 222, 203, 58, 15,
        215, 204, 106, 9, 213, 233, 208, 173, 124, 143, 108, 209, 19, 246, 224, 143, 58, 151, 72,
        57, 203, 232, 159, 92, 71, 8, 128, 138, 187, 209, 239, 209, 189, 91, 50, 55, 3, 226, 214,
        68, 115, 187, 151, 51, 139, 199, 133, 72, 135, 158, 46, 43, 168, 141, 235, 151, 168, 127,
        2, 17, 73, 195, 85, 131, 34, 150, 45, 210, 55, 18, 192, 113, 207, 229, 131, 213, 56, 48,
        60, 112, 76, 231, 109, 95, 46, 73, 56, 133, 2, 78, 96, 124, 119, 255, 5, 70, 120, 18, 146,
        6, 102, 190, 157, 134, 72, 125, 98, 88, 182, 38, 21, 13, 165, 135, 45, 143, 43, 121, 101,
        213, 130, 203, 180, 216, 230, 52, 163, 168, 125, 130, 100, 246, 179, 75, 211, 165, 66, 252,
        232, 223, 119, 44, 105, 62, 1, 100, 61, 132, 185, 114, 75, 234, 116, 186, 208, 227, 77, 81,
        18, 76, 125, 131, 116, 70, 180, 112, 76, 46, 36, 44, 243, 131, 190, 143, 166, 93, 72, 55,
        1, 92, 19, 242, 248, 90, 209, 160, 223, 191, 15, 167, 89, 34, 45, 252, 219, 235, 190, 232,
        32, 144, 232, 82, 173, 238, 213, 14, 157, 168, 162, 122, 24, 114, 192, 77, 18, 167, 147,
        136, 239, 98, 85, 107, 86, 161, 156, 221, 20, 86, 1, 183, 233, 166, 236, 96, 181, 90, 197,
        147, 189, 201, 71, 78, 206, 232, 115, 109, 222, 150, 213, 195, 164, 66, 123, 47, 151, 111,
        193, 165, 1, 16, 3,
    ];

    let expected = num_bigint::BigInt::from(-17).pow(613);

    assert_eq!(expected, decode_big_integer(&data).unwrap());
}

#[test]
fn encode_positive_int_as_integer() {
    let expected = vec![131, 98, 0, 0, 0, 12];

    let input = num_bigint::BigInt::from(12);

    assert_eq!(expected, encode_big_integer(&input));
}

#[test]
fn encode_negative_int_as_integer() {
    let expected = vec![131, 98, 255, 255, 254, 254];

    let input = num_bigint::BigInt::from(-258);

    assert_eq!(expected, encode_big_integer(&input));
}

#[test]
fn encode_negative_int_just_outside_32_bites_as_small_big_int() {
    let expected = vec![131, 110, 4, 1, 1, 0, 0, 128];

    let input = num_bigint::BigInt::from(i32::MIN as i64 - 1);

    assert_eq!(expected, encode_big_integer(&input));
}

#[test]
fn encode_small_big_int() {
    // :erlang.term_to_binary(24**120)
    let expected = vec![
        131, 110, 69, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 97, 68, 238, 176, 235, 252,
        240, 96, 176, 91, 142, 219, 66, 30, 177, 137, 199, 21, 191, 153, 182, 169, 73, 73,
    ];

    let input = num_bigint::BigInt::from(24).pow(120);

    assert_eq!(expected, encode_big_integer(&input));
}

#[test]
fn encode_negative_small_big_int() {
    // :erlang.term_to_binary(-17**121)
    let expected = vec![
        131, 110, 62, 1, 145, 35, 189, 92, 121, 166, 54, 134, 249, 187, 212, 168, 99, 15, 199, 18,
        199, 144, 202, 125, 24, 74, 178, 7, 108, 123, 248, 46, 241, 16, 48, 69, 1, 158, 117, 115,
        239, 213, 166, 221, 100, 3, 60, 67, 137, 0, 113, 26, 119, 247, 227, 213, 91, 112, 34, 59,
        186, 211, 12, 168, 222, 95,
    ];

    let input = num_bigint::BigInt::from(-17).pow(121);

    assert_eq!(expected, encode_big_integer(&input));
}

#[test]
fn encode_large_big_int() {
    // :erlang.term_to_binary(5**923)
    let expected = vec![
        131, 111, 0, 0, 1, 12, 0, 157, 49, 35, 72, 82, 113, 71, 89, 143, 96, 245, 181, 222, 60,
        192, 44, 126, 212, 89, 42, 70, 17, 126, 228, 141, 27, 155, 90, 226, 45, 64, 172, 156, 24,
        197, 15, 30, 187, 15, 9, 134, 118, 177, 56, 98, 1, 252, 238, 113, 38, 174, 187, 181, 161,
        128, 55, 122, 131, 27, 44, 224, 26, 96, 20, 60, 68, 84, 188, 139, 50, 27, 35, 46, 111, 179,
        103, 3, 199, 16, 76, 202, 27, 108, 124, 189, 232, 63, 190, 120, 107, 231, 233, 67, 46, 164,
        146, 45, 0, 180, 109, 22, 20, 201, 153, 9, 199, 7, 75, 91, 29, 48, 143, 191, 229, 182, 108,
        161, 177, 245, 218, 54, 70, 12, 89, 196, 140, 138, 73, 115, 163, 249, 101, 13, 116, 117,
        122, 72, 14, 71, 112, 6, 243, 124, 189, 51, 36, 199, 143, 80, 129, 8, 175, 54, 95, 104, 6,
        40, 35, 244, 36, 219, 112, 129, 114, 96, 207, 37, 61, 113, 250, 82, 185, 101, 176, 68, 250,
        128, 104, 151, 141, 54, 167, 30, 231, 244, 173, 120, 162, 209, 54, 193, 170, 19, 160, 192,
        6, 233, 134, 70, 73, 142, 77, 174, 157, 177, 249, 210, 65, 164, 1, 27, 205, 204, 128, 251,
        195, 234, 183, 185, 159, 191, 32, 252, 61, 28, 157, 38, 61, 130, 198, 248, 203, 196, 150,
        186, 42, 43, 70, 136, 200, 78, 114, 229, 221, 198, 252, 187, 38, 162, 107, 52, 143, 177,
        133, 185, 168, 30, 64, 0, 151, 20, 186, 82, 147, 226, 1, 2, 141,
    ];

    let input = num_bigint::BigInt::from(5).pow(923);

    assert_eq!(expected, encode_big_integer(&input));
}

#[test]
fn encode_negative_large_big_int() {
    // :erlang.term_to_binary(-17**613)
    let expected = vec![
        131, 111, 0, 0, 1, 58, 1, 81, 128, 217, 64, 63, 89, 203, 140, 179, 117, 112, 210, 237, 68,
        55, 214, 150, 63, 212, 77, 220, 75, 154, 120, 104, 18, 185, 178, 140, 7, 210, 84, 65, 183,
        69, 128, 159, 165, 220, 21, 229, 65, 80, 122, 240, 162, 174, 250, 62, 222, 203, 58, 15,
        215, 204, 106, 9, 213, 233, 208, 173, 124, 143, 108, 209, 19, 246, 224, 143, 58, 151, 72,
        57, 203, 232, 159, 92, 71, 8, 128, 138, 187, 209, 239, 209, 189, 91, 50, 55, 3, 226, 214,
        68, 115, 187, 151, 51, 139, 199, 133, 72, 135, 158, 46, 43, 168, 141, 235, 151, 168, 127,
        2, 17, 73, 195, 85, 131, 34, 150, 45, 210, 55, 18, 192, 113, 207, 229, 131, 213, 56, 48,
        60, 112, 76, 231, 109, 95, 46, 73, 56, 133, 2, 78, 96, 124, 119, 255, 5, 70, 120, 18, 146,
        6, 102, 190, 157, 134, 72, 125, 98, 88, 182, 38, 21, 13, 165, 135, 45, 143, 43, 121, 101,
        213, 130, 203, 180, 216, 230, 52, 163, 168, 125, 130, 100, 246, 179, 75, 211, 165, 66, 252,
        232, 223, 119, 44, 105, 62, 1, 100, 61, 132, 185, 114, 75, 234, 116, 186, 208, 227, 77, 81,
        18, 76, 125, 131, 116, 70, 180, 112, 76, 46, 36, 44, 243, 131, 190, 143, 166, 93, 72, 55,
        1, 92, 19, 242, 248, 90, 209, 160, 223, 191, 15, 167, 89, 34, 45, 252, 219, 235, 190, 232,
        32, 144, 232, 82, 173, 238, 213, 14, 157, 168, 162, 122, 24, 114, 192, 77, 18, 167, 147,
        136, 239, 98, 85, 107, 86, 161, 156, 221, 20, 86, 1, 183, 233, 166, 236, 96, 181, 90, 197,
        147, 189, 201, 71, 78, 206, 232, 115, 109, 222, 150, 213, 195, 164, 66, 123, 47, 151, 111,
        193, 165, 1, 16, 3,
    ];

    let input = num_bigint::BigInt::from(-17).pow(613);

    assert_eq!(expected, encode_big_integer(&input));
}
