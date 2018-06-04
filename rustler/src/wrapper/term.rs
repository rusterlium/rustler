use std::fmt;

use std::os::raw::c_char;

use super::nif_interface::NIF_TERM;

pub fn fmt<'a>(term: NIF_TERM, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    const SIZE: usize = 1024;
    let mut bytes: Vec<u8> = Vec::with_capacity(SIZE);

    let mut n = 0;
    for _ in 0..10 {
        let i = unsafe {
            enif_snprintf!(
                bytes.as_mut_ptr() as *mut c_char,
                bytes.capacity(),
                b"%T\x00" as *const u8 as *const c_char,
                term
            )
        };
        if i < 0 {
            // Do not propagate an error, because string formatting is
            // supposed to be infallible.
            break;
        }

        n = i as usize;
        if n >= bytes.capacity() {
            // Bizarrely, enif_snprintf consistently underestimates the
            // amount of memory it will need to write long lists. To try to
            // avoid going around the loop again, double the estimate.
            bytes.reserve_exact(2 * n + 1);

            // Ensure that the `set_len` call below does not expose
            // uninitialized bytes if we give up after 10 attempts.
            n = 0;
        } else {
            break;
        }
    }

    unsafe {
        bytes.set_len(n);
    }
    f.write_str(&String::from_utf8_lossy(&bytes))
}
