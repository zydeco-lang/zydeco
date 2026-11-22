//! OS/WASI random entropy source for the Zydeco runtime without `rand` crate overhead.

use std::io;
use zydeco_machine::word::RuntimeWord;

/// Read 8 uniform random bytes from the host OS or WASI environment.
pub fn random_bytes() -> io::Result<[u8; 8]> {
    let mut bytes = [0_u8; 8];
    fill_random_bytes(&mut bytes)?;
    Ok(bytes)
}

/// Produce a random signed immediate in [`RuntimeWord::SIGNED_MIN`..=`RuntimeWord::SIGNED_MAX`].
pub fn random_signed_int() -> io::Result<i64> {
    let bytes = random_bytes()?;
    Ok(RuntimeWord::wrap_signed(i64::from_ne_bytes(bytes)))
}

#[cfg(target_os = "wasi")]
fn fill_random_bytes(bytes: &mut [u8]) -> io::Result<()> {
    #[link(wasm_import_module = "wasi_snapshot_preview1")]
    unsafe extern "C" {
        fn random_get(buf: *mut u8, buf_len: usize) -> u32;
    }
    let errno = unsafe { random_get(bytes.as_mut_ptr(), bytes.len()) };
    if errno == 0 { Ok(()) } else { Err(io::Error::from_raw_os_error(errno as i32)) }
}

#[cfg(not(target_os = "wasi"))]
fn fill_random_bytes(bytes: &mut [u8]) -> io::Result<()> {
    use std::io::Read;
    let mut urandom = std::fs::File::open("/dev/urandom")?;
    urandom.read_exact(bytes)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generated_integers_fit_tagged_signed_word_range() {
        for _ in 0..32 {
            let value = random_signed_int().unwrap();
            assert!((RuntimeWord::SIGNED_MIN..=RuntimeWord::SIGNED_MAX).contains(&value));
            assert!(RuntimeWord::signed(value).is_some());
        }
    }
}
