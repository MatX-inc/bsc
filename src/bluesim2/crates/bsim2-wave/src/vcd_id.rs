//! VCD identifier codes — the base-94 scheme from `vcd.cxx:285-298`
//! (printable ASCII `'!'` (33) through `'~'` (126), most significant digit
//! first), kept byte-compatible so VCD output can be diffed against the
//! C++ Bluesim during migration.

/// Encode a numeric signal id as a VCD identifier code.
pub fn encode(mut num: u32) -> String {
    let mut buf = Vec::new();
    loop {
        buf.push(b'!' + (num % 94) as u8);
        num /= 94;
        if num == 0 {
            break;
        }
    }
    buf.reverse(); // C++ fills the buffer back-to-front (vcd.cxx:290-295)
    String::from_utf8(buf).expect("printable ASCII")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn small_ids_single_char() {
        assert_eq!(encode(0), "!");
        assert_eq!(encode(1), "\"");
        assert_eq!(encode(93), "~");
    }

    #[test]
    fn multi_char_most_significant_first() {
        // 94 = 1*94 + 0 -> digits [1,0] -> "\"!"
        assert_eq!(encode(94), "\"!");
        // 94*94 = 1*94^2 + 0*94 + 0 -> "\"!!"
        assert_eq!(encode(94 * 94), "\"!!");
        // 95 = 1*94 + 1 -> "\"\""
        assert_eq!(encode(95), "\"\"");
    }

    #[test]
    fn all_printable() {
        for n in [0u32, 5, 93, 94, 1000, 123_456] {
            assert!(encode(n).bytes().all(|b| (33..=126).contains(&b)));
        }
    }
}
