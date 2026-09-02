//! Word-decomposed layout for values wider than a machine word.
//!
//! bsc lowers struct build/select to Concat/Extract, so a wide value is
//! assembled from pieces and taken apart into pieces and never computed
//! on.  Materialising it as a single iN makes LLVM legalise every
//! operation over it, which on one MatX design cost ~7x what the same
//! work costs on legal types (measured: a 5600-bit pack/select/unpack
//! function, 2.41s -> 0.35s once word-decomposed).
//!
//! Chunks are uniform machine words: element `i` of a decomposed value
//! is bits `[i*W, (i+1)*W)`.  Cutting instead at the value's own field
//! boundaries would make every field read a bare reference rather than a
//! shift, but it needs a per-value layout threaded through every
//! operator, and on the design measured only ~6% of reads straddle a
//! word anyway (costing one extra `or`).  Uniform words keep the
//! representation self-describing.
//!
//! Word size is the target's, not a constant: `n32:64` in the data
//! layout means 64 here and 32 on a 32-bit target.

/// The target's native integer width, from a data-layout string's `n`
/// spec (e.g. `n8:16:32:64` -> 64).  Falls back to the pointer size, then
/// to 64.
pub fn word_bits(data_layout: &str, pointer_bits: u32) -> u32 {
    for field in data_layout.split('-') {
        if let Some(rest) = field.strip_prefix('n') {
            let widths: Vec<u32> =
                rest.split(':').filter_map(|s| s.parse().ok()).collect();
            if let Some(&m) = widths.iter().max() {
                if m > 0 {
                    return m;
                }
            }
        }
    }
    if pointer_bits > 0 {
        pointer_bits
    } else {
        64
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn word_bits_reads_the_native_spec() {
        assert_eq!(word_bits("e-m:e-p270:32:32-i64:64-n8:16:32:64-S128", 64), 64);
        assert_eq!(word_bits("e-m:e-p:32:32-i64:64-n8:16:32-S128", 32), 32);
        // no n spec: fall back to the pointer size
        assert_eq!(word_bits("e-m:e-i64:64-S128", 32), 32);
    }
}
