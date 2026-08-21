//! Bit-vector values.  Widths are explicit; all operations mask their
//! result to the target width.  Storage is 64-bit limbs, little-endian.
//! Correctness over speed — this is the oracle, not the product.

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Value {
    pub width: u32,
    limbs: Limbs,
}

/// Limb storage: values of one limb (width <= 64, plus the marker
/// widths — the overwhelming majority of every workload) live INLINE,
/// no heap.  Canonical invariant: len == 1 <=> S, so the derived
/// PartialEq is structural-and-correct.  Deref keeps every slice-shaped
/// use site (`[i]`, `.len()`, `.get()`, `.iter()`) unchanged.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Limbs {
    S([u64; 1]),
    W(Vec<u64>),
}

impl Limbs {
    #[inline]
    fn new(mut v: Vec<u64>) -> Limbs {
        if v.len() == 1 {
            Limbs::S([v[0]])
        } else {
            if v.is_empty() {
                v.push(0);
            }
            Limbs::W(v)
        }
    }
    #[inline]
    fn filled(n: usize, x: u64) -> Limbs {
        if n <= 1 {
            Limbs::S([x])
        } else {
            Limbs::W(vec![x; n])
        }
    }
    /// resize to exactly n limbs (zero-fill), keeping the canonical
    /// small form for n == 1
    fn resize_to(&mut self, n: usize) {
        match self {
            Limbs::S(a) if n <= 1 => {
                let _ = a;
            }
            Limbs::S(a) => {
                let mut v = vec![0u64; n];
                v[0] = a[0];
                *self = Limbs::W(v);
            }
            Limbs::W(v) if n <= 1 => {
                *self = Limbs::S([v.first().copied().unwrap_or(0)]);
            }
            Limbs::W(v) => v.resize(n, 0),
        }
    }
}

impl std::ops::Deref for Limbs {
    type Target = [u64];
    #[inline]
    fn deref(&self) -> &[u64] {
        match self {
            Limbs::S(a) => a,
            Limbs::W(v) => v,
        }
    }
}

impl std::ops::DerefMut for Limbs {
    #[inline]
    fn deref_mut(&mut self) -> &mut [u64] {
        match self {
            Limbs::S(a) => a,
            Limbs::W(v) => v,
        }
    }
}

/// Marker width for string-valued `Value`s (see `Value::str_ref`).
pub const STR_MARKER: u32 = u32::MAX;
pub const REAL_MARKER: u32 = u32::MAX - 1;

/// Reference Bluesim performs native integer division, so a zero divisor
/// kills the process with SIGFPE (bsc.misc/divmod expects exactly that);
/// reproduce the trap rather than inventing a result value.
fn raise_sigfpe() -> ! {
    extern "C" {
        fn raise(sig: std::ffi::c_int) -> std::ffi::c_int;
    }
    unsafe {
        raise(8 /* SIGFPE */);
    }
    // SIGFPE terminates by default; if the caller blocked it, mirror the
    // C++'s undefined-behavior death as best we can
    std::process::abort();
}

fn nlimbs(width: u32) -> usize {
    ((width as usize) + 63) / 64
}

impl Value {
    pub fn zero(width: u32) -> Value {
        Value { width, limbs: Limbs::filled(nlimbs(width).max(1), 0) }
    }

    /// The "undetermined" pattern for -unspecified-to A (0xAAAA...).
    pub fn undet(width: u32) -> Value {
        let mut v = Value { width, limbs: Limbs::filled(nlimbs(width).max(1), 0xAAAA_AAAA_AAAA_AAAA) };
        v.mask();
        v
    }

    pub fn from_u64(width: u32, x: u64) -> Value {
        let mut v = Value::zero(width);
        v.limbs[0] = x;
        v.mask();
        v
    }

    /// Little-endian 64-bit limbs (JIT arena interchange).
    pub fn limbs64(&self) -> &[u64] {
        &self.limbs
    }

    /// Build from little-endian 64-bit limbs, padding/truncating to the
    /// width's limb count and masking (JIT arena interchange).
    pub fn from_limbs64(width: u32, limbs: Vec<u64>) -> Value {
        let mut l = Limbs::new(limbs);
        l.resize_to(nlimbs(width).max(1));
        let mut v = Value { width, limbs: l };
        v.mask();
        v
    }

    /// From little-endian 32-bit limbs (the BIR constant encoding).
    pub fn from_limbs32(width: u32, l32: &[u32]) -> Value {
        let mut v = Value::zero(width);
        for (i, &l) in l32.iter().enumerate() {
            if i / 2 < v.limbs.len() {
                v.limbs[i / 2] |= (l as u64) << (32 * (i % 2));
            }
        }
        v.mask();
        v
    }

    fn mask(&mut self) {
        let n = nlimbs(self.width).max(1);
        self.limbs.resize_to(n);
        let rem = self.width % 64;
        if rem != 0 {
            let last = self.limbs.len() - 1;
            self.limbs[last] &= (1u64 << rem) - 1;
        }
        if self.width == 0 {
            self.limbs = Limbs::S([0]);
        }
    }

    pub fn as_u64(&self) -> u64 {
        self.limbs[0]
    }

    pub fn is_zero(&self) -> bool {
        self.limbs.iter().all(|&l| l == 0)
    }

    pub fn as_bool(&self) -> bool {
        !self.is_zero()
    }

    pub fn bit(&self, i: u32) -> bool {
        let i = i as usize;
        i / 64 < self.limbs.len() && (self.limbs[i / 64] >> (i % 64)) & 1 == 1
    }

    fn to_bigint(&self) -> Vec<u64> {
        self.limbs.to_vec()
    }

    /// Sign bit under the value's width.
    pub fn sign(&self) -> bool {
        self.width > 0 && self.bit(self.width - 1)
    }

    // --- arithmetic (schoolbook over limbs; widths per BSC semantics:
    // --- result width already decided by the compiler)

    pub fn add(&self, o: &Value, w: u32) -> Value {
        let mut r = Value::zero(w);
        let n = r.limbs.len();
        let mut carry = 0u128;
        for i in 0..n {
            let a = *self.limbs.get(i).unwrap_or(&0) as u128;
            let b = *o.limbs.get(i).unwrap_or(&0) as u128;
            let s = a + b + carry;
            r.limbs[i] = s as u64;
            carry = s >> 64;
        }
        r.mask();
        r
    }

    pub fn sub(&self, o: &Value, w: u32) -> Value {
        // two's complement: a + !b + 1
        let not_b = o.not(w);
        let one = Value::from_u64(w, 1);
        self.add(&not_b, w).add(&one, w)
    }

    pub fn neg(&self, w: u32) -> Value {
        Value::zero(w).sub(self, w)
    }

    pub fn mul(&self, o: &Value, w: u32) -> Value {
        let mut r = Value::zero(w);
        let n = r.limbs.len();
        for i in 0..self.limbs.len().min(n) {
            let mut carry = 0u128;
            for j in 0..o.limbs.len() {
                if i + j >= n {
                    break;
                }
                let cur = r.limbs[i + j] as u128
                    + (self.limbs[i] as u128) * (o.limbs[j] as u128)
                    + carry;
                r.limbs[i + j] = cur as u64;
                carry = cur >> 64;
            }
            // propagate the tail carry past the end of o's limbs (the
            // result can be wider than both operands' storage)
            let mut k = i + o.limbs.len();
            while carry != 0 && k < n {
                let cur = r.limbs[k] as u128 + carry;
                r.limbs[k] = cur as u64;
                carry = cur >> 64;
                k += 1;
            }
        }
        r.mask();
        r
    }

    pub fn quot(&self, o: &Value, w: u32) -> Value {
        // narrow fast path; wide division is rare in practice
        if self.limbs.len() == 1 && o.limbs.len() == 1 {
            let d = o.limbs[0];
            if d == 0 {
                raise_sigfpe();
            }
            return Value::from_u64(w, self.limbs[0] / d);
        }
        self.divmod_wide(o, w).0
    }

    pub fn rem(&self, o: &Value, w: u32) -> Value {
        if self.limbs.len() == 1 && o.limbs.len() == 1 {
            let d = o.limbs[0];
            if d == 0 {
                raise_sigfpe();
            }
            return Value::from_u64(w, self.limbs[0] % d);
        }
        self.divmod_wide(o, w).1
    }

    fn divmod_wide(&self, o: &Value, w: u32) -> (Value, Value) {
        // bit-serial long division; slow but exact
        let mut q = Value::zero(w);
        let mut r = Value::zero(self.width.max(o.width) + 1);
        if o.is_zero() {
            raise_sigfpe();
        }
        for i in (0..self.width).rev() {
            r = r.shl_bits(1);
            if self.bit(i) {
                r.limbs[0] |= 1;
            }
            if !r.ult(o) {
                r = r.sub(o, r.width);
                if (i as usize) / 64 < q.limbs.len() {
                    q.limbs[(i as usize) / 64] |= 1 << (i % 64);
                }
            }
        }
        q.mask();
        let mut rr = r;
        rr.width = w;
        rr.mask();
        (q, rr)
    }

    fn shl_bits(&self, k: u32) -> Value {
        let mut r = Value::zero(self.width);
        for i in 0..self.width {
            if i >= k && self.bit(i - k) {
                r.limbs[(i as usize) / 64] |= 1 << (i % 64);
            }
        }
        r
    }

    pub fn shl(&self, sh: u64, w: u32) -> Value {
        let mut r = Value::zero(w);
        if sh >= w as u64 {
            return r;
        }
        for i in 0..w {
            if (i as u64) >= sh && self.bit(i - sh as u32) {
                r.limbs[(i as usize) / 64] |= 1 << (i % 64);
            }
        }
        r
    }

    pub fn lshr(&self, sh: u64, w: u32) -> Value {
        let mut r = Value::zero(w);
        if sh >= self.width as u64 {
            return r; // also guards i + sh overflow for huge sh
        }
        for i in 0..w {
            let src = i as u64 + sh;
            if src < self.width as u64 && self.bit(src as u32) {
                r.limbs[(i as usize) / 64] |= 1 << (i % 64);
            }
        }
        r
    }

    pub fn ashr(&self, sh: u64, w: u32) -> Value {
        let s = self.sign();
        let mut r = Value::zero(w);
        if sh >= self.width as u64 {
            // pure sign fill; also guards i + sh overflow for huge sh
            if s {
                for i in 0..w {
                    r.limbs[(i as usize) / 64] |= 1 << (i % 64);
                }
            }
            return r;
        }
        for i in 0..w {
            let src = i as u64 + sh;
            let b = if src < self.width as u64 { self.bit(src as u32) } else { s };
            if b {
                r.limbs[(i as usize) / 64] |= 1 << (i % 64);
            }
        }
        r
    }

    // --- bitwise

    fn zip(&self, o: &Value, w: u32, f: impl Fn(u64, u64) -> u64) -> Value {
        let mut r = Value::zero(w);
        for i in 0..r.limbs.len() {
            r.limbs[i] = f(*self.limbs.get(i).unwrap_or(&0), *o.limbs.get(i).unwrap_or(&0));
        }
        r.mask();
        r
    }

    pub fn and(&self, o: &Value, w: u32) -> Value {
        self.zip(o, w, |a, b| a & b)
    }
    pub fn or(&self, o: &Value, w: u32) -> Value {
        self.zip(o, w, |a, b| a | b)
    }
    pub fn xor(&self, o: &Value, w: u32) -> Value {
        self.zip(o, w, |a, b| a ^ b)
    }
    pub fn not(&self, w: u32) -> Value {
        let mut r = Value::zero(w);
        for i in 0..r.limbs.len() {
            r.limbs[i] = !self.limbs.get(i).unwrap_or(&0);
        }
        r.mask();
        r
    }

    // --- comparisons (full-width)

    pub fn eq(&self, o: &Value) -> bool {
        let n = self.limbs.len().max(o.limbs.len());
        (0..n).all(|i| self.limbs.get(i).unwrap_or(&0) == o.limbs.get(i).unwrap_or(&0))
    }

    pub fn ult(&self, o: &Value) -> bool {
        let n = self.limbs.len().max(o.limbs.len());
        for i in (0..n).rev() {
            let a = *self.limbs.get(i).unwrap_or(&0);
            let b = *o.limbs.get(i).unwrap_or(&0);
            if a != b {
                return a < b;
            }
        }
        false
    }

    pub fn ule(&self, o: &Value) -> bool {
        !o.ult(self)
    }

    pub fn slt(&self, o: &Value) -> bool {
        match (self.sign(), o.sign()) {
            (true, false) => true,
            (false, true) => false,
            _ => self.ult(o),
        }
    }

    pub fn sle(&self, o: &Value) -> bool {
        !o.slt(self)
    }

    // --- structure

    pub fn extract(&self, hi: u64, lo: u64, w: u32) -> Value {
        let mut r = Value::zero(w);
        let mut dst = 0u32;
        let mut src = lo;
        while src <= hi && dst < w {
            if src < self.width as u64 && self.bit(src as u32) {
                r.limbs[(dst as usize) / 64] |= 1 << (dst % 64);
            }
            src += 1;
            dst += 1;
        }
        r
    }

    /// concat: self is the MORE significant part.
    pub fn concat(&self, lo: &Value, w: u32) -> Value {
        let mut r = Value::zero(w);
        for i in 0..lo.width.min(w) {
            if lo.bit(i) {
                r.limbs[(i as usize) / 64] |= 1 << (i % 64);
            }
        }
        for i in 0..self.width {
            let dst = i + lo.width;
            if dst < w && self.bit(i) {
                r.limbs[(dst as usize) / 64] |= 1 << (dst % 64);
            }
        }
        r
    }

    pub fn zext(&self, w: u32) -> Value {
        if self.width == STR_MARKER || self.width == REAL_MARKER {
            // string/real values pass through width adjustments unchanged
            // (they only ever flow into task arguments)
            return self.clone();
        }
        let mut r = self.clone();
        r.width = w;
        r.limbs.resize_to(nlimbs(w).max(1));
        r.mask();
        r
    }

    /// A dynamically selected string value: carries an interned string id
    /// instead of bits.  Only valid as a task argument; the marker width
    /// keeps it inert through muxes and def stores.
    pub fn str_ref(id: u32) -> Value {
        Value { width: STR_MARKER, limbs: Limbs::S([id as u64]) }
    }

    pub fn as_str_id(&self) -> Option<u32> {
        if self.width == STR_MARKER {
            Some(self.limbs[0] as u32)
        } else {
            None
        }
    }

    /// The value as little-endian 32-bit limbs (Bluesim's WideData layout
    /// and the BDPI `unsigned int*` ABI).
    pub fn to_u32_limbs(&self) -> Vec<u32> {
        let n = ((self.width.max(1) as usize) + 31) / 32;
        let mut out = Vec::with_capacity(n);
        for k in 0..n {
            let limb = self.limbs.get(k / 2).copied().unwrap_or(0);
            out.push(if k % 2 == 0 { limb as u32 } else { (limb >> 32) as u32 });
        }
        out
    }

    /// Rebuild a value from little-endian 32-bit limbs.
    pub fn from_u32_limbs(w: u32, limbs: &[u32]) -> Value {
        let mut v = Value::zero(w.max(1));
        for (k, &l) in limbs.iter().enumerate() {
            let idx = k / 2;
            if idx < v.limbs.len() {
                v.limbs[idx] |= (l as u64) << (32 * (k % 2));
            }
        }
        v.mask();
        v
    }

    /// A real-valued constant: carries f64 bits behind a marker width so
    /// it stays inert through muxes and def stores (only ever consumed as
    /// a task argument or module parameter).
    pub fn real(v: f64) -> Value {
        Value { width: REAL_MARKER, limbs: Limbs::S([v.to_bits()]) }
    }

    pub fn as_real(&self) -> Option<f64> {
        if self.width == REAL_MARKER {
            Some(f64::from_bits(self.limbs[0]))
        } else {
            None
        }
    }

    pub fn sext(&self, w: u32) -> Value {
        if !self.sign() {
            return self.zext(w);
        }
        let mut r = Value::zero(w);
        for i in 0..w {
            let b = if i < self.width { self.bit(i) } else { true };
            if b {
                r.limbs[(i as usize) / 64] |= 1 << (i % 64);
            }
        }
        r
    }

    // --- printing

    pub fn to_dec_string(&self) -> String {
        // repeated division by 10^19 over limbs
        if self.limbs.len() == 1 {
            return format!("{}", self.limbs[0]);
        }
        let mut digits = String::new();
        let mut cur = self.to_bigint();
        loop {
            let mut rem: u128 = 0;
            let mut all_zero = true;
            for i in (0..cur.len()).rev() {
                let acc = (rem << 64) | cur[i] as u128;
                cur[i] = (acc / 10) as u64;
                rem = acc % 10;
                if cur[i] != 0 {
                    all_zero = false;
                }
            }
            digits.push(char::from_digit(rem as u32, 10).unwrap());
            if all_zero {
                break;
            }
        }
        digits.chars().rev().collect()
    }

    pub fn to_hex_string(&self) -> String {
        let ndig = ((self.width as usize) + 3) / 4;
        let mut s = String::with_capacity(ndig.max(1));
        for d in (0..ndig.max(1)).rev() {
            let bitpos = d * 4;
            let mut nib = 0u32;
            for b in 0..4 {
                if (bitpos + b) < self.width as usize && self.bit((bitpos + b) as u32) {
                    nib |= 1 << b;
                }
            }
            s.push(char::from_digit(nib, 16).unwrap());
        }
        s
    }

    pub fn to_bin_string(&self) -> String {
        let n = self.width.max(1);
        (0..n).rev().map(|i| if self.bit(i) { '1' } else { '0' }).collect()
    }

    pub fn to_oct_string(&self) -> String {
        let ndig = ((self.width as usize) + 2) / 3;
        let mut s = String::with_capacity(ndig.max(1));
        for d in (0..ndig.max(1)).rev() {
            let bitpos = d * 3;
            let mut dig = 0u32;
            for b in 0..3 {
                if (bitpos + b) < self.width as usize && self.bit((bitpos + b) as u32) {
                    dig |= 1 << b;
                }
            }
            s.push(char::from_digit(dig, 8).unwrap());
        }
        s
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn arith_masks_to_width() {
        let a = Value::from_u64(8, 200);
        let b = Value::from_u64(8, 100);
        assert_eq!(a.add(&b, 8).as_u64(), 44); // 300 mod 256
        assert_eq!(a.sub(&b, 8).as_u64(), 100);
        assert_eq!(b.sub(&a, 8).as_u64(), 156); // -100 mod 256
        assert_eq!(a.mul(&b, 8).as_u64(), (200u64 * 100) & 0xFF);
    }

    #[test]
    fn wide_roundtrip_and_dec() {
        let v = Value::from_limbs32(96, &[0xFFFF_FFFF, 0xFFFF_FFFF, 0xF]);
        assert_eq!(v.to_hex_string(), format!("{}{}", "0".repeat(7), "f".repeat(17)));
        let small = Value::from_limbs32(70, &[10, 0, 0]);
        assert_eq!(small.to_dec_string(), "10");
    }

    #[test]
    fn extract_concat() {
        let v = Value::from_u64(16, 0xABCD);
        assert_eq!(v.extract(15, 8, 8).as_u64(), 0xAB);
        assert_eq!(v.extract(7, 0, 8).as_u64(), 0xCD);
        let hi = Value::from_u64(8, 0xAB);
        let lo = Value::from_u64(8, 0xCD);
        assert_eq!(hi.concat(&lo, 16).as_u64(), 0xABCD);
    }

    #[test]
    fn signed_ops() {
        let m1 = Value::from_u64(8, 0xFF); // -1
        let p1 = Value::from_u64(8, 1);
        assert!(m1.slt(&p1));
        assert!(!p1.slt(&m1));
        assert_eq!(m1.sext(16).as_u64(), 0xFFFF);
        assert_eq!(m1.ashr(4, 8).as_u64(), 0xFF);
    }

    #[test]
    fn wide_div() {
        let a = Value::from_limbs32(128, &[0, 0, 1, 0]); // 2^64
        let b = Value::from_u64(128, 3);
        let q = a.quot(&b, 128);
        assert_eq!(q.to_dec_string(), "6148914691236517205");
    }
}
