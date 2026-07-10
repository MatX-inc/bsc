//! FST wave sink over the vendored libfst — a one-to-one mirror of
//! the reference's `src/bluesim/fst.cxx` (same library, same call
//! sequence, same VCD-equivalent byte estimation for $dumplimit).
//! The dump ENGINE stays in vcd.rs; this module only turns scopes,
//! defs, times and value changes into fstWriter calls.

use std::ffi::{c_char, c_int, c_void, CString};

use crate::value::Value;

type FstHandle = u32;

extern "C" {
    fn fstWriterCreate(nam: *const c_char, use_compressed_hier: c_int) -> *mut c_void;
    fn fstWriterClose(ctx: *mut c_void);
    fn fstWriterFlushContext(ctx: *mut c_void);
    fn fstWriterSetPackType(ctx: *mut c_void, typ: c_int);
    fn fstWriterSetVersion(ctx: *mut c_void, vers: *const c_char);
    fn fstWriterSetTimescaleFromString(ctx: *mut c_void, s: *const c_char);
    fn fstWriterSetScope(
        ctx: *mut c_void,
        scopetype: c_int,
        scopename: *const c_char,
        scopecomp: *const c_char,
    );
    fn fstWriterSetUpscope(ctx: *mut c_void);
    fn fstWriterCreateVar(
        ctx: *mut c_void,
        vt: c_int,
        vd: c_int,
        len: u32,
        nam: *const c_char,
        aliasHandle: FstHandle,
    ) -> FstHandle;
    fn fstWriterEmitValueChange(ctx: *mut c_void, handle: FstHandle, val: *const c_void);
    fn fstWriterEmitValueChange32(ctx: *mut c_void, handle: FstHandle, bits: u32, val: u32);
    fn fstWriterEmitValueChange64(ctx: *mut c_void, handle: FstHandle, bits: u32, val: u64);
    fn fstWriterEmitTimeChange(ctx: *mut c_void, tim: u64);
    fn fstWriterEmitDumpActive(ctx: *mut c_void, enable: c_int);
}

const FST_WR_PT_LZ4: c_int = 2;
const FST_ST_VCD_MODULE: c_int = 0;
const FST_VT_VCD_REG: c_int = 5;
const FST_VD_IMPLICIT: c_int = 0;

/// The writer half of fst.cxx's FstWriter.
pub struct Fst {
    ctx: *mut c_void,
    /// per-num signal info, indexed by the engine's id numbers
    handles: Vec<FstHandle>,
    widths: Vec<u32>,
    /// FST times must be monotonically non-decreasing
    last_time: u64,
    wrote_time: bool,
    /// VCD-equivalent output size estimate: fstapi's own limit is
    /// checked every ~128MB section, far too coarse for Bluesim's
    /// per-event checks — estimate the equivalent VCD text so a
    /// $dumplimit stops an FST dump at the same simulation point
    bytes_emitted: u64,
}

impl Fst {
    /// fstWriterCreate + LZ4 pack type; reports errors like the
    /// reference's open() (perror-style on stderr).
    pub fn create(name: &str) -> Result<Fst, ()> {
        let Ok(cname) = CString::new(name) else { return Err(()) };
        let ctx = unsafe { fstWriterCreate(cname.as_ptr(), 1) };
        if ctx.is_null() {
            eprintln!("{name}: cannot create FST file");
            return Err(());
        }
        unsafe { fstWriterSetPackType(ctx, FST_WR_PT_LZ4) };
        Ok(Fst {
            ctx,
            handles: Vec::new(),
            widths: Vec::new(),
            last_time: 0,
            wrote_time: false,
            bytes_emitted: 0,
        })
    }

    pub fn write_header(&mut self, timescale: &str) {
        let version = CString::new(format!("Bluespec FST dumper 2.1")).unwrap();
        unsafe { fstWriterSetVersion(self.ctx, version.as_ptr()) };
        if let Ok(ts) = CString::new(timescale) {
            unsafe { fstWriterSetTimescaleFromString(self.ctx, ts.as_ptr()) };
        }
        // when a header is (re)written the engine hands out ids afresh
        self.handles.clear();
        self.widths.clear();
        // the equivalent of VCD's $date/$version/$timescale text
        self.bytes_emitted += 120;
    }

    pub fn scope_start(&mut self, name: &str, module_type: Option<&str>) {
        let Ok(n) = CString::new(name) else { return };
        let comp = module_type.and_then(|m| CString::new(m).ok());
        unsafe {
            fstWriterSetScope(
                self.ctx,
                FST_ST_VCD_MODULE,
                n.as_ptr(),
                comp.as_ref().map_or(std::ptr::null(), |c| c.as_ptr()),
            )
        };
        self.bytes_emitted += name.len() as u64 + 22; // "$scope module ... $end"
    }

    pub fn scope_end(&mut self) {
        unsafe { fstWriterSetUpscope(self.ctx) };
        self.bytes_emitted += 15; // "$upscope $end"
    }

    pub fn write_def(&mut self, num: u32, name: &str, width: u32) {
        let n = num as usize;
        if n >= self.handles.len() {
            self.handles.resize(n + 1, 0);
            self.widths.resize(n + 1, 0);
        }
        let Ok(cname) = CString::new(name) else { return };
        if self.handles[n] != 0 {
            // an additional definition of the same id is an alias
            unsafe {
                fstWriterCreateVar(
                    self.ctx,
                    FST_VT_VCD_REG,
                    FST_VD_IMPLICIT,
                    self.widths[n],
                    cname.as_ptr(),
                    self.handles[n],
                )
            };
            return;
        }
        let width = width.max(1);
        self.handles[n] = unsafe {
            fstWriterCreateVar(
                self.ctx,
                FST_VT_VCD_REG,
                FST_VD_IMPLICIT,
                width,
                cname.as_ptr(),
                0,
            )
        };
        self.widths[n] = width;
        self.bytes_emitted += name.len() as u64 + 18; // "$var reg N ! ... $end"
    }

    pub fn write_time(&mut self, mut time: u64) {
        // guard against any non-monotonic residue in the change buffering
        if self.wrote_time && time < self.last_time {
            time = self.last_time;
        }
        unsafe { fstWriterEmitTimeChange(self.ctx, time) };
        self.last_time = time;
        self.wrote_time = true;
        self.bytes_emitted += 8;
    }

    /// $dumpvars/$dumpall carry no extra information in FST (the
    /// values themselves are dumped); $dumpoff/$dumpon map to
    /// blackout regions.
    pub fn task(&mut self, task: &str) {
        match task {
            "$dumpoff" => unsafe { fstWriterEmitDumpActive(self.ctx, 0) },
            "$dumpon" => unsafe { fstWriterEmitDumpActive(self.ctx, 1) },
            _ => {}
        }
    }

    fn width_of(&self, num: u32) -> u32 {
        self.widths
            .get(num as usize)
            .copied()
            .filter(|&w| w != 0)
            .unwrap_or(1)
    }

    fn has_handle(&self, num: u32) -> bool {
        self.handles.get(num as usize).is_some_and(|&h| h != 0)
    }

    /// significant_bits digits (at least one), the 'b'/' ' framing
    /// for vectors, and roughly an id code plus a newline
    fn count_change(&mut self, width: u32, significant_bits: u32) {
        let digits = significant_bits.max(1) as u64;
        self.bytes_emitted += (if width == 1 { 1 } else { digits + 2 }) + 4;
    }

    pub fn write_x(&mut self, num: u32) {
        if !self.has_handle(num) {
            return;
        }
        let w = self.width_of(num);
        let buf = vec![b'x'; w as usize];
        unsafe {
            fstWriterEmitValueChange(
                self.ctx,
                self.handles[num as usize],
                buf.as_ptr() as *const c_void,
            )
        };
        self.count_change(w, 1);
    }

    /// fstWriterEmitValueChange requires exactly the declared number
    /// of bits, so changes pad (or truncate) to the declared width.
    pub fn write_val(&mut self, num: u32, v: &Value) {
        if !self.has_handle(num) {
            return;
        }
        let w = self.width_of(num);
        let h = self.handles[num as usize];
        if v.width <= 64 && w <= 32 {
            let val = v.as_u64() as u32;
            unsafe { fstWriterEmitValueChange32(self.ctx, h, w, val) };
            self.count_change(w, sig_bits_u64(v.as_u64()));
        } else if v.width <= 64 && w <= 64 {
            let val = v.as_u64();
            unsafe { fstWriterEmitValueChange64(self.ctx, h, w, val) };
            self.count_change(w, sig_bits_u64(val));
        } else {
            // char-per-bit, MSB first, padded/truncated to the
            // declared width (fst.cxx's wide path)
            let limbs = v.limbs64();
            let mut buf = vec![b'0'; w as usize];
            let mut sig = 0u32;
            for i in 0..(v.width.min(w) as usize) {
                let bit = (limbs.get(i / 64).copied().unwrap_or(0) >> (i % 64)) & 1;
                buf[w as usize - 1 - i] = b'0' + bit as u8;
                if bit == 1 {
                    sig = i as u32 + 1;
                }
            }
            unsafe {
                fstWriterEmitValueChange(self.ctx, h, buf.as_ptr() as *const c_void)
            };
            self.count_change(w, sig);
        }
    }

    pub fn flush(&mut self) {
        unsafe { fstWriterFlushContext(self.ctx) };
    }

    /// The engine's file-size limit against the VCD-equivalent
    /// estimate.  No in-file comment on a tripped limit (fst.cxx:
    /// older FST readers reject the comment attribute; the dump just
    /// stops and the file ends at the limit).
    pub fn limit_exceeded(&self, limit: u64) -> bool {
        limit != 0 && self.bytes_emitted > limit
    }
}

impl Drop for Fst {
    fn drop(&mut self) {
        if !self.ctx.is_null() {
            unsafe { fstWriterClose(self.ctx) };
            self.ctx = std::ptr::null_mut();
        }
    }
}

fn sig_bits_u64(v: u64) -> u32 {
    64 - v.leading_zeros()
}
