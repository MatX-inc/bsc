//! BDPI foreign-function dispatch.
//!
//! The C ABI is fixed by `ForeignFunctions.hs` (`toCtype`/`mkFFDecl`):
//! narrow values pass by value (char / unsigned int / unsigned long long
//! by width class), wide and polymorphic values pass as `unsigned int*`
//! little-endian 32-bit limb pointers, strings pass as `char*`, and a
//! wide/polymorphic RETURN becomes a void return with an out-pointer as
//! the FIRST argument.
//!
//! Every argument class is integer-class on x86-64/AArch64 SysV (no
//! floats in the ABI), so the call itself is made through arity-matched
//! `extern "C" fn(u64, ...) -> u64` pointers: narrow arguments are
//! zero-padded into full registers (callees read only their declared
//! low bits) and pointers ride in full slots.

use crate::format::Arg;
use crate::value::Value;
use trs_ir::{ForeignFunc, ForeignType};
use std::ffi::CString;

pub struct Bdpi {
    /// Keeps the dlopened user code alive for the run.
    _lib: libloading::Library,
    /// c_name -> resolved symbol address.
    syms: std::collections::HashMap<String, usize>,
}

impl Bdpi {
    /// dlopen the companion shared object and resolve every imported
    /// function eagerly (a missing symbol should fail at load, like the
    /// reference's link step would).
    pub fn load(
        path: &std::path::Path,
        funcs: &[(String, String)], // (name, c_name)
    ) -> Result<Bdpi, String> {
        let lib = unsafe { libloading::Library::new(path) }
            .map_err(|e| format!("{}: {e}", path.display()))?;
        let mut syms = std::collections::HashMap::new();
        for (name, c_name) in funcs {
            let sym: libloading::Symbol<unsafe extern "C" fn()> =
                unsafe { lib.get(c_name.as_bytes()) }.map_err(|e| {
                    format!("{}: undefined BDPI symbol {c_name:?}: {e}", path.display())
                })?;
            let fptr: unsafe extern "C" fn() = *sym;
            syms.insert(name.clone(), fptr as usize);
        }
        Ok(Bdpi { _lib: lib, syms })
    }

    pub fn has(&self, name: &str) -> bool {
        self.syms.contains_key(name)
    }

    /// Marshal and call.  `ret_width` is the bit width the call site
    /// expects back (the actual width for polymorphic returns).
    pub fn call(&self, ff: &ForeignFunc, name: &str, args: &[Arg], ret_width: u32) -> Value {
        let sym = *self.syms.get(name).unwrap_or_else(|| {
            panic!("BDPI function {name:?} not loaded");
        });

        let mut slots: Vec<u64> = Vec::new();
        // keep-alive storage for pointer arguments
        let mut cstrs: Vec<CString> = Vec::new();
        let mut bufs: Vec<Vec<u32>> = Vec::new();

        // wide/poly return: out-pointer is the first argument
        let ret_buf: Option<usize> = match ff.ret {
            ForeignType::Wide(n) => Some(((n.max(1) as usize) + 31) / 32),
            ForeignType::Poly => Some(((ret_width.max(1) as usize) + 31) / 32),
            _ => None,
        }
        .map(|n| {
            bufs.push(vec![0u32; n]);
            slots.push(bufs.last().unwrap().as_ptr() as u64);
            bufs.len() - 1
        });

        for (k, ft) in ff.args.iter().enumerate() {
            let a = args.get(k).unwrap_or_else(|| {
                panic!("BDPI {name:?}: missing argument {k}");
            });
            match (ft, a) {
                (ForeignType::CString, Arg::Str(s)) => {
                    let c = CString::new(s.as_str()).unwrap_or_default();
                    slots.push(c.as_ptr() as u64);
                    cstrs.push(c);
                }
                (ForeignType::Bits(_), Arg::Val(v, _)) => slots.push(v.as_u64()),
                (ForeignType::Wide(_), Arg::Val(v, _))
                | (ForeignType::Poly, Arg::Val(v, _)) => {
                    bufs.push(v.to_u32_limbs());
                    slots.push(bufs.last().unwrap().as_ptr() as u64);
                }
                // a string literal passed where bits are declared: its
                // packed bytes (mirrors fill_tValue's string handling)
                (ForeignType::Bits(_), Arg::Str(s)) => {
                    slots.push(crate::format::str_value(s).as_u64())
                }
                (ft, a) => panic!("BDPI {name:?}: argument {k} mismatch {ft:?} vs {a:?}"),
            }
        }

        let r = unsafe { call_integer_abi(sym, &slots) };

        match ff.ret {
            ForeignType::Void => Value::zero(1),
            ForeignType::Bits(n) => {
                let nn = n.min(64).max(1);
                let masked = if nn == 64 { r } else { r & ((1u64 << nn) - 1) };
                Value::from_u64(ret_width.max(nn), masked)
            }
            ForeignType::Wide(_) | ForeignType::Poly => {
                let buf = &bufs[ret_buf.unwrap()];
                Value::from_u32_limbs(ret_width.max(1), buf)
            }
            ForeignType::CString => panic!("BDPI {name:?}: string returns not supported"),
        }
    }
}

/// Call an integer-class-only C function of up to 8 u64 slots (BDPI
/// signatures in practice are tiny; extend if a design needs more).
unsafe fn call_integer_abi(f: usize, a: &[u64]) -> u64 {
    use std::mem::transmute as t;
    unsafe {
        match a.len() {
            0 => t::<usize, extern "C" fn() -> u64>(f)(),
            1 => t::<usize, extern "C" fn(u64) -> u64>(f)(a[0]),
            2 => t::<usize, extern "C" fn(u64, u64) -> u64>(f)(a[0], a[1]),
            3 => t::<usize, extern "C" fn(u64, u64, u64) -> u64>(f)(a[0], a[1], a[2]),
            4 => t::<usize, extern "C" fn(u64, u64, u64, u64) -> u64>(f)(a[0], a[1], a[2], a[3]),
            5 => t::<usize, extern "C" fn(u64, u64, u64, u64, u64) -> u64>(f)(
                a[0], a[1], a[2], a[3], a[4],
            ),
            6 => t::<usize, extern "C" fn(u64, u64, u64, u64, u64, u64) -> u64>(f)(
                a[0], a[1], a[2], a[3], a[4], a[5],
            ),
            7 => t::<usize, extern "C" fn(u64, u64, u64, u64, u64, u64, u64) -> u64>(f)(
                a[0], a[1], a[2], a[3], a[4], a[5], a[6],
            ),
            8 => t::<usize, extern "C" fn(u64, u64, u64, u64, u64, u64, u64, u64) -> u64>(f)(
                a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
            ),
            n => panic!("BDPI call with {n} slots (limit 8)"),
        }
    }
}
