//! RunCore boot (docs/RUNCORE.md): run a compiled artifact from its
//! baked sidecar with no design decode, no Interp, no planning, and
//! no reset choreography — the sidecar's window sections carry the
//! byte-witnessed post-reset-window state, so the boot drops straight
//! into the central loop's steady body.
//!
//! Opt-in via TRS_RUNCORE=1.  Every gate failure returns None and the
//! caller boots classic — a RunCore boot either runs byte-identically
//! or does not run at all.  The eligibility flag was proven at link
//! by mirroring the central loop's bail conditions, the arena image
//! and clock state were byte-compared against a classic run at the
//! engage instant (corpus-proven under TRS_RUNCORE_CHECK), and the
//! foreign surface is exactly the ForeignEnv split: what ForeignEnv
//! declines cannot occur on an eligible design, and reaching it here
//! is a loud panic, never a wrong byte.
//!
//! Compiled prim call sites (rung 3b) are serviced NATIVELY: the
//! sidecar bakes each bounce-reachable prim's static config, the boot
//! restores the identical prim.rs struct and adopt-attaches it to the
//! live arena slots (which attach makes the single source of truth —
//! the window image carries their live values), and runcore_prim_cb
//! dispatches the same trait methods jit_prim_cb would.  No design
//! decode, no Interp, no reflection: precisely the prims the compiled
//! code can bounce on, nothing else.

use std::collections::HashMap;

use crate::foreign::ForeignEnv;
use crate::format::Arg;
use crate::jit::{rle_decode, RC_SEC_CLOCK, RC_SEC_COMPS, RC_SEC_ELIG, RC_SEC_LOADS, RC_SEC_PATHS, RC_SEC_PRIMS, RC_SEC_STRINGS, RC_SEC_WARENA, RC_SEC_WARNS, RC_SEC_WSTATE};
use crate::prim::Prim;
use crate::value::Value;
use trs_codegen::abi::{self, FArgSpec, FnProtos, TOKEN_KIND_EXEC};

/// Everything a boot needs from the sidecar.
struct Boot {
    hash: u64,
    nslots: usize,
    strings: Vec<String>,
    paths: Vec<String>,
    hi: u64,
    lo: u64,
    pos: Vec<usize>,
    warns: Vec<(u64, u32, String)>,
    eligible: bool,
    /// (post-window arena, tp, cycle)
    window: Option<(Vec<u64>, u64, u64)>,
    /// bounce-reachable prim seeds: (inst, slot, tag, words, strings)
    prims: Vec<(usize, usize, u64, Vec<u64>, Vec<String>)>,
    /// mem-file loads: (inst, file, binary_format), construction order
    loads: Vec<(usize, String, bool)>,
}

/// Strict sidecar parse for the boot path: every field is hostile,
/// every anomaly is None (classic boot).  Unlike the witness parser
/// this one is silent — the classic boot under TRS_RUNCORE_CHECK is
/// where anomalies get diagnosed loudly.
fn parse_sidecar(bytes: &[u8]) -> Option<Boot> {
    if bytes.len() < 40 || &bytes[..8] != b"TRSARENA" {
        return None;
    }
    let rd = |k: usize| {
        u64::from_le_bytes(bytes[8 + 8 * k..16 + 8 * k].try_into().unwrap())
    };
    // version 5 = current eligibility semantics (mem-file overlay);
    // older versions' eligible flags were computed under different
    // gate rules — classic boot
    if rd(0) != 5 || rd(1) != abi::AOT_LAYOUT_REV {
        return None;
    }
    let hash = rd(2);
    let nslots = usize::try_from(rd(3)).ok()?;
    // skip the boot-time arena RLE (the window image supersedes it,
    // but the runs must still be walked to find the sections)
    let mut pos = 40usize;
    let mut slot = 0usize;
    while slot < nslots {
        let run = u64::from_le_bytes(
            bytes.get(pos + 8..pos + 16)?.try_into().ok()?,
        );
        pos += 16;
        let run = usize::try_from(run).ok()?;
        if run == 0 || run > nslots - slot {
            return None;
        }
        slot += run;
    }
    let take8 = |pos: &mut usize| -> Option<u64> {
        let v = bytes.get(*pos..*pos + 8)?;
        *pos += 8;
        Some(u64::from_le_bytes(v.try_into().unwrap()))
    };
    let take_str = |pos: &mut usize| -> Option<String> {
        let n = usize::try_from(take8(pos)?).ok()?;
        let s = bytes.get(*pos..pos.checked_add(n)?)?;
        *pos += n;
        Some(std::str::from_utf8(s).ok()?.to_string())
    };
    if bytes.get(pos..pos + 8) != Some(&b"TRSBOOTD"[..]) {
        return None;
    }
    pos += 8;
    let nsect = take8(&mut pos)?;
    if nsect > 64 {
        return None;
    }
    let mut b = Boot {
        hash,
        nslots,
        strings: Vec::new(),
        paths: Vec::new(),
        hi: 0,
        lo: 0,
        pos: Vec::new(),
        warns: Vec::new(),
        eligible: false,
        window: None,
        prims: Vec::new(),
        loads: Vec::new(),
    };
    let mut warena = None;
    let mut wstate = None;
    let mut seen = 0u64;
    for _ in 0..nsect {
        let tag = take8(&mut pos)?;
        let len = usize::try_from(take8(&mut pos)?).ok()?;
        let end = pos.checked_add(len)?.next_multiple_of(8);
        if end > bytes.len() {
            return None;
        }
        if (1..=63).contains(&tag) {
            if seen & (1 << tag) != 0 {
                return None;
            }
            seen |= 1 << tag;
        }
        let mut p = pos;
        match tag {
            RC_SEC_STRINGS => {
                let n = usize::try_from(take8(&mut p)?).ok()?;
                b.strings = Vec::with_capacity(n.min(1 << 20));
                for _ in 0..n {
                    b.strings.push(take_str(&mut p)?);
                }
            }
            RC_SEC_PATHS => {
                let n = usize::try_from(take8(&mut p)?).ok()?;
                b.paths = Vec::with_capacity(n.min(1 << 20));
                for _ in 0..n {
                    b.paths.push(take_str(&mut p)?);
                }
            }
            RC_SEC_CLOCK => {
                b.hi = take8(&mut p)?;
                b.lo = take8(&mut p)?;
                // delay/init_high/has_init: baked for completeness;
                // the steady loop needs only the period
            }
            RC_SEC_COMPS => {
                let np = usize::try_from(take8(&mut p)?).ok()?;
                for _ in 0..np {
                    b.pos.push(usize::try_from(take8(&mut p)?).ok()?);
                }
            }
            RC_SEC_WARNS => {
                let n = usize::try_from(take8(&mut p)?).ok()?;
                for _ in 0..n {
                    let slot = take8(&mut p)?;
                    let bits = take8(&mut p)? as u32;
                    let name = take_str(&mut p)?;
                    if slot >= b.nslots as u64 {
                        return None;
                    }
                    b.warns.push((slot, bits, name));
                }
            }
            RC_SEC_ELIG => {
                b.eligible = take8(&mut p)? != 0;
                let _central = take8(&mut p)?;
                let _reason = take_str(&mut p)?;
            }
            RC_SEC_WARENA => {
                warena = Some(rle_decode(&bytes[p..pos + len], b.nslots)?);
            }
            RC_SEC_WSTATE => {
                let tp = take8(&mut p)?;
                let _tn = take8(&mut p)?;
                let cyc = take8(&mut p)?;
                wstate = Some((tp, cyc));
            }
            RC_SEC_PRIMS => {
                let n = usize::try_from(take8(&mut p)?).ok()?;
                if n > 1 << 16 {
                    return None;
                }
                let mut seen_insts = std::collections::HashSet::new();
                for _ in 0..n {
                    let inst = usize::try_from(take8(&mut p)?).ok()?;
                    let slot = usize::try_from(take8(&mut p)?).ok()?;
                    let tag = take8(&mut p)?;
                    let nw = usize::try_from(take8(&mut p)?).ok()?;
                    if nw > 64 {
                        return None;
                    }
                    let mut ws = Vec::with_capacity(nw);
                    for _ in 0..nw {
                        ws.push(take8(&mut p)?);
                    }
                    let ns = usize::try_from(take8(&mut p)?).ok()?;
                    if ns > 8 {
                        return None;
                    }
                    let mut ss = Vec::with_capacity(ns);
                    for _ in 0..ns {
                        ss.push(take_str(&mut p)?);
                    }
                    // slot extent (footprint) is checked at restore
                    // time; the base at least must be in the arena
                    if slot >= b.nslots {
                        return None;
                    }
                    // duplicate rows would silently collapse in the
                    // boot's map, masking a missing seed
                    if !seen_insts.insert(inst) {
                        return None;
                    }
                    b.prims.push((inst, slot, tag, ws, ss));
                }
                // the rows must not have walked past this section
                if p > end {
                    return None;
                }
            }
            RC_SEC_LOADS => {
                let n = usize::try_from(take8(&mut p)?).ok()?;
                if n > 1 << 12 {
                    return None;
                }
                let mut seen_insts = std::collections::HashSet::new();
                for _ in 0..n {
                    let inst = usize::try_from(take8(&mut p)?).ok()?;
                    let file = take_str(&mut p)?;
                    let bin = take8(&mut p)? != 0;
                    if !seen_insts.insert(inst) {
                        return None;
                    }
                    b.loads.push((inst, file, bin));
                }
                if p > end {
                    return None;
                }
            }
            _ => return None,
        }
        pos = end;
    }
    if seen & (1 << RC_SEC_STRINGS) == 0
        || seen & (1 << RC_SEC_PATHS) == 0
        || seen & (1 << RC_SEC_CLOCK) == 0
        || seen & (1 << RC_SEC_COMPS) == 0
        || seen & (1 << RC_SEC_ELIG) == 0
        || seen & (1 << RC_SEC_PRIMS) == 0
        || seen & (1 << RC_SEC_LOADS) == 0
    {
        return None;
    }
    // prim seed insts index the instance-path table
    if b.prims.iter().any(|(inst, ..)| *inst >= b.paths.len()) {
        return None;
    }
    // every load row must have a seed row OF A MEM-FILE KIND: the
    // boot's overlay drives the load through that restored prim, and
    // only RegFile/Bram implement it — a row pointing at any other
    // tag would panic in the default runcore_overlay (review finding:
    // a hostile sidecar must bail to classic, never crash)
    if b.loads.iter().any(|(inst, ..)| {
        !b.prims.iter().any(|(pi, _, tag, ..)| {
            pi == inst
                && matches!(
                    *tag,
                    crate::prim::RC_PRIM_REGFILE | crate::prim::RC_PRIM_BRAM
                )
        })
    }) {
        return None;
    }
    if let (Some(a), Some((tp, cyc))) = (warena, wstate) {
        b.window = Some((a, tp, cyc));
    }
    (b.eligible && b.window.is_some() && b.hi + b.lo > 0 && !b.pos.is_empty())
        .then_some(b)
}

/// The boot's foreign context: ForeignEnv plus the baked tables —
/// the whole state behind runcore_foreign_cb.
struct RunCore {
    fe: ForeignEnv,
    strings: Vec<String>,
    dyn_strs: Vec<String>,
    arg_strs: HashMap<u32, std::sync::Arc<str>>,
    paths: Vec<String>,
    protos: Vec<FnProtos>,
    /// $random/$srandom (library rand32/srand BDPI): same fresh glibc
    /// stream a classic boot starts with; window-time draws are an
    /// eligibility gate, so fresh-at-boot is exact
    rng: crate::GlibcRandom,
    now: u64,
    /// native bounce servicers (rung 3b), keyed by global inst index:
    /// the SAME prim.rs structs the interp uses, restored from their
    /// baked static config and adopt-attached to the live arena slots
    /// (post-attach, slots are the single source of truth — the
    /// restored prim is indistinguishable from a classic-boot one)
    prims: HashMap<usize, Box<dyn Prim>>,
    /// per-boot scratch reused across foreign bounces (jit_foreign_cb's
    /// buffer discipline): the argv spine, task name, and %m location
    foreign_argv: Vec<Arg>,
    fname_buf: String,
    loc_buf: String,
    /// dense Arc cache for design-table string ids (the hot format
    /// strings); dyn ids stay on the arg_strs map
    arg_strs_vec: Vec<Option<std::sync::Arc<str>>>,
}

impl RunCore {
    fn s(&self, id: u32) -> &str {
        let n = self.strings.len();
        if (id as usize) < n {
            &self.strings[id as usize]
        } else {
            &self.dyn_strs[id as usize - n]
        }
    }
    fn arg_str(&mut self, id: u32) -> std::sync::Arc<str> {
        // design-table ids take a dense index (no per-call hashing);
        // dyn ids (appended past the table) stay on the map
        let n = self.strings.len();
        if (id as usize) < n {
            if self.arg_strs_vec.is_empty() {
                self.arg_strs_vec = vec![None; n];
            }
            if let Some(a) = &self.arg_strs_vec[id as usize] {
                return a.clone();
            }
            let a: std::sync::Arc<str> =
                std::sync::Arc::from(self.strings[id as usize].as_str());
            self.arg_strs_vec[id as usize] = Some(a.clone());
            return a;
        }
        if let Some(a) = self.arg_strs.get(&id) {
            return a.clone();
        }
        let a: std::sync::Arc<str> = std::sync::Arc::from(self.s(id));
        self.arg_strs.insert(id, a.clone());
        a
    }
}

/// jit_foreign_cb's twin over the baked tables: same token decode,
/// same marshaling, ForeignEnv instead of an Interp.  A declined
/// task ($dump*, BDPI) reaching here is an eligibility-gate bug —
/// panic loudly rather than produce a wrong byte.
unsafe extern "C" fn runcore_foreign_cb(
    env: *mut core::ffi::c_void,
    token: u64,
    args: *const u64,
    out: *mut u64,
) -> i32 {
    let rc = &mut *(env as *mut RunCore);
    let ordinal = (token >> 17) as usize;
    let is_exec = token & TOKEN_KIND_EXEC != 0;
    let local = (token & 0xffff) as usize;
    // take the protos table for the marshal walk (rc.arg_str needs
    // &mut rc) — a Vec move, not a copy; restored before dispatch
    let protos = std::mem::take(&mut rc.protos);
    let fs = if is_exec {
        &protos[ordinal].exec_foreign[local]
    } else {
        &protos[ordinal].sched_foreign[local]
    };
    let (inst, func, ret_width) = (fs.inst, fs.func, fs.ret_width);
    // per-boot scratch (jit_foreign_cb's buffer discipline): the argv
    // spine survives across bounces, single-limb Values stay inline
    let mut argv = std::mem::take(&mut rc.foreign_argv);
    argv.clear();
    argv.reserve(fs.args.len());
    let mut off = 0usize;
    for a in &fs.args {
        match *a {
            FArgSpec::Str(sid) => argv.push(Arg::Str(rc.arg_str(sid))),
            FArgSpec::Num { width, signed } => {
                let words = ((width.max(1) as usize) + 63) / 64;
                let limbs = std::slice::from_raw_parts(args.add(off), words);
                argv.push(Arg::Val(
                    Value::from_limb_slice(width, limbs),
                    signed,
                ));
                off += words;
            }
            FArgSpec::Real => {
                argv.push(Arg::Real(f64::from_bits(*args.add(off))));
                off += 1;
            }
            FArgSpec::StrDyn => {
                let word = *args.add(off);
                argv.push(Arg::Str(rc.arg_str(word as u32)));
                off += 1;
            }
        }
    }
    rc.protos = protos;
    if func == trs_codegen::abi::STRING_CONCAT_FUNC {
        let mut text = String::new();
        for a in &argv {
            if let Arg::Str(s) = a {
                text.push_str(s);
            }
        }
        let id = rc.strings.len() + rc.dyn_strs.len();
        rc.dyn_strs.push(text);
        *out = id as u64;
        argv.clear();
        rc.foreign_argv = argv;
        return 0;
    }
    let mut name = std::mem::take(&mut rc.fname_buf);
    name.clear();
    name.push_str(rc.s(func));
    let mut loc = std::mem::take(&mut rc.loc_buf);
    loc.clear();
    loc.push_str("top");
    let p = &rc.paths[inst];
    if !p.is_empty() {
        loc.push('.');
        loc.push_str(p);
    }
    if ret_width == 0 {
        if !rc.fe.action(&name, &argv, rc.now, &loc) {
            if name != "srand" {
                panic!(
                    "trs runcore: action task {name:?} reached the boot \
                     (eligibility-gate bug)"
                );
            }
            let seed = match argv.first() {
                Some(Arg::Val(v, _)) => v.as_u64() as u32,
                _ => 0,
            };
            rc.rng.srandom(seed);
        }
    } else {
        let v = match rc.fe.value(&name, &argv, ret_width, rc.now, &loc) {
            Some(v) => v,
            None if name == "rand32" => {
                Value::from_u64(ret_width.max(1), rc.rng.next() as u64)
            }
            None => panic!(
                "trs runcore: value task {name:?} reached the boot \
                 (eligibility-gate bug)"
            ),
        };
        let words = ((ret_width.max(1) as usize) + 63) / 64;
        let dst = std::slice::from_raw_parts_mut(out, words);
        for (i, d) in dst.iter_mut().enumerate() {
            *d = v.limbs64().get(i).copied().unwrap_or(0);
        }
    }
    // return the scratch (a re-entered task took fresh empties via
    // mem::take, so this only upgrades capacity back)
    argv.clear();
    rc.foreign_argv = argv;
    rc.fname_buf = name;
    rc.loc_buf = loc;
    0
}

/// jit_prim_cb's twin over the restored prims (rung 3b): same token
/// decode, same marshaling, same trait methods — the prim is the
/// identical prim.rs struct over the identical slots, so a bounce
/// here is byte-for-byte the classic bounce.  An inst without a
/// restored prim is an eligibility-gate bug: panic loudly rather
/// than produce a wrong byte.
unsafe extern "C" fn runcore_prim_cb(
    env: *mut core::ffi::c_void,
    token: u64,
    args: *const u64,
    out: *mut u64,
) {
    let rc = &mut *(env as *mut RunCore);
    let ordinal = (token >> 17) as usize;
    let is_exec = token & TOKEN_KIND_EXEC != 0;
    let local = (token & 0xffff) as usize;
    let pc = if is_exec {
        &rc.protos[ordinal].exec_prims[local]
    } else {
        &rc.protos[ordinal].sched_prims[local]
    };
    // marshal exactly as jit_prim_cb: w.max(1) words per argument on
    // both sides of the ABI, TRUE logical width on the Value
    let mut argv = Vec::with_capacity(pc.arg_widths.len());
    let mut off = 0usize;
    for &w in &pc.arg_widths {
        let words = ((w.max(1) as usize) + 63) / 64;
        argv.push(Value::from_limb_slice(
            w,
            std::slice::from_raw_parts(args.add(off), words),
        ));
        off += words;
    }
    let Some(p) = rc.prims.get_mut(&pc.inst) else {
        panic!(
            "trs runcore: prim bounce on an unseeded inst \
             (eligibility-gate bug)"
        );
    };
    crate::prim::FROM_COMPILED.with(|c| c.set(token));
    if pc.method == trs_codegen::abi::GATE_OUT_METHOD {
        // sentinel first: GATE_OUT is NOT a string id (panel finding —
        // resolving it through the table panics on every gate bounce)
        *out = p.gate_out() as u64;
    } else if pc.is_action {
        // method ids name design strings (never dyn); alloc-free
        let name: &str = rc
            .strings
            .get(pc.method as usize)
            .expect("prim method id outside the design string table");
        p.action_method(name, &argv, rc.now);
    } else {
        let name: &str = rc
            .strings
            .get(pc.method as usize)
            .expect("prim method id outside the design string table");
        let v = p.value_method(name, &argv, rc.now);
        let words = ((pc.ret_width.max(1) as usize) + 63) / 64;
        let dst = std::slice::from_raw_parts_mut(out, words);
        for (i, d) in dst.iter_mut().enumerate() {
            *d = v.limbs64().get(i).copied().unwrap_or(0);
        }
    }
    crate::prim::FROM_COMPILED.with(|c| c.set(u64::MAX));
}

/// Attempt a RunCore boot for `so` + its `.arena` sidecar.  Some(rc)
/// = the run completed here; None = boot classic (silently — the
/// classic path's witnesses own the diagnostics).
pub fn try_boot(so: &str, max_cycles: u64, plusargs: &[String]) -> Option<i32> {
    if std::env::var_os("TRS_STARTUP_TIME").is_some() {
        eprintln!("trs runcore: try_boot({so})");
    }
    if !std::env::var("TRS_RUNCORE")
        .map(|v| !(v.is_empty() || v == "0" || v == "off"))
        .unwrap_or(false)
    {
        return None;
    }
    // hybrid-JIT and wave requests want the full classic machinery
    if std::env::var("TRS_JIT")
        .map(|v| !(v.is_empty() || v == "0" || v == "off"))
        .unwrap_or(false)
    {
        return None;
    }
    if plusargs.iter().any(|a| a.starts_with("bscvcd") || a.starts_with("bscfst"))
    {
        return None;
    }
    let diag = std::env::var_os("TRS_STARTUP_TIME").is_some();
    let bail = |what: &str| {
        if diag {
            eprintln!("trs runcore: classic boot ({what})");
        }
    };
    let t0 = diag.then(std::time::Instant::now);
    let sidecar = std::path::Path::new(so).with_extension("arena");
    let Ok(bytes) = std::fs::read(&sidecar) else {
        bail("no sidecar");
        return None;
    };
    let Some(boot) = parse_sidecar(&bytes) else {
        bail("sidecar ineligible or malformed");
        return None;
    };
    // the artifact must be the sidecar's twin: same salted design
    // hash (untraced salt = 0 — traced designs never get a sidecar)
    // dlopen semantics: a slash-free path is a LIBRARY NAME lookup
    // (LD_LIBRARY_PATH etc.), never the cwd — anchor it explicitly
    let sopath = if so.contains('/') {
        so.to_string()
    } else {
        format!("./{so}")
    };
    let lib = match unsafe { libloading::Library::new(&sopath) } {
        Ok(l) => l,
        Err(_) => {
            bail("dlopen failed");
            return None;
        }
    };
    unsafe {
        let Ok(h) = lib.get::<*const u64>(b"trs_bir_hash") else {
            bail("no hash symbol");
            return None;
        };
        if **h != boot.hash {
            bail("hash mismatch");
            return None;
        }
        let Ok(el) = lib.get::<*const u64>(b"trs_edge_tab_len") else {
            bail("no edge table");
            return None;
        };
        let Ok(et) = lib.get::<*const usize>(b"trs_edge_tab") else {
            bail("no edge table");
            return None;
        };
        let ncomps = **el as usize;
        let edge_tab = std::slice::from_raw_parts(*et, ncomps);
        if boot.pos.iter().any(|&o| o >= ncomps || edge_tab[o] == 0) {
            bail("comp ordinal out of range");
            return None;
        }
        let (Ok(pl), Ok(pb)) = (
            lib.get::<*const u64>(b"trs_protos_len"),
            lib.get::<*const u8>(b"trs_protos"),
        ) else {
            bail("no protos table");
            return None;
        };
        let Some(protos) = abi::decode_protos(std::slice::from_raw_parts(
            *pb,
            **pl as usize,
        )) else {
            bail("corrupt protos table");
            return None;
        };
        // callback globals: foreign/prim/stdio.  BDPI globals cannot
        // exist (any foreign import is an eligibility gate).
        if let Ok(g) = lib.get::<*mut usize>(b"trs_cb_foreign") {
            **g = runcore_foreign_cb as usize;
        } else {
            bail("no foreign callback global");
            return None;
        }
        if let Ok(g) = lib.get::<*mut usize>(b"trs_cb_prim") {
            **g = runcore_prim_cb as usize;
        }
        if let Ok(g) = lib.get::<*mut usize>(b"trs_cb_stdio") {
            **g = crate::jit::jit_stdio_cb as usize;
        }
        if let Ok(g) = lib.get::<*mut usize>(b"trs_cb_sigfpe") {
            **g = crate::jit::jit_sigfpe_cb as usize;
        }
        // compiled-BRAM-tick helper (level-2 tick artifacts): pure
        // arena code in trs_codegen — the same target the classic
        // loader wires (a BRAM design calls through it every edge;
        // leaving it NULL was the first driver segfault)
        if let Ok(g) = lib.get::<*mut usize>(b"trs_bram_tick_cb") {
            **g = trs_codegen::abi::trs_bram_tick as usize;
        }
        // the artifact stays mapped for the process lifetime
        std::mem::forget(lib);
        // arena: the baked post-window image IS the boot state
        let (image, tp0, cycle0) = boot.window.as_ref().unwrap();
        let mut arena = image.clone().into_boxed_slice();
        let ap = arena.as_mut_ptr();
        // BRAM collision warnings: same hook, same registry, keyed by
        // this arena's absolute pointers
        for (slot, bits, name) in &boot.warns {
            crate::prim::bram_warn_register(
                ap.add(*slot as usize) as usize,
                name.clone(),
                *bits,
            );
        }
        let _ = abi::BRAM_WARN.set(crate::prim::bram_warn_hook);
        // native bounce servicers (rung 3b): restore every baked prim
        // seed UP FRONT and adopt its live slots — all hostile-file
        // failure modes surface here, before a byte of output, where
        // a classic boot is still a sound fallback.  The footprint
        // bound comes from the restored prim's own layout arithmetic.
        let mut prims: HashMap<usize, Box<dyn Prim>> = HashMap::new();
        for (inst, slot, tag, ws, ss) in &boot.prims {
            let Some((mut p, fp)) = crate::prim::runcore_restore(*tag, ws, ss)
            else {
                bail("prim seed unsupported or malformed");
                return None;
            };
            if slot.checked_add(fp).is_none_or(|end| end > boot.nslots) {
                bail("prim seed footprint out of range");
                return None;
            }
            p.arena_adopt(ap.add(*slot));
            prims.insert(*inst, p);
        }
        // coverage against the TRUSTED tables: every prim call site
        // the .so can reach must have a seed, or a bounce would panic
        // mid-run — bail to classic now, before any output (panel
        // finding).  Byte-integrity of the seeds themselves is trust-
        // rooted with the rest of the sidecar: the window arena IS
        // state, so a crafted sidecar could already alter bytes; the
        // checks here are against corruption and version skew.
        if protos.iter().any(|pr| {
            pr.sched_prims
                .iter()
                .chain(pr.exec_prims.iter())
                .any(|pc| !prims.contains_key(&pc.inst))
        }) {
            bail("prim call site without a baked seed");
            return None;
        }
        let mut rc = RunCore {
            fe: ForeignEnv::new(),
            strings: boot.strings,
            dyn_strs: Vec::new(),
            arg_strs: HashMap::new(),
            paths: boot.paths,
            protos,
            rng: crate::GlibcRandom::new(),
            now: 0,
            prims,
            foreign_argv: Vec::new(),
            fname_buf: String::new(),
            loc_buf: String::new(),
            arg_strs_vec: Vec::new(),
        };
        rc.fe.plusargs = plusargs.to_vec();
        // mem-file overlay (docs/RUNCORE.md, overlay rung): rewrite
        // each load region from the CURRENT file — construction
        // order, same loader, same diagnostics as a classic boot.
        // Placed after every bail: the loader may print (missing-file
        // diagnostics are output), and classic is no longer a sound
        // fallback once a byte is out.  Membership was checked at
        // parse, so the lookup cannot fail.
        for (inst, file, bin) in &boot.loads {
            rc.prims
                .get_mut(inst)
                .expect("load row without a restored prim (parse gate)")
                .runcore_overlay(file, *bin);
        }
        let envp = &mut rc as *mut RunCore as *mut core::ffi::c_void;
        let pos_fns: Vec<
            unsafe extern "C" fn(*mut u64, *mut core::ffi::c_void, u64) -> i32,
        > = boot
            .pos
            .iter()
            .map(|&o| std::mem::transmute(edge_tab[o]))
            .collect();
        if let Some(t0) = t0 {
            eprintln!(
                "trs runcore: boot {:?} ({} slots, {} comps)",
                t0.elapsed(),
                boot.nslots,
                pos_fns.len()
            );
        }
        // the central loop's steady body (advance_until), verbatim in
        // shape: cycle count, now stamp, fused posedge calls, the
        // finish/stop break BEFORE the period advance
        let period = boot.hi + boot.lo;
        let mut tp = *tp0;
        let mut cycle = *cycle0;
        while rc.fe.finished.is_none() && !rc.fe.stop_request && cycle < max_cycles
        {
            cycle += 1;
            rc.now = tp;
            for f in &pos_fns {
                f(ap, envp, tp);
            }
            if rc.fe.finished.is_some() || rc.fe.stop_request {
                break;
            }
            tp += period;
        }
        // teardown parity with run_and_release: flush stdout, leak the
        // arena (the caller exits), exit code = fataled only
        crate::out::flush();
        std::mem::forget(arena);
        Some(if rc.fe.fataled { 1 } else { 0 })
    }
}
