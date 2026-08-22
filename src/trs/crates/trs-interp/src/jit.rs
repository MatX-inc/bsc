//! Hybrid JIT (feature `jit`, runtime-gated by TRS_JIT=1): eligible
//! rules run as LLVM-compiled functions inside the interpreter's event
//! loop, over a shared u64 arena (see trs-codegen::lower).
//!
//! v1 scope — all-or-nothing: the whole design runs compiled or the
//! whole design stays interpreted.  A composition is compilable when it
//! has no early (clock-crossing) rules and every schedule node is a
//! rule (not a method) whose CF/WF cone and body lower successfully:
//! plain ≤64-bit sync registers, reset-port reads, the scalar PrimOps,
//! and $display-family statements whose arguments re-evaluate safely at
//! callback time.  VCD tracing disables the JIT (def-value recording
//! and per-prim dump hooks want the interpreted paths).

use super::*;
use std::sync::atomic::Ordering;
use std::sync::{Arc, OnceLock};

use trs_codegen::abi::{
    decode_protos, encode_protos,
    FusedComp, FusedNode,
    CompiledExec, CompiledSched, FArgSpec, FnProtos, ForeignCb, HelperMap, HelperRef,
    HelperSpec, InstEnv, PlanEnv, PrimCb, RecMeth, RuleSpec, SigfpeCb, AOT_LAYOUT_REV,
    TOKEN_KIND_EXEC,
};
#[cfg(feature = "jit")]
use trs_codegen::lower::{
    compile_design_object, compile_execs, compile_fused, compile_helpers,
    compile_helpers_object, compile_scheds, trial_lower,
};
use prim::ArenaKind;

/// TRS_PROF=1: cheap wall-time accounting of where a JIT/AOT run
/// spends its time (trampoline vs dispatch vs ticks).  Off = one
/// cached-bool branch per site.
pub(crate) mod prof {
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::sync::OnceLock;
    pub static PRIM_NS: AtomicU64 = AtomicU64::new(0);
    pub static PRIM_CALLS: AtomicU64 = AtomicU64::new(0);
    pub static FOREIGN_NS: AtomicU64 = AtomicU64::new(0);
    pub static FOREIGN_CALLS: AtomicU64 = AtomicU64::new(0);
    pub static DISPATCH_NS: AtomicU64 = AtomicU64::new(0);
    pub static TICK_NS: AtomicU64 = AtomicU64::new(0);
    /// per prim-method call counts (TRS_PROF=1)
    pub static PRIM_HIST: std::sync::Mutex<
        Option<std::collections::HashMap<String, u64>>,
    > = std::sync::Mutex::new(None);
    pub fn on() -> bool {
        static P: OnceLock<bool> = OnceLock::new();
        *P.get_or_init(|| std::env::var_os("TRS_PROF").is_some())
    }
    pub fn add(cell: &AtomicU64, t0: std::time::Instant) {
        cell.fetch_add(t0.elapsed().as_nanos() as u64, Ordering::Relaxed);
    }
    pub fn dump(total: std::time::Duration) {
        if let Some(h) = PRIM_HIST.lock().unwrap().as_ref() {
            let mut v: Vec<_> = h.iter().collect();
            v.sort_by_key(|(_, &n)| std::cmp::Reverse(n));
            for (meth, n) in v.into_iter().take(12) {
                eprintln!("trs prof:   {n:>9}  .{meth}");
            }
        }
        let g = |c: &AtomicU64| c.load(Ordering::Relaxed);
        eprintln!(
            "trs prof: total {:.3}s | dispatch {:.3}s | ticks {:.3}s | \
             prim cb {:.3}s ({} calls) | foreign cb {:.3}s ({} calls)",
            total.as_secs_f64(),
            g(&DISPATCH_NS) as f64 / 1e9,
            g(&TICK_NS) as f64 / 1e9,
            g(&PRIM_NS) as f64 / 1e9,
            g(&PRIM_CALLS),
            g(&FOREIGN_NS) as f64 / 1e9,
            g(&FOREIGN_CALLS),
        );
    }
}

/// Zero-divisor trap for compiled Quot/Rem: raise SIGFPE like the
/// interpreter (Value::quot) and native division.
pub(crate) unsafe extern "C" fn jit_sigfpe_cb() {
    libc::raise(libc::SIGFPE);
}

/// Prim-method trampoline: unmarshal per the call-site table, invoke
/// the boxed prim through the interpreter, marshal the result back.
pub(crate) unsafe extern "C" fn jit_prim_cb(
    env: *mut core::ffi::c_void,
    token: u64,
    args: *const u64,
    out: *mut u64,
) {
    let _t0 = prof::on().then(std::time::Instant::now);
    let interp = &mut *(env as *mut Interp);
    let ordinal = (token >> 17) as usize;
    let is_exec = token & TOKEN_KIND_EXEC != 0;
    let local = (token & 0xffff) as usize;
    let lz = interp.jit_shared.as_ref().expect("jit prim cb without plan").clone();
    let pc = if is_exec {
        &lz.cells[ordinal].get().expect("prim cb from uncompiled body").prim_calls[local]
    } else {
        &lz.scheds[ordinal].prim_calls[local]
    };
    let (inst, method, ret_width, is_action) =
        (pc.inst, pc.method, pc.ret_width, pc.is_action);
    // pc borrows the OWNED Arc clone, so it outlives every interp use
    // below — no arg_widths copy needed (this trampoline runs per
    // boxed prim access; a Vec clone + a Vec per argument showed as
    // the malloc traffic under TrafficBRAM's 403k calls)
    let mut argv = Vec::with_capacity(pc.arg_widths.len());
    let mut off = 0usize;
    for &w in &pc.arg_widths {
        // physical layout is w.max(1) words per argument on BOTH sides
        // of the ABI — a zero-width argument still occupies one (zero)
        // word (review finding: reading words.max(1) while advancing by
        // the logical count walked past the allocation)
        let words = ((w.max(1) as usize) + 63) / 64;
        // TRUE logical width: a zero-width prim arg must reach the prim
        // as the interp's width-0 Value (from_limb_slice masks and keeps
        // single-limb values off the heap)
        argv.push(Value::from_limb_slice(
            w,
            std::slice::from_raw_parts(args.add(off), words),
        ));
        off += words;
    }
    crate::prim::FROM_COMPILED.with(|c| c.set(token));
    if method == trs_codegen::abi::GATE_OUT_METHOD {
        // compiled Expr::Gate on a prim child: not a method — answer
        // gate_out(), the interp's exact read
        let g = match &interp.insts[inst].kind {
            InstKind::Prim(p) => p.gate_out() as u64,
            _ => 1,
        };
        *out = g;
    } else if is_action {
        interp.call_action(inst, method, &argv);
    } else {
        let v = interp.call_value(inst, method, &argv, ret_width);
        let words = ((ret_width.max(1) as usize) + 63) / 64;
        let dst = std::slice::from_raw_parts_mut(out, words);
        for (i, d) in dst.iter_mut().enumerate() {
            *d = v.limbs64().get(i).copied().unwrap_or(0);
        }
    }
    crate::prim::FROM_COMPILED.with(|c| c.set(u64::MAX));
    if let Some(t0) = _t0 {
        prof::add(&prof::PRIM_NS, t0);
        prof::PRIM_CALLS.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        // the gate sentinel is no string id — interp.s() would index OOB
        let meth = if method == trs_codegen::abi::GATE_OUT_METHOD {
            "$gate_out".to_string()
        } else {
            interp.s(method).to_string()
        };
        prof::PRIM_HIST
            .lock()
            .unwrap()
            .get_or_insert_with(Default::default)
            .entry(meth)
            .and_modify(|n| *n += 1)
            .or_insert(1);
    }
}

/// One dispatch step of a compiled composition, in entries order (rule
/// ordinals resolved through LazyJit at dispatch time).
pub(crate) enum JitNode {
    Sched(u32),
    Exec(u32),
}

/// Where a --code artifact lives: a shared object on disk, or the
/// process's own image (artifact-as-executable: the design objects
/// are linked INTO the exe with --export-dynamic, and dlopen(NULL)
/// resolves trs_snap / the edge fns from the global scope).
#[derive(Clone, Debug)]
pub enum ArtifactSource {
    Path(std::path::PathBuf),
    This,
}

impl ArtifactSource {
    pub(crate) fn open(&self) -> Result<libloading::Library, String> {
        match self {
            ArtifactSource::Path(so) => {
                // dlopen treats a bare filename as a library-search-
                // path lookup, NOT a cwd file (same fix as load_bdpi)
                let so_owned;
                let so = if so.to_str().is_some_and(|s| !s.contains('/')) {
                    so_owned = std::path::Path::new(".").join(so);
                    so_owned.as_path()
                } else {
                    so
                };
                unsafe { libloading::Library::new(so).map_err(|e| e.to_string()) }
            }
            ArtifactSource::This => {
                Ok(libloading::os::unix::Library::this().into())
            }
        }
    }
    pub(crate) fn display(&self) -> String {
        match self {
            ArtifactSource::Path(so) => so.display().to_string(),
            ArtifactSource::This => "<self>".to_string(),
        }
    }
}

/// What prime()'s planning pass should do with the compiled form:
/// JIT in-process (default), emit a persistent artifact .so (trs
/// link), or load one (trs run --code).
#[derive(Default)]
pub(crate) enum JitRequest {
    #[default]
    Run,
    Emit {
        so: std::path::PathBuf,
        exe: Option<(std::path::PathBuf, std::path::PathBuf)>,
    },
    Load {
        src: ArtifactSource,
    },
}

/// Shared compilation state: eligibility was proven by a synchronous
/// trial lowering at prime(); SCHED functions compile eagerly (they
/// run on every edge — sudoku's scheds are 4% of the IR thanks to
/// eager-slot cone sharing), and EXEC bodies fill per-rule cells on
/// background workers.  An Exec node whose cell is cold interprets the
/// body over the same arena-backed state (the interpreter's Def
/// evaluation falls through to the arena slots), so no global mode
/// flip exists.
pub(crate) struct LazyJit {
    /// owned snapshot the compile threads read (decouples lifetimes
    /// from the Interp)
    /// None when every body came preloaded from the artifact (the
    /// design is only read to compile cold cells) — skipping the
    /// O(design) clone on the pure-Load startup path.
    design: Option<Design>,
    insts: HashMap<usize, InstEnv>,
    specs: Vec<RuleSpec>,
    now_slot: u32,
    /// per-ordinal exec args: (region base index, token base) — the
    /// compiled body is shared across instances of a module type
    pub(crate) exec_args: Vec<(u64, u64)>,
    /// per-ordinal call-site tables (from trial_lower; per-ordinal even
    /// when the compiled body is shared, because prim targets differ)
    protos: Vec<FnProtos>,
    /// exec dedup classes: (representative ordinal, member ordinals)
    classes: Vec<(usize, Vec<usize>)>,
    /// outlined def-piece helpers (baked addresses; shared JIT/AOT
    /// lowering — AOT uses symbol refs at emit time instead)
    helpers: Arc<HelperMap>,
    /// eagerly compiled sched fns, one per rule ordinal
    pub(crate) scheds: Vec<CompiledSched>,
    /// batch index counter for body workers
    next_batch: std::sync::atomic::AtomicUsize,
    batch_size: usize,
    /// bodies not yet compiled (0 = fully warm: dispatch skips the
    /// latch/bridge machinery entirely)
    cold: std::sync::atomic::AtomicUsize,
    /// teardown flag: workers stop claiming batches so JitPlans::drop
    /// can join them before the model .so is dlclosed
    stop: std::sync::atomic::AtomicBool,
    cells: Vec<OnceLock<CompiledExec>>,
}

impl LazyJit {
    pub(crate) fn exec(&self, ord: usize) -> Option<&CompiledExec> {
        self.cells[ord].get()
    }

    pub(crate) fn any_cold(&self) -> bool {
        self.cold.load(Ordering::Acquire) != 0
    }

    /// No compile tier without `jit`: cells stay cold (and are never
    /// cold in practice — artifact loads pre-fill every cell, and the
    /// planner bails before spawning workers otherwise).
    #[cfg(not(feature = "jit"))]
    fn work(&self) {}

    /// Worker loop: claim CLASS batches, compile one representative
    /// per class, fill every member's cell with the shared body and
    /// its own call-site tables.
    #[cfg(feature = "jit")]
    fn work(&self) {
        loop {
            if self.stop.load(Ordering::Acquire) {
                return;
            }
            let b = self.next_batch.fetch_add(1, Ordering::AcqRel);
            let lo = b * self.batch_size;
            if lo >= self.classes.len() {
                return;
            }
            let hi = (lo + self.batch_size).min(self.classes.len());
            let env = PlanEnv {
                d: self
                    .design
                    .as_ref()
                    .expect("cold compile cell without a stashed design"),
                insts: &self.insts,
                now_slot: self.now_slot,
            };
            let reps: Vec<RuleSpec> =
                (lo..hi).map(|c| self.specs[self.classes[c].0].clone()).collect();
            let compiled = compile_execs(
                &env,
                &reps,
                Some(&self.helpers),
                jit_foreign_cb,
                jit_sigfpe_cb,
                jit_prim_cb,
            )
            .unwrap_or_else(|e| {
                // trial_lower proved eligibility at prime; only an
                // LLVM-level failure can land here
                panic!("trs jit: compile of proven-eligible bodies failed: {e}")
            });
            for (c, cr) in (lo..hi).zip(compiled) {
                for &m in &self.classes[c].1 {
                    let _ = self.cells[m].set(CompiledExec {
                        exec: cr.exec,
                        foreign_stmts: self.protos[m].exec_foreign.clone(),
                        prim_calls: self.protos[m].exec_prims.clone(),
                    });
                }
            }
            self.cold.fetch_sub(hi - lo, Ordering::AcqRel);
        }
    }
}

/// Per-comp compiled-tick lists + the covered tick-index sets (see
/// prim_tick_coverage).
pub(crate) struct TickCoverage {
    pub(crate) wire_clears: Vec<Vec<u32>>,
    pub(crate) creg_copies: Vec<Vec<(u32, u32)>>,
    pub(crate) bram_ticks: Vec<Vec<[u64; 3]>>,
    /// ticks covered by a level-1 artifact (wire clears only)
    pub(crate) covered_wire: Vec<std::collections::HashSet<usize>>,
    /// ticks covered by a level-2 artifact (wires + cregs + brams)
    pub(crate) covered_all: Vec<std::collections::HashSet<usize>>,
}

/// Compiled state carried by the Stepper.
pub(crate) struct JitPlans {
    /// the shared state arena; register prims and Interp::jit_arena_ptr
    /// hold raw pointers into this allocation (heap address is stable)
    _arena: Box<[u64]>,
    arena_ptr: *mut u64,
    /// per-composition dispatch lists (parallel to Stepper::rcomps)
    pub(crate) comp_nodes: Vec<Option<Vec<JitNode>>>,
    /// EN slots to zero before dispatching a composition (the C++
    /// schedule zeroes every enable at the top of the pass)
    pub(crate) en_slots: Vec<u32>,
    /// slot stamped with the current instant at every edge
    pub(crate) now_slot: u32,
    /// lazy compile cells (also reachable from Interp::jit_shared for
    /// the callbacks)
    pub(crate) lazy: Arc<LazyJit>,
    /// background body-compile workers: joined on drop (they execute
    /// code linked into the interactive model .so, which bluetcl
    /// dlcloses right after bk_shutdown — a still-running worker then
    /// executes unmapped code; short jit sessions crashed 5/5)
    workers: Vec<std::thread::JoinHandle<()>>,
    /// rule ordinal -> (instance, rule name, WF slot) for the
    /// interpreted-body fallback while its cell is cold
    pub(crate) exec_fallback: Vec<(usize, StrId, u32)>,
    /// per composition: rc.ticks indices whose work is compiled INTO
    /// the loaded edge fns (wire valid-bit clears) — the interp tick
    /// loop skips them when the fused fn ran, and the central-loop
    /// preconditions ignore them.  Empty unless an artifact with
    /// trs_edge_wire_ticks=1 loaded.
    pub(crate) covered_ticks: Vec<std::collections::HashSet<usize>>,
    /// the artifact's RunCore boot descriptor (sidecar v2 sections),
    /// parsed under TRS_RUNCORE_CHECK for the central-loop engage
    /// witness; None when unchecked, absent, or v1
    pub(crate) runcore_desc: Option<RunCoreDesc>,
    /// fused per-composition edge fns (task #17): compiled once all
    /// bodies are warm; 0 = composition not fused (fall back to the
    /// node walk).  fn(arena, env, now) -> i32 (nonzero = abort;
    /// $finish/$stop complete the edge and return 0).
    pub(crate) fused: std::sync::OnceLock<Vec<usize>>,
}

impl Drop for JitPlans {
    fn drop(&mut self) {
        // stop is per-BATCH: at most one in-flight class compile per
        // worker delays the join (ms-scale)
        self.lazy.stop.store(true, Ordering::Release);
        for h in self.workers.drain(..) {
            let _ = h.join();
        }
    }
}

impl JitPlans {
    /// Promote the schedule from data to code: one direct-call edge fn
    /// per composition.  Requires every body cell warm (the fused code
    /// bakes cell addresses).  Failure leaves the node walk in place.
    pub(crate) fn try_fuse(&self) {
        if std::env::var_os("TRS_NO_FUSION").is_some() {
            // still resolve the OnceLock: leaving it empty made the
            // dispatch guard re-enter here — a getenv PER EDGE for the
            // whole run, the trs/14 bug pattern (per-event-tax audit,
            // finding 1).  Zero entries = "nothing fused", the same
            // state the no-jit build uses.
            let _ = self.fused.get_or_init(|| vec![0; self.comp_nodes.len()]);
            return;
        }
        // no compile tier without `jit`: artifact-provided fused fns
        // pre-filled the cell at plan build; anything else stays on
        // the node walk
        #[cfg(not(feature = "jit"))]
        let _ = self.fused.get_or_init(|| vec![0; self.comp_nodes.len()]);
        #[cfg(feature = "jit")]
        let _ = self.fused.get_or_init(|| {
            let comps: Vec<FusedComp> = self
                .comp_nodes
                .iter()
                .map(|nodes| FusedComp {
                    en_slots: self.en_slots.clone(),
                    now_slot: self.now_slot,
                    nodes: nodes
                        .as_ref()
                        .map(|ns| {
                            ns.iter()
                                .map(|n| match *n {
                                    JitNode::Sched(o) => FusedNode::Sched(
                                        trs_codegen::abi::HelperRef::Addr(
                                            self.lazy.scheds[o as usize].sched as usize,
                                        ),
                                    ),
                                    JitNode::Exec(o) => {
                                        let (b, t) = self.lazy.exec_args[o as usize];
                                        FusedNode::Exec(
                                            trs_codegen::abi::HelperRef::Addr(
                                                self.lazy.cells[o as usize]
                                                    .get()
                                                    .expect("fuse before warm")
                                                    .exec
                                                    as usize,
                                            ),
                                            b,
                                            t,
                                        )
                                    }
                                })
                                .collect()
                        })
                        .unwrap_or_default(),
                })
                .collect();
            match compile_fused(&comps) {
                Ok(addrs) => {
                    if std::env::var_os("TRS_JIT_TRACE").is_some() {
                        eprintln!("trs jit: fused {} compositions", addrs.len());
                    }
                    addrs
                }
                Err(e) => {
                    if std::env::var_os("TRS_JIT_TRACE").is_some() {
                        eprintln!("trs jit: fusion off ({e})");
                    }
                    vec![0; self.comp_nodes.len()]
                }
            }
        });
    }
}

impl JitPlans {
    pub(crate) fn arena_ptr(&self) -> *mut u64 {
        self.arena_ptr
    }
}

/// The callback compiled code uses for foreign statements: rebuild
/// the Arg list from the call-site spec (numeric args arrive as word
/// runs, strings ride the table), dispatch through the interpreter's
/// foreign machinery, and marshal a task's result back.  A nonzero
/// return aborts the compiled edge (stop_bb) — reserved for genuine
/// aborts, never $finish/$stop.
pub(crate) unsafe extern "C" fn jit_foreign_cb(
    env: *mut core::ffi::c_void,
    token: u64,
    args: *const u64,
    out: *mut u64,
) -> i32 {
    let _t0 = prof::on().then(std::time::Instant::now);
    let interp = &mut *(env as *mut Interp);
    let ordinal = (token >> 17) as usize;
    let is_exec = token & TOKEN_KIND_EXEC != 0;
    let local = (token & 0xffff) as usize;
    let lz = interp.jit_shared.as_ref().expect("jit foreign cb without plan").clone();
    let fs = if is_exec {
        &lz.cells[ordinal].get().expect("foreign cb from uncompiled body").foreign_stmts
            [local]
    } else {
        &lz.scheds[ordinal].foreign_stmts[local]
    };
    let (inst, func, ret_width) = (fs.inst, fs.func, fs.ret_width);
    // per-Interp scratch: the argv spine survives across calls (its
    // element drops still run — Value buffers go with A3)
    let mut argv = std::mem::take(&mut interp.foreign_argv);
    argv.clear();
    argv.reserve(fs.args.len());
    let mut off = 0usize;
    for a in &fs.args {
        match *a {
            FArgSpec::Str(sid) => {
                // Arc-interned once per distinct string id: a clone is
                // a refcount bump, not a heap copy
                argv.push(Arg::Str(interp.arg_str(sid)));
            }
            FArgSpec::Num { width, signed } => {
                let w = width;
                let words = ((w.max(1) as usize) + 63) / 64;
                // the TRUE width, zero included: the formatter must see
                // the interp's width-0 Value for zero-width args, not a
                // width-1 impostor (from_limb_slice masks the buffer
                // word and keeps single-limb values off the heap)
                let limbs = std::slice::from_raw_parts(args.add(off), words);
                argv.push(Arg::Val(Value::from_limb_slice(w, limbs), signed));
                off += words;
            }
            FArgSpec::Real => {
                // one word of f64 bits -> the interp's Arg::Real, so
                // %f/%e/%g formatting is byte-identical
                let word = *args.add(off);
                argv.push(Arg::Real(f64::from_bits(word)));
                off += 1;
            }
            FArgSpec::StrDyn => {
                // one word of string id (static or runtime-interned)
                // -> the interp's Arg::Str, through the same Arc cache
                // (dyn ids are stable once interned)
                let word = *args.add(off);
                argv.push(Arg::Str(interp.arg_str(word as u32)));
                off += 1;
            }
        }
    }
    if func == trs_codegen::abi::STRING_CONCAT_FUNC {
        // compiled PrimOp::StringConcat: concatenate the resolved
        // texts and intern per evaluation, the interp's exact behavior
        // (func is a sentinel, not a string id — resolve nothing)
        let mut text = String::new();
        for a in &argv {
            if let Arg::Str(s) = a {
                text.push_str(s);
            }
        }
        let id = interp.intern_dyn(text);
        *out = id as u64;
        argv.clear();
        interp.foreign_argv = argv;
        if let Some(t0) = _t0 {
            prof::add(&prof::FOREIGN_NS, t0);
            prof::FOREIGN_CALLS
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        }
        return 0;
    }
    // task name + %m location through reused per-Interp buffers: no
    // per-call allocation (loc is "top[.path]", fname a table borrow —
    // both copied into scratch because foreign_action takes &mut self)
    let mut fname = std::mem::take(&mut interp.fname_buf);
    fname.clear();
    fname.push_str(interp.s(func));
    let mut loc = std::mem::take(&mut interp.loc_buf);
    loc.clear();
    loc.push_str("top");
    let p = &interp.insts[inst].path;
    if !p.is_empty() {
        loc.push('.');
        loc.push_str(p);
    }
    if ret_width == 0 {
        interp.foreign_action(&fname, &argv, &loc);
    } else {
        let v = interp.foreign_value(&fname, &argv, ret_width, &loc);
        let words = ((ret_width.max(1) as usize) + 63) / 64;
        let dst = std::slice::from_raw_parts_mut(out, words);
        for (i, d) in dst.iter_mut().enumerate() {
            *d = v.limbs64().get(i).copied().unwrap_or(0);
        }
    }
    // return the scratch (the task may have re-entered and taken fresh
    // buffers — mem::take left valid empties, so this only upgrades
    // capacity back)
    argv.clear();
    interp.foreign_argv = argv;
    interp.fname_buf = fname;
    interp.loc_buf = loc;
    if let Some(t0) = _t0 {
        prof::add(&prof::FOREIGN_NS, t0);
        prof::FOREIGN_CALLS.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    }
    // $finish/$stop do NOT abort compiled code: the reference runs the
    // in-flight edge (and the finishing rule's remaining statements) TO
    // COMPLETION — post-finish output is gated inside foreign_action
    // (dollar_display.cxx family) and the runtime loops stop at the
    // slice boundary.  The nonzero return -> stop_bb path is reserved
    // for genuine aborts; nothing requests one today.
    0
}

/// Body-splitting cone analysis: child classification for one module
/// type (uniform across its instances).
#[derive(Clone, Copy, PartialEq)]
pub(crate) enum ChildClass {
    Reg,
    CfgReg,
    Wire,
    Fifo { loopy: bool },
    Other,
}

/// Child resolution for the cone analyzer: an arena-backed prim, a
/// user submodule (recurse into its method cones), or opaque.
pub(crate) enum ChildRef {
    Prim(ChildClass),
    User(usize),
    Opaque,
}

/// Per-def piece statistics (see select_outlined).
#[derive(Clone)]
pub(crate) struct PieceInfo {
    pub eff: u32,
    pub outlinable: bool,
    pub stable: bool,
    pub outlined: bool,
    /// unbound data-port reads in the piece: helper parameters (v2);
    /// nonempty => no per-instant memo (value is per-call)
    pub ports: Vec<StrId>,
}

/// Bottom-up outline selection over a module's def DAG, recursing
/// through user-child value-method cones (callee Method.result/ready;
/// callee Port reads must be bound method args).  eff counts each
/// transitive non-outlined def once (SSA-memoized lowering cost);
/// outlined children are unit-cost calls.
pub(crate) struct ConeAnalyzer<'a> {
    pub d: &'a Design,
    /// (mir, child instance name) -> resolution via exemplar instances
    pub kind: &'a dyn Fn(usize, StrId) -> ChildRef,
    pub thresh: u32,
    memo: HashMap<(usize, StrId), PieceInfo>,
    reach: HashMap<(usize, StrId), std::collections::HashSet<(usize, StrId)>>,
    own: HashMap<(usize, StrId), u32>,
    seen: Vec<(usize, StrId)>,
}

impl<'a> ConeAnalyzer<'a> {
    pub fn new(d: &'a Design, kind: &'a dyn Fn(usize, StrId) -> ChildRef, thresh: u32) -> Self {
        ConeAnalyzer { d, kind, thresh, memo: HashMap::new(), reach: HashMap::new(), own: HashMap::new(), seen: Vec::new() }
    }

    pub fn module(&mut self, mir: usize) -> HashMap<StrId, PieceInfo> {
        let names: Vec<StrId> = self.d.modules[mir].defs.iter().map(|dd| dd.name).collect();
        names.iter().map(|&n| (n, self.def_piece(mir, n))).collect()
    }

    fn def_piece(&mut self, mir: usize, n: StrId) -> PieceInfo {
        if let Some(r) = self.memo.get(&(mir, n)) {
            return r.clone();
        }
        if self.seen.contains(&(mir, n)) {
            return PieceInfo { eff: 0, outlinable: false, stable: false, outlined: false, ports: Vec::new() };
        }
        let Some(di) = self.d.modules[mir].defs.iter().position(|dd| dd.name == n) else {
            return PieceInfo { eff: 0, outlinable: false, stable: false, outlined: false, ports: Vec::new() };
        };
        self.seen.push((mir, n));
        let e = self.d.modules[mir].defs[di].expr.clone();
        let mut rs = std::collections::HashSet::new();
        let mut ports = std::collections::BTreeSet::new();
        let (nodes, outl, stab) = self.walk(mir, &e, None, &mut rs, &mut ports);
        self.seen.pop();
        let mut eff = nodes;
        for k in &rs {
            eff = eff.saturating_add(*self.own.get(k).unwrap_or(&0));
        }
        self.own.insert((mir, n), nodes);
        // cap the parameter count: huge signatures cost more than the
        // split saves
        let outl = outl && ports.len() <= 8;
        let outlined = outl && eff >= self.thresh;
        if !outlined {
            rs.insert((mir, n));
            self.reach.insert((mir, n), rs);
        }
        let r = PieceInfo {
            eff,
            outlinable: outl,
            stable: stab,
            outlined,
            ports: ports.into_iter().collect(),
        };
        self.memo.insert((mir, n), r.clone());
        r
    }

    /// returns (own nodes, outlinable, stable); accumulates reached
    /// non-outlined defs into rs
    fn walk(
        &mut self,
        mir: usize,
        e: &trs_ir::Expr,
        margs: Option<&std::collections::HashSet<StrId>>,
        rs: &mut std::collections::HashSet<(usize, StrId)>,
        ports: &mut std::collections::BTreeSet<StrId>,
    ) -> (u32, bool, bool) {
        use trs_ir::Expr as E;
        let (mut nodes, mut outl, mut stab) = (1u32, true, true);
        macro_rules! sub {
            ($x:expr) => {{
                let (c, o, sb) = self.walk(mir, $x, margs, rs, ports);
                nodes = nodes.saturating_add(c);
                outl &= o;
                stab &= sb;
            }};
        }
        match e {
            E::Const { .. } | E::Str(_) | E::Real(_) => {}
            E::Port(pn) => {
                if margs.map(|a| a.contains(pn)).unwrap_or(false) {
                    // bound method arg: accounted at the call site
                } else {
                    let m = &self.d.modules[mir];
                    let is_en = m.inputs.iter().any(|q| {
                        q.name == *pn && q.kind == trs_ir::PortKind::MethodEnable
                    });
                    // data ports live in Module.inputs; METHOD ARG
                    // ports live in Method.args — both parameterize
                    let is_data = m.inputs.iter().any(|q| {
                        q.name == *pn && q.kind != trs_ir::PortKind::MethodEnable
                    }) || m
                        .methods
                        .iter()
                        .any(|me| me.args.iter().any(|q| q.name == *pn));
                    let is_reset = m
                        .resets
                        .iter()
                        .any(|_| false) // reset PORT names resolve via InstEnv; conservative below
                        ;
                    let _ = is_reset;
                    if is_en {
                        // EN slots change during dispatch
                        stab = false;
                    } else if is_data {
                        // data/method-arg port: helper parameter (v2)
                        ports.insert(*pn);
                        stab = false;
                    } else {
                        // unknown port kind (reset wires etc.): the
                        // lowering may not have a binding — taint
                        outl = false;
                        stab = false;
                    }
                }
            }
            E::Def(dn) => {
                let r = self.def_piece(mir, *dn);
                outl &= r.outlinable || r.outlined;
                stab &= r.stable;
                // a piece's port params propagate to its callers
                // (outlined callees receive them as call arguments)
                ports.extend(r.ports.iter().copied());
                if r.outlined {
                    nodes = nodes.saturating_add(1);
                } else {
                    if let Some(rr) = self.reach.get(&(mir, *dn)) {
                        rs.extend(rr.iter().cloned());
                    }
                    outl &= r.outlinable;
                }
            }
            E::MethCall { instance, method, args, .. } => {
                for a in args {
                    sub!(a);
                }
                let mname = self.d.strings[*method as usize].clone();
                match (self.kind)(mir, *instance) {
                    ChildRef::Prim(c) => {
                        let (ok, st) = match c {
                            ChildClass::Reg | ChildClass::CfgReg => {
                                (matches!(mname.as_str(), "read" | "get" | "_read"), true)
                            }
                            ChildClass::Wire => {
                                // schedule certification pending: not stable
                                (matches!(mname.as_str(), "whas" | "wget"), false)
                            }
                            ChildClass::Fifo { loopy } => match mname.as_str() {
                                // loopy i_* read LIVE elems — a
                                // same-instant deq changes them, so
                                // they never certify as stable
                                "i_notFull" | "i_notEmpty" => (true, !loopy),
                                "first" | "notFull" | "notEmpty" => (true, false),
                                _ => (false, false),
                            },
                            ChildClass::Other => (false, false),
                        };
                        outl &= ok;
                        stab &= st;
                    }
                    ChildRef::User(cmir) => {
                        let mm = self.d.modules[cmir]
                            .methods
                            .iter()
                            .find(|m| m.name == *method);
                        match mm {
                            Some(m) if m.body.is_empty() && m.result.is_some() => {
                                let aset: std::collections::HashSet<StrId> =
                                    m.args.iter().map(|p| p.name).collect();
                                let res = m.result.clone().unwrap();
                                // callee ports beyond its bound args
                                // are the CALLEE module's — v1 cannot
                                // parameterize across modules: taint
                                let mut cports = Default::default();
                                let (c, o, sb) =
                                    self.walk(cmir, &res, Some(&aset), rs, &mut cports);
                                nodes = nodes.saturating_add(c);
                                outl &= o && cports.is_empty();
                                stab &= sb;
                            }
                            Some(m) => {
                                if std::env::var_os("TRS_JIT_SPLIT_WHY").is_some() {
                                    eprintln!("why: method-with-body args={} res={}", m.body.len(), m.result.is_some());
                                }
                                outl = false;
                                stab = false;
                            }
                            None => {
                                if std::env::var_os("TRS_JIT_SPLIT_WHY").is_some() {
                                    eprintln!("why: method-not-found {}", self.d.strings[*method as usize]);
                                }
                                outl = false;
                                stab = false;
                            }
                        }
                    }
                    ChildRef::Opaque => {
                        if std::env::var_os("TRS_JIT_SPLIT_WHY").is_some() {
                            eprintln!("why: opaque-child {}", self.d.strings[*instance as usize]);
                        }
                        outl = false;
                        stab = false;
                    }
                }
            }
            E::Prim { args, .. } => {
                for a in args {
                    sub!(a);
                }
            }
            E::If { cond, then_, else_, .. } => {
                sub!(cond);
                sub!(then_);
                sub!(else_);
            }
            E::Case { scrutinee, arms, default, .. } => {
                sub!(scrutinee);
                for (_, a) in arms {
                    sub!(a);
                }
                sub!(default);
            }
            other => {
                if std::env::var_os("TRS_JIT_SPLIT_WHY").is_some() {
                    eprintln!("why: expr-kind {:?}", std::mem::discriminant(other));
                }
                outl = false;
                stab = false;
            }
        }
        (nodes, outl, stab)
    }
}

/// No compile tier without `jit`: a plan that needs freshly compiled
/// scheds cannot proceed — the caller falls back to the interpreter.
#[cfg(not(feature = "jit"))]
#[allow(clippy::too_many_arguments)]
fn aot_or_jit_scheds(
    _interp: &Interp,
    _inst_envs: &HashMap<usize, InstEnv>,
    _specs: &[RuleSpec],
    _now_slot: u32,
    _helpers: Option<&HelperMap>,
    _nworkers: usize,
    trace: bool,
) -> Option<Vec<CompiledSched>> {
    if trace {
        eprintln!("trs jit: off (no artifact and no compile tier)");
    }
    None
}

/// Eager parallel sched compile (in-process JIT path).
#[cfg(feature = "jit")]
fn aot_or_jit_scheds(
    interp: &Interp,
    inst_envs: &HashMap<usize, InstEnv>,
    specs: &[RuleSpec],
    now_slot: u32,
    helpers: Option<&HelperMap>,
    nworkers: usize,
    trace: bool,
) -> Option<Vec<CompiledSched>> {
    trs_codegen::lower::llvm_init_once();
    let t0 = std::time::Instant::now();
    let n = specs.len();
    let chunk = n.div_ceil(nworkers).max(1);
    let sched_results: Vec<_> = std::thread::scope(|sc| {
        let d = &interp.d;
        specs
            .chunks(chunk)
            .map(|c| {
                sc.spawn(move || {
                    let env = PlanEnv { d, insts: inst_envs, now_slot };
                    compile_scheds(&env, c, helpers, jit_foreign_cb, jit_sigfpe_cb, jit_prim_cb)
                })
            })
            .collect::<Vec<_>>()
            .into_iter()
            .map(|h| h.join().expect("sched compile thread"))
            .collect()
    });
    let mut scheds = Vec::with_capacity(n);
    for r in sched_results {
        match r {
            Ok(mut v) => scheds.append(&mut v),
            Err(e) => {
                if trace {
                    eprintln!("trs jit: off (sched compile: {e})");
                }
                return None;
            }
        }
    }
    if std::env::var_os("TRS_JIT_TIME").is_some() {
        eprintln!("trs jit: sched compile {:?}", t0.elapsed());
    }
    Some(scheds)
}

/// trs link: compile every rule (sched + exec) into PIC objects in
/// parallel, add the fingerprint object, and cc -shared them into the
/// artifact .so.
/// The shared-link driver: TRS_CC overrides (set by `trs link --cc`,
/// so hermetic builds pin the exact tool instead of PATH's `cc`).
fn cc_tool() -> String {
    std::env::var("TRS_CC").unwrap_or_else(|_| "cc".into())
}

/// aot_emit's failure channel: LOWERING ineligibility degrades to the
/// interp artifact (the reference always yields an executable; the
/// link CLI prints the compiled-mode-unavailable note); only
/// infrastructure failures (fs, cc, meta object) fail the link.
/// Trial lower catches most ineligibility earlier — this covers
/// shapes it does not walk (e.g. value-method reads reachable only
/// through another module's cones).
#[cfg(feature = "jit")]
/// Default measured edge-fn budget, instructions: safely under the
/// pipeline tier's 20k straight-line cap, so a budgeted edge keeps
/// default<O3>.  TRS_EDGE_INSN_BUDGET overrides; 0 disables.
const EDGE_INSN_BUDGET: u64 = 16_000;

enum EmitFail {
    Ineligible(String),
    Infra(String),
    /// an edge fn exceeded the instruction budget: the MEASURED
    /// oversized inlined sections (ordinals) — the caller extends the
    /// plan's outlined set and re-emits (see EDGE_INSN_BUDGET)
    EdgeOverBudget(std::collections::HashSet<usize>),
}

#[cfg(feature = "jit")]
#[allow(clippy::too_many_arguments)]
fn aot_emit(
    d: &Design,
    inst_envs: &HashMap<usize, InstEnv>,
    specs: &[RuleSpec],
    now_slot: u32,
    classes: &[(usize, Vec<usize>)],
    helper_specs: &[HelperSpec],
    refs_sym: &HelperMap,
    split_thresh: u32,
    protos: &[FnProtos],
    comp_nodes: &[Option<Vec<JitNode>>],
    en_slots: &[u32],
    so: &std::path::Path,
    exe: Option<&(std::path::PathBuf, std::path::PathBuf)>,
    bir_hash: u64,
    bir_hash_raw: u64,
    plan_a: &[u8],
    plan_b: &[u8],
    edge_plan: Option<&trs_codegen::abi::EdgeSsaPlan>,
    bdpi_names: &[String],
    // largest tolerated edge-fn size, instructions (0 = unbounded);
    // exceeding it returns EmitFail::EdgeOverBudget with measured
    // victims for the caller's replan (one_module + edge-SSA only)
    edge_insn_budget: u64,
) -> Result<(), EmitFail> {
    use trs_codegen::lower::{compile_meta_object, compile_object_chunk};
    trs_codegen::lower::llvm_init_once();
    let t0 = std::time::Instant::now();
    let nworkers = jit_workers(specs.len());
    let chunk = specs.len().div_ceil(nworkers).max(1);
    // sched functions per ordinal; exec bodies once per dedup class
    let reps: Vec<RuleSpec> =
        classes.iter().map(|(rep, _)| specs[*rep].clone()).collect();
    let rchunk = reps.len().div_ceil(nworkers).max(1);
    // whole-edge inlining (task #18): one module, one pipeline run —
    // the inliner flattens cheap scheds/helpers into the fused edges.
    // TRS_AOT_ONE_MODULE=0 restores parallel chunked emission.
    let one_module = std::env::var("TRS_AOT_ONE_MODULE").as_deref() != Ok("0");
    if one_module {
        let mut rep_of: Vec<usize> = vec![0; specs.len()];
        for (rep, members) in classes {
            for &m in members {
                rep_of[m] = *rep;
            }
        }
        // a class rep is needed only if some member's composition is
        // not covered by an SSA edge fn (covered rules run inline in
        // the edge; their standalone symbols would double the LLVM
        // mass — the loader stubs the elided ones)
        // EXEC coverage only: an ordinal is exec-covered iff its body
        // lowers INLINE in an edge fn (sched nodes don't count — that
        // was the bug that dropped every rep once outlined calls
        // started referencing them; pre-dial artifacts were fully
        // inlined so nothing noticed)
        let covered: std::collections::HashSet<usize> = edge_plan
            .map(|p| {
                p.nodes
                    .iter()
                    .flatten()
                    .filter(|&&(is_exec, o)| {
                        is_exec && !p.outlined_execs.contains(&o)
                    })
                    .map(|&(_, o)| o)
                    .collect()
            })
            .unwrap_or_default();
        let rep_ords: Vec<usize> = classes
            .iter()
            .filter(|(_, members)| {
                members.iter().any(|m| !covered.contains(m))
            })
            .map(|(r, _)| *r)
            .collect();
        let comps: Vec<FusedComp> = comp_nodes
            .iter()
            .map(|nodes| FusedComp {
                en_slots: en_slots.to_vec(),
                now_slot,
                nodes: nodes
                    .as_ref()
                    .map(|ns| {
                        ns.iter()
                            .map(|n| match *n {
                                JitNode::Sched(o) => FusedNode::Sched(HelperRef::Sym(
                                    format!("sched_{}", specs[o as usize].label),
                                )),
                                JitNode::Exec(o) => {
                                    let sp = &specs[o as usize];
                                    FusedNode::Exec(
                                        HelperRef::Sym(format!(
                                            "exec_{}",
                                            specs[rep_of[o as usize]].label
                                        )),
                                        inst_envs[&sp.inst].region.0 as u64,
                                        sp.token_base,
                                    )
                                }
                            })
                            .collect()
                    })
                    .unwrap_or_default(),
            })
            .collect();
        let env = PlanEnv { d, insts: inst_envs, now_slot };
        let _g = trs_codegen::abi::AotModeGuard::set();
        let t1 = std::time::Instant::now();
        let obj = match compile_design_object(
            &env,
            specs,
            &rep_ords,
            helper_specs,
            refs_sym,
            &comps,
            edge_plan,
            edge_insn_budget,
        )
        .map_err(|e| EmitFail::Ineligible(format!("design object: {e}")))?
        {
            trs_codegen::lower::DesignObject::Object(o) => o,
            trs_codegen::lower::DesignObject::EdgeOverBudget(sizes) => {
                // pick MEASURED victims: per over-budget comp, largest
                // inlined sections first until the comp fits
                let mut victims: std::collections::HashSet<usize> =
                    std::collections::HashSet::new();
                for comp in &sizes {
                    let mut total: u64 = comp.iter().map(|&(_, i)| i).sum();
                    if total <= edge_insn_budget {
                        continue;
                    }
                    let mut by_size: Vec<(u64, usize)> =
                        comp.iter().map(|&(o, i)| (i, o)).collect();
                    by_size.sort_unstable_by(|a, b| b.cmp(a));
                    for &(i, o) in &by_size {
                        if total <= edge_insn_budget {
                            break;
                        }
                        victims.insert(o);
                        total -= i;
                    }
                }
                return Err(EmitFail::EdgeOverBudget(victims));
            }
        };
        if std::env::var_os("TRS_JIT_TIME").is_some() {
            eprintln!("trs aot: one-module compile {:?}", t1.elapsed());
        }
        if std::env::var_os("TRS_EDGE_SSA_STATS").is_some() {
            let s = trs_codegen::abi::edge_ssa_sites();
            eprintln!(
                "trs edge-ssa census: fire-signal loads={} eager-reloads(exec)={} \
                 shared-reloads(sched)={} eager-stores={} promotable-load-words={}",
                s[0], s[1], s[2], s[3], s[4]
            );
        }
        let tmp =
            std::env::temp_dir().join(format!("trs-link-{}", std::process::id()));
        std::fs::create_dir_all(&tmp).map_err(|e| EmitFail::Infra(e.to_string()))?;
        let f = tmp.join("design.o");
        std::fs::write(&f, obj).map_err(|e| EmitFail::Infra(e.to_string()))?;
        let meta = compile_meta_object(
            bir_hash,
            bir_hash_raw,
            split_thresh as u64,
            &encode_protos(protos),
            edge_plan
                .map(|p| {
                    let any = |vs: &[Vec<u32>]| vs.iter().any(|v| !v.is_empty());
                    if any(&p.wire_clears)
                        || p.creg_copies.iter().any(|v| !v.is_empty())
                        || p.bram_ticks.iter().any(|v| !v.is_empty())
                    {
                        2
                    } else {
                        0
                    }
                })
                .unwrap_or(0),
            bdpi_names,
            &d.snap_encode(bir_hash).unwrap_or_default(),
            plan_a,
            plan_b,
        )
        .map_err(|e| EmitFail::Infra(format!("meta object: {e}")))?;
        let mf = tmp.join("meta.o");
        std::fs::write(&mf, meta).map_err(|e| EmitFail::Infra(e.to_string()))?;
        // temp+rename: a crash mid-cc must never leave a truncated
        // .so at the final path (it would dlopen-fail or worse on the
        // next run before the gates can judge it)
        let so_tmp = so.with_extension("so.tmp");
        // -Bsymbolic-functions: the artifact calls its OWN exec/sched
        // fns, but their symbols must stay dynamically exported (the
        // table-less loader path dlsyms them) — without local binding
        // every intra-.so call pays a PLT stub + GOT load (FloatTest:
        // 415 call sites, 5.5M indirect branches/run).  Function-only
        // binding keeps the callback DATA globals (trs_cb_*) on their
        // normal GOT path, which the loader writes through dlsym.
        let st = std::process::Command::new(cc_tool())
            .args(["-shared", "-Wl,-Bsymbolic-functions", "-o"])
            .arg(&so_tmp)
            .args([&f, &mf])
            .status()
            .map_err(|e| EmitFail::Infra(format!("cc: {e}")))?;
        if !st.success() {
            std::fs::remove_dir_all(&tmp).ok();
            std::fs::remove_file(&so_tmp).ok();
            return Err(EmitFail::Infra("cc -shared failed".into()));
        }
        std::fs::rename(&so_tmp, so)
            .map_err(|e| EmitFail::Infra(format!("rename .so: {e}")))?;
        if let Some((exe_out, libdir)) = exe {
            // artifact-as-executable: the SAME objects, plus a 3-line
            // main shim, linked as a PIE with --export-dynamic so the
            // runtime (via trs_run_main) resolves trs_snap and the
            // edge fns from our own image.  Prefer the slim LLVM-free
            // runtime (libtrs_rt.so): the full capi lib carries
            // statically-linked LLVM whose constructors cost ~5ms at
            // every exec of the produced binary.
            let rt = if libdir.join("libtrs_rt.so").exists() {
                "-l:libtrs_rt.so"
            } else {
                "-l:libtrs_capi.so"
            };
            let mc = tmp.join("trs_main.c");
            std::fs::write(
                &mc,
                "extern int trs_run_main(int argc, char** argv);\n                 int main(int argc, char** argv)                  { return trs_run_main(argc, argv); }\n",
            )
            .map_err(|e| EmitFail::Infra(e.to_string()))?;
            let exe_tmp = exe_out.with_extension("exe.tmp");
            let st = std::process::Command::new(cc_tool())
                .arg(&mc)
                .args([&f, &mf])
                .arg("-Wl,--export-dynamic")
                .arg("-Wl,--no-as-needed")
                .arg(format!("-L{}", libdir.display()))
                .arg(rt)
                .arg(format!("-Wl,-rpath,{}", libdir.display()))
                .args(["-o"])
                .arg(&exe_tmp)
                .status()
                .map_err(|e| EmitFail::Infra(format!("cc exe: {e}")))?;
            if !st.success() {
                std::fs::remove_dir_all(&tmp).ok();
                std::fs::remove_file(&exe_tmp).ok();
                return Err(EmitFail::Infra("cc exe link failed".into()));
            }
            std::fs::rename(&exe_tmp, exe_out)
                .map_err(|e| EmitFail::Infra(format!("rename exe: {e}")))?;
        }
        std::fs::remove_dir_all(&tmp).ok();
        if std::env::var_os("TRS_JIT_TIME").is_some() {
            eprintln!("trs aot: emit + link {:?}", t0.elapsed());
        }
        return Ok(());
    }
    if exe.is_some() {
        return Err(EmitFail::Infra(
            "--exe requires the one-module AOT path (TRS_AOT_ONE_MODULE=0 unsupported)".into(),
        ));
    }
    // helpers are best-effort in AOT exactly as in JIT: if their
    // object fails to compile, drop them and link the design unsplit
    // rather than failing the artifact
    let mut helpers_on = !helper_specs.is_empty();
    let mut helper_obj: Option<Vec<u8>> = None;
    if helpers_on {
        let _g = trs_codegen::abi::AotModeGuard::set();
        let env = PlanEnv { d, insts: inst_envs, now_slot };
        let pseudo = specs[0].clone();
        match compile_helpers_object(&env, helper_specs, refs_sym, &pseudo) {
            Ok(o) => helper_obj = Some(o),
            Err(e) => {
                eprintln!(
                    "trs link: note: split helpers disabled for this design ({e})"
                );
                helpers_on = false;
            }
        }
    }
    let objs: Vec<Result<Vec<u8>, _>> = std::thread::scope(|sc| {
        let mut handles = Vec::new();
        for c in specs.chunks(chunk) {
            handles.push(sc.spawn(move || {
                let _g = trs_codegen::abi::AotModeGuard::set();
                let env = PlanEnv { d, insts: inst_envs, now_slot };
                compile_object_chunk(&env, c, helpers_on.then_some(refs_sym), true, false)
            }));
        }
        for c in reps.chunks(rchunk) {
            handles.push(sc.spawn(move || {
                let _g = trs_codegen::abi::AotModeGuard::set();
                let env = PlanEnv { d, insts: inst_envs, now_slot };
                compile_object_chunk(&env, c, helpers_on.then_some(refs_sym), false, true)
            }));
        }
        handles
            .into_iter()
            .map(|h| h.join().expect("aot compile thread"))
            .collect()
    });
    let helper_obj = helpers_on.then_some(helper_obj).flatten();
    let tmp = std::env::temp_dir().join(format!("trs-link-{}", std::process::id()));
    std::fs::create_dir_all(&tmp).map_err(|e| EmitFail::Infra(e.to_string()))?;
    let mut files = Vec::new();
    for (i, o) in objs.into_iter().enumerate() {
        let bytes = o.map_err(|e| EmitFail::Ineligible(format!("object compile: {e}")))?;
        let f = tmp.join(format!("chunk{i}.o"));
        std::fs::write(&f, bytes).map_err(|e| EmitFail::Infra(e.to_string()))?;
        files.push(f);
    }
    if let Some(o) = helper_obj {
        let f = tmp.join("helpers.o");
        std::fs::write(&f, o).map_err(|e| EmitFail::Infra(e.to_string()))?;
        files.push(f);
    }
    // fused per-composition edge fns (task #17): symbol callees, ld
    // resolves inside the .so; exec callees use the dedup class rep
    {
        let mut rep_of: Vec<usize> = vec![0; specs.len()];
        for (rep, members) in classes {
            for &m in members {
                rep_of[m] = *rep;
            }
        }
        let comps: Vec<trs_codegen::lower::FusedComp> = comp_nodes
            .iter()
            .map(|nodes| trs_codegen::lower::FusedComp {
                en_slots: en_slots.to_vec(),
                now_slot,
                nodes: nodes
                    .as_ref()
                    .map(|ns| {
                        ns.iter()
                            .map(|n| match *n {
                                JitNode::Sched(o) => {
                                    trs_codegen::lower::FusedNode::Sched(
                                        HelperRef::Sym(format!(
                                            "sched_{}",
                                            specs[o as usize].label
                                        )),
                                    )
                                }
                                JitNode::Exec(o) => {
                                    let sp = &specs[o as usize];
                                    trs_codegen::lower::FusedNode::Exec(
                                        HelperRef::Sym(format!(
                                            "exec_{}",
                                            specs[rep_of[o as usize]].label
                                        )),
                                        inst_envs[&sp.inst].region.0 as u64,
                                        sp.token_base,
                                    )
                                }
                            })
                            .collect()
                    })
                    .unwrap_or_default(),
            })
            .collect();
        let o = trs_codegen::lower::compile_fused_object(&comps)
            .map_err(|e| EmitFail::Ineligible(format!("fused object: {e}")))?;
        let f = tmp.join("fused.o");
        std::fs::write(&f, o).map_err(|e| EmitFail::Infra(e.to_string()))?;
        files.push(f);
    }
    let meta = compile_meta_object(
        bir_hash,
        bir_hash_raw,
        split_thresh as u64,
        &encode_protos(protos),
        0, // chunked path never carries edge-SSA compiled ticks
        bdpi_names,
        &d.snap_encode(bir_hash).unwrap_or_default(),
        plan_a,
        plan_b,
    )
    .map_err(|e| EmitFail::Infra(format!("meta object: {e}")))?;
    let mf = tmp.join("meta.o");
    std::fs::write(&mf, meta).map_err(|e| EmitFail::Infra(e.to_string()))?;
    files.push(mf);
    // temp+rename, same discipline as the single-object emit; local
    // function binding for the same reason (see the one-module link)
    let so_tmp = so.with_extension("so.tmp");
    let st = std::process::Command::new("cc")
        .args(["-shared", "-Wl,-Bsymbolic-functions", "-o"])
        .arg(&so_tmp)
        .args(&files)
        .status()
        .map_err(|e| EmitFail::Infra(format!("cc: {e}")))?;
    std::fs::remove_dir_all(&tmp).ok();
    if !st.success() {
        std::fs::remove_file(&so_tmp).ok();
        return Err(EmitFail::Infra("cc -shared failed".into()));
    }
    std::fs::rename(&so_tmp, so)
        .map_err(|e| EmitFail::Infra(format!("rename .so: {e}")))?;
    if std::env::var_os("TRS_JIT_TIME").is_some() {
        eprintln!("trs aot: emit + link {:?}", t0.elapsed());
    }
    Ok(())
}

/// Baked PlanA from the artifact (trs_plan_a): gated on the baked
/// bir hash matching the interp's (same salted expression aot_load
/// checks — a mismatched artifact must fail BOTH ways together so the
/// fallback derives a plan consistent with the in-process compile),
/// the layout rev, and the PlanA blob version.  Any miss = None =
/// fresh derivation.
pub(crate) fn aot_plan_a(
    src: &ArtifactSource,
    expected_raw: u64,
) -> Option<crate::PlanA> {
    unsafe {
        let lib = src.open().ok()?;
        let hr: libloading::Symbol<*const u64> =
            lib.get(b"trs_bir_hash_raw").ok()?;
        if **hr != expected_raw {
            return None;
        }
        let l: libloading::Symbol<*const u64> = lib.get(b"trs_plan_a_len").ok()?;
        let len = **l as usize;
        if len == 0 {
            return None;
        }
        let r: libloading::Symbol<*const u64> = lib.get(b"trs_layout_rev").ok()?;
        if **r != trs_codegen::abi::AOT_LAYOUT_REV {
            return None;
        }
        let s: libloading::Symbol<*const u8> = lib.get(b"trs_plan_a").ok()?;
        let bytes = std::slice::from_raw_parts(*s, len);
        let plan: crate::PlanA = bincode::deserialize(bytes).ok()?;
        (plan_a_version(&plan) == crate::PLAN_A_VERSION).then_some(plan)
    }
}

fn plan_a_version(p: &crate::PlanA) -> u32 {
    p.version
}

/// The expensive-to-derive fraction of jit_plan, baked into artifacts
/// as trs_plan_b: per-ordinal always-fire bits (deriving them walks
/// WILL_FIRE def aliases and forces lazy expr decodes) and the exec
/// dedup classes (deriving them hashes every instance's slot layout).
/// The specs themselves re-derive at load — measured, that's plain
/// compute, and shipping them costs more in decode allocations than
/// the derivation.  Slot-layout consumers depend on trace mode
/// (recording slots shift the layout), so unlike PlanA this gates on
/// the SALTED hash — the same expression aot_load checks — plus the
/// layout rev and the blob version.
#[derive(serde::Serialize, serde::Deserialize)]
pub(crate) struct PlanB {
    version: u32,
    always_fire: Vec<u8>,
    class_rep: Vec<u64>,
    class_members: Vec<u64>,
    class_off: Vec<u32>,
}

pub(crate) const PLAN_B_VERSION: u32 = 3;

pub(crate) fn plan_b_encode(
    specs: &[RuleSpec],
    classes: &[(usize, Vec<usize>)],
) -> Vec<u8> {
    let mut class_members = Vec::new();
    let mut class_off = vec![0u32];
    for (_, m) in classes {
        class_members.extend(m.iter().map(|&x| x as u64));
        class_off.push(class_members.len() as u32);
    }
    let wire = PlanB {
        version: PLAN_B_VERSION,
        always_fire: specs.iter().map(|s| s.always_fire as u8).collect(),
        class_rep: classes.iter().map(|(r, _)| *r as u64).collect(),
        class_members,
        class_off,
    };
    bincode::serialize(&wire).unwrap_or_default()
}

pub(crate) fn aot_plan_b(
    src: &ArtifactSource,
    expected_salted: u64,
) -> Option<(Vec<u8>, Vec<(usize, Vec<usize>)>)> {
    let wire: PlanB = unsafe {
        let lib = src.open().ok()?;
        let h: libloading::Symbol<*const u64> = lib.get(b"trs_bir_hash").ok()?;
        if **h != expected_salted {
            return None;
        }
        let l: libloading::Symbol<*const u64> = lib.get(b"trs_plan_b_len").ok()?;
        let len = **l as usize;
        if len == 0 {
            return None;
        }
        let r: libloading::Symbol<*const u64> = lib.get(b"trs_layout_rev").ok()?;
        if **r != trs_codegen::abi::AOT_LAYOUT_REV {
            return None;
        }
        let s: libloading::Symbol<*const u8> = lib.get(b"trs_plan_b").ok()?;
        let bytes = std::slice::from_raw_parts(*s, len);
        bincode::deserialize(bytes).ok()?
    };
    if wire.version != PLAN_B_VERSION {
        return None;
    }
    let classes: Vec<(usize, Vec<usize>)> = (0..wire.class_rep.len())
        .map(|c| {
            (
                wire.class_rep[c] as usize,
                wire.class_members
                    [wire.class_off[c] as usize..wire.class_off[c + 1] as usize]
                    .iter()
                    .map(|&x| x as usize)
                    .collect(),
            )
        })
        .collect();
    Some((wire.always_fire, classes))
}

/// Full-AOT load: the design snapshot embedded in the artifact
/// (trs_snap + trs_bir_hash), so a --code run never opens the .bir.
/// None = pre-snap artifact, empty snap (encode failed at link), a
/// missing/unloadable .so, or a snap-gate failure — the caller falls
/// back to the .bir path and the normal fingerprint cross-check.
pub(crate) fn aot_embedded_design(
    src: &ArtifactSource,
) -> Option<(u64, trs_ir::Design)> {
    unsafe {
        let lib = src.open().ok()?;
        // the RAW design identity: trs_bir_hash is trace-salted (it
        // belongs to aot_load's mode gate) and would corrupt
        // interp.bir_hash for traced artifacts.  Artifacts that carry
        // a snap always carry the raw hash too (same commit).
        let h: libloading::Symbol<*const u64> =
            lib.get(b"trs_bir_hash_raw").ok()?;
        let hash = **h;
        let l: libloading::Symbol<*const u64> = lib.get(b"trs_snap_len").ok()?;
        let len = **l as usize;
        if len == 0 {
            return None;
        }
        let s: libloading::Symbol<*const u8> = lib.get(b"trs_snap").ok()?;
        let bytes = std::slice::from_raw_parts(*s, len);
        let d = trs_ir::Design::snap_decode_embedded(bytes, hash)?;
        Some((hash, d))
    }
}

/// trs run --code: dlopen the artifact, verify its fingerprint, fill
/// the callback pointer-globals, and resolve every rule's sched/exec
/// function.  Any failure falls back to in-process compilation.
#[allow(clippy::type_complexity)]
/// aot_load's marker error for an artifact compiled for the opposite
/// trace mode: current, not stale — the fallback recompile is silent.
const TRACE_MODE_MISMATCH: &str =
    "artifact trace mode differs from this run; compiling in-process";

fn aot_load(
    src: &ArtifactSource,
    bir_hash: u64,
    specs: &[RuleSpec],
    classes: &[(usize, Vec<usize>)],
    split_thresh: u32,
    ncomps: usize,
    bdpi_fill: &[(String, usize)],
) -> Result<
    (Vec<CompiledSched>, Vec<CompiledExec>, Vec<FnProtos>, Vec<usize>, u64),
    String,
> {
    unsafe {
        let lib = src.open()?;
        let h: libloading::Symbol<*const u64> =
            lib.get(b"trs_bir_hash").map_err(|e| e.to_string())?;
        if **h != bir_hash {
            // the OTHER trace mode's salt matching means the artifact is
            // current but compiled for the opposite dumping mode — an
            // expected, by-design in-process recompile, not staleness
            if **h == bir_hash ^ 0x5452_4143_4544 {
                return Err(TRACE_MODE_MISMATCH.into());
            }
            return Err("BIR fingerprint mismatch (stale artifact)".into());
        }
        let r: libloading::Symbol<*const u64> =
            lib.get(b"trs_layout_rev").map_err(|e| e.to_string())?;
        if **r != AOT_LAYOUT_REV {
            return Err(format!(
                "layout revision {} (this trs expects {AOT_LAYOUT_REV})",
                **r
            ));
        }
        let t: libloading::Symbol<*const u64> =
            lib.get(b"trs_split_thresh").map_err(|e| e.to_string())?;
        if **t != split_thresh as u64 {
            return Err(format!(
                "split threshold {} but this run plans with {split_thresh} \
                 (arena layouts differ)",
                **t
            ));
        }
        for (name, addr) in [
            (&b"trs_cb_foreign"[..], jit_foreign_cb as ForeignCb as usize),
            (&b"trs_cb_sigfpe"[..], jit_sigfpe_cb as SigfpeCb as usize),
            (&b"trs_cb_prim"[..], jit_prim_cb as PrimCb as usize),
        ] {
            let g: libloading::Symbol<*mut usize> =
                lib.get(name).map_err(|e| e.to_string())?;
            **g = addr;
        }
        // compiled-BRAM-tick helper (level-2 tick artifacts)
        if let Ok(g) = lib.get::<*mut usize>(b"trs_bram_tick_cb") {
            **g = trs_codegen::abi::trs_bram_tick as usize;
        }
        let pl: libloading::Symbol<*const u64> =
            lib.get(b"trs_protos_len").map_err(|e| e.to_string())?;
        let pg: libloading::Symbol<*const u8> =
            lib.get(b"trs_protos").map_err(|e| e.to_string())?;
        let pbytes = std::slice::from_raw_parts(*pg, **pl as usize);
        let protos = decode_protos(pbytes)
            .ok_or("corrupt trs_protos table")?;
        if protos.len() != specs.len() {
            return Err("protos count mismatch".into());
        }
        // edge-SSA artifacts elide standalone symbols for rules that
        // run inline in an edge fn; the token TABLES stay per-ordinal
        // (edge callbacks resolve through them).  A stub keeps the
        // types simple and fails LOUDLY if a supposedly-dead path runs.
        unsafe extern "C" fn missing_sched(_: *mut u64, _: *mut core::ffi::c_void) {
            panic!("trs: sched symbol elided by edge-SSA artifact was called");
        }
        unsafe extern "C" fn missing_exec(
            _: *mut u64,
            _: *mut core::ffi::c_void,
            _: u64,
            _: u64,
        ) -> i32 {
            panic!("trs: exec symbol elided by edge-SSA artifact was called");
        }
        // ordinal-indexed fn tables (one_module artifacts): 3 dlsyms
        // instead of ~one per rule.  Null entry = elided symbol.
        // Absent or size-mismatched tables (chunked artifacts) fall
        // back to the per-symbol path.
        let tab = |name: &[u8], len_name: &[u8], want: usize| -> Option<&[usize]> {
            let l = lib.get::<*const u64>(len_name).ok()?;
            let t = lib.get::<*const usize>(name).ok()?;
            (**l as usize == want)
                .then(|| std::slice::from_raw_parts(*t, want))
        };
        let sched_tab = tab(b"trs_sched_tab", b"trs_sched_tab_len", specs.len());
        let exec_tab = tab(b"trs_exec_tab", b"trs_exec_tab_len", specs.len());
        let edge_tab = tab(b"trs_edge_tab", b"trs_edge_tab_len", ncomps);
        let mut scheds = Vec::with_capacity(specs.len());
        for (o, (spec, proto)) in specs.iter().zip(protos.iter()).enumerate() {
            let sf = match sched_tab {
                Some(t) if t[o] != 0 => std::mem::transmute::<
                    usize,
                    unsafe extern "C" fn(*mut u64, *mut core::ffi::c_void),
                >(t[o]),
                Some(_) => missing_sched,
                None => lib
                    .get::<unsafe extern "C" fn(*mut u64, *mut core::ffi::c_void)>(
                        format!("sched_{}\0", spec.label).as_bytes(),
                    )
                    .map(|f| *f)
                    .unwrap_or(missing_sched),
            };
            scheds.push(CompiledSched {
                sched: sf,
                foreign_stmts: proto.sched_foreign.clone(),
                prim_calls: proto.sched_prims.clone(),
            });
        }
        // exec bodies: one symbol per dedup class, shared by members
        let mut execs: Vec<Option<CompiledExec>> =
            (0..specs.len()).map(|_| None).collect();
        for (rep, members) in classes {
            let ef = match exec_tab {
                Some(t) if t[*rep] != 0 => std::mem::transmute::<
                    usize,
                    unsafe extern "C" fn(
                        *mut u64,
                        *mut core::ffi::c_void,
                        u64,
                        u64,
                    ) -> i32,
                >(t[*rep]),
                Some(_) => missing_exec,
                None => lib
                    .get::<unsafe extern "C" fn(
                        *mut u64,
                        *mut core::ffi::c_void,
                        u64,
                        u64,
                    ) -> i32>(
                        format!("exec_{}\0", specs[*rep].label).as_bytes(),
                    )
                    .map(|f| *f)
                    .unwrap_or(missing_exec),
            };
            for &m in members {
                execs[m] = Some(CompiledExec {
                    exec: ef,
                    foreign_stmts: protos[m].exec_foreign.clone(),
                    prim_calls: protos[m].exec_prims.clone(),
                });
            }
        }
        let execs: Vec<CompiledExec> = execs
            .into_iter()
            .map(|o| o.expect("every ordinal belongs to a class"))
            .collect();
        // fused edge fns (absent in pre-fusion artifacts: rev-gated)
        let mut fused = Vec::with_capacity(ncomps);
        for k in 0..ncomps {
            let ef = match edge_tab {
                Some(t) if t[k] != 0 => t[k],
                Some(_) => return Err(format!("edge_c{k}: null table entry")),
                None => *lib
                    .get::<unsafe extern "C" fn(
                        *mut u64,
                        *mut core::ffi::c_void,
                        u64,
                    ) -> i32>(format!("edge_c{k}\0").as_bytes())
                    .map_err(|e| e.to_string())? as usize,
            };
            fused.push(ef);
        }
        // stdio-flush + direct-BDPI callee globals (all optional:
        // absent in old or BDPI-free artifacts)
        if let Ok(g) = lib.get::<*mut usize>(b"trs_cb_stdio") {
            unsafe { **g = jit_stdio_cb as usize };
        }
        for (gname, addr) in bdpi_fill {
            if let Ok(g) = lib.get::<*mut usize>(gname.as_bytes()) {
                unsafe { **g = *addr };
            }
        }
        // edge fns carry compiled ticks (absent symbol = old artifact
        // = 0; 1 = wire clears only, 2 = wires + cregs + brams)
        let tick_level = lib
            .get::<*const u64>(b"trs_edge_wire_ticks")
            .map(|g| unsafe { **g })
            .unwrap_or(0);
        // the artifact stays mapped for the process lifetime
        std::mem::forget(lib);
        Ok((scheds, execs, protos, fused, tick_level))
    }
}

/// Stage-A capture for the RunCore boot descriptor: what only the
/// Emit arm can see.  prime's runcore_desc_finish consumes it.
pub(crate) struct RunCoreStageA {
    /// per-comp rc.ticks indices the emitted edge fns cover
    pub(crate) covered: Vec<std::collections::HashSet<usize>>,
    /// edge-SSA emission was on (off = no compiled ticks = central
    /// loop never engages = ineligible)
    pub(crate) edge_ssa: bool,
    /// any prim stayed boxed (no arena slot): its window/steady state
    /// lives in structs a RunCore boot doesn't have — ineligible
    /// until lazy Reflect (adversarial-panel finding)
    pub(crate) boxed: bool,
    /// per bounce-reachable prim inst (any inst named by a sched or
    /// exec PrimCallSpec): (inst, slot, tag, words, strings) seed rows
    /// for the sidecar PRIMS section — the boot restores exactly these
    /// prims, adopts their live slots, and services bounces natively
    /// (rung 3b; the rows replace 3a's blanket prim-site gate).
    /// Err = why native servicing is impossible (unattached target,
    /// unseedable kind) — the design then boots classic.
    #[allow(clippy::type_complexity)]
    pub(crate) prim_rows:
        Result<Vec<(u64, u64, u64, Vec<u64>, Vec<String>)>, String>,
    /// BRAM warn registry rows keyed back to relative arena slots:
    /// (slot, addr_bits, full name)
    pub(crate) warns: Vec<(u64, u32, String)>,
    /// mem-file loads (overlay rung): (inst, file, binary_format) in
    /// construction order; every load inst is also in prim_rows
    pub(crate) load_rows: Vec<(u64, String, u64)>,
}

/// Parsed sidecar-v2 boot descriptor (clock + comp order +
/// eligibility), stashed in JitPlans for the engage-time witness and,
/// later, consumed by the RunCore boot driver.
#[derive(PartialEq, Debug)]
pub(crate) struct RunCoreDesc {
    pub(crate) hi: u64,
    pub(crate) lo: u64,
    pub(crate) delay: u64,
    pub(crate) init_high: bool,
    pub(crate) has_init: bool,
    pub(crate) pos: Vec<usize>,
    pub(crate) neg: Vec<usize>,
    /// RunCore boot eligibility (central + the boot-only gates)
    pub(crate) eligible: bool,
    /// central-loop mirror only — what the engage witness compares
    pub(crate) central: bool,
    pub(crate) reason: String,
    /// window-bake sections, when the link's post-emit bake found the
    /// reset window skippable: (post-window arena, tp, tn, cycle)
    pub(crate) window: Option<(Vec<u64>, u64, u64, u64)>,
}

// Boot-descriptor section tags (sidecar v2, after the RLE runs:
// b"TRSBOOTD", u64 section count, then per section u64 tag + u64
// payload byte length + payload padded to 8).
pub(crate) const RC_SEC_STRINGS: u64 = 1;
pub(crate) const RC_SEC_PATHS: u64 = 2;
pub(crate) const RC_SEC_CLOCK: u64 = 3;
pub(crate) const RC_SEC_COMPS: u64 = 4;
pub(crate) const RC_SEC_WARNS: u64 = 5;
pub(crate) const RC_SEC_ELIG: u64 = 6;
// window-bake sections (appended by the link's post-emit bake): the
// post-reset-window arena image and the clock state at the central-
// loop engage point — the state a RunCore boot starts from
pub(crate) const RC_SEC_WARENA: u64 = 7;
pub(crate) const RC_SEC_WSTATE: u64 = 8;
// bounce-reachable prim seeds (rung 3b): per row [inst, slot, tag,
// nwords, words..., nstrs, (len, bytes)...] — the boot restores these
// prims over their live slots and services compiled prim call sites
// natively (runcore_prim_cb)
pub(crate) const RC_SEC_PRIMS: u64 = 9;
// mem-file loads (overlay rung): per row [inst, file_len, file, bin]
// in construction order — the boot rewrites each prim's data region
// from the CURRENT file over the baked window image (the two-fill
// bake gate proved the rest of the window independent of it).  Every
// LOADS inst also has a PRIMS seed row.
pub(crate) const RC_SEC_LOADS: u64 = 10;

/// RLE-encode `words` as (value, run) LE u64 pairs onto `out`.
fn rle_push(words: &[u64], out: &mut Vec<u8>) {
    let mut i = 0;
    while i < words.len() {
        let v = words[i];
        let mut j = i + 1;
        while j < words.len() && words[j] == v {
            j += 1;
        }
        out.extend_from_slice(&v.to_le_bytes());
        out.extend_from_slice(&((j - i) as u64).to_le_bytes());
        i = j;
    }
}

/// Decode (value, run) pairs into exactly `nslots` words; None on any
/// truncation, overflow, or length mismatch (the boot path must treat
/// every field of the file as hostile — adversarial-panel finding).
pub(crate) fn rle_decode(bytes: &[u8], nslots: usize) -> Option<Vec<u64>> {
    let mut out = Vec::with_capacity(nslots);
    let mut pos = 0;
    while out.len() < nslots {
        let v = u64::from_le_bytes(bytes.get(pos..pos + 8)?.try_into().ok()?);
        let run = u64::from_le_bytes(
            bytes.get(pos + 8..pos + 16)?.try_into().ok()?,
        );
        pos += 16;
        let run = usize::try_from(run).ok()?;
        if run == 0 || run > nslots - out.len() {
            return None;
        }
        out.extend(std::iter::repeat(v).take(run));
    }
    (pos == bytes.len()).then_some(out)
}

impl Interp {
    /// RunCore arena sidecar, validation form (self-sufficient AOT
    /// init, rung 1): encode the freshly built post-attach arena as
    /// header + RLE runs.  Format: b"TRSARENA", then LE u64s
    /// [version, AOT_LAYOUT_REV, salted bir hash, nslots], then
    /// (value, run) u64 pairs covering nslots.  Version 1 ends there;
    /// runcore_desc_finish appends the boot-descriptor sections and
    /// bumps the version.  Mem-file designs are included (overlay
    /// rung): the link never reads load files, so the image is
    /// file-independent; the witnesses mask the data regions and the
    /// boot overlays them from the live file.
    fn runcore_image_encode(&self) -> Option<Vec<u8>> {
        if self.jit_arena_ptr.is_null()
            || self.jit_arena_len == 0
            // traced artifacts boot classic (their rec_inits land
            // after this hook, and the wave engine needs the interp)
            || self.vcd_trace
        {
            return None;
        }
        // mem-file designs are back in (overlay rung): the link never
        // reads load files (set_load_memfiles(false)), so this image
        // is file-independent by construction; the witnesses mask the
        // data regions and the boot overlays them from the live file
        let words = unsafe {
            std::slice::from_raw_parts(self.jit_arena_ptr, self.jit_arena_len)
        };
        let salted =
            self.bir_hash ^ (self.vcd_trace as u64 * 0x5452_4143_4544);
        let mut out = Vec::with_capacity(64 + words.len() / 4);
        out.extend_from_slice(b"TRSARENA");
        for v in [
            1u64,
            trs_codegen::abi::AOT_LAYOUT_REV,
            salted,
            self.jit_arena_len as u64,
        ] {
            out.extend_from_slice(&v.to_le_bytes());
        }
        rle_push(words, &mut out);
        Some(out)
    }

    /// prime's post-plan hook for an Emit run: assemble the boot
    /// descriptor (sidecar v2 sections) from stage A plus the clock
    /// and comp state prime just built, mirroring try_central's
    /// eligibility so the engage-time witness can compare decisions.
    pub(crate) fn runcore_desc_finish(
        &mut self,
        rcomps: &[RComp],
        sources: &[crate::ClockSource],
        clocks: &[StrId],
        driver_clock: &HashMap<usize, usize>,
    ) {
        let Some(mut img) = self.runcore_pending.take() else { return };
        let Some(sa) = self.runcore_stage_a.take() else {
            self.runcore_pending = Some(img);
            return;
        };
        // -- eligibility, two classes (adversarial-panel finding: the
        // central loop's conditions are WIDER than RunCore's — BDPI
        // and boxed prims engage the central loop just fine, so a
        // single flag made the engage witness cry wolf on them) --
        // central: mirrors try_central's bail conditions exactly; the
        //   engage witness compares THIS flag.
        // runcore: central + the boot-only gates; the driver boots on
        //   THIS flag.
        let mut c_reason: Option<String> = None;
        let mut r_reason: Option<String> = None;
        let ineligible = |r: &mut Option<String>, s: &str| {
            if r.is_none() {
                *r = Some(s.to_string());
            }
        };
        if !sa.edge_ssa {
            ineligible(&mut r_reason, "edge-SSA emission off");
        }
        // rung 3a: the ONLY foreign imports the boot services natively
        // are the two library ones (rand32/srand — a fresh GlibcRandom
        // matches the classic engine's fresh stream, and any window-
        // time draw already makes the window unskippable); anything
        // else, or an aliased name, boots classic.  Exactness matters:
        // the boot PANICS on an unexpected task rather than fall back,
        // so this gate must make that unreachable.
        let lib_only = self.d.foreign_funcs.iter().all(|f| {
            let (n, c) = (self.s(f.name), self.s(f.c_name));
            (n == "rand32" && c == "rand32") || (n == "srand" && c == "srand")
        });
        if !lib_only {
            ineligible(&mut r_reason, "foreign (BDPI) imports");
        }
        if sa.boxed {
            ineligible(&mut r_reason, "boxed prims (lazy Reflect pending)");
        }
        // dynamic scheduling: alternative interleavings select per
        // edge.  Compiled-alts artifacts DO reach Emit now (the edge
        // fns carry the guard dispatch), so this gate is load-bearing:
        // the RunCore boot replays baked plan state under one comp
        // order, and no single baked window/plan order is valid
        // across selections — alts designs boot classic (v1).
        if rcomps.iter().any(|rc| !rc.alts.is_empty()) {
            ineligible(&mut r_reason, "dynamic scheduling (alts)");
        }
        // the boot parser caps LOADS rows (hostile-file bound): refuse
        // HERE, where a reason can be recorded, rather than bake a
        // sidecar every boot silently rejects (review finding)
        if sa.load_rows.len() > 1 << 12 {
            ineligible(&mut r_reason, "mem-file load count");
        }
        // rung 3b: prim call sites no longer gate eligibility — the
        // PRIMS section bakes a native servicer seed for every
        // bounce-reachable prim instead.  Only a site whose target
        // cannot be seeded (unattached, unseedable kind) boots classic.
        let prim_rows: &[(u64, u64, u64, Vec<u64>, Vec<String>)] =
            match &sa.prim_rows {
                Ok(rows) => rows,
                Err(e) => {
                    ineligible(&mut r_reason, e);
                    &[]
                }
            };
        if !driver_clock.is_empty() {
            ineligible(&mut c_reason, "driver clocks");
        }
        if !self.rstgen_out.is_empty() {
            ineligible(&mut c_reason, "reset generators");
        }
        let mut wave = None;
        for (ci, src) in sources.iter().enumerate() {
            if let crate::ClockSource::Wave(w) = src {
                if wave.is_some() {
                    ineligible(&mut c_reason, "multiple wave clocks");
                }
                wave = Some((ci, *w));
            }
        }
        let (wci, wv) = match wave {
            Some((ci, w)) => (ci, Some(w)),
            None => {
                ineligible(&mut c_reason, "no wave clock");
                (usize::MAX, None)
            }
        };
        if wci != usize::MAX && Some(clocks[wci]) != self.d.default_clock {
            ineligible(&mut c_reason, "wave clock is not the default clock");
        }
        let mut pos: Vec<usize> = Vec::new();
        let mut neg: Vec<usize> = Vec::new();
        for (rci, rc) in rcomps.iter().enumerate() {
            if rc.clk != wci {
                ineligible(&mut c_reason, "composition on a non-wave clock");
                continue;
            }
            // the central mirror honors the ARTIFACT's tick coverage:
            // with edge-SSA off the emitted edge fns carry no ticks,
            // so the load-side covered set is empty regardless of
            // what coverage analysis would say
            let uncovered = rc.ticks.iter().enumerate().any(|(ti, t)| {
                !t.2 && !(sa.edge_ssa
                    && sa.covered.get(rci).is_some_and(|c| c.contains(&ti)))
            });
            if rc.posedge {
                if !rc.early.is_empty() {
                    ineligible(&mut c_reason, "early rules");
                }
                if uncovered {
                    ineligible(&mut c_reason, "uncovered prim tick");
                }
                pos.push(rci);
            } else {
                if rc.entries.iter().any(|e| !e.nodes.is_empty()) || uncovered
                {
                    ineligible(&mut c_reason, "negedge composition with work");
                }
                neg.push(rci);
            }
        }
        if pos.is_empty() {
            ineligible(&mut c_reason, "no posedge compositions");
        }
        let central = c_reason.is_none();
        let reason = c_reason.or(r_reason);
        // -- sections --
        let w64 = |o: &mut Vec<u8>, v: u64| o.extend_from_slice(&v.to_le_bytes());
        let sect = |o: &mut Vec<u8>, tag: u64, payload: &[u8]| {
            w64(o, tag);
            w64(o, payload.len() as u64);
            o.extend_from_slice(payload);
            o.resize(o.len().next_multiple_of(8), 0);
        };
        let mut p = Vec::new();
        img.extend_from_slice(b"TRSBOOTD");
        w64(&mut img, 8);
        // strings: the full design table (StrDyn tokens may select any)
        p.clear();
        w64(&mut p, self.d.strings.len() as u64);
        for s in &self.d.strings {
            w64(&mut p, s.len() as u64);
            p.extend_from_slice(s.as_bytes());
        }
        sect(&mut img, RC_SEC_STRINGS, &p);
        // instance paths (foreign %m locations)
        p.clear();
        w64(&mut p, self.insts.len() as u64);
        for i in &self.insts {
            w64(&mut p, i.path.len() as u64);
            p.extend_from_slice(i.path.as_bytes());
        }
        sect(&mut img, RC_SEC_PATHS, &p);
        // clock
        p.clear();
        let cvals = wv.map_or([0u64; 5], |w| {
            [w.hi, w.lo, w.delay, w.init_high as u64, w.has_init as u64]
        });
        for v in cvals {
            w64(&mut p, v);
        }
        sect(&mut img, RC_SEC_CLOCK, &p);
        // comp call order
        p.clear();
        w64(&mut p, pos.len() as u64);
        for &o in &pos {
            w64(&mut p, o as u64);
        }
        w64(&mut p, neg.len() as u64);
        for &o in &neg {
            w64(&mut p, o as u64);
        }
        sect(&mut img, RC_SEC_COMPS, &p);
        // BRAM warn rows
        p.clear();
        w64(&mut p, sa.warns.len() as u64);
        for (slot, bits, name) in &sa.warns {
            w64(&mut p, *slot);
            w64(&mut p, *bits as u64);
            w64(&mut p, name.len() as u64);
            p.extend_from_slice(name.as_bytes());
        }
        sect(&mut img, RC_SEC_WARNS, &p);
        // eligibility: [runcore flag, central-mirror flag, reason]
        p.clear();
        w64(&mut p, (central && reason.is_none()) as u64);
        w64(&mut p, central as u64);
        let r = reason.unwrap_or_default();
        w64(&mut p, r.len() as u64);
        p.extend_from_slice(r.as_bytes());
        sect(&mut img, RC_SEC_ELIG, &p);
        // bounce-reachable prim seeds (empty for site-free designs)
        p.clear();
        w64(&mut p, prim_rows.len() as u64);
        for (inst, slot, tag, ws, ss) in prim_rows {
            w64(&mut p, *inst);
            w64(&mut p, *slot);
            w64(&mut p, *tag);
            w64(&mut p, ws.len() as u64);
            for w in ws {
                w64(&mut p, *w);
            }
            w64(&mut p, ss.len() as u64);
            for s in ss {
                w64(&mut p, s.len() as u64);
                p.extend_from_slice(s.as_bytes());
            }
        }
        sect(&mut img, RC_SEC_PRIMS, &p);
        // mem-file loads (construction order; empty for file-free
        // designs)
        p.clear();
        w64(&mut p, sa.load_rows.len() as u64);
        for (inst, file, bin) in &sa.load_rows {
            w64(&mut p, *inst);
            w64(&mut p, file.len() as u64);
            p.extend_from_slice(file.as_bytes());
            w64(&mut p, *bin);
        }
        sect(&mut img, RC_SEC_LOADS, &p);
        // bump the header version: sections present.  The version IS
        // the eligibility-semantics revision — bump it whenever the
        // gate rules change, so a driver never trusts an `eligible`
        // flag computed under older rules (panel stale-pair finding).
        // 2 = pre-prim-gate; 3 = prim-site + lib-BDPI gates; 4 =
        // native prim servicing (sites eligible via PRIMS seeds); 5 =
        // mem-file overlay (LOADS rows + two-fill-gated windows).
        img[8..16].copy_from_slice(&5u64.to_le_bytes());
        self.runcore_pending = Some(img);
    }

    /// TRS_RUNCORE_CHECK=1: compare this load's freshly built arena
    /// against the artifact's sidecar image, and (v2) validate the
    /// boot-descriptor sections against live state — strings, inst
    /// paths, and BRAM warn rows compare here; clock, comp order, and
    /// eligibility parse into a RunCoreDesc for the central-loop
    /// engage witness.  A mismatch means a determinism or descriptor
    /// claim failed — loud, never fatal (this is the witness, not the
    /// boot).  Match reports under TRS_STARTUP_TIME.
    fn runcore_image_check(
        &self,
        sidecar: &std::path::Path,
    ) -> Option<RunCoreDesc> {
        let Ok(bytes) = std::fs::read(sidecar) else {
            eprintln!(
                "trs runcore: check requested but {} is unreadable",
                sidecar.display()
            );
            return None;
        };
        let fail = |what: &str| {
            eprintln!(
                "trs runcore: MISMATCH vs {}: {what}",
                sidecar.display()
            );
        };
        if bytes.len() < 8 + 32 || &bytes[..8] != b"TRSARENA" {
            fail("bad header");
            return None;
        }
        let rd = |k: usize| {
            u64::from_le_bytes(bytes[8 + 8 * k..16 + 8 * k].try_into().unwrap())
        };
        let salted =
            self.bir_hash ^ (self.vcd_trace as u64 * 0x5452_4143_4544);
        let version = rd(0);
        if version != 1 && version != 5 {
            // 2-4 = older eligibility-semantics revisions: stale
            fail("unknown or stale version");
            return None;
        }
        if rd(1) != trs_codegen::abi::AOT_LAYOUT_REV {
            fail("layout revision");
            return None;
        }
        if rd(2) != salted {
            fail("design hash");
            return None;
        }
        if rd(3) != self.jit_arena_len as u64 {
            fail("arena length");
            return None;
        }
        let words = unsafe {
            std::slice::from_raw_parts(self.jit_arena_ptr, self.jit_arena_len)
        };
        // mem-file data regions legitimately track the file (the link
        // never reads it, this load did): mask them out of the arena
        // compares; the LOADS section arm verifies the region claims
        let mask = self.runcore_load_mask();
        let masked =
            |k: usize| mask.iter().any(|&(s, l)| k >= s && k < s + l);
        // find where the RLE runs end (also validates them; every
        // field is file data and must be treated as hostile)
        let mut pos = 8 + 32;
        let mut slot = 0usize;
        while slot < words.len() {
            let (Some(vb), Some(rb)) =
                (bytes.get(pos..pos + 8), bytes.get(pos + 8..pos + 16))
            else {
                fail("image truncated mid-run");
                return None;
            };
            let v = u64::from_le_bytes(vb.try_into().unwrap());
            let run = u64::from_le_bytes(rb.try_into().unwrap());
            pos += 16;
            let Ok(run) = usize::try_from(run) else {
                fail("run length overflow");
                return None;
            };
            if run == 0 || run > words.len() - slot {
                fail("run length out of range");
                return None;
            }
            for k in slot..slot + run {
                if words[k] != v && !masked(k) {
                    fail(&format!(
                        "slot {k}: image {v:#x}, rebuilt {:#x}",
                        words[k]
                    ));
                    return None;
                }
            }
            slot += run;
        }
        if std::env::var_os("TRS_STARTUP_TIME").is_some() {
            eprintln!(
                "trs runcore: image MATCH ({} slots)",
                self.jit_arena_len
            );
        }
        if version == 1 {
            return None;
        }
        let _ = version;
        // -- v2 boot-descriptor sections --
        // every take is bounds-checked and every failure REPORTS: a
        // truncated or corrupt descriptor must never be a silent None
        // (this parser is destined for the unconditional boot path)
        let take8 = |pos: &mut usize| -> Option<u64> {
            let v = bytes.get(*pos..*pos + 8)?;
            *pos += 8;
            Some(u64::from_le_bytes(v.try_into().unwrap()))
        };
        let take_str = |pos: &mut usize| -> Option<&str> {
            let n = usize::try_from(take8(pos)?).ok()?;
            let s = bytes.get(*pos..pos.checked_add(n)?)?;
            *pos += n;
            std::str::from_utf8(s).ok()
        };
        macro_rules! want {
            ($e:expr, $what:literal) => {
                match $e {
                    Some(v) => v,
                    None => {
                        fail(concat!("descriptor truncated: ", $what));
                        return None;
                    }
                }
            };
        }
        if bytes.get(pos..pos + 8) != Some(&b"TRSBOOTD"[..]) {
            fail("missing boot descriptor");
            return None;
        }
        pos += 8;
        let nsect = want!(take8(&mut pos), "section count");
        if nsect > 64 {
            fail("absurd section count");
            return None;
        }
        let mut desc = RunCoreDesc {
            hi: 0,
            lo: 0,
            delay: 0,
            init_high: false,
            has_init: false,
            pos: Vec::new(),
            neg: Vec::new(),
            eligible: false,
            central: false,
            reason: String::new(),
            window: None,
        };
        let mut seen_tags = 0u64;
        let mut wstate = None;
        let mut warena: Option<Vec<u64>> = None;
        for _ in 0..nsect {
            let tag = want!(take8(&mut pos), "section tag");
            let len = want!(
                usize::try_from(want!(take8(&mut pos), "section length")).ok(),
                "section length overflow"
            );
            let end = want!(
                pos.checked_add(len).map(|e| e.next_multiple_of(8)),
                "section length overflow"
            );
            if end > bytes.len() {
                fail("section overruns file");
                return None;
            }
            if tag >= 1 && tag <= 63 {
                if seen_tags & (1 << tag) != 0 {
                    fail("duplicate section");
                    return None;
                }
                seen_tags |= 1 << tag;
            }
            let mut p = pos;
            match tag {
                RC_SEC_STRINGS => {
                    let n = want!(take8(&mut p), "string count");
                    if n != self.d.strings.len() as u64 {
                        fail("descriptor: string count");
                        return None;
                    }
                    for want in &self.d.strings {
                        if take_str(&mut p) != Some(want.as_str()) {
                            fail("descriptor: string table drift");
                            return None;
                        }
                    }
                }
                RC_SEC_PATHS => {
                    let n = want!(take8(&mut p), "inst count");
                    if n != self.insts.len() as u64 {
                        fail("descriptor: inst count");
                        return None;
                    }
                    for i in &self.insts {
                        if take_str(&mut p) != Some(i.path.as_str()) {
                            fail("descriptor: inst path drift");
                            return None;
                        }
                    }
                }
                RC_SEC_CLOCK => {
                    desc.hi = want!(take8(&mut p), "clock hi");
                    desc.lo = want!(take8(&mut p), "clock lo");
                    desc.delay = want!(take8(&mut p), "clock delay");
                    desc.init_high = want!(take8(&mut p), "clock init") != 0;
                    desc.has_init = want!(take8(&mut p), "clock has_init") != 0;
                }
                RC_SEC_COMPS => {
                    let np = want!(take8(&mut p), "pos count");
                    for _ in 0..np {
                        desc.pos
                            .push(want!(take8(&mut p), "pos ordinal") as usize);
                    }
                    let nn = want!(take8(&mut p), "neg count");
                    for _ in 0..nn {
                        desc.neg
                            .push(want!(take8(&mut p), "neg ordinal") as usize);
                    }
                }
                RC_SEC_WARNS => {
                    let reg = crate::prim::bram_warn_rows();
                    let n = want!(take8(&mut p), "warn count");
                    let mut seen = 0usize;
                    for _ in 0..n {
                        let slot = want!(take8(&mut p), "warn slot");
                        let bits = want!(take8(&mut p), "warn bits") as u32;
                        let name = want!(take_str(&mut p), "warn name");
                        // bounds BEFORE any pointer arithmetic: the
                        // slot is file data (UB finding)
                        if slot >= self.jit_arena_len as u64 {
                            fail(&format!("warn slot {slot} out of range"));
                            return None;
                        }
                        let key = unsafe {
                            self.jit_arena_ptr.add(slot as usize)
                        } as usize;
                        match reg.get(&key) {
                            Some((n2, b2)) if n2 == name && *b2 == bits => {
                                seen += 1;
                            }
                            _ => {
                                fail(&format!(
                                    "descriptor: warn row drift at slot {slot} ({name})"
                                ));
                                return None;
                            }
                        }
                    }
                    // count only rows in THIS arena's slot range: the
                    // registry is process-global and an earlier engine
                    // in the same process (selfcheck) leaves its own
                    let ours = reg
                        .keys()
                        .filter(|&&k| {
                            let base = self.jit_arena_ptr as usize;
                            k >= base && k < base + 8 * self.jit_arena_len
                        })
                        .count();
                    if seen != ours {
                        fail(&format!(
                            "descriptor: warn rows {seen} baked vs {ours} live"
                        ));
                        return None;
                    }
                }
                RC_SEC_ELIG => {
                    desc.eligible = want!(take8(&mut p), "elig flag") != 0;
                    desc.central =
                        want!(take8(&mut p), "central flag") != 0;
                    desc.reason =
                        want!(take_str(&mut p), "elig reason").to_string();
                }
                RC_SEC_WARENA => {
                    let Some(w) =
                        rle_decode(&bytes[p..pos + len], self.jit_arena_len)
                    else {
                        fail("window arena malformed");
                        return None;
                    };
                    warena = Some(w);
                }
                RC_SEC_WSTATE => {
                    wstate = Some((
                        want!(take8(&mut p), "window tp"),
                        want!(take8(&mut p), "window tn"),
                        want!(take8(&mut p), "window cycle"),
                    ));
                }
                RC_SEC_PRIMS => {
                    // recompute the bounce-reachable set from the live
                    // plan and compare each baked seed against the
                    // live prim's own serialization + attachment
                    let Some(lz) = self.jit_shared.as_ref() else {
                        fail("descriptor: prim seeds without a live plan");
                        return None;
                    };
                    let mut want_insts: std::collections::BTreeSet<usize> =
                        Default::default();
                    for pr in lz.protos.iter() {
                        for pc in
                            pr.sched_prims.iter().chain(pr.exec_prims.iter())
                        {
                            want_insts.insert(pc.inst);
                        }
                    }
                    // mem-file prims are in the PRIMS rows too (the
                    // boot's overlay drives them) — same union here
                    want_insts.extend(
                        self.runcore_live_loads().iter().map(|r| r.0 as usize),
                    );
                    // mirror the encoder: if ANY bounce-reachable inst
                    // is unattached or unseedable, desc_finish encoded
                    // ZERO rows (and an ineligible reason) — expecting
                    // the full set there made the witness cry wolf on
                    // every such design (panel finding)
                    let seedable = want_insts.iter().all(|&i| {
                        matches!(&self.insts[i].kind, InstKind::Prim(pm)
                            if pm.runcore_seed().is_some()
                                && pm.runcore_slot().is_some())
                    });
                    let expect = if seedable { want_insts.len() } else { 0 };
                    let n = want!(take8(&mut p), "prim seed count");
                    if n != expect as u64 {
                        fail(&format!(
                            "descriptor: {n} prim seeds baked vs {expect} \
                             expected live"
                        ));
                        return None;
                    }
                    let mut seen_insts = std::collections::HashSet::new();
                    for _ in 0..n {
                        let inst =
                            want!(take8(&mut p), "prim seed inst") as usize;
                        let slot = want!(take8(&mut p), "prim seed slot");
                        let tag = want!(take8(&mut p), "prim seed tag");
                        let nw = want!(take8(&mut p), "prim seed words");
                        if nw > 64 {
                            fail("descriptor: absurd prim seed words");
                            return None;
                        }
                        let mut ws = Vec::with_capacity(nw as usize);
                        for _ in 0..nw {
                            ws.push(want!(take8(&mut p), "prim seed word"));
                        }
                        let ns = want!(take8(&mut p), "prim seed strings");
                        if ns > 8 {
                            fail("descriptor: absurd prim seed strings");
                            return None;
                        }
                        let mut ss = Vec::with_capacity(ns as usize);
                        for _ in 0..ns {
                            ss.push(
                                want!(take_str(&mut p), "prim seed string")
                                    .to_string(),
                            );
                        }
                        if !want_insts.contains(&inst) {
                            fail(&format!(
                                "descriptor: prim seed for inst {inst} \
                                 which no live call site reaches"
                            ));
                            return None;
                        }
                        if !seen_insts.insert(inst) {
                            fail(&format!(
                                "descriptor: duplicate prim seed for \
                                 inst {inst}"
                            ));
                            return None;
                        }
                        let InstKind::Prim(pm) = &self.insts[inst].kind
                        else {
                            fail("descriptor: prim seed on non-prim inst");
                            return None;
                        };
                        if pm.runcore_seed() != Some((tag, ws, ss)) {
                            fail(&format!(
                                "descriptor: prim seed drift at inst \
                                 {inst} ({})",
                                self.insts[inst].path
                            ));
                            return None;
                        }
                        let live = pm.runcore_slot().and_then(|ptr| {
                            (ptr as usize)
                                .checked_sub(self.jit_arena_ptr as usize)
                                .map(|d| d / 8)
                        });
                        if live != Some(slot as usize) {
                            fail(&format!(
                                "descriptor: prim slot drift at inst \
                                 {inst}: baked {slot}, live {live:?}"
                            ));
                            return None;
                        }
                    }
                }
                RC_SEC_LOADS => {
                    // compare the baked rows against the live prims'
                    // retained load requests, in construction order
                    let live_loads = self.runcore_live_loads();
                    let n = want!(take8(&mut p), "load row count");
                    if n != live_loads.len() as u64 {
                        fail(&format!(
                            "descriptor: {n} load rows baked vs {} live",
                            live_loads.len()
                        ));
                        return None;
                    }
                    for want_row in &live_loads {
                        let inst = want!(take8(&mut p), "load inst");
                        let file =
                            want!(take_str(&mut p), "load file").to_string();
                        let bin = want!(take8(&mut p), "load bin");
                        if (inst, file.as_str(), bin)
                            != (want_row.0, want_row.1.as_str(), want_row.2)
                        {
                            fail(&format!(
                                "descriptor: load row drift at inst {inst} \
                                 ({file})"
                            ));
                            return None;
                        }
                    }
                }
                _ => {
                    fail(&format!("descriptor: unknown section {tag}"));
                    return None;
                }
            }
            pos = end;
        }
        // required sections: 1-6 + 9 + 10 always; window sections
        // travel as a pair or not at all
        for t in [
            RC_SEC_STRINGS,
            RC_SEC_PATHS,
            RC_SEC_CLOCK,
            RC_SEC_COMPS,
            RC_SEC_WARNS,
            RC_SEC_ELIG,
            RC_SEC_PRIMS,
            RC_SEC_LOADS,
        ] {
            if seen_tags & (1 << t) == 0 {
                fail("descriptor: missing required section");
                return None;
            }
        }
        match (warena, wstate) {
            (Some(a), Some((tp, tn, cyc))) => {
                desc.window = Some((a, tp, tn, cyc));
            }
            (None, None) => {}
            _ => {
                fail("descriptor: window sections must travel as a pair");
                return None;
            }
        }
        // an eligible claim with a degenerate clock or no posedge
        // comps is self-contradictory — refuse it before any boot
        // path could trust it
        if (desc.eligible || desc.central)
            && (desc.hi + desc.lo == 0 || desc.pos.is_empty())
        {
            fail("descriptor: eligible with degenerate clock/comps");
            return None;
        }
        if desc.eligible && !desc.central {
            fail("descriptor: runcore-eligible but not central-eligible");
            return None;
        }
        if std::env::var_os("TRS_STARTUP_TIME").is_some() {
            eprintln!(
                "trs runcore: descriptor sections MATCH (eligible={} \
                 central={} {}{})",
                desc.eligible,
                desc.central,
                desc.reason,
                if desc.window.is_some() { " +window" } else { "" }
            );
        }
        Some(desc)
    }

    /// Window-bake sections: the current (post-window) arena as RLE
    /// plus the engage-point clock state.  Called from the central
    /// loop's engage point when runcore_bake is armed.
    pub(crate) fn runcore_window_encode(&self, tp: u64, tn: u64) -> Vec<u8> {
        let words = unsafe {
            std::slice::from_raw_parts(self.jit_arena_ptr, self.jit_arena_len)
        };
        let w64 = |o: &mut Vec<u8>, v: u64| o.extend_from_slice(&v.to_le_bytes());
        let mut rle = Vec::new();
        rle_push(words, &mut rle);
        let mut out = Vec::with_capacity(rle.len() + 64);
        w64(&mut out, RC_SEC_WARENA);
        w64(&mut out, rle.len() as u64);
        out.extend_from_slice(&rle);
        // rle is 16-byte-granular, already 8-aligned
        w64(&mut out, RC_SEC_WSTATE);
        w64(&mut out, 24);
        for v in [tp, tn, self.cycle] {
            w64(&mut out, v);
        }
        out
    }

    // -- link-time window bake (docs/RUNCORE.md): runcore_bake_capture
    // runs the reset window quiet on a FRESH interp with an artifact
    // Load request armed; runcore_bake_commit gates (two-fill, for
    // mem-file designs) and splices the captured sections into the
    // sidecar.  Every non-clean outcome is a silent classic boot. --

    /// The live mem-file data regions as absolute (word index, len)
    /// ranges into the arena — the words whose content legitimately
    /// tracks the load file (masked by every arena witness compare;
    /// rewritten by the boot's overlay).
    pub(crate) fn runcore_load_mask(&self) -> Vec<(usize, usize)> {
        let base = self.jit_arena_ptr as usize;
        let mut m = Vec::new();
        for inst in &self.insts {
            if let InstKind::Prim(p) = &inst.kind {
                if p.runcore_load().is_some() {
                    if let (Some(ptr), Some((off, len))) =
                        (p.runcore_slot(), p.runcore_load_region())
                    {
                        if let Some(s) = (ptr as usize).checked_sub(base) {
                            m.push((s / 8 + off, len));
                        }
                    }
                }
            }
        }
        m
    }

    /// The live mem-file load rows, (inst, file, binary_format) in
    /// construction order — the single source both the encoder and
    /// the witness use (drift between them is exactly what the
    /// witness compare exists to catch, so they must share one walk).
    pub(crate) fn runcore_live_loads(&self) -> Vec<(u64, String, u64)> {
        let mut rows = Vec::new();
        for (ci, inst) in self.insts.iter().enumerate() {
            if let InstKind::Prim(p) = &inst.kind {
                if let Some((f, bin)) = p.runcore_load() {
                    rows.push((ci as u64, f, bin as u64));
                }
            }
        }
        rows
    }

    /// True when the design has mem-file prims (RegFileLoad /
    /// BRAM*Load) — the link's bake then runs the two-fill gate.
    pub fn runcore_has_loads(&self) -> bool {
        self.insts.iter().any(|i| {
            matches!(&i.kind, InstKind::Prim(p) if p.runcore_load().is_some())
        })
    }

    /// Run the reset window on the loaded artifact and capture the
    /// boundary state (docs/RUNCORE.md).  `fill`: two-fill gate
    /// pattern written into every mem-file data region at plan time.
    /// None = not bakeable — never engaged, dirty window, or a
    /// perturbed region was written during the window (the boot's
    /// overlay would lose that write) — the design boots classic.
    pub fn runcore_bake_capture(&mut self, fill: Option<u64>) -> Option<RunCoreBake> {
        self.set_quiet();
        self.runcore_bake = true;
        self.runcore_bake_fill = fill;
        let before =
            crate::prim::WINDOW_EFFECTS.load(std::sync::atomic::Ordering::Relaxed);
        // TWO cycles: advance(1) finishes cycle 1's timeslice and stops
        // BEFORE the pop that deasserts reset — the engage point (and
        // the capture) is only reached on the way to cycle 2.  The
        // capture snapshots the effects counter, so the one steady
        // cycle executed past the boundary cannot pollute the gate.
        self.advance(2);
        let captured = self.runcore_window.take();
        let clean = captured
            .as_ref()
            .is_some_and(|(_, at_capture, _)| *at_capture == before);
        // regions untouched by the window: with the fill in place, a
        // window write into a region would leave it != the fill image
        let regions_hold = fill.is_none_or(|pat| {
            self.insts.iter().all(|i| match &i.kind {
                InstKind::Prim(p)
                    if p.runcore_load().is_some()
                        && p.runcore_slot().is_some() =>
                {
                    p.runcore_region_is(pat)
                }
                _ => true,
            })
        });
        if std::env::var_os("TRS_STARTUP_TIME").is_some() {
            let bails: Vec<String> = crate::CENTRAL_BAIL
                .iter()
                .enumerate()
                .filter_map(|(i, c)| {
                    let n = c.load(std::sync::atomic::Ordering::Relaxed);
                    (n > 0).then(|| format!("#{i}x{n}"))
                })
                .collect();
            eprintln!(
                "trs runcore: bake engaged={} clean={clean} \
                 regions_hold={regions_hold} (bails [{}])",
                captured.is_some(),
                bails.join(" ")
            );
        }
        let (sections, _, state) = captured?;
        if !clean || !regions_hold {
            return None;
        }
        let arena = unsafe {
            std::slice::from_raw_parts(self.jit_arena_ptr, self.jit_arena_len)
        }
        .to_vec();
        Some(RunCoreBake {
            sections,
            arena,
            mask: self.runcore_load_mask(),
            state,
        })
    }
}

/// One window-bake capture: the encoded window sections plus what the
/// two-fill gate compares.
pub struct RunCoreBake {
    sections: Vec<u8>,
    /// the post-window (boundary) arena
    arena: Vec<u64>,
    /// mem-file data regions, absolute (word index, len)
    mask: Vec<(usize, usize)>,
    /// (tp, tn, cycle) at capture
    state: (u64, u64, u64),
}

/// Two-fill gate + sidecar splice.  For a mem-file design, `a` and
/// `b` are captures under DIFFERENT fill patterns: baking is sound
/// only when every word OUTSIDE the load regions (and the clock
/// state) agrees — the window provably does not depend on the file
/// content the boot's overlay will replace.  File-free designs pass
/// `b = None` (single capture, no gate).
pub fn runcore_bake_commit(
    sidecar: &std::path::Path,
    a: &RunCoreBake,
    b: Option<&RunCoreBake>,
) -> Result<bool, String> {
    if let Some(b) = b {
        let masked =
            |k: usize| a.mask.iter().any(|&(s, l)| k >= s && k < s + l);
        let sound = a.state == b.state
            && a.mask == b.mask
            && a.arena.len() == b.arena.len()
            && (0..a.arena.len()).all(|k| masked(k) || a.arena[k] == b.arena[k]);
        if !sound {
            if std::env::var_os("TRS_STARTUP_TIME").is_some() {
                eprintln!(
                    "trs runcore: bake refused — window depends on \
                     mem-file content (two-fill gate)"
                );
            }
            return Ok(false);
        }
    }
    let sections = &a.sections;
    let mut bytes =
        std::fs::read(sidecar).map_err(|e| format!("{}: {e}", sidecar.display()))?;
        // bump nsect (u64 right after TRSBOOTD) by 2 and append
        let Some(td) = bytes
            .windows(8)
            .position(|w| w == b"TRSBOOTD")
            .filter(|&i| i + 16 <= bytes.len())
        else {
            return Err("sidecar has no boot descriptor".into());
        };
        let nsect =
            u64::from_le_bytes(bytes[td + 8..td + 16].try_into().unwrap());
        bytes[td + 8..td + 16].copy_from_slice(&(nsect + 2).to_le_bytes());
        bytes.extend_from_slice(sections);
        let tmp = sidecar.with_extension("arena.tmp");
        std::fs::write(&tmp, &bytes)
            .and_then(|()| std::fs::rename(&tmp, sidecar))
            .map_err(|e| format!("{}: {e}", sidecar.display()))?;
        Ok(true)
}

/// Worker-thread count for compile fan-out (TRS_JIT_THREADS caps).
fn jit_workers(n: usize) -> usize {
    std::env::var("TRS_JIT_THREADS")
        .ok()
        .and_then(|v| v.parse::<usize>().ok())
        .unwrap_or_else(|| {
            std::thread::available_parallelism().map(|x| x.get()).unwrap_or(8)
        })
        .clamp(1, 64)
        .min(n.max(1))
}

/// Stdio-flush callback for direct BDPI calls: phase 0 flushes the
/// runtime's stdout sink BEFORE the C call, phase 1 fflush(NULL)es
/// libc's buffers after — the interleaving contract bdpi::Bdpi::call
/// keeps.
pub(crate) unsafe extern "C" fn jit_stdio_cb(phase: u64) {
    if phase == 0 {
        crate::out::flush();
    } else {
        unsafe { libc::fflush(std::ptr::null_mut()) };
    }
}

impl Interp {
    /// Ticks the edge fns compile: ungated, non-reset ticks of
    /// arena-backed wires (RWire/PulseWire valid-bit clears).  Returns
    /// per composition the valid-slot list (emitter) and the covered
    /// rc.ticks indices (runtime skip + central preconditions).  Must
    /// stay deterministic across processes: the linker bakes the
    /// clears, the loader re-derives the covered set.
    fn prim_tick_coverage(
        &self,
        inst_envs: &HashMap<usize, InstEnv>,
        rcomps: &[RComp],
    ) -> TickCoverage {
        let mut wire_of: HashMap<usize, u32> = HashMap::new();
        let mut creg_of: HashMap<usize, (u32, u32)> = HashMap::new();
        let mut bram_of: HashMap<usize, (u32, u32, u64, u32, u32, bool)> =
            HashMap::new();
        for ie in inst_envs.values() {
            for (name, &(base, _w)) in &ie.wire_slot {
                if let Some(&gi) = ie.children.get(name) {
                    wire_of.insert(gi, base);
                }
            }
            for (name, &(base, w)) in &ie.creg5_slot {
                if let Some(&gi) = ie.children.get(name) {
                    creg_of.insert(gi, (base, w.max(1).div_ceil(64)));
                }
            }
            for (name, &(base, w, sz, cs, nw, du, _pl)) in &ie.bram_slot {
                if let Some(&gi) = ie.children.get(name) {
                    bram_of.insert(gi, (base, w, sz, cs, nw, du));
                }
            }
        }
        let mut out = TickCoverage {
            wire_clears: Vec::with_capacity(rcomps.len()),
            creg_copies: Vec::with_capacity(rcomps.len()),
            bram_ticks: Vec::with_capacity(rcomps.len()),
            covered_wire: Vec::with_capacity(rcomps.len()),
            covered_all: Vec::with_capacity(rcomps.len()),
        };
        for rc in rcomps {
            let mut cl: Vec<u32> = Vec::new();
            let mut cc: Vec<(u32, u32)> = Vec::new();
            let mut bt: Vec<[u64; 3]> = Vec::new();
            let mut cov_w = std::collections::HashSet::new();
            let mut cov_a = std::collections::HashSet::new();
            for (ti, (inst, pname, is_rst, _owner, gexpr)) in
                rc.ticks.iter().enumerate()
            {
                if *is_rst || gexpr.is_some() || self.rstgen_out.contains_key(inst)
                {
                    continue;
                }
                if let Some(&slot) = wire_of.get(inst) {
                    cl.push(slot);
                    cov_w.insert(ti);
                    cov_a.insert(ti);
                } else if let Some(&(base, words)) = creg_of.get(inst) {
                    cc.push((base, words));
                    cov_a.insert(ti);
                } else if let Some(&(base, w, sz, cs, nw, du)) =
                    bram_of.get(inst)
                {
                    // tick order is preserved (the cross-port bypass
                    // reads the other port's just-latched written_at)
                    bt.push(trs_codegen::abi::bram_tick_args(
                        base,
                        pname == "clkB",
                        w,
                        sz,
                        cs,
                        nw,
                        du,
                    ));
                    cov_a.insert(ti);
                }
            }
            cl.sort_unstable();
            out.wire_clears.push(cl);
            out.creg_copies.push(cc);
            out.bram_ticks.push(bt);
            out.covered_wire.push(cov_w);
            out.covered_all.push(cov_a);
        }
        out
    }

    /// Task #24 M1: gap-wise cross-rule def-sharing legality census.
    /// For every def consumed by 2+ exec bodies of a composition,
    /// decide per consumer-gap whether the anchor value survives —
    /// i.e. no intervening exec writes state the def's cone reads
    /// UNSTABLY (stable = begin-of-instant prim contracts only:
    /// ConfigReg reads, FIFO i_* views).  Prints the shareable vs
    /// must-recompute mass and a kill histogram; this table is what
    /// the SSA edge emitter (M2) consumes as its legality oracle.
    fn edge_ssa_plan(
        &self,
        inst_envs: &HashMap<usize, InstEnv>,
        nodes: &[Vec<(bool, usize)>],
        specs: &[RuleSpec],
        has_early: bool,
        stats: bool,
        // ordinals forced OUTLINED by the measured edge budget (the
        // replan pass): joined into outlined_execs before any sharing/
        // hoist/elision table is computed, so the plan stays coherent
        forced_outline: &std::collections::HashSet<usize>,
    ) -> trs_codegen::abi::EdgeSsaPlan {
        let specs_lite: Vec<(usize, usize)> =
            specs.iter().map(|sp| (sp.inst, sp.rule_idx)).collect();
        let specs_lite = &specs_lite[..];
        use trs_ir::{Action as A, Expr as E, InstanceKind, Primitive as P, Stmt};
        use std::collections::HashSet;

        #[derive(Clone)]
        struct Cone {
            /// prim instances this cone reads with NO stability contract
            reads: HashSet<usize>,
            /// transitive def closure (inst, def), incl. the root
            defs: HashSet<(usize, StrId)>,
            /// root def's own expr node count (share-census units)
            mass: u64,
            /// hoist-poison bitmask (0 = pure/hoistable):
            /// 1=port read, 2=foreign/task ref, 4=non-arena-inline prim
            poison: u8,
        }
        impl Default for Cone {
            fn default() -> Self {
                Cone { reads: HashSet::new(), defs: HashSet::new(), mass: 0, poison: 0 }
            }
        }
        impl Cone {
            fn pure(&self) -> bool {
                self.poison == 0
            }
            fn absorb(&mut self, o: &Cone) {
                self.reads.extend(o.reads.iter().copied());
                self.defs.extend(o.defs.iter().copied());
                self.poison |= o.poison;
            }
        }

        // the exporter ships prims as Other{name} (prim.rs classifies
        // by the same strings); the enum variants are matched too in
        // case the exporter ever starts using them
        fn cat(p: &P, s: &dyn Fn(StrId) -> String) -> &'static str {
            match p {
                P::Reg { .. } => "reg",
                P::ConfigReg { .. } => "configreg",
                P::CReg { .. } => "creg",
                P::Wire { .. } => "wire",
                P::Fifo { .. } => "fifo",
                P::RegFile { .. } => "regfile",
                P::Bram { .. } => "bram",
                P::Other { name } => {
                    let n = s(*name);
                    if n.starts_with("ConfigReg") {
                        "configreg"
                    } else if n.starts_with("CReg") {
                        "creg"
                    } else if n.starts_with("Reg") {
                        "reg"
                    } else if n.contains("FIFO") {
                        "fifo"
                    } else if n.contains("Wire") {
                        "wire"
                    } else if n.starts_with("RegFile") {
                        "regfile"
                    } else if n.starts_with("BRAM") {
                        "bram"
                    } else {
                        "other"
                    }
                }
                _ => "other",
            }
        }
        fn stable_read(pc: &'static str, m: &str) -> bool {
            pc == "configreg" || (pc == "fifo" && m.starts_with("i_"))
        }
        fn expr_mass(e: &E) -> u64 {
            let mut n = 1u64;
            match e {
                E::MethCall { args, .. }
                | E::Prim { args, .. }
                | E::ForeignCall { args, .. } => {
                    for a in args {
                        n += expr_mass(a);
                    }
                }
                E::If { cond, then_, else_, .. } => {
                    n += expr_mass(cond) + expr_mass(then_) + expr_mass(else_);
                }
                E::Case { scrutinee, arms, default, .. } => {
                    n += expr_mass(scrutinee) + expr_mass(default);
                    for (_, a) in arms {
                        n += expr_mass(a);
                    }
                }
                _ => {}
            }
            n
        }
        fn child<'a>(
            d: &'a trs_ir::Design,
            inst_envs: &HashMap<usize, InstEnv>,
            inst: usize,
            name: StrId,
        ) -> Option<(usize, &'a InstanceKind)> {
            let ie = inst_envs.get(&inst)?;
            let gi = *ie.children.get(&name)?;
            let k = &d.modules[ie.mir]
                .instances
                .iter()
                .find(|i| i.name == name)?
                .kind;
            Some((gi, k))
        }

        struct Ctx<'a> {
            itp: &'a Interp,
            inst_envs: &'a HashMap<usize, InstEnv>,
            prim_cat: HashMap<usize, &'static str>,
            cone_memo: HashMap<(usize, StrId), Cone>,
            write_memo: HashMap<(usize, StrId), HashSet<usize>>,
        }

        fn walk_expr(cx: &mut Ctx, inst: usize, e: &E, out: &mut Cone) {
            match e {
                E::Def(n) => {
                    let c = cone(cx, inst, *n);
                    out.absorb(&c);
                    out.defs.insert((inst, *n));
                }
                E::MethCall { instance, method, args, .. } => {
                    for a in args {
                        walk_expr(cx, inst, a, out);
                    }
                    match child(&cx.itp.d, cx.inst_envs, inst, *instance) {
                        Some((gi, InstanceKind::Prim(p))) => {
                            let s = |n: StrId| cx.itp.s(n).to_string();
                            let pc = cat(p, &s);
                            cx.prim_cat.insert(gi, pc);
                            if !stable_read(pc, cx.itp.s(*method)) {
                                out.reads.insert(gi);
                            }
                            // hoist purity: the read must be an
                            // arena-inline load for THIS instance
                            let ie = &cx.inst_envs[&inst];
                            let inline_ok = ie.reg_slot.contains_key(instance)
                                || ie.wire_slot.contains_key(instance)
                                || ie.creg_slot.contains_key(instance)
                                || ie.fifo_slot.contains_key(instance);
                            if !inline_ok {
                                out.poison |= 4;
                            }
                        }
                        Some((gi, InstanceKind::Module(_))) => {
                            // value methods inline LAZILY (child frame,
                            // result expr, defs on demand) — the true
                            // cone is the RESULT's closure only.
                            // Walking the whole body drags in arg-
                            // dependent defs the emitted code never
                            // evaluates (over-poisoning; Ravi's catch)
                            let cmir = cx.inst_envs[&gi].mir;
                            let mm = cx.itp.d.modules[cmir]
                                .methods
                                .iter()
                                .find(|m| m.name == *method)
                                .cloned();
                            if let Some(mm) = mm {
                                if let Some(res) = &mm.result {
                                    walk_expr(cx, gi, res, out);
                                }
                            }
                        }
                        None => {}
                    }
                }
                E::Prim { op, args, .. } => {
                    // review-fleet finding: Quot/Rem lower with an
                    // unconditional zero-divisor SIGFPE trap — a
                    // hoisted cone would evaluate it on edges where
                    // the guarding rules are disabled.  Trapping ops
                    // poison hoistability.
                    if matches!(op, trs_ir::PrimOp::Quot | trs_ir::PrimOp::Rem)
                    {
                        out.poison |= 2;
                    }
                    for a in args {
                        walk_expr(cx, inst, a, out);
                    }
                }
                E::ForeignCall { args, .. } => {
                    out.poison |= 2;
                    for a in args {
                        walk_expr(cx, inst, a, out);
                    }
                }
                E::If { cond, then_, else_, .. } => {
                    walk_expr(cx, inst, cond, out);
                    walk_expr(cx, inst, then_, out);
                    walk_expr(cx, inst, else_, out);
                }
                E::Case { scrutinee, arms, default, .. } => {
                    walk_expr(cx, inst, scrutinee, out);
                    for (_, a) in arms {
                        walk_expr(cx, inst, a, out);
                    }
                    walk_expr(cx, inst, default, out);
                }
                // ports poison hoistability: method args are
                // call-site-specific, EN ports mutate DURING the edge
                // (unmodeled by the read-sets), and a hoisted frame has
                // no port bindings at all
                E::Port(n) => {
                    // classify: MethodArg = call-site-specific (bit 1),
                    // MethodEnable = intra-edge mutable EN (bit 8),
                    // Reset/Clock/Parameter = frame-independent (bit 16
                    // for now: admissible once the lowering resolves
                    // them outside their home frame)
                    let mir = cx.inst_envs[&inst].mir;
                    let kind = cx.itp.d.modules[mir]
                        .inputs
                        .iter()
                        .find(|pt| pt.name == *n)
                        .map(|pt| pt.kind);
                    out.poison |= match kind {
                        Some(trs_ir::PortKind::MethodEnable) => 8,
                        Some(trs_ir::PortKind::Reset)
                        | Some(trs_ir::PortKind::Clock)
                        | Some(trs_ir::PortKind::ClockGate)
                        | Some(trs_ir::PortKind::Parameter) => 16,
                        _ => 1,
                    };
                }
                E::TaskValue { .. } => {
                    out.poison |= 2;
                }
                _ => {}
            }
        }
        // defs (and their cones) referenced by a body statement,
        // EXPRESSION side only — actions' state effects live in
        // stmt_writes
        fn walk_stmt_defs(cx: &mut Ctx, inst: usize, st: &Stmt, out: &mut Cone) {
            match st {
                Stmt::Def { expr, .. } => walk_expr(cx, inst, expr, out),
                Stmt::Action(a) | Stmt::AvAction { action: a, .. } => match a {
                    A::MethCall { cond, args, instance, method, .. } => {
                        walk_expr(cx, inst, cond, out);
                        for x in args {
                            walk_expr(cx, inst, x, out);
                        }
                        // a child ACTION method's body may read further
                        // defs (in the child's frame)
                        if let Some((gi, InstanceKind::Module(_))) =
                            child(&cx.itp.d, cx.inst_envs, inst, *instance)
                        {
                            let cmir = cx.inst_envs[&gi].mir;
                            let mm = cx.itp.d.modules[cmir]
                                .methods
                                .iter()
                                .find(|m| m.name == *method)
                                .cloned();
                            if let Some(mm) = mm {
                                for st2 in &mm.body {
                                    walk_stmt_defs(cx, gi, st2, out);
                                }
                            }
                        }
                    }
                    A::Foreign { cond, args, .. } | A::Task { cond, args, .. } => {
                        walk_expr(cx, inst, cond, out);
                        for x in args {
                            walk_expr(cx, inst, x, out);
                        }
                    }
                },
                Stmt::Cond { cond, then_, else_ } => {
                    walk_expr(cx, inst, cond, out);
                    for s in then_ {
                        walk_stmt_defs(cx, inst, s, out);
                    }
                    for s in else_ {
                        walk_stmt_defs(cx, inst, s, out);
                    }
                }
            }
        }
        fn cone(cx: &mut Ctx, inst: usize, n: StrId) -> Cone {
            if let Some(c) = cx.cone_memo.get(&(inst, n)) {
                return c.clone();
            }
            // defs are a DAG; placeholder guards pathological input
            cx.cone_memo.insert((inst, n), Cone::default());
            let mir = cx.inst_envs[&inst].mir;
            let mut c = Cone::default();
            c.defs.insert((inst, n));
            let dd = cx.itp.d.modules[mir]
                .defs
                .iter()
                .find(|d| d.name == n)
                .cloned();
            if let Some(dd) = dd {
                c.mass = expr_mass(&dd.expr);
                walk_expr(cx, inst, &dd.expr, &mut c);
            } else {
                // not in any def table: a SYNTHETIC ActionValue result,
                // bound only inside the rule performing the call —
                // context-bound like an arg port, never hoist/share
                // (RadixSort: hoisting a slice of AVMeth_dut_response_get
                // into another rule's section has no binding to read)
                c.poison |= 1;
            }
            cx.cone_memo.insert((inst, n), c.clone());
            c
        }
        // prim instances an action body (rule or action method) writes
        fn stmt_writes(
            cx: &mut Ctx,
            inst: usize,
            stmts: &[Stmt],
            out: &mut HashSet<usize>,
        ) {
            for st in stmts {
                match st {
                    Stmt::Action(a) | Stmt::AvAction { action: a, .. } => {
                        if let A::MethCall { instance, method, .. } = a {
                            match child(&cx.itp.d, cx.inst_envs, inst, *instance) {
                                Some((gi, InstanceKind::Prim(p))) => {
                                    let s = |n: StrId| cx.itp.s(n).to_string();
                                    cx.prim_cat.insert(gi, cat(p, &s));
                                    out.insert(gi);
                                }
                                Some((gi, InstanceKind::Module(_))) => {
                                    let key = (gi, *method);
                                    if let Some(w) = cx.write_memo.get(&key) {
                                        out.extend(w.iter().copied());
                                    } else {
                                        cx.write_memo.insert(key, HashSet::new());
                                        let cmir = cx.inst_envs[&gi].mir;
                                        let mm = cx.itp.d.modules[cmir]
                                            .methods
                                            .iter()
                                            .find(|m| m.name == *method)
                                            .cloned();
                                        let mut w = HashSet::new();
                                        if let Some(mm) = mm {
                                            stmt_writes(cx, gi, &mm.body, &mut w);
                                        }
                                        out.extend(w.iter().copied());
                                        cx.write_memo.insert(key, w);
                                    }
                                }
                                None => {}
                            }
                        }
                    }
                    Stmt::Cond { then_, else_, .. } => {
                        stmt_writes(cx, inst, then_, out);
                        stmt_writes(cx, inst, else_, out);
                    }
                    Stmt::Def { .. } => {}
                }
            }
        }

        let mut cx = Ctx {
            itp: self,
            inst_envs,
            prim_cat: HashMap::new(),
            cone_memo: HashMap::new(),
            write_memo: HashMap::new(),
        };

        // per-ordinal exec write sets (all rules, once)
        let mut exec_writes: Vec<Vec<usize>> = Vec::with_capacity(specs_lite.len());
        let mut write_sets: Vec<HashSet<usize>> = Vec::with_capacity(specs_lite.len());
        for &(inst, ridx) in specs_lite {
            let mir = inst_envs[&inst].mir;
            let body = self.d.modules[mir].rules[ridx].body.clone();
            let mut w = HashSet::new();
            stmt_writes(&mut cx, inst, &body, &mut w);
            let mut v: Vec<usize> = w.iter().copied().collect();
            v.sort_unstable();
            exec_writes.push(v);
            write_sets.push(w);
        }

        let mut def_reads: HashMap<(usize, StrId), Vec<usize>> = HashMap::new();
        let mut hoists: Vec<Vec<Vec<(usize, StrId)>>> = Vec::with_capacity(nodes.len());
        // outline dial (link time): big NON-SHARING bodies stay
        // standalone.  Cost model (default): outline iff
        //   body_mass > max(FLOOR, FACTOR x consumed-sharable-mass)
        // — selects "large and shares little" directly instead of
        // hoping raw mass is a proxy (the sudoku knee showed monsters
        // are free to outline while mid-size sharers are not).
        // TRS_EDGE_SSA_OUTLINE=<mass> forces an absolute threshold;
        // TRS_EDGE_SSA_OUTLINE_FACTOR tunes the model (0 disables).
        let outline_abs: Option<u64> = std::env::var("TRS_EDGE_SSA_OUTLINE")
            .ok()
            .and_then(|v| v.parse().ok());
        let outline_factor: u64 = std::env::var("TRS_EDGE_SSA_OUTLINE_FACTOR")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(2);
        const OUTLINE_FLOOR: u64 = 800;
        let mut outlined_execs: std::collections::HashSet<usize> =
            forced_outline.clone();
        let mut tot_recompute = 0u64;
        let mut tot_saved = 0u64;
        let mut tot_gaps = 0usize;
        let mut tot_legal = 0usize;
        let mut tot_hoists = 0usize;
        let mut kills: HashMap<&'static str, usize> = HashMap::new();
        let mut poisoned: HashMap<&'static str, (usize, u64)> = HashMap::new();
        for (k, comp_nodes) in nodes.iter().enumerate() {
            // per-section body cones (exec sections only; scheds share
            // via the latched CF/WF/eager mechanism)
            let sections: Vec<Option<(Cone, usize)>> = comp_nodes
                .iter()
                .map(|&(is_exec, o)| {
                    if !is_exec {
                        return None;
                    }
                    let (inst, ridx) = specs_lite[o];
                    let mir = inst_envs[&inst].mir;
                    let body = self.d.modules[mir].rules[ridx].body.clone();
                    let mut c = Cone::default();
                    for st in body.iter() {
                        walk_stmt_defs(&mut cx, inst, st, &mut c);
                    }
                    Some((c, o))
                })
                .collect();
            // outline selection.  First pass: the SHARABLE def set
            // over ALL exec bodies (pure, unslotted, 2+ consumers) —
            // what a body would forfeit by leaving the mega-function.
            let mut all_counts: HashMap<(usize, StrId), usize> = HashMap::new();
            for sec in sections.iter().flatten() {
                for &d0 in &sec.0.defs {
                    *all_counts.entry(d0).or_insert(0) += 1;
                }
            }
            let sharable: HashMap<(usize, StrId), u64> = all_counts
                .iter()
                .filter(|(_, &c)| c >= 2)
                .filter_map(|(&(di, dn), _)| {
                    let dc = cone(&mut cx, di, dn);
                    (dc.mass > 0 && dc.pure() && {
                        let iev = &inst_envs[&di];
                        !iev.eager_slot.contains_key(&dn)
                            && !iev.cfwf_slot.contains_key(&dn)
                    })
                    .then_some(((di, dn), dc.mass))
                })
                .collect();
            // replication count per (mir, rule_idx): k instances of
            // the same module-type rule inline the same body k times
            // into the mega-edge, while ONE outlined body serves all
            // of them (per-module-type dedup)
            let mut type_reps: HashMap<(usize, usize), u64> = HashMap::new();
            for (sec, &(_, o)) in sections.iter().zip(comp_nodes.iter()) {
                if sec.is_some() {
                    let (inst, ridx) = specs_lite[o];
                    let mir = inst_envs[&inst].mir;
                    *type_reps.entry((mir, ridx)).or_insert(0) += 1;
                }
            }
            // second pass: outline iff large AND shares little.  The
            // floor amortizes over replication (grid v3: 1024 program
            // tiles inlined the same body 1024x — 202s link, 166s of
            // LLVM IR passes; Bluesim calls per-TYPE class methods).
            // Intra-tile sharing scales with k on both sides of the
            // comparison, so only the floor divides; k=1 designs keep
            // every existing decision.
            for (sec, &(_, o)) in sections.iter().zip(comp_nodes.iter()) {
                if let Some((c, _)) = sec {
                    let body_mass: u64 = c
                        .defs
                        .iter()
                        .map(|&(di, dn)| cone(&mut cx, di, dn).mass)
                        .sum();
                    let shared_mass: u64 = c
                        .defs
                        .iter()
                        .filter_map(|d0| sharable.get(d0))
                        .sum();
                    let (inst, ridx) = specs_lite[o];
                    let k = type_reps
                        .get(&(inst_envs[&inst].mir, ridx))
                        .copied()
                        .unwrap_or(1)
                        .max(1);
                    let outline = match outline_abs {
                        Some(t) => body_mass > t,
                        None => {
                            outline_factor > 0
                                && body_mass
                                    > (OUTLINE_FLOOR / k)
                                        .max(outline_factor * shared_mass)
                        }
                    };
                    if outline {
                        outlined_execs.insert(o);
                    }
                    if stats && body_mass > 400 {
                        eprintln!(
                            "trs edge-ssa: body o={o} mass={body_mass} \
                             shared={shared_mass} reps={k} outline={outline}"
                        );
                    }
                }
            }
            // consumers per (inst, def), section indices in order.
            // corder pins the def processing order deterministically
            // (HashMap iteration is process-seeded, and c.defs is a
            // HashSet, so both levels need pinning): first-consumer-
            // section major, (di, dn) within a section.  The ORDER
            // ITSELF is arbitrary — what matters is the topo pass
            // below, which puts deps before users per prelude so arm
            // expansion finds them in edge.shared instead of
            // re-emitting cones exponentially.
            let mut consumers: HashMap<(usize, StrId), Vec<usize>> = HashMap::new();
            let mut corder: Vec<(usize, StrId)> = Vec::new();
            for (p, (sec, &(_, o))) in
                sections.iter().zip(comp_nodes.iter()).enumerate()
            {
                if outlined_execs.contains(&o) {
                    continue;
                }
                if let Some((c, _)) = sec {
                    let mut ds: Vec<(usize, StrId)> =
                        c.defs.iter().copied().collect();
                    ds.sort_unstable();
                    for d0 in ds {
                        let e = consumers.entry(d0).or_default();
                        if e.is_empty() {
                            corder.push(d0);
                        }
                        e.push(p);
                    }
                }
            }
            let mut comp_hoists: Vec<Vec<(usize, StrId)>> =
                vec![Vec::new(); comp_nodes.len()];
            let mut comp_saved = 0u64;
            let mut comp_recompute = 0u64;
            let mut shared_defs = 0usize;
            for (di, dn) in corder {
                let ps = &consumers[&(di, dn)];
                if ps.len() < 2 {
                    continue;
                }
                let dc = cone(&mut cx, di, dn);
                if dc.mass == 0 {
                    continue; // body-local temp, not in the def table
                }
                shared_defs += 1;
                comp_recompute += dc.mass * (ps.len() as u64 - 1);
                if dc.poison != 0 {
                    for (bit, name) in [
                        (1u8, "arg-port"),
                        (2, "foreign"),
                        (4, "prim"),
                        (8, "en-port"),
                        (16, "rst-clk-port"),
                    ] {
                        if dc.poison & bit != 0 {
                            let e = poisoned.entry(name).or_insert((0usize, 0u64));
                            e.0 += 1;
                            e.1 += dc.mass * (ps.len() as u64 - 1);
                        }
                    }
                }
                // legality stats (anchor re-anchors on kill, anchor's
                // own writes included: the emitter post-evicts)
                let mut anchor = ps[0];
                for &pj in &ps[1..] {
                    tot_gaps += 1;
                    let killer = (anchor..pj).find_map(|q| {
                        sections[q].as_ref().and_then(|(_, o)| {
                            write_sets[*o].intersection(&dc.reads).next().copied()
                        })
                    });
                    match killer {
                        None => {
                            tot_legal += 1;
                            comp_saved += dc.mass;
                        }
                        Some(gi) => {
                            *kills
                                .entry(cx.prim_cat.get(&gi).copied().unwrap_or("?"))
                                .or_insert(0) += 1;
                            anchor = pj;
                        }
                    }
                }
                // emitter tables: only PURE, UNSLOTTED defs are cached/
                // hoisted (latched slots cover CF/WF/eager; impure cones
                // must never evaluate unconditionally)
                if !dc.pure() {
                    continue;
                }
                let iev = &inst_envs[&di];
                if iev.eager_slot.contains_key(&dn) || iev.cfwf_slot.contains_key(&dn)
                {
                    continue;
                }
                let mut reads: Vec<usize> = dc.reads.iter().copied().collect();
                reads.sort_unstable();
                def_reads.insert((di, dn), reads);
                // emitter-exact hoist walk: cache state tracks the
                // driver (pre/post-eviction on write intersection;
                // self-killing consumers never hoist — body-position
                // semantics)
                let mut cached = false;
                let mut pi = 0usize; // next consumer index in ps
                for (q, sec) in sections.iter().enumerate() {
                    let is_consumer = pi < ps.len() && ps[pi] == q;
                    let self_kill = sec.as_ref().is_some_and(|(_, o)| {
                        write_sets[*o].intersection(&dc.reads).next().is_some()
                    });
                    if is_consumer {
                        pi += 1;
                        if !cached && !self_kill {
                            comp_hoists[q].push((di, dn));
                            tot_hoists += 1;
                            cached = true;
                        }
                    }
                    if self_kill {
                        cached = false; // post-evict
                    }
                }
            }
            // topo-order each section's hoist prelude: deps before
            // users (Kahn; the ready set pops in pinned-corder position,
            // so the result is deterministic).  A dep materialized
            // before its user is found in edge.shared when lazy_mux
            // expands the user's arms — without that, BOTH arms of every
            // bit-test diamond re-expand the dep's cone and chained
            // folds (countOnes-style d_k = If(bit_k, d_k+1 +1, d_k+1))
            // emit 2^k-1 copies: memq's pinned order drew k=16 (47MB IR,
            // ir-passes 17s) where the old seed-random order drew k
            // stochastically (the historical ~13% bimodal link tail).
            for hq in comp_hoists.iter_mut() {
                if hq.len() < 2 {
                    continue;
                }
                let pos: HashMap<(usize, StrId), usize> = hq
                    .iter()
                    .copied()
                    .enumerate()
                    .map(|(i, d)| (d, i))
                    .collect();
                // dep edges restricted to this prelude (cross-section
                // deps are already in edge.shared: a dep's consumer set
                // is a superset of its user's, so it anchors no later)
                let deps: Vec<Vec<usize>> = hq
                    .iter()
                    .map(|&(di, dn)| {
                        let dc = cone(&mut cx, di, dn);
                        let mut v: Vec<usize> = dc
                            .defs
                            .iter()
                            .filter(|&&d| d != (di, dn))
                            .filter_map(|d| pos.get(d).copied())
                            .collect();
                        v.sort_unstable();
                        v
                    })
                    .collect();
                let mut indeg: Vec<usize> = deps.iter().map(|v| v.len()).collect();
                let mut users: Vec<Vec<usize>> = vec![Vec::new(); hq.len()];
                for (u, ds) in deps.iter().enumerate() {
                    for &d in ds {
                        users[d].push(u);
                    }
                }
                let mut ready: std::collections::BTreeSet<usize> = indeg
                    .iter()
                    .enumerate()
                    .filter(|&(_, &n)| n == 0)
                    .map(|(i, _)| i)
                    .collect();
                let mut order: Vec<usize> = Vec::with_capacity(hq.len());
                while let Some(&i) = ready.iter().next() {
                    ready.remove(&i);
                    order.push(i);
                    for &u in &users[i] {
                        indeg[u] -= 1;
                        if indeg[u] == 0 {
                            ready.insert(u);
                        }
                    }
                }
                if order.len() < hq.len() {
                    // cycle residue (unexpected for pure defs): append
                    // in pinned order — still deterministic
                    let inorder: HashSet<usize> = order.iter().copied().collect();
                    order.extend((0..hq.len()).filter(|i| !inorder.contains(i)));
                }
                let reordered: Vec<(usize, StrId)> =
                    order.iter().map(|&i| hq[i]).collect();
                *hq = reordered;
            }
            tot_recompute += comp_recompute;
            tot_saved += comp_saved;
            if stats && shared_defs > 0 {
                eprintln!(
                    "trs edge-ssa: comp {k}: sections={} shared-defs={shared_defs} \
                     mass shareable={comp_saved}/{comp_recompute}",
                    comp_nodes.len()
                );
            }
            hoists.push(comp_hoists);
        }
        if stats {
            let mut ks: Vec<_> = kills.iter().collect();
            ks.sort_by(|a, b| b.1.cmp(a.1));
            let ks: Vec<String> = ks.iter().map(|(c, n)| format!("{c}={n}")).collect();
            eprintln!(
                "trs edge-ssa: TOTAL gaps legal={tot_legal}/{tot_gaps} \
                 mass shareable={tot_saved}/{tot_recompute} hoists={tot_hoists} \
                 kills: {}",
                ks.join(" ")
            );
            let mut po: Vec<_> = poisoned.iter().collect();
            po.sort_by(|a, b| b.1 .1.cmp(&a.1 .1));
            let po: Vec<String> = po
                .iter()
                .map(|(n, (c, m))| format!("{n}={c}(mass {m})"))
                .collect();
            eprintln!("trs edge-ssa: poisoned shared defs: {}", po.join(" "));
        }
        // export keep-set (specialized compile: slot stores survive
        // only for COMPILED consumers — inhibitor loads and outlined
        // bodies; the slot-level debug contract is not part of the
        // edge-SSA artifact surface)
        let mut export_slots: std::collections::HashSet<u32> = specs
            .iter()
            .flat_map(|sp| sp.inhibit_slots.iter().copied())
            .collect();
        // interp-side consumers: with any early (clock-crossing) rule,
        // the PG_FINAL pass reads compiled CF slots (inhibitors via
        // latched_or_arena) and eager/WF defs via eval's arena
        // fallthrough — keep them all (early designs are rare; the
        // cost is per-edge scalar stores)
        if has_early {
            for e in inst_envs.values() {
                export_slots.extend(e.cfwf_slot.values().copied());
                export_slots.extend(e.eager_slot.values().map(|&(b, _)| b));
            }
        }
        for &o in &outlined_execs {
            export_slots.insert(specs[o].wf_slot);
            let (inst, ridx) = specs_lite[o];
            let mir = inst_envs[&inst].mir;
            let body = self.d.modules[mir].rules[ridx].body.clone();
            let mut c = Cone::default();
            for st in body.iter() {
                walk_stmt_defs(&mut cx, inst, st, &mut c);
            }
            for &(di, dn) in &c.defs {
                let iev = &inst_envs[&di];
                if let Some(&slot) = iev.cfwf_slot.get(&dn) {
                    export_slots.insert(slot);
                }
                if di == inst {
                    if let Some(&(base, _w)) = iev.eager_slot.get(&dn) {
                        export_slots.insert(base);
                    }
                }
            }
        }
        trs_codegen::abi::EdgeSsaPlan {
            nodes: nodes.to_vec(),
            exec_writes,
            def_reads,
            hoists,
            outlined_execs,
            wire_clears: Vec::new(),
            creg_copies: Vec::new(),
            bram_ticks: Vec::new(),
            // dynamic-scheduling variants are the CALLER's to fill
            // (jit_plan owns guard/inhibitor resolution); the plan
            // tables above already cover any variant rows appended to
            // `nodes` — this fn is row-uniform
            alt_rows: Vec::new(),
            sched_over: Vec::new(),
            ord_fnodes: HashMap::new(),
            export_slots,
        }
    }

    /// Call-site tables when the artifact supplied none: re-derive them
    /// by trial lowering (needs LLVM).  None = the plan is off, run
    /// interpreted (and Emit requests record their ineligibility).
    #[cfg(feature = "jit")]
    fn trial_protos(
        &mut self,
        inst_envs: &HashMap<usize, InstEnv>,
        specs: &[RuleSpec],
        now_slot: u32,
        request: &JitRequest,
        trace: bool,
    ) -> Option<Vec<FnProtos>> {
        let env = PlanEnv { d: &self.d, insts: inst_envs, now_slot };
        let t0 = std::time::Instant::now();
        match trial_lower(&env, specs) {
            Ok(p) => {
                if std::env::var_os("TRS_JIT_TIME").is_some() {
                    eprintln!("trs jit: trial lower {:?}", t0.elapsed());
                }
                Some(p)
            }
            Err(e) => {
                if let JitRequest::Emit { .. } = request {
                    self.jit_emit_result =
                        Some(crate::AotEmit::Ineligible(e.to_string()));
                }
                if trace {
                    eprintln!("trs jit: off ({e})");
                }
                None
            }
        }
    }

    /// No compile tier without `jit`: an artifact that loads without
    /// baked protos (pre-protos layouts are refused by the rev gate, so
    /// this is the artifact-load-failed path) runs interpreted.
    #[cfg(not(feature = "jit"))]
    fn trial_protos(
        &mut self,
        _inst_envs: &HashMap<usize, InstEnv>,
        _specs: &[RuleSpec],
        _now_slot: u32,
        _request: &JitRequest,
        trace: bool,
    ) -> Option<Vec<FnProtos>> {
        if trace {
            eprintln!("trs jit: off (no artifact protos and no compile tier)");
        }
        None
    }

    /// Build the JIT plan for the resolved compositions, or None to run
    /// fully interpreted.  Called once from prime().
    pub(crate) fn jit_plan(&mut self, rcomps: &[RComp]) -> Option<JitPlans> {
        let request = std::mem::take(&mut self.jit_request);
        // RunCore sidecar bookkeeping (validation form): an Emit plan
        // encodes its post-attach arena image at the tail for the
        // linker CLI to write beside the artifact; a Path load under
        // TRS_RUNCORE_CHECK=1 cross-checks its freshly built arena
        // against that image bit-for-bit
        let runcore_emit = matches!(request, JitRequest::Emit { .. });
        let runcore_sidecar: Option<std::path::PathBuf> = match &request {
            JitRequest::Load { src: ArtifactSource::Path(p) } => {
                Some(p.with_extension("arena"))
            }
            _ => None,
        };
        // early (clock-crossing) rules run interpreted in the PG_FINAL
        // pass and read compiled CF/eager slots — edge-SSA store
        // elision must keep those stores (see edge_ssa_plan)
        let has_early = rcomps.iter().any(|rc| !rc.early.is_empty());
        // direct-BDPI registries (task #22): baked-mode call emission
        // reads these; set-once, idempotent
        let _ = trs_codegen::abi::STDIO_CB.set(jit_stdio_cb as usize);
        if let Some(b) = &self.bdpi {
            // registry keys are C names (what call sites resolve)
            let m: std::collections::HashMap<String, usize> = b
                .syms()
                .iter()
                .map(|(n, &a)| {
                    let c = self
                        .d
                        .foreign_funcs
                        .iter()
                        .find(|ff| self.s(ff.name) == n)
                        .map(|ff| self.s(ff.c_name).to_string())
                        .unwrap_or_else(|| n.clone());
                    (c, a)
                })
                .collect();
            let _ = trs_codegen::abi::BDPI_SYMS.set(m);
        }
        // TRS_JIT=1 arms the hybrid; "0"/"off"/empty count as UNSET
        // (the old presence-only check made TRS_JIT=0 turn the JIT ON)
        let jit_env_on = std::env::var("TRS_JIT")
            .map(|v| !(v.is_empty() || v == "0" || v == "off"))
            .unwrap_or(false);
        if matches!(request, JitRequest::Run) && !jit_env_on && !self.jit_armed
        {
            return None;
        }
        let trace = std::env::var_os("TRS_JIT_TRACE").is_some();
        // BATCH waveform runs (-V / +bscvcd / +bscfst) stay fully
        // interpreted: compiled call sites don't maintain the boxed
        // prims' VCD bookkeeping (FIFO D_IN), so a compiled plan under
        // a runtime dump produced waveforms that were both wrong and
        // thread-timing nondeterministic.  wave_engine survives the
        // wave_pending take in prime (the old wave_pending check here
        // was dead — prime consumes it before planning).  Link-time
        // TRACED artifacts (the VCS/Verilator opt-in model) are the
        // fast path for dumps.
        if self.wave_engine || self.wave_pending.is_some() {
            if trace {
                eprintln!("trs jit: off (wave engine)");
            }
            return None;
        }
        // batch auto-fire of always_enabled top methods runs on the
        // interpreted entries loop: compiled call sites exist only
        // for parent-fused method calls, and the auto-fire positions
        // (segment cuts) are not in the compiled node stream — the
        // documented v1 engine contract for such designs is interp
        // (the regress battery and the sweep's engine column assert
        // this reason)
        if !self.autofire.is_empty() {
            if trace {
                eprintln!("trs jit: off (top always_enabled autofire)");
            }
            return None;
        }
        // dynamic-scheduling alternatives compile ONLY as edge-SSA
        // artifact emission — per-alternative bodies behind a
        // guard-dispatch prologue INSIDE each comp's edge fn — or a
        // Load of such an artifact (the dispatch ships in the .so;
        // the loader needs no variant plan).  Every other mode keeps
        // the interp fallback: the in-process JIT tier and
        // TRS_EDGE_SSA=0 emission drive standalone sched fns whose
        // inhibitor slots bake the BASE order (wrong under a selected
        // alternative), and traced plans would need per-variant
        // recording.  The alternative cap bounds edge-fn growth
        // (bodies multiply per alternative).
        let have_alts = rcomps.iter().any(|rc| !rc.alts.is_empty());
        let edge_ssa_on = std::env::var("TRS_EDGE_SSA").as_deref() != Ok("0");
        let alts_capped = rcomps.iter().all(|rc| rc.alts.len() <= 16);
        let alts_compiled = have_alts
            && alts_capped
            && match &request {
                JitRequest::Emit { .. } => edge_ssa_on && !self.vcd_trace,
                JitRequest::Load { .. } => true,
                JitRequest::Run => false,
            };
        if have_alts && !alts_compiled {
            if trace {
                eprintln!("trs jit: off (dynamic schedule)");
            }
            return None;
        }

        let mut sl = crate::startup::StartupLap::new();
        let mut nslots: u32 = 0;
        let alloc = |n: &mut u32, words: u32| {
            let s = *n;
            *n += words;
            s
        };

        // ---- pass A: collect scheduled rules (NO allocation) ----
        // Schedule order defines ordinals and shared-cone ownership;
        // slots are handed out in pass B per instance in
        // module-canonical order, so twin instances of one module type
        // get identical region-relative layouts (code dedup).
        struct RuleInfo {
            inst: usize,
            rule_idx: usize,
            ordinal: usize,
            cf_slot: u32,
            wf_slot: u32,
            eager: Vec<StrId>,
            shared: Vec<StrId>,
        }
        let mut rules: Vec<RuleInfo> = Vec::new();
        let mut rule_ord: HashMap<(usize, StrId), usize> = HashMap::new();
        for rc in rcomps {
            // clock-crossing "early" rules never enter the compiled
            // edge walk: the general loop's after-edge pass (PG_FINAL)
            // runs them interpreted over the same arena-backed state,
            // exactly like a cold exec cell — so they are SKIPPED here
            // (kept out of rule_ord and the node stream), not refused.
            // The central fast loop already bails on early comps.
            // eager defs owned by entries already walked in THIS comp,
            // per instance: later rules of the same instance may load
            // their slots instead of re-expanding the cone
            let mut owned_so_far: HashMap<usize, Vec<StrId>> = HashMap::new();
            for en in &rc.entries {
                for &node in &en.nodes {
                    let SchedNode::Sched(r) = node else { continue };
                    if rc.early.contains(&(en.inst, r)) {
                        continue; // after-edge pass runs it interpreted
                    }
                    if rule_ord.contains_key(&(en.inst, r)) {
                        continue;
                    }
                    let module = self.module_of(en.inst);
                    let mir = self.mods[module].ir;
                    // interface-method node in a segment: nothing to
                    // latch — the interp skips these identically (an
                    // external caller latches EN/args at call time;
                    // uncalled methods read EN as 0 through their
                    // arena slots), so the compiled walk omits them
                    let Some(&ri) = self.mods[module].rules.get(&r) else {
                        continue;
                    };
                    let shared =
                        owned_so_far.get(&en.inst).cloned().unwrap_or_default();
                    owned_so_far
                        .entry(en.inst)
                        .or_default()
                        .extend(en.eager.iter().copied());
                    rule_ord.insert((en.inst, r), rules.len());
                    rules.push(RuleInfo {
                        inst: en.inst,
                        rule_idx: ri,
                        ordinal: rules.len(),
                        cf_slot: 0,
                        wf_slot: 0,
                        eager: en.eager.clone(),
                        shared,
                    });
                    let _ = mir;
                }
            }
            // alternative interleavings reference the same segment
            // space as the base entries; a rule reachable ONLY
            // through an alternative still needs an ordinal.  Its
            // base-order share claims don't transfer, so it makes
            // none (the sched section re-expands its cone) — the
            // per-alternative override below never widens a claim.
            for alt in &rc.alts {
                for en in &alt.entries {
                    for &node in &en.nodes {
                        let SchedNode::Sched(r) = node else { continue };
                        if rc.early.contains(&(en.inst, r))
                            || rule_ord.contains_key(&(en.inst, r))
                        {
                            continue;
                        }
                        let module = self.module_of(en.inst);
                        let Some(&ri) = self.mods[module].rules.get(&r)
                        else {
                            continue;
                        };
                        rule_ord.insert((en.inst, r), rules.len());
                        rules.push(RuleInfo {
                            inst: en.inst,
                            rule_idx: ri,
                            ordinal: rules.len(),
                            cf_slot: 0,
                            wf_slot: 0,
                            eager: en.eager.clone(),
                            shared: Vec::new(),
                        });
                    }
                }
            }
        }
        let mut per_inst_rules: HashMap<usize, Vec<usize>> = HashMap::new();
        for (k, ri) in rules.iter().enumerate() {
            per_inst_rules.entry(ri.inst).or_default().push(k);
        }
        for v in per_inst_rules.values_mut() {
            v.sort_by_key(|&k| rules[k].rule_idx);
        }

        // ---- outline selection (TRS_JIT_SPLIT=<thresh> opt-in) ----
        // per module type: which def pieces become helper fns, and
        // which of those are per-instant memoizable.  Eager-set defs
        // are excluded: a helper body hitting the eager-slot fast path
        // could read slots whose owners have not run yet.
        let split_thresh: Option<u32> = std::env::var("TRS_JIT_SPLIT")
            .ok()
            .and_then(|v| v.parse().ok())
            .filter(|&t| t > 0);
        let outlined_sel: HashMap<(usize, StrId), (u32, bool, Vec<StrId>)> = if let Some(th) =
            split_thresh
        {
            let mut exemplar: HashMap<usize, usize> = HashMap::new();
            for i in 0..self.insts.len() {
                if let InstKind::User { module, .. } = &self.insts[i].kind {
                    exemplar.entry(self.mods[*module].ir).or_insert(i);
                }
            }
            let mut eager_excl: std::collections::HashSet<(usize, StrId)> =
                Default::default();
            for ri in &rules {
                let mir = self.mods[self.module_of(ri.inst)].ir;
                for &e in &ri.eager {
                    eager_excl.insert((mir, e));
                }
            }
            let insts = &self.insts;
            let mods = &self.mods;
            let ex2 = exemplar.clone();
            let kind = move |m: usize, name: StrId| -> ChildRef {
                let Some(&ex) = ex2.get(&m) else { return ChildRef::Opaque };
                let InstKind::User { children, .. } = &insts[ex].kind else {
                    return ChildRef::Opaque;
                };
                let Some(&ci) =
                    children.iter().find(|(k, _)| **k == name).map(|(_, v)| v)
                else {
                    return ChildRef::Opaque;
                };
                match &insts[ci].kind {
                    InstKind::Prim(p) => ChildRef::Prim(match p.arena_kind() {
                        Some(ArenaKind::Reg { .. }) => ChildClass::Reg,
                        Some(ArenaKind::ConfigReg { .. }) => ChildClass::CfgReg,
                        Some(ArenaKind::Wire { .. }) => ChildClass::Wire,
                        Some(ArenaKind::Fifo { loopy, .. }) => ChildClass::Fifo { loopy },
                        // arena-backed but NO stability contract and
                        // reads can WARN (bounds): the split analyzer
                        // treats it like an opaque prim
                        Some(ArenaKind::RegFile { .. }) => ChildClass::Other,
                        // arena-backed; reads are begin-of-instant
                        // (out changes only at tick) but keep the
                        // conservative class until the compiled tier
                        // exploits it
                        Some(ArenaKind::Bram { .. }) => ChildClass::Other,
                        // live port-chained reads: never stable
                        Some(ArenaKind::CReg5 { .. }) => ChildClass::Other,
                        // begin-of-instant reads, but keep the
                        // conservative class (no stability claim yet)
                        Some(ArenaKind::Counter { .. }) => ChildClass::Other,
                        None => ChildClass::Other,
                    }),
                    InstKind::User { module, .. } => ChildRef::User(mods[*module].ir),
                }
            };
            let mut an = ConeAnalyzer::new(&self.d, &kind, th);
            let mut sel = HashMap::new();
            let mut mirs: Vec<usize> = exemplar.keys().copied().collect();
            mirs.sort_unstable();
            for mir in mirs {
                for (name, pi) in an.module(mir) {
                    if pi.outlined && !eager_excl.contains(&(mir, name)) {
                        // self.mods[mir].defs is the prebuilt name index
                        let w = self.mods[mir]
                            .defs
                            .get(&name)
                            .map(|&i| self.d.modules[mir].defs[i].width.max(1))
                            .unwrap_or(1);
                        let stable = pi.stable && pi.ports.is_empty();
                        sel.insert((mir, name), (w, stable, pi.ports.clone()));
                    }
                }
            }
            if trace {
                eprintln!(
                    "trs jit: split thresh={th}: {} pieces ({} memoized)",
                    sel.len(),
                    sel.values().filter(|(_, st, _)| *st).count()
                );
            }
            sel
        } else {
            HashMap::new()
        };

        sl.lap("plan passA (rule collect)");
        // ---- pass B: DFS subtree-contiguous allocation ----
        // Every slot an instance's compiled code touches (its prims,
        // ENs, rule cf/wf/eager, and everything in its submodule
        // subtree) lands in one contiguous region, at offsets that are
        // uniform across instances of the same module type.
        let mut inst_envs: HashMap<usize, InstEnv> = HashMap::new();
        let mut attach: Vec<(usize, u32)> = Vec::new(); // (prim inst, base)
        let reset_node_slot: Vec<u32> =
            (0..self.rst_asserted.len()).map(|_| alloc(&mut nslots, 1)).collect();
        // the dispatcher stamps the current instant here at every edge
        let now_slot = alloc(&mut nslots, 1);
        // memo stamp slots initialize to u64::MAX (0 == instant 0)
        let mut memo_stamp_slots: Vec<u32> = Vec::new();
        let mut is_child = vec![false; self.insts.len()];
        for i in 0..self.insts.len() {
            if let InstKind::User { children, .. } = &self.insts[i].kind {
                for (_, &c) in children.iter() {
                    is_child[c] = true;
                }
            }
        }
        enum Walk {
            Enter(usize),
            Exit(usize),
        }
        let mut stack: Vec<Walk> = (0..self.insts.len())
            .rev()
            .filter(|&i| {
                !is_child[i] && matches!(self.insts[i].kind, InstKind::User { .. })
            })
            .map(Walk::Enter)
            .collect();
        let mut subtree: HashMap<usize, (u32, u32)> = HashMap::new();
        let mut dfs_order: Vec<usize> = Vec::new();
        // traced artifacts: per-module VCD var selection (the same walk
        // the writer uses) drives recording-slot allocation below
        let rec_mvs: HashMap<usize, std::rc::Rc<crate::ModVars>> = if self.vcd_trace {
            (0..self.mods.len()).map(|mi| (mi, self.vcd_mod_vars(mi))).collect()
        } else {
            HashMap::new()
        };
        let mut rec_inits: Vec<(u32, u64)> = Vec::new();
        while let Some(w) = stack.pop() {
            let i = match w {
                Walk::Exit(i) => {
                    subtree.get_mut(&i).expect("exit before enter").1 = nslots;
                    continue;
                }
                Walk::Enter(i) => i,
            };
            let InstKind::User {
                module, children, resets, params, str_params, gates, ..
            } = &self.insts[i].kind
            else {
                continue;
            };
            dfs_order.push(i);
            let region_start = nslots;
            let module = *module;
            let mir = self.mods[module].ir;
            let children: HashMap<StrId, usize> =
                children.iter().map(|(k, v)| (*k, *v)).collect();
            let mut reg_slot = HashMap::new();
            let mut wire_slot = HashMap::new();
            let mut creg_slot = HashMap::new();
            let mut fifo_slot = HashMap::new();
            let mut regfile_slot = HashMap::new();
            let mut bram_slot = HashMap::new();
            let mut creg5_slot = HashMap::new();
            let mut counter_slot = HashMap::new();
            // sorted iteration: slot assignment must be deterministic
            // across processes so an AOT artifact's baked slot numbers
            // match a fresh planning walk at load time
            let mut kids: Vec<(StrId, usize)> =
                children.iter().map(|(&k, &v)| (k, v)).collect();
            kids.sort_unstable();
            for &(name, ci) in &kids {
                let InstKind::Prim(p) = &self.insts[ci].kind else { continue };
                match p.arena_kind() {
                    Some(ArenaKind::Reg { width }) => {
                        let base = alloc(&mut nslots, width.div_ceil(64).max(1));
                        reg_slot.insert(name, (base, width));
                        attach.push((ci, base));
                    }
                    Some(ArenaKind::Wire { width }) => {
                        let base = alloc(&mut nslots, 1 + width.max(1).div_ceil(64));
                        wire_slot.insert(name, (base, width));
                        attach.push((ci, base));
                    }
                    Some(ArenaKind::ConfigReg { width }) => {
                        let words = width.max(1).div_ceil(64);
                        let base = alloc(&mut nslots, 2 * words + 1);
                        creg_slot.insert(name, (base, width));
                        attach.push((ci, base));
                    }
                    // traced plans keep FIFOs boxed: the D_IN port var
                    // dumps the last ATTEMPTED enq value (dummyval),
                    // maintained only by the boxed enq path, which
                    // inline compiled enqs bypass (CReg5/Counter
                    // precedent)
                    Some(ArenaKind::Fifo { .. }) if self.vcd_trace => {}
                    Some(ArenaKind::Fifo { width, size, guard, loopy }) => {
                        let words = width.max(1).div_ceil(64);
                        let base = alloc(&mut nslots, 7 + size * words);
                        fifo_slot.insert(name, (base, width, size, guard, loopy));
                        attach.push((ci, base));
                    }
                    Some(ArenaKind::RegFile { width, lo, hi }) => {
                        let words = width.max(1).div_ceil(64);
                        let entries = (hi - lo + 1) as u32;
                        let base =
                            alloc(&mut nslots, 2 + words * (1 + entries));
                        regfile_slot.insert(name, (base, width, lo, hi));
                        attach.push((ci, base));
                    }
                    // traced plans keep CRegs boxed: the per-port VCD
                    // bookkeeping (EN/D_IN history) lives in the boxed
                    // write path, which inline compiled writes bypass
                    Some(ArenaKind::CReg5 { .. }) if self.vcd_trace => {}
                    Some(ArenaKind::CReg5 { width }) => {
                        let base =
                            alloc(&mut nslots, 2 * width.max(1).div_ceil(64));
                        creg5_slot.insert(name, (base, width));
                        attach.push((ci, base));
                    }
                    // traced plans keep Counters boxed: the port VCD
                    // bookkeeping (a/b/c/f values + stamps) lives in
                    // the boxed action path, which inline adds bypass
                    Some(ArenaKind::Counter { .. }) if self.vcd_trace => {}
                    Some(ArenaKind::Counter { width }) => {
                        let w = width.max(1).div_ceil(64);
                        let base = alloc(&mut nslots, 4 * w + 4);
                        counter_slot.insert(name, (base, width));
                        attach.push((ci, base));
                    }
                    Some(ArenaKind::Bram {
                        width,
                        size,
                        chunk_size,
                        num_wens,
                        dual,
                        pipelined,
                    }) => {
                        let w = width.max(1).div_ceil(64);
                        let wenw = num_wens.max(1).div_ceil(64);
                        let pw = 3 + wenw + 4 * w;
                        let ports = if dual { 2 } else { 1 };
                        let base = alloc(
                            &mut nslots,
                            pw * ports + (size as u32) * w,
                        );
                        bram_slot.insert(
                            name,
                            (base, width, size, chunk_size, num_wens, dual, pipelined),
                        );
                        attach.push((ci, base));
                    }
                    None => {}
                }
            }
            // VCD recording slots (traced artifacts): one block per
            // declared member def (undet-initialized, the writer's
            // never-evaluated default), plus per-method port blocks —
            // EN time (u64::MAX = never), every argument, the result
            let mut rec_defs: HashMap<StrId, (u32, u32)> = HashMap::new();
            let mut rec_meths: HashMap<StrId, RecMeth> = HashMap::new();
            if let Some(mv) = rec_mvs.get(&module) {
                let irm = &self.d.modules[mir];
                let mut meth_names: Vec<StrId> = Vec::new();
                for var in mv.members.iter().chain(mv.ports.iter()) {
                    match &var.src {
                        crate::VcdSrc::Def(n) => {
                            let w = var.width.max(1);
                            let base = alloc(&mut nslots, w.div_ceil(64));
                            let u = Value::undet(w);
                            for (k, l) in u.limbs64().iter().enumerate() {
                                rec_inits.push((base + k as u32, *l));
                            }
                            rec_defs.insert(*n, (base, var.width));
                        }
                        crate::VcdSrc::PortEn(mn)
                        | crate::VcdSrc::PortArg(mn, _)
                        | crate::VcdSrc::PortRes(mn) => {
                            if !meth_names.contains(mn) {
                                meth_names.push(*mn);
                            }
                        }
                        crate::VcdSrc::Reset(_) => {}
                    }
                }
                for mn in meth_names {
                    let Some(me) = irm.methods.iter().find(|me| me.name == mn)
                    else {
                        continue;
                    };
                    let t = alloc(&mut nslots, 1);
                    rec_inits.push((t, u64::MAX));
                    // every arg gets a slot (the submodule port dump
                    // reads args the module-scope selection skipped)
                    let args: Vec<(u32, u32)> = me
                        .args
                        .iter()
                        .map(|a| {
                            let w = a.width.max(1);
                            let b = alloc(&mut nslots, w.div_ceil(64));
                            for k in 0..w.div_ceil(64) {
                                rec_inits.push((b + k, 0));
                            }
                            (b, a.width)
                        })
                        .collect();
                    let res = me.result.as_ref().map(|r| {
                        let w = match r {
                            Expr::Def(n) => irm
                                .defs
                                .iter()
                                .find(|d| d.name == *n)
                                .map(|d| d.width)
                                .unwrap_or(0),
                            Expr::Port(n) => irm
                                .inputs
                                .iter()
                                .find(|p| p.name == *n)
                                .map(|p| p.width)
                                .unwrap_or(0),
                            e => e.width(),
                        }
                        .max(1);
                        let b = alloc(&mut nslots, w.div_ceil(64));
                        for k in 0..w.div_ceil(64) {
                            rec_inits.push((b + k, 0));
                        }
                        (b, w)
                    });
                    rec_meths.insert(mn, RecMeth { t, args, res });
                }
            }
            let reset_slot: HashMap<StrId, u32> = resets
                .iter()
                .map(|(port, node)| (*port, reset_node_slot[*node]))
                .collect();
            // every EN_* port gets a slot (zeroed per dispatch, stored
            // by compiled call sites; method WF cones read them)
            let mut en_slot = HashMap::new();
            let mut enps: Vec<StrId> = self.mods[module]
                .ports
                .iter()
                .filter(|&(_, &(_w, kind))| kind == ir::PortKind::MethodEnable)
                .map(|(&pn, _)| pn)
                .collect();
            enps.sort_unstable();
            for pname in enps {
                en_slot.insert(pname, alloc(&mut nslots, 1));
            }
            // per-rule cf/wf slots in module-canonical rule order, then
            // the instance's eager-def UNION in sorted order: eager
            // attachment (first-Sched-node) can split differently
            // between twin instances, but the union and this layout
            // stay type-uniform (dedup depends on it)
            let mut cfwf_slot = HashMap::new();
            let mut eager_slot: HashMap<StrId, (u32, u32)> = HashMap::new();
            if let Some(rks) = per_inst_rules.get(&i) {
                for &k in rks {
                    let cf_slot = alloc(&mut nslots, 1);
                    let wf_slot = alloc(&mut nslots, 1);
                    let rr = &self.d.modules[mir].rules[rules[k].rule_idx];
                    cfwf_slot.insert(rr.can_fire, cf_slot);
                    cfwf_slot.insert(rr.will_fire, wf_slot);
                    rules[k].cf_slot = cf_slot;
                    rules[k].wf_slot = wf_slot;
                }
                let mut union: Vec<StrId> = Vec::new();
                for &k in rks {
                    for &e in &rules[k].eager {
                        if !union.contains(&e) {
                            union.push(e);
                        }
                    }
                }
                union.sort_unstable();
                for e in union {
                    let Some(ed) = self.mods[mir]
                        .defs
                        .get(&e)
                        .map(|&i| &self.d.modules[mir].defs[i])
                    else {
                        if trace {
                            eprintln!("trs jit: off (eager def unknown)");
                        }
                        return None;
                    };
                    let ew = ed.width.max(1);
                    let base = alloc(&mut nslots, ew.div_ceil(64));
                    eager_slot.insert(e, (base, ew));
                }
            }
            // memo slots for outlined stable defs of this module type
            // (sorted: type-uniform offsets, part of the dedup sig)
            let mut memo_slot: HashMap<StrId, (u32, u32)> = HashMap::new();
            {
                let mut ms: Vec<(StrId, u32)> = outlined_sel
                    .iter()
                    .filter(|((m, _), (_, st, _))| *m == mir && *st)
                    .map(|((_, dn), (w, _, _))| (*dn, *w))
                    .collect();
                ms.sort_unstable();
                for (dn, w) in ms {
                    let base = alloc(&mut nslots, 1 + w.div_ceil(64));
                    memo_slot.insert(dn, (base, w));
                    memo_stamp_slots.push(base);
                }
            }
            subtree.insert(i, (region_start, 0));
            stack.push(Walk::Exit(i));
            for &(_, c) in kids.iter().rev() {
                if matches!(self.insts[c].kind, InstKind::User { .. }) {
                    stack.push(Walk::Enter(c));
                }
            }
            // constant-valued input ports and parameters — the compiled
            // mirror of the interpreter's Port/Param fallthrough
            // (uncalled MethodArg reads 0, unbound clock/gate/reset-kind
            // ports read 1, numeric params read their bound value).
            // Dynamic bindings stay out: bound gates evaluate in the
            // parent (MCD), EN/reset ports have arena slots, string
            // params are marker values, and unslotted method enables
            // stay ineligible rather than folding to a wrong constant.
            // A BOUND value the u64 fold cannot carry (wide args,
            // Real's marker width) must stay out of the unbound-port
            // arms below too — the interp resolves `params` before any
            // read-as-1/0 fallthrough, so folding such a name as
            // "unbound" bakes a wrong constant (sysWideModArgPortTest,
            // sysTwoLevelReal2); unfolded means Ineligible -> interp.
            let mut port_consts: HashMap<StrId, (u32, u64)> = HashMap::new();
            let mut real_consts: HashMap<StrId, u64> = HashMap::new();
            let mut wide_consts: HashMap<StrId, (u32, Vec<u32>)> = HashMap::new();
            for (&pn, pv) in params {
                if pv.width >= 1 && pv.width <= 64 {
                    port_consts.insert(pn, (pv.width, pv.as_u64()));
                } else if let Some(r) = pv.as_real() {
                    // real params ride as f64 bits (task-arg carrier)
                    real_consts.insert(pn, r.to_bits());
                } else if pv.width > 64 && pv.width < u32::MAX - 1 {
                    // wide instantiation values as LE 32-bit limbs
                    let mut limbs = Vec::new();
                    for &l in pv.limbs64() {
                        limbs.push(l as u32);
                        limbs.push((l >> 32) as u32);
                    }
                    wide_consts.insert(pn, (pv.width, limbs));
                }
            }
            for (&pn, &(w, kind)) in &self.mods[module].ports {
                if port_consts.contains_key(&pn)
                    || params.contains_key(&pn)
                    || en_slot.contains_key(&pn)
                    || reset_slot.contains_key(&pn)
                    || gates.contains_key(&pn)
                    || str_params.contains_key(&pn)
                {
                    continue;
                }
                match kind {
                    trs_ir::PortKind::MethodArg => {
                        // LOGICAL width, zero included: the interp's
                        // fallback masks from_u64(0, _) to the empty
                        // vector, so a zero-width port must not become
                        // a width-1 value (review finding)
                        port_consts.insert(pn, (w, 0));
                    }
                    trs_ir::PortKind::MethodEnable => {}
                    _ => {
                        port_consts.insert(pn, (w, if w == 0 { 0 } else { 1 }));
                    }
                }
            }
            inst_envs.insert(
                i,
                InstEnv {
                    mir,
                    children,
                    reg_slot,
                    wire_slot,
                    creg_slot,
                    fifo_slot,
                    regfile_slot,
                    bram_slot,
                    creg5_slot,
                    counter_slot,
                    reset_slot,
                    en_slot,
                    cfwf_slot,
                    eager_slot,
                    memo_slot,
                    port_consts,
                    real_consts,
                    gates: gates.clone(),
                    str_consts: str_params.clone(),
                    wide_consts,
                    rec_defs,
                    rec_meths,
                    region: (region_start, 0),
                },
            );
        }
        // subtree extents (known only after the whole subtree walked)
        for (i, &(s0, s1)) in &subtree {
            if let Some(e) = inst_envs.get_mut(i) {
                e.region = (s0, s1);
            }
        }

        sl.lap("plan passB (slot alloc + inst envs)");
        // baked always-fire bits + dedup classes first (Load
        // requests): skip the WILL_FIRE alias walks (they force lazy
        // expr decodes) and the class derivation below.  Gated on the
        // salted hash, so everything here is exactly what derivation
        // would produce (same design, same trace mode, same layout
        // rev) — the in-process-compile fallback stays consistent if
        // aot_load fails later.
        let mut baked: Option<(Vec<u8>, Vec<(usize, Vec<usize>)>)> = None;
        if let JitRequest::Load { src } = &request {
            let mut psl = crate::startup::StartupLap::new();
            baked = aot_plan_b(
                src,
                self.bir_hash ^ (self.vcd_trace as u64 * 0x5452_4143_4544),
            );
            if baked.is_some() {
                psl.lap("plan-b (baked decode)");
            }
        }
        let (baked_af, baked_classes) = match baked {
            Some((a, c)) => (Some(a), Some(c)),
            None => (None, None),
        };
        // ---- per-instance subtree signatures (exec dedup classes) ----
        // Two instances share compiled exec bodies iff their signatures
        // match.  The sig must cover EVERY input the exec lowering
        // reads: module IR id, region-relative slot layout (all maps),
        // absolute reset-node slots, and the user children recursively.
        // (Stage-2a made twin IR raw-identical; the sweep + twin test
        // referee this invariant.)  Consumed by the class derivation
        // (skipped when classes are baked) and by helper symbol names
        // (only when outlining selected pieces).
        let inst_sig: HashMap<usize, u64> = if baked_classes.is_none()
            || !outlined_sel.is_empty()
        {
            use std::hash::{Hash, Hasher};
            let mut sigs: HashMap<usize, u64> = HashMap::new();
            for &i in dfs_order.iter().rev() {
                let e = &inst_envs[&i];
                let mut h = std::collections::hash_map::DefaultHasher::new();
                e.mir.hash(&mut h);
                (e.region.1 - e.region.0).hash(&mut h);
                let r0 = e.region.0;
                let mut m1: Vec<_> =
                    e.reg_slot.iter().map(|(&k, &(b, w))| (k, b - r0, w)).collect();
                m1.sort_unstable();
                m1.hash(&mut h);
                let mut m2: Vec<_> =
                    e.wire_slot.iter().map(|(&k, &(b, w))| (k, b - r0, w)).collect();
                m2.sort_unstable();
                m2.hash(&mut h);
                let mut m3: Vec<_> =
                    e.creg_slot.iter().map(|(&k, &(b, w))| (k, b - r0, w)).collect();
                m3.sort_unstable();
                m3.hash(&mut h);
                let mut m4: Vec<_> = e
                    .fifo_slot
                    .iter()
                    .map(|(&k, &(b, w, sz, g, lp))| (k, b - r0, w, sz, g, lp))
                    .collect();
                m4.sort_unstable();
                m4.hash(&mut h);
                let mut m5: Vec<_> =
                    e.en_slot.iter().map(|(&k, &b)| (k, b - r0)).collect();
                m5.sort_unstable();
                m5.hash(&mut h);
                let mut m6: Vec<_> =
                    e.cfwf_slot.iter().map(|(&k, &b)| (k, b - r0)).collect();
                m6.sort_unstable();
                m6.hash(&mut h);
                let mut m7: Vec<_> =
                    e.eager_slot.iter().map(|(&k, &(b, w))| (k, b - r0, w)).collect();
                m7.sort_unstable();
                m7.hash(&mut h);
                // reset nodes are design-global: absolute slots baked
                let mut m8: Vec<_> =
                    e.reset_slot.iter().map(|(&k, &b)| (k, b)).collect();
                m8.sort_unstable();
                m8.hash(&mut h);
                let mut m9: Vec<_> =
                    e.memo_slot.iter().map(|(&k, &(b, w))| (k, b - r0, w)).collect();
                m9.sort_unstable();
                m9.hash(&mut h);
                // params/const-ports are baked into compiled bodies:
                // instances of one module type with different param
                // values must not share exec code
                let mut m11: Vec<_> =
                    e.port_consts.iter().map(|(&k, &(w, v))| (k, w, v)).collect();
                m11.sort_unstable();
                m11.hash(&mut h);
                let mut m12: Vec<_> =
                    e.real_consts.iter().map(|(&k, &v)| (k, v)).collect();
                m12.sort_unstable();
                m12.hash(&mut h);
                // gate wiring pins the sig: owner slots are ABSOLUTE in
                // deduped bodies, so instances gated differently (other
                // owner, other expr) must never share exec code
                let mut m13: Vec<_> = e
                    .gates
                    .iter()
                    .map(|(&k, (o, g))| (k, *o, format!("{g:?}")))
                    .collect();
                m13.sort_unstable();
                m13.hash(&mut h);
                let mut m14: Vec<_> =
                    e.str_consts.iter().map(|(&k, &v)| (k, v)).collect();
                m14.sort_unstable();
                m14.hash(&mut h);
                let mut m15: Vec<_> = e
                    .wide_consts
                    .iter()
                    .map(|(&k, (w, l))| (k, *w, l.clone()))
                    .collect();
                m15.sort_unstable();
                m15.hash(&mut h);
                // the sig must cover every input the exec lowering
                // reads (handoff rule): regfile regions included
                let mut m10: Vec<_> = e
                    .regfile_slot
                    .iter()
                    .map(|(&k, &(b, w, lo, hi))| (k, b - r0, w, lo, hi))
                    .collect();
                m10.sort_unstable();
                m10.hash(&mut h);
                let mut m19: Vec<_> = e
                    .creg5_slot
                    .iter()
                    .map(|(&k, &(b, w))| (k, b - r0, w))
                    .collect();
                m19.sort_unstable();
                m19.hash(&mut h);
                let mut m20: Vec<_> = e
                    .counter_slot
                    .iter()
                    .map(|(&k, &(b, w))| (k, b - r0, w))
                    .collect();
                m20.sort_unstable();
                m20.hash(&mut h);
                let mut m18: Vec<_> = e
                    .bram_slot
                    .iter()
                    .map(|(&k, &(b, w, sz, cs, nw, du, pl))| {
                        (k, b - r0, w, sz, cs, nw, du, pl)
                    })
                    .collect();
                m18.sort_unstable();
                m18.hash(&mut h);
                // traced artifacts: recording layout is an exec input
                let mut m16: Vec<_> = e
                    .rec_defs
                    .iter()
                    .map(|(&k, &(b, w))| (k, b - r0, w))
                    .collect();
                m16.sort_unstable();
                m16.hash(&mut h);
                let mut m17: Vec<_> = e
                    .rec_meths
                    .iter()
                    .map(|(&k, rm)| {
                        (
                            k,
                            rm.t - r0,
                            rm.args
                                .iter()
                                .map(|&(b, w)| (b - r0, w))
                                .collect::<Vec<_>>(),
                            rm.res.map(|(b, w)| (b - r0, w)),
                        )
                    })
                    .collect();
                m17.sort_unstable();
                m17.hash(&mut h);
                let mut kids: Vec<_> = e
                    .children
                    .iter()
                    .filter_map(|(&n, &c)| sigs.get(&c).map(|&sg| (n, sg)))
                    .collect();
                kids.sort_unstable();
                kids.hash(&mut h);
                sigs.insert(i, h.finish());
            }
            sigs
        } else {
            HashMap::new()
        };

        // any Exec node of a RULE must belong to a scheduled rule
        // above; interface-method Exec nodes are no-ops (skipped by
        // the interp and by comp_nodes below)
        for rc in rcomps {
            for en in &rc.entries {
                let module = self.module_of(en.inst);
                for &node in &en.nodes {
                    let SchedNode::Exec(r) = node else { continue };
                    if !self.mods[module].rules.contains_key(&r) {
                        continue; // method exec node
                    }
                    if rc.early.contains(&(en.inst, r)) {
                        continue; // after-edge pass runs it interpreted
                    }
                    if !rule_ord.contains_key(&(en.inst, r)) {
                        if trace {
                            eprintln!("trs jit: off (exec without sched)");
                        }
                        return None;
                    }
                }
            }
        }

        sl.lap("plan sigs (inst_sig hashing)");
        // one design-wide spec list (ordinal order)
        let mut specs = Vec::new();
        // per ordinal: (ME-inhibitor slot count, per-comp base cross
        // slots) — the compiled-alts variant builder recomposes
        // inhibitors as ME + other comps' base cross + this comp's
        // ALTERNATIVE cross (a union over interleavings would inhibit
        // rules that do not precede the victim in the selected order)
        let mut sched_parts: Vec<(usize, Vec<Vec<u32>>)> = Vec::new();
        for ri in &rules {
            let mir = inst_envs[&ri.inst].mir;
            let rr = &self.d.modules[mir].rules[ri.rule_idx];
            let module = self.module_of(ri.inst);
            let mut inhibit_slots = Vec::new();
            for other in &rr.me_inhibits {
                let other_ri = self.mods[module].rules[other];
                let other_cf = self.d.modules[mir].rules[other_ri].can_fire;
                match inst_envs[&ri.inst].cfwf_slot.get(&other_cf) {
                    Some(&s) => inhibit_slots.push(s),
                    None => {
                        if trace {
                            eprintln!("trs jit: off (unslotted ME inhibitor)");
                        }
                        return None;
                    }
                }
            }
            let me_len = inhibit_slots.len();
            let mut cross_by_comp: Vec<Vec<u32>> =
                Vec::with_capacity(rcomps.len());
            for rc in rcomps {
                let mut cv = Vec::new();
                if let Some(cs) = rc.cross.get(&(ri.inst, rr.name)) {
                    for (oi, ocf) in cs {
                        match inst_envs.get(oi).and_then(|e| e.cfwf_slot.get(ocf)) {
                            Some(&s) => cv.push(s),
                            None => {
                                if trace {
                                    eprintln!(
                                        "trs jit: off (unslotted cross inhibitor)"
                                    );
                                }
                                return None;
                            }
                        }
                    }
                }
                inhibit_slots.extend(cv.iter().copied());
                cross_by_comp.push(cv);
            }
            sched_parts.push((me_len, cross_by_comp));
            // always-fire detection (task #23): the WILL_FIRE def
            // resolves (through Def aliases) to a constant-true value.
            // Only WF is the truth: bsc bakes preemption/urgency gating
            // into the WF def EXPRESSION (WF_a = CF_a && !WF_b), never
            // into me_inhibits — a const-true CAN_FIRE says nothing
            // (sysEspositoPreempt/sysRegFileVector regression).
            let always_fire = if let Some(af) = &baked_af {
                af.get(ri.ordinal).is_some_and(|&b| b != 0)
            } else {
                // self.mods[mir].defs is the prebuilt name index
                let didx = &self.mods[mir].defs;
                let const_true = |name: StrId| -> bool {
                    let defs = &self.d.modules[mir].defs;
                    let mut cur = name;
                    for _ in 0..32 {
                        let Some(dd) = didx.get(&cur).map(|&i| &defs[i]) else {
                            return false;
                        };
                        match &*dd.expr {
                            trs_ir::Expr::Const { limbs, .. } => {
                                return limbs.iter().any(|&l| l != 0)
                            }
                            trs_ir::Expr::Def(n) => cur = *n,
                            _ => return false,
                        }
                    }
                    false
                };
                inhibit_slots.is_empty() && const_true(rr.will_fire)
            };
            specs.push(RuleSpec {
                always_fire,
                inst: ri.inst,
                rule_idx: ri.rule_idx,
                inhibit_slots,
                cf_slot: ri.cf_slot,
                wf_slot: ri.wf_slot,
                eager: ri.eager.clone(),
                shared: ri.shared.clone(),
                label: format!("i{}_{}", ri.inst, ri.ordinal),
                token_base: (ri.ordinal as u64) << 17,
            });
        }
        // edge-SSA shareability analysis (task #24 M1,
        // TRS_EDGE_SSA_STATS=1): for every def consumed by 2+ exec
        // bodies in a composition, decide gap-wise whether the value
        // computed at the first consumer's position is still valid at
        // each later consumer — i.e. no intervening exec writes state
        // the def's cone reads UNSTABLY.  Stability is value-level prim
        // contract only (doctrine d97b7e4a): ConfigReg reads and FIFO
        // i_* views see begin-of-instant state, so intervening writes
        // to them cannot kill; everything else (plain Reg, wires,
        // immediate FIFO views, RegFile/BRAM/unknown prims) kills on
        // any intervening action.  Output sizes the cross-rule sharing
        // an SSA edge lowering may legally perform — the emitter's
        // classification table.
        if std::env::var_os("TRS_EDGE_SSA_STATS").is_some() {
            let nodes: Vec<Vec<(bool, usize)>> = rcomps
                .iter()
                .map(|rc| {
                    let mut v = Vec::new();
                    for en in &rc.entries {
                        for &node in &en.nodes {
                            let (is_exec, r) = match node {
                                SchedNode::Sched(r) => (false, r),
                                SchedNode::Exec(r) => (true, r),
                            };
                            if rc.early.contains(&(en.inst, r)) {
                                continue;
                            }
                            if let Some(&o) = rule_ord.get(&(en.inst, r)) {
                                v.push((is_exec, o));
                            }
                        }
                    }
                    v
                })
                .collect();
            let _ = self.edge_ssa_plan(
                &inst_envs, &nodes, &specs, has_early, true,
                &Default::default(),
            );
        }
        // sharing census (TRS_JIT_SHARE_STATS=1): how many defs are
        // consumed by 2+ rules of the same module — the cross-rule
        // recompute mass the memo/#25 lever would save per edge
        if std::env::var_os("TRS_JIT_SHARE_STATS").is_some() {
            use trs_ir::Expr as E;
            fn refs(e: &E, out: &mut Vec<StrId>) {
                match e {
                    E::Def(n) => out.push(*n),
                    E::MethCall { args, .. } | E::Prim { args, .. }
                    | E::ForeignCall { args, .. } => {
                        for a in args {
                            refs(a, out);
                        }
                    }
                    E::If { cond, then_, else_, .. } => {
                        refs(cond, out);
                        refs(then_, out);
                        refs(else_, out);
                    }
                    E::Case { scrutinee, arms, default, .. } => {
                        refs(scrutinee, out);
                        for (_, a) in arms {
                            refs(a, out);
                        }
                        refs(default, out);
                    }
                    _ => {}
                }
            }
            let mut mirs: Vec<usize> = inst_envs.values().map(|e| e.mir).collect();
            mirs.sort_unstable();
            mirs.dedup();
            for mir in mirs {
                let m = &self.d.modules[mir];
                let by: HashMap<StrId, usize> =
                    m.defs.iter().enumerate().map(|(i, d)| (d.name, i)).collect();
                // transitive def set per rule (cf+wf+body)
                let mut counts: HashMap<StrId, u32> = HashMap::new();
                let mut own: HashMap<StrId, u32> = HashMap::new();
                for d in &m.defs {
                    let mut r = Vec::new();
                    refs(&d.expr, &mut r);
                    let mut n = 0u32;
                    fn sz(e: &E, n: &mut u32) {
                        *n += 1;
                        match e {
                            E::MethCall { args, .. } | E::Prim { args, .. }
                            | E::ForeignCall { args, .. } => {
                                for a in args {
                                    sz(a, n);
                                }
                            }
                            E::If { cond, then_, else_, .. } => {
                                sz(cond, n);
                                sz(then_, n);
                                sz(else_, n);
                            }
                            E::Case { scrutinee, arms, default, .. } => {
                                sz(scrutinee, n);
                                for (_, a) in arms {
                                    sz(a, n);
                                }
                                sz(default, n);
                            }
                            _ => {}
                        }
                    }
                    sz(&d.expr, &mut n);
                    own.insert(d.name, n);
                }
                for r in &m.rules {
                    let mut seen: std::collections::HashSet<StrId> = Default::default();
                    let mut work: Vec<StrId> = vec![r.can_fire, r.will_fire];
                    for st in r.body.iter() {
                        match st {
                            trs_ir::Stmt::Def { expr, .. } => refs(expr, &mut work),
                            trs_ir::Stmt::Action(a)
                            | trs_ir::Stmt::AvAction { action: a, .. } => {
                                use trs_ir::Action as A;
                                match a {
                                    A::MethCall { cond, args, .. } => {
                                        refs(cond, &mut work);
                                        for x in args {
                                            refs(x, &mut work);
                                        }
                                    }
                                    A::Foreign { cond, args, .. } => {
                                        refs(cond, &mut work);
                                        for x in args {
                                            refs(x, &mut work);
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            _ => {}
                        }
                    }
                    while let Some(n) = work.pop() {
                        if !seen.insert(n) {
                            continue;
                        }
                        if let Some(&di) = by.get(&n) {
                            refs(&m.defs[di].expr, &mut work);
                        }
                    }
                    for n in seen {
                        *counts.entry(n).or_insert(0) += 1;
                    }
                }
                let shared: Vec<_> =
                    counts.iter().filter(|(_, &c)| c >= 2).collect();
                let mass: u64 = shared
                    .iter()
                    .map(|(n, &c)| {
                        own.get(n).copied().unwrap_or(0) as u64 * (c as u64 - 1)
                    })
                    .sum();
                let total: u64 = own.values().map(|&v| v as u64).sum();
                eprintln!(
                    "trs share: mir={mir} rules={} defs={} shared(2+ rules)={} \
                     recompute-mass={mass} (module DAG mass {total})",
                    m.rules.len(),
                    m.defs.len(),
                    shared.len()
                );
            }
        }

        sl.lap("plan specs");
        // ---- exec dedup classes: one compiled body per class ----
        let mut classes: Vec<(usize, Vec<usize>)> = baked_classes.unwrap_or_default();
        if classes.is_empty() {
            let mut key_to_class: HashMap<(u64, usize, Vec<(bool, u32)>), usize> =
                HashMap::new();
            // per-INSTANCE memo: rebuilding the own-slot set per spec was
            // O(rules x cfwf-slots) — 26ms of FloatTest's startup
            let mut own_by_inst: HashMap<
                usize,
                (std::collections::HashSet<u32>, u32),
            > = HashMap::new();
            for (o, sp) in specs.iter().enumerate() {
                // the compiled body bakes always_fire and inhibitor slot
                // LOADS; own-region slots are region-relative in codegen
                // (twins share safely), foreign-instance slots are
                // absolute (twins must not share) — the key mirrors that
                let (own, r0) = own_by_inst.entry(sp.inst).or_insert_with(|| {
                    let ie = &inst_envs[&sp.inst];
                    (ie.cfwf_slot.values().copied().collect(), ie.region.0)
                });
                let (own, r0) = (&*own, *r0);
                let mut inh: Vec<(bool, u32)> = sp
                    .inhibit_slots
                    .iter()
                    .map(|&sl| {
                        if own.contains(&sl) {
                            (true, sl - r0)
                        } else {
                            (false, sl)
                        }
                    })
                    .collect();
                inh.sort_unstable();
                let key = (inst_sig[&sp.inst], sp.rule_idx, inh);
                let c = *key_to_class.entry(key).or_insert_with(|| {
                    classes.push((o, Vec::new()));
                    classes.len() - 1
                });
                classes[c].1.push(o);
            }
        }
        if trace {
            eprintln!(
                "trs jit: {} exec bodies in {} classes",
                specs.len(),
                classes.len()
            );
        }

        let comp_nodes: Vec<Option<Vec<JitNode>>> = rcomps
            .iter()
            .map(|rc| {
                let mut nodes = Vec::new();
                for en in &rc.entries {
                    for &node in &en.nodes {
                        let (r, is_sched) = match node {
                            SchedNode::Sched(r) => (r, true),
                            SchedNode::Exec(r) => (r, false),
                        };
                        // early rules run in THIS comp's PG_FINAL pass
                        // interpreted — emitting them here would double-
                        // run a rule that another comp scheduled (and
                        // gave an ordinal to) normally
                        if rc.early.contains(&(en.inst, r)) {
                            continue;
                        }
                        // interface-method nodes have no ordinal:
                        // they are no-ops in the edge walk (interp
                        // parity — nothing to latch or execute)
                        let Some(&ord) = rule_ord.get(&(en.inst, r)) else {
                            continue;
                        };
                        let ord = ord as u32;
                        nodes.push(if is_sched {
                            JitNode::Sched(ord)
                        } else {
                            JitNode::Exec(ord)
                        });
                    }
                }
                Some(nodes)
            })
            .collect();
        // sorted: HashMap order is process-seeded, and this order is baked
        // into the edge fns' EN-zeroing store sequence (deterministic IR)
        let mut en_slots: Vec<u32> =
            inst_envs.values().flat_map(|e| e.en_slot.values().copied()).collect();
        en_slots.sort_unstable();


        // ---- helper fns for outlined pieces (split opt-in) ----
        // v1: only module types whose instances all share one subtree
        // sig (helper symbols are sig-keyed); shared JIT/AOT lowering,
        // resolution differs (baked addresses vs .so symbols)
        let mut helper_specs: Vec<HelperSpec> = Vec::new();
        if !outlined_sel.is_empty() && !specs.is_empty() {
            let mut mir_sigs: HashMap<usize, std::collections::HashSet<u64>> =
                HashMap::new();
            let mut exemplar: HashMap<usize, usize> = HashMap::new();
            let mut iis: Vec<usize> = inst_envs.keys().copied().collect();
            iis.sort_unstable();
            for i in iis {
                let e = &inst_envs[&i];
                mir_sigs.entry(e.mir).or_default().insert(inst_sig[&i]);
                exemplar.entry(e.mir).or_insert(i);
            }
            let mut keys: Vec<(usize, StrId)> = outlined_sel.keys().copied().collect();
            keys.sort_unstable();
            for (mir, dn) in keys {
                if mir_sigs.get(&mir).map(|x| x.len()) != Some(1) {
                    continue;
                }
                let ex = exemplar[&mir];
                let (w, st, ref pnames) = outlined_sel[&(mir, dn)];
                let mut ports: Vec<(StrId, u32)> = Vec::new();
                let mut ok = true;
                for &pn in pnames {
                    let m = &self.d.modules[mir];
                    let w = m
                        .inputs
                        .iter()
                        .find(|q| q.name == pn)
                        .map(|q| q.width)
                        .or_else(|| {
                            m.methods.iter().find_map(|me| {
                                me.args.iter().find(|q| q.name == pn).map(|q| q.width)
                            })
                        });
                    match w {
                        Some(w) => ports.push((pn, w.max(1))),
                        None => ok = false,
                    }
                }
                if !ok {
                    continue;
                }
                helper_specs.push(HelperSpec {
                    mir,
                    def: dn,
                    width: w,
                    sym: format!("hlp_{:016x}_{}", inst_sig[&ex], dn),
                    inst: ex,
                    memo_slot: if st {
                        Some(inst_envs[&ex].memo_slot[&dn].0)
                    } else {
                        None
                    },
                    ports,
                });
            }
        }
        let refs_sym: HelperMap = helper_specs
            .iter()
            .map(|h| {
                ((h.mir, h.def), (HelperRef::Sym(h.sym.clone()), h.width, h.ports.clone()))
            })
            .collect();
        // Load attempt FIRST: an artifact carrying protos skips
        // trial_lower entirely (0.32s of sudoku startup); any failure
        // falls back to in-process compilation (which trials below)
        sl.lap("plan classes+nodes");
        let mut preloaded: Option<(Vec<CompiledSched>, Vec<CompiledExec>)> = None;
        let mut tick_level_flag: u64 = 0;
        let mut protos_opt: Option<Vec<FnProtos>> = None;
        let mut fused_opt: Option<Vec<usize>> = None;
        if let JitRequest::Load { src } = &request {
            match aot_load(
                src,
                // trace-salted: a traced plan (recording slots shift
                // the whole layout) must never accept an untraced
                // artifact, and vice versa
                self.bir_hash ^ (self.vcd_trace as u64 * 0x5452_4143_4544),
                &specs,
                &classes,
                split_thresh.unwrap_or(0),
                comp_nodes.len(),
                &self
                    .bdpi
                    .as_ref()
                    .map(|b| {
                        b.syms()
                            .iter()
                            .map(|(n, &a)| {
                                // syms key by BSV name; globals by c_name
                                let c = self
                                    .d
                                    .foreign_funcs
                                    .iter()
                                    .find(|ff| self.s(ff.name) == n)
                                    .map(|ff| self.s(ff.c_name).to_string())
                                    .unwrap_or_else(|| n.clone());
                                (format!("trs_bdpi_{c}"), a)
                            })
                            .collect::<Vec<_>>()
                    })
                    .unwrap_or_default(),
            ) {
                Ok((sch, exe, pr, fu, wt)) => {
                    preloaded = Some((sch, exe));
                    protos_opt = Some(pr);
                    fused_opt = Some(fu);
                    tick_level_flag = wt;
                }
                Err(e) => {
                    // mode-mismatch fallbacks are by design (an untraced
                    // artifact run under -V, or vice versa): keep the
                    // note out of captured test output
                    if e != TRACE_MODE_MISMATCH || trace {
                        eprintln!(
                            "trs: artifact {}: {e}; compiling in-process instead",
                            src.display()
                        );
                    }
                }
            }
        }
        sl.lap("aot load (dlopen+gates+dlsym)");
        // eligibility + call-site tables via trial lowering (link, run,
        // and artifact-fallback paths; skipped on successful loads)
        let artifact_loaded = protos_opt.is_some();
        let protos: Vec<FnProtos> = match protos_opt {
            Some(p) => p,
            None => match self.trial_protos(&inst_envs, &specs, now_slot, &request, trace) {
                Some(p) => p,
                None => return None,
            },
        };

        // trs link: emit the artifact .so and stop (nothing runs)
        #[cfg(not(feature = "jit"))]
        if let JitRequest::Emit { .. } = &request {
            self.jit_emit_result = Some(crate::AotEmit::Failed(
                "this build has no compile tier (feature `jit`)".into(),
            ));
            return None;
        }
        #[cfg(feature = "jit")]
        if let JitRequest::Emit { so, exe } = &request {
            // whole-edge SSA emission (task #24, opt-in): build the
            // legality tables the edge emitter consumes
            // DEFAULT ON for AOT links (the specialized fast compile);
            // TRS_EDGE_SSA=0 restores the classic emission
            let mut nodes_for_plan: Vec<Vec<(bool, usize)>> = comp_nodes
                .iter()
                .map(|ns| {
                    ns.as_ref()
                        .map(|ns| {
                            ns.iter()
                                .map(|n| match *n {
                                    JitNode::Sched(o) => (false, o as usize),
                                    JitNode::Exec(o) => (true, o as usize),
                                })
                                .collect()
                        })
                        .unwrap_or_default()
                })
                .collect();
            // compiled dynamic-scheduling variants (Emit only — a Load
            // runs the dispatch already baked into the artifact): one
            // appended plan row per (composition, alternative), plus
            // the order-derived sched overrides for that row.  Guard
            // evaluation order = declaration order (interp parity:
            // first match wins, none matching walks the base row).
            struct AltVar {
                comp: usize,
                guard_inst: usize,
                guard: trs_ir::Expr,
                nodes: Vec<(bool, usize)>,
                over: HashMap<usize, trs_codegen::abi::SchedOver>,
            }
            let mut alt_vars: Vec<AltVar> = Vec::new();
            if alts_compiled {
                for (k, rc) in rcomps.iter().enumerate() {
                    for alt in &rc.alts {
                        let mut vnodes: Vec<(bool, usize)> = Vec::new();
                        // owned-earlier eager defs per instance in
                        // THIS interleaving: base share claims only
                        // survive where the owner still runs earlier
                        let mut owned: HashMap<
                            usize,
                            std::collections::HashSet<StrId>,
                        > = HashMap::new();
                        let mut over: HashMap<
                            usize,
                            trs_codegen::abi::SchedOver,
                        > = HashMap::new();
                        for en in &alt.entries {
                            for &node in &en.nodes {
                                let (r, is_sched) = match node {
                                    SchedNode::Sched(r) => (r, true),
                                    SchedNode::Exec(r) => (r, false),
                                };
                                if rc.early.contains(&(en.inst, r)) {
                                    continue;
                                }
                                let Some(&o) = rule_ord.get(&(en.inst, r))
                                else {
                                    continue;
                                };
                                vnodes.push((!is_sched, o));
                                if !is_sched {
                                    continue;
                                }
                                // inhibitors: ME + the OTHER comps'
                                // base cross + this alternative's own
                                // cross (never the base order's)
                                let (me_len, ref cbc) = sched_parts[o];
                                let mut inh: Vec<u32> =
                                    specs[o].inhibit_slots[..me_len].to_vec();
                                for (j, cv) in cbc.iter().enumerate() {
                                    if j != k {
                                        inh.extend(cv.iter().copied());
                                    }
                                }
                                if let Some(cs) = alt.cross.get(&(en.inst, r)) {
                                    for (oi, ocf) in cs {
                                        match inst_envs
                                            .get(oi)
                                            .and_then(|e| e.cfwf_slot.get(ocf))
                                        {
                                            Some(&s) => inh.push(s),
                                            None => {
                                                if trace {
                                                    eprintln!(
                                                        "trs jit: off (unslotted \
                                                         alt cross inhibitor)"
                                                    );
                                                }
                                                return None;
                                            }
                                        }
                                    }
                                }
                                let shared: Vec<StrId> = specs[o]
                                    .shared
                                    .iter()
                                    .filter(|d| {
                                        owned
                                            .get(&en.inst)
                                            .is_some_and(|s| s.contains(d))
                                    })
                                    .copied()
                                    .collect();
                                over.insert(
                                    o,
                                    trs_codegen::abi::SchedOver {
                                        inhibit_slots: inh,
                                        shared,
                                    },
                                );
                                owned
                                    .entry(en.inst)
                                    .or_default()
                                    .extend(en.eager.iter().copied());
                            }
                        }
                        alt_vars.push(AltVar {
                            comp: k,
                            guard_inst: alt.guard_inst,
                            guard: alt.guard.clone(),
                            nodes: vnodes,
                            over,
                        });
                    }
                }
            }
            let base_rows = nodes_for_plan.len();
            let mut alt_row_refs: Vec<Vec<trs_codegen::abi::AltRow>> =
                vec![Vec::new(); base_rows];
            let mut sched_over_rows: Vec<
                HashMap<usize, trs_codegen::abi::SchedOver>,
            > = vec![HashMap::new(); base_rows];
            // per appended row: its composition (tick-table inheritance)
            let mut alt_row_comp: Vec<usize> = Vec::new();
            for av in alt_vars {
                let row = nodes_for_plan.len();
                nodes_for_plan.push(av.nodes);
                sched_over_rows.push(av.over);
                alt_row_comp.push(av.comp);
                alt_row_refs[av.comp].push(trs_codegen::abi::AltRow {
                    row,
                    guard_inst: av.guard_inst,
                    guard: av.guard,
                });
            }
            // exec class representatives, for the variant rows'
            // outlined-call nodes (FusedComp streams cover base rows)
            let mut rep_of: Vec<usize> = (0..specs.len()).collect();
            for (rep, members) in &classes {
                for &m in members {
                    rep_of[m] = *rep;
                }
            }
            let mk_edge_plan = |forced: &std::collections::HashSet<usize>| {
                (std::env::var("TRS_EDGE_SSA").as_deref() != Ok("0")).then(|| {
                    let mut plan = self.edge_ssa_plan(
                        &inst_envs,
                        &nodes_for_plan,
                        &specs,
                        has_early,
                        false,
                        forced,
                    );
                    // traced artifacts keep wire ticks boxed: the tick
                    // latches `written` for the VCD dump (and clears
                    // valid through the slot), which a compiled clear
                    // inside the edge fn would starve
                    if !self.vcd_trace {
                        let cov = self.prim_tick_coverage(&inst_envs, rcomps);
                        plan.wire_clears = cov.wire_clears;
                        plan.creg_copies = cov.creg_copies;
                        plan.bram_ticks = cov.bram_ticks;
                    }
                    if !alt_row_comp.is_empty() {
                        plan.alt_rows = alt_row_refs.clone();
                        plan.sched_over = sched_over_rows.clone();
                        // variant rows resolve outlined execs per
                        // ordinal (no FusedComp stream to index)
                        for (o, sp) in specs.iter().enumerate() {
                            plan.ord_fnodes.insert(
                                o,
                                trs_codegen::abi::FusedNode::Exec(
                                    trs_codegen::abi::HelperRef::Sym(
                                        format!(
                                            "exec_{}",
                                            specs[rep_of[o]].label
                                        ),
                                    ),
                                    inst_envs[&sp.inst].region.0 as u64,
                                    sp.token_base,
                                ),
                            );
                        }
                        // variant rows inherit their composition's
                        // tick tables (ticks are per comp, not per
                        // interleaving); rows were appended in
                        // alt_row_comp order.  Length guard: traced
                        // plans leave the tables empty (and decline
                        // alts anyway) — never mis-index rows.
                        if plan.wire_clears.len() == base_rows {
                            for &c in &alt_row_comp {
                                let wc = plan.wire_clears[c].clone();
                                let cc = plan.creg_copies[c].clone();
                                let bt = plan.bram_ticks[c].clone();
                                plan.wire_clears.push(wc);
                                plan.creg_copies.push(cc);
                                plan.bram_ticks.push(bt);
                            }
                        }
                    }
                    plan
                })
            };
            // bake what this emit derived: a Load of the artifact
            // decodes these instead of re-deriving (see PlanB)
            let plan_b_bytes = plan_b_encode(&specs, &classes);
            let bdpi_names: Vec<String> = {
                let mut v: Vec<String> = self
                    .d
                    .foreign_funcs
                    .iter()
                    .map(|f| self.s(f.c_name).to_string())
                    .filter(|n| !crate::is_lib_bdpi(n))
                    .collect();
                v.sort_unstable();
                v.dedup();
                v
            };
            // measured edge budget: an over-budget emit hands back the
            // measured oversized sections, the plan re-runs with them
            // forced OUTLINED (so sharing/hoist/elision tables stay
            // coherent), and the second emit is unbounded — measured
            // sizes make one replan enough.  Lowering is ms-scale; the
            // expensive pass pipeline never runs on a discarded module.
            let budget: u64 = std::env::var("TRS_EDGE_INSN_BUDGET")
                .ok()
                .and_then(|v| v.parse().ok())
                .unwrap_or(EDGE_INSN_BUDGET);
            let mut forced: std::collections::HashSet<usize> =
                Default::default();
            let mut budget_now = budget;
            // outlining a consumer collapses sharing, so survivors
            // re-expand cones and the next measurement can still be
            // over budget: iterate (the forced set only grows, so this
            // terminates), with a round cap as the safety valve
            let mut rounds = 0u32;
            let result = loop {
                let edge_plan = mk_edge_plan(&forced);
                match aot_emit(
                    &self.d,
                    &inst_envs,
                    &specs,
                    now_slot,
                    &classes,
                    &helper_specs,
                    &refs_sym,
                    split_thresh.unwrap_or(0),
                    &protos,
                    &comp_nodes,
                    &en_slots,
                    so,
                    exe.as_ref(),
                    // trace-salted: a traced plan (recording slots shift
                    // the whole layout) must never accept an untraced
                    // artifact, and vice versa
                    self.bir_hash ^ (self.vcd_trace as u64 * 0x5452_4143_4544),
                    self.bir_hash,
                    self.plan_a_bytes.as_deref().unwrap_or(&[]),
                    &plan_b_bytes,
                    edge_plan.as_ref(),
                    &bdpi_names,
                    budget_now,
                ) {
                    Err(EmitFail::EdgeOverBudget(victims)) => {
                        if std::env::var_os("TRS_JIT_TRACE").is_some() {
                            eprintln!(
                                "trs jit: edge over budget — replanning \
                                 with {} measured sections outlined",
                                victims.len()
                            );
                        }
                        rounds += 1;
                        // no exec sections left to outline (the
                        // remainder is sched code) or the round cap:
                        // accept — the O1 size tier is the backstop
                        if victims.is_empty() || rounds >= 4 {
                            budget_now = 0;
                        }
                        forced.extend(victims);
                    }
                    other => break other,
                }
            };
            // RunCore sidecar (validation form): build the arena the
            // link would hand a boot — the SAME four steps as the load
            // tail below (alloc, attach, reset levels, memo stamps;
            // drift between the two sites is exactly what the
            // TRS_RUNCORE_CHECK load comparison exists to catch) — and
            // stash its encoding for the linker CLI.  The box is
            // leaked: prims are now slot-aware and the link process
            // exits without touching them again.
            if result.is_ok() {
                let mut arena = vec![0u64; nslots as usize].into_boxed_slice();
                let arena_ptr = arena.as_mut_ptr();
                for &(ci, slot) in &attach {
                    if let InstKind::Prim(p) = &mut self.insts[ci].kind {
                        p.arena_attach(unsafe {
                            arena_ptr.add(slot as usize)
                        });
                    }
                }
                for (node, &slot) in reset_node_slot.iter().enumerate() {
                    unsafe {
                        *arena_ptr.add(slot as usize) =
                            (!self.rst_asserted[node]) as u64
                    };
                }
                for &slot in &memo_stamp_slots {
                    unsafe { *arena_ptr.add(slot as usize) = u64::MAX };
                }
                self.jit_arena_ptr = arena_ptr;
                self.jit_arena_len = nslots as usize;
                self.runcore_pending = self.runcore_image_encode();
                // Boot-descriptor stage A: capture what only the emit
                // can see — per-comp tick coverage (needs inst_envs)
                // and the BRAM warn-registry rows the attach loop just
                // wrote (keyed back to relative slots).  prime's
                // post-plan hook (runcore_desc_finish) assembles the
                // full v2 descriptor from this plus its clock/comp
                // state.
                if self.runcore_pending.is_some() {
                    let covered =
                        self.prim_tick_coverage(&inst_envs, rcomps).covered_all;
                    let reg = crate::prim::bram_warn_rows();
                    let mut warns: Vec<(u64, u32, String)> = attach
                        .iter()
                        .filter_map(|&(_, slot)| {
                            let key =
                                unsafe { arena_ptr.add(slot as usize) } as usize;
                            reg.get(&key).map(|(name, bits)| {
                                (slot as u64, *bits, name.clone())
                            })
                        })
                        .collect();
                    warns.sort_by_key(|w| w.0);
                    let nprims = self
                        .insts
                        .iter()
                        .filter(|i| matches!(i.kind, InstKind::Prim(_)))
                        .count();
                    // bounce-reachable prims: every inst a compiled
                    // prim call site can name.  BTreeSet: the sidecar
                    // rows must be deterministic (byte-stable sidecars
                    // are a witness invariant).
                    let slot_of: HashMap<usize, u64> = attach
                        .iter()
                        .map(|&(ci, slot)| (ci, slot as u64))
                        .collect();
                    let mut sites: std::collections::BTreeSet<usize> =
                        Default::default();
                    for p in &protos {
                        for pc in
                            p.sched_prims.iter().chain(p.exec_prims.iter())
                        {
                            sites.insert(pc.inst);
                        }
                    }
                    // mem-file prims join the PRIMS rows (the boot's
                    // overlay drives them through the same restored
                    // structs); their load requests become LOADS rows
                    // in construction order
                    let load_rows = self.runcore_live_loads();
                    sites.extend(load_rows.iter().map(|r| r.0 as usize));
                    let prim_rows = sites
                        .iter()
                        .map(|&ci| {
                            let Some(&slot) = slot_of.get(&ci) else {
                                return Err(
                                    "prim call site on an unattached prim"
                                        .to_string(),
                                );
                            };
                            let InstKind::Prim(p) = &self.insts[ci].kind
                            else {
                                return Err(
                                    "prim call site on a non-prim inst"
                                        .to_string(),
                                );
                            };
                            match p.runcore_seed() {
                                Some((tag, words, strs)) => {
                                    Ok((ci as u64, slot, tag, words, strs))
                                }
                                None => Err(format!(
                                    "prim call site without a native \
                                     servicer ({})",
                                    self.insts[ci].path
                                )),
                            }
                        })
                        .collect::<Result<Vec<_>, String>>();
                    self.runcore_stage_a = Some(RunCoreStageA {
                        covered,
                        edge_ssa: std::env::var("TRS_EDGE_SSA").as_deref()
                            != Ok("0"),
                        // attach lists exactly the slot-allocated prims
                        boxed: nprims != attach.len(),
                        prim_rows,
                        warns,
                        load_rows,
                    });
                }
                std::mem::forget(arena);
            }
            self.jit_emit_result = Some(match result {
                Ok(()) => crate::AotEmit::Compiled,
                Err(EmitFail::Ineligible(e)) => crate::AotEmit::Ineligible(e),
                Err(EmitFail::Infra(e)) => crate::AotEmit::Failed(e),
                // the unbounded second pass cannot report over-budget
                Err(EmitFail::EdgeOverBudget(_)) => unreachable!(),
            });
            return None;
        }



        let n = specs.len();
        let nworkers = jit_workers(n);

        // SCHED functions compile eagerly (blocking, parallel): they
        // run on every edge and the cone-sharing keeps them small
        let chunk = n.div_ceil(nworkers).max(1);
        // deferred: Load requests only need addresses if the artifact
        // fails to load (in-process fallback) — never compile helpers
        // just to throw them away at every artifact startup
        #[cfg(feature = "jit")]
        let compile_helpers_now = |inst_envs: &HashMap<usize, InstEnv>| -> HelperMap {
            if helper_specs.is_empty() {
                return HelperMap::new();
            }
            trs_codegen::lower::llvm_init_once();
            let env = PlanEnv { d: &self.d, insts: inst_envs, now_slot };
            let pseudo = specs[0].clone();
            let t0 = std::time::Instant::now();
            match compile_helpers(&env, &helper_specs, &refs_sym, &pseudo) {
                Ok(addrs) => {
                    if std::env::var_os("TRS_JIT_TIME").is_some() {
                        eprintln!(
                            "trs jit: {} helpers compiled {:?}",
                            helper_specs.len(),
                            t0.elapsed()
                        );
                    }
                    let am: HashMap<String, usize> = addrs.into_iter().collect();
                    helper_specs
                        .iter()
                        .map(|h| {
                            (
                                (h.mir, h.def),
                                (HelperRef::Addr(am[&h.sym]), h.width, h.ports.clone()),
                            )
                        })
                        .collect()
                }
                Err(e) => {
                    if trace {
                        eprintln!("trs jit: helpers off ({e})");
                    }
                    HelperMap::new()
                }
            }
        };

        #[cfg(feature = "jit")]
        let helpers_addr: HelperMap = if preloaded.is_some() {
            HelperMap::new()
        } else {
            compile_helpers_now(&inst_envs)
        };
        // no compile tier: helper addresses only matter to in-process
        // sched compilation, which the stub below refuses anyway
        #[cfg(not(feature = "jit"))]
        let helpers_addr = HelperMap::new();
        let jit_helpers: Option<&HelperMap> =
            (!helpers_addr.is_empty()).then_some(&helpers_addr);
        let (scheds, preexecs) = if let Some((s, e)) = preloaded {
            (s, Some(e))
        } else {
            (
                aot_or_jit_scheds(
                    self, &inst_envs, &specs, now_slot, jit_helpers, nworkers, trace,
                )?,
                None,
            )
        };

        let exec_args: Vec<(u64, u64)> = specs
            .iter()
            .map(|sp| {
                let r0 = inst_envs[&sp.inst].region.0 as u64;
                (r0, sp.token_base)
            })
            .collect();
        let nclasses = classes.len();
        // batches are the stop-flag granularity: one whole share per
        // worker made JitPlans::drop join wait for the FULL body
        // compile (the fleet) — cap so teardown latency is bounded
        // by a few class compiles, not the design size
        let cchunk = nclasses.div_ceil(nworkers).clamp(1, 8);
        sl.lap("plan tail (protos/scheds)");
        let lazy = Arc::new(LazyJit {
            design: if preexecs.is_some() {
                None
            } else {
                Some(self.d.clone())
            },
            insts: inst_envs,
            specs,
            now_slot,
            exec_args,
            protos,
            classes,
            helpers: Arc::new(helpers_addr),
            scheds,
            next_batch: std::sync::atomic::AtomicUsize::new(0),
            batch_size: cchunk,
            cold: std::sync::atomic::AtomicUsize::new(if preexecs.is_some() {
                0
            } else {
                nclasses
            }),
            stop: std::sync::atomic::AtomicBool::new(false),
            cells: (0..n).map(|_| OnceLock::new()).collect(),
        });
        self.jit_shared = Some(lazy.clone());
        sl.lap("lazyjit build");

        let mut workers = Vec::new();
        match preexecs {
            Some(execs) => {
                // artifact bodies: every cell warm from the start
                for (i, ce) in execs.into_iter().enumerate() {
                    let _ = lazy.cells[i].set(ce);
                }
            }
            None => {
                // bodies compile in the background; cold bodies interpret
                for _ in 0..nworkers {
                    let lz = lazy.clone();
                    workers.push(std::thread::spawn(move || lz.work()));
                }
                if std::env::var_os("TRS_JIT_SYNC").is_some() {
                    let t0 = std::time::Instant::now();
                    while (0..n).any(|i| lazy.cells[i].get().is_none()) {
                        std::thread::yield_now();
                    }
                    if std::env::var_os("TRS_JIT_TIME").is_some() {
                        eprintln!("trs jit: sync body compile {:?}", t0.elapsed());
                    }
                }
            }
        }

        // allocate + wire the arena
        let mut arena = vec![0u64; nslots as usize].into_boxed_slice();
        let arena_ptr = arena.as_mut_ptr();
        for (ci, slot) in attach {
            if let InstKind::Prim(p) = &mut self.insts[ci].kind {
                p.arena_attach(unsafe { arena_ptr.add(slot as usize) });
            }
        }
        for (node, &slot) in reset_node_slot.iter().enumerate() {
            unsafe { *arena_ptr.add(slot as usize) = (!self.rst_asserted[node]) as u64 };
        }
        for &slot in &memo_stamp_slots {
            unsafe { *arena_ptr.add(slot as usize) = u64::MAX };
        }
        self.jit_arena_ptr = arena_ptr;
        self.jit_arena_len = nslots as usize;
        self.jit_reset_slots = reset_node_slot;
        // two-fill bake gate: perturb every mem-file data region
        // (post-attach, pre-advance) so the window capture can prove
        // the rest of the state independent of file content
        if let Some(pat) = self.runcore_bake_fill {
            for inst in &mut self.insts {
                if let InstKind::Prim(p) = &mut inst.kind {
                    if p.runcore_load().is_some() && p.runcore_slot().is_some()
                    {
                        p.runcore_fill_region(pat);
                    }
                }
            }
        }
        if trace {
            eprintln!(
                "trs jit: on ({} rules, {} slots, {} compositions)",
                rules.len(),
                nslots,
                comp_nodes.len()
            );
        }
        let exec_fallback: Vec<(usize, StrId, u32)> = {
            let mut v = Vec::with_capacity(rules.len());
            for ri in &rules {
                let mir = lazy.insts[&ri.inst].mir;
                v.push((ri.inst, self.d.modules[mir].rules[ri.rule_idx].name, ri.wf_slot));
            }
            v
        };
        // interpreted bodies resolve fire signals and schedule-position
        // defs straight from the arena (same values the native scheds
        // stored; matches the proven full-interpreter eager semantics)
        self.jit_eager_slots = lazy
            .insts
            .iter()
            .flat_map(|(&i, e)| {
                e.cfwf_slot
                    .iter()
                    .map(move |(&d, &s)| ((i, d), (s, 1u32)))
                    .chain(e.eager_slot.iter().map(move |(&d, &(b, w))| ((i, d), (b, w))))
            })
            .collect();
        // interp method calls during body fallback must write EN slots
        // through so native scheds see them
        self.jit_en_slots = lazy
            .insts
            .iter()
            .flat_map(|(&i, e)| e.en_slot.iter().map(move |(&p, &s)| ((i, p), s)))
            .collect();
        // traced artifacts: initialize the recording slots, flatten the
        // per-instance tables for the runtime recorder/writer, and seed
        // slots from any pre-plan recordings (hybrid warm-up slices ran
        // interpreted into the maps) — the slot is the single authority
        // from here on, so seeded map entries are dropped
        for &(slot, v) in &rec_inits {
            unsafe { *arena_ptr.add(slot as usize) = v };
        }
        self.jit_rec_defs = lazy
            .insts
            .iter()
            .flat_map(|(&i, e)| {
                e.rec_defs.iter().map(move |(&n, &sl)| ((i, n), sl))
            })
            .collect();
        self.jit_rec_meths = lazy
            .insts
            .iter()
            .flat_map(|(&i, e)| {
                e.rec_meths.iter().map(move |(&n, rm)| {
                    ((i, n), crate::RecSlots { t: rm.t, args: rm.args.clone(), res: rm.res })
                })
            })
            .collect();
        let keys: Vec<_> = self.jit_rec_defs.keys().cloned().collect();
        for (i, n) in keys {
            if let Some(v) = self.vcd_def_vals.remove(&(i, n)) {
                let (base, w) = self.jit_rec_defs[&(i, n)];
                let vv = v.zext(w.max(1));
                unsafe {
                    for (k, l) in vv.limbs64().iter().enumerate().take(
                        (w.max(1) as usize).div_ceil(64),
                    ) {
                        *arena_ptr.add(base as usize + k) = *l;
                    }
                }
            }
        }
        let keys: Vec<_> = self.jit_rec_meths.keys().cloned().collect();
        for (i, n) in keys {
            let rs = self.jit_rec_meths[&(i, n)].clone();
            if let Some((t, argv)) = self.vcd_meth_calls.remove(&(i, n)) {
                unsafe { *arena_ptr.add(rs.t as usize) = t };
                for (a, &(base, w)) in argv.iter().zip(&rs.args) {
                    let vv = a.clone().zext(w.max(1));
                    unsafe {
                        for (k, l) in vv.limbs64().iter().enumerate().take(
                            (w.max(1) as usize).div_ceil(64),
                        ) {
                            *arena_ptr.add(base as usize + k) = *l;
                        }
                    }
                }
            }
            if let Some(v) = self.vcd_meth_results.remove(&(i, n)) {
                if let Some((base, w)) = rs.res {
                    let vv = v.zext(w.max(1));
                    unsafe {
                        for (k, l) in vv.limbs64().iter().enumerate().take(
                            (w.max(1) as usize).div_ceil(64),
                        ) {
                            *arena_ptr.add(base as usize + k) = *l;
                        }
                    }
                }
            }
        }
        // RunCore sidecar, validation form: the post-attach image is
        // deterministic (the slot walk is process-stable — AOT dedup
        // depends on it — and prim constructors write fixed state).
        // The Emit branch above stashed its own construction's
        // encoding; a checked load rebuilds classically HERE and
        // compares bit-for-bit.  The boot path that TRUSTS the image
        // lands on top of this witness.
        let _ = runcore_emit;
        let mut runcore_desc = None;
        // gate on a SUCCESSFUL artifact load: fallback runs (trace-
        // mode mismatch, layout drift) execute freshly compiled code,
        // so the sidecar's claims are about a run that isn't happening
        // — checking them there is witness noise (panel finding)
        if artifact_loaded && std::env::var_os("TRS_RUNCORE_CHECK").is_some() {
            if let Some(p) = &runcore_sidecar {
                runcore_desc = self.runcore_image_check(p);
            }
        }
        sl.lap("arena+flatmaps+workers");
        let covered_ticks = if tick_level_flag >= 2 {
            self.prim_tick_coverage(&lazy.insts, rcomps).covered_all
        } else if tick_level_flag == 1 {
            self.prim_tick_coverage(&lazy.insts, rcomps).covered_wire
        } else {
            vec![Default::default(); rcomps.len()]
        };
        Some(JitPlans {
            _arena: arena,
            arena_ptr,
            comp_nodes,
            en_slots,
            now_slot,
            lazy,
            workers,
            exec_fallback,
            covered_ticks,
            runcore_desc,
            fused: {
                let cell = std::sync::OnceLock::new();
                if let Some(fu) = fused_opt {
                    let _ = cell.set(fu);
                }
                cell
            },
        })
    }
}
