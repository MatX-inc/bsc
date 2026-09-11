//! BVI import: the verilate-or-cache pipeline (design of record: the KB
//! draft "KB: BVI-via-Verilator design (trs)", v4 sec 5.2; validated
//! end-to-end by the M0 spike at src/trs/spike/bvi-m0/).
//!
//! VERILATION IS A BUILD STEP (v1.5, ratified 2026-08-23): only the
//! build entry points -- `trs link` and `trs vlt build` -- verilate,
//! via [`build_all`] plus the elaboration pass that resolves
//! forwarded-parameter classes.  The run side (`trs run`, artifact
//! wrappers) is LOAD-ONLY through [`find_model_resolved`], which walks
//! the vlt/byid run-key index the build wrote -- no verilator binary,
//! no source tree, no staleness re-check at run; a cold cache is a
//! rebuild instruction.  The cache is PER-PROJECT (Q3 ratified):
//! TRS_VLT_CACHE, defaulting to `trs-vlt` beside the design.
//!
//! Per contract class -- (verilator version, shim-generator revision,
//! contract shape, typed parameters, defines, resolved top file) --
//! the pipeline resolves sources, verilates ONCE with `--timing`,
//! reads the model metadata from that build's stable products
//! (V<top>.h port macros + VM_TIMING in classes.mk; v1.4, see
//! meta.rs), applies the refusals (DPI via the V<top>__Dpi.h
//! backstop), generates the engine-neutral shim, builds a shared
//! object, and caches it content-addressed over the depfile closure
//! with a per-class lock.
//!
//! `--timing` is always passed; Verilator itself reports through
//! VM_TIMING whether the model USES timing, which selects
//! verilated_timing.o and the platform coroutine flags at link (the
//! shim's vlt_advance drain loop is shape-independent).  The floor is
//! any --timing-capable Verilator (5.x); a binary that rejects the
//! option gets a clear error naming TRS_VERILATOR.

use std::fmt;
use std::os::unix::io::AsRawFd;
use std::path::{Path, PathBuf};
use std::process::Command;

use trs_ir::bvi::{BviContract, BviParamValue};
use trs_ir::Design;

pub mod meta;
pub mod resolve;
// the digest lives in trs-ir: the link needs it to key a module by
// its content, and trs-ir is below every crate that does
pub use trs_ir::sha256;
pub mod shim;

// ---------------------------------------------------------------
// Error taxonomy

#[derive(Debug)]
pub enum VltError {
    /// A design shape or model property v1 refuses, with a stable tag
    /// (delay, dpi, contract-mismatch, param-type, ...).
    Refuse { tag: String, detail: String },
    /// A toolchain step failed (verilator, make, g++, dlopen).
    Tool { stage: String, detail: String },
    /// Source resolution failed.
    Resolve { detail: String },
}

impl VltError {
    pub fn refuse(tag: &str, detail: String) -> Self {
        VltError::Refuse {
            tag: tag.into(),
            detail,
        }
    }
    pub fn tool(stage: &str, detail: String) -> Self {
        VltError::Tool {
            stage: stage.into(),
            detail,
        }
    }
    pub fn resolve(detail: String) -> Self {
        VltError::Resolve { detail }
    }
}

impl fmt::Display for VltError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VltError::Refuse { tag, detail } => {
                write!(f, "REFUSE({tag}): {detail}")
            }
            VltError::Tool { stage, detail } => {
                write!(f, "{stage} failed: {detail}")
            }
            VltError::Resolve { detail } => write!(f, "source resolution: {detail}"),
        }
    }
}

impl std::error::Error for VltError {}

// ---------------------------------------------------------------
// Options

#[derive(Debug, Clone)]
pub struct BuildOptions {
    pub verilator: PathBuf,
    pub cache_dir: PathBuf,
    /// Extra search dirs, tried BEFORE the contract's own vpath.
    pub extra_vpath: Vec<PathBuf>,
    /// Explicitly named source files; override all path search.
    pub extra_vfiles: Vec<PathBuf>,
    pub verbose: bool,
}

impl BuildOptions {
    /// Defaults per the RATIFIED Q3 answer (2026-08-23): the cache is
    /// PER-PROJECT.  TRS_VLT_CACHE when set; otherwise `trs-vlt` --
    /// the trs entry points resolve that next to the design's .bir and
    /// export it, so a bare library call (no env, no entry point)
    /// lands on ./trs-vlt under the working directory.  TRS_VERILATOR
    /// picks the binary (default: `verilator` on PATH).  TRS_VLT_VPATH
    /// / TRS_VLT_VFILES (colon-separated) carry `trs vlt build`'s
    /// --vpath/--vfile extras into the elaboration pass that builds
    /// forwarded-parameter classes.
    pub fn from_env() -> Self {
        let verilator = std::env::var_os("TRS_VERILATOR")
            .map(PathBuf::from)
            .unwrap_or_else(|| PathBuf::from("verilator"));
        let cache_dir = std::env::var_os("TRS_VLT_CACHE")
            .map(PathBuf::from)
            .unwrap_or_else(|| PathBuf::from("trs-vlt"));
        let split_paths = |var: &str| -> Vec<PathBuf> {
            std::env::var(var)
                .ok()
                .map(|v| {
                    v.split(':')
                        .filter(|s| !s.is_empty())
                        .map(PathBuf::from)
                        .collect()
                })
                .unwrap_or_default()
        };
        BuildOptions {
            verilator,
            cache_dir,
            extra_vpath: split_paths("TRS_VLT_VPATH"),
            extra_vfiles: split_paths("TRS_VLT_VFILES"),
            verbose: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BuiltModel {
    pub so_path: PathBuf,
    /// sha256[:16] of the contract JSON; must equal the hash the loaded
    /// model reports through vlt_contract.
    pub contract_hash: String,
    pub cached: bool,
    pub class: String,
    pub verilog_name: String,
}

// ---------------------------------------------------------------
// Typed -G parameter serialization (v4 sec 4.5: semantics, not text)

/// A forwarded (FromArg) parameter's value after the interpreter
/// evaluated the instantiation argument in the parent context.  Aligns
/// 1:1 with the contract's params list where used.
#[derive(Debug, Clone)]
pub enum ResolvedParam {
    /// Take the contract's own literal value (non-forwarded slots).
    Contract,
    Bits {
        width: u32,
        hex: String,
    },
    Str(String),
    Real(f64),
}

fn serialize_param(name: &str, v: &BviParamValue, strings: &[String]) -> Result<String, VltError> {
    let s = |id: u32| strings.get(id as usize).map(String::as_str).unwrap_or("");
    Ok(match v {
        BviParamValue::IntSigned { value, .. } => format!("-G{name}={value}"),
        BviParamValue::Bits { width, hex } => {
            format!("-G{name}={width}'h{}", s(*hex))
        }
        BviParamValue::Str(sid) => {
            let esc = s(*sid).replace('\\', "\\\\").replace('"', "\\\"");
            format!("-G{name}=\"{esc}\"")
        }
        BviParamValue::Real(d) => format!("-G{name}={d:?}"),
        BviParamValue::FromArg { .. } => {
            return Err(VltError::refuse(
                "param-unresolved",
                format!(
                    "parameter '{name}' is forwarded from the enclosing \
                     module and has no value yet (it resolves at \
                     instantiation; `trs vlt build` cannot verilate this \
                     class standalone)"
                ),
            ))
        }
    })
}

/// The full -G serialization for a contract's parameter list, with
/// instantiation-resolved values overriding the forwarded slots.
fn serialize_params(
    c: &BviContract,
    strings: &[String],
    resolved: Option<&[ResolvedParam]>,
) -> Result<Vec<String>, VltError> {
    let s = |id: u32| strings.get(id as usize).map(String::as_str).unwrap_or("");
    if let Some(r) = resolved {
        if r.len() != c.params.len() {
            return Err(VltError::tool(
                "param resolve",
                format!(
                    "{}: {} resolved values for {} contract parameters",
                    s(c.verilog_name),
                    r.len(),
                    c.params.len()
                ),
            ));
        }
    }
    let mut gparams = Vec::new();
    for (i, prm) in c.params.iter().enumerate() {
        let over = resolved.and_then(|r| r.get(i));
        match over {
            Some(ResolvedParam::Contract) | None => {
                gparams.push(serialize_param(s(prm.name), &prm.value, strings)?)
            }
            Some(r) => gparams.push(serialize_resolved(s(prm.name), r)),
        }
    }
    Ok(gparams)
}

/// The RUN-KNOWABLE identity of a model class: everything a load-only
/// consumer can compute from the contract and the resolved parameter
/// values alone -- no verilator binary (whose version is part of the
/// class key) and no source tree (whose resolved top file is too).
/// Builds record class-key-by-run-key under vlt/byid/ so `trs run`
/// finds the artifact the build step produced; when several build
/// environments share one identity (a pin bump re-verilates), the
/// LAST BUILD WINS -- the build step decides what the run consumes.
fn run_key(c: &BviContract, strings: &[String], gparams: &[String]) -> String {
    let s = |id: u32| strings.get(id as usize).map(String::as_str).unwrap_or("");
    let mut src = String::new();
    src.push_str(&format!("shimgen={}\n", shim::SHIMGEN_REV));
    src.push_str(&format!("contract={}\n", shim::contract_json(c, strings)));
    for g in gparams {
        src.push_str(&format!("gparam={g}\n"));
    }
    // defines and params are NOT part of the contract JSON (that hash
    // is the interface-shape identity the shim bakes in), so they must
    // appear here explicitly or two same-interface valuations would
    // collide in byid and a run could silently load the wrong model
    for (k, v) in &c.defines {
        src.push_str(&format!(
            "define={}={}\n",
            s(*k),
            v.map(|v| s(v)).unwrap_or("")
        ));
    }
    for &d in &c.vpath {
        src.push_str(&format!("vpath={}\n", s(d)));
    }
    sha256::digest_hex(src.as_bytes())[..32].to_string()
}

/// The public form of the run key: the identity a LOAD-ONLY consumer
/// can compute for this contract + valuation.  `trs run`'s precheck
/// dedups on this (NOT on the Verilog top name -- two imports of one
/// module with different literal parameters are distinct classes).
pub fn run_identity(
    c: &BviContract,
    strings: &[String],
    resolved: Option<&[ResolvedParam]>,
) -> Result<String, VltError> {
    let gparams = serialize_params(c, strings, resolved)?;
    Ok(run_key(c, strings, &gparams))
}

/// Record run-key -> class-key (atomic: tmp + rename, so a concurrent
/// run never reads a torn pointer); best-effort -- a failed pointer
/// write degrades to a run-side cold-cache refusal, never a wrong
/// artifact -- but LOUD, because the symptom (build exits 0, run says
/// rebuild, rebuilding doesn't help) is otherwise undiagnosable.
fn write_byid(cache_dir: &Path, runkey: &str, class: &str) {
    let dir = cache_dir.join("vlt").join("byid");
    let warn = |e: &dyn fmt::Display| {
        eprintln!(
            "trs-vlt: warning: could not record the run index entry \
             {}/{runkey} ({e}); runs will report a cold cache for this \
             model even though it is built",
            dir.display()
        );
    };
    if let Err(e) = std::fs::create_dir_all(&dir) {
        return warn(&e);
    }
    let tmp = dir.join(format!("{runkey}.tmp{}", std::process::id()));
    match std::fs::write(&tmp, class) {
        Err(e) => warn(&e),
        Ok(()) => {
            if let Err(e) = std::fs::rename(&tmp, dir.join(runkey)) {
                warn(&e);
            }
        }
    }
}

fn serialize_resolved(name: &str, r: &ResolvedParam) -> String {
    match r {
        ResolvedParam::Contract => unreachable!("caller resolves Contract slots"),
        ResolvedParam::Bits { width, hex } => format!("-G{name}={width}'h{hex}"),
        ResolvedParam::Str(s) => {
            let esc = s.replace('\\', "\\\\").replace('"', "\\\"");
            format!("-G{name}=\"{esc}\"")
        }
        ResolvedParam::Real(d) => format!("-G{name}={d:?}"),
    }
}

// ---------------------------------------------------------------
// Per-class file lock (advisory flock; released on drop)

struct ClassLock {
    _file: std::fs::File,
}

impl ClassLock {
    fn acquire(dir: &Path) -> Result<ClassLock, VltError> {
        let path = dir.join(".lock");
        let file = std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .open(&path)
            .map_err(|e| VltError::tool("cache lock", format!("{}: {e}", path.display())))?;
        let rc = unsafe { libc::flock(file.as_raw_fd(), libc::LOCK_EX) };
        if rc != 0 {
            return Err(VltError::tool(
                "cache lock",
                format!(
                    "flock {}: {}",
                    path.display(),
                    std::io::Error::last_os_error()
                ),
            ));
        }
        Ok(ClassLock { _file: file })
    }
}

// ---------------------------------------------------------------
// Depfile manifest: content-address the transitive source closure

fn parse_depfiles(obj: &Path) -> Vec<PathBuf> {
    let mut deps: Vec<PathBuf> = Vec::new();
    if let Ok(rd) = std::fs::read_dir(obj) {
        for ent in rd.flatten() {
            let p = ent.path();
            if p.extension().and_then(|e| e.to_str()) != Some("d") {
                continue;
            }
            let Ok(text) = std::fs::read_to_string(&p) else {
                continue;
            };
            // "target: dep dep \\\n dep ..." -- possibly several rules
            for rule in text.split(':').skip(1) {
                for tok in rule.split_whitespace() {
                    if tok == "\\" {
                        continue;
                    }
                    let dep = PathBuf::from(tok);
                    // only source-tree files matter; generated files in
                    // the obj dir would self-invalidate the manifest
                    if dep.is_file() && !dep.starts_with(obj) && !deps.contains(&dep) {
                        deps.push(dep);
                    }
                }
            }
        }
    }
    deps.sort();
    deps
}

fn hash_file(p: &Path) -> Option<String> {
    std::fs::read(p).ok().map(|b| sha256::digest_hex(&b))
}

fn write_manifest(path: &Path, deps: &[PathBuf]) -> Result<(), VltError> {
    let mut out = String::new();
    for d in deps {
        let h = hash_file(d).unwrap_or_default();
        out.push_str(&format!("{h}  {}\n", d.display()));
    }
    std::fs::write(path, out)
        .map_err(|e| VltError::tool("manifest write", format!("{}: {e}", path.display())))
}

fn manifest_valid(path: &Path) -> bool {
    let Ok(text) = std::fs::read_to_string(path) else {
        return false;
    };
    for line in text.lines() {
        let Some((h, p)) = line.split_once("  ") else {
            return false;
        };
        match hash_file(Path::new(p)) {
            Some(now) if now == h => {}
            _ => return false,
        }
    }
    true
}

// ---------------------------------------------------------------
// License packaging: the artifact embeds the Verilator runtime

const NOTICE: &str = "\
This directory contains a shared object built by trs from a Verilog
design using Verilator.  The object embeds the Verilator runtime
(verilated.cpp and headers), which is licensed under
LGPL-3.0-only OR Artistic-2.0 (see the COPYING and COPYING.LESSER
files in the Verilator installation).  The generated model code and
the trs shim are derived from the user's Verilog sources and the trs
contract respectively.  Redistribution of this object must comply
with the Verilator runtime license.
";

// ---------------------------------------------------------------
// The pipeline

fn run_logged(mut cmd: Command, stage: &str) -> Result<std::process::Output, VltError> {
    let out = cmd
        .output()
        .map_err(|e| VltError::tool(stage, e.to_string()))?;
    if !out.status.success() {
        let mut detail = String::from_utf8_lossy(&out.stderr).to_string();
        if detail.trim().is_empty() {
            detail = String::from_utf8_lossy(&out.stdout).to_string();
        }
        return Err(VltError::tool(stage, detail));
    }
    Ok(out)
}

/// Build (or reuse from cache) the verilated model for one contract.
/// Every parameter must be a literal in the contract; forwarded
/// (FromArg) parameters need [`build_model_resolved`].
pub fn build_model(
    c: &BviContract,
    strings: &[String],
    opts: &BuildOptions,
) -> Result<BuiltModel, VltError> {
    build_model_resolved(c, strings, opts, None)
}

/// Build with instantiation-resolved parameter values.  `resolved`
/// aligns 1:1 with the contract's params; `ResolvedParam::Contract`
/// slots take the contract's own literal.  Each distinct valuation
/// serializes to different -G arguments and therefore its own cache
/// class.
pub fn build_model_resolved(
    c: &BviContract,
    strings: &[String],
    opts: &BuildOptions,
    resolved: Option<&[ResolvedParam]>,
) -> Result<BuiltModel, VltError> {
    let s = |id: u32| strings.get(id as usize).map(String::as_str).unwrap_or("");
    let top = s(c.verilog_name).to_string();

    // ---- resolve sources
    let mut vpath: Vec<PathBuf> = opts.extra_vpath.clone();
    vpath.extend(c.vpath.iter().map(|&id| PathBuf::from(s(id))));
    let vpath = resolve::clean_dirs(&vpath);
    let mut vfiles: Vec<PathBuf> = opts.extra_vfiles.clone();
    vfiles.extend(c.vfiles.iter().map(|&id| PathBuf::from(s(id))));
    let top_file = resolve::resolve_top(&top, &vpath, &vfiles)?;
    let top_file = top_file
        .canonicalize()
        .map_err(|e| VltError::resolve(format!("{}: {e}", top_file.display())))?;

    // ---- typed params and defines
    let gparams = serialize_params(c, strings, resolved)?;
    let runkey = run_key(c, strings, &gparams);
    let defines: Vec<(String, Option<String>)> = c
        .defines
        .iter()
        .map(|(k, v)| (s(*k).to_string(), v.map(|v| s(v).to_string())))
        .collect();

    // ---- class key
    let (vmaj, vmin, vfull) = meta::verilator_version(&opts.verilator)?;
    let cjson = shim::contract_json(c, strings);
    let mut keysrc = String::new();
    keysrc.push_str(&format!("shimgen={}\n", shim::SHIMGEN_REV));
    keysrc.push_str(&format!("verilator={vfull}\n"));
    keysrc.push_str(&format!("top={top}\n"));
    keysrc.push_str(&format!("topfile={}\n", top_file.display()));
    keysrc.push_str(&format!("contract={cjson}\n"));
    for g in &gparams {
        keysrc.push_str(&format!("gparam={g}\n"));
    }
    for (k, v) in &defines {
        keysrc.push_str(&format!("define={k}={}\n", v.as_deref().unwrap_or("")));
    }
    for d in &vpath {
        keysrc.push_str(&format!("vpath={}\n", d.display()));
    }
    let class = sha256::digest_hex(keysrc.as_bytes())[..32].to_string();

    let class_dir = opts.cache_dir.join("vlt").join(&class);
    std::fs::create_dir_all(&class_dir)
        .map_err(|e| VltError::tool("cache dir", format!("{}: {e}", class_dir.display())))?;
    let _lock = ClassLock::acquire(&class_dir)?;

    let so_path = class_dir.join(format!("lib{top}_shim.vlt.so"));
    let manifest = class_dir.join("manifest");

    // expected contract hash (recomputed cheaply; also embedded in the
    // shim at build time)
    let expect_hash = sha256::digest_hex(cjson.as_bytes())[..16].to_string();

    // ---- cache hit: manifest present AND every dep content unchanged
    if so_path.is_file() && manifest.is_file() && manifest_valid(&manifest) {
        dlopen_check(&so_path, &expect_hash)?;
        write_byid(&opts.cache_dir, &runkey, &class);
        return Ok(BuiltModel {
            so_path,
            contract_hash: expect_hash,
            cached: true,
            class,
            verilog_name: top,
        });
    }

    if opts.verbose {
        eprintln!("trs-vlt: verilating {top} (class {class}, verilator {vmaj}.{vmin:03})");
    }

    let decl_path = class_dir.join("trs_printf_decl.h");
    std::fs::write(&decl_path, shim::PRINTF_DECL_H)
        .map_err(|e| VltError::tool("shim write", format!("{}: {e}", decl_path.display())))?;

    // ---- verilate (always --timing; VM_TIMING in the build products
    // then says whether the model USES timing) with depfiles.  This is
    // also the metadata source (v1.4): ports come from V<top>.h and
    // the timing bit from classes.mk, so there is no separate
    // inspection run and no version-sensitive dump format.
    let obj = class_dir.join("obj");
    let cflags = format!(
        "-DVL_USER_FATAL -DVL_USER_FINISH -DVL_PRINTF=trs_vlt_printf -include {} -fPIC",
        decl_path.display()
    );
    let mut cmd = Command::new(&opts.verilator);
    cmd.arg("--cc")
        .arg("--timing")
        .arg("--x-assign")
        .arg("0")
        .arg("--x-initial")
        .arg("0")
        .arg("-O2")
        .arg("--assert")
        .arg("--MMD")
        .arg("--top-module")
        .arg(&top)
        .arg("-Mdir")
        .arg(&obj)
        .arg("-CFLAGS")
        .arg(&cflags);
    for d in &vpath {
        cmd.arg("-y").arg(d);
    }
    cmd.arg("+libext+.v+.sv");
    for (k, v) in &defines {
        match v {
            Some(v) => cmd.arg(format!("-D{k}={v}")),
            None => cmd.arg(format!("-D{k}")),
        };
    }
    for g in &gparams {
        cmd.arg(g);
    }
    cmd.arg(&top_file);
    run_logged(cmd, "verilate").map_err(|e| {
        // capability floor: a verilator without --timing (pre-5.0)
        // rejects the OPTION itself -- report the requirement, not the
        // design
        if let VltError::Tool { detail, .. } = &e {
            let lc = detail.to_ascii_lowercase();
            if lc.contains("timing")
                && (lc.contains("invalid option") || lc.contains("unknown option"))
            {
                let found = meta::verilator_version(&opts.verilator)
                    .map(|(_, _, full)| full)
                    .unwrap_or_else(|_| "unknown".into());
                return VltError::tool(
                    "verilator floor",
                    format!(
                        "{} does not support --timing; trs requires \
                         Verilator >= 5.0 (found: {found}). Point \
                         TRS_VERILATOR at the pinned build.",
                        opts.verilator.display()
                    ),
                );
            }
        }
        e
    })?;

    // ---- DPI refusal: V<top>__Dpi.h emission is deterministic on
    // every version -- a version-proof file-existence check, the only
    // DPI detection since v1.4.
    if obj.join(format!("V{top}__Dpi.h")).is_file() {
        return Err(VltError::refuse(
            "dpi",
            format!("{top} imports or exports DPI (V{top}__Dpi.h emitted)"),
        ));
    }

    // ---- metadata from the build products; timing selects the
    // verilated_timing.o + coroutine link inputs and refuses the
    // out-clock combination
    let m = meta::scrape(&obj, &top)?;
    let timing = m.vm_timing;
    if timing && !c.out_clocks.is_empty() {
        return Err(VltError::refuse(
            "timing-outclock",
            format!(
                "{top} has both delay constructs and output clocks; a \
                 --timing drain can move an output clock through several \
                 edges between commits, which the derived-clock network \
                 cannot observe"
            ),
        ));
    }

    // ---- shim generation (includes contract-vs-model checks)
    let (shim_cpp, chash) = shim::generate(c, strings, &m)?;
    debug_assert_eq!(chash, expect_hash);
    let shim_path = class_dir.join("shim.cpp");
    std::fs::write(&shim_path, shim_cpp)
        .map_err(|e| VltError::tool("shim write", format!("{}: {e}", shim_path.display())))?;

    // ---- compile the model library
    let mut mk = Command::new("make");
    mk.arg("-s")
        .arg("-C")
        .arg(&obj)
        .arg("-f")
        .arg(format!("V{top}.mk"))
        .arg(format!("V{top}__ALL.a"))
        .arg("verilated.o")
        .arg("verilated_threads.o");
    if timing {
        // --timing adds verilated_timing to VM_GLOBAL (the generated
        // classes.mk sets VM_TIMING=1, which also turns on the
        // coroutine flags inside verilated.mk)
        mk.arg("verilated_timing.o");
    }
    run_logged(mk, "model build")?;

    // ---- link the shim into a shared object
    let vroot = String::from_utf8_lossy(
        &run_logged(
            {
                let mut c = Command::new(&opts.verilator);
                c.arg("--getenv").arg("VERILATOR_ROOT");
                c
            },
            "verilator root probe",
        )?
        .stdout,
    )
    .trim()
    .to_string();
    // per-platform C++ configuration from the pinned verilator's own
    // build config: include/verilated.mk is written by its configure
    // and is the SAME file the model objects were just built with, so
    // the shim compiles with the matching compiler and (for timing
    // models) the platform's coroutine flags -- gcc wants -fcoroutines,
    // clang/macOS configures differently.  Missing values fall back to
    // the Linux/gcc defaults.
    let vmk = std::fs::read_to_string(format!("{vroot}/include/verilated.mk")).unwrap_or_default();
    let mk_var = |name: &str| -> Option<String> {
        vmk.lines().find_map(|l| {
            let (k, v) = l.split_once('=')?;
            let k = k.trim_end_matches(['?', ':']).trim();
            (k == name).then(|| v.trim().to_string())
        })
    };
    let cxx = mk_var("CXX")
        .and_then(|v| v.split_whitespace().next().map(String::from))
        .unwrap_or_else(|| "g++".into());
    let coroutine_flags: Vec<String> = mk_var("CFG_CXXFLAGS_COROUTINES")
        .map(|v| v.split_whitespace().map(String::from).collect())
        .unwrap_or_else(|| vec!["-fcoroutines".into()]);
    let mut ld = Command::new(&cxx);
    ld.arg("-shared")
        .arg("-fPIC")
        .arg("-O2")
        .arg("-std=c++17")
        .arg("-DVL_USER_FATAL");
    if timing {
        // the shim includes V<top>.h, which pulls verilated_timing.h
        // for timing models; platform flags AFTER -std so a configured
        // standard override wins
        for f in &coroutine_flags {
            ld.arg(f);
        }
    }
    ld.arg("-DVL_USER_FINISH")
        .arg("-DVL_PRINTF=trs_vlt_printf")
        .arg("-include")
        .arg(&decl_path)
        .arg("-I")
        .arg(&obj)
        .arg("-I")
        .arg(format!("{vroot}/include"))
        .arg("-I")
        .arg(format!("{vroot}/include/vltstd"))
        .arg(&shim_path)
        .arg(obj.join(format!("V{top}__ALL.a")))
        .arg(obj.join("verilated.o"))
        .arg(obj.join("verilated_threads.o"));
    if timing {
        ld.arg(obj.join("verilated_timing.o"));
    }
    ld.arg("-lpthread").arg("-lz").arg("-o").arg(&so_path);
    run_logged(ld, "shim link")?;

    // ---- license notice, manifest (manifest LAST: its presence marks
    // the class entry valid), load check
    let _ = std::fs::write(class_dir.join("NOTICE"), NOTICE);
    let mut deps = parse_depfiles(&obj);
    if !deps.contains(&top_file) {
        deps.push(top_file.clone());
    }
    deps.sort();
    write_manifest(&manifest, &deps)?;
    dlopen_check(&so_path, &expect_hash)?;
    write_byid(&opts.cache_dir, &runkey, &class);

    Ok(BuiltModel {
        so_path,
        contract_hash: expect_hash,
        cached: false,
        class,
        verilog_name: top,
    })
}

/// LOAD-ONLY lookup: find the model artifact a prior BUILD step (trs
/// link / trs vlt build) produced for this contract and parameter
/// valuation, without touching the verilator binary or the source
/// tree.  Verilation is a build step (Ravi, 2026-08-23): the run side
/// never verilates, so a miss here is the caller's cue to report
/// "rebuild", not to build.  Staleness is deliberately NOT re-checked
/// (the manifest's dep contents belong to the build environment) --
/// like any compiled artifact, a stale model runs until the build step
/// is re-run; the contract hash is still verified on load.
pub fn find_model_resolved(
    c: &BviContract,
    strings: &[String],
    opts: &BuildOptions,
    resolved: Option<&[ResolvedParam]>,
) -> Result<Option<BuiltModel>, VltError> {
    let s = |id: u32| strings.get(id as usize).map(String::as_str).unwrap_or("");
    let top = s(c.verilog_name).to_string();
    let gparams = serialize_params(c, strings, resolved)?;
    let runkey = run_key(c, strings, &gparams);
    let byid = opts.cache_dir.join("vlt").join("byid").join(&runkey);
    let Ok(class) = std::fs::read_to_string(&byid) else {
        return Ok(None);
    };
    let class = class.trim().to_string();
    let so_path = opts
        .cache_dir
        .join("vlt")
        .join(&class)
        .join(format!("lib{top}_shim.vlt.so"));
    if !so_path.is_file() {
        return Ok(None);
    }
    let cjson = shim::contract_json(c, strings);
    let expect_hash = sha256::digest_hex(cjson.as_bytes())[..16].to_string();
    dlopen_check(&so_path, &expect_hash)?;
    Ok(Some(BuiltModel {
        so_path,
        contract_hash: expect_hash,
        cached: true,
        class,
        verilog_name: top,
    }))
}

/// dlopen the built model and confirm it reports the expected contract
/// hash through vlt_contract -- the load-time identity check that model
/// substitution (--bvi-model) also goes through.
fn dlopen_check(so: &Path, expect_hash: &str) -> Result<(), VltError> {
    use std::ffi::{CStr, CString};
    let cpath = CString::new(so.display().to_string())
        .map_err(|_| VltError::tool("dlopen", "path contains NUL".into()))?;
    let handle = unsafe { libc::dlopen(cpath.as_ptr(), libc::RTLD_NOW | libc::RTLD_LOCAL) };
    if handle.is_null() {
        let err = unsafe { CStr::from_ptr(libc::dlerror()) };
        return Err(VltError::tool(
            "dlopen",
            format!("{}: {}", so.display(), err.to_string_lossy()),
        ));
    }
    let sym = CString::new("vlt_contract").unwrap();
    let f = unsafe { libc::dlsym(handle, sym.as_ptr()) };
    let result = if f.is_null() {
        Err(VltError::tool(
            "dlopen",
            format!("{}: no vlt_contract symbol", so.display()),
        ))
    } else {
        let getc: extern "C" fn() -> *const libc::c_char = unsafe { std::mem::transmute(f) };
        let text = unsafe { CStr::from_ptr(getc()) }
            .to_string_lossy()
            .to_string();
        let want = format!("\"hash\":\"{expect_hash}\"");
        if text.contains(&want) {
            Ok(())
        } else {
            Err(VltError::refuse(
                "contract-mismatch",
                format!(
                    "{}: loaded model's contract hash does not match the design \
                     (rebuilt from a different contract?)",
                    so.display()
                ),
            ))
        }
    };
    unsafe { libc::dlclose(handle) };
    result
}

/// Build every distinct BVI model class in a design.  Returns one entry
/// per (module, instance) carrying the built model; classes are shared,
/// so two instances of the same import build once.
pub fn build_all(
    design: &Design,
    opts: &BuildOptions,
) -> Result<Vec<(String, BuiltModel)>, VltError> {
    let mut out = Vec::new();
    let mut done: Vec<(String, BuiltModel)> = Vec::new();
    for m in &design.modules {
        for inst in &m.instances {
            if let trs_ir::InstanceKind::Bvi(c) = &inst.kind {
                let iname = design.strings[inst.name as usize].clone();
                // forwarded parameters resolve at instantiation (in the
                // parent context); those classes verilate during load,
                // which still happens inside `trs link`
                if c.params
                    .iter()
                    .any(|p| matches!(p.value, BviParamValue::FromArg { .. }))
                {
                    if opts.verbose {
                        eprintln!(
                            "trs-vlt: {} ({}) has forwarded parameters; \
                             verilating at instantiation",
                            iname, design.strings[c.verilog_name as usize]
                        );
                    }
                    continue;
                }
                // dedup on the RUN IDENTITY (contract + literal
                // parameter values + defines + vpath), never on the
                // bare contract JSON: that hash is interface-shape
                // only, and two imports of one module with different
                // literal parameters are DISTINCT classes that must
                // each build (and each get a byid entry)
                let key = run_identity(c, &design.strings, None)?;
                let built = match done.iter().find(|(k, _)| *k == key) {
                    Some((_, b)) => b.clone(),
                    None => {
                        let b = build_model(c, &design.strings, opts)?;
                        done.push((key, b.clone()));
                        b
                    }
                };
                out.push((iname, built));
            }
        }
    }
    Ok(out)
}
