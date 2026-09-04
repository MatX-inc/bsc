//! Startup-path code (BIR loading, snapshot sidecar, phase timing),
//! kept out of lib.rs for hygiene.
//!
//! DOCTRINE (2026-07-10 fence flags): never spawn a thread on the run
//! startup path.  One short-lived thread permanently drops glibc
//! malloc's single-threaded fast path, and the interpreter's
//! Value-clone-heavy eval loop paid ~50% wall for it (dft64 22s->44s).
//! Compiled/arena designs don't notice — the interp fallback does.

use crate::{bir_fingerprint, Design, Interp, WaveFormat};

/// TRS_STARTUP_TIME: wall-clock laps for the startup phases (decode,
/// instance build, prime, plan) — the run-side counterpart of
/// TRS_JIT_TIME's compile-phase brackets.
pub(crate) struct StartupLap(Option<std::time::Instant>);
impl StartupLap {
    #[cold]
    #[inline(never)]
    pub(crate) fn new() -> Self {
        Self(std::env::var_os("TRS_STARTUP_TIME").map(|_| std::time::Instant::now()))
    }
    #[cold]
    #[inline(never)]
    pub(crate) fn lap(&mut self, phase: &str) {
        if let Some(t) = &mut self.0 {
            eprintln!("trs startup: {phase} {:?}", t.elapsed());
            *t = std::time::Instant::now();
        }
    }
}

#[cold]
#[inline(never)]
pub fn load_file(
    path: &str,
    plusargs: &[String],
    binds: &[crate::topbind::TopBind],
    vcd_file: Option<&str>,
) -> Result<Interp, String> {
    load_file_inner(path, plusargs, binds, vcd_file, true)
}

/// Code-aware load: prefer the design snapshot EMBEDDED in the
/// artifact (--code), so the fast path never opens the .bir (full-AOT
/// doctrine: the .bir is the debug/link sidecar).  Falls back to the
/// .bir for pre-snap artifacts or any embedded-gate failure; the
/// fallback keeps the fingerprint cross-check.
#[cfg(feature = "aot")]
pub fn load_file_or_code(
    path: &str,
    code: Option<&str>,
    plusargs: &[String],
    binds: &[crate::topbind::TopBind],
    vcd_file: Option<&str>,
) -> Result<Interp, String> {
    let mut sl = StartupLap::new();
    // binding designs load from the .bir: the embedded snap adopts
    // the ARTIFACT's identity hash (which folded the LINK-time bind
    // salt), so a run with different bindings would wrongly accept
    // the baked code.  Loading from the .bir recomputes the identity
    // from this run's bindings and the stamp check does its job.
    if let Some(so) = code.filter(|_| binds.is_empty()) {
        if let Some((hash, design)) =
            crate::jit::aot_embedded_design(&crate::jit::ArtifactSource::Path(so.into()))
        {
            sl.lap("design load (artifact-embedded snap)");
            let mut interp = Interp::new_bound(design, binds)?;
            sl.lap("interp build (instantiate)");
            interp.bir_hash = hash ^ interp.top_binds_salt();
            interp.fe.plusargs = plusargs.to_vec();
            interp.wave_pending = vcd_file.map(|f| (WaveFormat::Vcd, Some(f.to_string())));
            // user BDPI code stays a companion .so: prefer the
            // artifact's sibling, fall back to the .bir's
            let stems = [
                so.strip_suffix(".so").unwrap_or(so).to_string(),
                path.strip_suffix(".bir").unwrap_or(path).to_string(),
            ];
            for stem in stems {
                let b = stem + ".bdpi.so";
                if std::path::Path::new(&b).exists() {
                    let b = if b.contains('/') { b } else { format!("./{b}") };
                    interp.load_bdpi(&b)?;
                    break;
                }
            }
            return Ok(interp);
        }
    }
    load_file_inner(path, plusargs, binds, vcd_file, true)
}

#[cfg(not(feature = "aot"))]
pub fn load_file_or_code(
    path: &str,
    _code: Option<&str>,
    plusargs: &[String],
    binds: &[crate::topbind::TopBind],
    vcd_file: Option<&str>,
) -> Result<Interp, String> {
    load_file_inner(path, plusargs, binds, vcd_file, true)
}

/// `load_file` that ignores any snapshot sidecar.  `trs link` is the
/// snapshot WRITER: it must decode the .bir source of truth, never a
/// prior cache, so a gate-passing-but-wrong snapshot can never be
/// laundered into a fresh artifact and re-persisted under a valid
/// header (the relink pays ~the CBOR decode against a multi-second
/// LLVM link — noise).
#[cold]
#[inline(never)]
pub fn load_file_fresh(
    path: &str,
    plusargs: &[String],
    binds: &[crate::topbind::TopBind],
    vcd_file: Option<&str>,
) -> Result<Interp, String> {
    load_file_inner(path, plusargs, binds, vcd_file, false)
}

/// `load_file_fresh` over one design's fragments -- one per
/// synthesized module -- linked into a design by
/// `trs_ir::link::assemble`.  No snapshot: the sidecar is keyed by a
/// single file's fingerprint, and the design here is not any one of
/// these files.
#[cold]
#[inline(never)]
pub fn load_fragments_fresh(
    paths: &[&str],
    plusargs: &[String],
    binds: &[crate::topbind::TopBind],
    vcd_file: Option<&str>,
) -> Result<Interp, String> {
    let mut sl = StartupLap::new();
    // the top's fragment is named last: it is where a companion
    // .bdpi.so sits and what a diagnostic should name
    let path = *paths.last().ok_or("no fragments to link")?;
    let mut birs = Vec::with_capacity(paths.len());
    for p in paths {
        let bytes = std::fs::read(p).map_err(|e| format!("{p}: {e}"))?;
        birs.push(trs_ir::Bir::decode(&bytes).map_err(|e| format!("{p}: {e}"))?);
    }
    sl.lap("fragment read+decode");
    let design = trs_ir::link::assemble(birs).map_err(|e| e.to_string())?;
    // the fingerprint is of the ASSEMBLED design, not of the files it
    // came from: it is what stamps the compiled artifact, and what the
    // artifact carries alongside is that same design
    // (Interp::write_bir).  Keyed by the inputs instead, every run
    // would find the stamp stale and compile again.
    let hash = bir_fingerprint(&design.encode());
    sl.lap("fragment link");
    finish_load(design, hash, sl, path, plusargs, binds, vcd_file)
}

/// Read one .bir and everything it needs, then link.
///
/// A .bir bsc writes holds one synthesized module and names the ones it
/// instantiates.  Those are looked for by module name beside the file
/// given, which is what bsc's own linker does with .ba -- see
/// `getABIHierarchy`, which searches its path for a child's file rather
/// than being handed the set.  `trs link --multi-fragments` remains the
/// way to name a set explicitly.
#[cold]
#[inline(never)]
/// Where the compiler's own .bir files live.
///
/// bsc ships a .ba for each `import "BDPI"' its libraries declare
/// (Randomizable's rand32 and srand), and the install exports one .bir
/// each beside them.  A design that calls one names it in its fragment
/// and has no copy of it to offer, so the search that used to happen
/// when the signature was read at export time happens here instead.
fn library_dir() -> Option<std::path::PathBuf> {
    if let Some(d) = std::env::var_os("BLUESPECDIR") {
        return Some(std::path::PathBuf::from(d).join("Libraries"));
    }
    // <prefix>/bin/trs -> <prefix>/lib/Libraries, as bsc's wrapper
    // derives BLUESPECDIR when nothing in the environment has
    let exe = std::env::current_exe().ok()?;
    Some(exe.parent()?.parent()?.join("lib").join("Libraries"))
}

fn decode_with_siblings(path: &str, bytes: &[u8]) -> Result<Design, String> {
    let first = trs_ir::Bir::decode(bytes).map_err(|e| format!("{path}: {e}"))?;
    let dir = std::path::Path::new(path)
        .parent()
        .unwrap_or(std::path::Path::new("."));

    // Modules and BDPI imports are chased the same way -- each is a
    // name the file references and a .bir beside it -- but they are
    // separate namespaces, so a module never satisfies an import.
    let strs = |v: Vec<&str>| -> Vec<String> { v.into_iter().map(|s| s.to_string()).collect() };
    let mut have: std::collections::HashSet<(bool, String)> = std::collections::HashSet::new();
    let mut want: Vec<(bool, String)> = Vec::new();
    let mut note = |bir: &trs_ir::Bir,
                    have: &mut std::collections::HashSet<(bool, String)>,
                    want: &mut Vec<(bool, String)>| {
        have.extend(strs(bir.module_names()).into_iter().map(|n| (false, n)));
        have.extend(strs(bir.foreign_names()).into_iter().map(|n| (true, n)));
        want.extend(strs(bir.extern_names()).into_iter().map(|n| (false, n)));
        want.extend(
            strs(bir.foreign_call_names())
                .into_iter()
                .map(|n| (true, n)),
        );
    };
    note(&first, &mut have, &mut want);
    let mut birs = vec![first];

    while let Some((is_ff, name)) = want.pop() {
        if have.contains(&(is_ff, name.clone())) {
            continue;
        }
        let p = dir.join(format!("{name}.bir"));
        // beside the file that names it, then among the compiler's own
        let (p, b) = match std::fs::read(&p) {
            Ok(b) => (p, b),
            Err(e) => {
                let lib = library_dir().map(|d| d.join(format!("{name}.bir")));
                match lib.as_ref().and_then(|l| std::fs::read(l).ok()) {
                    Some(b) => (lib.expect("read implies a path"), b),
                    None => {
                        return Err(format!(
                            "{}: {} `{name}', and {} could not be read: {e}",
                            path,
                            if is_ff { "imports" } else { "instantiates" },
                            p.display()
                        ))
                    }
                }
            }
        };
        let bir = trs_ir::Bir::decode(&b).map_err(|e| format!("{}: {e}", p.display()))?;
        note(&bir, &mut have, &mut want);
        birs.push(bir);
    }

    trs_ir::link::assemble(birs).map_err(|e| e.to_string())
}

#[cold]
#[inline(never)]
fn load_file_inner(
    path: &str,
    plusargs: &[String],
    binds: &[crate::topbind::TopBind],
    vcd_file: Option<&str>,
    use_snap: bool,
) -> Result<Interp, String> {
    let mut sl = StartupLap::new();
    let bytes = std::fs::read(path).map_err(|e| format!("{path}: {e}"))?;
    // decoded-design snapshot beside the .bir (written by trs link):
    // skip the CBOR parse when every snap_decode gate passes (all
    // gates run BEFORE the payload deserialize, so a stale or corrupt
    // snap costs a header read, not a decode).
    let snap = format!("{}.birsnap", path.strip_suffix(".bir").unwrap_or(path));
    // NO threads here (or anywhere before the event loop): spawning
    // even one short-lived thread permanently drops glibc malloc's
    // single-threaded fast path, which cost interp-fallback designs
    // (dft64) ~50% wall (2026-07-10 fence flags).
    // A run's .bir is the design (a link wrote it), so its own bytes
    // identify it.  A LINK may have been handed one fragment of a
    // design and pulled the rest in beside it, so what identifies the
    // artifact is the design that came out, not the file that started
    // it -- and that is the .bir the artifact carries.
    let hash = bir_fingerprint(&bytes);
    let snapped = if use_snap {
        std::fs::read(&snap)
            .ok()
            .and_then(|sb| Design::snap_decode(&sb, hash))
    } else {
        None
    };
    sl.lap("bir read+fingerprint+snap decode");
    let design = match snapped {
        Some(d) => {
            sl.lap("design load (snapshot)");
            d
        }
        None => {
            let d = decode_with_siblings(path, &bytes)?;
            sl.lap("design load (cbor)");
            d
        }
    };
    let hash = if use_snap {
        hash
    } else {
        bir_fingerprint(&design.encode())
    };
    // BIR-level Extract-of-Concat folding (TRS_BIR_FOLD).  Off by
    // default: it fires on a small minority of concats and shows no
    // wall-clock win yet -- see trs_ir::fold.  TRS_FOLD_STATS prints the
    // census, which is the part actually worth having.
    let mut design = design;
    if std::env::var_os("TRS_BIR_FOLD").is_some_and(|v| v != "0") {
        let mut tot = trs_ir::fold::FoldStats::default();
        for m in design.modules.iter_mut() {
            tot.merge(trs_ir::fold::fold_module(m));
        }
        if std::env::var_os("TRS_FOLD_STATS").is_some() {
            eprintln!("{tot}");
        }
        sl.lap("bir fold (extract-of-concat)");
    }
    finish_load(design, hash, sl, path, plusargs, binds, vcd_file)
}

/// The half of a load that does not care where the design came from.
#[cold]
#[inline(never)]
fn finish_load(
    design: Design,
    hash: u64,
    mut sl: StartupLap,
    path: &str,
    plusargs: &[String],
    binds: &[crate::topbind::TopBind],
    vcd_file: Option<&str>,
) -> Result<Interp, String> {
    let mut interp = Interp::new_bound(design, binds)?;
    sl.lap("interp build (instantiate)");
    // the bind salt differentiates compiled artifacts by their baked
    // constants (stamp and check both derive from bir_hash); the
    // snapshot key strips it again (write_snapshot)
    interp.bir_hash = hash ^ interp.top_binds_salt();
    // +NAME=value arguments consumed as bindings are not plusargs
    interp.fe.plusargs = plusargs
        .iter()
        .filter(|p| !interp.consumed_plus().iter().any(|c| c == *p))
        .cloned()
        .collect();
    interp.wave_pending = vcd_file.map(|f| (WaveFormat::Vcd, Some(f.to_string())));
    // user BDPI code lives in a companion shared object next to the .bir
    let so = path.strip_suffix(".bir").unwrap_or(path).to_string() + ".bdpi.so";
    if std::path::Path::new(&so).exists() {
        // dlopen treats a bare filename as a library-search-path lookup;
        // make the sibling path explicit
        let so = if so.contains('/') {
            so
        } else {
            format!("./{so}")
        };
        interp.load_bdpi(&so)?;
    }
    Ok(interp)
}

impl Interp {
    /// Write the design this interp holds as a whole-design .bir.
    ///
    /// What a fragment link puts in its artifact is the assembled
    /// design: there is no single input file to copy, and the top's
    /// fragment alone would decode as a design of one module.
    #[cold]
    #[inline(never)]
    pub fn write_bir(&self, path: &str) -> Result<(), String> {
        std::fs::write(path, self.d.encode()).map_err(|e| format!("{path}: {e}"))
    }

    /// Write the decoded-design snapshot sidecar (`Design::snap_encode`)
    /// keyed by this interp's .bir fingerprint.  The snapshot holds
    /// the DECODED DESIGN, which is binding-independent, so the key
    /// strips the top-binds salt the loaders folded into bir_hash —
    /// a later run with different bindings may still replay it.
    #[cold]
    #[inline(never)]
    pub fn write_snapshot(&self, path: &str) -> Result<(), String> {
        let b = self.d.snap_encode(self.bir_hash ^ self.top_binds_salt())?;
        std::fs::write(path, b).map_err(|e| format!("{path}: {e}"))
    }
}
