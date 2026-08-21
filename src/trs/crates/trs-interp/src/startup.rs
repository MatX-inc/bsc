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
        Self(
            std::env::var_os("TRS_STARTUP_TIME")
                .map(|_| std::time::Instant::now()),
        )
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
        if let Some((hash, design)) = crate::jit::aot_embedded_design(
            &crate::jit::ArtifactSource::Path(so.into()),
        ) {
            sl.lap("design load (artifact-embedded snap)");
            let mut interp = Interp::new_bound(design, binds)?;
            sl.lap("interp build (instantiate)");
            interp.bir_hash = hash ^ interp.top_binds_salt();
            interp.fe.plusargs = plusargs.to_vec();
            interp.wave_pending =
                vcd_file.map(|f| (WaveFormat::Vcd, Some(f.to_string())));
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
    let snap =
        format!("{}.birsnap", path.strip_suffix(".bir").unwrap_or(path));
    // NO threads here (or anywhere before the event loop): spawning
    // even one short-lived thread permanently drops glibc malloc's
    // single-threaded fast path, which cost interp-fallback designs
    // (dft64) ~50% wall (2026-07-10 fence flags).
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
            let d = Design::decode(&bytes).map_err(|e| e.to_string())?;
            sl.lap("design load (cbor)");
            d
        }
    };
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
    interp.wave_pending =
        vcd_file.map(|f| (WaveFormat::Vcd, Some(f.to_string())));
    // user BDPI code lives in a companion shared object next to the .bir
    let so = path.strip_suffix(".bir").unwrap_or(path).to_string() + ".bdpi.so";
    if std::path::Path::new(&so).exists() {
        // dlopen treats a bare filename as a library-search-path lookup;
        // make the sibling path explicit
        let so = if so.contains('/') { so } else { format!("./{so}") };
        interp.load_bdpi(&so)?;
    }
    Ok(interp)
}

impl Interp {
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
