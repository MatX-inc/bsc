//! trs — the TRS driver.
//!
//! Invoked by `bsc` where `simLink` runs today, or directly by build
//! systems.  Planned subcommands (DESIGN.md §3, §10):
//!
//!   trs ir dump <mod.bir>       pretty-print BIR (P0 diff-testing)
//!   trs link <top> <bir...>     plan + codegen + link a simulation
//!   trs run <top> [args]        JIT-and-run without artifacts

use std::process::ExitCode;

use trs_interp::hostlink;

fn usage() -> ExitCode {
    eprintln!("trs {} (phase P0 scaffold)", env!("CARGO_PKG_VERSION"));
    eprintln!("usage: trs ir dump <module.bir>");
    eprintln!("       trs ir dump --multi-fragments <module.bir>...");
    eprintln!("       trs link <module.bir> [-o <out.cexe>] [+NAME=value...]");
    eprintln!("       trs link --multi-fragments <module.bir>... [-o <out.cexe>]");
    eprintln!("       trs compile <design.bir> [-o <model.so>] [--exe] [--dump-formats vcd,fst]");
    eprintln!("                    [--spec-obj-in <dir>[:<dir>...]] [--spec-obj-out <dir>]");
    eprintln!("       trs specializations <design.bir> [-o <manifest.json>] [same codegen flags]");
    eprintln!("       trs run <module.bir> [-m max_cycles] [--code <model.so>] [--only-compiled] [+NAME=value...]");
    eprintln!("       trs vlt build <module.bir> [--vpath <dir>]... [--vfile <file>]... [--verilator <bin>] [--cache <dir>]");
    eprintln!();
    eprintln!("A link assembles the fragments into one whole-design .bir and");
    eprintln!("writes an artifact that runs it interpreted -- no LLVM, so it");
    eprintln!("is quick even on a large design.  `trs compile' is the optional");
    eprintln!("post-process that turns that design into the .so the artifact");
    eprintln!("loads on its next run, with no relink; --exe additionally links");
    eprintln!("a standalone executable from the same objects.  They are");
    eprintln!("separate because the compile costs a large design hours and");
    eprintln!("only pays for itself when the run is long enough to earn it.");
    eprintln!("`--only-compiled' refuses to run at all without that .so,");
    eprintln!("for runs whose whole point is to measure the compiled engine.");
    eprintln!("");
    eprintln!("A compile emits one object per SPECIALIZATION -- a module type");
    eprintln!("at one parameter valuation -- plus the design's own.  A");
    eprintln!("specialization's object does not depend on the design it was");
    eprintln!("compiled in, so two designs sharing a fragment share its object");
    eprintln!("and neither has to depend on the other:");
    eprintln!("  trs specializations  write the manifest and stop.  Planning");
    eprintln!("                       only -- seconds where a compile is hours.");
    eprintln!("                       Names each object, the .bir it comes from");
    eprintln!("                       and the parameters that select it, so a");
    eprintln!("                       build graph can declare its inputs before");
    eprintln!("                       compiling anything.  JSON by default,");
    eprintln!("                       --format text for a person, stdout unless");
    eprintln!("                       -o names a file.  Pass the SAME codegen");
    eprintln!("                       flags as the compile: they salt the names.");
    eprintln!("  --spec-obj-in <ds>   `:'-separated directories to READ, each");
    eprintln!("                       another build's output.  Never written.");
    eprintln!("  --spec-obj-out <d>   the one directory WRITTEN, never read.");
    eprintln!("Inputs and output are separate so an action has declared inputs");
    eprintln!("and a declared output; one directory serving as both would be");
    eprintln!("shared mutable state a build system cannot model.  The manifest");
    eprintln!("records the codegen flags it was made under: they salt the object");
    eprintln!("names, so a compile with different ones looks for different files.");
    eprintln!();
    eprintln!("bsc writes one .bir per synthesized module and one per");
    eprintln!("`import \"BDPI\"'.  A link given the top follows its");
    eprintln!("instantiations and its imports, finding each by name beside");
    eprintln!("it; --multi-fragments names the set explicitly instead, top");
    eprintln!("last, and the artifact is named after that one.");
    eprintln!();
    eprintln!("Top-level bindings: a top module compiled with -trs may take");
    eprintln!("Bit-typed arguments/parameters; bind them with +NAME=value or");
    eprintln!("--bind NAME=value.  Bindings given to `trs link` are BAKED into");
    eprintln!("the compiled artifact (different values require a relink);");
    eprintln!("`trs run` takes them per-run.  always_enabled method arguments");
    eprintln!("bind as +<method>.<arg>=value.");
    ExitCode::from(2)
}

/// Output-affecting knobs travel as flags rather than as environment: a
/// build system keys an action on its argv, so a knob reachable only
/// through the environment does not take part in the action's identity
/// and a changed setting silently reuses the previous result.  The env
/// vars stay as the internal spelling; a flag wins by writing one,
/// single-threaded, before any planning or workers.
///
/// Knobs the LINK's own work depends on.  `--cc' names the C compiler it
/// drives -- for the BDPI companion, and for the capi link behind
/// `--interactive'; `--capi-lib' names the staticlib that link needs.
/// Both are also taken by `trs compile', which drives a C compiler of
/// its own.
fn link_knob_env(flag: &str) -> Option<&'static str> {
    Some(match flag {
        "--cc" => "TRS_CC",
        "--capi-lib" => "TRS_CAPI_LIB",
        _ => return None,
    })
}

/// Knobs that steer CODEGEN, and so belong to `trs compile' alone.  The
/// link took them before the two were split; it runs no LLVM now (not
/// even for `--interactive', which embeds the BIR and links the capi
/// rather than compiling), so it rejects them.  A flag that is quietly
/// accepted and does nothing is worse than one that is refused.
fn compile_knob_env(flag: &str) -> Option<&'static str> {
    Some(match flag {
        "--edge-ssa" => "TRS_EDGE_SSA",
        "--jit-split" => "TRS_JIT_SPLIT",
        "--jit-opt" => "TRS_JIT_OPT",
        "--jit-pipeline" => "TRS_JIT_PIPELINE",
        "--jit-threads" => "TRS_JIT_THREADS",
        "--outline" => "TRS_EDGE_SSA_OUTLINE",
        "--outline-factor" => "TRS_EDGE_SSA_OUTLINE_FACTOR",
        // Per-class object reuse.  Flags rather than environment
        // because a build system keys an action on its argv: the
        // directories a compile reads are its declared INPUTS and the
        // one it writes is its declared OUTPUT, and both belong where
        // the action can see them.  --spec-obj-in is `:`-separated
        // and read-only; --spec-obj-out is written and never read.
        "--spec-obj-in" => "TRS_SPEC_OBJ_IN",
        "--spec-obj-out" => "TRS_SPEC_OBJ_OUT",
        _ => return None,
    })
}

/// The same, for the codegen knobs that take no value.
fn compile_knob_switch(flag: &str) -> Option<&'static str> {
    Some(match flag {
        "--no-fusion" => "TRS_NO_FUSION",
        "--jit-novec" => "TRS_JIT_NOVEC",
        _ => return None,
    })
}

/// argv[0] artifact dispatch: `trs link -o art` emits `art` as a
/// SYMLINK to the runner with `art.bir`/`.so`/`.opts` beside it.
/// Invoked under a name with a sibling .bir, this binary IS the
/// artifact and recovers the run IN-PROCESS — the sh wrapper this
/// replaces cost ~5ms per exec (sh startup + two command-substitution
/// forks + an exec), more than binary load and design boot combined
/// on small designs.  Returns the synthesized `run` argv, or None for
/// a normal CLI invocation; the routed tiers (Tcl -c/-f, $TRS
/// override, slim-to-full selfcheck) exec away and never return.
#[cfg(unix)]
fn artifact_dispatch(user_args: &[String]) -> Option<Vec<String>> {
    use std::os::unix::process::CommandExt;
    let arg0 = std::env::args_os().next()?;
    let p = std::path::PathBuf::from(&arg0);
    let name = p.file_name()?.to_str()?.to_string();
    if name == "trs" || name == "trs-run" {
        return None;
    }
    // the artifact directory, like the wrapper's dirname "$0"; a bare
    // name (PATH lookup) falls back to the working directory
    let dir = match p.parent() {
        Some(d) if !d.as_os_str().is_empty() => d.to_path_buf(),
        _ => std::path::PathBuf::from("."),
    };
    let bir = dir.join(format!("{name}.bir"));
    if !bir.is_file() {
        return None;
    }
    // baked link options — what the wrapper carried as script text
    let mut top = String::new();
    let mut formats = "vcd".to_string();
    let mut split = String::new();
    let mut baked_binds: Vec<String> = Vec::new();
    if let Ok(s) = std::fs::read_to_string(dir.join(format!("{name}.opts"))) {
        for line in s.lines() {
            if let Some(v) = line.strip_prefix("top=") {
                top = v.to_string();
            } else if let Some(v) = line.strip_prefix("formats=") {
                formats = v.to_string();
            } else if let Some(v) = line.strip_prefix("split=") {
                split = v.to_string();
            } else if let Some(v) = line.strip_prefix("bind=") {
                // link-time top bindings, re-supplied per run (the
                // compiled bodies baked them via port_consts; a
                // conflicting user +NAME=value errors at load)
                baked_binds.push(v.to_string());
            }
        }
    }
    // -c/-f: the debug/script tier — stock bluetcl + the capi shim
    // (bluesim.tcl), exactly the wrapper's dispatch
    let capi = dir.join(format!("{name}.capi.so"));
    if user_args.iter().any(|a| a == "-c" || a == "-f") && capi.is_file() && !top.is_empty() {
        let bsdir = std::process::Command::new("bluetcl")
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .spawn()
            .ok()
            .and_then(|mut c| {
                use std::io::Write;
                c.stdin
                    .take()?
                    .write_all(b"puts $env(BLUESPECDIR)\n")
                    .ok()?;
                let out = c.wait_with_output().ok()?;
                out.status
                    .success()
                    .then(|| String::from_utf8_lossy(&out.stdout).trim().to_string())
            });
        if let Some(bsdir) = bsdir.filter(|b| !b.is_empty()) {
            let e = std::process::Command::new(format!("{bsdir}/tcllib/bluespec/bluesim.tcl"))
                .arg(&capi)
                .arg(&top)
                .arg("--script_name")
                .arg(&name)
                .args(user_args)
                .env("TRS_CAPI_FORMATS", &formats)
                .exec();
            eprintln!("trs: bluesim.tcl: {e}");
            std::process::exit(2);
        }
        // bluetcl absent: fall through to the fast runner, like the
        // wrapper's test -f guard did
    }
    let mut synth = vec!["run".to_string(), bir.to_str()?.to_string()];
    let so = dir.join(format!("{name}.so"));
    if so.is_file() {
        synth.push("--code".into());
        synth.push(so.to_str()?.into());
    }
    if !split.is_empty() {
        synth.push("--split".into());
        synth.push(split);
    }
    synth.push("--formats".into());
    synth.push(formats);
    for b in baked_binds {
        synth.push("--bind".into());
        synth.push(b);
    }
    synth.extend(user_args.iter().cloned());
    // $TRS points the run at a specific build (the testsuite's hook)
    if let Some(t) = std::env::var_os("TRS") {
        let e = std::process::Command::new(&t).args(&synth).exec();
        eprintln!("trs: exec {}: {e}", std::path::Path::new(&t).display());
        std::process::exit(2);
    }
    // slim build: selfcheck/jit modes need the FULL binary beside the
    // real runner (arm_jit is a no-op here — it would silently label a
    // second interp shadow "jit" and weaken the 3-way oracle)
    #[cfg(not(feature = "jit"))]
    {
        let wants_full = user_args.iter().any(|a| a == "--selfcheck")
            || std::env::var_os("TRS_SELFCHECK").is_some()
            || std::env::var_os("TRS_JIT").is_some();
        if wants_full {
            if let Ok(me) = std::env::current_exe() {
                let full = me.with_file_name("trs");
                if full.is_file() {
                    let e = std::process::Command::new(&full).args(&synth).exec();
                    eprintln!("trs: exec {}: {e}", full.display());
                    std::process::exit(2);
                }
            }
        }
    }
    Some(synth)
}

/// `trs compile`, and `trs specializations` which is the same action stopped
/// after planning.  One parser for both: the codegen knobs salt the
/// object names, so a manifest produced under different ones names
/// files the compile will never look for, and a second parser would
/// drift from this one without saying so.
fn compile_cmd(rest: &[&str]) -> ExitCode {
        let mut path: Option<&str> = None;
        let mut out: Option<String> = None;
        // the link's own default, so a compile that is told nothing
        // stamps what a link that was told nothing would have
        let mut fmt_arg: Option<String> = None;
        let mut want_exe = false;
        let mut manifest = false;
        let mut as_text = false;
        let mut it = rest.iter().copied();
        while let Some(a) = it.next() {
            match a {
                "-o" | "--output" => match it.next() {
                    Some(v) => out = Some(v.to_string()),
                    None => {
                        eprintln!("trs compile: -o needs a file");
                        return ExitCode::from(2);
                    }
                },
                // the standalone executable: the SAME objects as
                // the .so plus a main shim, so it is a second
                // output of one codegen, not a step after it
                "--exe" => want_exe = true,
                // Write the manifest and stop: which specializations
                // this design needs, and the object file each
                // would be reused from.  Planning only, no LLVM --
                // seconds where a compile is hours, which is what
                // lets a build graph name its inputs before paying
                // for any of them.
                //
                // appended by `trs specializations`; not a user-facing flag,
                // which is why it takes no value and is absent from the
                // usage text
                "--manifest-mode" => manifest = true,
                // JSON is what a build integration reads; text is for
                // a person asking what a design is made of.  JSON is
                // the default because an integration that silently
                // started receiving prose would break, and a person
                // can ask for the other.
                "--format" => match it.next() {
                    Some(v @ ("json" | "text")) => as_text = v == "text",
                    Some(v) => {
                        eprintln!("trs specializations: --format takes json or text, not `{v}'");
                        return ExitCode::from(2);
                    }
                    None => {
                        eprintln!("trs specializations: --format needs json or text");
                        return ExitCode::from(2);
                    }
                },
                // the allowed wave formats fold into the design's
                // identity, so a compile must be told whatever the
                // link was told
                "--dump-formats" => match it.next() {
                    Some(v) => fmt_arg = Some(v.to_string()),
                    None => {
                        eprintln!("trs compile: --dump-formats needs a value");
                        return ExitCode::from(2);
                    }
                },
                // the codegen knobs describe THIS step's work, so
                // this is where they have to be reachable as flags
                _ if compile_knob_env(a).is_some() || link_knob_env(a).is_some() => {
                    let key = compile_knob_env(a).or_else(|| link_knob_env(a)).unwrap();
                    match it.next() {
                        Some(v) => std::env::set_var(key, v),
                        None => {
                            eprintln!("trs compile: {a} requires a value");
                            return ExitCode::from(2);
                        }
                    }
                }
                _ if compile_knob_switch(a).is_some() => {
                    std::env::set_var(compile_knob_switch(a).unwrap(), "1")
                }
                _ if a.starts_with('-') => {
                    eprintln!("trs compile: unknown option `{a}'");
                    return ExitCode::from(2);
                }
                _ if path.is_none() => path = Some(a),
                _ => {
                    eprintln!("trs compile: one .bir at a time");
                    return ExitCode::from(2);
                }
            }
        }
        let Some(path) = path else {
            eprintln!("trs compile: no .bir named");
            return ExitCode::from(2);
        };
        // The .so is named for the .bir beside it, because that is
        // where the artifact looks: <base>.bir -> <base>.so.
        let base = path.strip_suffix(".bir").unwrap_or(path).to_string();
        // -o names the manifest rather than an object, because an
        // object is not what this action produces, and `-` or no -o
        // means stdout -- in EITHER format.  A build rule always
        // passes -o, because it has to declare the file it produces;
        // everyone else is at a terminal or on the left of a pipe,
        // and `trs specializations d.bir | jq` needs no flags.
        let spec_path = manifest.then(|| out.clone().unwrap_or_else(|| "-".to_string()));
        let so = out.unwrap_or_else(|| format!("{base}.so"));
        // Replay the link's own settings from <base>.opts.  Baked
        // bindings and the allowed wave formats both fold into the
        // design's identity hash, so a .so compiled without them
        // carries a stamp the artifact will reject at run time --
        // it would fall back to interpreting and look, wrongly,
        // like a design the compiler could not take.
        let mut binds: Vec<trs_interp::TopBind> = Vec::new();
        let mut recorded: Option<String> = None;
        if let Ok(txt) = std::fs::read_to_string(format!("{base}.opts")) {
            for line in txt.lines() {
                if let Some(v) = line.strip_prefix("bind=") {
                    match trs_interp::parse_bind(v, true) {
                        Ok(b) => binds.push(b),
                        Err(e) => {
                            eprintln!("trs compile: {base}.opts: {e}");
                            return ExitCode::from(2);
                        }
                    }
                } else if let Some(v) = line.strip_prefix("formats=") {
                    recorded = Some(v.to_string());
                }
            }
        }
        // an explicit --dump-formats wins over the recorded one,
        // and the link's own default stands in for neither
        let fmt = fmt_arg.or(recorded).unwrap_or_else(|| "vcd".to_string());
        let formats = (
            fmt.split(',').any(|t| t == "vcd"),
            fmt.split(',').any(|t| t == "fst"),
        );
        // This loads the design, and loading a BVI design builds its
        // BviPrims, which look the model cache up.  A compile is a
        // CONSUMER of models -- the link verilated them -- so it
        // resolves the cache beside the design, as a run does,
        // rather than allowing verilation here.
        ensure_vlt_env(path, false);
        let mut interp = match trs_interp::startup::load_file(path, &[], &binds, None) {
            Ok(i) => i,
            Err(e) => {
                eprintln!("trs compile: {e}");
                return ExitCode::FAILURE;
            }
        };
        interp.set_allowed_wave_formats(formats.0, formats.1);
        if want_exe {
            // <base> becomes a real PIE (design objects + a main
            // shim + the slim runtime), replacing whatever the
            // link left at that name
            if !binds.is_empty() || interp.has_autofire() {
                eprintln!(
                    "trs compile: --exe does not support designs \
                     with top-level bindings or always_enabled top \
                     methods (batch artifacts only)"
                );
                return ExitCode::FAILURE;
            }
            let libdir = std::env::current_exe()
                .ok()
                .and_then(|p| p.parent().map(|d| d.to_path_buf()))
                .unwrap_or_else(|| ".".into());
            interp.aot_request_emit_exe(so.clone().into(), base.clone().into(), libdir);
        } else {
            interp.aot_request_emit(so.clone().into());
        }
        if let Some(cp) = &spec_path {
            interp.aot_request_specializations(cp.into(), as_text);
        }
        interp.prime();
        // Producing the .so IS the job here, so anything short of
        // it is a failure -- unlike a link, which has a perfectly
        // good interpreted artifact to fall back on.
        match interp.aot_take_emit_result() {
            Some(trs_interp::AotEmit::Compiled) => {
                // RunCore arena sidecar (validation form): the plan's
                // deterministic post-attach arena image, cross-checked by
                // loads under TRS_RUNCORE_CHECK=1; None (interp-only or
                // traced link) removes any stale sidecar
                let arena_written = match interp.take_runcore_image() {
                    Some(img) => {
                        let t = format!("{base}.arena.tmp");
                        let ok = std::fs::write(&t, img)
                            .and_then(|()| std::fs::rename(&t, format!("{base}.arena")))
                            .is_ok();
                        if !ok {
                            eprintln!("trs compile: note: {base}.arena not written");
                        }
                        ok
                    }
                    None => {
                        let _ = std::fs::remove_file(format!("{base}.arena"));
                        false
                    }
                };
                // post-emit window bake (docs/RUNCORE.md): run the reset
                // window on the just-written artifact — quiet, on the
                // compiled engine, exactly as a run would — and bake the
                // post-window state into the sidecar when the window is
                // effect-free.  Every non-clean outcome is silent: the
                // design simply boots classic.
                if arena_written {
                    // no binds: a binding design's load refuses without
                    // them, so its bake is a silent no-op and it boots
                    // classic (run_file gates RunCore off under binds).
                    // Mem-file designs capture the window TWICE under
                    // different fill patterns (the two-fill gate): the
                    // bake is committed only if everything outside the
                    // load regions agrees — proof the boot's overlay
                    // replaces the only file-dependent state.
                    let sidecar = format!("{base}.arena");
                    let bake = (|| -> Result<bool, String> {
                        let mut b1 = trs_interp::startup::load_file(path, &[], &[], None)?;
                        b1.aot_request_code(format!("{base}.so").into());
                        if !b1.runcore_has_loads() {
                            let Some(cap) = b1.runcore_bake_capture(None) else {
                                return Ok(false);
                            };
                            return trs_interp::runcore_bake_commit(
                                std::path::Path::new(&sidecar),
                                &cap,
                                None,
                            );
                        }
                        let Some(a) = b1.runcore_bake_capture(Some(0x5555_5555_5555_5555))
                        else {
                            return Ok(false);
                        };
                        let mut b2 = trs_interp::startup::load_file(path, &[], &[], None)?;
                        b2.aot_request_code(format!("{base}.so").into());
                        let Some(b) = b2.runcore_bake_capture(Some(0xAAAA_AAAA_AAAA_AAAA))
                        else {
                            return Ok(false);
                        };
                        trs_interp::runcore_bake_commit(
                            std::path::Path::new(&sidecar),
                            &a,
                            Some(&b),
                        )
                    })();
                    if let Err(e) = bake {
                        eprintln!("trs compile: note: window bake skipped: {e}");
                    }
                }
                // A link points the artifact at the full binary,
                // because it cannot know a .so will ever exist and
                // the slim runner cannot JIT in-process.  One does
                // now, so the startup cost of LLVM's constructors
                // buys nothing -- re-point at the slim runner.
                // Only a symlink: an --exe PIE is the artifact.
                if !want_exe {
                    if let Ok(m) = std::fs::symlink_metadata(&base) {
                        if m.file_type().is_symlink() {
                            if let Some(slim) = std::env::current_exe()
                                .ok()
                                .map(|p| p.with_file_name("trs-run"))
                                .filter(|p| p.is_file())
                            {
                                let t = format!("{base}.lnk.tmp");
                                let _ = std::fs::remove_file(&t);
                                if std::os::unix::fs::symlink(&slim, &t)
                                    .and_then(|()| std::fs::rename(&t, &base))
                                    .is_err()
                                {
                                    let _ = std::fs::remove_file(&t);
                                }
                            }
                        }
                    }
                }
                // the capi's aot engine looks for the compiled
                // design beside the model it loads
                if std::path::Path::new(&format!("{base}.capi.so")).exists() {
                    let aot = format!("{base}.capi.aot.so");
                    let _ = std::fs::remove_file(&aot);
                    if std::os::unix::fs::symlink(
                        std::path::Path::new(&so)
                            .file_name()
                            .unwrap_or(std::ffi::OsStr::new(&so)),
                        &aot,
                    )
                    .is_err()
                    {
                        let _ = std::fs::copy(&so, &aot);
                    }
                }
                // silent on success, like any other compiler: the
                // .so is the output, and stderr here is captured
                // beside codegen dumps that must not pick up a
                // stray line
                ExitCode::SUCCESS
            }
            Some(trs_interp::AotEmit::Failed(e)) => {
                eprintln!("trs compile: {e}");
                ExitCode::FAILURE
            }
            Some(trs_interp::AotEmit::Manifest) => {
                eprintln!(
                    "trs specializations: wrote {}",
                    match spec_path.as_deref() {
                        Some("-") | None => "(stdout)",
                        Some(p) => p,
                    }
                );
                ExitCode::SUCCESS
            }
            Some(trs_interp::AotEmit::Ineligible(e)) => {
                eprintln!(
                    "trs compile: compiled mode is unavailable for \
                     this design ({e})"
                );
                ExitCode::from(86)
            }
            None => {
                eprintln!(
                    "trs compile: compiled mode is unavailable for \
                     this design (TRS_JIT_TRACE=1 shows why)"
                );
                ExitCode::from(86)
            }
        }
    }

fn main() -> ExitCode {
    // reference parity for `./model | head`: Rust starts with SIGPIPE
    // ignored, so a $display into a closed pipe returned EPIPE and the
    // print panicked — and the panic crossed the jit's extern "C"
    // foreign callback, aborting with a backtrace wall.  The reference
    // C++ model just dies on SIGPIPE (shell reports 141); restore that.
    #[cfg(unix)]
    unsafe {
        libc::signal(libc::SIGPIPE, libc::SIG_DFL);
    }
    #[allow(unused_mut)]
    let mut args: Vec<String> = std::env::args().skip(1).collect();
    #[cfg(unix)]
    if let Some(synth) = artifact_dispatch(&args) {
        args = synth;
    }
    match args
        .iter()
        .map(String::as_str)
        .collect::<Vec<_>>()
        .as_slice()
    {
        // trs features: print the compiled-in feature set, one per line
        // (the testsuite probes for "jit" to decide whether link-artifact
        // checks are supported)
        ["features"] => {
            if cfg!(feature = "aot") {
                println!("aot");
            }
            if cfg!(feature = "jit") {
                println!("jit");
            }
            ExitCode::SUCCESS
        }
        // trs ir dump: the decoded design.  --multi-fragments dumps
        // what the same set of fragments would link to, which is the
        // only way to see the assembled design without building it.
        ["ir", "dump", "--multi-fragments", paths @ ..] if !paths.is_empty() => {
            let mut birs = Vec::with_capacity(paths.len());
            for p in paths {
                let b = std::fs::read(p)
                    .map_err(|e| format!("{p}: {e}"))
                    .and_then(|b| trs_ir::Bir::decode(&b).map_err(|e| format!("{p}: {e}")));
                match b {
                    Ok(b) => birs.push(b),
                    Err(e) => {
                        eprintln!("trs: {e}");
                        return ExitCode::FAILURE;
                    }
                }
            }
            match trs_ir::link::assemble(birs) {
                Ok(design) => {
                    println!("{design:#?}");
                    ExitCode::SUCCESS
                }
                Err(e) => {
                    eprintln!("trs: {e}");
                    ExitCode::FAILURE
                }
            }
        }
        // A design body is linked before it is printed, so what comes
        // out is the design as it will run -- the derived schedule
        // included.  A fragment cannot be linked on its own, so it
        // prints as the fragment it is.
        ["ir", "dump", path] => match std::fs::read(path)
            .map_err(|e| format!("{path}: {e}"))
            .and_then(|b| trs_ir::Bir::decode(&b).map_err(|e| format!("{path}: {e}")))
        {
            Ok(bir) if matches!(bir.body, trs_ir::BirBody::Fragment(_)) => {
                println!("{bir:#?}");
                ExitCode::SUCCESS
            }
            Ok(bir) => match trs_ir::link::assemble(vec![bir]) {
                Ok(design) => {
                    println!("{design:#?}");
                    ExitCode::SUCCESS
                }
                Err(e) => {
                    eprintln!("trs: {path}: {e}");
                    ExitCode::FAILURE
                }
            },
            Err(e) => {
                eprintln!("trs: {e}");
                ExitCode::FAILURE
            }
        },
        // trs compile: the LLVM half of what a link used to do, on its
        // own.  A link writes the design and an artifact that runs it
        // interpreted; this compiles that design into the .so the
        // artifact loads on its next run.  Split because the compile
        // costs hours on a large design and only pays for itself when
        // the run is long enough -- which is a judgement for whoever
        // is running it, not for the link.
        // `trs specializations` is `trs compile` in manifest mode, deliberately
        // sharing this arm.  It is a different ACTION -- seconds not
        // hours, a manifest not an object -- and deserves its own name
        // in a build rule.  But the manifest must be produced under
        // exactly the codegen knobs the compile will use, because they
        // salt the object names, and a second parser would drift from
        // this one silently: the salt would change, the compile would
        // look for files the manifest never named, and the only
        // symptom is a 0% reuse rate.  One parser, two entry points.
        ["specializations", rest @ ..] if !rest.is_empty() => {
            let mut v: Vec<&str> = Vec::with_capacity(rest.len() + 1);
            v.extend_from_slice(rest);
            v.push("--manifest-mode");
            return compile_cmd(&v);
        }
        ["compile", rest @ ..] if !rest.is_empty() => {
            return compile_cmd(rest);
        }
        // trs link: assemble the design and write the persistent
        // artifact: <out> (wrapper script with the same CLI as
        // reference Bluesim) and <out>.bir.  The compiled <out>.so is
        // `trs compile''s to write, and the artifact picks it up on
        // whichever run comes after it exists.
        // trs vlt build: verilate every BVI model class in a design and
        // print the built shared objects — the standalone entry to the
        // verilate-or-cache pipeline that link/run also perform.
        ["vlt", "build", path, rest @ ..] => {
            // the standalone BUILD entry point: resolve the per-project
            // cache beside the .bir and allow verilation
            ensure_vlt_env(path, true);
            let mut opts = trs_vlt::BuildOptions::from_env();
            opts.verbose = true;
            let mut it = rest.iter();
            while let Some(a) = it.next() {
                let need = |v: Option<&&str>, what: &str| -> Result<String, ExitCode> {
                    v.map(|s| s.to_string()).ok_or_else(|| {
                        eprintln!("Error: {what} requires a value");
                        ExitCode::from(2)
                    })
                };
                match *a {
                    "--vpath" => match need(it.next(), "--vpath") {
                        Ok(v) => opts.extra_vpath.push(v.into()),
                        Err(e) => return e,
                    },
                    "--vfile" => match need(it.next(), "--vfile") {
                        Ok(v) => opts.extra_vfiles.push(v.into()),
                        Err(e) => return e,
                    },
                    "--verilator" => match need(it.next(), "--verilator") {
                        Ok(v) => opts.verilator = v.into(),
                        Err(e) => return e,
                    },
                    "--cache" => match need(it.next(), "--cache") {
                        Ok(v) => opts.cache_dir = v.into(),
                        Err(e) => return e,
                    },
                    other => {
                        eprintln!("Error: invalid vlt build option '{other}'");
                        return ExitCode::from(2);
                    }
                }
            }
            let bytes = match std::fs::read(path) {
                Ok(b) => b,
                Err(e) => {
                    eprintln!("trs vlt: {path}: {e}");
                    return ExitCode::FAILURE;
                }
            };
            // the whole design, not the one fragment: an import can
            // sit in any module the link reaches
            let design = match trs_interp::startup::decode_with_siblings(path, &bytes) {
                Ok(d) => d,
                Err(e) => {
                    eprintln!("trs vlt: {path}: {e}");
                    return ExitCode::FAILURE;
                }
            };
            let has_bvi = design.modules.iter().any(|m| {
                m.instances
                    .iter()
                    .any(|i| matches!(i.kind, trs_ir::InstanceKind::Bvi(_)))
            });
            let has_fwd = design.modules.iter().any(|m| {
                m.instances.iter().any(|i| match &i.kind {
                    trs_ir::InstanceKind::Bvi(c) => c
                        .params
                        .iter()
                        .any(|p| matches!(p.value, trs_ir::bvi::BviParamValue::FromArg { .. })),
                    _ => false,
                })
            });
            match trs_vlt::build_all(&design, &opts) {
                Ok(_) if !has_bvi => {
                    println!("trs vlt: no BVI instances in {path}");
                    ExitCode::SUCCESS
                }
                Ok(models) => {
                    for (inst, m) in &models {
                        println!(
                            "{inst}: {} ({}, contract {})",
                            m.so_path.display(),
                            if m.cached { "cached" } else { "built" },
                            m.contract_hash
                        );
                    }
                    // forwarded-parameter classes resolve in parent
                    // context: an elaboration pass builds them with the
                    // exact instantiation semantics of a real load, so
                    // `trs vlt build` is a COMPLETE build step (the run
                    // side is load-only since v1.5).  Flags reach the
                    // pass through the env (the house pattern): opts is
                    // written back so BviPrim::new resolves identically.
                    if has_fwd {
                        std::env::set_var("TRS_VLT_CACHE", &opts.cache_dir);
                        std::env::set_var("TRS_VERILATOR", &opts.verilator);
                        let join = |v: &[std::path::PathBuf]| {
                            v.iter()
                                .map(|p| p.display().to_string())
                                .collect::<Vec<_>>()
                                .join(":")
                        };
                        if !opts.extra_vpath.is_empty() {
                            std::env::set_var("TRS_VLT_VPATH", join(&opts.extra_vpath));
                        }
                        if !opts.extra_vfiles.is_empty() {
                            std::env::set_var("TRS_VLT_VFILES", join(&opts.extra_vfiles));
                        }
                        trs_interp::prim::set_load_memfiles(false);
                        match trs_interp::startup::load_file_fresh(path, &[], &[], None) {
                            Ok(_) => println!(
                                "trs vlt: forwarded-parameter classes \
                                 verilated via elaboration"
                            ),
                            Err(e) => {
                                eprintln!("trs vlt: {e}");
                                return ExitCode::FAILURE;
                            }
                        }
                    }
                    ExitCode::SUCCESS
                }
                Err(e) => {
                    eprintln!("trs vlt: {e}");
                    ExitCode::FAILURE
                }
            }
        }
        // trs link: compile the design ahead of time and write the
        // persistent artifact: <out> (wrapper script with the same CLI
        // as reference Bluesim), <out>.bir, <out>.so.  Runs never
        // compile again — same amortization as Verilator/VCS/Bluesim.
        ["link", rest @ ..] if !rest.is_empty() => {
            let mut out: Option<String> = None;
            // --multi-fragments: the positional arguments are one
            // design's fragments, one per synthesized module, named
            // explicitly rather than found beside the first.  Read
            // before the loop, so it governs the positionals whatever
            // order they were written in.
            let multi = rest.contains(&"--multi-fragments");
            let mut frags: Vec<&str> = Vec::new();
            // BDPI implementations.  bsc takes these at ITS link too
            // (`bsc -sim -e top foo.c`): a foreign function belongs to
            // a design, not to any one of its modules, so the export
            // has no business compiling them.
            let mut bdpi: Vec<&str> = Vec::new();
            let mut bdpi_libs: Vec<&str> = Vec::new();
            let mut bdpi_paths: Vec<&str> = Vec::new();
            let mut interactive = false;
            // -dump-formats plumbing from bsc: which waveform writers
            // the artifact carries (reference default: vcd only)
            let mut fmt_arg = "vcd".to_string();
            // top-level bindings (+NAME=value / --bind NAME=value):
            // link has no plusarg namespace, so every `+` here is a
            // binding and an unknown name is a loud error at load
            let mut binds: Vec<trs_interp::TopBind> = Vec::new();
            let mut it = rest.iter();
            while let Some(a) = it.next() {
                match *a {
                    "-o" => out = it.next().map(|s| s.to_string()),
                    "--bind" => match it.next() {
                        Some(v) => match trs_interp::parse_bind(v, true) {
                            Ok(b) => binds.push(b),
                            Err(e) => {
                                eprintln!("trs link: {e}");
                                return ExitCode::from(2);
                            }
                        },
                        None => {
                            eprintln!("Error: --bind requires NAME=value");
                            return ExitCode::from(2);
                        }
                    },
                    p if p.starts_with('+') => match trs_interp::parse_bind(&p[1..], true) {
                        Ok(b) => binds.push(b),
                        Err(e) => {
                            eprintln!("trs link: {e}");
                            return ExitCode::from(2);
                        }
                    },
                    "--multi-fragments" => {}
                    "--bdpi" | "-l" | "-L" => {
                        let Some(v) = it.next() else {
                            eprintln!("Error: {a} requires a value");
                            return ExitCode::from(2);
                        };
                        match *a {
                            "--bdpi" => bdpi.push(v),
                            "-l" => bdpi_libs.push(v),
                            _ => bdpi_paths.push(v),
                        }
                    }
                    "--interactive" => interactive = true,
                    "--dump-formats" => {
                        let Some(v) = it.next() else {
                            eprintln!("Error: --dump-formats requires a value");
                            return ExitCode::from(2);
                        };
                        for tok in v.split(',').filter(|t| !t.is_empty()) {
                            if !matches!(tok, "none" | "vcd" | "fst") {
                                eprintln!(
                                    "trs link: unsupported dump format \
                                     `{tok}' (supported: vcd, fst, none)"
                                );
                                return ExitCode::FAILURE;
                            }
                        }
                        fmt_arg = v.to_string();
                    }
                    _ if link_knob_env(a).is_some() => {
                        let key = link_knob_env(a).unwrap();
                        match it.next() {
                            Some(v) => std::env::set_var(key, v),
                            None => {
                                eprintln!("Error: {a} requires a value");
                                return ExitCode::from(2);
                            }
                        }
                    }
                    _ if compile_knob_env(a).is_some() || compile_knob_switch(a).is_some() => {
                        eprintln!(
                            "trs link: `{a}' steers codegen, which is `trs \
                             compile''s work -- pass it there"
                        );
                        return ExitCode::from(2);
                    }
                    other if !other.starts_with('-') => frags.push(other),
                    other => {
                        eprintln!("Error: invalid link option '{other}'");
                        return ExitCode::from(2);
                    }
                }
            }
            if !multi && frags.len() != 1 {
                eprintln!(
                    "Error: link takes one .bir (or several with \
                     --multi-fragments)"
                );
                return ExitCode::from(2);
            }
            // the top's fragment is named last, so it is the one the
            // artifact is named after and the one a .bdpi.so sits
            // beside -- the same role the single whole-design .bir
            // plays
            let path = *frags.last().expect("at least one positional");
            let base = out
                .unwrap_or_else(|| format!("{}.cexe", path.strip_suffix(".bir").unwrap_or(path)));
            // a .mem is an input to the simulation, not to the build:
            // the reference reads a load file when the model object is
            // constructed, so the artifact written here opens its own
            // when it runs (see prim::LOAD_MEMFILES)
            trs_interp::prim::set_load_memfiles(false);
            // BVI imports: link is a BUILD step -- verilate-or-cache
            // the models before load, so every source/toolchain/
            // refusal error fires at link and the run side (load-only
            // since v1.5) finds finished artifacts.  Forwarded-
            // parameter classes build during the load below (the
            // elaboration resolves them in parent context).  The cache
            // default resolves beside the OUTPUT artifact (`base`),
            // which is where the wrapper and its .bir copy look at run.
            ensure_vlt_env(&base, true);
            if let Err(e) = bvi_prebuild(&frags, multi) {
                eprintln!("trs link: {e}");
                return ExitCode::FAILURE;
            }
            // drop any stale RunCore sidecar BEFORE the new .so is
            // emitted: a link that dies between the two writes must
            // leave "no sidecar" (classic boot), never a new .so
            // beside an old descriptor (adversarial-panel finding)
            let _ = std::fs::remove_file(format!("{base}.arena"));
            // _fresh: link WRITES the snapshot, so it decodes the .bir
            // source of truth, never a prior sidecar (see startup.rs)
            // every load this link does -- the one below and the
            // RunCore bake's -- has to see the same design it was
            // given, so they all go through here.  A fragment set has
            // no snapshot to prefer: the sidecar is keyed by one
            // file's fingerprint.
            let load = |binds: &[trs_interp::TopBind], fresh: bool| match (multi, fresh) {
                (true, _) => trs_interp::startup::load_fragments_fresh(&frags, &[], binds, None),
                (false, true) => trs_interp::startup::load_file_fresh(path, &[], binds, None),
                (false, false) => trs_interp::startup::load_file(path, &[], binds, None),
            };
            let mut interp = match load(&binds, true) {
                Ok(i) => i,
                Err(e) => {
                    eprintln!("trs link: {e}");
                    return ExitCode::FAILURE;
                }
            };
            // the interactive bk_* surface can neither supply bindings
            // nor auto-fire always_enabled methods, so it refuses such
            // designs (v1).  The --exe PIE has the same limit and says
            // so from `trs compile', which is where it is built now.
            if interactive && (!binds.is_empty() || interp.has_autofire()) {
                eprintln!(
                    "trs link: --interactive does not support designs \
                     with top-level bindings or always_enabled top \
                     methods (batch artifacts only)"
                );
                return ExitCode::FAILURE;
            }
            // What the artifact carries is the design that was linked,
            // not the file the link was pointed at -- those differ
            // whenever the design arrived as fragments, whether named
            // with --multi-fragments or found beside the one given.
            // The sidecar, the interactive shim's baked path and the
            // bake's reload all name it from here on.
            let bir_dst = format!("{base}.bir");
            // A .so left by an earlier link of this name belongs to
            // whatever design that was.  If the new .bir is the same
            // bytes it is still good -- and it may have cost hours --
            // so it stays; otherwise it can never be loaded again and
            // leaving it only invites a stale-artifact note on every
            // run.  Compared before the write, while the old .bir is
            // still there to compare against.
            let fresh = interp.encoded_bir();
            let so_dst = format!("{base}.so");
            if std::path::Path::new(&so_dst).exists() {
                let same = std::fs::read(&bir_dst).ok().is_some_and(|old| old == fresh);
                if !same {
                    let _ = std::fs::remove_file(&so_dst);
                }
            }
            if let Err(e) = interp.write_bir(&bir_dst, &fresh) {
                eprintln!("trs link: {e}");
                return ExitCode::FAILURE;
            }
            // `path` still names the file the link was pointed at: a
            // BDPI companion sits beside THAT, not beside the design
            // written out here.
            let design_bir = bir_dst.as_str();
            // The companion the runtime dlopens, built before anything
            // in the design runs: priming executes early cycles, and
            // those can call an import.
            if !bdpi.is_empty() || !bdpi_libs.is_empty() {
                let so = format!("{base}.bdpi.so");
                if let Err(e) = build_bdpi(
                    &bdpi,
                    &bdpi_libs,
                    &bdpi_paths,
                    &interp.foreign_c_names(),
                    &so,
                ) {
                    eprintln!("trs link: {e}");
                    return ExitCode::FAILURE;
                }
                if let Err(e) = interp.load_bdpi(&so) {
                    eprintln!("trs link: {e}");
                    return ExitCode::FAILURE;
                }
            }
            let fmt_vcd = fmt_arg.split(',').any(|t| t == "vcd");
            let fmt_fst = fmt_arg.split(',').any(|t| t == "fst");
            // `none` turns recording off: the artifact is the pure
            // untraced fast model (and the trace salt follows)
            interp.set_allowed_wave_formats(fmt_vcd, fmt_fst);
            if interactive {
                // DEBUG/interactive product: a bluetcl-loadable model
                // .so (docs/TCL-CAPI.md) + the reference's bluesim.tcl
                // wrapper — a different artifact from the fast one.
                // The fast-artifact design .so ships BESIDE the model
                // as <base>.aot.so: the capi's aot engine loads it
                // (warm bodies from t=0); designs the compiler cannot
                // take stay interp/jit with a note.
                //
                // This one still compiles at link time, unlike the
                // fast artifact.  The capi builds its own interp from
                // the .bir and stamps the companion against THAT, and
                // a `trs compile' run does not reproduce the stamp --
                // the identity depends on how the capi constructs its
                // engine, not on the design alone.  Deferring this the
                // way the fast artifact defers is a separate piece of
                // work; until then the debug tier keeps its companion.
                //
                // An earlier link's companion is stale the moment this
                // one runs, and an ineligible design writes none: drop
                // it so the capi finds nothing rather than something
                // that will be refused.  <base>.so is not touched --
                // for this product that name is the MODEL, which
                // link_interactive writes below.
                let _ = std::fs::remove_file(format!("{base}.aot.so"));
                interp.aot_request_emit(format!("{base}.aot.so").into());
                interp.prime();
                match interp.aot_take_emit_result() {
                    // Manifest cannot arise here: only `trs specializations`
                    // sets the mode, and it does not take this path
                    Some(trs_interp::AotEmit::Compiled)
                    | Some(trs_interp::AotEmit::Manifest)
                    | None => {}
                    Some(trs_interp::AotEmit::Failed(e)) => {
                        eprintln!("trs link --interactive: {e}");
                        return ExitCode::FAILURE;
                    }
                    Some(trs_interp::AotEmit::Ineligible(e)) => eprintln!(
                        "trs link --interactive: note: the aot tier is \
                         unavailable for this design ({e}); its engines \
                         run interp/jit"
                    ),
                }
                return link_interactive(design_bir, &base, interp.top_name(), &fmt_arg);
            }
            // No codegen here at all: a link assembles the design and
            // writes something that runs it interpreted.  Every
            // compiled product -- the .so, its sidecars, and the
            // standalone executable -- is `trs compile''s, and the
            // artifact picks the .so up on whichever run comes after
            // it exists.
            interp.prime();
            // the .bir sibling the script runs is already written
            // above: it is the design, not whichever of its files the
            // link was pointed at
            // decoded-design snapshot: run startup skips the CBOR parse
            // when its fingerprint gate matches (a cache, never a source
            // of truth; stale/missing -> normal decode)
            if let Err(e) = interp.write_snapshot(&format!("{base}.birsnap")) {
                eprintln!("trs link: note: snapshot not written ({e})");
            }
            // user BDPI code travels with the artifact: load_file looks
            // for <base>.bdpi.so next to the (renamed) .bir
            let bdpi_src = format!("{}.bdpi.so", path.strip_suffix(".bir").unwrap_or(path));
            let bdpi_dst = format!("{base}.bdpi.so");
            if std::path::Path::new(&bdpi_src).exists()
                && std::path::Path::new(&bdpi_src).canonicalize().ok()
                    != std::path::Path::new(&bdpi_dst).canonicalize().ok()
            {
                if let Err(e) = std::fs::copy(&bdpi_src, &bdpi_dst) {
                    eprintln!("trs link: copy {bdpi_src} -> {bdpi_dst}: {e}");
                    return ExitCode::FAILURE;
                }
            }
            // wrapper script (trs must be on PATH, like bluetcl for
            // reference Bluesim executables)
            let split = std::env::var("TRS_JIT_SPLIT").unwrap_or_default();
            let split_arg = if split.is_empty() {
                String::new()
            } else {
                format!(" --split {split}")
            };
            // baked bindings ride in the wrapper (and .opts, for the
            // symlink dispatch): every run of the artifact re-supplies
            // them, and a conflicting user +NAME=value errors at load
            let bind_args: String = binds
                .iter()
                .map(|b| format!(" --bind \"{}={}\"", b.name, b.value))
                .collect();
            // honor $TRS like bsc's interp wrapper (the testsuite
            // points it at a specific build); the DEFAULT is the
            // absolute path of the binary that linked the artifact —
            // a bare `trs` PATH lookup silently picked up stale
            // installs (caught by the perf fence: every artifact ran
            // interpreted under an old inst/bin binary)
            let self_exe = std::env::current_exe()
                .ok()
                .and_then(|p| p.to_str().map(String::from))
                .unwrap_or_else(|| "trs".into());
            // slim runner: an LLVM-free `trs-run` installed beside
            // this binary execs ~6ms cheaper (no static-LLVM
            // constructors/relocations; Dividers exe 14.2 -> 7.9ms).
            // Baked as the wrapper's default runner when present.
            // Selfcheck runs (--selfcheck, or TRS_SELFCHECK/TRS_JIT
            // in the env) route back to the FULL binary: the slim
            // build's arm_jit is a no-op, which would silently label
            // a second interp shadow "jit" and weaken the 3-way
            // oracle.  TRS= in the env still overrides both.
            let slim_exe = std::env::current_exe()
                .ok()
                .map(|p| p.with_file_name("trs-run"))
                .filter(|p| p.is_file())
                .and_then(|p| p.to_str().map(String::from));
            // debug/script tier: -c/-f are Tcl (while/foreach/expr —
            // bluesim.tcl's `source`/`eval`), so those runs go through
            // stock bluetcl + the capi shim, not the fast runner.  The
            // capi's own default engine applies (traced-plan jit:
            // fast `sim run` AND slot-recorded def/port peeks);
            // TRS_CAPI_ENGINES overrides, and the -dump-formats
            // contract travels via TRS_CAPI_FORMATS.
            let top = interp.top_name().to_string();
            // the compile links its own aot companion when it makes one
            let capi = write_capi_shim(path, &base, &top, false);
            let dispatch = if capi {
                format!(
                    "for arg in ${{1+\"$@\"}}\n\
                     do\n\
                     \x20 case \"$arg\" in\n\
                     \x20 -c|-f)\n\
                     \x20   if test -f \"$d/$b.capi.so\"; then\n\
                     \x20     TRS_CAPI_FORMATS=\"{fmt_arg}\"; export TRS_CAPI_FORMATS\n\
                     \x20     TRS_VLT_CACHE=\"${{TRS_VLT_CACHE:-$d/trs-vlt}}\"; export TRS_VLT_CACHE\n\
                     \x20     BLUESPECDIR=`echo 'puts $env(BLUESPECDIR)' | bluetcl`\n\
                     \x20     exec $BLUESPECDIR/tcllib/bluespec/bluesim.tcl \"$d/$b.capi.so\" {top} --script_name \"$b\" ${{1+\"$@\"}}\n\
                     \x20   fi\n\
                     \x20   ;;\n\
                     \x20 esac\n\
                     done\n"
                )
            } else {
                String::new()
            };
            // Always the full binary: a link leaves no .so, and the
            // slim runner cannot JIT in-process.  `trs compile'
            // re-points this at the slim runner once the compiled
            // design exists.
            let script = if false {
                let pick = match &slim_exe {
                    Some(slim) => format!(
                        "r=\"{slim}\"\n\
                         case \" $* \" in *\" --selfcheck\"*) r=\"{self_exe}\";; esac\n\
                         if [ -n \"${{TRS_SELFCHECK}}${{TRS_JIT}}\" ]; then r=\"{self_exe}\"; fi\n"
                    ),
                    None => format!("r=\"{self_exe}\"\n"),
                };
                format!(
                    "#!/bin/sh\nd=`dirname \"$0\"`\nb=`basename \"$0\"`\n{pick}{dispatch}\
                     exec \"${{TRS:-$r}}\" run \"$d/$b.bir\" --code \"$d/$b.so\"{split_arg} --formats {fmt_arg}{bind_args} ${{1+\"$@\"}}\n"
                )
            } else {
                format!(
                    "#!/bin/sh\nd=`dirname \"$0\"`\nb=`basename \"$0\"`\n{dispatch}\
                     exec \"${{TRS:-{self_exe}}}\" run \"$d/$b.bir\" --formats {fmt_arg}{bind_args} ${{1+\"$@\"}}\n"
                )
            };
            // temp+rename: a crash mid-write must never leave a
            // truncated-but-executable wrapper (a script missing its
            // exec line runs and exits 0 doing nothing)
            // baked link options for the argv[0] dispatch (one ~60-byte
            // read replaces the wrapper's two command-substitution
            // forks); written for both artifact forms
            let mut opts = format!("top={top}\nformats={fmt_arg}\nsplit={split}\n");
            for b in &binds {
                opts.push_str(&format!("bind={}={}\n", b.name, b.value));
            }
            let opts_tmp = format!("{base}.opts.tmp");
            if let Err(e) = std::fs::write(&opts_tmp, opts)
                .and_then(|()| std::fs::rename(&opts_tmp, format!("{base}.opts")))
            {
                eprintln!("trs link: {base}.opts: {e}");
                return ExitCode::FAILURE;
            }
            // the artifact itself: a SYMLINK to the runner — main()'s
            // argv[0] dispatch recovers <base>.bir/.so/.opts from the
            // link NAME and runs IN-PROCESS (no sh, no forks, no
            // second exec; the wrapper cost ~5ms per invocation).
            // Compiled artifacts point at the slim runner; the
            // non-compiled form keeps the full binary (TRS_JIT=1
            // hybrid runs need it).  The sh script remains the
            // fallback where symlinks fail — and for output names
            // that would defeat the dispatch's own-name guard.
            #[cfg(unix)]
            let linked = {
                let bname = std::path::Path::new(&base)
                    .file_name()
                    .and_then(|n| n.to_str())
                    .unwrap_or("");
                let runner = self_exe.clone();
                bname != "trs" && bname != "trs-run" && {
                    let tmp = format!("{base}.lnk.tmp");
                    let _ = std::fs::remove_file(&tmp);
                    std::os::unix::fs::symlink(&runner, &tmp)
                        .and_then(|()| std::fs::rename(&tmp, &base))
                        .is_ok()
                }
            };
            #[cfg(not(unix))]
            let linked = false;
            if !linked {
                let base_tmp = format!("{base}.tmp");
                if let Err(e) = std::fs::write(&base_tmp, script)
                    .and_then(|()| std::fs::rename(&base_tmp, &base))
                {
                    eprintln!("trs link: {base}: {e}");
                    return ExitCode::FAILURE;
                }
                #[cfg(unix)]
                {
                    use std::os::unix::fs::PermissionsExt;
                    let _ = std::fs::set_permissions(&base, std::fs::Permissions::from_mode(0o755));
                }
            }
            ExitCode::SUCCESS
        }
        // trs capi-so: build the ONE shared libtrs_capi.so the fast
        // link's per-design shims link against (install it beside the
        // trs binary).  The 4s / 50MB capi+engine link happens once
        // here instead of once per design (docs/TCL-CAPI.md).
        ["capi-so", rest @ ..] => {
            let mut out: Option<String> = None;
            // --rt: build the slim artifact RUNTIME (libtrs_rt.so) from
            // the LLVM-free libtrs_rt.a instead — what `trs link --exe`
            // binaries load (the full capi lib carries statically-linked
            // LLVM whose constructors cost ~5ms at every exec)
            let mut rt = false;
            let mut it = rest.iter();
            while let Some(a) = it.next() {
                match *a {
                    "-o" => match it.next() {
                        Some(v) => out = Some(v.to_string()),
                        None => {
                            eprintln!("Error: -o requires a value");
                            return ExitCode::from(2);
                        }
                    },
                    "--rt" => rt = true,
                    "--capi-lib" => match it.next() {
                        Some(v) => std::env::set_var("TRS_CAPI_LIB", v),
                        None => {
                            eprintln!("Error: --capi-lib requires a value");
                            return ExitCode::from(2);
                        }
                    },
                    other => {
                        eprintln!("Error: invalid capi-so option '{other}'");
                        return ExitCode::from(2);
                    }
                }
            }
            let out = out
                .unwrap_or_else(|| if rt { "libtrs_rt.so" } else { "libtrs_capi.so" }.to_string());
            let Some(lib) = find_staticlib(if rt {
                ("TRS_RT_LIB", "libtrs_rt.a")
            } else {
                ("TRS_CAPI_LIB", "libtrs_capi.a")
            }) else {
                eprintln!(
                    "trs capi-so: {} not found (set {} or install it \
                     next to the trs binary)",
                    if rt { "libtrs_rt.a" } else { "libtrs_capi.a" },
                    if rt { "TRS_RT_LIB" } else { "TRS_CAPI_LIB" },
                );
                return ExitCode::FAILURE;
            };
            let tmp = std::env::temp_dir().join(format!("trs-capi-so-{}", std::process::id()));
            if let Err(e) = std::fs::create_dir_all(&tmp) {
                eprintln!("trs capi-so: {}: {e}", tmp.display());
                return ExitCode::FAILURE;
            }
            let map = tmp.join("export.map");
            // no new_MODEL_* here: the per-design shims provide those
            if let Err(e) = std::fs::write(&map, "{ global: bk_*; trs_*; local: *; };\n") {
                eprintln!("trs capi-so: write {}: {e}", map.display());
                return ExitCode::FAILURE;
            }
            let r = capi_cc_link(
                &out,
                &[],
                &lib,
                &map,
                &["bk_*", "trs_*", "new_MODEL_*"],
                !rt,
            );
            let _ = std::fs::remove_dir_all(&tmp);
            match r {
                Ok(()) => {
                    println!("trs capi-so: shared capi written: {out}");
                    ExitCode::SUCCESS
                }
                Err(e) => {
                    eprintln!("trs capi-so: {e}");
                    ExitCode::FAILURE
                }
            }
        }
        ["run", path, rest @ ..] => {
            // mirror the bluesim.tcl driver's argument handling: -m N is
            // the cycle limit, +foo registers a plusarg (sans '+'),
            // anything else is an error
            let mut max_cycles = u64::MAX;
            let mut plusargs: Vec<String> = Vec::new();
            // top-level bindings: --bind NAME=value is explicit (an
            // unknown name errors); +NAME=value is opportunistic — it
            // binds when NAME is a top-level argument and stays an
            // ordinary plusarg otherwise (existing designs unchanged)
            let mut binds: Vec<trs_interp::TopBind> = Vec::new();
            let mut wave: Option<(trs_interp::WaveFormat, Option<String>)> = None;
            let mut vcd_file: Option<String> = None;
            let mut code_so: Option<String> = None;
            let mut only_compiled = false;
            // (vcd, fst) writers this model carries; None = the
            // reference default (vcd only) applied at load
            let mut formats: Option<(bool, bool)> = None;
            // Some((N, announce)) = lockstep selfcheck, compare
            // cadence N posedges.  TRS_SELFCHECK=1 arms it
            // environmentally — existing artifact wrappers then run
            // checked with no relink (how the corpus sweep and the
            // DejaGnu suite drive it); env-armed runs suppress the
            // skip notes (announce=false) because byte-compare
            // harnesses capture stderr.
            let mut selfcheck: Option<(u64, bool)> = std::env::var_os("TRS_SELFCHECK").map(|_| {
                (
                    std::env::var("TRS_SELFCHECK_EVERY")
                        .ok()
                        .and_then(|v| v.parse().ok())
                        .unwrap_or(1000),
                    false,
                )
            });
            let mut script_cmds = String::new();
            // bluesim.tcl's usage text, printed for -h and after the
            // deprecated-flag notices; the driver exits 0 in both cases
            let script = path.strip_suffix(".bir").unwrap_or(path).to_string();
            let usage_exit = || -> ExitCode {
                println!("Usage: {script} [opts]");
                println!();
                println!("Options:");
                println!("  -c <commands> = execute commands given as an argument");
                println!("  -f <file>     = execute script from file");
                println!("  -h            = print help and exit");
                println!("  -m <N>        = execute for N cycles");
                println!("  -v            = print version information and exit");
                println!("  -V [<file>]   = dump waveforms to VCD file (default: dump.vcd)");
                println!("  +<arg>        = Verilog-style plus-arg");
                println!();
                println!("Examples:");
                println!("  {script}");
                println!("  {script} -m 3000");
                println!("  {script} -V sim.vcd");
                println!("  {script} +doFoo");
                ExitCode::SUCCESS
            };
            let mut it = rest.iter().peekable();
            while let Some(a) = it.next() {
                match *a {
                    // deprecated interactive-debug flags: notice + usage,
                    // exit 0 (matching bluesim.tcl)
                    f @ ("-s" | "-ss" | "-r" | "-cc") => {
                        println!("Error: {f} is deprecated in favor of scriptable debug");
                        println!("See entry #031 in the KPnS document.");
                        return usage_exit();
                    }
                    "-h" | "-help" | "--help" => return usage_exit(),
                    "-v" => {
                        println!("trs {} (TRS runtime)", env!("CARGO_PKG_VERSION"));
                        return ExitCode::SUCCESS;
                    }
                    "--script_name" => {
                        let _ = it.next();
                    }
                    "--code" => {
                        code_so = it.next().map(|s| s.to_string());
                    }
                    // strict execution: byte parity cannot tell the
                    // engines apart, so a run that must be compiled
                    // has to say so rather than silently degrade
                    "--only-compiled" => only_compiled = true,
                    "--bind" => match it.next() {
                        Some(v) => match trs_interp::parse_bind(v, true) {
                            Ok(b) => binds.push(b),
                            Err(e) => {
                                eprintln!("trs: {e}");
                                return ExitCode::from(2);
                            }
                        },
                        None => {
                            eprintln!("Error: --bind requires NAME=value");
                            return ExitCode::from(2);
                        }
                    },
                    // lockstep selfcheck: a quiet interp shadow runs
                    // beside the primary engine; state compared every
                    // N default-clock posedges (default 1000, or
                    // --selfcheck-every / TRS_SELFCHECK_EVERY)
                    "--selfcheck" => {
                        selfcheck = Some((
                            selfcheck.map(|(n, _)| n).unwrap_or_else(|| {
                                std::env::var("TRS_SELFCHECK_EVERY")
                                    .ok()
                                    .and_then(|v| v.parse().ok())
                                    .unwrap_or(1000)
                            }),
                            true,
                        ));
                    }
                    "--selfcheck-every" => match it.next() {
                        Some(n) => match n.parse::<u64>() {
                            Ok(n) => selfcheck = Some((n, true)),
                            Err(_) => {
                                eprintln!("Error: --selfcheck-every requires a number");
                                return ExitCode::from(2);
                            }
                        },
                        None => {
                            eprintln!("Error: --selfcheck-every requires a number");
                            return ExitCode::from(2);
                        }
                    },
                    // -dump-formats baked into the artifact wrapper
                    "--formats" => {
                        if let Some(v) = it.next() {
                            formats = Some((
                                v.split(',').any(|t| t == "vcd"),
                                v.split(',').any(|t| t == "fst"),
                            ));
                        }
                    }
                    // artifacts pin their split threshold (arena layout)
                    "--split" => {
                        if let Some(n) = it.next() {
                            std::env::set_var("TRS_JIT_SPLIT", n);
                        }
                    }
                    "--creation_time" => {
                        let _ = it.next();
                    }
                    // -c/-f collect script commands (bluesim.tcl:94-124);
                    // a later deprecated flag still wins (exit 0 above)
                    "-c" => match it.next() {
                        Some(cmds) => {
                            script_cmds.push_str(cmds);
                            script_cmds.push('\n');
                        }
                        None => {
                            println!("Error: -c requires a command argument");
                            return usage_exit();
                        }
                    },
                    "-f" => match it.next() {
                        Some(f) => match std::fs::read_to_string(f) {
                            Ok(s) => {
                                script_cmds.push_str(&s);
                                script_cmds.push('\n');
                            }
                            Err(e) => {
                                eprintln!("trs: {f}: {e}");
                                return ExitCode::from(2);
                            }
                        },
                        None => {
                            println!("Error: -f requires a script filename argument");
                            return usage_exit();
                        }
                    },
                    "-m" => {
                        max_cycles = it
                            .next()
                            .and_then(|n| n.parse::<u64>().ok())
                            .unwrap_or(u64::MAX);
                    }
                    // -V [file]: dump waveforms (default dump.vcd)
                    "-V" => {
                        let takes_arg = it
                            .peek()
                            .map(|n| !n.starts_with('-') && !n.starts_with('+'))
                            .unwrap_or(false);
                        vcd_file = Some(if takes_arg {
                            it.next().unwrap().to_string()
                        } else {
                            "dump.vcd".to_string()
                        });
                    }
                    p if p.starts_with('+') => {
                        // +bscvcd / +bscfst select waveform dumping
                        // like bluesim.tcl (and stay design-visible
                        // plusargs, as in Verilog); a named file
                        // rides after '='
                        if p == "+bscvcd" {
                            wave = Some((trs_interp::WaveFormat::Vcd, None));
                        } else if let Some(f) = p.strip_prefix("+bscvcd=") {
                            wave = Some((
                                trs_interp::WaveFormat::Vcd,
                                (!f.is_empty()).then(|| f.to_string()),
                            ));
                        } else if p == "+bscfst" {
                            wave = Some((trs_interp::WaveFormat::Fst, None));
                        } else if let Some(f) = p.strip_prefix("+bscfst=") {
                            wave = Some((
                                trs_interp::WaveFormat::Fst,
                                (!f.is_empty()).then(|| f.to_string()),
                            ));
                        }
                        // NAME=value is also a top-level binding
                        // candidate; the loader consumes it (and drops
                        // it from the plusargs) iff NAME is a top
                        // argument of this design
                        if let Ok(b) = trs_interp::parse_bind(&p[1..], false) {
                            binds.push(b);
                        }
                        plusargs.push(p[1..].to_string());
                    }
                    other => {
                        eprintln!("Error: invalid option '{other}'");
                        return ExitCode::from(2);
                    }
                }
            }
            if !script_cmds.is_empty() {
                if matches!(selfcheck, Some((_, true))) {
                    // the bluetcl tier's equivalent is the multi-engine
                    // oracle (TRS_CAPI_ENGINES=interp,jit — see
                    // docs/SELFCHECK.md); the batch lockstep driver
                    // does not apply to script runs
                    eprintln!(
                        "trs: note: --selfcheck applies to batch runs; \
                         ignored with -c/-f (use TRS_CAPI_ENGINES for \
                         the script tier's oracle)"
                    );
                }
                return run_script(
                    path,
                    max_cycles,
                    &plusargs,
                    &binds,
                    vcd_file.as_deref(),
                    wave.clone(),
                    code_so.as_deref(),
                    formats,
                    only_compiled,
                    &script_cmds,
                );
            }
            // single-file UX: `trs run design.so` — the artifact
            // carries its design, so the .so IS the runnable unit;
            // the derived .bir name stays only as the fallback path
            // for pre-snap artifacts
            let so_direct;
            let (path, code_so): (&str, Option<String>) =
                if path.ends_with(".so") && code_so.is_none() {
                    so_direct = path.strip_suffix(".so").unwrap().to_string() + ".bir";
                    (so_direct.as_str(), Some(path.to_string()))
                } else {
                    (path as &str, code_so)
                };
            // A run is load-only: resolve the model cache beside the
            // design and clear any inherited build marker, exactly as
            // the scripted path does.  Without it the cache falls back
            // to the RELATIVE "trs-vlt", so a BVI design runs only from
            // its own directory and reports a missing model anywhere
            // else -- and the clean precheck error never gets to fire.
            ensure_vlt_env(path, false);
            // no bvi_precheck here on purpose: it costs a full decode
            // plus link::assemble, which is exactly the CBOR parse the
            // .birsnap fast path below exists to skip, and a missing
            // model already reports itself clearly at instantiation.
            match trs_interp::run_file(
                path,
                max_cycles,
                &plusargs,
                &binds,
                vcd_file.as_deref(),
                wave,
                code_so.as_deref(),
                formats,
                selfcheck,
                only_compiled,
            ) {
                Ok(code) => {
                    use std::io::Write;
                    trs_interp::stdout_flush();
                    let _ = std::io::stderr().flush();
                    // bypass atexit teardown: JIT body workers may still
                    // be inside LLVM and would stall process exit
                    unsafe { libc::_exit(code.clamp(0, 255) as i32) }
                }
                Err(e) => {
                    eprintln!("trs: {e}");
                    ExitCode::FAILURE
                }
            }
        }
        _ => usage(),
    }
}

/// The scripting subset of bluesim.tcl's `sim` command that the testsuite
/// uses outside bsc.bluesim/interactive: `sim run`/`sim step N` (multi-step
/// resumable, on Interp::advance) plus `sim time`/`sim clock` queries and
/// `puts [...]` printing.  The full interactive surface arrives with the
/// bk_* compat .so (task #20); anything beyond this subset errors out
/// loudly.
/// The dlsym'd bk surface (docs/TCL-CAPI.md) — the -u keep list for
/// the interactive .so link.
const BK_EXPORTS: &[&str] = &[
    "bk_init",
    "bk_shutdown",
    "bk_now",
    "bk_set_timescale",
    "bk_version",
    "bk_append_argument",
    "bk_define_clock",
    "bk_num_clocks",
    "bk_get_nth_clock",
    "bk_clock_name",
    "bk_get_clock_by_name",
    "bk_clock_initial_value",
    "bk_clock_first_edge",
    "bk_clock_duration",
    "bk_clock_val",
    "bk_clock_cycle_count",
    "bk_clock_edge_count",
    "bk_clock_last_edge",
    "bk_quit_after_edge",
    "bk_schedule_ui_event",
    "bk_remove_ui_event",
    "bk_set_interactive",
    "bk_advance",
    "bk_is_running",
    "bk_sync",
    "bk_abort_now",
    "bk_finished",
    "bk_exit_status",
    "bk_fataled",
    "bk_top_symbol",
    "bk_lookup_symbol",
    "bk_get_size",
    "bk_get_key",
    "bk_is_module",
    "bk_is_rule",
    "bk_is_single_value",
    "bk_is_value_range",
    "bk_peek_symbol_value",
    "bk_get_range_min_addr",
    "bk_get_range_max_addr",
    "bk_peek_range_value",
    "bk_num_symbols",
    "bk_get_nth_symbol",
    "bk_set_VCD_file",
    "bk_get_VCD_file_name",
    "bk_enable_VCD_dumping",
    "bk_disable_VCD_dumping",
    "bk_set_waveform_format",
];

/// A runtime staticlib (libtrs_capi.a or libtrs_rt.a): env override,
/// then alongside the binary.
fn find_staticlib((env, name): (&str, &str)) -> Option<std::path::PathBuf> {
    std::env::var(env)
        .ok()
        .map(std::path::PathBuf::from)
        .or_else(|| {
            let exe = std::env::current_exe().ok()?;
            let d = exe.parent()?;
            [d.join(name), d.join("../lib").join(name)]
                .into_iter()
                .find(|p| p.exists())
        })
}

/// The shared libtrs_capi.so (built once by `trs capi-so`): env
/// override, then alongside the binary — the fast link's shim tier
/// exists only when this is installed.
fn find_capi_shared() -> Option<std::path::PathBuf> {
    std::env::var("TRS_CAPI_SO")
        .ok()
        .map(std::path::PathBuf::from)
        .or_else(|| {
            let exe = std::env::current_exe().ok()?;
            let d = exe.parent()?;
            [d.join("libtrs_capi.so"), d.join("../lib/libtrs_capi.so")]
                .into_iter()
                .find(|p| p.exists())
        })
}

/// The cc link shared by the fat `--interactive` .so and the
/// once-per-install `trs capi-so` shared library: force-keep exactly
/// the dlsym'd bk surface from the staticlib, dead-strip the rest,
/// and (jit staticlibs — `llvm`) resolve the staticlib's LLVM
/// references against the shared libLLVM.  The slim libtrs_rt.a has
/// no LLVM references: pass llvm=false so it links in LLVM-less
/// environments too.
fn capi_cc_link(
    out: &str,
    inputs: &[&std::path::Path],
    lib: &std::path::Path,
    map: &std::path::Path,
    keep: &[&str],
    llvm: bool,
) -> Result<(), String> {
    // the export list's format is the host's business, not the
    // caller's: a version script and an exported-symbols list say the
    // same thing in different shapes
    let export_flags =
        hostlink::export_list(map, keep).map_err(|e| format!("write {}: {e}", map.display()))?;
    let mut cc = std::process::Command::new(trs_interp::cc_tool());
    cc.arg("-shared").arg("-fPIC").arg("-o").arg(out);
    for i in inputs {
        cc.arg(i);
    }
    // force-keep exactly the exported surface: --whole-archive would
    // drag every llvm-sys binding object (LineEditor -> libedit, ffi
    // stubs) into the .so; -u pulls only what the bk_*/new_MODEL
    // closure actually needs
    for sym in BK_EXPORTS {
        cc.arg(hostlink::undefined(sym));
    }
    cc.arg(lib)
        // rust emits function sections: dead-strip everything the
        // -u keep-list doesn't reach, and drop symbols (166MB -> )
        .args(hostlink::dead_strip())
        .args(hostlink::strip_symbols())
        .args(hostlink::local_binding())
        .args(&export_flags)
        .args(hostlink::system_libs())
        .arg("-lm")
        // vendored libfst (trs-interp build.rs) gzip-frames FST output
        // through zlib in every flavor, slim included
        .arg("-lz")
        // -shared tolerates undefined symbols; RTLD_NOW (and a static
        // exe link against this .so) does not — fail at LINK time
        // instead of at sim load
        .args(hostlink::no_undefined());
    if llvm && cfg!(feature = "jit") {
        // a jit-featured capi staticlib references LLVM (rustc links
        // it into BINARIES only); use the shared libLLVM
        // llvm-config by whichever name reaches it: the distro's
        // versioned one, then the prefix llvm-sys was pointed at, then
        // the Debian default -- a build configured through
        // LLVM_SYS_181_PREFIX has no versioned binary on PATH.
        let ask = |prog: &std::path::Path, args: &[&str]| {
            std::process::Command::new(prog)
                .args(args)
                .output()
                .ok()
                .filter(|o| o.status.success())
                .and_then(|o| String::from_utf8(o.stdout).ok())
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
        };
        // Which LLVM this staticlib was built against is a fact about
        // the build, so the prefix llvm-sys was given is baked in at
        // compile time; the environment at run time belongs to whoever
        // is running trs and need not have it set at all.  A runtime
        // value still wins, for moving an install.
        let prefix = |p: &str| std::path::Path::new(p).join("bin").join("llvm-config");
        let cfg = [
            std::env::var("LLVM_SYS_181_PREFIX")
                .ok()
                .map(|p| prefix(&p)),
            option_env!("LLVM_SYS_181_PREFIX").map(prefix),
            Some(std::path::PathBuf::from("llvm-config-18")),
        ]
        .into_iter()
        .flatten()
        .find(|p| ask(p, &["--libdir"]).is_some());
        let libdir = cfg
            .as_deref()
            .and_then(|p| ask(p, &["--libdir"]))
            .unwrap_or_else(|| "/usr/lib/llvm-18/lib".into());
        // the staticlib's llvm-sys bindings reference LLVM's own system
        // libraries directly, and which ones those are is a property of
        // this LLVM build, not of the platform -- ask it rather than
        // keeping a list that is right on one distribution
        let sys_libs: Vec<String> = cfg
            .as_deref()
            .and_then(|p| ask(p, &["--system-libs", "--link-static"]))
            .map(|s| s.split_whitespace().map(str::to_string).collect())
            .unwrap_or_else(|| {
                hostlink::llvm_support_libs()
                    .iter()
                    .map(|s| s.to_string())
                    .collect()
            });
        cc.arg(format!("-L{libdir}"))
            .arg("-lLLVM-18")
            .arg(hostlink::cxx_runtime())
            .args(&sys_libs)
            // the execution engine bindings reference libffi, which
            // llvm-config does not report
            .arg("-lffi")
            .args(hostlink::lib_search_paths());
    }
    match cc.status() {
        Ok(s) if s.success() => Ok(()),
        Ok(s) => Err(format!("{} exited {s}", trs_interp::cc_tool())),
        Err(e) => Err(format!("{}: {e}", trs_interp::cc_tool())),
    }
}

/// Write the model shim sources into `tmp`: bir.s (the design's BIR
/// embedded via incbin) and shim.c (the Model struct + the
/// `new_MODEL_<top>` constructor BluesimLoader.hs dlsym's).  Returns
/// (bir.s, shim.c).
fn write_shim_sources(
    tmp: &std::path::Path,
    bir_abs: &std::path::Path,
    top: &str,
) -> Result<(std::path::PathBuf, std::path::PathBuf), String> {
    let shim_s = tmp.join("bir.s");
    let shim_c = tmp.join("shim.c");
    std::fs::write(
        &shim_s,
        hostlink::incbin_stub(bir_abs, "trs_bir_start", "trs_bir_end"),
    )
    .map_err(|e| format!("write {}: {e}", shim_s.display()))?;
    std::fs::write(
        &shim_c,
        format!(
            r##"/* generated by trs link */
typedef struct {{
    const unsigned char* bir_ptr;
    unsigned long        bir_len;
    const char*          top;
}} Model;
extern const unsigned char trs_bir_start[], trs_bir_end[];
static Model M;
void* new_MODEL_{top}(void) {{
    M.bir_ptr = trs_bir_start;
    M.bir_len = (unsigned long)(trs_bir_end - trs_bir_start);
    M.top = "{top}";
    return &M;
}}
"##
        ),
    )
    .map_err(|e| format!("write {}: {e}", shim_c.display()))?;
    Ok((shim_s, shim_c))
}

/// Fast-link debug tier: emit `<base>.capi.so`, a tiny bluetcl-loadable
/// shim (embedded BIR + `new_MODEL_<top>`) with a DT_NEEDED on the
/// shared libtrs_capi.so installed beside the trs binary — dlsym on the
/// shim's handle resolves the bk_* surface through the dependency
/// scope.  Companions follow bk_init's dladdr lookup (model base =
/// `<base>.capi`): `<base>.capi.bdpi.so` and `<base>.capi.aot.so` are
/// same-directory symlinks onto the fast artifact's files.  Returns
/// false when the shared lib is absent or the link fails — the fast
/// artifact stays fully usable, only the -c/-f script tier is missing.
fn write_capi_shim(bir_path: &str, base: &str, top: &str, compiled: bool) -> bool {
    let note = |m: String| {
        eprintln!("trs link: note: {m}; -c/-f scripting will not be available");
        false
    };
    let Some(shared) = find_capi_shared() else {
        // Every other way this can fail says so, and so does this one:
        // the artifact contract is unchanged either way, but an
        // artifact quietly missing a documented tier is indisting-
        // uishable from one that has it.  A lean install reaches here
        // on purpose; so does a host where `trs capi-so` could not
        // link, and only the message tells them apart.
        return note(
            "no libtrs_capi.so (looked at $TRS_CAPI_SO, then beside \
             the trs binary and in ../lib)"
                .to_string(),
        );
    };
    let Some(shared_dir) = shared.parent().map(std::path::Path::to_path_buf) else {
        return note(format!("{} has no parent directory", shared.display()));
    };
    let Ok(shared_dir) = shared_dir.canonicalize() else {
        return note(format!("cannot resolve {}", shared_dir.display()));
    };
    let Ok(bir_abs) = std::path::Path::new(bir_path).canonicalize() else {
        return note(format!("cannot resolve {bir_path}"));
    };
    let tmp = std::env::temp_dir().join(format!("trs-capi-shim-{}", std::process::id()));
    if let Err(e) = std::fs::create_dir_all(&tmp) {
        return note(format!("{}: {e}", tmp.display()));
    }
    let sources = write_shim_sources(&tmp, &bir_abs, top);
    let (shim_s, shim_c) = match sources {
        Ok(p) => p,
        Err(e) => return note(e),
    };
    let map = tmp.join("export.map");
    let shim_exports = match hostlink::export_list(&map, &["new_MODEL_*", "trs_*"]) {
        Ok(f) => f,
        Err(e) => return note(format!("write {}: {e}", map.display())),
    };
    let so = format!("{base}.capi.so");
    let mut cc = std::process::Command::new(trs_interp::cc_tool());
    cc.arg("-shared")
        .arg("-fPIC")
        .arg("-o")
        .arg(&so)
        .arg(&shim_c)
        .arg(&shim_s)
        .arg(format!("-L{}", shared_dir.display()))
        // -l: form pins DT_NEEDED to the plain soname; the rpath
        // resolves it at load (an artifact moved to another machine
        // falls back to that machine's installed capi).  The shim
        // itself references NO capi symbol — bluetcl dlsym's bk_*
        // through the dependency scope — so --as-needed (many distros'
        // default) would silently drop the DT_NEEDED: disable it.
        .args(hostlink::no_as_needed())
        .arg(hostlink::link_shared(&shared, "libtrs_capi.so"))
        .arg(format!("-Wl,-rpath,{}", shared_dir.display()))
        .args(&shim_exports)
        .args(hostlink::no_undefined());
    let r = cc.status();
    let _ = std::fs::remove_dir_all(&tmp);
    match r {
        Ok(s) if s.success() => {}
        Ok(s) => return note(format!("{} exited {s}", trs_interp::cc_tool())),
        Err(e) => return note(format!("{}: {e}", trs_interp::cc_tool())),
    }
    // companions: same-directory RELATIVE symlinks (they survive the
    // whole artifact directory moving together); a filesystem without
    // symlinks gets copies
    let file = std::path::Path::new(base)
        .file_name()
        .map(|f| f.to_string_lossy().into_owned())
        .unwrap_or_else(|| base.to_string());
    let link_beside = |link: &str, target_file: &str| {
        let _ = std::fs::remove_file(link);
        #[cfg(unix)]
        if std::os::unix::fs::symlink(target_file, link).is_ok() {
            return;
        }
        let target = std::path::Path::new(base)
            .parent()
            .map(|d| d.join(target_file))
            .unwrap_or_else(|| std::path::PathBuf::from(target_file));
        let _ = std::fs::copy(target, link);
    };
    if compiled {
        link_beside(&format!("{base}.capi.aot.so"), &format!("{file}.so"));
    }
    if std::path::Path::new(&format!("{base}.bdpi.so")).exists() {
        link_beside(&format!("{base}.capi.bdpi.so"), &format!("{file}.bdpi.so"));
    }
    true
}

/// `trs link --interactive`: produce <base>.so (the bk_* capi model
/// with the BIR embedded via incbin) and <base>, the same bluesim.tcl
/// wrapper the reference emits — `sim load`-able by stock bluetcl and
/// runnable by the interactive testsuite unchanged.
fn link_interactive(bir_path: &str, base: &str, top: &str, formats: &str) -> ExitCode {
    let fail = |m: String| {
        eprintln!("trs link --interactive: {m}");
        ExitCode::FAILURE
    };
    let Some(lib) = find_staticlib(("TRS_CAPI_LIB", "libtrs_capi.a")) else {
        return fail(
            "libtrs_capi.a not found (set TRS_CAPI_LIB or install it              next to the trs binary)"
                .into(),
        );
    };
    let Ok(bir_abs) = std::path::Path::new(bir_path).canonicalize() else {
        return fail(format!("cannot resolve {bir_path}"));
    };
    let tmp = std::env::temp_dir().join(format!("trs-capi-{}", std::process::id()));
    if let Err(e) = std::fs::create_dir_all(&tmp) {
        return fail(format!("{}: {e}", tmp.display()));
    }
    let (shim_s, shim_c) = match write_shim_sources(&tmp, &bir_abs, top) {
        Ok(p) => p,
        Err(e) => return fail(e),
    };
    let map = tmp.join("export.map");
    let so = format!("{base}.so");
    if let Err(e) = capi_cc_link(
        &so,
        &[&shim_c, &shim_s],
        &lib,
        &map,
        &["bk_*", "trs_*", "new_MODEL_*"],
        true,
    ) {
        return fail(e);
    }
    let _ = std::fs::remove_dir_all(&tmp);
    // user BDPI code travels with the model: bk_init dladdr's its own
    // .so and loads <model>.bdpi.so from beside it
    let bdpi_src = format!(
        "{}.bdpi.so",
        bir_path.strip_suffix(".bir").unwrap_or(bir_path)
    );
    let bdpi_dst = format!("{base}.bdpi.so");
    if std::path::Path::new(&bdpi_src).exists()
        && std::path::Path::new(&bdpi_src).canonicalize().ok()
            != std::path::Path::new(&bdpi_dst).canonicalize().ok()
    {
        if let Err(e) = std::fs::copy(&bdpi_src, &bdpi_dst) {
            return fail(format!("copy {bdpi_src} -> {bdpi_dst}: {e}"));
        }
    }
    // the reference's wrapper, verbatim shape (bsc.hs writeBluesimWrapper)
    // Which writers the model carries is a link-time decision the
    // model itself cannot see: bk_init reads it from the environment,
    // and absent it takes the historical default of vcd alone.  The
    // fast wrapper exports this in its -c/-f dispatch for the same
    // reason; without it here, `sim fst` on a model linked
    // -dump-formats fst answers that it has no FST support.
    let wrapper = format!(
        r##"#!/bin/sh

TRS_CAPI_FORMATS="{formats}"; export TRS_CAPI_FORMATS
BLUESPECDIR=`echo 'puts $env(BLUESPECDIR)' | bluetcl`
TRS_VLT_CACHE="${{TRS_VLT_CACHE:-`dirname $0`/trs-vlt}}"; export TRS_VLT_CACHE

for arg in $@
do
  if (test "$arg" = "-h")
  then
    exec $BLUESPECDIR/tcllib/bluespec/bluesim.tcl $0.so {top} --script_name `basename $0` -h
  fi
done
exec $BLUESPECDIR/tcllib/bluespec/bluesim.tcl $0.so {top} --script_name `basename $0` "$@"
"##
    );
    if let Err(e) = std::fs::write(base, wrapper) {
        return fail(format!("write {base}: {e}"));
    }
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let _ = std::fs::set_permissions(base, std::fs::Permissions::from_mode(0o755));
    }
    println!("trs link: interactive model written: {so}");
    ExitCode::SUCCESS
}

/// Build the BDPI companion the runtime dlopens beside the .bir.
///
/// A .c or .cxx is compiled here; a .o or .a is taken as given.  An
/// object contributes its symbols whatever else happens, but an archive
/// contributes only the members something asks for -- and nothing
/// inside this object references the imports, since the design calls
/// them through the runtime.  So when a library is on the line, every
/// foreign function the design calls is named with -u to hold it in.
fn build_bdpi(
    srcs: &[&str],
    libs: &[&str],
    paths: &[&str],
    c_names: &[String],
    out: &str,
) -> Result<(), String> {
    let cc = trs_interp::cc_tool();
    let mut objs: Vec<String> = Vec::new();
    for src in srcs {
        if src.ends_with(".o") || src.ends_with(".a") {
            objs.push((*src).to_string());
            continue;
        }
        let stem = std::path::Path::new(src)
            .file_stem()
            .map(|s| s.to_string_lossy().into_owned())
            .unwrap_or_else(|| "bdpi".to_string());
        let obj = format!("{stem}.bdpi.o");
        let st = std::process::Command::new(&cc)
            .args(["-fPIC", "-c", "-o", &obj, src])
            .status()
            .map_err(|e| format!("{cc}: {e}"))?;
        if !st.success() {
            return Err(format!("compiling {src} failed"));
        }
        objs.push(obj);
    }
    let mut cmd = std::process::Command::new(&cc);
    cmd.args(["-shared", "-fPIC"]);
    for p in paths {
        cmd.arg(format!("-L{p}"));
    }
    if !libs.is_empty() {
        // -u wants the LINKER's spelling: Mach-O prefixes an
        // underscore to every C symbol, ELF does not
        let under = cfg!(target_os = "macos");
        for n in c_names {
            cmd.arg(format!("-Wl,-u,{}{n}", if under { "_" } else { "" }));
        }
    }
    cmd.arg("-o").arg(out);
    for o in &objs {
        cmd.arg(o);
    }
    for l in libs {
        cmd.arg(format!("-l{l}"));
    }
    let st = cmd.status().map_err(|e| format!("{cc}: {e}"))?;
    if !st.success() {
        return Err(format!("linking {out} failed"));
    }
    Ok(())
}

/// Verilation is a BUILD step (Q3 ratified per-project, 2026-08-23):
/// resolve the model cache next to the design's .bir when the user
/// did not choose one, and mark build entry points (link, vlt build)
/// as allowed to verilate.  Written to the env early -- these arms are
/// single-threaded here, before any load or workers -- so every later
/// BuildOptions::from_env in this process (and in BviPrim::new during
/// elaboration) resolves identically.
fn ensure_vlt_env(bir_path: &str, build_step: bool) {
    if std::env::var_os("TRS_VLT_CACHE").is_none() {
        let dir = std::path::Path::new(bir_path)
            .parent()
            .filter(|p| !p.as_os_str().is_empty())
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| std::path::PathBuf::from("."));
        let dir = dir.canonicalize().unwrap_or(dir);
        std::env::set_var("TRS_VLT_CACHE", dir.join("trs-vlt"));
    }
    if build_step {
        std::env::set_var("TRS_VLT_BUILD", "1");
    } else {
        // a run is NEVER a build step: actively clear an inherited
        // marker (a wrapper script or CI exporting it around a link
        // would otherwise silently re-enable runtime verilation)
        std::env::remove_var("TRS_VLT_BUILD");
    }
}

/// Verilate-or-cache every BVI model class in the design (design v4
/// sec 5.2) before the interpreter loads it.  BUILD entry points only
/// (trs link, trs vlt build).  A design with no Bvi instances is
/// untouched.  Errors are user errors (refusals, missing sources,
/// toolchain failures), reported with the `bvi:` prefix.
fn bvi_prebuild(frags: &[&str], multi: bool) -> Result<Vec<(String, trs_vlt::BuiltModel)>, String> {
    // The design the LINK will build, assembled the way the link
    // assembles it.  An import can sit in any module the link reaches,
    // so one fragment is never enough -- and under --multi-fragments
    // the set is the one named on the command line, not whatever
    // happens to sit beside the last file.  Resolving those two
    // differently would verilate one design and run another.
    let path = *frags.last().ok_or("no fragments to link")?;
    let design = if multi {
        let mut birs = Vec::with_capacity(frags.len());
        for p in frags {
            let bytes = std::fs::read(p).map_err(|e| format!("{p}: {e}"))?;
            birs.push(trs_ir::Bir::decode(&bytes).map_err(|e| format!("{p}: {e}"))?);
        }
        trs_ir::link::assemble(birs).map_err(|e| e.to_string())?
    } else {
        let bytes = std::fs::read(path).map_err(|e| format!("{path}: {e}"))?;
        trs_interp::startup::decode_with_siblings(path, &bytes)
            .map_err(|e| format!("{path}: {e}"))?
    };
    let has_bvi = design.modules.iter().any(|m| {
        m.instances
            .iter()
            .any(|i| matches!(i.kind, trs_ir::InstanceKind::Bvi(_)))
    });
    if !has_bvi {
        return Ok(Vec::new());
    }
    let mut opts = trs_vlt::BuildOptions::from_env();
    opts.verbose = true;
    trs_vlt::build_all(&design, &opts).map_err(|e| format!("bvi: {e}"))
}

/// LOAD-ONLY check that the build step already produced every BVI
/// model this design needs -- `trs run` (and the artifact wrappers
/// that re-enter it) never verilate.  Forwarded-parameter classes
/// resolve at instantiation and are checked there (load-only too);
/// this precheck covers the literal classes with a clean error before
/// elaboration starts.
fn bvi_precheck(path: &str) -> Result<(), String> {
    let bytes = std::fs::read(path).map_err(|e| format!("{path}: {e}"))?;
    let design = trs_interp::startup::decode_with_siblings(path, &bytes)
        .map_err(|e| format!("{path}: {e}"))?;
    let opts = trs_vlt::BuildOptions::from_env();
    let mut missing: Vec<String> = Vec::new();
    let mut seen: Vec<String> = Vec::new();
    for m in &design.modules {
        for inst in &m.instances {
            let trs_ir::InstanceKind::Bvi(c) = &inst.kind else {
                continue;
            };
            if c.params
                .iter()
                .any(|p| matches!(p.value, trs_ir::bvi::BviParamValue::FromArg { .. }))
            {
                continue;
            }
            let top = design.strings[c.verilog_name as usize].clone();
            // dedup on the RUN IDENTITY, not the top name: two imports
            // of one module with different literal parameters are
            // distinct classes and each needs its own artifact
            let ident = trs_vlt::run_identity(c, &design.strings, None)
                .map_err(|e| format!("bvi: {top}: {e}"))?;
            if seen.contains(&ident) {
                continue;
            }
            seen.push(ident);
            match trs_vlt::find_model_resolved(c, &design.strings, &opts, None) {
                Ok(Some(_)) => {}
                Ok(None) => missing.push(top),
                Err(e) => return Err(format!("bvi: {top}: {e}")),
            }
        }
    }
    if missing.is_empty() {
        Ok(())
    } else {
        Err(format!(
            "bvi: verilated models not built for: {} (cache {}); \
             verilation is a build step -- link the design \
             (`trs link {path}`) or run `trs vlt build {path}` first",
            missing.join(", "),
            opts.cache_dir.display()
        ))
    }
}

fn run_script(
    path: &str,
    max_cycles: u64,
    plusargs: &[String],
    binds: &[trs_interp::TopBind],
    vcd: Option<&str>,
    wave: Option<(trs_interp::WaveFormat, Option<String>)>,
    code: Option<&str>,
    formats: Option<(bool, bool)>,
    only_compiled: bool,
    script: &str,
) -> ExitCode {
    // script-tier command responses print through std stdout between
    // sim advances — pin the sim's stdout sink to the same LineWriter
    // so the two cannot reorder (out.rs)
    trs_interp::stdout_force_line();
    // BVI imports: LOAD-ONLY (v1.5) -- verilation happened at the
    // build step (trs link / trs vlt build); a cold cache is a rebuild
    // instruction, never a runtime verilation
    ensure_vlt_env(path, false);
    if let Err(e) = bvi_precheck(path) {
        eprintln!("trs run: {e}");
        return ExitCode::FAILURE;
    }
    let mut interp = match trs_interp::load_file(path, plusargs, binds, vcd) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("trs: {e}");
            return ExitCode::FAILURE;
        }
    };
    if let Some((v, f)) = formats {
        interp.set_allowed_wave_formats(v, f);
    }
    if let Some((f, file)) = wave {
        interp.wave_request(f, file);
    }
    interp.set_require_compiled(only_compiled);
    if let Some(so) = code {
        interp.aot_request_code(so.into());
    }
    for raw in script.split(['\n', ';']) {
        let cmd = raw.trim();
        if cmd.is_empty() {
            continue;
        }
        // `puts [sim x]`: evaluate the bracketed command and print it
        let (do_print, inner) = match cmd.strip_prefix("puts ") {
            Some(rest) => {
                let r = rest.trim();
                let r = r
                    .strip_prefix('[')
                    .and_then(|r| r.strip_suffix(']'))
                    .unwrap_or(r);
                (true, r.trim().to_string())
            }
            None => (false, cmd.to_string()),
        };
        let words: Vec<&str> = inner.split_whitespace().collect();
        let out = match words.as_slice() {
            ["sim", "run"] | ["sim", "step"] | ["sim", "step", _] => {
                // the reference kernel refuses to continue after $finish
                if interp.is_finished() {
                    let what = if words[1] == "run" {
                        "run anymore"
                    } else {
                        "step"
                    };
                    eprintln!("Error: $finish has been called -- cannot {what}");
                    interp.finish();
                    return ExitCode::FAILURE;
                }
                // step N advances N default-clock posedges from the
                // current cycle cursor; run goes to the -m limit
                let target = match words.as_slice() {
                    ["sim", "step", n] => interp
                        .cycles()
                        .saturating_add(n.parse::<u64>().unwrap_or(1)),
                    ["sim", "step"] => interp.cycles() + 1,
                    _ => max_cycles,
                };
                interp.advance(target.min(max_cycles));
                String::new()
            }
            ["sim", "time"] => format!("{}", interp.now()),
            ["sim", "clock"] => interp
                .clock_info()
                .iter()
                .enumerate()
                .map(|(i, c)| {
                    format!(
                        "{{{} {} {} {} {} {} {} {} {} {}}}",
                        i,
                        (i == 0) as u32,
                        c.name,
                        c.initial_val as u32,
                        c.first_edge,
                        c.low_dur,
                        c.high_dur,
                        c.cycles,
                        c.cur_val as u32,
                        c.last_edge,
                    )
                })
                .collect::<Vec<_>>()
                .join(" "),
            ["sim", "config", "interactive"] => String::new(),
            // bluetcl's `sim vcd` / `sim fst`: select the format
            // (refused with the reference's error when the model was
            // not built with it), then on|off|<file>|query
            ["sim", f @ ("vcd" | "fst")] => {
                // query: current dump file name (empty list when none)
                let _ = f;
                interp.vcd_file_name().to_string()
            }
            ["sim", f @ ("vcd" | "fst"), arg] => {
                let fmt = if *f == "fst" {
                    trs_interp::WaveFormat::Fst
                } else {
                    trs_interp::WaveFormat::Vcd
                };
                match *arg {
                    "off" => interp.vcd_disable(),
                    "on" => {
                        if interp.wave_set_format(fmt) {
                            let _ = interp.vcd_enable();
                        }
                    }
                    file => {
                        if interp.wave_set_format(fmt) && interp.vcd_set_file(Some(file)).is_ok() {
                            let _ = interp.vcd_enable();
                        }
                    }
                }
                String::new()
            }
            _ => {
                eprintln!(
                    "trs: unsupported -c/-f command {cmd:?} \
                     (the interactive surface is not yet implemented)"
                );
                return ExitCode::from(2);
            }
        };
        if do_print {
            println!("{out}");
        }
    }
    // end-of-session epilogue: final VCD flush + $fatal exit code
    ExitCode::from(if interp.finish() != 0 { 1 } else { 0 })
}
