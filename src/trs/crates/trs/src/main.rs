//! trs — the TRS driver.
//!
//! Invoked by `bsc` where `simLink` runs today, or directly by build
//! systems.  Planned subcommands (DESIGN.md §3, §10):
//!
//!   trs ir dump <mod.bir>       pretty-print BIR (P0 diff-testing)
//!   trs link <top> <bir...>     plan + codegen + link a simulation
//!   trs run <top> [args]        JIT-and-run without artifacts

use std::process::ExitCode;

fn usage() -> ExitCode {
    eprintln!("trs {} (phase P0 scaffold)", env!("CARGO_PKG_VERSION"));
    eprintln!("usage: trs ir dump <module.bir>");
    eprintln!("       trs link <module.bir> [-o <out.cexe>]");
    eprintln!("       trs run <module.bir> [-m max_cycles] [--code <model.so>]");
    ExitCode::from(2)
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
    let args: Vec<String> = std::env::args().skip(1).collect();
    match args.iter().map(String::as_str).collect::<Vec<_>>().as_slice() {
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
        ["ir", "dump", path] => match std::fs::read(path) {
            Ok(bytes) => match trs_ir::Design::decode(&bytes) {
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
                eprintln!("trs: {path}: {e}");
                ExitCode::FAILURE
            }
        },
        // trs link: compile the design ahead of time and write the
        // persistent artifact: <out> (wrapper script with the same CLI
        // as reference Bluesim), <out>.bir, <out>.so.  Runs never
        // compile again — same amortization as Verilator/VCS/Bluesim.
        ["link", path, rest @ ..] => {
            let mut out: Option<String> = None;
            let mut interactive = false;
            let mut exe = false;
            // -dump-formats plumbing from bsc: which waveform writers
            // the artifact carries (reference default: vcd only)
            let mut fmt_arg = "vcd".to_string();
            let mut it = rest.iter();
            while let Some(a) = it.next() {
                match *a {
                    "-o" => out = it.next().map(|s| s.to_string()),
                    "--interactive" => interactive = true,
                    "--exe" => exe = true,
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
                    // hermeticity: every output-affecting knob is a
                    // flag (bsc passes these through; build systems
                    // key actions on argv, not env).  The env vars
                    // stay as the internal spelling — a flag wins by
                    // writing the env here, single-threaded, before
                    // any planning or workers.
                    "--cc" | "--edge-ssa" | "--aot-one-module" | "--jit-split"
                    | "--jit-opt" | "--jit-pipeline" | "--jit-threads"
                    | "--outline" | "--outline-factor" | "--capi-lib" => {
                        let key = match *a {
                            "--cc" => "TRS_CC",
                            "--edge-ssa" => "TRS_EDGE_SSA",
                            "--aot-one-module" => "TRS_AOT_ONE_MODULE",
                            "--jit-split" => "TRS_JIT_SPLIT",
                            "--jit-opt" => "TRS_JIT_OPT",
                            "--jit-pipeline" => "TRS_JIT_PIPELINE",
                            "--jit-threads" => "TRS_JIT_THREADS",
                            "--outline" => "TRS_EDGE_SSA_OUTLINE",
                            "--outline-factor" => "TRS_EDGE_SSA_OUTLINE_FACTOR",
                            _ => "TRS_CAPI_LIB",
                        };
                        match it.next() {
                            Some(v) => std::env::set_var(key, v),
                            None => {
                                eprintln!("Error: {a} requires a value");
                                return ExitCode::from(2);
                            }
                        }
                    }
                    "--no-fusion" => std::env::set_var("TRS_NO_FUSION", "1"),
                    "--jit-novec" => std::env::set_var("TRS_JIT_NOVEC", "1"),
                    other => {
                        eprintln!("Error: invalid link option '{other}'");
                        return ExitCode::from(2);
                    }
                }
            }
            let base = out.unwrap_or_else(|| {
                format!("{}.cexe", path.strip_suffix(".bir").unwrap_or(path))
            });
            // a .mem is an input to the simulation, not to the build:
            // the reference reads a load file when the model object is
            // constructed, so the artifact written here opens its own
            // when it runs (see prim::LOAD_MEMFILES)
            trs_interp::prim::set_load_memfiles(false);
            // _fresh: link WRITES the snapshot, so it decodes the .bir
            // source of truth, never a prior sidecar (see startup.rs)
            let mut interp = match trs_interp::startup::load_file_fresh(path, &[], None) {
                Ok(i) => i,
                Err(e) => {
                    eprintln!("trs: {e}");
                    return ExitCode::FAILURE;
                }
            };
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
                // take stay interp/jit with a note, like plain link.
                interp.aot_request_emit(format!("{base}.aot.so").into());
                interp.prime();
                match interp.aot_take_emit_result() {
                    Some(trs_interp::AotEmit::Compiled) => {}
                    Some(trs_interp::AotEmit::Failed(e)) => {
                        eprintln!("trs link: {e}");
                        return ExitCode::FAILURE;
                    }
                    _ => {
                        if std::env::var_os("TRS_REQUIRE_AOT").is_some() {
                            eprintln!(
                                "trs link: TRS_REQUIRE_AOT is set but the \
                                 aot engine is unavailable for this \
                                 design; refusing"
                            );
                            return ExitCode::from(86);
                        }
                        eprintln!(
                            "trs link: note: aot engine unavailable for \
                             this design; the model's aot selection will \
                             run interpreted"
                        )
                    }
                }
                return link_interactive(path, &base, interp.top_name());
            }
            if exe {
                // artifact-as-executable: <base> becomes a real PIE
                // (design objects + main shim + libtrs_capi.so from
                // the install dir) instead of the wrapper script
                let libdir = std::env::current_exe()
                    .ok()
                    .and_then(|p| p.parent().map(|d| d.to_path_buf()))
                    .unwrap_or_else(|| ".".into());
                interp.aot_request_emit_exe(
                    format!("{base}.so").into(),
                    base.clone().into(),
                    libdir,
                );
            } else {
                interp.aot_request_emit(format!("{base}.so").into());
            }
            interp.prime();
            // ineligible designs still get a valid artifact — it runs
            // interpreted (reference Bluesim always yields an
            // executable); only infrastructure failures fail the link
            let compiled = match interp.aot_take_emit_result() {
                Some(trs_interp::AotEmit::Compiled) => true,
                Some(trs_interp::AotEmit::Failed(e)) => {
                    eprintln!("trs link: {e}");
                    return ExitCode::FAILURE;
                }
                Some(trs_interp::AotEmit::Ineligible(e)) => {
                    if std::env::var_os("TRS_REQUIRE_AOT").is_some() {
                        eprintln!(
                            "trs link: TRS_REQUIRE_AOT is set but compiled \
                             mode is unavailable ({e}); refusing"
                        );
                        return ExitCode::from(86);
                    }
                    eprintln!(
                        "trs link: note: compiled mode unavailable ({e}); \
                         artifact will run interpreted"
                    );
                    false
                }
                None => {
                    if std::env::var_os("TRS_REQUIRE_AOT").is_some() {
                        eprintln!(
                            "trs link: TRS_REQUIRE_AOT is set but compiled \
                             mode is unavailable (TRS_JIT_TRACE=1 shows \
                             why); refusing"
                        );
                        return ExitCode::from(86);
                    }
                    eprintln!(
                        "trs link: note: compiled mode unavailable \
                         (TRS_JIT_TRACE=1 shows why); artifact will run \
                         interpreted"
                    );
                    false
                }
            };
            // .bir sibling: the script runs <base>.bir next to the .so
            let bir_dst = format!("{base}.bir");
            if std::path::Path::new(path).canonicalize().ok()
                != std::path::Path::new(&bir_dst).canonicalize().ok()
            {
                if let Err(e) = std::fs::copy(path, &bir_dst) {
                    eprintln!("trs link: copy {path} -> {bir_dst}: {e}");
                    return ExitCode::FAILURE;
                }
            }
            // decoded-design snapshot: run startup skips the CBOR parse
            // when its fingerprint gate matches (a cache, never a source
            // of truth; stale/missing -> normal decode)
            if let Err(e) = interp.write_snapshot(&format!("{base}.birsnap")) {
                eprintln!("trs link: note: snapshot not written ({e})");
            }
            // user BDPI code travels with the artifact: load_file looks
            // for <base>.bdpi.so next to the (renamed) .bir
            let bdpi_src =
                format!("{}.bdpi.so", path.strip_suffix(".bir").unwrap_or(path));
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
            if exe {
                if !compiled {
                    // no compiled artifact = no PIE was linked; an
                    // interpreted wrapper is what plain link is for
                    eprintln!(
                        "trs link: --exe requires the compiled artifact \
                         (this design is not aot-eligible)"
                    );
                    return ExitCode::FAILURE;
                }
                // --exe: aot_emit already linked the PIE at <base>;
                // no wrapper script.  The capi/debug companions still
                // ride beside the .so as usual.
                let top = interp.top_name().to_string();
                let _ = write_capi_shim(path, &base, &top, compiled);
                return ExitCode::SUCCESS;
            }
            // wrapper script (trs must be on PATH, like bluetcl for
            // reference Bluesim executables)
            let split = std::env::var("TRS_JIT_SPLIT").unwrap_or_default();
            let split_arg = if split.is_empty() {
                String::new()
            } else {
                format!(" --split {split}")
            };
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
            // debug/script tier: -c/-f are Tcl (while/foreach/expr —
            // bluesim.tcl's `source`/`eval`), so those runs go through
            // stock bluetcl + the capi shim, not the fast runner.  The
            // capi's own default engine applies (traced-plan jit:
            // fast `sim run` AND slot-recorded def/port peeks);
            // TRS_CAPI_ENGINES overrides, and the -dump-formats
            // contract travels via TRS_CAPI_FORMATS.
            let top = interp.top_name().to_string();
            let capi = write_capi_shim(path, &base, &top, compiled);
            let dispatch = if capi {
                format!(
                    "for arg in ${{1+\"$@\"}}\n\
                     do\n\
                     \x20 case \"$arg\" in\n\
                     \x20 -c|-f)\n\
                     \x20   if test -f \"$d/$b.capi.so\"; then\n\
                     \x20     TRS_CAPI_FORMATS=\"{fmt_arg}\"; export TRS_CAPI_FORMATS\n\
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
            let script = if compiled {
                format!(
                    "#!/bin/sh\nd=`dirname \"$0\"`\nb=`basename \"$0\"`\n{dispatch}\
                     exec \"${{TRS:-{self_exe}}}\" run \"$d/$b.bir\" --code \"$d/$b.so\"{split_arg} --formats {fmt_arg} ${{1+\"$@\"}}\n"
                )
            } else {
                format!(
                    "#!/bin/sh\nd=`dirname \"$0\"`\nb=`basename \"$0\"`\n{dispatch}\
                     exec \"${{TRS:-{self_exe}}}\" run \"$d/$b.bir\" --formats {fmt_arg} ${{1+\"$@\"}}\n"
                )
            };
            // temp+rename: a crash mid-write must never leave a
            // truncated-but-executable wrapper (a script missing its
            // exec line runs and exits 0 doing nothing)
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
                let _ = std::fs::set_permissions(
                    &base,
                    std::fs::Permissions::from_mode(0o755),
                );
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
            let out = out.unwrap_or_else(|| {
                if rt { "libtrs_rt.so" } else { "libtrs_capi.so" }.to_string()
            });
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
            let tmp = std::env::temp_dir()
                .join(format!("trs-capi-so-{}", std::process::id()));
            if let Err(e) = std::fs::create_dir_all(&tmp) {
                eprintln!("trs capi-so: {}: {e}", tmp.display());
                return ExitCode::FAILURE;
            }
            let map = tmp.join("export.map");
            // no new_MODEL_* here: the per-design shims provide those
            if let Err(e) = std::fs::write(
                &map,
                "{ global: bk_*; trs_*; local: *; };\n",
            ) {
                eprintln!("trs capi-so: write {}: {e}", map.display());
                return ExitCode::FAILURE;
            }
            let r = capi_cc_link(&out, &[], &lib, &map, !rt);
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
            let mut wave: Option<(trs_interp::WaveFormat, Option<String>)> =
                None;
            let mut vcd_file: Option<String> = None;
            let mut code_so: Option<String> = None;
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
            let mut selfcheck: Option<(u64, bool)> =
                std::env::var_os("TRS_SELFCHECK").map(|_| {
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
            let script = path
                .strip_suffix(".bir")
                .unwrap_or(path)
                .to_string();
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
                        println!(
                            "Error: {f} is deprecated in favor of scriptable debug"
                        );
                        println!("See entry #031 in the KPnS document.");
                        return usage_exit();
                    }
                    "-h" | "-help" | "--help" => return usage_exit(),
                    "-v" => {
                        println!(
                            "trs {} (TRS runtime)",
                            env!("CARGO_PKG_VERSION")
                        );
                        return ExitCode::SUCCESS;
                    }
                    "--script_name" => {
                        let _ = it.next();
                    }
                    "--code" => {
                        code_so = it.next().map(|s| s.to_string());
                    }
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
                                eprintln!(
                                    "Error: --selfcheck-every requires a number"
                                );
                                return ExitCode::from(2);
                            }
                        },
                        None => {
                            eprintln!(
                                "Error: --selfcheck-every requires a number"
                            );
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
                    vcd_file.as_deref(),
                    wave.clone(),
                    code_so.as_deref(),
                    formats,
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
                    so_direct =
                        path.strip_suffix(".so").unwrap().to_string() + ".bir";
                    (so_direct.as_str(), Some(path.to_string()))
                } else {
                    (path as &str, code_so)
                };
            match trs_interp::run_file(
                path,
                max_cycles,
                &plusargs,
                vcd_file.as_deref(),
                wave,
                code_so.as_deref(),
                formats,
                selfcheck,
            ) {
                Ok(code) => {
                    use std::io::Write;
                    let _ = std::io::stdout().flush();
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
    "bk_init", "bk_shutdown", "bk_now", "bk_set_timescale", "bk_version",
    "bk_append_argument", "bk_define_clock", "bk_num_clocks",
    "bk_get_nth_clock", "bk_clock_name", "bk_get_clock_by_name",
    "bk_clock_initial_value", "bk_clock_first_edge", "bk_clock_duration",
    "bk_clock_val", "bk_clock_cycle_count", "bk_clock_edge_count",
    "bk_clock_last_edge", "bk_quit_after_edge", "bk_schedule_ui_event",
    "bk_remove_ui_event", "bk_set_interactive", "bk_advance",
    "bk_is_running", "bk_sync", "bk_abort_now", "bk_finished",
    "bk_exit_status", "bk_fataled", "bk_top_symbol", "bk_lookup_symbol",
    "bk_get_size", "bk_get_key", "bk_is_module", "bk_is_rule",
    "bk_is_single_value", "bk_is_value_range", "bk_peek_symbol_value",
    "bk_get_range_min_addr", "bk_get_range_max_addr",
    "bk_peek_range_value", "bk_num_symbols", "bk_get_nth_symbol",
    "bk_set_VCD_file", "bk_get_VCD_file_name", "bk_enable_VCD_dumping",
    "bk_disable_VCD_dumping", "bk_set_waveform_format",
];

/// A runtime staticlib (libtrs_capi.a or libtrs_rt.a): env override,
/// then alongside the binary.
fn find_staticlib((env, name): (&str, &str)) -> Option<std::path::PathBuf> {
    std::env::var(env).ok().map(std::path::PathBuf::from).or_else(|| {
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
    std::env::var("TRS_CAPI_SO").ok().map(std::path::PathBuf::from).or_else(|| {
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
    llvm: bool,
) -> Result<(), String> {
    let mut cc = std::process::Command::new("cc");
    cc.arg("-shared").arg("-fPIC").arg("-o").arg(out);
    for i in inputs {
        cc.arg(i);
    }
    // force-keep exactly the exported surface: --whole-archive would
    // drag every llvm-sys binding object (LineEditor -> libedit, ffi
    // stubs) into the .so; -u pulls only what the bk_*/new_MODEL
    // closure actually needs
    for sym in BK_EXPORTS {
        cc.arg(format!("-Wl,-u,{sym}"));
    }
    cc.arg(lib)
        // rust emits function sections: dead-strip everything the
        // -u keep-list doesn't reach, and drop symbols (166MB -> )
        .arg("-Wl,--gc-sections")
        .arg("-Wl,-s")
        .arg("-Wl,-Bsymbolic")
        .arg(format!("-Wl,--version-script={}", map.display()))
        .arg("-lpthread")
        .arg("-ldl")
        .arg("-lm")
        // vendored libfst (trs-interp build.rs) gzip-frames FST output
        // through zlib in every flavor, slim included
        .arg("-lz")
        // -shared tolerates undefined symbols; RTLD_NOW (and a static
        // exe link against this .so) does not — fail at LINK time
        // instead of at sim load
        .arg("-Wl,--no-undefined");
    if llvm && cfg!(feature = "jit") {
        // a jit-featured capi staticlib references LLVM (rustc links
        // it into BINARIES only); use the shared libLLVM
        let libdir = std::process::Command::new("llvm-config-18")
            .arg("--libdir")
            .output()
            .ok()
            .and_then(|o| String::from_utf8(o.stdout).ok())
            .map(|s| s.trim().to_string())
            .unwrap_or_else(|| "/usr/lib/llvm-18/lib".into());
        cc.arg(format!("-L{libdir}"))
            .arg("-lLLVM-18")
            .arg("-lstdc++")
            // the execution engine bindings reference libffi (shared
            // libLLVM does not re-export it)
            .arg("-lffi")
            // terminfo + zstd: llvm-sys support-library residue
            .arg("-ltinfo")
            .arg("-lzstd");
    }
    match cc.status() {
        Ok(s) if s.success() => Ok(()),
        Ok(s) => Err(format!("cc exited {s}")),
        Err(e) => Err(format!("cc: {e}")),
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
        format!(
            r##"	.section .note.GNU-stack,"",@progbits
	.section .rodata
	.align 8
	.globl trs_bir_start
trs_bir_start:
	.incbin "{}"
	.globl trs_bir_end
trs_bir_end:
"##,
            bir_abs.display()
        ),
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
        // silent: an install without the shared capi simply has no
        // script tier — the artifact contract doesn't change
        return false;
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
    let tmp =
        std::env::temp_dir().join(format!("trs-capi-shim-{}", std::process::id()));
    if let Err(e) = std::fs::create_dir_all(&tmp) {
        return note(format!("{}: {e}", tmp.display()));
    }
    let sources = write_shim_sources(&tmp, &bir_abs, top);
    let (shim_s, shim_c) = match sources {
        Ok(p) => p,
        Err(e) => return note(e),
    };
    let map = tmp.join("export.map");
    if let Err(e) = std::fs::write(
        &map,
        "{ global: new_MODEL_*; trs_*; local: *; };\n",
    ) {
        return note(format!("write {}: {e}", map.display()));
    }
    let so = format!("{base}.capi.so");
    let mut cc = std::process::Command::new("cc");
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
        .arg("-Wl,--no-as-needed")
        .arg("-l:libtrs_capi.so")
        .arg(format!("-Wl,-rpath,{}", shared_dir.display()))
        .arg(format!("-Wl,--version-script={}", map.display()))
        .arg("-Wl,--no-undefined");
    let r = cc.status();
    let _ = std::fs::remove_dir_all(&tmp);
    match r {
        Ok(s) if s.success() => {}
        Ok(s) => return note(format!("cc exited {s}")),
        Err(e) => return note(format!("cc: {e}")),
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
fn link_interactive(bir_path: &str, base: &str, top: &str) -> ExitCode {
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
    if let Err(e) = std::fs::write(
        &map,
        "{ global: bk_*; trs_*; new_MODEL_*; local: *; };\n",
    ) {
        return fail(format!("write {}: {e}", map.display()));
    }
    let so = format!("{base}.so");
    if let Err(e) = capi_cc_link(&so, &[&shim_c, &shim_s], &lib, &map, true) {
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
    let wrapper = format!(
        r##"#!/bin/sh

BLUESPECDIR=`echo 'puts $env(BLUESPECDIR)' | bluetcl`

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

fn run_script(
    path: &str,
    max_cycles: u64,
    plusargs: &[String],
    vcd: Option<&str>,
    wave: Option<(trs_interp::WaveFormat, Option<String>)>,
    code: Option<&str>,
    formats: Option<(bool, bool)>,
    script: &str,
) -> ExitCode {
    let mut interp = match trs_interp::load_file(path, plusargs, vcd) {
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
                    let what =
                        if words[1] == "run" { "run anymore" } else { "step" };
                    eprintln!("Error: $finish has been called -- cannot {what}");
                    interp.finish();
                    return ExitCode::FAILURE;
                }
                // step N advances N default-clock posedges from the
                // current cycle cursor; run goes to the -m limit
                let target = match words.as_slice() {
                    ["sim", "step", n] => {
                        interp.cycles().saturating_add(n.parse::<u64>().unwrap_or(1))
                    }
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
                        if interp.wave_set_format(fmt)
                            && interp.vcd_set_file(Some(file)).is_ok()
                        {
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
