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
    let args: Vec<String> = std::env::args().skip(1).collect();
    match args.iter().map(String::as_str).collect::<Vec<_>>().as_slice() {
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
            let mut it = rest.iter();
            while let Some(a) = it.next() {
                match *a {
                    "-o" => out = it.next().map(|s| s.to_string()),
                    "--interactive" => interactive = true,
                    other => {
                        eprintln!("Error: invalid link option '{other}'");
                        return ExitCode::from(2);
                    }
                }
            }
            let base = out.unwrap_or_else(|| {
                format!("{}.cexe", path.strip_suffix(".bir").unwrap_or(path))
            });
            let mut interp = match trs_interp::load_file(path, &[], None) {
                Ok(i) => i,
                Err(e) => {
                    eprintln!("trs: {e}");
                    return ExitCode::FAILURE;
                }
            };
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
                    _ => eprintln!(
                        "trs link: note: aot engine unavailable for \
                         this design; the model's aot selection will \
                         run interpreted"
                    ),
                }
                return link_interactive(path, &base, interp.top_name());
            }
            interp.aot_request_emit(format!("{base}.so").into());
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
                    eprintln!(
                        "trs link: note: compiled mode unavailable ({e}); \
                         artifact will run interpreted"
                    );
                    false
                }
                None => {
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
            let script = if compiled {
                format!(
                    "#!/bin/sh\nd=`dirname \"$0\"`\nb=`basename \"$0\"`\n\
                     exec \"${{TRS:-{self_exe}}}\" run \"$d/$b.bir\" --code \"$d/$b.so\"{split_arg} ${{1+\"$@\"}}\n"
                )
            } else {
                format!(
                    "#!/bin/sh\nd=`dirname \"$0\"`\nb=`basename \"$0\"`\n\
                     exec \"${{TRS:-{self_exe}}}\" run \"$d/$b.bir\" ${{1+\"$@\"}}\n"
                )
            };
            if let Err(e) = std::fs::write(&base, script) {
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
        ["run", path, rest @ ..] => {
            // mirror the bluesim.tcl driver's argument handling: -m N is
            // the cycle limit, +foo registers a plusarg (sans '+'),
            // anything else is an error
            let mut max_cycles = u64::MAX;
            let mut plusargs: Vec<String> = Vec::new();
            let mut vcd_file: Option<String> = None;
            let mut code_so: Option<String> = None;
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
                    p if p.starts_with('+') => plusargs.push(p[1..].to_string()),
                    other => {
                        eprintln!("Error: invalid option '{other}'");
                        return ExitCode::from(2);
                    }
                }
            }
            if !script_cmds.is_empty() {
                return run_script(
                    path,
                    max_cycles,
                    &plusargs,
                    vcd_file.as_deref(),
                    code_so.as_deref(),
                    &script_cmds,
                );
            }
            match trs_interp::run_file(
                path,
                max_cycles,
                &plusargs,
                vcd_file.as_deref(),
                code_so.as_deref(),
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
    "bk_set_VCD_file", "bk_enable_VCD_dumping", "bk_disable_VCD_dumping",
];

/// `trs link --interactive`: produce <base>.so (the bk_* capi model
/// with the BIR embedded via incbin) and <base>, the same bluesim.tcl
/// wrapper the reference emits — `sim load`-able by stock bluetcl and
/// runnable by the interactive testsuite unchanged.
fn link_interactive(bir_path: &str, base: &str, top: &str) -> ExitCode {
    let fail = |m: String| {
        eprintln!("trs link --interactive: {m}");
        ExitCode::FAILURE
    };
    // the capi staticlib: env override, then alongside the binary
    let lib = std::env::var("TRS_CAPI_LIB").ok().map(std::path::PathBuf::from).or_else(|| {
        let exe = std::env::current_exe().ok()?;
        let d = exe.parent()?;
        [d.join("libtrs_capi.a"), d.join("../lib/libtrs_capi.a")]
            .into_iter()
            .find(|p| p.exists())
    });
    let Some(lib) = lib else {
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
    let shim_s = tmp.join("bir.s");
    let shim_c = tmp.join("shim.c");
    let map = tmp.join("export.map");
    let w = |p: &std::path::Path, c: String| std::fs::write(p, c);
    if let Err(e) = w(
        &shim_s,
        format!(
            r##"	.section .rodata
	.align 8
	.globl trs_bir_start
trs_bir_start:
	.incbin "{}"
	.globl trs_bir_end
trs_bir_end:
"##,
            bir_abs.display()
        ),
    ) {
        return fail(format!("write {}: {e}", shim_s.display()));
    }
    if let Err(e) = w(
        &shim_c,
        format!(
            r##"/* generated by trs link --interactive */
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
    ) {
        return fail(format!("write {}: {e}", shim_c.display()));
    }
    if let Err(e) = w(
        &map,
        "{ global: bk_*; trs_*; new_MODEL_*; local: *; };\n".into(),
    ) {
        return fail(format!("write {}: {e}", map.display()));
    }
    let so = format!("{base}.so");
    let mut cc = std::process::Command::new("cc");
    cc.arg("-shared")
        .arg("-fPIC")
        .arg("-o")
        .arg(&so)
        .arg(&shim_c)
        .arg(&shim_s);
    // force-keep exactly the exported surface: --whole-archive would
    // drag every llvm-sys binding object (LineEditor -> libedit, ffi
    // stubs) into the .so; -u pulls only what the bk_*/new_MODEL
    // closure actually needs
    for sym in BK_EXPORTS {
        cc.arg(format!("-Wl,-u,{sym}"));
    }
    cc.arg(&lib)
        // rust emits function sections: dead-strip everything the
        // -u keep-list doesn't reach, and drop symbols (166MB -> )
        .arg("-Wl,--gc-sections")
        .arg("-Wl,-s")
        .arg("-Wl,-Bsymbolic")
        .arg(format!("-Wl,--version-script={}", map.display()))
        .arg("-lpthread")
        .arg("-ldl")
        .arg("-lm");
    if cfg!(feature = "jit") {
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
            .arg("-lz")
            // the execution engine bindings reference libffi (shared
            // libLLVM does not re-export it)
            .arg("-lffi")
            // terminfo + zstd: llvm-sys support-library residue
            .arg("-ltinfo")
            .arg("-lzstd")
            // -shared tolerates undefined symbols; RTLD_NOW does not —
            // fail at LINK time instead of at sim load
            .arg("-Wl,--no-undefined");
    }
    let st = cc.status();
    match st {
        Ok(s) if s.success() => {}
        Ok(s) => return fail(format!("cc exited {s}")),
        Err(e) => return fail(format!("cc: {e}")),
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
    code: Option<&str>,
    script: &str,
) -> ExitCode {
    let mut interp = match trs_interp::load_file(path, plusargs, vcd) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("trs: {e}");
            return ExitCode::FAILURE;
        }
    };
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
