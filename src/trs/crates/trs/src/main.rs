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
    eprintln!("       trs run <module.bir> [-m max_cycles]");
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
        ["run", path, rest @ ..] => {
            // mirror the bluesim.tcl driver's argument handling: -m N is
            // the cycle limit, +foo registers a plusarg (sans '+'),
            // anything else is an error
            let mut max_cycles = u64::MAX;
            let mut plusargs: Vec<String> = Vec::new();
            let mut vcd_file: Option<String> = None;
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
                return run_script(path, max_cycles, &plusargs, vcd_file.as_deref(), &script_cmds);
            }
            match trs_interp::run_file(path, max_cycles, &plusargs, vcd_file.as_deref()) {
                Ok(code) => ExitCode::from(code.clamp(0, 255) as u8),
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
/// uses outside bsc.bluesim/interactive: a single `sim run`/`sim step N`
/// plus `sim time`/`sim clock` queries and `puts [...]` printing.  The
/// full interactive surface arrives with the bk_* compat .so (task #20);
/// anything beyond this subset errors out loudly.
fn run_script(
    path: &str,
    max_cycles: u64,
    plusargs: &[String],
    vcd: Option<&str>,
    script: &str,
) -> ExitCode {
    let mut interp = match trs_interp::load_file(path, plusargs, vcd) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("trs: {e}");
            return ExitCode::FAILURE;
        }
    };
    let mut ran = false;
    let mut fatal = false;
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
                if ran {
                    eprintln!(
                        "trs: -c/-f supports a single run/step per session \
                         (the interactive surface is not yet implemented)"
                    );
                    return ExitCode::from(2);
                }
                ran = true;
                let n = match words.as_slice() {
                    ["sim", "step", n] => n.parse::<u64>().unwrap_or(1),
                    ["sim", "step"] => 1,
                    _ => max_cycles,
                };
                let rc = interp.run(n.min(max_cycles));
                fatal = rc != 0;
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
    ExitCode::from(if fatal { 1 } else { 0 })
}
