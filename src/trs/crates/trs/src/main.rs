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
            let mut it = rest.iter();
            while let Some(a) = it.next() {
                match *a {
                    "-m" => {
                        max_cycles = it
                            .next()
                            .and_then(|n| n.parse::<u64>().ok())
                            .unwrap_or(u64::MAX);
                    }
                    p if p.starts_with('+') => plusargs.push(p[1..].to_string()),
                    other => {
                        eprintln!("Error: invalid option '{other}'");
                        return ExitCode::from(2);
                    }
                }
            }
            match trs_interp::run_file(path, max_cycles, &plusargs) {
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
