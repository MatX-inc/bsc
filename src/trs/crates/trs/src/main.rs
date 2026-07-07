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
            let max_cycles = match rest {
                ["-m", n] => n.parse().unwrap_or(u64::MAX),
                _ => u64::MAX,
            };
            match trs_interp::run_file(path, max_cycles) {
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
