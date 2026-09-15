//! Top-file resolution (design v4 sec 5.1 item 4): find the Verilog
//! source defining the contract's top module.
//!
//! Order: explicitly named files (--vfile / contract vfiles) win; then
//! `<top>.v` / `<top>.sv` directly in a vpath dir; then a
//! module-declaration scan of every .v/.sv in the vpath dirs (the
//! `Verilog_SRAM_model.v` name-split case).  The scan is comment-naive
//! by design: a commented-out `module top;` in some other file only
//! ADDS a candidate, and candidates are verified by the verilator
//! dump's own top-module check downstream; a miss here errors loudly
//! naming every directory searched.

use std::path::PathBuf;

use crate::VltError;

/// Does `text` contain a declaration of `module <top>` (word-boundary)?
fn declares_module(text: &str, top: &str) -> bool {
    let mut rest = text;
    while let Some(pos) = rest.find("module") {
        let before_ok = pos == 0
            || !rest[..pos]
                .chars()
                .next_back()
                .map(|c| c.is_alphanumeric() || c == '_' || c == '$')
                .unwrap_or(false);
        let after = &rest[pos + "module".len()..];
        if before_ok {
            let after_trim = after.trim_start();
            if after_trim.starts_with(top) {
                let tail = &after_trim[top.len()..];
                let boundary = tail
                    .chars()
                    .next()
                    .map(|c| !(c.is_alphanumeric() || c == '_' || c == '$'))
                    .unwrap_or(true);
                if boundary {
                    return true;
                }
            }
        }
        rest = after;
    }
    false
}

pub fn resolve_top(top: &str, vpath: &[PathBuf], vfiles: &[PathBuf]) -> Result<PathBuf, VltError> {
    // explicit files first
    for f in vfiles {
        if !f.is_file() {
            return Err(VltError::resolve(format!(
                "named file does not exist: {}",
                f.display()
            )));
        }
        let text = std::fs::read_to_string(f).unwrap_or_default();
        if declares_module(&text, top) {
            return Ok(f.clone());
        }
    }
    if !vfiles.is_empty() {
        return Err(VltError::resolve(format!(
            "none of the named files declare module '{top}': {}",
            vfiles
                .iter()
                .map(|f| f.display().to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )));
    }
    // <top>.v / <top>.sv directly
    for d in vpath {
        for ext in ["v", "sv"] {
            let cand = d.join(format!("{top}.{ext}"));
            if cand.is_file() {
                return Ok(cand);
            }
        }
    }
    // module-declaration scan
    for d in vpath {
        let rd = match std::fs::read_dir(d) {
            Ok(rd) => rd,
            Err(_) => continue,
        };
        let mut names: Vec<PathBuf> = rd
            .flatten()
            .map(|e| e.path())
            .filter(|p| {
                matches!(
                    p.extension().and_then(|e| e.to_str()),
                    Some("v") | Some("sv")
                )
            })
            .collect();
        names.sort();
        for p in names {
            if let Ok(text) = std::fs::read_to_string(&p) {
                if declares_module(&text, top) {
                    return Ok(p);
                }
            }
        }
    }
    Err(VltError::resolve(format!(
        "cannot find a Verilog source declaring module '{top}'; searched: {}",
        if vpath.is_empty() {
            "(no vpath directories)".to_string()
        } else {
            vpath
                .iter()
                .map(|d| d.display().to_string())
                .collect::<Vec<_>>()
                .join(", ")
        }
    )))
}

/// Existing directories only, order-preserving, deduped.
pub fn clean_dirs(dirs: &[PathBuf]) -> Vec<PathBuf> {
    let mut out: Vec<PathBuf> = Vec::new();
    for d in dirs {
        if d.is_dir() && !out.contains(d) {
            out.push(d.clone());
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn declaration_scan() {
        assert!(declares_module("module Foo(input x);", "Foo"));
        assert!(declares_module("  module  Foo (", "Foo"));
        assert!(declares_module("module Foo;", "Foo"));
        assert!(!declares_module("module FooBar(", "Foo"));
        assert!(!declares_module("endmodule Foo", "Foo"));
        assert!(!declares_module("mymodule Foo(", "Foo"));
        assert!(declares_module("module Bar; endmodule\nmodule Foo;", "Foo"));
    }
}
