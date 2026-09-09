//! Verilator metadata adapter (v1.4 stable-interface, 2026-08-23;
//! supersedes the --json-only frontend dump).
//!
//! Everything trs needs about the model is read from STABLE build
//! products of the verilate step itself -- no frontend dump, no
//! version-sensitive schema:
//!
//!   - ports: the VL_IN*/VL_OUT*/VL_INOUT* declarations in V<top>.h
//!     (this grammar has been stable across Verilator majors): the
//!     direction and size class come from the macro name, the width
//!     from the msb/lsb arguments, and the member name is Verilator's
//!     C++ identifier, whose __0xx escapes decode back to the source
//!     port name for contract matching.  Only primary ports appear in
//!     the top wrapper header -- exactly the set the contract binds.
//!   - timing: the build ALWAYS passes --timing, and Verilator itself
//!     reports through VM_TIMING in the generated V<top>_classes.mk
//!     whether the model actually uses timing constructs (a delay-free
//!     source under --timing gets VM_TIMING=0, verified on 5.020 and
//!     5.050).  VM_TIMING selects verilated_timing.o and the coroutine
//!     flags at link; the shim's drain loop is shape-independent
//!     because eventsPending()/nextTimeSlot() are declared on the
//!     model class either way (false/never-called when untimed).
//!   - DPI: the V<top>__Dpi.h backstop in the builder (file emission
//!     is deterministic on every version) is the ONLY check.
//!
//! The floor is therefore any --timing-capable Verilator (5.x): a
//! binary that rejects the option gets a clear error naming
//! TRS_VERILATOR (see the builder).  The pin remains the plan of
//! record -- re-run the r3 battery on any pin change.

use std::path::Path;
use std::process::Command;

use crate::VltError;

#[derive(Debug, Clone)]
pub struct MetaPort {
    /// Verilator's C++ member name (possibly escape-encoded).
    pub name: String,
    /// The source-level port name; contract ports match against this.
    pub orig_name: String,
    pub dir: PortDir,
    pub width: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PortDir {
    Input,
    Output,
    Inout,
}

#[derive(Debug, Clone)]
pub struct ModelMeta {
    pub ports: Vec<MetaPort>,
    /// VM_TIMING from the generated classes.mk: the model uses timing
    /// constructs (delays), so the link needs verilated_timing.o and
    /// the platform coroutine flags.
    pub vm_timing: bool,
}

pub fn verilator_version(vlt: &Path) -> Result<(u32, u32, String), VltError> {
    let out = Command::new(vlt)
        .arg("--version")
        .output()
        .map_err(|e| VltError::tool("version probe", format!("{}: {e}", vlt.display())))?;
    let text = String::from_utf8_lossy(&out.stdout).trim().to_string();
    // "Verilator 5.020 2023-10-29 rev ..."
    let mut major = 0u32;
    let mut minor = 0u32;
    for tok in text.split_whitespace() {
        if let Some((a, b)) = tok.split_once('.') {
            if let (Ok(x), Ok(y)) = (a.parse(), b.parse()) {
                major = x;
                minor = y;
                break;
            }
        }
    }
    if major == 0 {
        return Err(VltError::tool(
            "version probe",
            format!("unrecognized --version output: {text:?}"),
        ));
    }
    Ok((major, minor, text))
}

/// Decode Verilator's C++ identifier escaping: a character outside
/// [a-zA-Z0-9_] is encoded as "__0" + two hex digits (e.g. "$" ->
/// "__024").  Names without escapes pass through unchanged.
fn decode_name(s: &str) -> String {
    let b = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let mut i = 0;
    while i < b.len() {
        if i + 5 <= b.len() && &b[i..i + 3] == b"__0" {
            if let Ok(c) = u8::from_str_radix(&s[i + 3..i + 5], 16) {
                out.push(c as char);
                i += 5;
                continue;
            }
        }
        out.push(b[i] as char);
        i += 1;
    }
    out
}

/// Scrape the model metadata from a completed verilate output
/// directory (the -Mdir the builder just ran `--cc --timing` into).
pub fn scrape(mdir: &Path, top: &str) -> Result<ModelMeta, VltError> {
    let hdr_path = mdir.join(format!("V{top}.h"));
    let hdr = std::fs::read_to_string(&hdr_path)
        .map_err(|e| VltError::tool("metadata scrape", format!("{}: {e}", hdr_path.display())))?;

    let mut ports = Vec::new();
    for line in hdr.lines() {
        let t = line.trim_start();
        let Some(rest) = t.strip_prefix("VL_") else {
            continue;
        };
        let (dir, rest) = if let Some(r) = rest.strip_prefix("INOUT") {
            (PortDir::Inout, r)
        } else if let Some(r) = rest.strip_prefix("IN") {
            (PortDir::Input, r)
        } else if let Some(r) = rest.strip_prefix("OUT") {
            (PortDir::Output, r)
        } else {
            continue;
        };
        // VL_IN8(&name,msb,lsb); VL_IN(&name,31,0); VL_INW(&name,msb,lsb,words);
        let Some(open) = rest.find('(') else { continue };
        let (suffix, args) = rest.split_at(open);
        if !matches!(suffix, "" | "8" | "16" | "64" | "W") {
            continue;
        }
        let mut it = args[1..]
            .trim_end()
            .trim_end_matches(';')
            .trim_end_matches(')')
            .split(',');
        let name = it
            .next()
            .unwrap_or("")
            .trim()
            .trim_start_matches('&')
            .to_string();
        let parse_bound = |tok: Option<&str>| -> Result<i64, VltError> {
            tok.map(str::trim)
                .and_then(|v| v.parse().ok())
                .ok_or_else(|| {
                    VltError::tool(
                        "metadata scrape",
                        format!("{}: unparseable port line: {line}", hdr_path.display()),
                    )
                })
        };
        let msb = parse_bound(it.next())?;
        let lsb = parse_bound(it.next())?;
        let width = ((msb - lsb).unsigned_abs() as u32) + 1;
        ports.push(MetaPort {
            orig_name: decode_name(&name),
            name,
            dir,
            width,
        });
    }

    let cls_path = mdir.join(format!("V{top}_classes.mk"));
    let cls = std::fs::read_to_string(&cls_path)
        .map_err(|e| VltError::tool("metadata scrape", format!("{}: {e}", cls_path.display())))?;
    let vm_timing = cls.lines().any(|l| {
        l.split_once('=')
            .map(|(k, v)| k.trim() == "VM_TIMING" && v.trim() == "1")
            .unwrap_or(false)
    });

    Ok(ModelMeta { ports, vm_timing })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn port_grammar() {
        let dir = std::env::temp_dir().join(format!("trs-vlt-meta-test-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(
            dir.join("VShapes.h"),
            "class VShapes {\n\
             \x20   // PORTS\n\
             \x20   VL_IN8(&CLK,0,0);\n\
             \x20   VL_INOUT8(&IO,3,0);\n\
             \x20   VL_IN(&A,31,0);\n\
             \x20   VL_INW(&W,99,0,4);\n\
             \x20   VL_OUT(&Q,31,0);\n\
             \x20   VL_OUT16(&H,14,0);\n\
             \x20   VL_IN64(&B,63,0);\n\
             \x20   VL_IN8(&esc__024x,0,0);\n\
             };\n",
        )
        .unwrap();
        std::fs::write(dir.join("VShapes_classes.mk"), "VM_TIMING = 0\n").unwrap();
        let m = scrape(&dir, "Shapes").unwrap();
        assert!(!m.vm_timing);
        let find = |o: &str| m.ports.iter().find(|p| p.orig_name == o).unwrap();
        assert_eq!(find("CLK").width, 1);
        assert_eq!(find("IO").dir, PortDir::Inout);
        assert_eq!(find("A").width, 32);
        assert_eq!(find("W").width, 100);
        assert_eq!(find("Q").dir, PortDir::Output);
        assert_eq!(find("H").width, 15);
        assert_eq!(find("B").width, 64);
        // escape decode: "__024" -> '$'
        assert_eq!(find("esc$x").name, "esc__024x");
        std::fs::remove_dir_all(&dir).ok();
    }

    #[test]
    fn vm_timing_on() {
        let dir = std::env::temp_dir().join(format!("trs-vlt-meta-test2-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("VDly.h"), "VL_OUT8(&P,7,0);\n").unwrap();
        std::fs::write(dir.join("VDly_classes.mk"), "# gen\nVM_TIMING = 1\n").unwrap();
        let m = scrape(&dir, "Dly").unwrap();
        assert!(m.vm_timing);
        assert_eq!(m.ports.len(), 1);
        std::fs::remove_dir_all(&dir).ok();
    }
}
