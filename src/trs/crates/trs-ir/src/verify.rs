//! Decode-time validation: every reference resolves, the schedule mentions
//! only known rules, widths are sane.  This is the guard that makes BIR a
//! contract rather than a hope (DESIGN.md §3.1).

use crate::{Design, StrId};

#[derive(Debug)]
pub enum VerifyError {
    BadStringRef { id: StrId, len: usize },
    NoTopModule { top: String },
    DuplicateModule { name: String },
    BadBvi { instance: String, what: String },
}

impl std::fmt::Display for VerifyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VerifyError::BadStringRef { id, len } => {
                write!(f, "string id {id} out of range (table has {len} entries)")
            }
            VerifyError::NoTopModule { top } => {
                write!(f, "top module {top:?} not present in module list")
            }
            VerifyError::DuplicateModule { name } => {
                write!(f, "module {name:?} defined more than once")
            }
            VerifyError::BadBvi { instance, what } => {
                write!(f, "BVI instance {instance:?}: {what}")
            }
        }
    }
}

impl std::error::Error for VerifyError {}

pub fn verify(design: &Design) -> Result<(), VerifyError> {
    let nstrings = design.strings.len();
    let check = |id: StrId| -> Result<(), VerifyError> {
        if (id as usize) < nstrings {
            Ok(())
        } else {
            Err(VerifyError::BadStringRef { id, len: nstrings })
        }
    };

    check(design.top)?;
    let mut seen = std::collections::HashSet::new();
    for m in &design.modules {
        check(m.name)?;
        if !seen.insert(m.name) {
            return Err(VerifyError::DuplicateModule {
                name: design.strings[m.name as usize].clone(),
            });
        }
    }
    if !seen.contains(&design.top) {
        return Err(VerifyError::NoTopModule {
            top: design.strings[design.top as usize].clone(),
        });
    }
    // BVI contracts: reference integrity and the invariants the exporter
    // promises (directed paths, un-aliased outputs, kind consistency).
    for m in &design.modules {
        for inst in &m.instances {
            if let crate::InstanceKind::Bvi(c) = &inst.kind {
                let iname = design.strings[inst.name as usize].clone();
                verify_bvi(design, &iname, c, &check)?;
            }
        }
    }
    // TODO(P0): resolve every Def/Port/Param/instance/method reference in
    // exprs, actions, and schedules; check width consistency; check that
    // every rule's can_fire/will_fire defs exist and are so flagged.
    Ok(())
}

fn verify_bvi(
    design: &Design,
    iname: &str,
    c: &crate::bvi::BviContract,
    check: &dyn Fn(StrId) -> Result<(), VerifyError>,
) -> Result<(), VerifyError> {
    use crate::bvi::{BviDir, BviMethodKind, BviPortKind};
    let bad = |what: String| VerifyError::BadBvi {
        instance: iname.to_string(),
        what,
    };
    // the verilate inputs are string ids like any other: unchecked,
    // a truncated fragment gets past decode and panics later inside
    // link::Remap::s instead of returning a DecodeError here
    for id in c.vpath.iter().chain(c.vfiles.iter()) {
        check(*id)?;
    }
    for (k, v) in &c.defines {
        check(*k)?;
        if let Some(v) = v {
            check(*v)?;
        }
    }
    let nports = c.ports.len() as u32;
    let pidx = |i: u32, what: &str| -> Result<(), VerifyError> {
        if i < nports {
            Ok(())
        } else {
            Err(bad(format!(
                "{what} port index {i} out of range ({nports} ports)"
            )))
        }
    };
    check(c.verilog_name)?;
    for p in &c.ports {
        check(p.name)?;
        if p.width == 0 {
            return Err(bad(format!(
                "zero-width port {}",
                design.strings[p.name as usize]
            )));
        }
    }
    // Output-port ownership: a port may be exactly one method's result OR
    // exactly one method's rdy -- any sharing is the aliasing the exporter
    // must refuse (undeclared cross-method paths hide behind it).
    let mut out_owner: std::collections::HashMap<u32, StrId> = Default::default();
    let mut claim =
        |port: u32, owner: StrId, ports: &[crate::bvi::BviPort]| -> Result<(), VerifyError> {
            if out_owner.insert(port, owner).is_some() {
                return Err(VerifyError::BadBvi {
                    instance: iname.to_string(),
                    what: format!(
                        "output port {} aliased by more than one method (exporter must refuse)",
                        design.strings[ports[port as usize].name as usize]
                    ),
                });
            }
            Ok(())
        };
    for meth in &c.methods {
        check(meth.name)?;
        // None does NOT mean the method has no clock: `clocks` holds
        // only the PORTED input clocks, so a method on a portless clock
        // -- a domain association with no wire, which is how
        // combinational IP places its methods -- cannot name one here.
        // The two are indistinguishable at this point and the runtime
        // reads this field nowhere, so the genuinely-clockless case is
        // the exporter's to refuse, which it does.
        if let Some(ci) = meth.clock {
            if ci as usize >= c.clocks.len() {
                return Err(bad(format!("method clock index {ci} out of range")));
            }
        }
        for &a in &meth.args {
            pidx(a, "arg")?;
            if c.ports[a as usize].dir != BviDir::Input {
                return Err(bad("method arg bound to an output port".into()));
            }
        }
        for &r in &meth.results {
            pidx(r, "result")?;
            if c.ports[r as usize].dir != BviDir::Output {
                return Err(bad("method result bound to an input port".into()));
            }
            claim(r, meth.name, &c.ports)?;
        }
        if let Some(en) = meth.enable {
            pidx(en, "enable")?;
            if c.ports[en as usize].kind != BviPortKind::Enable {
                return Err(bad("enable index does not name an Enable port".into()));
            }
        }
        if let Some(rd) = meth.rdy {
            pidx(rd, "rdy")?;
            if c.ports[rd as usize].kind != BviPortKind::Rdy {
                return Err(bad("rdy index does not name a Rdy port".into()));
            }
            claim(rd, meth.name, &c.ports)?;
        }
    }
    for cl in &c.clocks {
        check(cl.name)?;
        check(cl.tick_port)?;
        pidx(cl.osc_port, "clock osc")?;
        if let Some(g) = cl.gate_port {
            pidx(g, "clock gate")?;
        }
    }
    let mut seen_arg: Vec<u32> = Vec::with_capacity(c.resets.len());
    for r in &c.resets {
        check(r.name)?;
        pidx(r.port, "reset")?;
        // the arg index is what the runtime matches on, so a duplicate
        // or a stale one silently drives the wrong reset
        if seen_arg.contains(&r.arg) {
            return Err(bad(format!("two resets bound to argument {}", r.arg)));
        }
        seen_arg.push(r.arg);
    }
    for r in &c.out_resets {
        check(r.name)?;
        pidx(r.port, "output reset")?;
        if c.ports[r.port as usize].dir != BviDir::Output
            || c.ports[r.port as usize].kind != BviPortKind::Reset
        {
            return Err(bad(
                "out_resets index must name an Output port of kind Reset".into(),
            ));
        }
    }
    for cl in &c.out_clocks {
        check(cl.name)?;
        pidx(cl.port, "output clock")?;
        if c.ports[cl.port as usize].dir != BviDir::Output
            || c.ports[cl.port as usize].kind != BviPortKind::Clock
        {
            return Err(bad(
                "out_clocks index must name an Output port of kind Clock".into(),
            ));
        }
    }
    for &(from, to) in &c.paths {
        pidx(from, "path source")?;
        pidx(to, "path target")?;
        if c.ports[from as usize].dir != BviDir::Input || c.ports[to as usize].dir != BviDir::Output
        {
            return Err(bad("path must run input -> output".into()));
        }
    }
    let check_val = |v: &crate::bvi::BviParamValue| -> Result<(), VerifyError> {
        match v {
            crate::bvi::BviParamValue::Bits { hex, .. } => check(*hex),
            crate::bvi::BviParamValue::Str(s) => check(*s),
            _ => Ok(()),
        }
    };
    for prm in &c.params {
        check(prm.name)?;
        check_val(&prm.value)?;
    }
    for (pi, v) in &c.const_args {
        pidx(*pi, "const arg")?;
        if c.ports[*pi as usize].kind != BviPortKind::ConstArg {
            return Err(bad("const_args index does not name a ConstArg port".into()));
        }
        check_val(v)?;
    }
    Ok(())
}
