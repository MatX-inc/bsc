//! Decode-time validation: every reference resolves, the schedule mentions
//! only known rules, widths are sane.  This is the guard that makes BIR a
//! contract rather than a hope (DESIGN.md §3.1).

use crate::{Design, GlobalStrId};

#[derive(Debug)]
pub enum VerifyError {
    BadStringRef { at: usize, len: usize },
    NoTopModule { top: String },
    DuplicateModule { name: String },
}

impl std::fmt::Display for VerifyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VerifyError::BadStringRef { at, len } => {
                write!(f, "string id {at} out of range (table has {len} entries)")
            }
            VerifyError::NoTopModule { top } => {
                write!(f, "top module {top:?} not present in module list")
            }
            VerifyError::DuplicateModule { name } => {
                write!(f, "module {name:?} defined more than once")
            }
        }
    }
}

impl std::error::Error for VerifyError {}

pub fn verify(design: &Design) -> Result<(), VerifyError> {
    let nstrings = design.strings.len();
    let check = |id: GlobalStrId| -> Result<(), VerifyError> {
        if id.idx() < nstrings {
            Ok(())
        } else {
            Err(VerifyError::BadStringRef { at: id.idx(), len: nstrings })
        }
    };

    check(design.top)?;
    let mut seen = std::collections::HashSet::new();
    for m in &design.modules {
        check(m.name)?;
        if !seen.insert(m.name) {
            return Err(VerifyError::DuplicateModule {
                name: design.global_name(m.name).to_string(),
            });
        }
    }
    if !seen.contains(&design.top) {
        return Err(VerifyError::NoTopModule {
            top: design.global_name(design.top).to_string(),
        });
    }
    // TODO(P0): resolve every Def/Port/Param/instance/method reference in
    // exprs, actions, and schedules; check width consistency; check that
    // every rule's can_fire/will_fire defs exist and are so flagged.
    Ok(())
}
