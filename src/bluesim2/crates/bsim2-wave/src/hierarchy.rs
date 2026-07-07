//! Design hierarchy for waveform headers.
//!
//! Both writers consume this; it carries what today's `dump_VCD_defs`
//! reconstructs by walking live module objects — plus the **module
//! definition name** per scope, which VCD cannot express natively (we emit
//! it as a comment) but FST records first-class, so viewers can group
//! instances of the same module.

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SignalId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarKind {
    /// Register state.
    Reg,
    /// Combinational def / wire.
    Wire,
    /// A rule's CAN_FIRE/WILL_FIRE (emitted under -keep-fires).
    Fire,
    /// A clock oscillator.
    Clock,
}

#[derive(Debug, Clone)]
pub struct SignalDef {
    pub id: SignalId,
    pub name: String,
    pub width: u32,
    pub kind: VarKind,
    /// Clock(s) whose `combinational_at` governs this signal's displayed
    /// change time; empty = changes display at the current time.
    pub clocks: Vec<u32>,
}

#[derive(Debug, Clone)]
pub struct Scope {
    /// Instance name (e.g. "fifo1").
    pub instance: String,
    /// Defining BSV module name (e.g. "mkSizedFIFO") — module definition
    /// information for the wave file.
    pub module: String,
    pub signals: Vec<SignalDef>,
    pub children: Vec<Scope>,
}

#[derive(Debug, Clone)]
pub struct Hierarchy {
    pub top: Scope,
    pub timescale_exp: i8,
    /// Total number of signal ids allocated (ids are dense, 0..count).
    pub signal_count: u32,
}

impl Hierarchy {
    /// Depth-first walk over all scopes.
    pub fn visit_scopes<'a>(&'a self, f: &mut impl FnMut(&'a Scope, usize)) {
        fn go<'a>(s: &'a Scope, depth: usize, f: &mut impl FnMut(&'a Scope, usize)) {
            f(s, depth);
            for c in &s.children {
                go(c, depth + 1, f);
            }
        }
        go(&self.top, 0, f);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn walk_visits_all_scopes_in_order() {
        let h = Hierarchy {
            top: Scope {
                instance: "top".into(),
                module: "mkTop".into(),
                signals: vec![],
                children: vec![
                    Scope {
                        instance: "a".into(),
                        module: "mkA".into(),
                        signals: vec![],
                        children: vec![],
                    },
                    Scope {
                        instance: "b".into(),
                        module: "mkA".into(),
                        signals: vec![],
                        children: vec![],
                    },
                ],
            },
            timescale_exp: -12,
            signal_count: 0,
        };
        let mut seen = Vec::new();
        h.visit_scopes(&mut |s, d| seen.push((s.instance.clone(), d)));
        assert_eq!(
            seen,
            vec![("top".into(), 0), ("a".into(), 1), ("b".into(), 1)]
        );
    }
}
