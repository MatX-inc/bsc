//! Intra-timeslice event ordering — ported from `src/bluesim/priority.cxx`:
//!
//! ```text
//! priority = (group << 28) | ((slot & 0xF) << 24) | (clock & 0x00FFFFFF)
//! ```
//!
//! Groups order the phases within one time step; slots order event types
//! within a group; the clock number is the final tiebreaker so simultaneous
//! edges of different clocks order deterministically.

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Priority(pub u32);

/// Phase groups within a timeslice (`priority.h:18`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u32)]
pub enum Group {
    Initial = 0,
    BeforeLogic = 1,
    Logic = 2,
    AfterLogic = 3,
    Final = 4,
}

/// Event-type slots within a group (`priority.h:26`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u32)]
pub enum Slot {
    Reset = 0,
    Ui = 1,
    CycleDump = 2,
    Vcd = 3,
    Execute = 4,
    RuleDump = 5,
    StateDump = 6,
    Combinational = 7,
}

impl Priority {
    pub fn new(group: Group, slot: Slot, clock: u32) -> Priority {
        Priority((group as u32) << 28 | (slot as u32 & 0xF) << 24 | (clock & 0x00FF_FFFF))
    }

    pub fn group(self) -> u32 {
        self.0 >> 28
    }

    pub fn slot(self) -> u32 {
        (self.0 >> 24) & 0xF
    }

    pub fn clock(self) -> u32 {
        self.0 & 0x00FF_FFFF
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn packing_matches_bluesim() {
        // Spot-check against the C++ formula.
        let p = Priority::new(Group::Logic, Slot::Execute, 3);
        assert_eq!(p.0, (2 << 28) | (4 << 24) | 3);
        assert_eq!(p.group(), 2);
        assert_eq!(p.slot(), 4);
        assert_eq!(p.clock(), 3);
    }

    #[test]
    fn phase_ordering() {
        let edge = Priority::new(Group::Logic, Slot::Execute, 0);
        let vcd = Priority::new(Group::AfterLogic, Slot::Vcd, 0);
        let combo = Priority::new(Group::Final, Slot::Combinational, 0);
        let ui = Priority::new(Group::Final, Slot::Ui, 0);
        // Edge logic before VCD before after-edge combinational; UI yield
        // ahead of combinational within FINAL (slot order).
        assert!(edge < vcd);
        assert!(vcd < combo);
        assert!(ui < combo);
    }

    #[test]
    fn clock_number_breaks_ties() {
        let c0 = Priority::new(Group::Logic, Slot::Execute, 0);
        let c1 = Priority::new(Group::Logic, Slot::Execute, 1);
        assert!(c0 < c1);
    }
}
