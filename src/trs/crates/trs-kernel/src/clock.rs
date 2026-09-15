//! Clock state — the `tClockInfo` analogue (`kernel.h:92`).
//!
//! A clock is either *periodic* (a waveform laid out as events at
//! definition/alteration time) or *aperiodic* (`period == 0`), in which case
//! edges are injected by clock-generating primitives (ClockGen, GatedClock,
//! ClockDivider, MakeClock) — the `bk_trigger_clock_edge` protocol.

use crate::Time;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ClockHandle(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EdgeDirection {
    Rising,
    Falling,
}

#[derive(Debug, Clone)]
pub struct Clock {
    pub name: String,
    pub current_value: bool,
    pub initial_value: Option<bool>,
    /// Waveform; all zero for aperiodic clocks.
    pub initial_delay: Time,
    pub low_phase: Time,
    pub high_phase: Time,
    /// Bookkeeping read by the schedule and the waveform time-correction.
    pub last_posedge_at: Option<Time>,
    pub last_negedge_at: Option<Time>,
    /// Time of the edge *before* the most recent one — combinational values
    /// computed at edge N are displayed at this time (`combinational_at`).
    pub combinational_at: Option<Time>,
    pub posedge_count: u64,
    pub negedge_count: u64,
    /// Edge limits for `step`-style driver control (`bk_quit_after_edge`).
    pub posedge_limit: Option<u64>,
    pub negedge_limit: Option<u64>,
}

impl Clock {
    pub fn aperiodic(name: impl Into<String>) -> Clock {
        Clock {
            name: name.into(),
            current_value: false,
            initial_value: None,
            initial_delay: 0,
            low_phase: 0,
            high_phase: 0,
            last_posedge_at: None,
            last_negedge_at: None,
            combinational_at: None,
            posedge_count: 0,
            negedge_count: 0,
            posedge_limit: None,
            negedge_limit: None,
        }
    }

    pub fn periodic(
        name: impl Into<String>,
        initial_value: bool,
        initial_delay: Time,
        low_phase: Time,
        high_phase: Time,
    ) -> Clock {
        Clock {
            initial_value: Some(initial_value),
            initial_delay,
            low_phase,
            high_phase,
            ..Clock::aperiodic(name)
        }
    }

    pub fn period(&self) -> Time {
        self.low_phase + self.high_phase
    }

    pub fn is_periodic(&self) -> bool {
        self.period() != 0
    }

    /// Record an edge occurring now; returns the edge count in this
    /// direction.  Updates `combinational_at` to the previous edge time,
    /// which is what waveform time-correction consumes.
    pub fn record_edge(&mut self, dir: EdgeDirection, now: Time) -> u64 {
        let prev = std::cmp::max(self.last_posedge_at, self.last_negedge_at);
        self.combinational_at = prev;
        self.current_value = dir == EdgeDirection::Rising;
        match dir {
            EdgeDirection::Rising => {
                self.last_posedge_at = Some(now);
                self.posedge_count += 1;
                self.posedge_count
            }
            EdgeDirection::Falling => {
                self.last_negedge_at = Some(now);
                self.negedge_count += 1;
                self.negedge_count
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn edges_update_combinational_time() {
        let mut c = Clock::periodic("CLK", false, 0, 5, 5);
        assert_eq!(c.period(), 10);
        c.record_edge(EdgeDirection::Rising, 5);
        assert_eq!(c.combinational_at, None);
        c.record_edge(EdgeDirection::Falling, 10);
        assert_eq!(c.combinational_at, Some(5));
        c.record_edge(EdgeDirection::Rising, 15);
        assert_eq!(c.combinational_at, Some(10));
        assert_eq!(c.posedge_count, 2);
        assert!(c.current_value);
    }
}
