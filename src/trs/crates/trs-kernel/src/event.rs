//! The event queue: a min-heap over `(time, priority)`, semantics ported
//! from `src/bluesim/event_queue.cxx`.
//!
//! Differences from the C++ version are implementation-only: handlers are
//! enum variants dispatched by the kernel loop rather than raw function
//! pointers, which keeps the hot path branch-predictable and lets the
//! borrow checker see through the dispatch.

use std::cmp::Ordering;
use std::collections::BinaryHeap;

use crate::priority::Priority;
use crate::{ClockHandle, Time};

/// What an event does when it fires.  The kernel loop interprets these;
/// codegen'd schedule functions are called for `ClockEdge`/`AfterEdge`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EventKind {
    /// A clock edge: run the (clock, direction) schedule function, advance
    /// counters, reschedule at +period for periodic clocks.
    ClockEdge { clock: ClockHandle, rising: bool },
    /// The after-edge ("combinational") function for clock-crossing rules.
    AfterEdge { clock: ClockHandle, rising: bool },
    /// Flush pending waveform changes for this timeslice.
    Wave,
    /// Assert/deassert the model reset.
    Reset { asserted: bool },
    /// Return control to the driver (UI/bluetcl yield).
    Yield,
    /// Drain the queue and stop.
    Quit,
}

#[derive(Debug, Clone, Copy)]
pub struct Event {
    pub at: Time,
    pub priority: Priority,
    pub kind: EventKind,
    /// Reschedule interval: fire again at `at + period` (0 = one-shot),
    /// as `tEventFn`'s return value does in the C++ kernel.
    pub period: Time,
}

impl PartialEq for Event {
    fn eq(&self, other: &Self) -> bool {
        self.at == other.at && self.priority == other.priority
    }
}
impl Eq for Event {}

impl PartialOrd for Event {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// `event_queue.cxx:18`: order by time, then priority.  Reversed here
/// because `BinaryHeap` is a max-heap.
impl Ord for Event {
    fn cmp(&self, other: &Self) -> Ordering {
        (other.at, other.priority).cmp(&(self.at, self.priority))
    }
}

#[derive(Debug, Default)]
pub struct EventQueue {
    heap: BinaryHeap<Event>,
}

impl EventQueue {
    pub fn new() -> EventQueue {
        EventQueue::default()
    }

    pub fn schedule(&mut self, ev: Event) {
        self.heap.push(ev);
    }

    /// Pop the next event.  The caller (kernel loop) executes it and, if
    /// `period != 0`, re-schedules it — mirroring `EventQueue::execute`.
    pub fn pop(&mut self) -> Option<Event> {
        self.heap.pop()
    }

    pub fn peek(&self) -> Option<&Event> {
        self.heap.peek()
    }

    pub fn is_empty(&self) -> bool {
        self.heap.is_empty()
    }

    pub fn len(&self) -> usize {
        self.heap.len()
    }

    /// Remove all events matching `pred` (linear scan, as the C++ kernel's
    /// `find`/`remove` are; used for canceling VCD/yield/clock events).
    pub fn remove_where(&mut self, pred: impl Fn(&Event) -> bool) {
        let kept: Vec<Event> = self.heap.drain().filter(|e| !pred(e)).collect();
        self.heap = kept.into();
    }

    pub fn clear(&mut self) {
        self.heap.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::priority::{Group, Slot};

    fn ev(at: Time, group: Group, slot: Slot, clock: u32, kind: EventKind) -> Event {
        Event { at, priority: Priority::new(group, slot, clock), kind, period: 0 }
    }

    #[test]
    fn time_then_priority_ordering() {
        let mut q = EventQueue::new();
        // Insert out of order: a later edge, this timeslice's VCD flush,
        // this timeslice's edge, and an after-edge combo event.
        q.schedule(ev(20, Group::Logic, Slot::Execute, 0, EventKind::ClockEdge { clock: ClockHandle(0), rising: true }));
        q.schedule(ev(10, Group::AfterLogic, Slot::Vcd, 0, EventKind::Wave));
        q.schedule(ev(10, Group::Logic, Slot::Execute, 0, EventKind::ClockEdge { clock: ClockHandle(0), rising: true }));
        q.schedule(ev(10, Group::Final, Slot::Combinational, 0, EventKind::AfterEdge { clock: ClockHandle(0), rising: true }));

        let order: Vec<(Time, EventKind)> = std::iter::from_fn(|| q.pop()).map(|e| (e.at, e.kind)).collect();
        assert!(matches!(order[0], (10, EventKind::ClockEdge { .. })));
        assert!(matches!(order[1], (10, EventKind::Wave)));
        assert!(matches!(order[2], (10, EventKind::AfterEdge { .. })));
        assert!(matches!(order[3], (20, EventKind::ClockEdge { .. })));
    }

    #[test]
    fn simultaneous_edges_order_by_clock_number() {
        let mut q = EventQueue::new();
        q.schedule(ev(5, Group::Logic, Slot::Execute, 1, EventKind::ClockEdge { clock: ClockHandle(1), rising: true }));
        q.schedule(ev(5, Group::Logic, Slot::Execute, 0, EventKind::ClockEdge { clock: ClockHandle(0), rising: true }));
        let first = q.pop().unwrap();
        assert!(matches!(first.kind, EventKind::ClockEdge { clock: ClockHandle(0), .. }));
    }

    #[test]
    fn remove_where_cancels_events() {
        let mut q = EventQueue::new();
        q.schedule(ev(5, Group::AfterLogic, Slot::Vcd, 0, EventKind::Wave));
        q.schedule(ev(5, Group::Logic, Slot::Execute, 0, EventKind::ClockEdge { clock: ClockHandle(0), rising: true }));
        q.remove_where(|e| matches!(e.kind, EventKind::Wave));
        assert_eq!(q.len(), 1);
        assert!(matches!(q.pop().unwrap().kind, EventKind::ClockEdge { .. }));
    }
}
