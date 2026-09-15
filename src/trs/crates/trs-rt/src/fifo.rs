//! FIFO primitive — semantics ported from `bs_prim_mod_fifo.h`.
//!
//! Three variants (`bs_prim_mod_fifo.h:11-26`):
//! - simple:  enq CF deq when neither full nor empty; enq conflicts with
//!            itself, deq with itself.
//! - loopy:   `deq < enq` legal even when full at cycle start (mkLFIFO).
//! - bypass:  `enq < deq` legal even when empty at cycle start.
//!
//! The C++ implementation tags mutations with `bk_now()` timestamps to
//! detect "first mutation this instant" from arbitrary call orders.  Here
//! the kernel's cycle stamp is passed in by codegen (it is a compile-time
//! constant expression per edge function), keeping the primitive free of
//! global state.

/// Element type is erased to u64 limbs at the ABI boundary for narrow data;
/// this generic form is the in-crate reference implementation the
/// monomorphized `extern "C"` shims (codegen targets) wrap.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FifoKind {
    Simple,
    Loopy,
    Bypass,
}

#[derive(Debug)]
pub struct Fifo<T> {
    kind: FifoKind,
    guarded: bool,
    data: Vec<Option<T>>,
    head: usize,
    len: usize,
    /// Cycle stamp of the last mutation, and the element count at the start
    /// of that cycle (`saved_elems` in the C++).
    stamp: u64,
    saved_len: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub enum FifoError {
    EnqFull,
    DeqEmpty,
}

impl<T: Clone> Fifo<T> {
    pub fn new(kind: FifoKind, guarded: bool, depth: usize) -> Fifo<T> {
        Fifo {
            kind,
            guarded,
            data: vec![None; depth.max(1)],
            head: 0,
            len: 0,
            stamp: u64::MAX,
            saved_len: 0,
        }
    }

    fn snapshot(&mut self, now: u64) {
        if self.stamp != now {
            self.stamp = now;
            self.saved_len = self.len;
        }
    }

    /// Begin-of-cycle count, for the CF status methods (`i_notEmpty`,
    /// `i_notFull`, `bs_prim_mod_fifo.h:181-208`).
    fn cycle_start_len(&self, now: u64) -> usize {
        if self.stamp == now {
            self.saved_len
        } else {
            self.len
        }
    }

    pub fn capacity(&self) -> usize {
        self.data.len()
    }

    /// Live count — the `notFull`/`notEmpty` (SB-ordered) view.
    pub fn not_empty(&self) -> bool {
        self.len > 0
    }

    pub fn not_full(&self) -> bool {
        self.len < self.capacity()
    }

    /// Conflict-free status views: report against the cycle-start count.
    pub fn i_not_empty(&self, now: u64) -> bool {
        self.cycle_start_len(now) > 0
    }

    pub fn i_not_full(&self, now: u64) -> bool {
        self.cycle_start_len(now) < self.capacity()
    }

    pub fn first(&self) -> Option<&T> {
        self.data[self.head].as_ref()
    }

    pub fn enq(&mut self, x: T, now: u64) -> Result<(), FifoError> {
        self.snapshot(now);
        // Guarded non-loopy FIFOs judge fullness against the cycle-start
        // count: enq is CF with deq, so a deq earlier in this cycle must not
        // make an illegal enq legal (`bs_prim_mod_fifo.h:122-150`).  Loopy
        // FIFOs are the exception: deq < enq is a legal sequence.  These are
        // guard violations the scheduler normally prevents — an error here
        // is a bug in schedule/codegen, not in the design.
        let full = if self.guarded && self.kind != FifoKind::Loopy {
            self.cycle_start_len(now) >= self.capacity()
        } else {
            self.len >= self.capacity()
        };
        if full || self.len >= self.capacity() {
            return Err(FifoError::EnqFull);
        }
        let tail = (self.head + self.len) % self.capacity();
        self.data[tail] = Some(x);
        self.len += 1;
        Ok(())
    }

    pub fn deq(&mut self, now: u64) -> Result<T, FifoError> {
        self.snapshot(now);
        if self.len == 0 {
            return Err(FifoError::DeqEmpty);
        }
        let x = self.data[self.head].take().expect("occupied slot");
        self.head = (self.head + 1) % self.capacity();
        self.len -= 1;
        Ok(x)
    }

    pub fn clear(&mut self, now: u64) {
        self.snapshot(now);
        self.data.iter_mut().for_each(|s| *s = None);
        self.head = 0;
        self.len = 0;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn same_cycle_deq_then_enq_loopy() {
        // Pipeline FIFO (loopy) of depth 1: full at cycle start, yet
        // deq < enq lets both fire in one cycle (`FIFO_LOOPY`).
        let mut f: Fifo<u32> = Fifo::new(FifoKind::Loopy, true, 1);
        f.enq(1, 100).unwrap();
        // cycle 101: deq then enq (deq < enq)
        assert_eq!(f.deq(101).unwrap(), 1);
        f.enq(2, 101).unwrap();
        // The CF status views report the cycle-start state...
        assert!(f.i_not_empty(101));
        assert_eq!(f.cycle_start_len(101), 1);
        // ...while the live views see the result of both.
        assert!(f.not_empty());
        assert_eq!(f.first(), Some(&2));
    }

    #[test]
    fn simple_guarded_enq_judged_at_cycle_start() {
        // A *simple* guarded FIFO's enq is CF with deq: fullness is judged
        // at cycle start, so deq-then-enq on a full FIFO is still an error.
        let mut f: Fifo<u32> = Fifo::new(FifoKind::Simple, true, 1);
        f.enq(1, 100).unwrap();
        assert_eq!(f.deq(101).unwrap(), 1);
        assert_eq!(f.enq(2, 101), Err(FifoError::EnqFull));
    }

    #[test]
    fn bypass_enq_then_deq_when_empty() {
        let mut f: Fifo<u32> = Fifo::new(FifoKind::Bypass, true, 1);
        // Empty at cycle start; bypass allows enq < deq in one cycle.
        f.enq(7, 5).unwrap();
        assert_eq!(f.deq(5).unwrap(), 7);
        assert!(!f.not_empty());
    }

    #[test]
    fn guarded_deq_empty_is_error() {
        let mut f: Fifo<u32> = Fifo::new(FifoKind::Simple, true, 2);
        assert_eq!(f.deq(0), Err(FifoError::DeqEmpty));
    }

    #[test]
    fn wraparound() {
        let mut f: Fifo<u32> = Fifo::new(FifoKind::Simple, true, 2);
        for cycle in 0..10u64 {
            f.enq(cycle as u32, cycle).unwrap();
            assert_eq!(f.deq(cycle + 100).unwrap(), cycle as u32);
        }
    }
}
