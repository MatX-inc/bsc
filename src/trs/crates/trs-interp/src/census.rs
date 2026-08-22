//! Activity census (TRS_ACTIVITY_CENSUS=1): a measurement instrument
//! for the activity-gating rung, NOT a product feature.  It rides the
//! interp walk and answers, from ground truth, three questions per
//! rule: how often does it fire, how often are its schedule cone's
//! inputs untouched since the last edge (= the cone would be skippable
//! under a sensitivity mask), and — the soundness tripwire — does a
//! "skippable" cone's freshly recomputed CF/WF ever differ from its
//! previous value (which would mean the captured read set has a hole).
//!
//! Read sets are captured DYNAMICALLY: every prim value read and every
//! latched-def read that happens while a rule's CF/WF cone is being
//! latched is attributed to that rule and unioned over the run.  The
//! per-cycle skip simulation replays the edge's latch events in order,
//! propagating dirtiness through latched-def chains (a rule reading a
//! dirty rule's CF is itself dirty), exactly as the compiled mask
//! scheme would.
//!
//! Dirty sources per cycle: instances whose architectural scalar state
//! changed at the previous edge (hash-compared over prim_state_children,
//! candidates limited to instances that received an action call), any
//! instance with ranged (memory) state that was written (conservative:
//! always counted changed), and transient (wire) instances written this
//! cycle (conservative full-cycle set — order within the edge ignored,
//! which can only under-count skips, never fake soundness).

use crate::prim::PrimSym;
use crate::value::Value;
use std::collections::{HashMap, HashSet};
use trs_ir::StrId;

enum Ev {
    /// an entry's eager schedule-position defs ran (key = rci<<32|ei)
    Eager(u64),
    /// one latch_rule completed: dense rule id + fresh CF/WF
    Latch { r: usize, cf: bool, wf: bool },
}

#[derive(Default)]
struct RuleStat {
    n: u64,
    fires: u64,
    skippable: u64,
    violations: u64,
    first_viol: Option<u64>,
    read_events: u64,
    latches: u64,
}

pub(crate) struct Census {
    from: u64,
    out: Option<String>,
    // identity
    rule_ids: HashMap<(usize, StrId), usize>,
    rules: Vec<(usize, StrId, StrId, StrId)>, // (inst, rule, cf def, wf def)
    // capture state
    cur_rule: Option<usize>,
    cur_exec: Option<usize>,
    eager_on: bool,
    cur_entry: u64,
    events: Vec<Ev>,
    written: HashSet<usize>,
    // unions
    reads: Vec<HashSet<usize>>,
    lreads: Vec<HashSet<(usize, StrId)>>,
    eager_reads: HashMap<u64, HashSet<usize>>,
    eager_lreads: HashMap<u64, HashSet<(usize, StrId)>>,
    eager_defs: HashMap<u64, HashSet<(usize, StrId)>>,
    /// def keys latched during EXEC (method ENs, task cookies) with a
    /// value fingerprint, this cycle and last: a sched read of such a
    /// key is dirty when the key's presence or value moved between
    /// cycles (writers always exec before a latched read can happen —
    /// the latch map clears per edge — so a same-value latch is
    /// invisible to readers)
    exec_now: HashMap<(usize, StrId), u64>,
    exec_prev: HashMap<(usize, StrId), u64>,
    entry_of: Vec<u64>,
    // change detection
    scal: HashMap<(usize, &'static str), Option<Value>>,
    changed_prev: HashSet<usize>,
    /// transients written LAST cycle: a wire un-written this cycle
    /// reverts to its default — that transition is a change too
    transients_prev: HashSet<usize>,
    kind: HashMap<usize, u8>, // 0 scalar, 1 transient, 2 has-range
    change_count: HashMap<usize, u64>,
    // stats
    stats: Vec<RuleStat>,
    prev_cfwf: Vec<Option<(bool, bool)>>,
    cycles_seen: u64,
    cycles_sim: u64,
    union_growth_after_from: u64,
    reported: bool,
}

impl Census {
    pub(crate) fn from_env() -> Option<Box<Census>> {
        std::env::var_os("TRS_ACTIVITY_CENSUS")?;
        let from = std::env::var("TRS_ACTIVITY_CENSUS_FROM")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(2000);
        let out = std::env::var("TRS_ACTIVITY_CENSUS_OUT").ok();
        Some(Box::new(Census {
            from,
            out,
            rule_ids: HashMap::new(),
            rules: Vec::new(),
            cur_rule: None,
            cur_exec: None,
            eager_on: false,
            cur_entry: 0,
            events: Vec::new(),
            written: HashSet::new(),
            reads: Vec::new(),
            lreads: Vec::new(),
            eager_reads: HashMap::new(),
            eager_lreads: HashMap::new(),
            eager_defs: HashMap::new(),
            exec_now: HashMap::new(),
            exec_prev: HashMap::new(),
            entry_of: Vec::new(),
            scal: HashMap::new(),
            changed_prev: HashSet::new(),
            transients_prev: HashSet::new(),
            kind: HashMap::new(),
            change_count: HashMap::new(),
            stats: Vec::new(),
            prev_cfwf: Vec::new(),
            cycles_seen: 0,
            cycles_sim: 0,
            union_growth_after_from: 0,
            reported: false,
        }))
    }

    // ---- capture hooks ----

    pub(crate) fn begin_entry(&mut self, rci: usize, ei: usize) {
        self.cur_entry = ((rci as u64) << 32) | ei as u64;
    }
    pub(crate) fn begin_eager(&mut self) {
        self.eager_on = true;
        self.events.push(Ev::Eager(self.cur_entry));
    }
    pub(crate) fn end_eager(&mut self) {
        self.eager_on = false;
    }
    pub(crate) fn eager_def(&mut self, inst: usize, name: StrId) {
        self.eager_defs
            .entry(self.cur_entry)
            .or_default()
            .insert((inst, name));
    }

    pub(crate) fn begin_latch(
        &mut self,
        inst: usize,
        rule: StrId,
        cf: StrId,
        wf: StrId,
    ) {
        let next = self.rules.len();
        let id = *self.rule_ids.entry((inst, rule)).or_insert(next);
        if id == next {
            self.rules.push((inst, rule, cf, wf));
            self.reads.push(HashSet::new());
            self.lreads.push(HashSet::new());
            self.stats.push(RuleStat::default());
            self.prev_cfwf.push(None);
            self.entry_of.push(self.cur_entry);
        }
        self.entry_of[id] = self.cur_entry;
        self.stats[id].latches += 1;
        self.cur_rule = Some(id);
    }
    pub(crate) fn end_latch(&mut self, cf: bool, wf: bool) {
        if let Some(r) = self.cur_rule.take() {
            self.events.push(Ev::Latch { r, cf, wf });
        }
    }

    /// prim value read (call_value on a prim instance)
    pub(crate) fn read(&mut self, callee: usize) {
        if let Some(r) = self.cur_rule {
            self.stats[r].read_events += 1;
            if self.reads[r].insert(callee) && self.cycles_seen >= self.from {
                self.union_growth_after_from += 1;
            }
        } else if self.eager_on {
            self.eager_reads
                .entry(self.cur_entry)
                .or_default()
                .insert(callee);
        }
    }
    /// latched-def read (Def/EN/TaskValue served from the latch map)
    pub(crate) fn lread(&mut self, inst: usize, name: StrId) {
        if let Some(r) = self.cur_rule {
            if self.lreads[r].insert((inst, name))
                && self.cycles_seen >= self.from
            {
                self.union_growth_after_from += 1;
            }
        } else if self.eager_on {
            self.eager_lreads
                .entry(self.cur_entry)
                .or_default()
                .insert((inst, name));
        }
    }
    /// prim action call (state mutation candidate this cycle)
    pub(crate) fn write(&mut self, callee: usize) {
        self.written.insert(callee);
    }

    pub(crate) fn begin_exec(&mut self, inst: usize, rule: StrId) {
        // the rule was interned by its own latch earlier this edge
        self.cur_exec = self.rule_ids.get(&(inst, rule)).copied();
    }
    pub(crate) fn end_exec(&mut self) {
        self.cur_exec = None;
    }
    /// a def key latched during exec (method EN, task cookie)
    pub(crate) fn exec_latch(&mut self, inst: usize, name: StrId, v: &Value) {
        if self.cur_exec.is_none() {
            return; // sched/eager latches are modeled by their own events
        }
        let mut h: u64 = 0xcbf29ce484222325;
        for l in v.limbs64() {
            h = (h ^ l).wrapping_mul(0x100000001b3);
        }
        self.exec_now.insert((inst, name), h);
    }

    pub(crate) fn active_capture(&self) -> bool {
        self.cur_rule.is_some() || self.eager_on
    }

    // ---- end-of-cycle: classify, simulate, roll ----

    pub(crate) fn end_of_cycle(&mut self, it: &mut crate::Interp) {
        self.cycles_seen = it.cycle;
        let sim = it.cycle >= self.from;
        if sim {
            self.cycles_sim += 1;
        }

        // 1. classify this cycle's written instances
        let written: Vec<usize> = self.written.drain().collect();
        let mut transients_w: HashSet<usize> = HashSet::new();
        let mut state_w: Vec<usize> = Vec::new();
        for i in written {
            let k = *self.kind.entry(i).or_insert_with(|| {
                match &it.insts[i].kind {
                    crate::InstKind::Prim(p) => {
                        if p.sym_transient() {
                            1
                        } else if p.sym_bypass() {
                            3
                        } else if p
                            .state_children()
                            .iter()
                            .any(|ps: &PrimSym| ps.range.is_some())
                        {
                            2
                        } else {
                            0
                        }
                    }
                    _ => 0,
                }
            });
            match k {
                1 => {
                    transients_w.insert(i);
                }
                3 => {
                    // same-cycle-visible state (CReg, loopy/bypass
                    // FIFO): a write dirties readers THIS cycle like a
                    // wire, and next cycle via change detection
                    transients_w.insert(i);
                    state_w.push(i);
                }
                _ => state_w.push(i),
            }
        }
        // a wire's revert-to-default when its writer stops is a change:
        // dirty = written this cycle OR last cycle (conservative — a
        // same-value rewrite still counts dirty)
        let dirty_wires: HashSet<usize> = transients_w
            .union(&self.transients_prev)
            .copied()
            .collect();

        // 2. replay the edge's latch events in order (mask simulation)
        let events = std::mem::take(&mut self.events);
        let mut dirty_defs: HashSet<(usize, StrId)> = HashSet::new();
        // exec-latched keys whose presence or value moved vs last cycle
        let mut exec_dirty: HashSet<(usize, StrId)> = HashSet::new();
        for (k, h) in &self.exec_now {
            if self.exec_prev.get(k) != Some(h) {
                exec_dirty.insert(*k);
            }
        }
        for k in self.exec_prev.keys() {
            if !self.exec_now.contains_key(k) {
                exec_dirty.insert(*k);
            }
        }
        for ev in &events {
            match ev {
                Ev::Eager(e) => {
                    let er = self.eager_reads.get(e);
                    let el = self.eager_lreads.get(e);
                    let dirty = er.is_some_and(|s| {
                        s.iter().any(|i| {
                            self.changed_prev.contains(i)
                                || dirty_wires.contains(i)
                        })
                    }) || el.is_some_and(|s| {
                        s.iter().any(|k| {
                            dirty_defs.contains(k) || exec_dirty.contains(k)
                        })
                    });
                    if dirty {
                        if let Some(defs) = self.eager_defs.get(e) {
                            for k in defs {
                                dirty_defs.insert(*k);
                            }
                        }
                    }
                }
                Ev::Latch { r, cf, wf } => {
                    let r = *r;
                    let dirty = self.reads[r].iter().any(|i| {
                        self.changed_prev.contains(i)
                            || dirty_wires.contains(i)
                    }) || self.lreads[r].iter().any(|k| {
                        dirty_defs.contains(k) || exec_dirty.contains(k)
                    }) || {
                            // eager defs of this rule's entry dirty =
                            // schedule-position inputs moved
                            let e = self.entry_of[r];
                            self.eager_defs.get(&e).is_some_and(|defs| {
                                defs.iter().any(|k| dirty_defs.contains(k))
                            })
                        };
                    let prev = self.prev_cfwf[r];
                    let mut mark_dirty = dirty;
                    if sim {
                        let st = &mut self.stats[r];
                        st.n += 1;
                        if *wf {
                            st.fires += 1;
                        }
                        if !dirty {
                            match prev {
                                Some((pc, pw)) if pc == *cf && pw == *wf => {
                                    st.skippable += 1;
                                }
                                Some(_) => {
                                    // TRIPWIRE: inputs "clean" but the
                                    // ground-truth recompute moved — the
                                    // captured read set has a hole
                                    st.violations += 1;
                                    if st.first_viol.is_none() {
                                        st.first_viol = Some(it.cycle);
                                    }
                                    mark_dirty = true;
                                }
                                None => {
                                    // first observation: not skippable,
                                    // not a violation
                                    mark_dirty = true;
                                }
                            }
                        }
                    }
                    if mark_dirty {
                        let (inst, _, cfn, wfn) = self.rules[r];
                        dirty_defs.insert((inst, cfn));
                        dirty_defs.insert((inst, wfn));
                    }
                    self.prev_cfwf[r] = Some((*cf, *wf));
                }
            }
        }

        // 3. change detection over this cycle's written state instances
        let mut changed_new: HashSet<usize> = HashSet::new();
        for i in state_w {
            if self.kind.get(&i) == Some(&2) {
                // memory-carrying prim written: conservatively changed
                changed_new.insert(i);
                *self.change_count.entry(i).or_insert(0) += 1;
                continue;
            }
            let children = it.prim_state_children(i);
            let mut ch = false;
            for ps in &children {
                if ps.range.is_some() {
                    continue;
                }
                let v = it.prim_sym_read(i, ps.key);
                match self.scal.get(&(i, ps.key)) {
                    Some(old) if *old == v => {}
                    _ => {
                        ch = true;
                        self.scal.insert((i, ps.key), v);
                    }
                }
            }
            if ch {
                changed_new.insert(i);
                *self.change_count.entry(i).or_insert(0) += 1;
            }
        }
        self.changed_prev = changed_new;
        self.transients_prev = transients_w;
        self.exec_prev = std::mem::take(&mut self.exec_now);
    }

    // ---- report ----

    pub(crate) fn report(&mut self, it: &crate::Interp) {
        if self.reported || self.rules.is_empty() {
            return;
        }
        self.reported = true;
        let mut lines = Vec::new();
        let (mut tn, mut tf, mut ts, mut tv) = (0u64, 0u64, 0u64, 0u64);
        let (mut wn, mut wsk) = (0f64, 0f64);
        let mut rows: Vec<usize> = (0..self.rules.len()).collect();
        rows.sort_by_key(|&r| std::cmp::Reverse(self.stats[r].read_events));
        for &r in &rows {
            let (inst, rname, _, _) = self.rules[r];
            let st = &self.stats[r];
            tn += st.n;
            tf += st.fires;
            ts += st.skippable;
            tv += st.violations;
            let mass = if st.latches > 0 {
                st.read_events as f64 / st.latches as f64
            } else {
                0.0
            };
            wn += st.n as f64 * mass;
            wsk += st.skippable as f64 * mass;
            lines.push(format!(
                "{{\"rule\":\"{}.{}\",\"n\":{},\"fires\":{},\"skippable\":{},\
                 \"violations\":{},\"first_viol\":{},\"reads\":{},\
                 \"lreads\":{},\"mean_reads\":{:.2}}}",
                it.insts[inst].path,
                it.s(rname),
                st.n,
                st.fires,
                st.skippable,
                st.violations,
                st.first_viol.map_or("null".into(), |c| c.to_string()),
                self.reads[r].len(),
                self.lreads[r].len(),
                mass,
            ));
        }
        let mut top: Vec<(usize, u64)> =
            self.change_count.iter().map(|(a, b)| (*a, *b)).collect();
        top.sort_by_key(|&(_, c)| std::cmp::Reverse(c));
        let topch: Vec<String> = top
            .iter()
            .take(25)
            .map(|&(i, c)| {
                format!(
                    "{{\"inst\":\"{}\",\"changes\":{},\"kind\":{}}}",
                    it.insts[i].path,
                    c,
                    self.kind.get(&i).copied().unwrap_or(0)
                )
            })
            .collect();
        let body = format!(
            "{{\"cycles_total\":{},\"cycles_simulated\":{},\
             \"latch_events\":{},\"fires\":{},\"skippable\":{},\
             \"violations\":{},\"skip_frac_unweighted\":{:.4},\
             \"skip_frac_read_weighted\":{:.4},\
             \"union_growth_after_from\":{},\"from_cycle\":{},\
             \"rules\":[\n{}\n],\"top_changed\":[\n{}\n]}}\n",
            self.cycles_seen,
            self.cycles_sim,
            tn,
            tf,
            ts,
            tv,
            if tn > 0 { ts as f64 / tn as f64 } else { 0.0 },
            if wn > 0.0 { wsk / wn } else { 0.0 },
            self.union_growth_after_from,
            self.from,
            lines.join(",\n"),
            topch.join(",\n"),
        );
        match &self.out {
            Some(p) => {
                let _ = std::fs::write(p, &body);
                eprintln!(
                    "trs census: report written to {p} \
                     (cycles={}, skip_unweighted={:.4}, violations={})",
                    self.cycles_sim,
                    if tn > 0 { ts as f64 / tn as f64 } else { 0.0 },
                    tv
                );
            }
            None => eprint!("{body}"),
        }
    }
}
