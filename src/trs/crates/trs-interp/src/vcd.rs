//! VCD dumping, mirroring Bluesim's `vcd.cxx` byte-for-byte (see
//! docs/VCD-CONTRACT.md):
//!
//! - header: `$date` (ctime text) / `$version` ("Bluespec VCD dumper
//!   2.1") / `$timescale` (default "1 us");
//! - `$scope module NAME $end` scopes, `$var reg W <id> <name> $end`
//!   definitions (every var is type `reg`), base-94 printable ids
//!   starting at '!';
//! - `#<time>` markers, 1-bit changes as `0<id>`/`1<id>`/`x<id>`,
//!   multi-bit as `b<binary-no-leading-zeros> <id>` / `bx <id>`;
//! - `$dumpvars`/`$dumpoff`/`$dumpall`/`$dumpon` task sections closed
//!   with `$end` just before the next time marker;
//! - change buffering: combinational signals are back-dated to their
//!   clock's previous edge (`time_of_change`) and flushed only once
//!   every clock's combinational window has passed (`min_pending`).

use crate::value::Value;
use std::collections::{BTreeMap, HashMap};
use std::io::Write;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum VcdState {
    Off,
    Header,
    Enabled,
    Disabled,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DumpType {
    None,
    Xs,
    Initial,
    Checkpoint,
    Changes,
    Restart,
}

enum Change {
    Val(Value),
    X(u32),
}

pub struct Vcd {
    file: Option<std::fs::File>,
    filename: Option<String>,
    pub state: VcdState,
    pub enabled: bool,
    checkpoint: bool,
    go_xs: bool,
    pub depth: u32,
    limit: u64,
    next_seq: u32,
    kept_seq: u32,
    /// buffered changes per (backdated) time
    changes: BTreeMap<u64, Vec<(u32, Change)>>,
    tasks: HashMap<u64, &'static str>,
    min_pending: u64,
    changes_now: bool,
    last_time_written: Option<u64>,
    need_end_task: bool,
    /// id -> clock indices whose combinational time stamps its changes
    clk_map: HashMap<u32, Vec<usize>>,
    /// per-clock combinational time (previous same-direction edge),
    /// maintained by the run loop
    pub clk_combinational: Vec<u64>,
    pub timescale: String,
}

impl Vcd {
    pub fn new() -> Vcd {
        Vcd {
            file: None,
            filename: None,
            state: VcdState::Off,
            enabled: false,
            checkpoint: false,
            go_xs: false,
            depth: 0,
            limit: 0,
            next_seq: 0,
            kept_seq: 0,
            changes: BTreeMap::new(),
            tasks: HashMap::new(),
            min_pending: 0,
            changes_now: false,
            last_time_written: None,
            need_end_task: false,
            clk_map: HashMap::new(),
            clk_combinational: Vec::new(),
            timescale: "1 us".to_string(),
        }
    }

    // ===============
    // id allocation

    pub fn reserve_ids(&mut self, n: u32) -> u32 {
        let r = self.next_seq;
        self.next_seq += n;
        r
    }

    pub fn keep_ids(&mut self) {
        self.kept_seq = self.next_seq;
    }

    pub fn set_clock(&mut self, id: u32, clk: usize) {
        self.clk_map.entry(id).or_default().push(clk);
    }

    fn id_str(mut num: u32) -> String {
        // big-endian base-94, digits '!'..'~'
        let mut digits = Vec::new();
        loop {
            digits.push((b'!' + (num % 94) as u8) as char);
            num /= 94;
            if num == 0 {
                break;
            }
        }
        digits.iter().rev().collect()
    }

    // ===============
    // file control (bk_set_VCD_file / vcd_set_state)

    pub fn set_file(&mut self, name: &str) -> Result<(), ()> {
        if self.file.is_some() {
            self.flush_all_pending();
            self.file = None;
        }
        self.state = VcdState::Off;
        match std::fs::File::create(name) {
            Ok(f) => {
                self.file = Some(f);
                self.filename = Some(name.to_string());
                // C++ zero-inits last_time_written, which suppresses the
                // '#0' marker (and the task text) at the initial dump
                self.last_time_written = Some(0);
                self.need_end_task = false;
                Ok(())
            }
            Err(e) => {
                eprintln!("{name}: {e}");
                self.filename = None;
                Err(())
            }
        }
    }

    pub fn set_state(&mut self, on: bool) {
        if on && self.file.is_none() {
            let _ = self.set_file("dump.vcd");
        }
        self.enabled = on;
    }

    /// bk_set_VCD_file(NULL) (vcd.cxx:36): close the file, clear the
    /// name, dumping off — success.  (The reference's previous-file
    /// append branch is DEAD CODE: previous_files is never populated
    /// in vcd.cxx, so set_file's plain create mirrors re-opens
    /// bug-for-bug.)
    pub fn close_file(&mut self) {
        if self.file.is_some() {
            self.flush_all_pending();
            self.file = None;
        }
        self.filename = None;
        self.state = VcdState::Off;
    }

    /// bk_enable_VCD_dumping (kernel.cxx:1521): idempotent; opening
    /// the default dump.vcd can fail -> false, NOT enabled (unlike
    /// set_state, which enables unconditionally for the $dumpon task).
    pub fn enable(&mut self) -> bool {
        if self.enabled {
            return true;
        }
        if self.file.is_none() && self.set_file("dump.vcd").is_err() {
            return false;
        }
        self.enabled = true;
        true
    }

    /// bk_disable_VCD_dumping (kernel.cxx:1534): no-op when off; the
    /// Xs section is deferred to the next VCD event exactly like the
    /// reference (vcd_dump_xs just sets go_xs).
    pub fn disable(&mut self) {
        if !self.enabled {
            return;
        }
        self.enabled = false;
        self.go_xs = true;
    }

    pub fn set_depth(&mut self, d: u32) {
        if self.state == VcdState::Off {
            self.depth = d;
        }
    }

    /// bk_get_VCD_file_name's source: "" when unset (C++ c_str()).
    pub fn file_name(&self) -> &str {
        self.filename.as_deref().unwrap_or("")
    }

    pub fn set_limit(&mut self, l: u64) {
        self.limit = l;
    }

    pub fn request_checkpoint(&mut self) {
        if self.file.is_none() {
            let _ = self.set_file("dump.vcd");
        }
        self.checkpoint = true;
    }

    pub fn dump_xs(&mut self) {
        self.go_xs = true;
    }

    pub fn is_active(&self) -> bool {
        self.enabled || self.checkpoint || self.go_xs
    }

    pub fn flush(&mut self) {
        if let Some(f) = self.file.as_mut() {
            let _ = f.flush();
        }
    }

    /// get_VCD_dump_type's state machine.
    pub fn dump_type(&mut self) -> DumpType {
        if self.checkpoint {
            self.checkpoint = false;
            self.go_xs = !self.enabled;
            return DumpType::Checkpoint;
        }
        if self.go_xs {
            self.go_xs = false;
            self.state = VcdState::Disabled;
            return DumpType::Xs;
        }
        match self.state {
            VcdState::Off => DumpType::None,
            VcdState::Header => {
                self.state = VcdState::Enabled;
                DumpType::Initial
            }
            VcdState::Enabled => DumpType::Changes,
            VcdState::Disabled => {
                self.state = VcdState::Enabled;
                DumpType::Restart
            }
        }
    }

    // ===============
    // writing

    fn out(&mut self, s: &str) {
        if let Some(f) = self.file.as_mut() {
            let _ = f.write_all(s.as_bytes());
        }
    }

    /// vcd_write_header: only when state==Off; resets model ids.
    pub fn write_header(&mut self) -> bool {
        if self.state != VcdState::Off {
            return false;
        }
        let date = ctime_now();
        self.out(&format!("$date\n\t{date}$end\n"));
        self.out("$version\n\tBluespec VCD dumper 2.1\n$end\n");
        let ts = self.timescale.clone();
        self.out(&format!("$timescale\n\t{ts}\n$end\n"));
        self.next_seq = self.kept_seq;
        self.state = VcdState::Header;
        true
    }

    pub fn scope_start(&mut self, name: &str) {
        self.out(&format!("$scope module {name} $end\n"));
    }

    pub fn scope_end(&mut self) {
        self.out("$upscope $end\n");
    }

    pub fn write_def(&mut self, id: u32, name: &str, width: u32) {
        let ids = Self::id_str(id);
        self.out(&format!("$var reg {width} {ids} {name} $end\n"));
    }

    pub fn enddefinitions(&mut self) {
        self.out("$enddefinitions $end\n");
    }

    pub fn task(&mut self, t: u64, name: &'static str) {
        self.tasks.insert(t, name);
    }

    /// vcd_output_at_time: '#t' with task open/close bracketing.
    fn output_at_time(&mut self, t: u64) {
        if self.last_time_written == Some(t) {
            return;
        }
        if self.need_end_task {
            self.out("$end\n");
            self.need_end_task = false;
        }
        self.out(&format!("#{t}\n"));
        self.last_time_written = Some(t);
        if let Some(task) = self.tasks.remove(&t) {
            self.out(&format!("{task}\n"));
            self.need_end_task = true;
        }
    }

    fn print_change(&mut self, id: u32, c: &Change) {
        let ids = Self::id_str(id);
        match c {
            Change::X(w) => {
                if *w == 1 {
                    self.out(&format!("x{ids}\n"));
                } else {
                    self.out(&format!("bx {ids}\n"));
                }
            }
            Change::Val(v) => {
                if v.width == 1 {
                    let b = if v.as_u64() & 1 == 1 { '1' } else { '0' };
                    self.out(&format!("{b}{ids}\n"));
                } else {
                    let s = bin_no_leading_zeros(v);
                    self.out(&format!("b{s} {ids}\n"));
                }
            }
        }
    }

    /// time_of_change: clk_map ids stamp at their clock's combinational
    /// time; unmapped ids at `now`.
    fn time_of_change(&self, id: u32, now: u64) -> u64 {
        if self.changes_now {
            return now;
        }
        match self.clk_map.get(&id) {
            None => now,
            Some(clks) => clks
                .iter()
                .map(|&c| self.clk_combinational.get(c).copied().unwrap_or(0))
                .max()
                .unwrap_or(now),
        }
    }

    fn emit(&mut self, id: u32, c: Change, now: u64) {
        let t = self.time_of_change(id, now);
        if t > self.min_pending {
            self.changes.entry(t).or_default().push((id, c));
        } else {
            self.output_at_time(t);
            self.print_change(id, &c);
        }
    }

    pub fn write_val(&mut self, id: u32, v: &Value, now: u64) {
        self.emit(id, Change::Val(v.clone()), now);
    }

    pub fn write_x(&mut self, id: u32, width: u32, now: u64) {
        self.emit(id, Change::X(width), now);
    }

    /// vcd_advance: recompute min_pending and flush strictly-older
    /// buffered changes.
    pub fn advance(&mut self, now: u64, immediate: bool) {
        let min_clk = self.clk_combinational.iter().copied().min().unwrap_or(now);
        self.min_pending = now.min(min_clk);
        self.flush_changes();
        self.changes_now = immediate;
    }

    fn flush_changes(&mut self) {
        loop {
            let Some((&t, _)) = self.changes.iter().next() else { break };
            if t >= self.min_pending {
                break;
            }
            let list = self.changes.remove(&t).unwrap();
            self.output_at_time(t);
            for (id, c) in &list {
                self.print_change(*id, c);
            }
        }
    }

    /// vcd_reset's flush (end of run): everything strictly before `now`.
    pub fn flush_all_pending(&mut self) {
        self.changes_now = false;
        self.flush_changes();
        self.flush();
    }

    pub fn set_final_min_pending(&mut self, now: u64) {
        self.min_pending = now;
    }

    /// vcd_check_file_size after each event.
    pub fn check_file_size(&mut self, now: u64) {
        if self.limit == 0 {
            return;
        }
        let too_big = self
            .file
            .as_ref()
            .and_then(|f| f.metadata().ok())
            .map(|m| m.len() > self.limit)
            .unwrap_or(false);
        if too_big {
            self.out("$comment\nVCD file size limit exceeded\n$end\n");
            self.set_final_min_pending(now);
            self.flush_all_pending();
            self.file = None;
            self.filename = None;
            self.enabled = false;
            self.state = VcdState::Off;
            self.depth = 0;
            self.limit = 0;
        }
    }
}

/// Binary text with leading zeros elided; zero prints "0" (print_binary).
fn bin_no_leading_zeros(v: &Value) -> String {
    let mut s = String::new();
    let mut seen = false;
    for i in (0..v.width).rev() {
        let b = v.bit(i);
        if b {
            seen = true;
        }
        if seen {
            s.push(if b { '1' } else { '0' });
        }
    }
    if s.is_empty() {
        s.push('0');
    }
    s
}

/// ctime(3)-format local time: "Www Mmm dd hh:mm:ss yyyy\n" (day-of-month
/// space-padded), matching the reference header's $date line.
fn ctime_now() -> String {
    let secs = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or(0);
    // UTC civil-time conversion (the container runs UTC)
    let days = secs.div_euclid(86400);
    let tod = secs.rem_euclid(86400);
    let (h, m, s) = (tod / 3600, (tod % 3600) / 60, tod % 60);
    // Howard Hinnant's civil_from_days
    let z = days + 719468;
    let era = z.div_euclid(146097);
    let doe = z.rem_euclid(146097);
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let mon = if mp < 10 { mp + 3 } else { mp - 9 };
    let year = if mon <= 2 { y + 1 } else { y };
    let wd = (days + 4).rem_euclid(7); // 1970-01-01 was Thursday
    let wdays = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
    let months = [
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
    ];
    format!(
        "{} {} {:2} {:02}:{:02}:{:02} {}\n",
        wdays[wd as usize],
        months[(mon - 1) as usize],
        d,
        h,
        m,
        s,
        year
    )
}
