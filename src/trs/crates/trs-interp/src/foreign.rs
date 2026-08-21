//! ForeignEnv — the runtime state behind the Verilog system-task
//! family, split out of the Interp so the compiled tier's foreign
//! bounces (jit_foreign_cb) can be serviced by something much smaller
//! than a full interpreter.  Everything here is design-independent:
//! console/file handles, the finish/fatal/stop latches, plusargs, the
//! timescale, and the $display scratch buffer.  The design-coupled
//! arms stay with the owner — $dump* needs the VCD writer and the
//! instance tree, BDPI needs the design's foreign-function table — so
//! `action`/`value` return not-handled for those and the caller
//! decides (the Interp dispatches them directly; a RunCore driver
//! treats them as materialization triggers).

use std::collections::HashMap;

use crate::emit_output_errors;
use crate::format::{self, Arg};
use crate::value::Value;

/// One Verilog file-table slot (VLFiles keeps FILE*; the std streams are
/// distinguished so they are never closed and write to the right place).
pub(crate) enum FSlot {
    Stdin,
    Stdout,
    Stderr,
    File(std::fs::File),
    /// quiet-engine write-mode $fopen: the slot (and its design-
    /// visible key) exists so fd values match the primary engine
    /// exactly, but nothing touches the filesystem
    Sink,
    Closed,
}

pub(crate) struct ForeignEnv {
    /// Verilog file handles, mirroring VLFiles (dollar_display.cxx):
    /// one-arg $fopen returns a one-hot MCD key (slot 0 = stdout, first
    /// user file = 0x2, writes fan out to every set bit); two-arg $fopen
    /// returns 0x8000_0000+index with stdin/stdout/stderr preregistered
    /// (first user fd = 0x8000_0003)
    pub(crate) mcd_files: Vec<FSlot>,
    pub(crate) fd_files: Vec<FSlot>,
    /// per-key pushback stack for $ungetc; $fgetc pops from here first
    pub(crate) pushback: HashMap<u64, Vec<u8>>,
    /// command-line +args (without the '+'), for $test$plusargs
    pub(crate) plusargs: Vec<String>,
    /// bk_set_timescale factor: $time/%t display = now * timescale
    /// (kernel bk_now semantics).  CAVEAT: the edge-SSA join
    /// re-materialization of $time loads the raw now slot — the
    /// interp engine (which the capi debug tier uses) is exact;
    /// compiled engines assume timescale == 1.
    pub(crate) timescale: u64,
    pub(crate) finished: Option<i32>,
    /// $fatal was called: the bluesim.tcl driver exits 1 in that case
    /// and 0 otherwise ($finish codes are not process exit codes)
    pub(crate) fataled: bool,
    /// secondary oracle engine (docs/TCL-CAPI.md): every output sink
    /// is suppressed — console, design files ($fopen(w) -> Sink), and
    /// VCD — while all STATE effects (including $finish/$fatal flags
    /// and file reads) run normally so lockstep compare is meaningful
    pub(crate) quiet: bool,
    /// $stop yield: ends the current advance at the slice boundary
    /// but does NOT finish the sim — cleared at the next advance so
    /// the session resumes (the reference's resumable-$stop contract)
    pub(crate) stop_request: bool,
    /// $display scratch: reused output buffer (see write_display)
    pub(crate) fmt_out: String,
}

impl ForeignEnv {
    pub(crate) fn new() -> Self {
        ForeignEnv {
            mcd_files: vec![FSlot::Stdout],
            fd_files: vec![FSlot::Stdin, FSlot::Stdout, FSlot::Stderr],
            pushback: HashMap::new(),
            plusargs: Vec::new(),
            timescale: 1,
            finished: None,
            fataled: false,
            quiet: false,
            stop_request: false,
            fmt_out: String::new(),
        }
    }

    pub(crate) fn write_fd(&mut self, key: u64, text: &str) {
        use std::io::Write;
        if self.quiet {
            // oracle secondary: every write sink suppressed
            crate::prim::note_window_effect();
            return;
        }
        let write_slot = |s: &mut FSlot| match s {
            FSlot::Stdout => print!("{text}"),
            FSlot::Stderr => eprint!("{text}"),
            FSlot::File(f) => {
                let _ = f.write_all(text.as_bytes());
            }
            _ => {}
        };
        if key >= 0x8000_0000 {
            let idx = (key - 0x8000_0000) as usize;
            if let Some(s) = self.fd_files.get_mut(idx) {
                write_slot(s);
            }
        } else {
            let mut k = key;
            let mut i = 0usize;
            while k != 0 {
                if k & 1 == 1 {
                    if let Some(s) = self.mcd_files.get_mut(i) {
                        write_slot(s);
                    }
                }
                k >>= 1;
                i += 1;
            }
        }
    }

    /// VLFiles::closeFiles: fd keys above the std handles close their
    /// slot; MCD masks close every set bit except stdout (bit 0).
    pub(crate) fn close_files(&mut self, key: u64) {
        if key > 0x8000_0002 {
            let idx = (key - 0x8000_0000) as usize;
            if let Some(s) = self.fd_files.get_mut(idx) {
                if matches!(s, FSlot::File(_) | FSlot::Sink) {
                    *s = FSlot::Closed;
                }
            }
        } else if key < 0x0800_0000 {
            let mut k = key >> 1; // skip stdout
            let mut i = 1usize;
            while k != 0 {
                if k & 1 == 1 {
                    if let Some(s) = self.mcd_files.get_mut(i) {
                        if matches!(s, FSlot::File(_) | FSlot::Sink) {
                            *s = FSlot::Closed;
                        }
                    }
                }
                k >>= 1;
                i += 1;
            }
        }
    }

    /// Format a $display-family call into the reused scratch and write
    /// it to stdout in ONE locked write_all (println!-per-call paid a
    /// fresh String, a lock, and a core::fmt walk each; the scratch
    /// survives across calls so the hot path allocates nothing).  Same
    /// std LineWriter, so flush semantics ($fflush, the BDPI phase-0
    /// flush, newline-triggered line flushes) are untouched.
    pub(crate) fn write_display(
        &mut self,
        args: &[Arg],
        base: u32,
        now: u64,
        loc: &str,
        newline: bool,
        errs: &mut Vec<String>,
    ) {
        let mut out = std::mem::take(&mut self.fmt_out);
        out.clear();
        format::format_args_into(&mut out, args, base, now, loc, errs);
        if newline {
            out.push('\n');
        }
        use std::io::Write;
        let _ = std::io::stdout().lock().write_all(out.as_bytes());
        self.fmt_out = out;
    }

    /// Handle an action (void) system task.  Returns true when the
    /// task was consumed here — including quiet/post-finish
    /// suppression — and false when the name belongs to the owner
    /// ($dump* family, BDPI imports).
    pub(crate) fn action(&mut self, name: &str, args: &[Arg], now: u64, loc: &str) -> bool {
        // oracle secondary: console output and the VCD task family are
        // suppressed wholesale ($f* writes die in write_fd; Sink slots
        // cover the files).  $fatal is NOT in this list — its finished/
        // fataled state must still latch (print gated in its arm).
        if self.quiet
            && matches!(
                name,
                "$display"
                    | "$displayb"
                    | "$displayo"
                    | "$displayh"
                    | "$write"
                    | "$writeb"
                    | "$writeo"
                    | "$writeh"
                    | "$error"
                    | "$warning"
                    | "$info"
                    | "$fflush"
                    | "$dumpfile"
                    | "$dumpvars"
                    | "$dumpon"
                    | "$dumpoff"
                    | "$dumpall"
                    | "$dumplimit"
                    | "$dumpflush"
            )
        {
            crate::prim::note_window_effect();
            return true;
        }
        if self.finished.is_some()
            && matches!(
                name,
                "$display"
                    | "$displayb"
                    | "$displayo"
                    | "$displayh"
                    | "$write"
                    | "$writeb"
                    | "$writeo"
                    | "$writeh"
                    | "$fdisplay"
                    | "$fdisplayb"
                    | "$fdisplayo"
                    | "$fdisplayh"
                    | "$fwrite"
                    | "$fwriteb"
                    | "$fwriteo"
                    | "$fwriteh"
                    | "$error"
                    | "$warning"
                    | "$info"
                    | "$fatal"
            )
        {
            // post-$finish OUTPUT tasks are suppressed in the
            // reference — the whole dollar_display.cxx family (29
            // bk_finished gates: console, file, and severity tasks);
            // the rules themselves still run.  The value-bearing
            // $swrite/$sformat AV tasks are also gated there but
            // their post-finish return is unwitnessed — left live
            // until a test pins the contract.
            return true;
        }
        match name {
            "$fdisplay" | "$fwrite" | "$fdisplayh" | "$fwriteh"
            | "$fdisplayb" | "$fwriteb" | "$fdisplayo" | "$fwriteo" => {
                let base = match name.chars().last() {
                    Some('h') => 16,
                    Some('b') => 2,
                    Some('o') => 8,
                    _ => 10,
                };
                let fd = match args.first() {
                    Some(Arg::Val(v, _)) => v.as_u64(),
                    _ => 0x8000_0000,
                };
                let mut errs = Vec::new();
                let mut text = format::format_args(&args[1..], base, now, loc, &mut errs);
                if name.starts_with("$fdisplay") {
                    text.push('\n');
                }
                self.write_fd(fd, &text);
                emit_output_errors(&errs);
            }
            "$fclose" => {
                // fd/mcd table state is not in the baked arena — a
                // window-time close is an effect the skipped window
                // cannot replay (adversarial-panel finding)
                if crate::prim::quiet_engine() {
                    crate::prim::note_window_effect();
                }
                if let Some(Arg::Val(v, _)) = args.first() {
                    self.close_files(v.as_u64());
                }
            }
            "$fflush" => {
                use std::io::Write;
                let _ = std::io::stdout().flush();
                let key = match args.first() {
                    Some(Arg::Val(v, _)) => Some(v.as_u64()),
                    _ => None,
                };
                for tbl in [&mut self.fd_files, &mut self.mcd_files] {
                    for s in tbl.iter_mut() {
                        if let FSlot::File(f) = s {
                            if key.is_none() {
                                let _ = f.flush();
                            }
                        }
                    }
                }
                if let Some(k) = key {
                    // flush the key's fan-out by writing nothing through
                    // the same decode path, then flushing each file
                    if k >= 0x8000_0000 {
                        if let Some(FSlot::File(f)) =
                            self.fd_files.get_mut((k - 0x8000_0000) as usize)
                        {
                            let _ = f.flush();
                        }
                    } else {
                        let (mut kk, mut i) = (k, 0usize);
                        while kk != 0 {
                            if kk & 1 == 1 {
                                if let Some(FSlot::File(f)) = self.mcd_files.get_mut(i) {
                                    let _ = f.flush();
                                }
                            }
                            kk >>= 1;
                            i += 1;
                        }
                    }
                }
            }
            "$display" => {
                let mut errs = Vec::new();
                self.write_display(args, 10, now, loc, true, &mut errs);
                emit_output_errors(&errs);
            }
            "$displayh" => {
                let mut errs = Vec::new();
                self.write_display(args, 16, now, loc, true, &mut errs);
                emit_output_errors(&errs);
            }
            "$displayb" => {
                let mut errs = Vec::new();
                self.write_display(args, 2, now, loc, true, &mut errs);
                emit_output_errors(&errs);
            }
            "$displayo" => {
                let mut errs = Vec::new();
                self.write_display(args, 8, now, loc, true, &mut errs);
                emit_output_errors(&errs);
            }
            "$write" => {
                let mut errs = Vec::new();
                self.write_display(args, 10, now, loc, false, &mut errs);
                emit_output_errors(&errs);
            }
            "$writeh" => {
                let mut errs = Vec::new();
                self.write_display(args, 16, now, loc, false, &mut errs);
                emit_output_errors(&errs);
            }
            "$writeb" => {
                let mut errs = Vec::new();
                self.write_display(args, 2, now, loc, false, &mut errs);
                emit_output_errors(&errs);
            }
            "$writeo" => {
                let mut errs = Vec::new();
                self.write_display(args, 8, now, loc, false, &mut errs);
                emit_output_errors(&errs);
            }
            // dollar_error/dollar_warning/dollar_info format exactly like
            // $display — bsc compiles the severity prefix into the message
            "$error" | "$warning" | "$info" => {
                let mut errs = Vec::new();
                self.write_display(args, 10, now, loc, true, &mut errs);
                emit_output_errors(&errs);
            }
            "$fatal" => {
                // first argument is the status passed to bk_fatal_now; the
                // driver ignores it and exits 1 whenever $fatal fired
                let rest = match args.split_first() {
                    Some((Arg::Val(_, _), rest)) => rest,
                    _ => args,
                };
                if !self.quiet {
                    let mut errs = Vec::new();
                    self.write_display(rest, 10, now, loc, true, &mut errs);
                    emit_output_errors(&errs);
                } else {
                    crate::prim::note_window_effect();
                }
                self.fataled = true;
                self.finished = Some(1);
            }
            "$finish" => {
                let code = match args.first() {
                    Some(Arg::Val(v, _)) => v.as_u64() as i32,
                    _ => 0,
                };
                self.finished = Some(code);
            }
            // $stop PAUSES (resumable yield: bk_finished stays false,
            // `sim step`/`sim run` resume); $finish TERMINATES.  The
            // batch driver observes the yield, reaches script end, and
            // exits 0 — byte-identical to the reference's batch $stop.
            "$stop" => self.stop_request = true,
            _ => return false,
        }
        true
    }

    /// Handle a value-returning system task.  None = not mine (the
    /// owner tries its BDPI table next).
    pub(crate) fn value(
        &mut self,
        name: &str,
        args: &[Arg],
        w: u32,
        now: u64,
        loc: &str,
    ) -> Option<Value> {
        Some(match name {
            "$time" | "$stime" => {
                // the reference's $time goes through bk_now =
                // sim_timescale * sim_time (dollar_time.cxx)
                Value::from_u64(w.max(1), now.wrapping_mul(self.timescale))
            }
            "$fopen" => {
                // any window-time file open is a run-time effect the
                // skipped window cannot reproduce (truncation, reads)
                if crate::prim::quiet_engine() {
                    crate::prim::note_window_effect();
                }
                let path = match args.first() {
                    Some(Arg::Str(s)) => s.clone(),
                    _ => return Some(Value::zero(w.max(1))),
                };
                // one-arg form = MCD (always write mode); two-arg = fd
                let mcd = !matches!(args.get(1), Some(Arg::Str(_)));
                let write_mode = !matches!(args.get(1), Some(Arg::Str(m)) if m.starts_with('r'));
                // oracle secondary: a write-mode open would truncate the
                // file the primary just wrote — allocate a Sink slot so
                // the design-visible key matches without touching the
                // filesystem.  Read-mode opens stay real (reads feed
                // design state, which must track the primary).
                let f = if self.quiet && write_mode {
                    Ok(FSlot::Sink)
                } else if write_mode {
                    std::fs::File::create(&*path).map(FSlot::File)
                } else {
                    std::fs::File::open(&*path).map(FSlot::File)
                };
                match f {
                    Ok(f) => {
                        let key = if mcd {
                            // registerFile(true,..): append below 31 bits,
                            // else reuse a closed slot
                            if self.mcd_files.len() < 31 {
                                self.mcd_files.push(f);
                                1u64 << (self.mcd_files.len() - 1)
                            } else if let Some(i) = self
                                .mcd_files
                                .iter()
                                .position(|s| matches!(s, FSlot::Closed))
                            {
                                self.mcd_files[i] = f;
                                1u64 << i
                            } else {
                                return Some(Value::zero(w.max(1)));
                            }
                        } else {
                            self.fd_files.push(f);
                            0x8000_0000 + (self.fd_files.len() as u64 - 1)
                        };
                        Value::from_u64(w.max(32), key)
                    }
                    Err(_) => Value::zero(w.max(1)),
                }
            }
            // prefix match against the registered +args (bk_match_argument)
            "$test$plusargs" => {
                // plusargs differ between link (none) and run
                if crate::prim::quiet_engine() {
                    crate::prim::note_window_effect();
                }
                let name = match args.first() {
                    Some(Arg::Str(s)) => s.to_string(),
                    Some(Arg::Val(v, _)) => format::unpack_str_pub(v),
                    _ => String::new(),
                };
                let hit = self.plusargs.iter().any(|a| a.starts_with(&name));
                Value::from_u64(w.max(1), hit as u64)
            }
            "$fgetc" => {
                use std::io::Read;
                // consumes stdin/file position — a run-time effect
                if crate::prim::quiet_engine() {
                    crate::prim::note_window_effect();
                }
                let fd = match args.first() {
                    Some(Arg::Val(v, _)) => v.as_u64(),
                    _ => return Some(Value::from_u64(w.max(32), u32::MAX as u64)),
                };
                if let Some(b) = self.pushback.get_mut(&fd).and_then(|s| s.pop()) {
                    return Some(Value::from_u64(w.max(32), b as u64));
                }
                // getFD: the fd table only
                let mut byte = [0u8; 1];
                if fd >= 0x8000_0000 {
                    match self.fd_files.get_mut((fd - 0x8000_0000) as usize) {
                        Some(FSlot::File(f)) => {
                            if f.read_exact(&mut byte).is_ok() {
                                return Some(Value::from_u64(w.max(32), byte[0] as u64));
                            }
                        }
                        Some(FSlot::Stdin) => {
                            if std::io::stdin().read_exact(&mut byte).is_ok() {
                                return Some(Value::from_u64(w.max(32), byte[0] as u64));
                            }
                        }
                        _ => {}
                    }
                }
                // EOF / bad fd: -1
                Value::from_u64(w.max(32), 0xFFFF_FFFF)
            }
            "$ungetc" => {
                // args: (char, fd); pushes back for the next $fgetc and
                // returns the char (C ungetc semantics)
                if crate::prim::quiet_engine() {
                    crate::prim::note_window_effect();
                }
                let c = match args.first() {
                    Some(Arg::Val(v, _)) => v.as_u64() as u8,
                    _ => 0,
                };
                let fd = match args.get(1) {
                    Some(Arg::Val(v, _)) => v.as_u64(),
                    _ => 0,
                };
                // valid on any live fd-table entry (getFD != NULL)
                let live = fd >= 0x8000_0000
                    && !matches!(
                        self.fd_files.get((fd - 0x8000_0000) as usize),
                        None | Some(FSlot::Closed)
                    );
                if live {
                    self.pushback.entry(fd).or_default().push(c);
                    Value::from_u64(w.max(32), c as u64)
                } else {
                    Value::from_u64(w.max(32), 0xFFFF_FFFF)
                }
            }
            "$swriteAV" | "$sformatAV" | "$swritebAV" | "$swriteoAV" | "$swritehAV" => {
                // format into a string, then pack the ASCII bytes into the
                // result width (right-justified, like the C++ BufferTarget
                // + copy_back)
                let base = match name {
                    "$swritebAV" => 2,
                    "$swriteoAV" => 8,
                    "$swritehAV" => 16,
                    _ => 10,
                };
                let mut errs = Vec::new();
                let text = format::format_sformat(
                    args, base, now, loc, name == "$sformatAV", &mut errs,
                );
                emit_output_errors(&errs);
                let packed = format::str_value(&text);
                if packed.width >= w {
                    packed.extract(w as u64 - 1, 0, w)
                } else {
                    packed.zext(w)
                }
            }
            "$fclose" => {
                if crate::prim::quiet_engine() {
                    crate::prim::note_window_effect();
                }
                if let Some(Arg::Val(v, _)) = args.first() {
                    self.close_files(v.as_u64());
                }
                Value::zero(w.max(1))
            }
            _ => return None,
        })
    }
}
