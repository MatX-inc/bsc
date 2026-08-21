//! Process-level stdout sink.  The reference C engines inherit stdio's
//! buffering contract: block-buffered when fd 1 is not a terminal,
//! line-buffered when it is.  Rust's std stdout is ALWAYS a LineWriter,
//! which costs a write(2) per $display line on piped batch runs.  Every
//! stdout write in the runtime goes through this sink; when fd 1 is not
//! a tty it is a 64KiB block buffer over the raw fd, flushed at the
//! points where ordering is observable from outside the buffer:
//!
//!   - $fflush (foreign.rs)
//!   - the BDPI stdio contract, phase 0 (bdpi.rs / jit_stdio_cb) —
//!     user C code writes through libc's own buffers
//!   - run teardown (main.rs exits via libc::_exit, which skips the
//!     atexit net below; runcore.rs mirrors that teardown)
//!   - atexit, covering every exit(3) path (registered on block init)
//!
//! Panics and signals lose buffered output, exactly like C block
//! buffering.  Write errors are ignored (the write_display contract
//! predating the sink).
//!
//! Line mode is forced by TRS_STDOUT_LINE (any value) or `force_line()`.
//! Two tiers pin it: the capi .so is dlopened into a driver (bluetcl)
//! whose own stdio interleaves with sim output — two independent block
//! buffers on one fd would reorder — and the in-process script tier
//! (`trs run -c/-f`) prints command responses through std stdout
//! between sim advances.

use std::io::Write;
use std::sync::{Mutex, OnceLock};

enum Sink {
    /// std stdout (LineWriter): tty, forced, or capi/script tiers
    Line,
    Block(Mutex<std::io::BufWriter<std::fs::File>>),
}

static SINK: OnceLock<Sink> = OnceLock::new();

extern "C" fn flush_at_exit() {
    flush();
}

fn sink() -> &'static Sink {
    SINK.get_or_init(|| {
        if std::env::var_os("TRS_STDOUT_LINE").is_some()
            || unsafe { libc::isatty(1) } != 0
        {
            Sink::Line
        } else {
            // SAFETY: fd 1 is open and stays open for the process
            // lifetime; the File lives in a never-dropped static, so
            // the fd is never closed through it.
            let f = unsafe {
                <std::fs::File as std::os::fd::FromRawFd>::from_raw_fd(1)
            };
            unsafe { libc::atexit(flush_at_exit) };
            Sink::Block(Mutex::new(std::io::BufWriter::with_capacity(
                1 << 16,
                f,
            )))
        }
    })
}

/// Pin the sink to line-buffered std stdout.  Must run before the
/// first write; a no-op once the sink is initialized.
pub fn force_line() {
    let _ = SINK.set(Sink::Line);
}

pub fn write_bytes(b: &[u8]) {
    match sink() {
        Sink::Line => {
            let _ = std::io::stdout().lock().write_all(b);
        }
        Sink::Block(m) => {
            let _ = m.lock().unwrap_or_else(|e| e.into_inner()).write_all(b);
        }
    }
}

pub fn write_str(s: &str) {
    write_bytes(s.as_bytes());
}

pub fn write_fmt(args: std::fmt::Arguments) {
    match sink() {
        Sink::Line => {
            let _ = std::io::stdout().lock().write_fmt(args);
        }
        Sink::Block(m) => {
            let _ = m.lock().unwrap_or_else(|e| e.into_inner()).write_fmt(args);
        }
    }
}

/// println! shape: one lock for the formatted body plus the newline.
pub fn writeln_fmt(args: std::fmt::Arguments) {
    match sink() {
        Sink::Line => {
            let mut o = std::io::stdout().lock();
            let _ = o.write_fmt(args);
            let _ = o.write_all(b"\n");
        }
        Sink::Block(m) => {
            let mut o = m.lock().unwrap_or_else(|e| e.into_inner());
            let _ = o.write_fmt(args);
            let _ = o.write_all(b"\n");
        }
    }
}

pub fn flush() {
    match sink() {
        Sink::Line => {
            let _ = std::io::stdout().flush();
        }
        Sink::Block(m) => {
            let _ = m.lock().unwrap_or_else(|e| e.into_inner()).flush();
        }
    }
}
