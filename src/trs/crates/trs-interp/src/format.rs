//! $display-family formatting, matching Bluesim's engine
//! (`dollar_display.cxx`): every string argument is a format string
//! consuming subsequent arguments; non-string arguments print in the
//! task's default base; Verilog column rules (%d space-pads to the
//! signal's maximal decimal width, %h zero-pads to ceil(bits/4), ...).

use crate::value::Value;

#[derive(Debug, Clone)]
pub enum Arg {
    /// value + signed-display flag
    Val(Value, bool),
    Str(String),
}

/// Maximal printed width of an N-bit value in the given base — Verilog's
/// default column width for unsized format specifiers.
fn max_width(bits: u32, base: u32, signed: bool) -> usize {
    if bits == 0 {
        return 1;
    }
    match base {
        2 => bits as usize,
        8 => ((bits as usize) + 2) / 3,
        16 => ((bits as usize) + 3) / 4,
        _ => {
            // dollar_display.cxx maxWidth: sign digit + digit count, where
            // the digit count for <=64 bits is a closed-form for
            // digits(2^bits-1) even when signed, but for wide data the
            // signed count switches to digits(2^(bits-1)) (the magnitude)
            let sign_digit = if signed { 1 } else { 0 };
            let digits = if bits > 64 {
                let m = if signed {
                    Value::from_u64(bits, 1).shl((bits - 1) as u64, bits)
                } else {
                    Value::zero(bits).not(bits) // 2^bits-1
                };
                m.to_dec_string().len()
            } else {
                let factor: i64 = if bits > 12 { 2 - ((bits as i64 - 3) / 10) } else { 2 };
                ((bits as i64 + factor) / 3) as usize
            };
            sign_digit + digits
        }
    }
}

fn fmt_val(v: &Value, base: u32, zero_pad: bool, width: Option<usize>, signed: bool) -> String {
    let s = match base {
        2 => v.to_bin_string(),
        8 => v.to_oct_string(),
        16 => v.to_hex_string(),
        _ if signed && v.sign() => format!("-{}", v.neg(v.width).to_dec_string()),
        _ => v.to_dec_string(),
    };
    // %h/%b/%o strings are already max-width with leading zeros; trim per
    // explicit width or keep; %d needs padding up.
    let s = if base == 10 {
        s
    } else {
        s.trim_start_matches('0').to_string()
    };
    let s = if s.is_empty() { "0".to_string() } else { s };
    let w = width.unwrap_or(0);
    if s.len() >= w {
        s
    } else if zero_pad || base != 10 {
        format!("{}{}", "0".repeat(w - s.len()), s)
    } else {
        format!("{}{}", " ".repeat(w - s.len()), s)
    }
}

/// Format one $display-style call.  `default_base` is 10 for the plain
/// tasks, 16/8/2 for $displayh/$displayo/$displayb.
pub fn format_args(args: &[Arg], default_base: u32, now: u64, loc: &str) -> String {
    let mut out = String::new();
    let mut i = 0;
    while i < args.len() {
        match &args[i] {
            Arg::Str(fmt) => {
                i += 1;
                format_str(fmt, args, &mut i, &mut out, now, loc);
            }
            Arg::Val(v, sg) => {
                out.push_str(&fmt_val(
                    v, default_base, false,
                    Some(max_width(v.width, default_base, *sg)), *sg,
                ));
                i += 1;
            }
        }
    }
    out
}

/// A bit-packed string value back to text: bytes MSB-first, leading NUL
/// bytes skipped (how Bluesim reads a Bit#(n) used as a format).
fn unpack_str(v: &Value) -> String {
    let nbytes = ((v.width as usize) + 7) / 8;
    let mut s = String::new();
    for k in (0..nbytes).rev() {
        let b = v.lshr((k * 8) as u64, v.width).as_u64() as u8;
        if b == 0 && s.is_empty() {
            continue;
        }
        s.push(b as char);
    }
    s
}

/// $swrite/$sformat semantics: ONLY the first argument is a format (a
/// string, or a bit-packed string value); remaining string arguments are
/// literal text, remaining values format in the default base.
/// `fmt_first`: $sformat's first argument is always the format, even when
/// it is a bit-packed string value; $swrite formats a leading value as a
/// plain value instead.
pub fn format_sformat(
    args: &[Arg],
    default_base: u32,
    now: u64,
    loc: &str,
    fmt_first: bool,
) -> String {
    let mut out = String::new();
    let mut i = 0;
    match args.first() {
        Some(Arg::Str(f)) => {
            i = 1;
            let fmt = f.clone();
            format_str(&fmt, args, &mut i, &mut out, now, loc);
        }
        Some(Arg::Val(v, _)) if fmt_first => {
            i = 1;
            let fmt = unpack_str(v);
            format_str(&fmt, args, &mut i, &mut out, now, loc);
        }
        _ => {}
    }
    while i < args.len() {
        match &args[i] {
            Arg::Str(text) => out.push_str(text),
            Arg::Val(v, sg) => {
                out.push_str(&fmt_val(
                    v, default_base, false,
                    Some(max_width(v.width, default_base, *sg)), *sg,
                ));
            }
        }
        i += 1;
    }
    out
}

fn next_val(args: &[Arg], i: &mut usize) -> (Value, bool) {
    while *i < args.len() {
        let a = &args[*i];
        *i += 1;
        match a {
            Arg::Val(v, sg) => return (v.clone(), *sg),
            // a string consumed by a numeric spec formats as its bytes
            Arg::Str(st) => return (str_value(st), false),
        }
    }
    (Value::zero(1), false)
}

/// A string literal as a bit vector: bytes MSB-first (Verilog packing).
pub fn str_value(s: &str) -> Value {
    let bytes = s.as_bytes();
    let w = (bytes.len() as u32 * 8).max(8);
    let mut v = Value::zero(w);
    for &b in bytes {
        v = v.shl(8, w).or(&Value::from_u64(w, b as u64), w);
    }
    v
}

fn next_arg(args: &[Arg], i: &mut usize) -> Option<Arg> {
    if *i < args.len() {
        let a = args[*i].clone();
        *i += 1;
        Some(a)
    } else {
        None
    }
}

fn format_str(fmt: &str, args: &[Arg], i: &mut usize, out: &mut String, now: u64, loc: &str) {
    let mut cs = fmt.chars().peekable();
    while let Some(c) = cs.next() {
        if c == '\\' {
            match cs.next() {
                Some('n') => out.push('\n'),
                Some('t') => out.push('\t'),
                Some('\\') => out.push('\\'),
                Some('"') => out.push('"'),
                Some(other) => out.push(other),
                None => {}
            }
            continue;
        }
        if c != '%' {
            out.push(c);
            continue;
        }
        // parse %[0][width]spec
        let mut zero_pad = false;
        let mut width: Option<usize> = None;
        if cs.peek() == Some(&'0') {
            zero_pad = true;
            cs.next();
        }
        let mut wdigits = String::new();
        while let Some(d) = cs.peek() {
            if d.is_ascii_digit() {
                wdigits.push(*d);
                cs.next();
            } else {
                break;
            }
        }
        if !wdigits.is_empty() {
            width = Some(wdigits.parse().unwrap());
        }
        let spec = cs.next().unwrap_or('%');
        // "%0d" means minimal width (the 0 is a width of zero), while a
        // bare "%d" means the maximal column width
        let explicit_min = zero_pad && width.is_none();
        match spec.to_ascii_lowercase() {
            '%' => out.push('%'),
            'd' | 'u' => {
                let (v, sg) = next_val(args, i);
                let w = if explicit_min {
                    None
                } else {
                    width.or(Some(max_width(v.width, 10, sg)))
                };
                out.push_str(&fmt_val(&v, 10, false, w, sg));
            }
            'h' | 'x' => {
                let (v, _) = next_val(args, i);
                let w = if explicit_min {
                    None
                } else {
                    width.or(Some(max_width(v.width, 16, false)))
                };
                out.push_str(&fmt_val(&v, 16, true, w, false));
            }
            'o' => {
                let (v, _) = next_val(args, i);
                let w = if explicit_min {
                    None
                } else {
                    width.or(Some(max_width(v.width, 8, false)))
                };
                out.push_str(&fmt_val(&v, 8, true, w, false));
            }
            'b' => {
                let (v, _) = next_val(args, i);
                let w = if explicit_min {
                    None
                } else {
                    width.or(Some(max_width(v.width, 2, false)))
                };
                out.push_str(&fmt_val(&v, 2, true, w, false));
            }
            'c' => {
                let (v, _) = next_val(args, i);
                out.push((v.as_u64() & 0xFF) as u8 as char);
            }
            's' => match next_arg(args, i) {
                Some(Arg::Str(s)) => out.push_str(&s),
                Some(Arg::Val(v, _)) => {
                    // sized string: bytes, MSB first, skipping leading NULs
                    let n = ((v.width + 7) / 8) as usize;
                    let mut bytes = Vec::new();
                    for k in (0..n).rev() {
                        let b = v.extract((k * 8 + 7) as u64, (k * 8) as u64, 8).as_u64() as u8;
                        bytes.push(b);
                    }
                    let s: String = bytes
                        .into_iter()
                        .skip_while(|&b| b == 0)
                        .map(|b| b as char)
                        .collect();
                    out.push_str(&s);
                }
                None => {}
            },
            't' => {
                let (v, _) = next_val(args, i);
                let w = width.unwrap_or(20);
                let s = v.to_dec_string();
                if s.len() < w && !explicit_min {
                    out.push_str(&" ".repeat(w - s.len()));
                }
                out.push_str(&s);
            }
            '0' => { /* %0 alone: nothing */ }
            'm' => out.push_str(loc),
            other => panic!("trs-interp: unimplemented format %{other} (now={now})"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(w: u32, x: u64) -> Arg {
        Arg::Val(Value::from_u64(w, x), false)
    }

    #[test]
    fn verilog_column_widths() {
        // 8-bit %d pads to 3 columns
        let s = format_args(&[Arg::Str("v=%d".into()), v(8, 7)], 10, 0);
        assert_eq!(s, "v=  7");
        let s = format_args(&[Arg::Str("v=%0d".into()), v(8, 7)], 10, 0);
        assert_eq!(s, "v=7");
        let s = format_args(&[Arg::Str("v=%h".into()), v(8, 7)], 10, 0);
        assert_eq!(s, "v=07");
    }

    #[test]
    fn bare_args_default_base() {
        let s = format_args(&[v(4, 5)], 10, 0);
        assert_eq!(s, " 5"); // 4-bit max is 15 -> 2 columns
    }

    #[test]
    fn escapes() {
        let s = format_args(&[Arg::Str("a\\nb".into())], 10, 0);
        assert_eq!(s, "a\nb");
    }
}
