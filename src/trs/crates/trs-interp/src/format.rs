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
    /// real-valued argument (the C++ 'r' annotation)
    Real(f64),
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
/// tasks, 16/8/2 for $displayh/$displayo/$displayb.  Output errors
/// (dollar_display's add_error) accumulate in `errs`; the caller prints
/// them after the task output, newest first (Target's LIFO list).
pub fn format_args(
    args: &[Arg],
    default_base: u32,
    now: u64,
    loc: &str,
    errs: &mut Vec<String>,
) -> String {
    let mut out = String::new();
    let mut i = 0;
    while i < args.len() {
        match &args[i] {
            Arg::Str(fmt) => {
                i += 1;
                let fmt = fmt.clone();
                format_str(&fmt, args, &mut i, &mut out, now, loc, errs);
            }
            Arg::Val(v, sg) => {
                out.push_str(&fmt_val(
                    v, default_base, false,
                    Some(max_width(v.width, default_base, *sg)), *sg,
                ));
                i += 1;
            }
            Arg::Real(r) => {
                // a bare real hits the integer default format: error and
                // print the (signed long long) conversion (fill_tValue)
                errs.push("unexpected real number argument\n".to_string());
                let v = Value::from_u64(64, (*r as i64) as u64);
                out.push_str(&fmt_val(
                    &v, default_base, false,
                    Some(max_width(64, default_base, true)), true,
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
    errs: &mut Vec<String>,
) -> String {
    let mut out = String::new();
    let mut i = 0;
    match args.first() {
        Some(Arg::Str(f)) => {
            i = 1;
            let fmt = f.clone();
            format_str(&fmt, args, &mut i, &mut out, now, loc, errs);
        }
        Some(Arg::Val(v, _)) if fmt_first => {
            i = 1;
            let fmt = unpack_str(v);
            format_str(&fmt, args, &mut i, &mut out, now, loc, errs);
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
            Arg::Real(r) => {
                errs.push("unexpected real number argument\n".to_string());
                let v = Value::from_u64(64, (*r as i64) as u64);
                out.push_str(&fmt_val(
                    &v, default_base, false,
                    Some(max_width(64, default_base, true)), true,
                ));
            }
        }
        i += 1;
    }
    out
}

fn next_val(args: &[Arg], i: &mut usize, errs: &mut Vec<String>) -> (Value, bool) {
    while *i < args.len() {
        let a = &args[*i];
        *i += 1;
        match a {
            Arg::Val(v, sg) => return (v.clone(), *sg),
            // a string consumed by a numeric spec formats as its bytes
            Arg::Str(st) => return (str_value(st), false),
            // a real consumed by an integer spec: error, then the
            // (signed long long) conversion at 64 bits (fill_tValue)
            Arg::Real(r) => {
                errs.push("unexpected real number argument\n".to_string());
                return (Value::from_u64(64, (*r as i64) as u64), true);
            }
        }
    }
    (Value::zero(1), false)
}

/// Consume one argument for a real (%e/%f/%g) spec: non-real arguments
/// error and convert as best as possible (tValueToDouble).
fn next_double(args: &[Arg], i: &mut usize, errs: &mut Vec<String>) -> f64 {
    while *i < args.len() {
        let a = &args[*i];
        *i += 1;
        match a {
            Arg::Real(r) => return *r,
            Arg::Val(v, sg) => {
                errs.push(
                    "expected real argument, found non-real argument\n".to_string(),
                );
                if v.width > 64 {
                    return 0.0; // tValueToDouble punts on wide data
                }
                if v.width == 1 {
                    return if v.as_u64() & 1 == 1 { 1.0 } else { 0.0 };
                }
                if *sg {
                    // sign-extend to i64
                    let x = v.as_u64();
                    let sh = 64 - v.width;
                    return (((x << sh) as i64) >> sh) as f64;
                }
                return v.as_u64() as f64;
            }
            Arg::Str(_) => {
                errs.push(
                    "expected real argument, found non-real argument\n".to_string(),
                );
                return 0.0; // string fills a wide tValue; wide -> 0
            }
        }
    }
    0.0
}

/// C printf %f/%e/%g for the parsed spec, plus space padding to `width`.
fn c_format_real(spec: char, width: Option<usize>, prec: Option<usize>, v: f64) -> String {
    fn c_e(v: f64, prec: usize) -> String {
        let s = format!("{:.*e}", prec, v);
        let (m, e) = s.split_once('e').unwrap();
        let exp: i32 = e.parse().unwrap();
        format!("{}e{}{:02}", m, if exp < 0 { '-' } else { '+' }, exp.abs())
    }
    fn trim_g(s: &str) -> String {
        if s.contains('.') {
            s.trim_end_matches('0').trim_end_matches('.').to_string()
        } else {
            s.to_string()
        }
    }
    let s = match spec {
        'f' => format!("{:.*}", prec.unwrap_or(6), v),
        'e' => c_e(v, prec.unwrap_or(6)),
        _ => {
            // %g: %e if exp < -4 or exp >= P, else %f with P-1-exp
            // decimals; trailing zeros removed
            let p = prec.unwrap_or(6).max(1);
            let es = format!("{:.*e}", p - 1, v);
            let exp: i32 = es.split_once('e').unwrap().1.parse().unwrap();
            if exp < -4 || exp >= p as i32 {
                let m = trim_g(es.split_once('e').unwrap().0);
                format!("{}e{}{:02}", m, if exp < 0 { '-' } else { '+' }, exp.abs())
            } else {
                let decs = (p as i32 - 1 - exp).max(0) as usize;
                trim_g(&format!("{:.*}", decs, v))
            }
        }
    };
    let w = width.unwrap_or(0);
    if s.len() < w {
        format!("{}{}", " ".repeat(w - s.len()), s)
    } else {
        s
    }
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

fn format_str(
    fmt: &str,
    args: &[Arg],
    i: &mut usize,
    out: &mut String,
    now: u64,
    loc: &str,
    errs: &mut Vec<String>,
) {
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
        // optional .precision (real formats)
        let mut prec: Option<usize> = None;
        if cs.peek() == Some(&'.') {
            cs.next();
            let mut pdigits = String::new();
            while let Some(d) = cs.peek() {
                if d.is_ascii_digit() {
                    pdigits.push(*d);
                    cs.next();
                } else {
                    break;
                }
            }
            prec = Some(pdigits.parse().unwrap_or(0));
        }
        let spec = cs.next().unwrap_or('%');
        // "%0d" means minimal width (the 0 is a width of zero), while a
        // bare "%d" means the maximal column width
        let explicit_min = zero_pad && width.is_none();
        match spec.to_ascii_lowercase() {
            '%' => out.push('%'),
            'd' | 'u' => {
                let (v, sg) = next_val(args, i, errs);
                let w = if explicit_min {
                    None
                } else {
                    width.or(Some(max_width(v.width, 10, sg)))
                };
                out.push_str(&fmt_val(&v, 10, false, w, sg));
            }
            'h' | 'x' => {
                let (v, _) = next_val(args, i, errs);
                let w = if explicit_min {
                    None
                } else {
                    width.or(Some(max_width(v.width, 16, false)))
                };
                out.push_str(&fmt_val(&v, 16, true, w, false));
            }
            'o' => {
                let (v, _) = next_val(args, i, errs);
                let w = if explicit_min {
                    None
                } else {
                    width.or(Some(max_width(v.width, 8, false)))
                };
                out.push_str(&fmt_val(&v, 8, true, w, false));
            }
            'b' => {
                let (v, _) = next_val(args, i, errs);
                let w = if explicit_min {
                    None
                } else {
                    width.or(Some(max_width(v.width, 2, false)))
                };
                out.push_str(&fmt_val(&v, 2, true, w, false));
            }
            'c' => {
                let (v, _) = next_val(args, i, errs);
                out.push((v.as_u64() & 0xFF) as u8 as char);
            }
            's' => match next_arg(args, i) {
                Some(Arg::Str(s)) => out.push_str(&s),
                Some(Arg::Real(r)) => {
                    errs.push("unexpected real number argument\n".to_string());
                    let v = Value::from_u64(64, (r as i64) as u64);
                    let n = ((v.width + 7) / 8) as usize;
                    let mut bytes = Vec::new();
                    for k in (0..n).rev() {
                        let b =
                            v.extract((k * 8 + 7) as u64, (k * 8) as u64, 8).as_u64() as u8;
                        bytes.push(b);
                    }
                    let s: String = bytes
                        .into_iter()
                        .skip_while(|&b| b == 0)
                        .map(|b| b as char)
                        .collect();
                    out.push_str(&s);
                }
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
                let (v, _) = next_val(args, i, errs);
                let w = width.unwrap_or(20);
                let s = v.to_dec_string();
                if s.len() < w && !explicit_min {
                    out.push_str(&" ".repeat(w - s.len()));
                }
                out.push_str(&s);
            }
            'f' | 'e' | 'g' => {
                let v = next_double(args, i, errs);
                out.push_str(&c_format_real(spec.to_ascii_lowercase(), width, prec, v));
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
        let mut e = Vec::new();
        let s = format_args(&[Arg::Str("v=%d".into()), v(8, 7)], 10, 0, "", &mut e);
        assert_eq!(s, "v=  7");
        let s = format_args(&[Arg::Str("v=%0d".into()), v(8, 7)], 10, 0, "", &mut e);
        assert_eq!(s, "v=7");
        let s = format_args(&[Arg::Str("v=%h".into()), v(8, 7)], 10, 0, "", &mut e);
        assert_eq!(s, "v=07");
    }

    #[test]
    fn bare_args_default_base() {
        let mut e = Vec::new();
        let s = format_args(&[v(4, 5)], 10, 0, "", &mut e);
        assert_eq!(s, " 5"); // 4-bit max is 15 -> 2 columns
    }

    #[test]
    fn escapes() {
        let mut e = Vec::new();
        let s = format_args(&[Arg::Str("a\\nb".into())], 10, 0, "", &mut e);
        assert_eq!(s, "a\nb");
    }
}
