//! $display-family formatting, matching Bluesim's engine
//! (`dollar_display.cxx`): every string argument is a format string
//! consuming subsequent arguments; non-string arguments print in the
//! task's default base; Verilog column rules (%d space-pads to the
//! signal's maximal decimal width, %h zero-pads to ceil(bits/4), ...).

use crate::value::Value;

#[derive(Debug, Clone)]
pub enum Arg {
    Val(Value),
    Str(String),
}

/// Maximal printed width of an N-bit value in the given base — Verilog's
/// default column width for unsized format specifiers.
fn max_width(bits: u32, base: u32) -> usize {
    if bits == 0 {
        return 1;
    }
    match base {
        2 => bits as usize,
        8 => ((bits as usize) + 2) / 3,
        16 => ((bits as usize) + 3) / 4,
        _ => Value::zero(bits).not(bits).to_dec_string().len(), // 2^bits-1
    }
}

fn fmt_val(v: &Value, base: u32, zero_pad: bool, width: Option<usize>) -> String {
    let s = match base {
        2 => v.to_bin_string(),
        8 => v.to_oct_string(),
        16 => v.to_hex_string(),
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
pub fn format_args(args: &[Arg], default_base: u32, now: u64) -> String {
    let mut out = String::new();
    let mut i = 0;
    while i < args.len() {
        match &args[i] {
            Arg::Str(fmt) => {
                i += 1;
                format_str(fmt, args, &mut i, &mut out, now);
            }
            Arg::Val(v) => {
                out.push_str(&fmt_val(v, default_base, false, Some(max_width(v.width, default_base))));
                i += 1;
            }
        }
    }
    out
}

fn next_val(args: &[Arg], i: &mut usize) -> Value {
    while *i < args.len() {
        let a = &args[*i];
        *i += 1;
        match a {
            Arg::Val(v) => return v.clone(),
            Arg::Str(_) => continue, // strings consumed as %s only
        }
    }
    Value::zero(1)
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

fn format_str(fmt: &str, args: &[Arg], i: &mut usize, out: &mut String, now: u64) {
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
                let v = next_val(args, i);
                let w = if explicit_min {
                    None
                } else {
                    width.or(Some(max_width(v.width, 10)))
                };
                out.push_str(&fmt_val(&v, 10, false, w));
            }
            'h' | 'x' => {
                let v = next_val(args, i);
                let w = if explicit_min {
                    None
                } else {
                    width.or(Some(max_width(v.width, 16)))
                };
                out.push_str(&fmt_val(&v, 16, true, w));
            }
            'o' => {
                let v = next_val(args, i);
                let w = if explicit_min {
                    None
                } else {
                    width.or(Some(max_width(v.width, 8)))
                };
                out.push_str(&fmt_val(&v, 8, true, w));
            }
            'b' => {
                let v = next_val(args, i);
                let w = if explicit_min {
                    None
                } else {
                    width.or(Some(max_width(v.width, 2)))
                };
                out.push_str(&fmt_val(&v, 2, true, w));
            }
            'c' => {
                let v = next_val(args, i);
                out.push((v.as_u64() & 0xFF) as u8 as char);
            }
            's' => match next_arg(args, i) {
                Some(Arg::Str(s)) => out.push_str(&s),
                Some(Arg::Val(v)) => {
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
                let v = next_val(args, i);
                let w = width.unwrap_or(20);
                let s = v.to_dec_string();
                if s.len() < w && !explicit_min {
                    out.push_str(&" ".repeat(w - s.len()));
                }
                out.push_str(&s);
            }
            '0' => { /* %0 alone: nothing */ }
            other => panic!("trs-interp: unimplemented format %{other} (now={now})"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(w: u32, x: u64) -> Arg {
        Arg::Val(Value::from_u64(w, x))
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
