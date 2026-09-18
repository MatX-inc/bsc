//! Surfer translator for Bluesim waveform dumps.
//!
//! Bluesim records each signal's Bluespec type name in an FST dump, and
//! bluetcl's `module wavedebuginfo` writes the bit layout of every such
//! type (and which dumped signal is which source entity) to a JSON file.
//! With that file beside the dump as `<dump>.debug.json`, this plugin
//! shows a struct as its named fields, a tagged union as its active
//! variant and that variant's fields, an enum as its constructor name and
//! a vector as its elements.
//!
//! A signal's type is taken from the dump's type name when the dump has
//! one (FST), else from the debug information's entry for the signal's
//! path (VCD).

use std::collections::HashMap;
use std::sync::Mutex;

use extism_pdk::{host_fn, plugin_fn, FnResult, Json};
use serde::Deserialize;
use surfer_translation_types::plugin_types::TranslateParams;
use surfer_translation_types::translator::WaveSource;
use surfer_translation_types::{
    SubFieldTranslationResult, TranslationPreference, TranslationResult, ValueKind, ValueRepr,
    VariableInfo, VariableMeta, VariableValue,
};

#[host_fn]
extern "ExtismHost" {
    fn read_file(filename: String) -> Vec<u8>;
    fn file_exists(filename: String) -> bool;
}

// ---------------------------------------------------------------------
// The debug information, as `module wavedebuginfo` writes it

#[derive(Deserialize)]
struct DebugInfo {
    #[serde(default)]
    signals: Vec<Signal>,
    #[serde(default)]
    types: HashMap<String, TypeDesc>,
}

#[derive(Deserialize)]
struct Signal {
    synthpath: Vec<String>,
    #[serde(rename = "type")]
    ty: Option<String>,
}

#[derive(Deserialize, Clone)]
struct TypeDesc {
    kind: String,
    width: Option<u64>,
    layout: Option<String>,
    #[serde(default)]
    members: Vec<Member>,
    tag: Option<TagRange>,
    target: Option<String>,
    elem: Option<String>,
    length: Option<u64>,
    stride: Option<u64>,
}

#[derive(Deserialize, Clone)]
struct Member {
    name: String,
    #[serde(rename = "type")]
    ty: Option<String>,
    width: Option<u64>,
    lo: Option<u64>,
    hi: Option<u64>,
    tag: Option<u64>,
    value: Option<u64>,
}

#[derive(Deserialize, Clone)]
struct TagRange {
    lo: u64,
    hi: u64,
}

struct Loaded {
    info: DebugInfo,
    // dump path ("main.top.core.cell") -> the signal's type name
    type_by_path: HashMap<String, String>,
}

static LOADED: Mutex<Option<Loaded>> = Mutex::new(None);

fn load(wave_path: &str) {
    let path = format!("{wave_path}.debug.json");
    let mut slot = LOADED.lock().unwrap();
    *slot = None;
    let exists = unsafe { file_exists(path.clone()) }.unwrap_or(false);
    if !exists {
        return;
    }
    let Ok(bytes) = (unsafe { read_file(path) }) else {
        return;
    };
    let Ok(info) = serde_json::from_slice::<DebugInfo>(&bytes) else {
        return;
    };
    let type_by_path = info
        .signals
        .iter()
        .filter_map(|s| s.ty.clone().map(|t| (s.synthpath.join("."), t)))
        .collect();
    *slot = Some(Loaded { info, type_by_path });
}

// ---------------------------------------------------------------------
// Types

/// The type description a variable decodes with, aliases resolved
fn resolve<'a>(loaded: &'a Loaded, name: &str) -> Option<&'a TypeDesc> {
    let mut name: &str = name;
    for _ in 0..16 {
        let desc = loaded.info.types.get(name)?;
        match (desc.kind.as_str(), &desc.target) {
            ("alias", Some(target)) => name = target.as_str(),
            _ => return Some(desc),
        }
    }
    None
}

/// The type of a variable: the dump's own record of it, else the debug
/// information's entry for the variable's path
fn type_of(loaded: &Loaded, variable: &VariableMeta<(), ()>) -> Option<String> {
    if let Some(t) = &variable.variable_type_name {
        return Some(t.clone());
    }
    let path = variable.var.full_path().join(".");
    loaded.type_by_path.get(&path).cloned()
}

fn layout_known(desc: &TypeDesc) -> bool {
    desc.layout.as_deref() == Some("derived")
}

/// Whether the plugin has anything to add over Surfer's own formatting
fn decodes(loaded: &Loaded, name: &str) -> bool {
    match resolve(loaded, name) {
        Some(desc) => match desc.kind.as_str() {
            "struct" | "union" => layout_known(desc),
            "enum" => true,
            "vector" => desc.stride.is_some() && desc.length.is_some(),
            _ => false,
        },
        None => false,
    }
}

fn info_of(loaded: &Loaded, name: &str, depth: u32) -> VariableInfo {
    if depth > 32 {
        return VariableInfo::Bits;
    }
    let Some(desc) = resolve(loaded, name) else {
        return VariableInfo::Bits;
    };
    let sub = |ty: &Option<String>| match ty {
        Some(t) => info_of(loaded, t, depth + 1),
        None => VariableInfo::Bits,
    };
    match desc.kind.as_str() {
        "struct" | "union" if layout_known(desc) => VariableInfo::Compound {
            subfields: desc
                .members
                .iter()
                .map(|m| (m.name.clone(), sub(&m.ty)))
                .collect(),
        },
        "vector" => match (desc.length, &desc.elem) {
            (Some(n), Some(elem)) => VariableInfo::Compound {
                subfields: (0..n)
                    .map(|i| (format!("[{i}]"), info_of(loaded, elem, depth + 1)))
                    .collect(),
            },
            _ => VariableInfo::Bits,
        },
        _ => match desc.width {
            Some(1) => VariableInfo::Bool,
            _ => VariableInfo::Bits,
        },
    }
}

// ---------------------------------------------------------------------
// Values

/// The bits of a value, most significant first, exactly `width` of them
fn bits_of(value: &VariableValue, width: usize) -> String {
    let raw = match value {
        VariableValue::BigUint(v) => format!("{v:b}"),
        VariableValue::String(s) => s.clone(),
    };
    if raw.len() >= width {
        raw[raw.len() - width..].to_string()
    } else {
        let fill = raw.chars().next().filter(|c| matches!(c, 'x' | 'z' | 'u')).unwrap_or('0');
        let mut s: String = std::iter::repeat(fill).take(width - raw.len()).collect();
        s.push_str(&raw);
        s
    }
}

/// Bits [lo, hi) of `bits` (which lists bit width-1 first)
fn slice(bits: &str, lo: u64, hi: u64) -> String {
    let width = bits.len() as u64;
    if hi > width || lo > hi {
        return String::new();
    }
    bits[(width - hi) as usize..(width - lo) as usize].to_string()
}

fn as_number(bits: &str) -> Option<u64> {
    if bits.is_empty() {
        return Some(0);
    }
    u64::from_str_radix(bits, 2).ok()
}

fn raw(bits: &str) -> TranslationResult {
    let kind = if bits.chars().all(|c| c == '0' || c == '1') {
        ValueKind::Normal
    } else if bits.contains('x') {
        ValueKind::Undef
    } else if bits.contains('z') {
        ValueKind::HighImp
    } else {
        ValueKind::Warn
    };
    TranslationResult {
        val: ValueRepr::Bits(bits.len() as u32, bits.to_string()),
        subfields: vec![],
        kind,
    }
}

/// A value that is not there (the fields of a union's inactive arms)
fn not_present(loaded: &Loaded, name: &str, depth: u32) -> TranslationResult {
    let subfields = match resolve(loaded, name) {
        Some(desc) if depth < 32 => match desc.kind.as_str() {
            "struct" | "union" if layout_known(desc) => desc
                .members
                .iter()
                .map(|m| SubFieldTranslationResult {
                    name: m.name.clone(),
                    result: match &m.ty {
                        Some(t) => not_present(loaded, t, depth + 1),
                        None => not_present_leaf(),
                    },
                })
                .collect(),
            "vector" => match (desc.length, &desc.elem) {
                (Some(n), Some(elem)) => (0..n)
                    .map(|i| SubFieldTranslationResult {
                        name: format!("[{i}]"),
                        result: not_present(loaded, elem, depth + 1),
                    })
                    .collect(),
                _ => vec![],
            },
            _ => vec![],
        },
        _ => vec![],
    };
    TranslationResult {
        val: ValueRepr::NotPresent,
        subfields,
        kind: ValueKind::Normal,
    }
}

fn not_present_leaf() -> TranslationResult {
    TranslationResult {
        val: ValueRepr::NotPresent,
        subfields: vec![],
        kind: ValueKind::Normal,
    }
}

fn decode(loaded: &Loaded, name: &str, bits: &str, depth: u32) -> TranslationResult {
    let Some(desc) = resolve(loaded, name) else {
        return raw(bits);
    };
    if depth > 32 || !bits.chars().all(|c| c == '0' || c == '1') {
        // an unknown or partly-unknown value: shown raw, with its
        // structure marked absent
        let mut r = raw(bits);
        r.subfields = not_present(loaded, name, depth).subfields;
        return r;
    }
    let member_bits = |m: &Member| match (m.lo, m.hi) {
        (Some(lo), Some(hi)) => slice(bits, lo, hi),
        _ => String::new(),
    };
    let member_value = |m: &Member, b: &str| match &m.ty {
        Some(t) => decode(loaded, t, b, depth + 1),
        None => raw(b),
    };
    match desc.kind.as_str() {
        "struct" if layout_known(desc) => TranslationResult {
            val: ValueRepr::Struct,
            subfields: desc
                .members
                .iter()
                .map(|m| SubFieldTranslationResult {
                    name: m.name.clone(),
                    result: member_value(m, &member_bits(m)),
                })
                .collect(),
            kind: ValueKind::Normal,
        },
        "union" if layout_known(desc) => {
            let tag = desc
                .tag
                .as_ref()
                .and_then(|t| as_number(&slice(bits, t.lo, t.hi)));
            let active = tag.and_then(|v| desc.members.iter().position(|m| m.tag == Some(v)));
            let subfields = desc
                .members
                .iter()
                .enumerate()
                .map(|(i, m)| SubFieldTranslationResult {
                    name: m.name.clone(),
                    result: if Some(i) == active {
                        if m.width == Some(0) {
                            TranslationResult {
                                val: ValueRepr::String(String::new()),
                                subfields: vec![],
                                kind: ValueKind::Normal,
                            }
                        } else {
                            member_value(m, &member_bits(m))
                        }
                    } else {
                        match &m.ty {
                            Some(t) => not_present(loaded, t, depth + 1),
                            None => not_present_leaf(),
                        }
                    },
                })
                .collect();
            match active {
                Some(idx) => TranslationResult {
                    val: ValueRepr::Enum {
                        idx,
                        name: desc.members[idx].name.clone(),
                    },
                    subfields,
                    kind: ValueKind::Normal,
                },
                // a tag encoding no constructor uses
                None => TranslationResult {
                    val: ValueRepr::String(format!("?tag {}", tag.map_or("-".to_string(), |v| v.to_string()))),
                    subfields,
                    kind: ValueKind::Warn,
                },
            }
        }
        "enum" => match as_number(bits)
            .and_then(|v| desc.members.iter().find(|m| m.value == Some(v)))
        {
            Some(m) => TranslationResult {
                val: ValueRepr::String(m.name.clone()),
                subfields: vec![],
                kind: ValueKind::Normal,
            },
            None => TranslationResult {
                val: ValueRepr::String(format!("?{bits}")),
                subfields: vec![],
                kind: ValueKind::Warn,
            },
        },
        "vector" => match (desc.length, desc.stride, &desc.elem) {
            (Some(n), Some(stride), Some(elem)) => TranslationResult {
                val: ValueRepr::Array,
                subfields: (0..n)
                    .map(|i| SubFieldTranslationResult {
                        name: format!("[{i}]"),
                        result: decode(loaded, elem, &slice(bits, i * stride, (i + 1) * stride), depth + 1),
                    })
                    .collect(),
                kind: ValueKind::Normal,
            },
            _ => raw(bits),
        },
        _ => raw(bits),
    }
}

// ---------------------------------------------------------------------
// The plugin interface

#[plugin_fn]
pub fn name() -> FnResult<String> {
    Ok("Bluespec".to_string())
}

#[plugin_fn]
pub fn set_wave_source(Json(source): Json<Option<WaveSource>>) -> FnResult<()> {
    match source {
        Some(WaveSource::File(path)) => load(&path),
        _ => *LOADED.lock().unwrap() = None,
    }
    Ok(())
}

#[plugin_fn]
pub fn translates(variable: VariableMeta<(), ()>) -> FnResult<TranslationPreference> {
    let slot = LOADED.lock().unwrap();
    let Some(loaded) = slot.as_ref() else {
        return Ok(TranslationPreference::No);
    };
    Ok(match type_of(loaded, &variable) {
        Some(t) if decodes(loaded, &t) => TranslationPreference::Prefer,
        _ => TranslationPreference::No,
    })
}

#[plugin_fn]
pub fn variable_info(variable: VariableMeta<(), ()>) -> FnResult<VariableInfo> {
    let slot = LOADED.lock().unwrap();
    let Some(loaded) = slot.as_ref() else {
        return Ok(VariableInfo::Bits);
    };
    Ok(match type_of(loaded, &variable) {
        Some(t) => info_of(loaded, &t, 0),
        None => VariableInfo::Bits,
    })
}

#[plugin_fn]
pub fn translate(TranslateParams { variable, value }: TranslateParams) -> FnResult<TranslationResult> {
    let slot = LOADED.lock().unwrap();
    let width = variable.num_bits.unwrap_or(0) as usize;
    let bits = bits_of(&value, width);
    let Some(loaded) = slot.as_ref() else {
        return Ok(raw(&bits));
    };
    Ok(match type_of(loaded, &variable) {
        Some(t) => decode(loaded, &t, &bits, 0),
        None => raw(&bits),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    // The exporter's own test output, as the fixture: mkWaveTypesCore's
    // debug information (the Tcl transcript precedes the JSON)
    fn fixture() -> Loaded {
        let text = std::fs::read_to_string(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../testsuite/bsc.bluetcl/commands/wavedebuginfo.tcl.bluetcl-out.expected"
        ))
        .unwrap();
        let json = &text[text.find('{').unwrap()..];
        let info: DebugInfo = serde_json::from_str(json).unwrap();
        let type_by_path = info
            .signals
            .iter()
            .filter_map(|s| s.ty.clone().map(|t| (s.synthpath.join("."), t)))
            .collect();
        Loaded { info, type_by_path }
    }

    fn flat(r: &TranslationResult) -> String {
        let own = match &r.val {
            ValueRepr::Bits(_, b) => b.clone(),
            ValueRepr::String(s) => format!("\"{s}\""),
            ValueRepr::Struct => "struct".into(),
            ValueRepr::Array => "array".into(),
            ValueRepr::Enum { name, .. } => format!("enum {name}"),
            ValueRepr::NotPresent => "-".into(),
            _ => "?".into(),
        };
        let subs: Vec<String> = r
            .subfields
            .iter()
            .map(|s| format!("{}={}", s.name, flat(&s.result)))
            .collect();
        if subs.is_empty() {
            own
        } else {
            format!("{own}{{{}}}", subs.join(", "))
        }
    }

    #[test]
    fn struct_fields_first_field_high() {
        let l = fixture();
        // Pixel { x = 1; y = 2; color = Green }
        let r = decode(&l, "WaveTypes::Pixel", "000000010000001001", 0);
        assert_eq!(
            flat(&r),
            "struct{x=00000001, y=00000010, color=\"Green\"}"
        );
    }

    #[test]
    fn union_tag_high_payload_low() {
        let l = fixture();
        // Idx 5: tag 1 in the top two bits, the 16-bit payload right-aligned
        let r = decode(&l, "WaveTypes::Cell", "01000000000000000101", 0);
        assert_eq!(
            flat(&r),
            "enum Idx{Px=-{x=-, y=-, color=-}, Idx=0000000000000101, Empty=-}"
        );
        // Empty: a void arm
        let r = decode(&l, "WaveTypes::Cell", "10000000000000000000", 0);
        assert!(flat(&r).starts_with("enum Empty{"));
        // a tag no constructor uses
        let r = decode(&l, "WaveTypes::Cell", "11000000000000000000", 0);
        assert!(matches!(r.kind, ValueKind::Warn));
    }

    #[test]
    fn maybe_and_vector() {
        let l = fixture();
        let r = decode(&l, "Maybe#(UInt#(16))", "10000000000000011", 0);
        assert_eq!(flat(&r), "enum Valid{Invalid=\"\", Valid=0000000000000011}".replace("Invalid=\"\"", "Invalid=-"));
        // [Red, Green, Blue, Red] with element 0 lowest
        let r = decode(&l, "Vector::Vector#(4, WaveTypes::Color)", "00100100", 0);
        assert_eq!(
            flat(&r),
            "array{[0]=\"Red\", [1]=\"Green\", [2]=\"Blue\", [3]=\"Red\"}"
        );
    }

    #[test]
    fn unknown_bits_stay_raw() {
        let l = fixture();
        let r = decode(&l, "WaveTypes::Pixel", "xxxxxxxxxxxxxxxxxx", 0);
        assert!(matches!(r.kind, ValueKind::Undef));
        assert_eq!(r.subfields.len(), 3);
    }

    #[test]
    fn info_and_preference() {
        let l = fixture();
        assert!(decodes(&l, "WaveTypes::Cell"));
        assert!(!decodes(&l, "UInt#(8)"));
        match info_of(&l, "WaveTypes::Pixel", 0) {
            VariableInfo::Compound { subfields } => assert_eq!(subfields.len(), 3),
            _ => panic!("struct should be compound"),
        }
        assert_eq!(l.type_by_path.get("main.top.cell").map(String::as_str), Some("WaveTypes::Cell"));
    }
}
