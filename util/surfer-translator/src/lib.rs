//! Surfer translator for Bluesim waveform dumps.
//!
//! Bluesim records each signal's Bluespec type name in an FST dump, and
//! bluetcl's `module wavedebuginfo` writes the bit layout of every such
//! type (and which dumped signal is which source entity) to a JSON file.
//! With that file beside the dump as `<dump>.debug.json`, this plugin
//! shows a struct as its named fields, a tagged union as its active
//! variant and that variant's fields, an enum as its constructor name and
//! a vector as its elements.  A layout is a bit range per member where
//! the type's `Bits` instance is that simple, and otherwise an expression
//! over the packed value, which the plugin evaluates.
//!
//! A signal's type is taken from the dump's type name when the dump has
//! one (FST), else from the debug information's entry for the signal's
//! path (VCD).

use std::collections::HashMap;
use std::sync::Mutex;

use extism_pdk::{host_fn, plugin_fn, FnResult, Json};
use serde::Deserialize;
use surfer_translation_types::plugin_types::TranslateParams;
use surfer_translation_types::translator::{VariableNameInfo, WaveSource};
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
// The debug information, as the exporter writes it: a bundle beside the
// dump, `<dump>.debug/`, whose `index.json` names the top module and one
// fragment per synthesized module the design reaches

#[derive(Deserialize)]
struct Index {
    top: String,
    #[serde(default)]
    fragments: Vec<String>,
}

/// One synthesized module's debug information
#[derive(Deserialize)]
struct Fragment {
    module: String,
    #[serde(default)]
    children: Vec<Child>,
    #[serde(default)]
    signals: Vec<Signal>,
    #[serde(default)]
    types: HashMap<String, TypeDesc>,
}

/// An instance of a synthesized module: its name in the synthesized
/// hierarchy, its path in the source (the instance names down through
/// the inlined modules), and the module whose fragment describes it
#[derive(Deserialize)]
struct Child {
    instance: String,
    #[serde(default)]
    path: Vec<String>,
    module: String,
}

/// One of a module's own signals: its path in a dump of the synthesized
/// hierarchy and in the source (also its path in a dump made with
/// -wave-source-hierarchy), both relative to the module's own scope, and
/// what it is.  The source position the debug information also carries
/// is not used here.
#[derive(Deserialize)]
struct Signal {
    synthpath: Vec<String>,
    #[serde(default)]
    bsvpath: Vec<String>,
    #[serde(rename = "type")]
    ty: Option<String>,
    kind: Option<String>,
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

/// A member's place in the packed value.  Its own bits are the range
/// `lo..hi` or the expression `bits`; a union arm or enum constant is
/// selected by the type's tag bits equalling `tag` or one of `tags` (for
/// an enum, by the whole value equalling `value` or one of `values`), or
/// else by the expression `when` being 1.
#[derive(Deserialize, Clone)]
struct Member {
    name: String,
    #[serde(rename = "type")]
    ty: Option<String>,
    width: Option<u64>,
    lo: Option<u64>,
    hi: Option<u64>,
    bits: Option<Expr>,
    tag: Option<u64>,
    #[serde(default)]
    tags: Vec<u64>,
    value: Option<u64>,
    #[serde(default)]
    values: Vec<u64>,
    when: Option<Expr>,
}

/// An expression over the packed value, as the exporter writes it: the
/// operation, its operands, a constant's bits, an extract's range, and
/// the result width where the operands do not imply it
#[derive(Deserialize, Clone)]
struct Expr {
    op: String,
    width: Option<u64>,
    bits: Option<String>,
    hi: Option<u64>,
    lo: Option<u64>,
    #[serde(default)]
    args: Vec<Expr>,
}

#[derive(Deserialize, Clone)]
struct TagRange {
    lo: u64,
    hi: u64,
}

/// A module's fragment, indexed by the paths a dump spells: its signals
/// under the synthesized and the source hierarchy's names, its children
/// by instance name and by source path
struct ModuleInfo {
    signals: Vec<Signal>,
    by_synth: HashMap<String, usize>,
    by_bsv: HashMap<String, usize>,
    children: Vec<Child>,
    child_by_instance: HashMap<String, usize>,
    child_by_path: HashMap<String, usize>,
}

impl ModuleInfo {
    fn new(signals: Vec<Signal>, children: Vec<Child>) -> ModuleInfo {
        let mut by_synth = HashMap::new();
        let mut by_bsv = HashMap::new();
        for (i, s) in signals.iter().enumerate() {
            by_synth.entry(s.synthpath.join(".")).or_insert(i);
            if !s.bsvpath.is_empty() {
                by_bsv.entry(s.bsvpath.join(".")).or_insert(i);
            }
        }
        let mut child_by_instance = HashMap::new();
        let mut child_by_path = HashMap::new();
        for (i, c) in children.iter().enumerate() {
            child_by_instance.entry(c.instance.clone()).or_insert(i);
            if !c.path.is_empty() {
                child_by_path.entry(c.path.join(".")).or_insert(i);
            }
        }
        ModuleInfo {
            signals,
            by_synth,
            by_bsv,
            children,
            child_by_instance,
            child_by_path,
        }
    }
}

struct Loaded {
    top: String,
    modules: HashMap<String, ModuleInfo>,
    // every fragment's types by name; a name two fragments describe
    // keeps the first description
    types: HashMap<String, TypeDesc>,
    // dump path -> the module and signal it names, remembered: the
    // plugin is asked about a variable at every column
    located: HashMap<String, Option<(String, usize)>>,
    // the dump scopes found to hold the top module, tried first
    roots: Vec<Vec<String>>,
}

// The plugin's state, behind a lock that is never held across a call
// into the host: a host error traps the plugin without unwinding, so a
// lock held at that moment would stay held for the rest of the session.
static LOADED: Mutex<Option<Loaded>> = Mutex::new(None);

fn load(wave_path: &str) {
    let dir = format!("{wave_path}.debug");
    let loaded = host_read_file(&format!("{dir}/index.json"))
        .and_then(|bytes| serde_json::from_slice::<Index>(&bytes).ok())
        .map(|index| {
            let fragments = index
                .fragments
                .iter()
                .filter_map(|name| host_read_file(&format!("{dir}/{name}")))
                .filter_map(|bytes| serde_json::from_slice::<Fragment>(&bytes).ok())
                .collect();
            Loaded::new(index.top, fragments)
        });
    *LOADED.lock().unwrap() = loaded;
}

impl Loaded {
    fn new(top: String, mut fragments: Vec<Fragment>) -> Loaded {
        // the top module's fragment first, so its types take precedence
        fragments.sort_by_key(|f| f.module != top);
        let mut modules = HashMap::new();
        let mut types = HashMap::new();
        for f in fragments {
            for (name, desc) in f.types {
                types.entry(name).or_insert(desc);
            }
            modules.insert(f.module, ModuleInfo::new(f.signals, f.children));
        }
        Loaded {
            top,
            modules,
            types,
            located: HashMap::new(),
            roots: vec![],
        }
    }

    /// The module and signal a dump path names, found by walking the
    /// instance tree from the top module under whichever dump scope
    /// holds it: a scope already found to, else each prefix of the path
    /// in turn (bsc's simulation harness puts the top module at
    /// `main.top`; a testbench or a formal wrapper has its own)
    fn locate(&mut self, path: &[String]) -> Option<(String, usize)> {
        let key = path.join(".");
        if let Some(found) = self.located.get(&key) {
            return found.clone();
        }
        let mut found = None;
        for root in &self.roots {
            if path.len() > root.len() && path[..root.len()] == root[..] {
                found = self.walk(&self.top, &path[root.len()..]);
                if found.is_some() {
                    break;
                }
            }
        }
        if found.is_none() {
            for r in 0..path.len() {
                found = self.walk(&self.top, &path[r..]);
                if found.is_some() {
                    let root = path[..r].to_vec();
                    if !self.roots.contains(&root) {
                        self.roots.push(root);
                    }
                    break;
                }
            }
        }
        self.located.insert(key, found.clone());
        found
    }

    /// `rest` under the scope of an instance of `module`: one of the
    /// module's own signals, or inside a child's scope, named by its
    /// instance in the synthesized hierarchy or by its source path
    fn walk(&self, module: &str, rest: &[String]) -> Option<(String, usize)> {
        let info = self.modules.get(module)?;
        if rest.is_empty() {
            return None;
        }
        let key = rest.join(".");
        if let Some(&i) = info.by_synth.get(&key).or_else(|| info.by_bsv.get(&key)) {
            return Some((module.to_string(), i));
        }
        for k in 1..rest.len() {
            let prefix = rest[..k].join(".");
            let child = info
                .child_by_path
                .get(&prefix)
                .or_else(|| (k == 1).then(|| info.child_by_instance.get(&prefix)).flatten());
            if let Some(&c) = child {
                if let Some(found) = self.walk(&info.children[c].module, &rest[k..]) {
                    return Some(found);
                }
            }
        }
        None
    }

    fn signal_at(&mut self, path: &[String]) -> Option<&Signal> {
        let (module, i) = self.locate(path)?;
        self.modules.get(&module).map(|m| &m.signals[i])
    }

    fn type_at(&mut self, path: &[String]) -> Option<String> {
        self.signal_at(path).and_then(|s| s.ty.clone())
    }
}

// ---------------------------------------------------------------------
// Types

/// The type description a variable decodes with, aliases resolved
fn resolve<'a>(loaded: &'a Loaded, name: &str) -> Option<&'a TypeDesc> {
    let mut name: &str = name;
    for _ in 0..16 {
        let desc = loaded.types.get(name)?;
        match (desc.kind.as_str(), &desc.target) {
            ("alias", Some(target)) => name = target.as_str(),
            _ => return Some(desc),
        }
    }
    None
}

/// The type of a variable: the dump's own record of it, else the debug
/// information's entry for the variable's path
fn type_of(loaded: &mut Loaded, variable: &VariableMeta<(), ()>) -> Option<String> {
    if let Some(t) = &variable.variable_type_name {
        return Some(t.clone());
    }
    loaded.type_at(&variable.var.full_path())
}

/// A file's bytes, through the host, None where there is no such file;
/// the native test build has no host and reads the file system instead
#[cfg(not(test))]
fn host_read_file(path: &str) -> Option<Vec<u8>> {
    // The host reports a missing file as an error, which traps the
    // plugin, so only a file that exists is read
    if !unsafe { file_exists(path.to_string()) }.unwrap_or(false) {
        return None;
    }
    unsafe { read_file(path.to_string()) }.ok()
}

#[cfg(test)]
fn host_read_file(path: &str) -> Option<Vec<u8>> {
    std::fs::read(path).ok()
}

// ---------------------------------------------------------------------
// Names

/// A compiler-introduced value: bsc names its intermediate results
/// `<name>__h<n>` (a value lifted out of a rule) or `<name>__d<n>` (an
/// unnamed subexpression)
fn is_compiler_temporary(name: &str) -> bool {
    ["__h", "__d"].iter().any(|marker| {
        name.match_indices(marker)
            .any(|(i, _)| name[i + marker.len()..].starts_with(|c: char| c.is_ascii_digit()))
    })
}

/// How the variable list should present a variable: the design's own
/// signals ahead of everything else, and a compiler temporary hidden.
/// Variables keep their dumped names: a Bluespec signal's declaration
/// site, often inside an inlined library module, does not identify it
/// the way the flattened name does.
fn name_info(loaded: &mut Loaded, variable: &VariableMeta<(), ()>) -> Option<VariableNameInfo> {
    let hidden = VariableNameInfo {
        true_name: None,
        priority: Some(-1),
        visible: Some(false),
    };
    let Some(signal) = loaded.signal_at(&variable.var.full_path()) else {
        return is_compiler_temporary(&variable.var.name).then_some(hidden);
    };
    let priority = match signal.kind.as_deref() {
        // the compiler's own definitions, described for completeness
        Some("net") => return Some(hidden),
        Some("state") | Some("module") => 2,
        Some("port") | Some("fire") | Some("clock") | Some("reset") => 1,
        _ => 0,
    };
    Some(VariableNameInfo {
        true_name: None,
        priority: Some(priority),
        visible: None,
    })
}

fn layout_known(desc: &TypeDesc) -> bool {
    matches!(desc.layout.as_deref(), Some("derived") | Some("custom"))
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
        // Bool is drawn as a level: Surfer draws a Bool field low when its
        // text is "0" or "false" (in any case), high otherwise, so the
        // constructor names False and True serve as they are.  Other enums'
        // constructor names are text.
        "enum" if is_bool(name) => VariableInfo::Bool,
        "enum" => VariableInfo::String,
        _ => match desc.width {
            Some(1) => VariableInfo::Bool,
            _ => VariableInfo::Bits,
        },
    }
}

/// The Prelude's Bool, which is an enum of False and True
fn is_bool(name: &str) -> bool {
    name == "Bool"
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
        let fill = raw
            .chars()
            .next()
            .filter(|c| matches!(c, 'x' | 'z' | 'u'))
            .unwrap_or('0');
        let mut s: String = std::iter::repeat_n(fill, width - raw.len()).collect();
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

// ---------------------------------------------------------------------
// Expressions over the packed value

fn to_u128(bits: &str) -> Option<u128> {
    if bits.is_empty() {
        return Some(0);
    }
    if bits.len() > 128 {
        return None;
    }
    u128::from_str_radix(bits, 2).ok()
}

fn to_i128(bits: &str) -> Option<i128> {
    let v = to_u128(bits)?;
    if bits.starts_with('1') && bits.len() < 128 {
        Some(v as i128 - (1i128 << bits.len()))
    } else {
        Some(v as i128)
    }
}

/// `v` as exactly `width` bits
fn from_u128(v: u128, width: u64) -> String {
    let s = format!("{v:b}");
    let width = width as usize;
    if s.len() >= width {
        s[s.len() - width..].to_string()
    } else {
        format!("{}{}", "0".repeat(width - s.len()), s)
    }
}

/// The value of an expression for the packed bits `arg`, or None where
/// it is a don't-care (or wider than this evaluator's arithmetic)
fn eval(e: &Expr, arg: &str) -> Option<String> {
    let arg1 = |i: usize| eval(e.args.get(i)?, arg);
    let bitwise = |f: fn(bool, bool) -> bool| -> Option<String> {
        let a = arg1(0)?;
        let b = arg1(1)?;
        if a.len() != b.len() {
            return None;
        }
        Some(
            a.chars()
                .zip(b.chars())
                .map(|(x, y)| if f(x == '1', y == '1') { '1' } else { '0' })
                .collect(),
        )
    };
    let compare = |f: fn(&str, &str) -> Option<bool>| -> Option<String> {
        Some(if f(&arg1(0)?, &arg1(1)?)? {
            "1".into()
        } else {
            "0".into()
        })
    };
    let arith = |f: fn(u128, u128) -> u128| -> Option<String> {
        let w = e.width?;
        Some(from_u128(f(to_u128(&arg1(0)?)?, to_u128(&arg1(1)?)?), w))
    };
    match e.op.as_str() {
        "arg" => Some(arg.to_string()),
        "const" => e.bits.clone(),
        "undet" => None,
        "extract" => {
            let x = arg1(0)?;
            Some(slice(&x, e.lo?, e.hi? + 1))
        }
        "concat" => {
            let mut s = String::new();
            for a in &e.args {
                s.push_str(&eval(a, arg)?);
            }
            Some(s)
        }
        "if" => {
            if arg1(0)?.contains('1') {
                arg1(1)
            } else {
                arg1(2)
            }
        }
        "case" => {
            let scrutinee = arg1(0)?;
            for arm in e.args[2..].chunks(2) {
                if eval(&arm[0], arg)? == scrutinee {
                    return eval(&arm[1], arg);
                }
            }
            arg1(1)
        }
        "not" => Some(
            arg1(0)?
                .chars()
                .map(|c| if c == '1' { '0' } else { '1' })
                .collect(),
        ),
        "and" => bitwise(|a, b| a && b),
        "or" => bitwise(|a, b| a || b),
        "xor" => bitwise(|a, b| a != b),
        "eq" => compare(|a, b| Some(a == b)),
        "ult" => compare(|a, b| Some(to_u128(a)? < to_u128(b)?)),
        "ule" => compare(|a, b| Some(to_u128(a)? <= to_u128(b)?)),
        "slt" => compare(|a, b| Some(to_i128(a)? < to_i128(b)?)),
        "sle" => compare(|a, b| Some(to_i128(a)? <= to_i128(b)?)),
        "zext" | "trunc" => {
            let x = arg1(0)?;
            let w = e.width? as usize;
            Some(if x.len() >= w {
                x[x.len() - w..].to_string()
            } else {
                format!("{}{}", "0".repeat(w - x.len()), x)
            })
        }
        "sext" => {
            let x = arg1(0)?;
            let w = e.width? as usize;
            let fill = x.chars().next().unwrap_or('0');
            Some(if x.len() >= w {
                x[x.len() - w..].to_string()
            } else {
                format!("{}{}", fill.to_string().repeat(w - x.len()), x)
            })
        }
        "add" => arith(|a, b| a.wrapping_add(b)),
        "sub" => arith(|a, b| a.wrapping_sub(b)),
        "mul" => arith(|a, b| a.wrapping_mul(b)),
        "neg" => Some(from_u128(to_u128(&arg1(0)?)?.wrapping_neg(), e.width?)),
        "sl" => arith(|a, b| if b >= 128 { 0 } else { a << b }),
        "srl" => arith(|a, b| if b >= 128 { 0 } else { a >> b }),
        "sra" => {
            let a = to_i128(&arg1(0)?)?;
            let b = to_u128(&arg1(1)?)?.min(127);
            Some(from_u128((a >> b) as u128, e.width?))
        }
        _ => None,
    }
}

/// A member's own bits within `bits`: its range, else its expression
fn member_bits(m: &Member, bits: &str) -> Option<String> {
    match (m.lo, m.hi, &m.bits) {
        (Some(lo), Some(hi), _) => Some(slice(bits, lo, hi)),
        (_, _, Some(e)) => eval(e, bits),
        _ => None,
    }
}

/// The member the packed `bits` are: told apart by the tag bits (the
/// type's tag range for a union, the whole value for an enum) when the
/// description gives tag values, else by each member's own test
fn active_member(desc: &TypeDesc, bits: &str) -> Option<usize> {
    let tag = match &desc.tag {
        Some(t) => as_number(&slice(bits, t.lo, t.hi)),
        None => as_number(bits),
    };
    let by_tag = |m: &Member| match tag {
        Some(v) => {
            m.tag == Some(v) || m.tags.contains(&v) || m.value == Some(v) || m.values.contains(&v)
        }
        None => false,
    };
    let by_when = |m: &Member| match &m.when {
        Some(e) => eval(e, bits).is_some_and(|v| v.contains('1')),
        None => false,
    };
    desc.members.iter().position(|m| by_tag(m) || by_when(m))
}

/// Normal for a fully known value, else the kind of unknown in it
fn kind_of(bits: &str) -> ValueKind {
    if bits.chars().all(|c| c == '0' || c == '1') {
        ValueKind::Normal
    } else if bits.contains('x') {
        ValueKind::Undef
    } else if bits.contains('z') {
        ValueKind::HighImp
    } else {
        ValueKind::Warn
    }
}

fn raw(bits: &str) -> TranslationResult {
    TranslationResult {
        val: ValueRepr::Bits(bits.len() as u32, bits.to_string()),
        subfields: vec![],
        kind: kind_of(bits),
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
    // raw, with the structure marked absent: too deep to decode, or a
    // union or enum with unknown bits, which select no constructor
    let raw_absent = || {
        let mut r = raw(bits);
        r.subfields = not_present(loaded, name, depth).subfields;
        r
    };
    if depth > 32 {
        return raw_absent();
    }
    // A struct or vector with unknown bits still decodes member by
    // member, each member showing its own bits (one computed by an
    // expression rather than sliced is absent); the whole carries the
    // kind of unknown.
    let kind = kind_of(bits);
    let known = matches!(kind, ValueKind::Normal);
    // a member's value; a don't-care shows as absent
    let member_value = |m: &Member| match (member_bits(m, bits), &m.ty) {
        (Some(b), Some(t)) => decode(loaded, t, &b, depth + 1),
        (Some(b), None) => raw(&b),
        (None, _) => not_present_leaf(),
    };
    match desc.kind.as_str() {
        "struct" if layout_known(desc) => TranslationResult {
            val: ValueRepr::Struct,
            subfields: desc
                .members
                .iter()
                .map(|m| SubFieldTranslationResult {
                    name: m.name.clone(),
                    result: member_value(m),
                })
                .collect(),
            kind,
        },
        "union" if layout_known(desc) && !known => raw_absent(),
        "enum" if !known => raw(bits),
        "union" if layout_known(desc) => {
            let active = active_member(desc, bits);
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
                            member_value(m)
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
                // an encoding no constructor uses
                None => TranslationResult {
                    val: ValueRepr::String(format!("?{bits}")),
                    subfields,
                    kind: ValueKind::Warn,
                },
            }
        }
        "enum" => match active_member(desc, bits) {
            Some(idx) => TranslationResult {
                val: ValueRepr::String(desc.members[idx].name.clone()),
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
                        result: decode(
                            loaded,
                            elem,
                            &slice(bits, i * stride, (i + 1) * stride),
                            depth + 1,
                        ),
                    })
                    .collect(),
                kind,
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
    let mut slot = LOADED.lock().unwrap();
    let Some(loaded) = slot.as_mut() else {
        return Ok(TranslationPreference::No);
    };
    Ok(match type_of(loaded, &variable) {
        Some(t) if decodes(loaded, &t) => TranslationPreference::Prefer,
        _ => TranslationPreference::No,
    })
}

#[plugin_fn]
pub fn variable_name_info(variable: VariableMeta<(), ()>) -> FnResult<Option<VariableNameInfo>> {
    let mut slot = LOADED.lock().unwrap();
    let Some(loaded) = slot.as_mut() else {
        return Ok(None);
    };
    Ok(name_info(loaded, &variable))
}

#[plugin_fn]
pub fn variable_info(variable: VariableMeta<(), ()>) -> FnResult<VariableInfo> {
    let mut slot = LOADED.lock().unwrap();
    let Some(loaded) = slot.as_mut() else {
        return Ok(VariableInfo::Bits);
    };
    Ok(match type_of(loaded, &variable) {
        Some(t) => info_of(loaded, &t, 0),
        None => VariableInfo::Bits,
    })
}

#[plugin_fn]
pub fn translate(
    TranslateParams { variable, value }: TranslateParams,
) -> FnResult<TranslationResult> {
    let mut slot = LOADED.lock().unwrap();
    let width = variable.num_bits.unwrap_or(0) as usize;
    let bits = bits_of(&value, width);
    let Some(loaded) = slot.as_mut() else {
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

    // The exporter's testsuite goldens as the fixture: the Bluesim
    // fragments of mkWaveTypesCore and of mkStash, which it instantiates
    fn fixture() -> Loaded {
        let fragments = ["mkWaveTypesCore", "mkStash"]
            .iter()
            .map(|module| {
                let path = format!(
                    "{}/../../testsuite/bsc.bluehs/wave-debug-info/{module}.bluesim.wave-debug.json.expected",
                    env!("CARGO_MANIFEST_DIR")
                );
                serde_json::from_str(&std::fs::read_to_string(path).unwrap()).unwrap()
            })
            .collect();
        Loaded::new("mkWaveTypesCore".into(), fragments)
    }

    fn ty(l: &mut Loaded, path: &str) -> Option<String> {
        let path: Vec<String> = path.split('.').map(str::to_string).collect();
        l.type_at(&path)
    }

    // a variable at the given dump scope, as Surfer hands it to the plugin
    fn meta(scope: &str, name: &str) -> VariableMeta<(), ()> {
        serde_json::from_value(serde_json::json!({
            "var": {"path": {"strs": scope.split('.').collect::<Vec<_>>(), "id": null},
                    "name": name, "id": null, "index": null},
            "num_bits": null, "variable_type": null, "variable_type_name": null,
            "index": null, "direction": null, "enum_map": {}, "encoding": "BitVector"
        }))
        .unwrap()
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
        assert_eq!(flat(&r), "struct{x=00000001, y=00000010, color=\"Green\"}");
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
        assert_eq!(
            flat(&r),
            "enum Valid{Invalid=\"\", Valid=0000000000000011}".replace("Invalid=\"\"", "Invalid=-")
        );
        // [Red, Green, Blue, Red] with element 0 lowest
        let r = decode(&l, "Vector::Vector#(4, WaveTypes::Color)", "00100100", 0);
        assert_eq!(
            flat(&r),
            "array{[0]=\"Red\", [1]=\"Green\", [2]=\"Blue\", [3]=\"Red\"}"
        );
    }

    #[test]
    fn hand_written_instances() {
        let l = fixture();
        // Source: Alu's payload shares the bits that tell the arms apart
        let r = decode(&l, "WaveTypes::Source", "010", 0);
        assert_eq!(flat(&r), "enum Alu{Zeros=-, Alu=1, VecMem=-, Other=-}");
        let r = decode(&l, "WaveTypes::Source", "001", 0);
        assert_eq!(flat(&r), "enum Alu{Zeros=-, Alu=0, VecMem=-, Other=-}");
        let r = decode(&l, "WaveTypes::Source", "110", 0);
        assert!(flat(&r).starts_with("enum Other{"));
        // Packet: the short arm's payload is not right-aligned
        let r = decode(&l, "WaveTypes::Packet", "110101100", 0);
        assert_eq!(flat(&r), "enum Narrow{Wide=-, Narrow=101011}");
        let r = decode(&l, "WaveTypes::Packet", "010101100", 0);
        assert_eq!(flat(&r), "enum Wide{Wide=10101100, Narrow=-}");
        assert!(decodes(&l, "WaveTypes::Source"));
    }

    #[test]
    fn expressions() {
        let e: Expr = serde_json::from_str(
            r#"{"op":"if","args":[{"op":"eq","width":1,"args":[{"op":"extract","hi":3,"lo":2,"args":[{"op":"arg"}]},{"op":"const","bits":"10"}]},
                                  {"op":"add","width":4,"args":[{"op":"arg"},{"op":"const","bits":"0001"}]},
                                  {"op":"sext","width":6,"args":[{"op":"extract","hi":1,"lo":0,"args":[{"op":"arg"}]}]}]}"#,
        )
        .unwrap();
        assert_eq!(eval(&e, "1011").as_deref(), Some("1100"));
        assert_eq!(eval(&e, "0010").as_deref(), Some("111110"));
        let u: Expr = serde_json::from_str(r#"{"op":"undet","width":2}"#).unwrap();
        assert_eq!(eval(&u, "00"), None);
    }

    #[test]
    fn bool_is_a_level_named_by_its_constructor() {
        let l = fixture();
        assert!(matches!(info_of(&l, "Bool", 0), VariableInfo::Bool));
        assert_eq!(flat(&decode(&l, "Bool", "1", 0)), "\"True\"");
        assert_eq!(flat(&decode(&l, "Bool", "0", 0)), "\"False\"");
        // other enums are text
        assert!(matches!(
            info_of(&l, "WaveTypes::Color", 0),
            VariableInfo::String
        ));
    }

    #[test]
    fn unknown_bits_decode_member_by_member() {
        let l = fixture();
        // a struct's fields each show their own bits, known or not
        let r = decode(&l, "WaveTypes::Pixel", "00000001xxxxxxxx01", 0);
        assert!(matches!(r.kind, ValueKind::Undef));
        assert_eq!(flat(&r), "struct{x=00000001, y=xxxxxxxx, color=\"Green\"}");
        let r = decode(&l, "WaveTypes::Pixel", "xxxxxxxxxxxxxxxxxx", 0);
        assert_eq!(flat(&r), "struct{x=xxxxxxxx, y=xxxxxxxx, color=xx}");
        // so do a vector's elements
        let r = decode(&l, "Vector::Vector#(4, WaveTypes::Color)", "xx100100", 0);
        assert_eq!(
            flat(&r),
            "array{[0]=\"Red\", [1]=\"Green\", [2]=\"Blue\", [3]=xx}"
        );
        // a union selects no arm: raw, with the arms absent
        let r = decode(&l, "WaveTypes::Cell", "zzzzzzzzzzzzzzzzzzzz", 0);
        assert!(matches!(r.kind, ValueKind::HighImp));
        assert!(matches!(r.val, ValueRepr::Bits(..)));
        assert_eq!(r.subfields.len(), 3);
        assert!(r
            .subfields
            .iter()
            .all(|s| matches!(s.result.val, ValueRepr::NotPresent)));
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
    }

    #[test]
    fn paths() {
        let mut l = fixture();
        // the top module under bsc's harness, found by its first signal
        assert_eq!(ty(&mut l, "main.top.cell").as_deref(), Some("WaveTypes::Cell"));
        assert_eq!(l.roots, vec![vec!["main".to_string(), "top".to_string()]]);
        // an inlined instance's register, under either hierarchy's path
        assert_eq!(ty(&mut l, "main.top.pair_lo").as_deref(), Some("UInt#(4)"));
        assert_eq!(ty(&mut l, "main.top.pair.lo").as_deref(), Some("UInt#(4)"));
        assert_eq!(ty(&mut l, "main.top.pair.d._r").as_deref(), Some("UInt#(4)"));
        // a synthesized child's signals, in its own fragment
        assert_eq!(ty(&mut l, "main.top.stash.cnt").as_deref(), Some("UInt#(8)"));
        assert_eq!(ty(&mut l, "main.top.stash.last").as_deref(), Some("WaveTypes::Pixel"));
        assert_eq!(ty(&mut l, "main.top.stash"), None);
        assert_eq!(ty(&mut l, "main.top.nothing"), None);
        assert_eq!(ty(&mut l, "main.top.stash.nothing"), None);
        // the same design under a testbench of its own
        assert_eq!(ty(&mut l, "tb.dut.stash.cnt").as_deref(), Some("UInt#(8)"));
        assert_eq!(l.roots.len(), 2);
        // the top module is not the dump's root itself
        assert_eq!(ty(&mut l, "cnt"), None);
    }

    #[test]
    fn names() {
        let mut l = fixture();
        // the state of the design outranks its fires, which outrank the rest
        let state = name_info(&mut l, &meta("main.top", "cell")).unwrap();
        assert_eq!(state.priority, Some(2));
        assert!(state.true_name.is_none());
        let child = name_info(&mut l, &meta("main.top.stash", "cnt")).unwrap();
        assert_eq!(child.priority, Some(2));
        // a flattened name keeps its dumped name
        let fire = name_info(&mut l, &meta("main.top", "WILL_FIRE_RL_step")).unwrap();
        assert_eq!(fire.priority, Some(1));
        assert!(fire.true_name.is_none());
        assert_eq!(
            name_info(&mut l, &meta("main.top", "CLK")).unwrap().priority,
            Some(1)
        );
        // a compiler temporary is hidden; anything else unknown is left alone
        let tmp = name_info(&mut l, &meta("main.top", "cnt___d3")).unwrap();
        assert_eq!((tmp.priority, tmp.visible), (Some(-1), Some(false)));
        assert!(name_info(&mut l, &meta("main.top", "other")).is_none());
        assert!(is_compiler_temporary("x__h944"));
        assert!(!is_compiler_temporary("my__data"));
    }

    #[test]
    fn nets_are_hidden() {
        // a net of the generated Verilog, as the Verilog fragments describe
        let fragment: Fragment = serde_json::from_str(
            r#"{"module": "m", "signals": [
                 {"synthpath": ["x__h12"], "kind": "net", "type": "Bool"},
                 {"synthpath": ["cnt"], "kind": "state", "type": "UInt#(8)"}]}"#,
        )
        .unwrap();
        let mut l = Loaded::new("m".into(), vec![fragment]);
        let net = name_info(&mut l, &meta("main.top", "x__h12")).unwrap();
        assert_eq!((net.priority, net.visible), (Some(-1), Some(false)));
        assert_eq!(ty(&mut l, "main.top.x__h12").as_deref(), Some("Bool"));
        let state = name_info(&mut l, &meta("main.top", "cnt")).unwrap();
        assert_eq!((state.priority, state.visible), (Some(2), None));
    }
}
