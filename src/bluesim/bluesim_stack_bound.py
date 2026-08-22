#!/usr/bin/env python3
"""Static worst-case stack-depth bound for a Bluesim model shared object.

This tool computes an upper bound, in bytes, on the stack consumed on
the simulation thread by a call to bk_sync_run() / bk_sync_step(),
from per-function stack-usage data and a call graph emitted by GCC,
plus a hand-annotated table for the runtime's indirect calls.

Inputs
------
Every translation unit linked into the model .so (the generated model
TUs and the prebuilt runtime objects from libbskernel.a/libbsprim.a)
is compiled with:

    -fstack-usage -fcallgraph-info=su

and this tool is pointed at the resulting .ci files (as files, or
directories that are scanned for *.ci).  The .ci file is the sole
input format: with the "=su" modifier it embeds the same per-function
stack-usage numbers that the .su files carry, attached to a call
graph recorded after inlining, and it marks every remaining indirect
call site with an explicit "__indirect_call" edge carrying the source
location.  We prefer it over disassembly (objdump -d) because:

  * it is recorded by the compiler itself after inlining, so the
    edges match the code that was actually emitted;
  * indirect calls are explicit, whereas in disassembly an indirect
    jump for a switch table is not distinguishable from an indirect
    tail call without decoding relocations and jump tables;
  * it is architecture independent;
  * the final .so is stripped of local symbols (the event handlers,
    schedule functions and reset trampolines are all file-static), so
    disassembling the linked artifact would lose exactly the names
    the analysis needs.

The .su files are still emitted and shipped for human inspection, but
the tool does not parse them.

Indirect calls
--------------
The Bluesim runtime dispatches through a small, closed set of
function-pointer types.  Each indirect call site found in a reachable
function must be covered by the annotation table below
(INDIRECT_SITE_TABLE), which maps the containing (demangled) function
to the set of functions the pointer can target:

  * tEventFn (event_queue.h): EventQueue::execute() dispatches to the
    functions ever assigned to tEvent.fn -- a closed set of
    file-static kernel handlers (EVENT_HANDLERS below).
  * tScheduleFn (bluesim_types.h): only written by
    bk_set_clock_event_fn(), whose only callers are generated
    create_model() functions passing the generated static schedule
    functions (SimBlocksToC.mkSchedName: 'schedule[_after]_(pos|neg)edge_*').
  * tResetFn (bluesim_types.h): the generated per-module reset
    trampolines ('MOD_*::static_reset_*', SimCCBlock.mkSetResetFnName
    registration) and the reset primitives' own static forwarders.
  * tEventPredicate: the kernel's file-static event predicates.
  * Model virtuals (bs_model.h): the single generated MODEL_* class.
  * Target virtuals (bs_target.h): FileTarget/BufferTarget overrides.
  * bs_host_ops function pointers: the embedder boundary (below).

The target sets are resolved against the functions defined in the .ci
inputs, so per-design sets (schedule functions, reset trampolines,
Model virtuals) are discovered from the model's own TUs.  An indirect
call site in a reachable function that is not covered by the table is
an error: the tool refuses to produce a bound (see "No bound") and
prints the site, so a runtime change that adds a new indirect call is
caught loudly rather than silently under-approximated.

Reset chains
------------
Reset propagation can chain synchronously: a reset primitive calls
its registered tResetFn, whose module may assert a derived reset on
another primitive, and so on.  The wiring is a per-design DAG, so a
synchronous chain visits each reset primitive instance at most once,
but this tool only sees the per-class trampoline functions.  Sets
marked 'chain' therefore contribute, at a call site:

    M * sum(frame_m + overhead for m in set) + max(depth'(m))

where depth' cuts further chain edges (they are accounted for by the
sum) and M (--chain-multiplicity, default 4) bounds how many
instances of the same module class a single synchronous chain is
assumed to thread through.  M=4 is generous for real reset
topologies; designs with deeper same-class reset chains should raise
it.

Declared assumptions
--------------------
Three places rest on a declared, overridable assumption instead of
pure call-graph arithmetic (each is reported when it is used):

  * --chain-multiplicity (default 4): see "Reset chains" above.

  * --format-vla-bytes (default 8192): the runtime formats wide
    values and computes wide division/modulus with VLAs sized by the
    value's width (WideData::print_*, max_decimal_digits, the wide
    division/modulus entry points, and $display's print_binary).  GCC
    reports these frames as unbounded 'dynamic', so each such frame
    is charged its static part plus this many bytes.  The default
    covers values up to 8 Kbit formatted in binary (the widest
    format); designs formatting or dividing wider values must raise
    it for the bound to hold.

  * --module-depth (default 32): $display's %%m writes the
    hierarchical instance name via Module::write_name(), which
    recurses parent-first.  The recursion depth is the instance
    hierarchy depth, so that self-edge is charged this many frames.

Boundaries (documented exclusions)
----------------------------------
Two kinds of calls are deliberately outside the bound:

  * Host operations: every call through the bs_host_ops table
    (bluesim_host_ops.h).  The table is supplied by the embedder at
    bk_sync_init() time, so its stack cost belongs to the embedder;
    the noreturn members (divide_by_zero, out_of_bounds,
    event_queue_overflow) additionally never return.  An embedder
    that wants a whole-thread figure must add the worst case of its
    own host-ops implementation on top of the exposed bound.

  * Terminating/unwinding paths: abort(), assertion and
    stack-protector failures, and C++ exception raising/unwinding
    (only reached on allocation failure or a host longjmp/rethrow);
    these end the run rather than returning into it.

External leaf calls (memcpy/memset/..., malloc/operator new and the
out-of-line libstdc++ string/list helpers) have no stack-usage data
because they live in libc/libstdc++.  They are covered by a
conservative fixed allowance per call (EXTERNAL_ALLOWANCES).  Any
other external function reachable from an entry point -- in
particular imported BDPI functions, which are compiled outside this
scheme -- makes the bound unsound, so no bound is produced.

No bound
--------
When a sound bound cannot be computed the tool prints the reasons to
stderr and reports a bound of 0; 0 is the documented "no bound
available" value exposed by bk_stack_depth_bound().  This happens
for: reachable BDPI or other unknown external functions, reachable
functions with unbounded dynamic stack usage (the VLA-based wide
division/modulus and wide-value $display formatting in the runtime),
recursion cycles, an indirect call site missing from the annotation
table, or missing .ci data for a linked object.  Usage errors (bad
arguments, unreadable files) exit nonzero instead; a computed result
-- including a no-bound 0 -- exits 0.

Output
------
The bound in bytes is printed to stdout.  With --report a worst-path
report is printed to stderr.  With --emit-tu FILE a small C++
translation unit is written that defines the constant
'bs_stack_depth_bound' returned by the kernel's
bk_stack_depth_bound() accessor (the runtime carries a weak 0
default in stack_bound_default.cxx, so a link without this TU --
e.g. a SystemC build -- reports no bound).

The per-frame call overhead (return address, potential alignment) is
not included in GCC's stack-usage numbers; each frame on a path is
charged an extra CALL_OVERHEAD bytes (default 16, --call-overhead).

The bound covers the simulation thread from the bk_sync_run() /
bk_sync_step() frame downward.  It does not cover bk_sync_init(),
model construction, signal handlers, or anything the host does; the
report lists new_MODEL_*() and bk_sync_init() separately when they
can be bounded ('dynamic,bounded' frames from alloca/VLA with a
compiler-known bound are folded in exactly).
"""

import argparse
import os
import re
import sys

CALL_OVERHEAD_DEFAULT = 16
CHAIN_MULTIPLICITY_DEFAULT = 4
FORMAT_VLA_BYTES_DEFAULT = 8192
MODULE_DEPTH_DEFAULT = 32

# ---------------------------------------------------------------------------
# The hand-annotated indirect-edge table (see the module docstring).
# ---------------------------------------------------------------------------

TARGET_SETS = {
    # Every function ever assigned to tEvent.fn ('.fn =' in the
    # runtime sources: kernel.cxx and reset.cxx).
    "EVENT_HANDLERS": {
        "required": True,
        "members": [
            r"^tTime reset_event\(tSimStateHdl",            # reset.cxx
            r"^tTime reset_model_event\(tSimStateHdl",      # kernel.cxx
            r"^tTime dump_cycle_event\(tSimStateHdl",       # kernel.cxx
            r"^tTime run_edge_schedule_event\(tSimStateHdl",
            r"^tTime run_combo_schedule_event\(tSimStateHdl",
            r"^tTime quit_event\(tSimStateHdl",
            r"^tTime yield_event\(tSimStateHdl",
        ],
    },
    # tScheduleFn targets (generated, file-static in the model TU).
    "SCHEDULE_FNS": {
        "required": False,   # a clockless design registers none
        "members": [
            r"^void schedule(_after)?_(posedge|negedge)_[A-Za-z0-9_$]*\(",
        ],
    },
    # tResetFn targets; 'chain' because reset propagation can nest
    # synchronously through these (see "Reset chains" above).
    "RESET_FNS": {
        "required": False,   # a design without reset primitives has none
        "chain": True,
        "members": [
            r"::static_reset_",
            r"::static_do_select\(",
        ],
    },
    # The kernel's tEventPredicate functions (EventQueue::find/remove).
    "PREDICATE_FNS": {
        "required": True,
        "members": [
            r"^bool is[A-Z][A-Za-z0-9_]*\(tSimStateHdl, const tEvent&\)",
        ],
    },
    # Virtual calls on the Model base class (bs_model.h): exactly one
    # Model subclass (MODEL_<top>) is linked into a .so, and a virtual
    # call site dispatches on one specific vtable slot, so each slot
    # used from the event loop gets its own precise set.
    "MODEL_GET_INSTANCE": {
        "required": True,
        "members": [
            r"\bMODEL_[A-Za-z0-9_]+::get_instance\(",
        ],
    },
    "MODEL_RESET_MODEL": {
        "required": True,
        "members": [
            r"\bMODEL_[A-Za-z0-9_]+::reset_model\(",
        ],
    },
    # Virtual calls on Target (bs_target.h): the write_char/write_data
    # overrides of its two subclasses.  (Module (bs_module.h) has no
    # virtual functions, so there is no set for it.)
    "TARGET_VIRTUALS": {
        "required": False,
        "members": [
            r"^virtual void (File|Buffer)Target::write_(char|data)\(",
        ],
    },
    # Calls through the bs_host_ops table: the documented embedder
    # boundary (see the module docstring).  Cost 0.
    "HOST_OPS": {"boundary": True},
}

# caller-name regex -> [set names].  Every indirect call site whose
# containing function's demangled name matches one of these gets the
# union of the listed sets; the union is a maximum, so extra sets are
# conservative, never unsound.  Regexes are matched with re.search.
INDIRECT_SITE_TABLE = [
    # The dispatcher itself.
    (r"^void EventQueue::execute\(", ["EVENT_HANDLERS"]),
    # Event search/removal invoke the kernel's predicates.
    (r"^(void|const tEvent\*) EventQueue::(remove|find)\(",
     ["PREDICATE_FNS"]),
    # Edge/combo handlers: the registered schedule function, plus
    # model->get_instance(); print_cycle_description()'s host output
    # helpers can be inlined into them.
    (r"^tTime run_edge_schedule_event\(",
     ["SCHEDULE_FNS", "MODEL_GET_INSTANCE", "HOST_OPS"]),
    (r"^tTime run_combo_schedule_event\(",
     ["SCHEDULE_FNS", "MODEL_GET_INSTANCE"]),
    # Reset delivery and synchronous reset forwarding.
    (r"^tTime reset_event\(", ["RESET_FNS"]),
    (r"::static_reset_", ["RESET_FNS"]),
    (r"::static_do_select\(", ["RESET_FNS"]),
    (r"::reset_IN_RST\(", ["RESET_FNS"]),
    (r"::reset_syncRst\$gen_rst\(", ["RESET_FNS"]),
    (r"::do_select\(", ["RESET_FNS"]),
    (r"^tTime reset_model_event\(", ["MODEL_RESET_MODEL"]),
    # Generated module reset methods forward to submodule/primitive
    # resets, which may synchronously chain (async SyncReset etc.).
    (r"\bMOD_[A-Za-z0-9_]+::reset_", ["RESET_FNS"]),
    (r"\bMODEL_[A-Za-z0-9_]+::reset_model\(", ["RESET_FNS"]),
    # Cycle-count dumping writes through the host.
    (r"^tTime dump_cycle_event\(", ["HOST_OPS"]),
    (r"^void print_cycle_description\(", ["HOST_OPS"]),
    (r"^void host_write_(str|char|dec)\(", ["HOST_OPS"]),
    # The sync entry points flush host files on pause;
    # sync_run_events() may be inlined into either.
    (r"^tStatus bk_sync_run\(", ["HOST_OPS"]),
    (r"^tStatus bk_sync_step\(", ["HOST_OPS"]),
    (r"^tStatus sync_run_events\(", ["HOST_OPS"]),
    # Fatal-condition reporters (noreturn host ops).
    (r"^void bk_event_queue_overflow\(", ["HOST_OPS"]),
    (r"^void bk_divide_by_zero\(", ["HOST_OPS"]),
    (r"^void bk_out_of_bounds\(", ["HOST_OPS"]),
    # The runtime's formatted-output engine ($display and friends):
    # writes go through Target virtuals and end at the host-ops
    # boundary.
    (r"\bTarget::", ["TARGET_VIRTUALS", "HOST_OPS"]),
    (r"(File|Buffer)Target::", ["HOST_OPS"]),
    (r"(dollar_|fdollar_)", ["TARGET_VIRTUALS", "HOST_OPS"]),
    (r"^const char\* (print_|handle_escape\(|handle_format\()",
     ["TARGET_VIRTUALS", "HOST_OPS"]),
    (r"^void (format|pad)\(", ["TARGET_VIRTUALS", "HOST_OPS"]),
    (r"^void Module::write_name\(", ["TARGET_VIRTUALS", "HOST_OPS"]),
    (r"\bWideData::(print_|max_decimal_digits)", ["TARGET_VIRTUALS",
                                                   "HOST_OPS"]),
    (r"\bArgList::", ["TARGET_VIRTUALS", "HOST_OPS"]),
    # Memory-file preloading ($readmemh at time 0) reads through the
    # host and calls back into the primitive being preloaded; the
    # callbacks are non-virtual members, but the reads are host ops.
    (r"\b(read_mem_file|MemFileParser|parse_mem)", ["HOST_OPS"]),
]

# Conservative per-call stack allowances, in bytes, for external
# functions with no stack-usage data (libc/libstdc++/compiler
# builtins).  Matched, in order, against the call target's plain name
# (C functions), mangled name and demangled prototype.  Allowance 0
# marks terminating/unwinding paths (documented exclusions).  These
# are deliberately generous for common glibc/libstdc++
# implementations.
EXTERNAL_ALLOWANCES = [
    # terminating / unwinding paths
    (r"^(abort|exit|_exit|_Exit|__assert_fail|__stack_chk_fail)$", 0),
    (r"^(__cxa_[a-z_]+|_Unwind_Resume|__builtin_unwind_resume)$", 0),
    (r"^void __builtin_unwind_resume\(", 0),
    (r"\bstd::__throw_[a-z_]+", 0),
    (r"\bstd::terminate\(\)", 0),
    # string/memory primitives (vectorized implementations use only
    # registers and a small spill area)
    (r"^(memcpy|memmove|memset|memcmp|mempcpy|bcmp)$", 512),
    (r"^__mem[a-z]+_chk$", 512),
    (r"^(strlen|strcmp|strncmp|strchr|strrchr|strstr|strcpy|strncpy"
     r"|strcat|strncat|strcspn|strspn|strpbrk)$", 1024),
    (r"^__str[a-z]+_chk$", 1024),
    (r"^(strerror|__errno_location)$", 1024),
    (r"^(strtol|strtoul|strtoll|strtoull|strtod|atoi|atol)$", 2048),
    # allocator family (glibc malloc worst path: arena setup + mmap)
    (r"^(malloc|free|cfree|calloc|realloc|posix_memalign|strdup)$", 8192),
    (r"\boperator (new|delete)\s*(\[\])?\s*\(", 8192),
    (r"^_Z(nw|na|dl|da)m?", 8192),
    # out-of-line libstdc++ helpers (extern-template std::string,
    # std::list node base); they may allocate
    (r"\bstd::__cxx11::basic_string<", 8192),
    (r"\bstd::__detail::_List_node_base::", 1024),
    # time
    (r"^(time|clock_gettime|gettimeofday)$", 1024),
]

# The runtime's known VLA users (all sized by the width of the value
# being formatted or divided, or by a format field's own length): a
# plain-'dynamic' frame matching one of these is charged its static
# part plus --format-vla-bytes; any other plain-'dynamic' frame
# defeats the bound.
DYNAMIC_VLA_FUNCTIONS = [
    # dollar_display.cxx: string arguments staged on the stack
    # (FILL_TVALUE_KEEPING_STRINGS), numeric values reinterpreted as
    # format strings, the real-format field copy, and the $swrite
    # family's caller-side BufferTarget storage
    r"^const char\* print_(binary|decimal|hex|octal|real)\(tFieldDesc&",
    r"^void format\(const char\*",
    # the variadic system tasks are matched by mangled name: GCC's
    # callgraph labels demangle their '...' prototypes to just ")"
    r"^_Z12dollar_fatalP9tSimState",
    r"^_Z1\ddollar_s(write[boh]?|format)AVP9tSimState",
    # tree-valued string names flattened into stack buffers sized by
    # the tree's own byte count: $fopen's file name and mode, the
    # plusargs name, and the RegFile/BRAM load-file constructors
    # (see bs_str.h)
    r"^_Z12dollar_fopenPKc",
    r"^_Z26dollar_test_dollar_plusargsP9tSimState",
    r"\bMOD_RegFile<.*>::MOD_RegFile\(.*tStr",
    r"\bMOD_BRAM<.*>::MOD_BRAM\(.*tStr",
    # target.cxx: over-long formatted reals retried on the stack
    r"\bTarget::write_real\(",
    r"\bWideData::print_(binary|hex|octal|decimal)\(",
    r"\bWideData::max_decimal_digits\(",
    r"^WideData operator[/%]\(",                     # wide_data.cxx
    r"^void wop_(quot|rem)\(",                       # wide_data.cxx
]

# Self-recursive functions charged a declared number of frames; the
# limit is either an integer or the name of the argparse attribute
# holding the frame count.
RECURSION_LIMITS = [
    (r"^void Module::write_name\(", "module_depth"),   # %m hierarchy walk
    # registerFile() can call back into ensure_std_registered(), but a
    # flag set before the call makes the re-entry return immediately,
    # so the dynamic depth is exactly 2.
    (r"::ensure_std_registered\(", 2),                 # dollar_display.cxx
]

# ---------------------------------------------------------------------------
# .ci (VCG callgraph-info) parsing
# ---------------------------------------------------------------------------

NODE_RE = re.compile(
    r'^node:\s*\{\s*title:\s*"((?:[^"\\]|\\.)*)"\s*label:\s*"((?:[^"\\]|\\.)*)"')
EDGE_RE = re.compile(
    r'^edge:\s*\{\s*sourcename:\s*"((?:[^"\\]|\\.)*)"'
    r'\s*targetname:\s*"((?:[^"\\]|\\.)*)"'
    r'(?:\s*label:\s*"((?:[^"\\]|\\.)*)")?')
SU_PART_RE = re.compile(r'^(\d+)\s+bytes\s+\((.*)\)$')
LOC_PART_RE = re.compile(r'^(<built-in>|\S.*:\d+(:\d+)?)$')

INDIRECT = "__indirect_call"

# complete-object <-> base-object constructor/destructor aliases
CTOR_DTOR_ALIAS = [(re.compile(r"(C)1(E)"), r"\g<1>2\g<2>"),
                   (re.compile(r"(C)2(E)"), r"\g<1>1\g<2>"),
                   (re.compile(r"(D)1(E)"), r"\g<1>2\g<2>"),
                   (re.compile(r"(D)2(E)"), r"\g<1>1\g<2>")]


def unescape(s):
    return s.replace('\\"', '"').replace('\\\\', '\\')


def split_label(label):
    """Split a node label into (pretty, loc, su, qual).

    The label is pretty-name, optional location and optional
    stack-usage joined by newlines -- but the pretty name of a
    template function can itself contain newlines, so parse from the
    end: the su line and location line have recognizable shapes.
    """
    parts = label.split("\\n")
    su = qual = loc = None
    while len(parts) > 1:
        m = SU_PART_RE.match(parts[-1])
        if m and su is None:
            su = int(m.group(1))
            qual = m.group(2)
            parts.pop()
            continue
        if LOC_PART_RE.match(parts[-1]) and loc is None:
            loc = parts.pop()
            continue
        break
    return " ".join(parts), loc, su, qual


class Func(object):
    """One function node from a .ci file."""

    def __init__(self, tu, title, label):
        self.tu = tu
        self.title = title            # mangled; file-qualified for statics
        self.pretty, self.loc, self.su, self.qual = split_label(label)
        self.raw_edges = []           # (target_title, site_label)

    @property
    def defined(self):
        return self.su is not None

    def __repr__(self):
        return "<Func %s>" % self.pretty


class CallGraph(object):
    def __init__(self):
        self.nodes = {}        # (tu, title) -> Func
        self.defined = {}      # title -> Func (global titles, max su)

    def load_ci(self, path):
        tu = path
        try:
            fh = open(path, "r", errors="replace")
        except OSError as e:
            raise SystemExit("error: cannot read %s: %s" % (path, e))
        with fh:
            for line in fh:
                line = line.strip()
                if line.startswith("node:"):
                    m = NODE_RE.match(line)
                    if not m:
                        raise SystemExit(
                            "error: unparsable node line in %s: %s"
                            % (path, line))
                    title = unescape(m.group(1))
                    f = Func(tu, title, unescape(m.group(2)))
                    old = self.nodes.get((tu, title))
                    # a TU can list a node twice; keep the defined one
                    if old is None or (not old.defined and f.defined):
                        if old is not None:
                            f.raw_edges = old.raw_edges
                        self.nodes[(tu, title)] = f
                elif line.startswith("edge:"):
                    m = EDGE_RE.match(line)
                    if not m:
                        raise SystemExit(
                            "error: unparsable edge line in %s: %s"
                            % (path, line))
                    src = unescape(m.group(1))
                    tgt = unescape(m.group(2))
                    lbl = unescape(m.group(3)) if m.group(3) else None
                    node = self.nodes.get((tu, src))
                    if node is None:
                        # .ci always emits the node before its edges
                        raise SystemExit(
                            "error: edge before node %r in %s" % (src, path))
                    node.raw_edges.append((tgt, lbl))

    def index(self):
        """Build the global title index (max su wins for comdat dups)."""
        for (tu, title), f in self.nodes.items():
            if not f.defined:
                continue
            prev = self.defined.get(title)
            if prev is None or (f.su or 0) > (prev.su or 0):
                self.defined[title] = f

    def _lookup(self, tu, title):
        f = self.nodes.get((tu, title))
        if f is not None and f.defined:
            return f
        return self.defined.get(title)

    def resolve(self, tu, title):
        """Resolve an edge target: same-TU static first, then global;
        fall back to local-alias and complete/base ctor-dtor aliases."""
        f = self._lookup(tu, title)
        if f is not None:
            return f
        if title.endswith(".localalias"):
            base = title[:-len(".localalias")]
            f = self.resolve(tu, base)
            if f is None and ":" in base:
                # the alias name is file-qualified even when the
                # aliased function has external linkage
                f = self.resolve(tu, base.rsplit(":", 1)[1])
            return f
        for (rx, repl) in CTOR_DTOR_ALIAS:
            alias = rx.sub(repl, title, count=1)
            if alias != title:
                f = self._lookup(tu, alias)
                if f is not None:
                    return f
        return None

    def all_defined(self):
        seen = set()
        for f in self.defined.values():
            seen.add((f.tu, f.title))
            yield f
        for key, f in self.nodes.items():
            if f.defined and key not in seen:
                yield f


# ---------------------------------------------------------------------------
# Analysis
# ---------------------------------------------------------------------------

class Analysis(object):
    def __init__(self, graph, call_overhead, chain_multiplicity,
                 format_vla_bytes, module_depth):
        self.graph = graph
        self.call_overhead = call_overhead
        self.chain_multiplicity = chain_multiplicity
        self.format_vla_bytes = format_vla_bytes
        self.recursion_limits = {"module_depth": module_depth}
        self.problems = []           # soundness problems (-> no bound)
        self.notes = []              # declared assumptions actually used
        self.sets = {}               # set name -> [Func]
        self.site_table = [(re.compile(rx), sets)
                           for (rx, sets) in INDIRECT_SITE_TABLE]
        self.allowances = [(re.compile(rx), b)
                           for (rx, b) in EXTERNAL_ALLOWANCES]
        self.vla_table = [re.compile(rx) for rx in DYNAMIC_VLA_FUNCTIONS]
        self.recursion_table = [(re.compile(rx), attr)
                                for (rx, attr) in RECURSION_LIMITS]
        # memo[mode][id(Func)] = (inclusive bytes, worst callee or None)
        self._memo = {"full": {}, "nochain": {}}
        self._resolve_sets()

    def note(self, msg):
        if msg not in self.notes:
            self.notes.append(msg)

    def _matches(self, cre, func):
        return cre.search(func.pretty) or cre.search(func.title)

    def problem(self, msg):
        if msg not in self.problems:
            self.problems.append(msg)

    def _resolve_sets(self):
        funcs = list(self.graph.all_defined())
        for name, spec in TARGET_SETS.items():
            if spec.get("boundary"):
                self.sets[name] = []
                continue
            members, seen = [], set()
            for rx in spec["members"]:
                cre = re.compile(rx)
                for f in funcs:
                    if self._matches(cre, f) and id(f) not in seen:
                        seen.add(id(f))
                        members.append(f)
            self.sets[name] = members
            if spec.get("required") and not members:
                self.problem(
                    "indirect-target set %s resolved no functions "
                    "(runtime renamed? annotation table stale?)" % name)

    def _sets_for(self, func):
        names = []
        for (cre, set_names) in self.site_table:
            if self._matches(cre, func):
                for n in set_names:
                    if n not in names:
                        names.append(n)
        return names

    def _allowance(self, tu, title):
        """Allowance for an external call target, or None if unknown."""
        node = self.graph.nodes.get((tu, title))
        pretty = node.pretty if node is not None else title
        for (cre, b) in self.allowances:
            if cre.search(title) or cre.search(pretty):
                return pretty, b
        return pretty, None

    def _chain_cost(self, set_name, mode):
        """Cost of a call through a 'chain' set (see module docstring)."""
        members = self.sets[set_name]
        if not members:
            return 0, None
        if mode == "nochain":
            # the continuation is accounted for by the caller's sum
            return 0, None
        self.note("reset-chain sites bounded with multiplicity %d "
                  "(see --chain-multiplicity)" % self.chain_multiplicity)
        total = self.chain_multiplicity * sum(
            (m.su or 0) + self.call_overhead for m in members)
        best, best_m = 0, None
        for m in members:
            d = self._depth(m, "nochain")
            if d > best:
                best, best_m = d, m
        return total + best, best_m

    def _depth(self, func, mode, _stack=None):
        """Worst-case stack bytes of calling func (inclusive), memoized."""
        memo = self._memo[mode]
        key = id(func)
        if key in memo:
            return memo[key][0]
        if _stack is None:
            _stack = ([], set())
        stack, on_stack = _stack
        if key in on_stack:
            cyc = [f.pretty for f in stack[stack.index(func):]]
            self.problem("recursion cycle: " +
                         " -> ".join(cyc + [func.pretty]))
            memo[key] = (0, None)
            return 0
        stack.append(func)
        on_stack.add(key)

        frame = func.su or 0
        if func.qual == "dynamic":
            # plain 'dynamic': the reported number excludes the
            # VLA/alloca part, so no static bound exists -- except
            # for the runtime's known width-sized VLA users, which
            # are charged the declared --format-vla-bytes allowance.
            if any(self._matches(cre, func) for cre in self.vla_table):
                frame += self.format_vla_bytes
                self.note("width-sized VLA frames charged %d extra bytes "
                          "(see --format-vla-bytes)" % self.format_vla_bytes)
            else:
                self.problem(
                    "unbounded dynamic stack usage (VLA/alloca) in "
                    "'%s' (%s)" % (func.pretty, func.loc or func.tu))
        # 'static' is exact; for 'dynamic,bounded' GCC's number is the
        # bounded total, so both use func.su as-is.

        recursion_frames = 1
        for (cre, attr) in self.recursion_table:
            if self._matches(cre, func):
                if isinstance(attr, str):
                    recursion_frames = self.recursion_limits[attr]
                    self.note("self-recursion in '%s' charged %d frames "
                              "(see --%s)" % (func.pretty, recursion_frames,
                                              attr.replace("_", "-")))
                else:
                    recursion_frames = attr
                break

        best, best_next = 0, None
        indirect_done = False
        for (tgt, lbl) in func.raw_edges:
            if tgt == INDIRECT:
                if indirect_done:
                    continue
                indirect_done = True
                set_names = self._sets_for(func)
                if not set_names:
                    self.problem(
                        "unannotated indirect call in '%s' at %s (add it "
                        "to INDIRECT_SITE_TABLE in bluesim_stack_bound.py)"
                        % (func.pretty, lbl or "?"))
                    continue
                for name in set_names:
                    spec = TARGET_SETS[name]
                    if spec.get("boundary"):
                        continue   # host-ops boundary: cost 0
                    if spec.get("chain"):
                        d, m = self._chain_cost(name, mode)
                        if d > best:
                            best, best_next = d, m
                        continue
                    for m in self.sets[name]:
                        d = self._depth(m, mode, _stack)
                        if d > best:
                            best, best_next = d, m
                continue
            f = self.graph.resolve(func.tu, tgt)
            if f is not None:
                if f is func and recursion_frames > 1:
                    continue   # self-recursion charged via the limit
                d = self._depth(f, mode, _stack)
                if d > best:
                    best, best_next = d, f
                continue
            pretty, allowance = self._allowance(func.tu, tgt)
            if allowance is None:
                self.problem(
                    "no stack-usage data for '%s' called from '%s'%s "
                    "(imported/BDPI or unlisted external function)"
                    % (pretty, func.pretty,
                       (" at %s" % lbl) if lbl else ""))
                allowance = 0
            d = allowance + self.call_overhead
            if d > best:
                best, best_next = d, ("ext", pretty, allowance)

        stack.pop()
        on_stack.discard(key)
        total = (frame + self.call_overhead) * recursion_frames + best
        memo[key] = (total, best_next)
        return total

    def depth(self, func):
        return self._depth(func, "full")

    def worst_path(self, func):
        path = []
        cur = func
        while cur is not None and not isinstance(cur, tuple):
            entry = self._memo["full"].get(id(cur)) \
                or self._memo["nochain"].get(id(cur))
            if entry is None:
                break
            path.append((cur.pretty, cur.su, cur.qual, entry[0]))
            cur = entry[1]
        if isinstance(cur, tuple):
            path.append((cur[1] + " [external allowance]", cur[2],
                         "allowance", cur[2] + self.call_overhead))
        return path




def find_entries(graph, pattern):
    cre = re.compile(pattern)
    return [f for f in graph.all_defined() if cre.search(f.pretty)]


def format_path(path, overhead):
    lines = []
    for (pretty, su, qual, depth) in path:
        lines.append("  %8d  frame %6s (%s)  %s"
                     % (depth, su if su is not None else "?",
                        qual or "?", pretty))
    lines.append("  (each frame includes a %d-byte call overhead allowance)"
                 % overhead)
    return lines


def main(argv):
    ap = argparse.ArgumentParser(
        description="Compute a static stack-depth bound for a Bluesim model")
    ap.add_argument("ci", nargs="+",
                    help=".ci files, or directories scanned for *.ci")
    ap.add_argument("--entry", action="append", default=[],
                    metavar="REGEX",
                    help="entry-point regex over demangled names "
                         "(default: bk_sync_run and bk_sync_step)")
    ap.add_argument("--call-overhead", type=int,
                    default=CALL_OVERHEAD_DEFAULT, metavar="BYTES",
                    help="per-call overhead added to every frame "
                         "(default %d)" % CALL_OVERHEAD_DEFAULT)
    ap.add_argument("--chain-multiplicity", type=int,
                    default=CHAIN_MULTIPLICITY_DEFAULT, metavar="N",
                    help="assumed same-class multiplicity of synchronous "
                         "reset chains (default %d)"
                         % CHAIN_MULTIPLICITY_DEFAULT)
    ap.add_argument("--format-vla-bytes", type=int,
                    default=FORMAT_VLA_BYTES_DEFAULT, metavar="BYTES",
                    help="allowance for each width-sized VLA frame in the "
                         "wide formatting/division runtime (default %d)"
                         % FORMAT_VLA_BYTES_DEFAULT)
    ap.add_argument("--module-depth", type=int,
                    default=MODULE_DEPTH_DEFAULT, metavar="N",
                    help="assumed maximum module hierarchy depth, for "
                         "$display %%m (default %d)" % MODULE_DEPTH_DEFAULT)
    ap.add_argument("--report", action="store_true",
                    help="print a worst-path report to stderr")
    ap.add_argument("--emit-tu", metavar="FILE",
                    help="write a C++ TU defining bs_stack_depth_bound")
    args = ap.parse_args(argv)

    graph = CallGraph()
    ci_files = []
    for arg in args.ci:
        if os.path.isdir(arg):
            found = sorted(
                os.path.join(arg, n) for n in os.listdir(arg)
                if n.endswith(".ci"))
            if not found:
                raise SystemExit("error: no .ci files in directory %s" % arg)
            ci_files.extend(found)
        else:
            ci_files.append(arg)
    for path in ci_files:
        graph.load_ci(path)
    graph.index()

    entry_patterns = args.entry or [r"^tStatus bk_sync_run\(",
                                    r"^tStatus bk_sync_step\("]

    analysis = Analysis(graph, args.call_overhead, args.chain_multiplicity,
                        args.format_vla_bytes, args.module_depth)

    bound = 0
    report = []
    for rx in entry_patterns:
        funcs = find_entries(graph, rx)
        if not funcs:
            analysis.problem("entry point %r not found "
                             "(kernel .ci files missing?)" % rx)
            continue
        for f in funcs:
            d = analysis.depth(f)
            bound = max(bound, d)
            report.append("entry %s: %d bytes" % (f.pretty, d))
            report.extend(format_path(analysis.worst_path(f),
                                      args.call_overhead))
    report.extend(analysis.notes)

    # Secondary entries, reported only (never part of the bound):
    # model construction and simulation setup.
    secondary = []
    for rx in [r"^void\* new_MODEL_", r"^tSimStateHdl bk_sync_init\("]:
        for f in find_entries(graph, rx):
            n_problems = len(analysis.problems)
            d = analysis.depth(f)
            new = analysis.problems[n_problems:]
            if not new:
                secondary.append("secondary entry %s: %d bytes "
                                 "(not part of the exposed bound)"
                                 % (f.pretty, d))
            else:
                secondary.append("secondary entry %s: no bound (%s)"
                                 % (f.pretty, "; ".join(new)))
                del analysis.problems[n_problems:]

    if analysis.problems:
        sys.stderr.write("bluesim_stack_bound: NO SOUND BOUND -- "
                         "reporting 0 (no bound available):\n")
        for p in analysis.problems:
            sys.stderr.write("  * %s\n" % p)
        bound = 0

    if args.report:
        for line in report + secondary:
            sys.stderr.write(line + "\n")

    if args.emit_tu:
        emit_tu(args.emit_tu, bound, report + secondary, analysis.problems)

    sys.stdout.write("%d\n" % bound)
    return 0


def emit_tu(path, bound, report, problems):
    lines = []
    lines.append("/* Generated by bluesim_stack_bound.py -- do not edit.")
    lines.append(" *")
    if bound > 0:
        lines.append(" * Static stack-depth bound: %d bytes." % bound)
        for r in report:
            lines.append(" * " + r.replace("*/", "* /"))
    else:
        lines.append(" * No sound static stack bound is available for "
                     "this design:")
        for p in problems:
            lines.append(" *   " + p.replace("*/", "* /"))
    lines.append(" */")
    lines.append('#include "bluesim_types.h"')
    lines.append("")
    lines.append("/* Strong definition; the runtime carries a weak 0 "
                 "default (stack_bound_default.cxx). */")
    lines.append('extern "C" const tUInt64 bs_stack_depth_bound = %dllu;'
                 % bound)
    lines.append("")
    with open(path, "w") as fh:
        fh.write("\n".join(lines))


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
