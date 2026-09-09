#!/usr/bin/env python3
"""M0 spike driver: contract fixture -> verilate -> shim -> ctypes harness.

Implements the v4 shadow-vector protocol in Python against the shim ABI
and runs fixture scenarios.  Usage:
    driver.py <verilator-binary> <fixture-name> [workdir]
Exit 0 on scenario pass; nonzero with a specific message otherwise.
"""
import ctypes
import json
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).parent
sys.path.insert(0, str(HERE))
import metaparse  # noqa: E402
import shimgen    # noqa: E402


def serialize_param(name, spec):
    """Typed -G serialization (v4 sec 4.5): semantics, not text."""
    t = spec["type"]
    v = spec["value"]
    if t == "int_signed":
        return f"-G{name}={int(v)}"                      # signed decimal
    if t == "bits":
        return f"-G{name}={spec['width']}'h{v}"          # sized hex literal
    if t == "string":
        # verilog string literal; exact escaping of quotes/backslashes
        esc = str(v).replace("\\", "\\\\").replace('"', '\\"')
        return f'-G{name}="{esc}"'
    if t == "real":
        return f"-G{name}={float(v)!r}"                  # round-trip repr
    raise SystemExit(f"REFUSE(param-type): {name} has unsupported type {t}")


def build(vlt, contract, workdir):
    top = contract["verilog_name"]
    work = Path(workdir); work.mkdir(parents=True, exist_ok=True)
    rtl = HERE / "rtl" / f"{top}.v"

    gparams = [serialize_param(n, sp) for n, sp in
               (contract.get("params") or {}).items()]
    meta = metaparse.extract(vlt, top, [rtl], workdir=work / "meta",
                             extra_args=gparams)
    (work / "meta.json").write_text(json.dumps(meta, indent=2))

    if meta["has_delay"]:
        raise SystemExit(f"REFUSE(delay): {top} contains delay constructs")
    if meta["has_dpi"]:
        raise SystemExit(f"REFUSE(dpi): {top} imports/exports DPI")

    chash = shimgen.gen(contract, meta, work / "shim.cpp")

    obj = work / "obj"
    decl = HERE / "trs_printf_decl.h"
    defines = f"-DVL_USER_FATAL -DVL_PRINTF=trs_vlt_printf -include {decl} -fPIC"
    r = subprocess.run(
        [vlt, "--cc", "--no-timing", "--x-assign", "0", "--x-initial", "0",
         "-O2", "--assert", "--top-module", top, "-Mdir", str(obj),
         "-CFLAGS", defines] + gparams + [str(rtl)],
        capture_output=True, text=True)
    if r.returncode != 0:
        raise SystemExit(f"verilate failed:\n{r.stderr}")
    # DPI backstop: verilator emits V<top>__Dpi.h iff DPI is present --
    # deterministic on every version (5.020's XML has no DPI marker).
    if (obj / f"V{top}__Dpi.h").exists():
        raise SystemExit(f"REFUSE(dpi): {top} imports/exports DPI (V{top}__Dpi.h emitted)")
    r = subprocess.run(
        ["make", "-s", "-C", str(obj), "-f", f"V{top}.mk",
         f"V{top}__ALL.a", "verilated.o", "verilated_threads.o"],
        capture_output=True, text=True)
    if r.returncode != 0:
        raise SystemExit(f"model build failed:\n{r.stderr}\n{r.stdout}")

    vroot = subprocess.run([vlt, "--getenv", "VERILATOR_ROOT"],
                           capture_output=True, text=True).stdout.strip()
    so = work / f"lib{top}_shim.so"
    r = subprocess.run(
        ["g++", "-shared", "-fPIC", "-O2", "-std=c++17",
         "-DVL_USER_FATAL", "-DVL_PRINTF=trs_vlt_printf", "-include", str(HERE / "trs_printf_decl.h"),
         "-I", str(obj), "-I", f"{vroot}/include", "-I", f"{vroot}/include/vltstd",
         str(work / "shim.cpp"),
         str(obj / f"V{top}__ALL.a"),
         str(obj / "verilated.o"), str(obj / "verilated_threads.o"),
         "-lpthread", "-lz", "-o", str(so)],
        capture_output=True, text=True)
    if r.returncode != 0:
        raise SystemExit(f"shim link failed:\n{r.stderr}")
    return so, chash


class Bvi:
    """Shadow-vector protocol over the shim ABI (v4 sec 4.1)."""

    def __init__(self, so_path, contract):
        self.lib = ctypes.CDLL(str(so_path))
        self.lib.vlt_new.restype = ctypes.c_void_p
        self.lib.vlt_new.argtypes = [ctypes.c_char_p, ctypes.c_int,
                                     ctypes.POINTER(ctypes.c_char_p)]
        self.lib.vlt_contract.restype = ctypes.c_char_p
        self.lib.vlt_fatal_msg.restype = ctypes.c_char_p
        self.contract = contract
        self.pidx = {p["name"]: i for i, p in enumerate(contract["ports"])}
        self.h = ctypes.c_void_p(self.lib.vlt_new(b"m0", 0, None))
        assert self.h.value, "vlt_new failed"
        self.shadow = {}       # port name -> value (pending publication)
        self.published = {}    # last published values
        self.dirty = True      # born true (initial blocks must run)
        self.pending_edges = {}  # port name -> level
        self.en_group = []     # ENs to clear post-edge
        self.time = 0
        # -- TRS_BVI_CHECK=observe (spike form) --------------------------
        self.check = False
        self.witnesses = []
        self.epoch = {}        # port -> publish epoch
        self.obs = {}          # output port -> (value, epoch snapshot)
        self.cones = self._declared_cones()
        self.struct_ports = {p["name"] for p in contract["ports"]
                             if p["kind"] in ("clock", "reset")}

    def _declared_cones(self):
        """Declared dependency cone per output port: owning method's args
        + enable, plus declared path sources targeting it."""
        cones = {}
        for m in self.contract["methods"]:
            outs = list(m.get("results") or [])
            if m.get("rdy"):
                outs.append(m["rdy"])
            for o in outs:
                cone = set(m.get("args") or [])
                if m.get("enable"):
                    cone.add(m["enable"])
                cones.setdefault(o, set()).update(cone)
        for p in self.contract.get("paths", []):
            cones.setdefault(p["to"], set()).add(p["from"])
        return cones

    def _limbs(self, width, value):
        n = max(1, (width + 63) // 64)
        arr = (ctypes.c_uint64 * n)()
        for i in range(n):
            arr[i] = (value >> (64 * i)) & 0xFFFFFFFFFFFFFFFF
        return arr

    def _set(self, name, value):
        p = self.contract["ports"][self.pidx[name]]
        rc = self.lib.vlt_set(self.h, self.pidx[name],
                              self._limbs(p["width"], value))
        assert rc == 0, f"vlt_set({name}) rc={rc}: {self.lib.vlt_fatal_msg().decode()}"

    def _get(self, name):
        p = self.contract["ports"][self.pidx[name]]
        n = max(1, (p["width"] + 63) // 64)
        arr = (ctypes.c_uint64 * n)()
        rc = self.lib.vlt_get(self.h, self.pidx[name], arr)
        assert rc == 0, f"vlt_get({name}) rc={rc}"
        v = 0
        for i in range(n):
            v |= arr[i] << (64 * i)
        return v

    def drive(self, name, value):
        """Shadow update -- no eval."""
        if self.published.get(name) != value:
            self.shadow[name] = value

    def _publish_and_settle(self):
        for k, v in self.shadow.items():
            self._set(k, v)
            self.published[k] = v
            self.epoch[k] = self.epoch.get(k, 0) + 1
            self.dirty = True
        self.shadow.clear()
        if self.dirty:
            self.lib.vlt_set_time(self.h, self.time)
            rc = self.lib.vlt_eval(self.h)
            assert rc == 0, f"eval failed: {self.lib.vlt_fatal_msg().decode()}"
            self.dirty = False
            if self.check:
                self._check_observed()

    def _check_observed(self):
        """A sound witness: a previously-observed output changed while no
        input in its DECLARED cone changed (and no clock/reset moved)."""
        for port, (val, snap) in list(self.obs.items()):
            cur = self._get(port)
            if cur == val:
                continue
            cone = self.cones.get(port, set()) | self.struct_ports
            cone_moved = any(self.epoch.get(p, 0) != snap.get(p, 0)
                             for p in cone)
            changed = [p for p in self.epoch
                       if self.epoch[p] != snap.get(p, 0)]
            if not cone_moved:
                self.witnesses.append({
                    "port": port, "old": val, "new": cur,
                    "changed_inputs": changed,
                    "verdict": "DYNAMIC_LIE: undeclared influence or protocol violation"})
            self.obs[port] = (cur, dict(self.epoch))

    # -- protocol operations -------------------------------------------
    def call_action(self, method, **args):
        m = self._method(method)
        for aname, aval in args.items():
            self.drive(aname, aval)
        if m["enable"]:
            self.drive(m["enable"], 1)
            self.en_group.append(m["enable"])

    def observe(self, port):
        """Observation frontier: publish everything dirty, settle, read."""
        self._publish_and_settle()
        v = self._get(port)
        if self.check:
            self.obs[port] = (v, dict(self.epoch))
        return v

    def commit_edge(self, clock_ports_high, dt=5):
        """Three-phase batched commit (v4 4.1 step 4)."""
        self.time += dt
        # (a) inputs
        self._publish_and_settle()
        # (b) edges -- all coincident clock levels, one eval
        for cp, lvl in clock_ports_high.items():
            self._set(cp, lvl)
            self.published[cp] = lvl
        self.lib.vlt_set_time(self.h, self.time)
        rc = self.lib.vlt_eval(self.h)
        assert rc == 0, f"edge eval failed: {self.lib.vlt_fatal_msg().decode()}"
        # (c) post -- clear ENs, settle
        for en in self.en_group:
            self._set(en, 0)
            self.published[en] = 0
        self.en_group.clear()
        rc = self.lib.vlt_eval(self.h)
        assert rc == 0
        self.dirty = False
        self.obs.clear()   # new instant: edge commits legitimately change outputs

    def set_reset(self, port, level):
        self.drive(port, level)
        self._publish_and_settle()

    def _method(self, name):
        for m in self.contract["methods"]:
            if m["name"] == name:
                return m
        raise KeyError(name)

    def close(self):
        self.lib.vlt_free(self.h)


def scenario_counter(so, contract):
    b = Bvi(so, contract)
    # startup: deassert-init, settle (initial blocks run), then t=0 assert
    b.drive("CLK", 0)
    b.drive("RST_N", 1)
    _ = b.observe("count")            # forces initial publish+settle
    b.set_reset("RST_N", 0)           # real assertion transition
    b.commit_edge({"CLK": 1}); b.commit_edge({"CLK": 0})
    b.set_reset("RST_N", 1)           # deassert
    got = []
    for cyc in range(4):
        rdy = b.observe("RDY_bump")
        assert rdy == 1, f"RDY low at cycle {cyc}"
        b.call_action("bump", bump_amt=3)
        pre = b.observe("count")      # value read BEFORE edge: old value
        b.commit_edge({"CLK": 1})
        post = b.observe("count")     # after edge: committed
        b.commit_edge({"CLK": 0})
        got.append((pre, post))
    expect = [(0, 3), (3, 6), (6, 9), (9, 12)]
    assert got == expect, f"counter mismatch: {got} vs {expect}"
    b.close()
    return "counter: pre/post per cycle == hand-derived NBA semantics"


SCENARIOS = {"counter": scenario_counter}

def scenario_shadow(so, contract):
    """Self-SBR shadow group (v4 4.2, Ravi's atomic-read condition).

    Two calls to m in one instant form a replacement group.  Pins:
      1. The LAST caller's AV read is exact (== netlist settled value).
      2. An EARLIER caller's per-call read differs from the netlist's
         settled value -- the divergence the consumed-and-coactive
         refusal exists to keep out of accepted designs.
      3. The edge commits the FINAL selected argument (netlist mux).
    """
    b = Bvi(so, contract)
    b.drive("CLK", 0); b.drive("RST_N", 1)
    _ = b.observe("LAST")
    b.set_reset("RST_N", 0)
    b.commit_edge({"CLK": 1}); b.commit_edge({"CLK": 0})
    b.set_reset("RST_N", 1)

    # instant: caller A then caller B, same shadow group
    b.call_action("m", IN=10)
    early = b.observe("OUT")          # A's per-call read
    b.call_action("m", IN=20)         # replacement: B's args win
    late = b.observe("OUT")           # B's read (last caller)
    assert early == 11, f"early per-call read: {early} != 11"
    assert late == 21, f"last-caller read: {late} != 21"
    # netlist settled value == 21 == late: last-caller consumption is exact;
    # 'early' (11) is what a non-final consumer would wrongly keep -- refused.
    b.commit_edge({"CLK": 1})
    latched = b.observe("LAST")
    assert latched == 20, f"edge latched {latched} != 20 (replacement)"
    b.commit_edge({"CLK": 0})

    # next instant: only A calls -> A is the last firing caller, exact.
    b.call_action("m", IN=7)
    only = b.observe("OUT")
    assert only == 8, f"solo caller read: {only} != 8"
    b.commit_edge({"CLK": 1})
    assert b.observe("LAST") == 7
    b.close()
    return "shadow: last-caller exact, early-read divergence pinned, edge = replacement"

SCENARIOS["shadow"] = scenario_shadow

def scenario_argrdy(so, contract):
    """Argument-dependent RDY: the readiness read's cone includes an arg
    port, so a frontier read after driving the arg must see the settled
    combinational answer (even -> ready, odd -> not ready)."""
    b = Bvi(so, contract)
    b.drive("CLK", 0); b.drive("RST_N", 1)
    _ = b.observe("STORED")
    b.set_reset("RST_N", 0)
    b.commit_edge({"CLK": 1}); b.commit_edge({"CLK": 0})
    b.set_reset("RST_N", 1)

    b.drive("put_x", 4)
    assert b.observe("RDY_put") == 1, "even arg should be ready"
    b.drive("put_x", 5)
    assert b.observe("RDY_put") == 0, "odd arg should not be ready"
    b.drive("put_x", 6)
    assert b.observe("RDY_put") == 1
    b.call_action("put", put_x=6)
    b.commit_edge({"CLK": 1})
    assert b.observe("STORED") == 6
    b.commit_edge({"CLK": 0})
    b.close()
    return "argrdy: RDY tracks the argument cone at frontiers; edge commits the enabled value"

SCENARIOS["argrdy"] = scenario_argrdy

def _startup(b, clocks=("CLK",), rst="RST_N"):
    for c in clocks:
        b.drive(c, 0)
    b.drive(rst, 1)
    b._publish_and_settle()
    b.set_reset(rst, 0)
    b.commit_edge({c: 1 for c in clocks}); b.commit_edge({c: 0 for c in clocks})
    b.set_reset(rst, 1)


def scenario_liar(so, contract):
    """Undeclared path (put_x -> PEEK) caught by observe mode: a sound
    DYNAMIC_LIE witness with attribution, from an ordinary guard probe."""
    b = Bvi(so, contract)
    b.check = True
    _startup(b)
    v0 = b.observe("PEEK")            # observed under put_x's initial value
    b.drive("put_x", 0x5A)            # a guard probe drives the arg...
    _ = b.observe("STORED")           # ...next frontier publishes it
    assert b.witnesses, "liar not caught"
    w = b.witnesses[0]
    assert w["port"] == "PEEK" and "put_x" in w["changed_inputs"], w
    assert v0 == 0 and w["new"] == 0x5A, w
    b.close()
    return f"liar: DYNAMIC_LIE witness fired ({w['port']} moved 0x{w['old']:x}->0x{w['new']:x} on undeclared put_x)"


def scenario_xing(so, contract):
    """Coincident two-clock NBA batching: the batched commit captures the
    OLD source register; a per-edge sequential protocol shoot-throughs.
    Pins the review's empirical counterexample permanently."""
    b = Bvi(so, contract)
    _startup(b, clocks=("SCLK", "DCLK"))
    b.call_action("send", s_din=5)
    b.commit_edge({"SCLK": 1, "DCLK": 1})       # ONE batched presentation
    sreg, dreg = b.observe("SREG"), b.observe("DREG")
    assert (sreg, dreg) == (5, 0), f"batched: {(sreg, dreg)} != (5, 0) [NBA old-value]"
    b.commit_edge({"SCLK": 0, "DCLK": 0})
    b.commit_edge({"SCLK": 1, "DCLK": 1})
    assert b.observe("DREG") == 5
    b.close()

    # The WRONG protocol, demonstrated: sequential per-clock commits.
    bad = Bvi(so, contract)
    _startup(bad, clocks=("SCLK", "DCLK"))
    bad.call_action("send", s_din=5)
    bad.commit_edge({"SCLK": 1})                 # source domain commits...
    bad.commit_edge({"DCLK": 1})                 # ...then dest sees NEW sreg
    shoot = bad.observe("DREG")
    assert shoot == 5, f"sequential protocol unexpectedly gave {shoot}"
    bad.close()
    return "xing: batched commit == NBA old-value; sequential per-edge protocol shoot-throughs (divergence pinned)"


def scenario_violator(so, contract):
    """Protocol violator: state clocked by a raw argument transition.
    Inter-edge argument values have no BVI meaning; observe mode reports
    the resulting undeclared influence as a witness."""
    b = Bvi(so, contract)
    b.check = True
    _startup(b)
    c0 = b.observe("COUNT")
    for v in (1, 0, 1, 0):            # guard-probe-like transitions, EN low
        b.drive("put_x", v)
        b._publish_and_settle()
    c1 = b.observe("COUNT")
    assert c1 - c0 == 2, f"violator advanced {c1 - c0}, expected 2 posedges"
    assert b.witnesses, "violation not witnessed"
    b.close()
    return f"violator: arg-transition-clocked state advanced {c1 - c0} with EN low; witnessed"


def scenario_xprobe(so, contract):
    """Two-state accepted limitation: === 1'bx readiness is constant false
    under --x-initial 0, where a 4-state simulator starts ready.  CONTROL
    divergence, recorded -- not a bug in the mechanism."""
    b = Bvi(so, contract)
    _startup(b)
    rdy = b.observe("RDYX")
    assert rdy == 0, f"two-state x-probe readiness: {rdy} != 0"
    b.commit_edge({"CLK": 1})
    assert b.observe("RDYX") == 0
    b.close()
    return "xprobe: X-probing RDY constant-false under two-state (4-state would start true) -- accepted limitation"

SCENARIOS["liar"] = scenario_liar
SCENARIOS["xing"] = scenario_xing
SCENARIOS["violator"] = scenario_violator
SCENARIOS["xprobe"] = scenario_xprobe

def scenario_params(so, contract):
    """Typed -G semantics: signed sign-extension, wide limbs through the
    VlWide get path, and string/real round-trip captured via the
    VL_PRINTF redirect callback (initial-block $display)."""
    lines = []
    b = Bvi(so, contract)
    CB = ctypes.CFUNCTYPE(None, ctypes.c_void_p, ctypes.c_char_p)
    cb = CB(lambda _c, txt: lines.append(txt.decode()))
    b.lib.vlt_set_output_cb(cb, None)
    b.drive("CLK", 0)
    sint = b.observe("P_SINT")        # first settle also runs initial blocks
    wide = b.observe("P_WIDE")
    assert sint == 0xFFFFFFFB, f"signed -5 came back 0x{sint:08x}"
    assert wide == 0x0123456789abcdef01234567, f"wide: 0x{wide:x}"
    out = "".join(lines)
    assert "STR=hello" in out and "RVAL=2.5" in out, f"display capture: {out!r}"
    b.close()
    return f"params: signed/-5 exact, 96-bit wide exact through VlWide, display capture {out.strip()!r}"

SCENARIOS["params"] = scenario_params

def scenario_fatal(so, contract):
    """$fatal containment: the model's assertion fires at the enabled
    edge; the host must SURVIVE, the eval must return an error, and the
    fatal message must be retrievable."""
    b = Bvi(so, contract)
    _startup(b)
    assert b.observe("OUT") == 7
    b.call_action("go")
    blew = False
    try:
        b.commit_edge({"CLK": 1})
    except AssertionError:
        blew = True
    assert blew, "fatal edge did not report an error"
    fin = b.lib.vlt_finished(b.h)
    msg = b.lib.vlt_fatal_msg().decode()
    assert fin & 2, f"fatal flag not set (fin={fin})"
    # 5.020 routes assertion failure through $stop; the contained message
    # carries the SOURCE LOCATION, while the user text goes to the output
    # stream.  Containment is the property under test, not the wording.
    assert "BviFatal.v" in msg, f"fatal msg lacks location: {msg!r}"
    b.close()
    return f"fatal: host survived; contained message {msg.split(': ')[-1]!r}"

SCENARIOS["fatal"] = scenario_fatal







def main():
    vlt, fixture = sys.argv[1], sys.argv[2]
    workdir = sys.argv[3] if len(sys.argv) > 3 else str(HERE / "out" / fixture)
    contract = json.loads((HERE / "contracts" / f"{fixture}.json").read_text())
    so, chash = build(vlt, contract, workdir)
    msg = SCENARIOS[fixture](so, contract)
    print(f"PASS [{fixture}] contract={chash} :: {msg}")


if __name__ == "__main__":
    main()
