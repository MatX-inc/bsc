#!/usr/bin/env python3
"""P1 differential sweep: interpreter vs Bluesim over the testsuite.

For every testsuite `sys*.out.expected` whose top module can be located:
compile with `bsc -sim`, link with `-bir`, run the reference Bluesim
executable and `trs run` on the exported BIR, and diff stdout.

Every failure is classified so the output is a work list, not a score:
  COMPILE_FAIL   bsc could not compile the design (env/flags/etc)
  LINK_FAIL      bsc link failed for reasons other than export
  NOT_SUPPORTED  reference Bluesim cannot run the design either (BVI)
  EXPORT_FAIL    SimExportIR internalError (unhandled IR construct)
  REF_FAIL       reference Bluesim run failed/timed out
  DECODE_FAIL    trs could not decode the .bir
  INTERP_PANIC   interpreter hit an unimplemented feature (reason kept)
  TIMEOUT        trs exceeded the per-run limit (known-slow interp)
  DIFF           both ran; stdout differs
  PASS           bit-identical stdout
"""

import argparse
import collections
import json
import multiprocessing
import os
import re
import shutil
import subprocess
import sys

REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
BSC = os.path.join(REPO, "inst", "bin", "bsc")
# the release build keeps heavyweight tests (SHA512, GlibcRandom) well
# under the timeout; fall back to debug if it hasn't been built.
# DIFFSWEEP_TRS (set by --trs) is read at module level because pool
# workers re-import this module under spawn/forkserver (Python >= 3.14
# default): a global assigned only in main() would silently revert to
# the default path in every worker.
TRS = os.environ.get("DIFFSWEEP_TRS", "")
if not TRS:
    TRS = os.path.join(REPO, "src", "trs", "target", "release", "trs")
    if not os.path.exists(TRS):
        TRS = os.path.join(REPO, "src", "trs", "target", "debug", "trs")
ENV = dict(os.environ, PATH=os.path.join(REPO, "inst", "bin") + ":" + os.environ["PATH"])

MAX_CYCLES = "4000"
# reference runs, compiles, and normal trs runs.  Env-able for the
# selfcheck sweep (TRS_SELFCHECK=1): the interp shadow rides along, so
# RegFile-heavy designs legitimately exceed the flat budget.
TIMEOUT = float(os.environ.get("DIFFSWEEP_TIMEOUT", "60"))
# Enable-gated long tests (their directory carries a *.exp.golden, the
# bsc.long_tests convention) are exactly the designs whose interpreter
# runs blow up; they get a tight leash instead of the flat TIMEOUT:
# max(TIMEOUT_FLOOR, TIMEOUT_FACTOR x the reference's wall time).
# Every limit is additionally floored at the reference's build+run
# wall: sync-JIT compiles inside the timed window, and the reference's
# own compile (C++ codegen + cc) happened off the clock.
TIMEOUT_FLOOR = float(os.environ.get("DIFFSWEEP_TIMEOUT_FLOOR", "5"))
TIMEOUT_FACTOR = float(os.environ.get("DIFFSWEEP_TIMEOUT_FACTOR", "5"))
# --aot: trs link to a persistent artifact, then run the wrapper
# script (build cost is NOT counted against the sim-time leash — the
# incumbents amortize their compiles the same way)
AOT = os.environ.get("DIFFSWEEP_AOT", "") == "1"
# golden-output cache directory ("" = off): reference-side results are
# keyed by (bsc binary, top, cycles, every design input file) and
# replayed on hit, so a regression sweep pays only the trs side —
# measured, the reference apparatus (bsc compile + Bluesim build + ref
# run) is ~95% of a full sweep's wall clock
GOLDEN = os.environ.get("DIFFSWEEP_GOLDEN", "")
_BSC_ID = None


def _bsc_id():
    """Content hash of the bsc binary: golden entries from another bsc
    build must never replay.  An installed `bsc` is a wrapper SCRIPT
    that never changes across rebuilds (it exec's core/bsc beside it):
    hashing only the wrapper kept the cache warm across bsc rebuilds
    and replayed stale reference .birs — hash the real executable too."""
    global _BSC_ID
    if _BSC_ID is None:
        import hashlib
        h = hashlib.sha256()
        targets = [BSC]
        core = os.path.join(os.path.dirname(BSC), "core",
                            os.path.basename(BSC))
        if os.path.exists(core):
            targets.append(core)
        for t in targets:
            with open(t, "rb") as f:
                for chunk in iter(lambda: f.read(1 << 20), b""):
                    h.update(chunk)
        _BSC_ID = h.hexdigest().encode()
    return _BSC_ID


# every file class one_test copies into the work dir feeds the key —
# sources, BDPI C/H (incl. .keep), and data files (they shape the
# reference OUTPUT, not just the build)
_GOLD_INPUTS = (".bsv", ".bs", ".c", ".c.keep", ".h", ".h.keep",
                ".dat", ".hex", ".bin", ".txt", ".mem", ".vec",
                ".input", ".vectors", ".handbuilt", ".rom", ".data")


def _golden_key(testdir, top):
    import hashlib
    h = hashlib.sha256()
    h.update(_bsc_id())
    h.update(top.encode())
    h.update(MAX_CYCLES.encode())
    for f in sorted(os.listdir(testdir)):
        if f.startswith("vpi_") or not f.endswith(_GOLD_INPUTS):
            continue
        h.update(f.encode())
        try:
            with open(os.path.join(testdir, f), "rb") as fh:
                h.update(fh.read())
        except OSError:
            h.update(b"<unreadable>")
    return h.hexdigest()


def find_source(testdir, top):
    def scan(name):
        for ext in (".bsv", ".bs"):
            cand = os.path.join(testdir, name + ext)
            if os.path.exists(cand):
                return cand
        pats = [
            re.compile(r"module\s+(?:\[[^\]]*\]\s*)?" + re.escape(name) + r"\b"),
            re.compile(r"`define\s+\w+\s+" + re.escape(name) + r"\b"),
        ]
        # .bs (Bluespec Haskell syntax): a top-level type signature or
        # binding for the module name (sysMips was invisible to the sweep
        # until the full testsuite caught a bug in it — scan .bs too)
        bs_pats = [
            re.compile(r"^" + re.escape(name) + r"\s*::", re.M),
            re.compile(r"^" + re.escape(name) + r"\s*=", re.M),
        ]
        for f in sorted(os.listdir(testdir)):
            if f.endswith((".bsv", ".bs")):
                try:
                    text = open(os.path.join(testdir, f), errors="replace").read()
                except OSError:
                    continue
                use = bs_pats if f.endswith(".bs") else pats
                if any(p.search(text) for p in use):
                    return os.path.join(testdir, f)
        # filename convention: sysFoo defined in Foo.bsv (or .bs)
        if name.startswith("sys"):
            for ext in (".bsv", ".bs"):
                cand = os.path.join(testdir, name[3:] + ext)
                if os.path.exists(cand):
                    return cand
        return None

    # expected files may carry a variant suffix: sysFoo_flagvariant
    name = top
    while True:
        found = scan(name)
        if found:
            return found, name
        if "_" not in name:
            return None
        name = name.rsplit("_", 1)[0]


def run(cmd, cwd, timeout=TIMEOUT, env=None):
    try:
        return subprocess.run(
            cmd, cwd=cwd, env=env or ENV, timeout=timeout,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True,
        )
    except subprocess.TimeoutExpired:
        return None


def _gold_terminal(gdir, cls, note):
    """Cache a deterministic terminal classification (meta written
    last: its presence is the entry's validity marker)."""
    if not gdir:
        return
    try:
        os.makedirs(gdir, exist_ok=True)
        with open(os.path.join(gdir, "meta.json"), "w") as f:
            json.dump({"status": cls, "note": note}, f)
    except OSError:
        pass


def _gold_save(gdir, wk, top, ref, ref_secs, ref_build_secs):
    """Cache a successful reference: the .bir (trs's input), any
    .bdpi.so, and the golden outputs + timings."""
    if not gdir:
        return
    try:
        os.makedirs(gdir, exist_ok=True)
        for f in os.listdir(wk):
            if f == top + ".bir" or f.endswith(".bdpi.so"):
                shutil.copy(os.path.join(wk, f), gdir)
        with open(os.path.join(gdir, "ref.stdout"), "w") as f:
            f.write(ref.stdout)
        with open(os.path.join(gdir, "ref.stderr"), "w") as f:
            f.write(ref.stderr)
        with open(os.path.join(gdir, "meta.json"), "w") as f:
            json.dump({"status": "REF_OK", "returncode": ref.returncode,
                       "ref_secs": ref_secs,
                       "ref_build_secs": ref_build_secs}, f)
    except OSError:
        pass


def one_test(job):
    testdir, top, workroot = job
    rel = os.path.relpath(testdir, REPO)
    wk = os.path.abspath(os.path.join(workroot, rel.replace("/", "_") + "_" + top))
    shutil.rmtree(wk, ignore_errors=True)
    os.makedirs(wk, exist_ok=True)

    found = find_source(testdir, top)
    if found is None:
        return (rel, top, "NO_SOURCE", "")
    src, top = found

    # data files ($readmem, file reads) load relative to the run directory
    for f in os.listdir(testdir):
        if f.endswith((".dat", ".hex", ".bin", ".txt", ".mem", ".vec", ".input", ".vectors", ".handbuilt", ".rom", ".data")):
            try:
                shutil.copy(os.path.join(testdir, f), wk)
            except OSError:
                pass

    # trs-only golden-compared designs (<top>.trsonly.expected
    # sidecar): no reference Bluesim executable exists BY DESIGN —
    # classic Bluesim refuses the design class (top-level args/params,
    # always_enabled top methods, dynamic scheduling) — so the "reference" is the stored
    # hand-derived golden and the sidecar's bindings/engine
    # expectations.  The name ends in "expected" because the
    # testsuite's `make clean` deletes sys*/mk* files EXCEPT
    # %expected/%.exp/... (cleanonly.mk) — a bare <top>.trsonly was
    # silently removed by every fullparallel-setup.
    trsonly = os.path.join(testdir, top + ".trsonly.expected")
    if os.path.exists(trsonly):
        return _trsonly_test(rel, top, wk, testdir, src, trsonly)

    gdir = None
    if GOLDEN:
        gdir = os.path.join(GOLDEN, _golden_key(testdir, top))
        meta_p = os.path.join(gdir, "meta.json")
        if os.path.exists(meta_p):
            try:
                meta = json.load(open(meta_p))
            except (OSError, ValueError):
                meta = None
            if meta and meta.get("status") != "REF_OK":
                # cached terminal classification (COMPILE_FAIL etc.)
                return (rel, top, meta["status"], meta.get("note", ""))
            if meta:
                # cached reference: reconstitute without touching bsc
                for f in os.listdir(gdir):
                    if f.endswith((".bir", ".bdpi.so")):
                        shutil.copy(os.path.join(gdir, f), wk)
                import types
                ref = types.SimpleNamespace(
                    stdout=open(os.path.join(gdir, "ref.stdout"),
                                errors="replace").read(),
                    stderr=open(os.path.join(gdir, "ref.stderr"),
                                errors="replace").read(),
                    returncode=meta["returncode"],
                )
                return _trs_side(rel, top, wk, testdir,
                                 os.path.join(wk, top + ".bir"), ref,
                                 meta["ref_secs"], meta["ref_build_secs"])

    # sources are COPIED into the work dir and testdir stays OFF the
    # search path: fullparallel leaves version-matched .bo/.ba residue
    # in-tree, and -u would silently reuse it — substituting the .exp
    # recipes' flag flavor (e.g. -keep-fires) for the sweep's own
    # elaboration (measured: traffic_light_controller_separate linked
    # a stale-.ba design for two whole sweep generations)
    for f in os.listdir(testdir):
        if f.endswith((".bsv", ".bs")):
            try:
                shutil.copy(os.path.join(testdir, f), wk)
            except OSError:
                pass

    common = ["-bdir", wk, "-info-dir", wk, "-simdir", wk,
              "-p", wk + ":+"]

    # BDPI designs need the user's C files at link (the .exp recipes pass
    # them; .c.keep is the testsuite convention for inactive copies)
    cfiles = []
    has_bdpi = any(
        'import "BDPI"' in open(os.path.join(testdir, f), errors="replace").read()
        for f in os.listdir(testdir) if f.endswith(".bsv")
    )
    if has_bdpi:
        for f in os.listdir(testdir):
            # Verilog-VPI wrapper residue (vpi_wrapper_*.c,
            # vpi_startup_array.c) is generated IN-TREE by testsuite
            # Verilog runs and needs vpi_user.h — never a BDPI link
            # input (40 phantom LINK_FAILs after a fullparallel run)
            if f.startswith("vpi_"):
                continue
            if f.endswith(".c"):
                shutil.copy(os.path.join(testdir, f), wk)
                cfiles.append(f)
            elif f.endswith(".c.keep"):
                shutil.copy(os.path.join(testdir, f), os.path.join(wk, f[:-5]))
                cfiles.append(f[:-5])
            elif f.endswith(".h"):
                # C sources #include local headers (the foreign battery
                # was LINK_FAIL-invisible for want of common.h)
                shutil.copy(os.path.join(testdir, f), wk)
            elif f.endswith(".h.keep"):
                shutil.copy(os.path.join(testdir, f), os.path.join(wk, f[:-5]))
    r = run([BSC, "-sim", "-u", "-g", top] + common + [src], cwd=wk, timeout=180)
    if r is None or r.returncode != 0:
        msg = "" if r is None else (r.stderr + r.stdout)
        if r is None:
            return (rel, top, "COMPILE_FAIL", "compile timeout")
        if "(G0097)" in msg or "(G0098)" in msg:
            # Inout is not supported by Bluesim at all
            _gold_terminal(gdir, "NOT_SUPPORTED", first_error(msg))
            return (rel, top, "NOT_SUPPORTED", first_error(msg))
        _gold_terminal(gdir, "COMPILE_FAIL", first_error(msg))
        return (rel, top, "COMPILE_FAIL", first_error(msg))

    import time as _time
    tb0 = _time.monotonic()
    # 420s default: sysBRAM0Test/sysFloatTest reference builds measure
    # 166-256s depending on load and FLAPPED at a 180s ceiling
    # (LINK_FAIL "unknown" = timeout with empty stderr).  The
    # ConflictFree*Large pair measures 489s cold on a 4-CPU box —
    # DIFFSWEEP_BUILD_TIMEOUT raises the ceiling for such rechecks.
    build_limit = int(os.environ.get("DIFFSWEEP_BUILD_TIMEOUT", "420"))
    r = run([BSC, "-sim", "-bir", "-e", top, "-o", "sim.exe"] + common + cfiles,
            cwd=wk, timeout=build_limit)
    ref_build_secs = _time.monotonic() - tb0
    if r is None or r.returncode != 0:
        msg = "" if r is None else (r.stderr + r.stdout)
        if "SimExportIR" in msg:
            cls = "EXPORT_FAIL"
        elif "(G0084)" in msg or ("Bluesim" in msg and "import" in msg):
            # reference Bluesim cannot run this design either (BVI import)
            cls = "NOT_SUPPORTED"
        else:
            cls = "LINK_FAIL"
        if cls != "LINK_FAIL":
            # LINK_FAIL stays uncached: it includes build TIMEOUTS,
            # which are load- and box-dependent (ConflictFree*Large)
            _gold_terminal(gdir, cls, first_error(msg))
        return (rel, top, cls, first_error(msg))

    bir = os.path.join(wk, top + ".bir")
    if not os.path.exists(bir):
        return (rel, top, "EXPORT_FAIL", "no .bir produced")

    t0 = _time.monotonic()
    ref = run(["./sim.exe", "-m", MAX_CYCLES], cwd=wk)
    ref_secs = _time.monotonic() - t0
    if ref is None:
        return (rel, top, "REF_FAIL", "timeout")
    if ref.returncode < 0:
        return (rel, top, "REF_FAIL", f"signal {-ref.returncode}")

    _gold_save(gdir, wk, top, ref, ref_secs, ref_build_secs)
    return _trs_side(rel, top, wk, testdir, bir, ref, ref_secs,
                     ref_build_secs)


def _trsonly_test(rel, top, wk, testdir, src, trsonly):
    """One trs-only golden-compared design (<top>.trsonly.expected).
    The sidecar reads:
         flags=<bsc flags> extra bsc flags admitting the design
                           (e.g. -sched-dynamic), all bsc calls
         refuse=<tag>      classic Bluesim link must fail with this tag
         bind=NAME=value   trs binding, repeated (recorded per design)
         engine=aot|interp expected engine, ASSERTED (byte parity is
                           engine-blind by the oracle contract)
         why=<substring>   engine=interp only: the traced decline reason
       Gates: classic refusal tag; `trs link` + the ARTIFACT and a
       plain `trs run` both byte-match <top>.out.expected; the engine
       expectation holds.  Any miss classifies DIFF so the census's
       0-DIFF gate stays the single tripwire."""
    import time as _time
    refuse, binds, engine, why, flags = "", [], "aot", "", []
    for line in open(trsonly, errors="replace"):
        line = line.strip()
        if line.startswith("refuse="):
            refuse = line[len("refuse="):]
        elif line.startswith("bind="):
            binds.append("+" + line[len("bind="):])
        elif line.startswith("engine="):
            engine = line[len("engine="):]
        elif line.startswith("why="):
            why = line[len("why="):]
        elif line.startswith("flags="):
            flags += line[len("flags="):].split()
    golden_p = os.path.join(testdir, top + ".out.expected")
    try:
        golden = open(golden_p, errors="replace").read()
    except OSError:
        return (rel, top, "DIFF", "trsonly: no golden " + golden_p)

    for f in os.listdir(testdir):
        if f.endswith((".bsv", ".bs")):
            try:
                shutil.copy(os.path.join(testdir, f), wk)
            except OSError:
                pass
    common = ["-bdir", wk, "-info-dir", wk, "-simdir", wk, "-p", wk + ":+"]
    r = run([BSC, "-sim"] + flags + ["-u", "-g", top] + common + [src],
            cwd=wk, timeout=180)
    if r is None or r.returncode != 0:
        msg = "" if r is None else (r.stderr + r.stdout)
        return (rel, top, "COMPILE_FAIL",
                "compile timeout" if r is None else first_error(msg))

    # the classic refusal is part of the contract: these designs are
    # trs-only, and classic Bluesim's error must not drift
    r = run([BSC, "-sim"] + flags + ["-bir", "-e", top, "-o", "classic.exe"]
            + common, cwd=wk, timeout=420)
    if r is None:
        return (rel, top, "LINK_FAIL", "classic-refusal probe timeout")
    if r.returncode == 0:
        return (rel, top, "DIFF", "classic Bluesim link unexpectedly "
                "succeeded (expected " + refuse + ")")
    if refuse and refuse not in (r.stderr + r.stdout):
        return (rel, top, "DIFF", "classic refusal drifted (expected " +
                refuse + "): " + first_error(r.stderr + r.stdout))

    # export the .bir through bsc's own -trs link; for designs with
    # required bindings that link step fails (loudly) AFTER exporting,
    # so only the .bir's existence gates here
    env = dict(ENV, TRS=TRS)
    run([BSC, "-sim"] + flags + ["-bir", "-trs", "-e", top, "-o", "trs.exe"]
        + common, cwd=wk, timeout=420, env=env)
    bir = os.path.join(wk, top + ".bir")
    if not os.path.exists(bir):
        return (rel, top, "EXPORT_FAIL", "no .bir produced")

    cexe = os.path.join(wk, top + ".aot.cexe")
    link_env = dict(ENV)
    link_env["TRS_JIT_TRACE"] = "1"
    tl0 = _time.monotonic()
    lk = run([TRS, "link", bir] + binds + ["-o", cexe], cwd=wk, timeout=300,
             env=link_env)
    trs_link_secs = _time.monotonic() - tl0
    if lk is None or lk.returncode != 0:
        msg = "" if lk is None else (lk.stderr + lk.stdout)
        return (rel, top, "AOT_LINK_FAIL",
                "timeout" if lk is None else first_error(msg))
    compiled = os.path.exists(cexe + ".so")
    got_engine = "aot" if compiled else "interp"
    got_why = link_fallback_reason(lk.stderr) if not compiled else ""
    if got_engine != engine:
        return (rel, top, "DIFF",
                f"engine drifted: expected {engine}, got {got_engine}" +
                (f" why={got_why}" if got_why else ""))
    if engine == "interp" and why and why != got_why:
        return (rel, top, "DIFF",
                f"decline reason drifted: expected why={why}, got "
                f"why={got_why}")

    env = dict(ENV)
    env["PATH"] = os.path.dirname(TRS) + os.pathsep + env.get("PATH", "")
    tr0 = _time.monotonic()
    art = run([cexe, "-m", MAX_CYCLES], cwd=wk, timeout=TIMEOUT, env=env)
    trs_run_secs = _time.monotonic() - tr0
    if art is None:
        return (rel, top, "TIMEOUT", f"limit {TIMEOUT:.0f}s (artifact)")
    if art.stdout != golden:
        return (rel, top, "DIFF",
                "artifact: " + diff_summary(golden, art.stdout))
    # the per-run binding path must agree byte-for-byte too
    inp = run([TRS, "run", bir] + binds + ["-m", MAX_CYCLES], cwd=wk,
              timeout=TIMEOUT)
    if inp is None:
        return (rel, top, "TIMEOUT", f"limit {TIMEOUT:.0f}s (trs run)")
    if inp.stdout != golden:
        return (rel, top, "DIFF", "run: " + diff_summary(golden, inp.stdout))
    # ref_* stay 0.0: below the fence floors by construction, so
    # trs-only designs never enter the perf fence
    timing = (f"t ref_build=0.00 ref_run=0.000"
              f" trs_link={trs_link_secs:.2f} trs_run={trs_run_secs:.3f}"
              f" engine={got_engine}" +
              (f" why={got_why}" if got_why else ""))
    return (rel, top, "PASS", timing)


def _trs_side(rel, top, wk, testdir, bir, ref, ref_secs, ref_build_secs):
    """The trs half of one_test: link, run, byte-compare against the
    reference result (live or golden-replayed)."""
    import time as _time
    is_long = any(f.endswith(".exp.golden") for f in os.listdir(testdir))
    limit = max(TIMEOUT_FLOOR, TIMEOUT_FACTOR * ref_secs) if is_long else TIMEOUT
    # trs may compile INSIDE the timed window (sync JIT); the
    # reference compiled off the clock in the bsc call above.  Floor
    # the limit at the reference's own build+run wall so no mode is
    # asked to beat a budget Bluesim itself did not meet.
    limit = max(limit, ref_build_secs + ref_secs)
    trs_link_secs = 0.0
    engine_note = ""
    if AOT:
        cexe = os.path.join(wk, top + ".aot.cexe")
        # engine-outcome telemetry: trace makes every plan gate and
        # trial-lower refusal name itself on stderr, so the sweep
        # records WHY a design runs interpreted, not just that it does
        link_env = dict(ENV)
        link_env["TRS_JIT_TRACE"] = "1"
        tl0 = _time.monotonic()
        lk = run([TRS, "link", bir, "-o", cexe], cwd=wk, timeout=300, env=link_env)
        trs_link_secs = _time.monotonic() - tl0
        if lk is None or lk.returncode != 0:
            msg = "" if lk is None else (lk.stderr + lk.stdout)
            return (rel, top, "AOT_LINK_FAIL",
                    "timeout" if lk is None else first_error(msg))
        # the sibling .so is the durable record of a compiled link —
        # it works for both artifact forms (the symlink-to-runner form
        # is a binary, so reading the artifact as text no longer does)
        compiled = os.path.exists(cexe + ".so")
        engine_note = " engine=aot" if compiled else (
            " engine=interp why=" + link_fallback_reason(lk.stderr))
        env = dict(ENV)
        env["PATH"] = os.path.dirname(TRS) + os.pathsep + env.get("PATH", "")
        tr0 = _time.monotonic()
        inp = run([cexe, "-m", MAX_CYCLES], cwd=wk, timeout=limit, env=env)
        trs_run_secs = _time.monotonic() - tr0
    else:
        tr0 = _time.monotonic()
        inp = run([TRS, "run", bir, "-m", MAX_CYCLES], cwd=wk, timeout=limit)
        trs_run_secs = _time.monotonic() - tr0
    if inp is None:
        return (rel, top, "TIMEOUT", f"limit {limit:.0f}s (ref {ref_secs:.2f}s)")
    if inp.returncode != 0 and "panicked" in inp.stderr:
        return (rel, top, "INTERP_PANIC", panic_reason(inp.stderr))
    # infra-failure heuristic: only when the reference did NOT fail the
    # same way — a design that legitimately exits nonzero (e.g. $fatal)
    # while printing "Error" must reach the output diff, not hide in an
    # infra bucket
    if ("error" in inp.stderr.lower() and inp.returncode != 0
            and inp.returncode != ref.returncode):
        return (rel, top, "DECODE_FAIL", first_error(inp.stderr))

    # stderr is a sim output channel too ($fdisplay to stderr): diff it
    # like stdout, with trs' own infra notes (all "trs"-prefixed lines)
    # filtered out of the trs side first
    trs_notes = [l for l in inp.stderr.splitlines(True) if l.startswith("trs")]
    inp_err = "".join(
        l for l in inp.stderr.splitlines(True) if not l.startswith("trs"))
    if engine_note == " engine=aot" and any(
            "compiling in-process instead" in l for l in trs_notes):
        # the wrapper had --code but the artifact refused at load: the
        # run was NOT aot — never credit aot timings to a fallback
        engine_note = " engine=interp why=stale_artifact_load_fallback"
    if ref.stdout == inp.stdout and ref.stderr != inp_err:
        return (rel, top, "DIFF",
                "stderr: " + diff_summary(ref.stderr, inp_err))
    if ref.stdout == inp.stdout:
        if ref.returncode != inp.returncode:
            return (rel, top, "DIFF",
                    f"exit codes differ: ref={ref.returncode} int={inp.returncode}")
        # timing columns (5th field): the corpus slowdown table —
        # ratios rank the next optimization targets.  ref_build is the
        # bsc -sim link phase (C++ codegen + cc), the fair comparand
        # for trs_link.
        timing = (f"t ref_build={ref_build_secs:.2f} ref_run={ref_secs:.3f}"
                  f" trs_link={trs_link_secs:.2f} trs_run={trs_run_secs:.3f}"
                  f"{engine_note}")
        return (rel, top, "PASS", timing)
    return (rel, top, "DIFF", diff_summary(ref.stdout, inp.stdout))


def link_fallback_reason(stderr):
    """Fallback reason from a traced `trs link`: prefer the specific
    `trs jit: off (...)` line (the last one wins — trial lower may
    follow a plan gate), else the CLI's compiled-mode-unavailable
    note.  Whitespace collapses to _ so the note stays one token."""
    reason = ""
    for line in stderr.splitlines():
        l = line.strip()
        if l.startswith("trs jit: off (") and l.endswith(")"):
            reason = l[len("trs jit: off ("):-1]
        elif "compiled mode unavailable (" in l and not reason:
            reason = l.split("compiled mode unavailable (", 1)[1]
            reason = reason.split(");", 1)[0]
    return re.sub(r"\s+", "_", reason)[:100] or "unknown"


def first_error(msg):
    for line in msg.splitlines():
        l = line.strip()
        if l.startswith("Error") or "internal error" in l.lower() or "SimExportIR" in l:
            return l[:160]
    for line in msg.splitlines():
        if line.strip():
            return line.strip()[:160]
    return "unknown"


def panic_reason(stderr):
    m = re.search(r"panicked at [^\n]*:\n([^\n]*)", stderr)
    if m:
        return m.group(1).strip()[:160]
    for line in stderr.splitlines():
        if "panicked" in line:
            return line.strip()[:160]
    return "panic"


def diff_summary(a, b):
    la, lb = a.splitlines(), b.splitlines()
    for i, (x, y) in enumerate(zip(la, lb)):
        if x != y:
            return f"line {i+1}: ref={x[:60]!r} int={y[:60]!r}"
    return f"lengths differ: ref={len(la)} int={len(lb)} lines"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--limit", type=int, default=0, help="max tests (0 = all)")
    ap.add_argument("--jobs", type=int, default=8)
    ap.add_argument("--filter", default="", help="substring filter on test dir")
    ap.add_argument("--out", default="diffsweep-results.json")
    ap.add_argument("--costs", default="",
                    help="prior sweep --out JSON for LPT scheduling "
                    "(default tools/sweep-costs.json if present)")
    ap.add_argument("--fence-baseline", action="store_true",
                    help="write tools/perf-fence.json from this run's "
                    "timings instead of checking against it")
    ap.add_argument("--aot", action="store_true",
                    help="trs link + run the artifact script instead of trs run")
    ap.add_argument("--timeout-floor", type=float, default=None,
                    help="minimum trs timeout for enable-gated long "
                    "tests, seconds (default 5)")
    ap.add_argument("--timeout-factor", type=float, default=None,
                    help="long-test trs timeout as a multiple of the "
                    "reference's wall time (default 5)")
    ap.add_argument(
        "--golden", default="",
        help="golden-output cache dir: reference results are cached by "
        "(bsc, design inputs) and replayed on hit, so the sweep pays "
        "only the trs side")
    ap.add_argument(
        "--trs",
        default="",
        help="trs binary to sweep (default: the repo release build); "
        "lets a scratch build be tested without touching target/release",
    )
    args = ap.parse_args()
    if args.trs:
        global TRS
        TRS = os.path.abspath(args.trs)
        # workers re-import this module (spawn/forkserver); hand the
        # override down via the environment
        os.environ["DIFFSWEEP_TRS"] = TRS
    # bind the globals too: fork-start pools never re-import, so the
    # env-only handoff silently no-ops the flags there
    if args.timeout_floor is not None:
        global TIMEOUT_FLOOR
        TIMEOUT_FLOOR = args.timeout_floor
        os.environ["DIFFSWEEP_TIMEOUT_FLOOR"] = str(args.timeout_floor)
    if args.timeout_factor is not None:
        global TIMEOUT_FACTOR
        TIMEOUT_FACTOR = args.timeout_factor
        os.environ["DIFFSWEEP_TIMEOUT_FACTOR"] = str(args.timeout_factor)
    if args.aot:
        global AOT
        AOT = True
        os.environ["DIFFSWEEP_AOT"] = "1"
    if args.golden:
        global GOLDEN
        GOLDEN = os.path.abspath(args.golden)
        os.environ["DIFFSWEEP_GOLDEN"] = GOLDEN
        os.makedirs(GOLDEN, exist_ok=True)
    print(f"trs binary: {TRS}", flush=True)

    jobs = []
    workroot = os.path.join(os.path.dirname(args.out) or ".", "diffsweep-work")
    for dirpath, _dirs, files in os.walk(os.path.join(REPO, "testsuite")):
        for f in files:
            m = re.match(r"((?:sys|mk)\w+)\.out\.expected$", f)
            if m and (not args.filter or args.filter in dirpath):
                jobs.append((dirpath, m.group(1), workroot))
    jobs.sort()
    if args.limit:
        jobs = jobs[: args.limit]

    # LPT scheduling: the tail of a sweep is one straggler (FloatTest's
    # multi-minute reference build) behind idle workers.  Sort the
    # queue by recorded per-design cost DESCENDING using a prior
    # sweep's --out JSON (--costs, or the file next to the fence);
    # unknown designs keep their alphabetical order after the known
    # ones (they are overwhelmingly small).  Same jobs count, same
    # timing fidelity — only the schedule changes.
    costs_path = args.costs or os.path.join(
        os.path.dirname(os.path.abspath(__file__)), "sweep-costs.json")
    try:
        prior = json.load(open(costs_path))
        cost = {}
        for rel, top, status, note in prior:
            if status == "PASS" and note.startswith("t "):
                total = 0.0
                for kv in note[2:].split():
                    k, _, v = kv.partition("=")
                    if k not in ("ref_build", "ref_run", "trs_link", "trs_run"):
                        continue  # engine=/why= (even numeric-looking)
                    try:
                        total += float(v)
                    except ValueError:
                        pass
                cost[(rel, top)] = total
        def jobkey(j):
            rel = os.path.relpath(j[0], REPO)
            return -cost.get((rel, j[1]), 0.0)
        jobs.sort(key=jobkey)
        if cost:
            print(f"LPT schedule from {costs_path} ({len(cost)} costs)",
                  flush=True)
    except (OSError, ValueError):
        pass

    print(f"sweeping {len(jobs)} tests with {args.jobs} jobs", flush=True)
    with multiprocessing.Pool(args.jobs) as pool:
        results = []
        for i, res in enumerate(pool.imap_unordered(one_test, jobs)):
            results.append(res)
            if (i + 1) % 50 == 0:
                print(f"  {i+1}/{len(jobs)} done", flush=True)

    by_class = collections.Counter(r[2] for r in results)
    print("\n=== classes ===")
    for cls, n in by_class.most_common():
        print(f"  {cls:14} {n}")

    # An --aot sweep run with a jit-less trs binary passes byte parity
    # on every design while measuring nothing about the compiled
    # engine (the top-level `make install` builds trs without the jit
    # feature unless LLVM_SYS_181_PREFIX is set).  That is never a
    # legitimate census: fail loudly instead of printing an all-interp
    # engine column.
    if AOT:
        nojit = sum(1 for r in results
                    if "built_without_JIT" in (r[3] or ""))
        if nojit:
            print(f"\nFATAL: {nojit} designs ran interpreted because "
                  f"this trs was built without the `jit` feature; "
                  f"rebuild with `cargo build --release --features jit` "
                  f"(or set LLVM_SYS_181_PREFIX for make) and re-sweep.")
            sys.exit(2)

    print("\n=== top interpreter/export work items ===")
    reasons = collections.Counter(
        r[3] for r in results if r[2] in ("INTERP_PANIC", "EXPORT_FAIL")
    )
    for reason, n in reasons.most_common(20):
        print(f"  {n:4}  {reason}")

    print("\n=== diffs (first 10) ===")
    for r in [x for x in results if x[2] == "DIFF"][:10]:
        print(f"  {r[0]}/{r[1]}: {r[3]}")

    with open(args.out, "w") as f:
        json.dump([list(r) for r in results], f, indent=1)
    print(f"\nfull results: {args.out}")

    # ---- performance fence ----
    # Ratios vs the reference measured in the SAME run self-normalize
    # against machine load; the baseline pins them per design (above a
    # wall-clock floor).  Regressions print PERF_REGRESS lines — treat
    # them with the same discipline as DIFFs.
    fence_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                              "perf-fence.json")
    timings = {}
    for (rel, top, status, note) in results:
        if status == "PASS" and note.startswith("t "):
            t = {}
            for kv in note[2:].split():
                k, _, v = kv.partition("=")
                try:
                    t[k] = float(v)
                except ValueError:
                    pass  # non-timing columns (engine=, why=)
            timings[f"{rel}:{top}"] = t
    def _ratios(t):
        out = {}
        if t["ref_run"] >= 0.10:
            out["run"] = t["trs_run"] / t["ref_run"]
        if t["ref_build"] >= 2.0:
            out["link"] = t["trs_link"] / t["ref_build"]
        return out
    if args.fence_baseline:
        base = {k: r for k, t in sorted(timings.items()) if (r := _ratios(t))}
        with open(fence_path, "w") as f:
            json.dump(base, f, indent=1, sort_keys=True)
        print(f"perf fence baseline written: {fence_path} ({len(base)} designs)")
    elif os.path.exists(fence_path) and timings:
        base = json.load(open(fence_path))
        perf_regress = 0
        for k, expect in sorted(base.items()):
            got = _ratios(timings[k]) if k in timings else {}
            for dim, exp in expect.items():
                if dim in got and got[dim] > exp * 1.3 + 0.05:
                    print(f"PERF_REGRESS {k} {dim}: {got[dim]:.2f} "
                          f"(baseline {exp:.2f})")
                    perf_regress += 1
        if perf_regress:
            print(f"PERF_REGRESS total: {perf_regress}")
        else:
            print(f"perf fence: clean ({len(base)} designs)")


if __name__ == "__main__":
    main()
