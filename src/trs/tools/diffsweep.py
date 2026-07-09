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
TIMEOUT = 60          # reference runs, compiles, and normal trs runs
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

    common = ["-bdir", wk, "-info-dir", wk, "-simdir", wk,
              "-p", wk + ":" + testdir + ":+"]

    # BDPI designs need the user's C files at link (the .exp recipes pass
    # them; .c.keep is the testsuite convention for inactive copies)
    cfiles = []
    has_bdpi = any(
        'import "BDPI"' in open(os.path.join(testdir, f), errors="replace").read()
        for f in os.listdir(testdir) if f.endswith(".bsv")
    )
    if has_bdpi:
        for f in os.listdir(testdir):
            if f.endswith(".c"):
                shutil.copy(os.path.join(testdir, f), wk)
                cfiles.append(f)
            elif f.endswith(".c.keep"):
                shutil.copy(os.path.join(testdir, f), os.path.join(wk, f[:-5]))
                cfiles.append(f[:-5])
    r = run([BSC, "-sim", "-u", "-g", top] + common + [src], cwd=wk, timeout=180)
    if r is None or r.returncode != 0:
        msg = "" if r is None else (r.stderr + r.stdout)
        if r is None:
            return (rel, top, "COMPILE_FAIL", "compile timeout")
        if "(G0097)" in msg or "(G0098)" in msg:
            # Inout is not supported by Bluesim at all
            return (rel, top, "NOT_SUPPORTED", first_error(msg))
        return (rel, top, "COMPILE_FAIL", first_error(msg))

    import time as _time
    tb0 = _time.monotonic()
    r = run([BSC, "-sim", "-bir", "-e", top, "-o", "sim.exe"] + common + cfiles,
            cwd=wk, timeout=180)
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

    is_long = any(f.endswith(".exp.golden") for f in os.listdir(testdir))
    limit = max(TIMEOUT_FLOOR, TIMEOUT_FACTOR * ref_secs) if is_long else TIMEOUT
    # trs may compile INSIDE the timed window (sync JIT); the
    # reference compiled off the clock in the bsc call above.  Floor
    # the limit at the reference's own build+run wall so no mode is
    # asked to beat a budget Bluesim itself did not meet.
    limit = max(limit, ref_build_secs + ref_secs)
    b3_link_secs = 0.0
    if AOT:
        cexe = os.path.join(wk, top + ".aot.cexe")
        tl0 = _time.monotonic()
        lk = run([TRS, "link", bir, "-o", cexe], cwd=wk, timeout=300)
        b3_link_secs = _time.monotonic() - tl0
        if lk is None or lk.returncode != 0:
            msg = "" if lk is None else (lk.stderr + lk.stdout)
            return (rel, top, "AOT_LINK_FAIL",
                    "timeout" if lk is None else first_error(msg))
        env = dict(ENV)
        env["PATH"] = os.path.dirname(TRS) + os.pathsep + env.get("PATH", "")
        tr0 = _time.monotonic()
        inp = run([cexe, "-m", MAX_CYCLES], cwd=wk, timeout=limit, env=env)
        b3_run_secs = _time.monotonic() - tr0
    else:
        tr0 = _time.monotonic()
        inp = run([TRS, "run", bir, "-m", MAX_CYCLES], cwd=wk, timeout=limit)
        b3_run_secs = _time.monotonic() - tr0
    if inp is None:
        return (rel, top, "TIMEOUT", f"limit {limit:.0f}s (ref {ref_secs:.2f}s)")
    if inp.returncode != 0 and "panicked" in inp.stderr:
        return (rel, top, "INTERP_PANIC", panic_reason(inp.stderr))
    if "error" in inp.stderr.lower() and inp.returncode != 0:
        return (rel, top, "DECODE_FAIL", first_error(inp.stderr))

    if ref.stdout == inp.stdout:
        if ref.returncode != inp.returncode:
            return (rel, top, "DIFF",
                    f"exit codes differ: ref={ref.returncode} int={inp.returncode}")
        # timing columns (5th field): the corpus slowdown table —
        # ratios rank the next optimization targets.  ref_build is the
        # bsc -sim link phase (C++ codegen + cc), the fair comparand
        # for b3_link.
        timing = (f"t ref_build={ref_build_secs:.2f} ref_run={ref_secs:.3f}"
                  f" b3_link={b3_link_secs:.2f} b3_run={b3_run_secs:.3f}")
        return (rel, top, "PASS", timing)
    return (rel, top, "DIFF", diff_summary(ref.stdout, inp.stdout))


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
    if args.timeout_floor is not None:
        os.environ["DIFFSWEEP_TIMEOUT_FLOOR"] = str(args.timeout_floor)
    if args.timeout_factor is not None:
        os.environ["DIFFSWEEP_TIMEOUT_FACTOR"] = str(args.timeout_factor)
    if args.aot:
        global AOT
        AOT = True
        os.environ["DIFFSWEEP_AOT"] = "1"
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
            fv = dict(kv.split("=") for kv in note[2:].split())
            timings[f"{rel}:{top}"] = {k: float(v) for k, v in fv.items()}
    def _ratios(t):
        out = {}
        if t["ref_run"] >= 0.10:
            out["run"] = t["b3_run"] / t["ref_run"]
        if t["ref_build"] >= 2.0:
            out["link"] = t["b3_link"] / t["ref_build"]
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
