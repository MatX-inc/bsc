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
# under the timeout; fall back to debug if it hasn't been built
TRS = os.path.join(REPO, "src", "trs", "target", "release", "trs")
if not os.path.exists(TRS):
    TRS = os.path.join(REPO, "src", "trs", "target", "debug", "trs")
ENV = dict(os.environ, PATH=os.path.join(REPO, "inst", "bin") + ":" + os.environ["PATH"])

MAX_CYCLES = "4000"
TIMEOUT = 25


def find_source(testdir, top):
    def scan(name):
        cand = os.path.join(testdir, name + ".bsv")
        if os.path.exists(cand):
            return cand
        pats = [
            re.compile(r"module\s+(?:\[[^\]]*\]\s*)?" + re.escape(name) + r"\b"),
            re.compile(r"`define\s+\w+\s+" + re.escape(name) + r"\b"),
        ]
        for f in sorted(os.listdir(testdir)):
            if f.endswith(".bsv"):
                try:
                    text = open(os.path.join(testdir, f), errors="replace").read()
                except OSError:
                    continue
                if any(p.search(text) for p in pats):
                    return os.path.join(testdir, f)
        # filename convention: sysFoo defined in Foo.bsv
        if name.startswith("sys"):
            cand = os.path.join(testdir, name[3:] + ".bsv")
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


def run(cmd, cwd, timeout=TIMEOUT):
    try:
        return subprocess.run(
            cmd, cwd=cwd, env=ENV, timeout=timeout,
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
    r = run([BSC, "-sim", "-u", "-g", top] + common + [src], cwd=wk, timeout=180)
    if r is None or r.returncode != 0:
        msg = "" if r is None else (r.stderr + r.stdout)
        if r is None:
            return (rel, top, "COMPILE_FAIL", "compile timeout")
        if "(G0097)" in msg or "(G0098)" in msg:
            # Inout is not supported by Bluesim at all
            return (rel, top, "NOT_SUPPORTED", first_error(msg))
        return (rel, top, "COMPILE_FAIL", first_error(msg))

    r = run([BSC, "-sim", "-bir", "-e", top, "-o", "sim.exe"] + common, cwd=wk,
            timeout=180)
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

    ref = run(["./sim.exe", "-m", MAX_CYCLES], cwd=wk)
    if ref is None:
        return (rel, top, "REF_FAIL", "timeout")
    if ref.returncode < 0:
        return (rel, top, "REF_FAIL", f"signal {-ref.returncode}")

    inp = run([TRS, "run", bir, "-m", MAX_CYCLES], cwd=wk)
    if inp is None:
        return (rel, top, "INTERP_PANIC", "timeout")
    if inp.returncode != 0 and "panicked" in inp.stderr:
        return (rel, top, "INTERP_PANIC", panic_reason(inp.stderr))
    if "error" in inp.stderr.lower() and inp.returncode != 0:
        return (rel, top, "DECODE_FAIL", first_error(inp.stderr))

    if ref.stdout == inp.stdout:
        if ref.returncode != inp.returncode:
            return (rel, top, "DIFF",
                    f"exit codes differ: ref={ref.returncode} int={inp.returncode}")
        return (rel, top, "PASS", "")
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
    args = ap.parse_args()

    jobs = []
    workroot = os.path.join(os.path.dirname(args.out) or ".", "diffsweep-work")
    for dirpath, _dirs, files in os.walk(os.path.join(REPO, "testsuite")):
        for f in files:
            m = re.match(r"(sys\w+)\.out\.expected$", f)
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


if __name__ == "__main__":
    main()
