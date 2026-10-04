#!/usr/bin/env python3
"""Capture clean DejaGNU runs; build install-src from the root first.

This captures evidence, not a parity verdict. Use bsc-test-plan import-sum and
compare afterwards. The original summary paths are preserved below each run.
"""

import argparse
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import time

from capture_lifecycle import CaptureInterrupted, CaptureSession


ROOT = Path(__file__).resolve().parents[3]
SUITE = ROOT / "testsuite"
CATEGORIES = ("PASS", "XFAIL", "FAIL", "XPASS", "KFAIL", "KPASS",
              "UNRESOLVED", "UNTESTED", "UNSUPPORTED", "ERROR", "WARNING")
INTERNAL_TOOLS = ("dumpbo", "dumpba", "vcdcheck", "bsc2bsv")


def positive_integer(value):
    try:
        number = int(value)
    except ValueError:
        raise argparse.ArgumentTypeError("must be a positive integer") from None
    if number <= 0:
        raise argparse.ArgumentTypeError("must be a positive integer")
    return number


def installation_digest():
    digest = hashlib.sha256()
    for path in sorted((ROOT / "inst").rglob("*")):
        name = str(path.relative_to(ROOT / "inst")).encode()
        digest.update(name + b"\0")
        digest.update(str(path.lstat().st_mode).encode() + b"\0")
        if path.is_symlink():
            digest.update(b"link\0" + os.readlink(path).encode())
        elif path.is_file():
            digest.update(b"file\0")
            with path.open("rb") as stream:
                for chunk in iter(lambda: stream.read(1024 * 1024), b""):
                    digest.update(chunk)
        digest.update(b"\0")
    return digest.hexdigest()


def testsuite_source_digest():
    """Hash tracked test inputs, including local edits but excluding this planner."""
    tracked = subprocess.check_output(
        ["git", "ls-files", "-z", "--", "testsuite"], cwd=ROOT).split(b"\0")
    inputs = sorted(path for path in tracked if path
                    and not path.startswith(b"testsuite/planner/"))
    if not inputs:
        raise ValueError("no tracked testsuite inputs found")
    digest = hashlib.sha256()
    for name in inputs:
        path = ROOT / os.fsdecode(name)
        digest.update(name + b"\0")
        digest.update(str(path.lstat().st_mode).encode() + b"\0")
        if path.is_symlink():
            digest.update(b"link\0" + os.fsencode(os.readlink(path)))
        elif path.is_file():
            digest.update(b"file\0")
            with path.open("rb") as stream:
                for chunk in iter(lambda: stream.read(1024 * 1024), b""):
                    digest.update(chunk)
        else:
            raise ValueError(f"tracked test input is not a file or symlink: {path}")
        digest.update(b"\0")
    return digest.hexdigest()


def tally():
    counts = dict.fromkeys(CATEGORIES, 0)
    failures = set()
    for summary in SUITE.glob("bsc.*/**/testrun.sum"):
        try:
            contents = summary.read_text(errors="replace")
        except FileNotFoundError:
            # fullparallel cleans the preceding run before starting jobs.
            continue
        for line in contents.splitlines():
            category = line.partition(":")[0]
            if category in counts:
                counts[category] += 1
            if category in {"FAIL", "XPASS", "KPASS", "UNRESOLVED", "ERROR"}:
                failures.add(str(summary.relative_to(SUITE)))
    return counts, failures


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path,
                        default=SUITE / ".stage1-validation" / "baselines")
    parser.add_argument("--internal-checks", type=int, choices=(0, 1), default=1,
                        help="enable internal artifact checks (default: 1)")
    parser.add_argument("--runs", type=positive_integer, default=2,
                        help="number of complete suite runs (default: 2)")
    args = parser.parse_args()
    try:
        with CaptureSession(ROOT) as session:
            return capture(args, parser, session)
    except CaptureInterrupted as error:
        print(str(error), file=sys.stderr)
        return 128 + error.signum
    except RuntimeError as error:
        parser.exit(1, f"capture: {error}\n")


def capture(args, parser, session):
    out = args.output_dir.resolve()
    if out.exists():
        parser.error(f"refusing to overwrite existing evidence: {out}")
    suite = SUITE.resolve()
    evidence = suite / ".stage1-validation"
    if (out == suite or suite in out.parents) and not (out == evidence or evidence in out.parents):
        parser.error("evidence must be under testsuite/.stage1-validation or outside testsuite")
    if not (ROOT / "inst/bin/bsc").is_file():
        parser.error("first run from repo root: make -j32 GHCJOBS=16 install-src")
    if args.internal_checks:
        # bsc_initialize requires these four tools when DO_INTERNAL_CHECKS=1.
        # The source installation's bin launchers execute bin/core/<name>.
        required = [ROOT / directory / name for name in INTERNAL_TOOLS
                    for directory in ("inst/bin", "inst/bin/core")]
        missing = [str(path.relative_to(ROOT)) for path in required
                   if not path.is_file() or not os.access(path, os.X_OK)]
        if missing:
            parser.error("internal checks require installed executable tools; "
                         "missing or non-executable: " + ", ".join(missing))
    out.mkdir(parents=True)
    environment = dict(os.environ)
    environment.update(
        PATH=str(ROOT / "inst/bin") + os.pathsep + environment.get("PATH", ""),
        TEST_SYSTEMC_INC="/usr/include",
        TEST_SYSTEMC_LIB="/usr/lib/x86_64-linux-gnu",
        TEST_SYSTEMC_CXXFLAGS="-std=c++17",
        VTEST="1", CTEST="1", SYSTEMCTEST="1",
        TEST_BSC_VERILOG_SIM="iverilog",
        DO_INTERNAL_CHECKS=str(args.internal_checks),
        TEST_BSC_OPTIONS="",
        TEST_RELEASE=str(ROOT / "inst"),
        TEST_BSC=str(ROOT / "inst/bin/bsc"),
        TEST_BLUETCL=str(ROOT / "inst/bin/bluetcl"),
        TEST_SHOWRULES=str(ROOT / "inst/bin/showrules"),
        TEST_BSC2BSV=str(ROOT / "inst/bin/bsc2bsv"),
        TEST_DUMPBO=str(ROOT / "inst/bin/dumpbo"),
        TEST_DUMPBA=str(ROOT / "inst/bin/dumpba"),
        TEST_VCDCHECK=str(ROOT / "inst/bin/vcdcheck"),
        TEST_BSDIR=str(ROOT / "inst/lib"),
        TEST_CONFIG=str(SUITE / "config"),
    )
    # Prevent inherited developer scoping from silently shrinking the baseline.
    for key in ("TESTDIRS", "SKIP_SLOWEST_TESTCASES", "RUN_TESTCASES_IN_ORDER_OF_TIME",
                "MAKEFLAGS", "MFLAGS", "GNUMAKEFLAGS", "MAKEFILES", "MAKEOVERRIDES",
                "RTFLAGS", "RUNTESTFLAGS",
                "RUNTESTENV", "PARALLEL_FLAGS", "INIT", "tool",
                "TEST_OSTYPE", "TEST_MACHTYPE"):
        environment.pop(key, None)
    for key, probe in (("TEST_OSTYPE", "ostype"), ("TEST_MACHTYPE", "machtype")):
        environment[key] = subprocess.check_output(
            [str(ROOT / "platform.sh"), probe], cwd=ROOT, text=True).strip()
    command = ["make", "-j128", "-C", "testsuite", "fullparallel"]
    identity = installation_digest()
    source_identity = testsuite_source_digest()
    # The random-vector generators use this absolute shebang, not PATH's Perl.
    perl = Path("/usr/bin/perl")
    metadata = {
        "base": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=ROOT,
                                        text=True).strip(),
        "suite_root": str(SUITE), "command": command, "runs": args.runs,
        "installation_sha256": identity,
        "testsuite_source_sha256": source_identity,
        "random_seed_environment": {
            "PERL_RAND_SEED": environment.get("PERL_RAND_SEED")},
        "perl_executable": {
            "path": str(perl), "resolved_path": str(perl.resolve(strict=True)),
            "sha256": hashlib.sha256(perl.read_bytes()).hexdigest(),
            "version": subprocess.check_output(
                [str(perl), "-e", "print $^V"], text=True).strip()},
        "configuration": {key: environment[key] for key in (
            "TEST_SYSTEMC_INC", "TEST_SYSTEMC_LIB", "TEST_SYSTEMC_CXXFLAGS",
            "VTEST", "CTEST", "SYSTEMCTEST", "TEST_BSC_VERILOG_SIM",
            "DO_INTERNAL_CHECKS", "TEST_BSC_OPTIONS", "TEST_OSTYPE", "TEST_MACHTYPE")},
        "inherited_tool_settings": {key: value for key, value in environment.items()
                                    if key in {"CC", "CXX", "CFLAGS", "CXXFLAGS",
                                               "CPPFLAGS", "LDFLAGS", "LINKER"}
                                    or key.startswith(("VCOMP_", "VRUN_"))},
    }
    (out / "configuration.json").write_text(json.dumps(metadata, indent=2) + "\n")
    for number in range(1, args.runs + 1):
        dest = out / f"run-{number}"
        dest.mkdir()
        started_at = datetime.now(timezone.utc).isoformat()
        started = time.monotonic()
        print(f"Starting clean DejaGNU run {number}/{args.runs}: "
              f"{' '.join(command)}", flush=True)
        with (dest / "fullparallel.log").open("w") as log:
            process = session.start(command, cwd=ROOT, env=environment,
                                    stdout=log, stderr=subprocess.STDOUT)
            next_report = started + 120
            seen_failures = set()
            while True:
                try:
                    process.wait(timeout=5)
                    break
                except subprocess.TimeoutExpired:
                    pass
                counts, failures = tally()
                new_failures = failures - seen_failures
                if new_failures:
                    print(f"Run {number} failing directories: " +
                          ", ".join(sorted(new_failures)), flush=True)
                    seen_failures.update(new_failures)
                if time.monotonic() >= next_report:
                    print(f"Run {number} running tally: {counts}", flush=True)
                    next_report = time.monotonic() + 120
        suite_wall_seconds = time.monotonic() - started
        finished_at = datetime.now(timezone.utc).isoformat()
        counts, failures = tally()
        summaries = list(SUITE.glob("bsc.*/**/testrun.sum"))
        for summary in summaries:
            target = dest / summary.relative_to(SUITE)
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(summary, target)
            detail = summary.with_suffix(".log")
            if detail.is_file():
                shutil.copy2(detail, target.with_suffix(".log"))
        timings = list(SUITE.glob("bsc.*/**/time.out"))
        for timing in timings:
            target = dest / timing.relative_to(SUITE)
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(timing, target)
        test_list = SUITE / "all_tests.mk"
        discovery_valid = False
        if test_list.is_file():
            shutil.copy2(test_list, dest / "all_tests.mk")
            entries = test_list.read_text().partition(":=")[2].split()
            scheduled = sorted(str(Path(entry)) for entry in entries
                               if Path(entry).parts[0].startswith("bsc."))
            discovered = sorted(str(path.relative_to(SUITE))
                                for path in SUITE.glob("bsc.*/**/*.exp"))
            discovery_valid = bool(discovered) and scheduled == discovered
            (dest / "expected-tests.txt").write_text(
                "".join(entry + "\n" for entry in discovered))
            (dest / "scheduled-tests.txt").write_text(
                "".join(entry + "\n" for entry in scheduled))
            (dest / "harness-inputs.txt").write_text("".join(
                entry + "\n" for entry in entries
                if not Path(entry).parts[0].startswith("bsc.")))
        source_after = testsuite_source_digest()
        result = {"exit_code": process.returncode, "counts": counts,
                  "seconds": time.monotonic() - started,
                  "suite_wall_seconds": suite_wall_seconds,
                  "started_at_utc": started_at, "finished_at_utc": finished_at,
                  "summary_files": len(summaries),
                  "timing_files": len(timings),
                  "scheduled_discovery_matches_source": discovery_valid,
                  "installation_unchanged": installation_digest() == identity,
                  "testsuite_source_sha256_after": source_after,
                  "testsuite_source_unchanged": source_after == source_identity}
        (dest / "result.json").write_text(json.dumps(result, indent=2) + "\n")
        print(f"Run {number} final tally: {result}", flush=True)
        if (process.returncode or failures or not summaries or not discovery_valid
                or not result["installation_unchanged"]
                or not result["testsuite_source_unchanged"]):
            print("Baseline is not clean; stopping capture.", file=sys.stderr)
            return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
