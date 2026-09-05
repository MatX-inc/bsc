#!/usr/bin/env python3
"""Run P0a evidence checks; never report the unimplemented P0 gate as passed."""

import argparse
import json
from pathlib import Path
import platform
import shutil
import subprocess
import sys


SPIKE = Path(__file__).resolve().parent
REPO = SPIKE.parents[3]
COMPILER_PIN = "a9462e0e28102f07a89b3f3c478b33c806d62a3e"
BVI_PIN = "d4a72767c63e6dbdf569cac4c61aeb8afc597c04"
HANDOFF_PIN = "054ba3f6400bfed18c20c1552981212e8cc956bd"
SOURCE_PATHS = (
    "src/comp/ASchedule.hs",
    "src/comp/AScheduleInfo.hs",
    "src/comp/SimPackage.hs",
    "src/comp/SimExpand.hs",
    "src/comp/SimMakeCBlocks.hs",
    "src/trs/trs-bir/SimExportIR.hs",
    "src/trs/crates/trs-ir/src/schedule.rs",
    "src/trs/crates/trs-ir/src/merge.rs",
)
COMPILER_SUBTREES = ("src/comp", "src/bluesim", "src/trs/crates", "src/trs/trs-bir")
REQUIRED_TOOLS = (
    "bsc", "bluetcl", "iverilog", "vvp", "verilator",
    "rustc", "cargo", "fst2vcd",
)
BUILD_TOOLS = ("ghc", "cabal", "make", "g++")
REMAINING_GATES = (
    "P0a protocol and semantic-inventory design-owner review",
    "actual BSC module-local extraction and Verilog dynamic-parity runs",
    "real mixed BVI/Verilator and timed-event witnesses",
    "reviewed compositional safety/progress argument and closed inventory",
    "P0b structural and scaling evidence from the actual hierarchical engine",
)


def command(args):
    return subprocess.run(
        args, cwd=REPO, text=True, capture_output=True, timeout=20,
        check=False,
    )


def source_check():
    """Check reference identity only, not the correctness of source analysis."""
    checks = []
    for path in SOURCE_PATHS:
        expected = command(["git", "rev-parse", f"{COMPILER_PIN}:{path}"])
        actual = command(["git", "hash-object", path])
        matches = (
            expected.returncode == actual.returncode == 0
            and expected.stdout.strip() == actual.stdout.strip()
        )
        checks.append({
            "path": path,
            "expected_blob": expected.stdout.strip() if expected.returncode == 0 else None,
            "actual_blob": actual.stdout.strip() if actual.returncode == 0 else None,
            "matches_pin": matches,
            "error": (expected.stderr + actual.stderr).strip() or None,
        })
    bvi = command(["git", "cat-file", "-e", f"{BVI_PIN}:src/trs/crates/trs-ir/src/bvi.rs"])
    unchanged = command(["git", "diff", "--quiet", COMPILER_PIN, "--", *COMPILER_SUBTREES])
    ok = all(c["matches_pin"] for c in checks) and bvi.returncode == unchanged.returncode == 0
    return {
        "status": "source_pins_verified" if ok else "source_check_failed",
        "scope": "Source identity only; no compiler or semantic execution.",
        "compiler_pin": COMPILER_PIN,
        "bvi_pin": BVI_PIN,
        "handoff_pin": HANDOFF_PIN,
        "checks": checks,
        "bvi_reference_available": bvi.returncode == 0,
        "bvi_reference_error": bvi.stderr.strip() or None,
        "compiler_subtrees_match_pin": unchanged.returncode == 0,
        "compiler_subtree_error": unchanged.stderr.strip() or None,
    }, 0 if ok else 3


def model_check():
    result = command([sys.executable, str(SPIKE / "experiments/local_protocol.py")])
    try:
        evidence = json.loads(result.stdout)
        if not isinstance(evidence, dict):
            raise ValueError("experiment report must be a JSON object")
        markers = {
            "model_checks_passed": True,
            "p0_pass": False,
            "compiler_derived": False,
            "rtl_oracle_compared": False,
        }
        if any(evidence.get(key) is not value for key, value in markers.items()):
            raise ValueError("experiment must explicitly identify successful P0a-only, non-compiler, non-RTL evidence")
    except ValueError as exc:
        return {
            "status": "model_check_failed",
            "error": str(exc),
            "stdout": result.stdout,
            "stderr": result.stderr,
            "returncode": result.returncode,
        }, 3
    ok = result.returncode == 0
    return {
        "status": "model_checks_passed" if ok else "model_check_failed",
        "scope": "P0a hand-authored model only; not BSC/RTL parity or a P0 pass.",
        "returncode": result.returncode,
        "stderr": result.stderr,
        "evidence": evidence,
    }, 0 if ok else 3


def environment_check():
    tools = {name: shutil.which(name) for name in REQUIRED_TOOLS + BUILD_TOOLS}
    missing = [name for name in REQUIRED_TOOLS if tools[name] is None]
    return {
        "status": "required_tools_available" if not missing else "blocked_missing_tools",
        "python": sys.version.split()[0],
        "platform": platform.platform(),
        "tools": tools,
        "missing_required_tools": missing,
        "notes": [
            "Tool presence does not validate versions or produce semantic evidence.",
            "GHC/Cabal are build aids; a verified pinned BSC binary can replace a local compiler build.",
            "No tool installation or compiler/simulator execution is performed by this check.",
        ],
    }, 2 if missing else 0


def run(mode):
    modes = {"source": source_check, "model": model_check, "environment": environment_check}
    selected = modes if mode == "all" else {mode: modes[mode]}
    reports = {}
    codes = []
    for name, check in selected.items():
        try:
            report, code = check()
        except (OSError, subprocess.TimeoutExpired) as exc:
            report, code = {"status": "check_failed", "error": str(exc)}, 3
        reports[name] = report
        codes.append(code)
    exit_code = max(codes)
    if mode == "all":
        exit_code = max(exit_code, 2)
    return {
        "mode": mode,
        "p0_status": "blocked/inconclusive",
        "remaining_gates": list(REMAINING_GATES),
        "checks": reports,
        "exit_code": exit_code,
    }, exit_code


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=("source", "model", "environment", "all"), nargs="?", default="all")
    args = parser.parse_args()
    report, code = run(args.mode)
    print(json.dumps(report, indent=2, sort_keys=True))
    return code


if __name__ == "__main__":
    sys.exit(main())
