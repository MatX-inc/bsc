#!/usr/bin/env python3
"""Check plan/explain golden outputs and whole-selection rejection semantics."""

import argparse
import json
from pathlib import Path
import subprocess
import tempfile


FIXTURES = Path(__file__).parent / "fixtures" / "plan"
CASES = ("basic", "negative", "variants", "empty")


def check(condition, message):
    if not condition:
        raise AssertionError(message)


def selector(identifier):
    test = identifier["test"]
    site = ".".join(map(str, identifier["site"]))
    return f"v1:{len(test)}:{test}:{site}:{identifier['role']}"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("planner", type=Path)
    args = parser.parse_args()
    executable = args.planner.resolve(strict=True)
    with tempfile.TemporaryDirectory(prefix="bsc-plan-cli-") as temporary:
        work = Path(temporary)
        suite = work / "suite"
        group = suite / "bsc.plan"
        group.mkdir(parents=True)

        def run(*arguments):
            return subprocess.run([str(executable), *map(str, arguments)],
                                  cwd=work, capture_output=True, text=True, timeout=30)

        def plan(target, *extra):
            return run("plan", "--config", "golden", "--suite-root", suite,
                       *extra, target)

        plans = {}
        for case in CASES:
            source = group / f"{case}.exp"
            source.write_text((FIXTURES / f"{case}.tcl").read_text(encoding="utf-8"),
                              encoding="utf-8")
            result = plan(source)
            check(result.returncode == 0, f"{case} failed: {result.stderr}")
            actual = json.loads(result.stdout)
            expected = json.loads((FIXTURES / f"{case}.plan.json").read_text(encoding="utf-8"))
            check(actual == expected, f"{case} plan differs from its golden")
            check(plan(source).stdout == result.stdout, "serialization is not deterministic")
            plans[case] = actual
        basic = plans["basic"]["scenarios"][0]
        no_internal = plan(group / "basic.exp", "--internal-checks", "0")
        check(no_internal.returncode == 0, no_internal.stderr)
        ordinary = json.loads(no_internal.stdout)["scenarios"][0]
        check(len(basic["checks"]) == 2 and len(ordinary["checks"]) == 1,
              "internal policy must add exactly one object-load assertion")
        check(ordinary["checks"][0]["id"] == basic["checks"][0]["id"],
              "internal policy changed ordinary check identity")
        configured = plan(group / "basic.exp", "--compiler-option", "-v")
        check(configured.returncode == 0 and
              json.loads(configured.stdout)["scenarios"][0]["steps"][0]["operation"]["options"] == ["-v"],
              "explicit configuration option was not retained")

        plan_path = work / "basic.json"
        plan_path.write_text(json.dumps(plans["basic"]), encoding="utf-8")
        explained = run("explain", plan_path, selector(basic["checks"][1]["id"]))
        check(explained.returncode == 0, explained.stderr)
        check(explained.stdout == (FIXTURES / "basic.explain.txt").read_text(encoding="utf-8"),
              "explanation differs from its golden")
        unknown = run("explain", plan_path, "unknown")
        check(unknown.returncode != 0 and not unknown.stdout, "unknown check was accepted")

        whole = plan(suite)
        check(whole.returncode == 0, whole.stderr)
        check([item["test"] for item in json.loads(whole.stdout)["scenarios"]] ==
              [f"bsc.plan/{case}.exp" for case in sorted(CASES)],
              "suite selection omitted or reordered discovered scripts")
        for root in (str(suite), "suite"):
            for root_suffix in ("", "/"):
                for target in (str(suite), "suite", str(group), "suite/bsc.plan"):
                    for target_suffix in ("", "/"):
                        selected = run("plan", "--config", "golden", "--suite-root",
                                       root + root_suffix, target + target_suffix)
                        check(selected.returncode == 0,
                              f"equivalent directory selection failed: {selected.stderr}")
                        check(selected.stdout == whole.stdout,
                              "directory spelling changed the selected plan")
        broken = group / "unsupported.exp"
        broken.write_text("compile_pass Example.bs\nexec touch SHOULD_NOT_EXIST\n", encoding="utf-8")
        refused = plan(suite)
        check(refused.returncode == 2 and not refused.stdout,
              "an unsupported script emitted a partial plan")
        check("bsc.plan/unsupported.exp:2:1: exec:" in refused.stderr,
              "unsupported diagnostic omitted its source location or construct")
        check(not (work / "SHOULD_NOT_EXIST").exists(), "planner executed Tcl")
        malformed = group / "unsupported.exp"
        malformed.write_text("compile_pass {unterminated\n", encoding="utf-8")
        refused = plan(malformed)
        check(refused.returncode == 2 and not refused.stdout and "Tcl syntax" in refused.stderr,
              "malformed Tcl was not rejected")
        for extra in (("--internal-checks", "2"), ("--config", "duplicate"),
                      ("--compiler-option", "-unsupported")):
            invalid = plan(group / "basic.exp", *extra)
            check(invalid.returncode != 0 and not invalid.stdout,
                  f"invalid configuration was accepted: {extra}")
        outside = work / "outside.exp"
        outside.write_text("compile_pass Example.bs\n", encoding="utf-8")
        escaped = plan(outside)
        check(escaped.returncode != 0 and not escaped.stdout, "outside-suite script was accepted")
    print("Plan/explain CLI goldens, configuration, selection, and rejection checks passed")


if __name__ == "__main__":
    main()
