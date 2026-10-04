#!/usr/bin/env python3
"""Check semantic plan/explain goldens, explicit issues, and CLI validation."""

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
    return f"v3:{len(test)}:{test}:{identifier['number']}"


def check_summary(result, plan):
    statuses = [item["status"] for script in plan["scripts"] for item in script["items"]]
    expected = (f"Plan: {statuses.count('planned')} planned, "
                f"{statuses.count('unsupported')} unsupported, "
                f"{statuses.count('unresolved')} unresolved.")
    check(expected in result.stderr, "plan summary differs from represented items")


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
            check_summary(result, actual)
            plans[case] = actual
        basic = plans["basic"]["scripts"][0]
        no_internal = plan(group / "basic.exp", "--internal-checks", "0")
        check(no_internal.returncode == 0, no_internal.stderr)
        ordinary = json.loads(no_internal.stdout)
        check(ordinary["scripts"] == plans["basic"]["scripts"],
              "internal policy changed semantic tests or their identities")
        check(plans["basic"]["configuration"]["internal_checks"] is True and
              ordinary["configuration"]["internal_checks"] is False,
              "internal policy was not retained in the configuration")
        check_summary(no_internal, ordinary)
        configured = plan(group / "basic.exp", "--compiler-option", "-v")
        check(configured.returncode == 0 and
              json.loads(configured.stdout)["scripts"][0]["items"][0]["kind"]["options"] == ["-v"],
              "explicit configuration option was not retained")

        plan_path = work / "basic.json"
        plan_path.write_text(json.dumps(plans["basic"]), encoding="utf-8")
        first_test = next(item for item in basic["items"] if item["status"] == "planned")
        explained = run("explain", plan_path, selector(first_test["id"]))
        check(explained.returncode == 0, explained.stderr)
        check(explained.stdout == (FIXTURES / "basic.explain.txt").read_text(encoding="utf-8"),
              "explanation differs from its golden")
        unknown = run("explain", plan_path, "unknown")
        check(unknown.returncode != 0 and not unknown.stdout, "unknown test was accepted")

        whole = plan(suite)
        check(whole.returncode == 0, whole.stderr)
        whole_plan = json.loads(whole.stdout)
        check([script["path"] for script in whole_plan["scripts"]] ==
              [f"bsc.plan/{case}.exp" for case in sorted(CASES)],
              "suite selection omitted or reordered discovered scripts")
        check_summary(whole, whole_plan)
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
        mixed = group / "mixed.exp"
        mixed.write_text("compile_pass Before.bs\ncompile_verilog_pass Example.bs\n"
                         "compile_pass After.bs\n", encoding="utf-8")
        mixed_result = plan(mixed)
        check(mixed_result.returncode == 0, mixed_result.stderr)
        mixed_plan = json.loads(mixed_result.stdout)
        mixed_items = mixed_plan["scripts"][0]["items"]
        check([item["status"] for item in mixed_items] ==
              ["planned", "unsupported", "planned"],
              "unsupported helper removed surrounding semantic tests")
        check([item["id"]["number"] if item["id"] else None for item in mixed_items] ==
              [1, None, 2], "unsupported helper consumed a test invocation number")
        check("bsc.plan/mixed.exp:2:1:" in mixed_result.stderr and
              "compile_verilog_pass" in mixed_result.stderr,
              "unsupported diagnostic omitted its source location or construct")
        check_summary(mixed_result, mixed_plan)
        mixed_path = work / "mixed.json"
        mixed_path.write_text(mixed_result.stdout, encoding="utf-8")
        last_explained = run("explain", mixed_path, selector(mixed_items[2]["id"]))
        check(last_explained.returncode == 0 and "After.bs" in last_explained.stdout,
              "numbered test after an unnumbered helper could not be explained")

        numbered = group / "numbered.exp"
        numbered.write_text("set unused ignored\ncompile_pass First.bs {-unknown}\n"
                            "compile_pass Last.bs\n", encoding="utf-8")
        numbered_result = plan(numbered)
        check(numbered_result.returncode == 0, numbered_result.stderr)
        numbered_plan = json.loads(numbered_result.stdout)
        numbered_items = numbered_plan["scripts"][0]["items"]
        check([item["id"]["number"] for item in numbered_items] == [1, 2] and
              [item["status"] for item in numbered_items] == ["unsupported", "planned"],
              "recognized unsupported invocation did not reserve its number")
        numbered_path = work / "numbered.json"
        numbered_path.write_text(numbered_result.stdout, encoding="utf-8")
        issue_explained = run("explain", numbered_path, selector(numbered_items[0]["id"]))
        check(issue_explained.returncode == 0 and
              "unsupported" in issue_explained.stdout.lower() and
              "compile_pass" in issue_explained.stdout,
              "numbered unsupported invocation could not be explained")

        effectful = group / "effectful.exp"
        effectful.write_text("compile_pass Before.bs\nexec touch SHOULD_NOT_EXIST\n"
                             "compile_pass After.bs\n", encoding="utf-8")
        effectful_result = plan(effectful)
        check(effectful_result.returncode == 0, effectful_result.stderr)
        effectful_plan = json.loads(effectful_result.stdout)
        effectful_items = effectful_plan["scripts"][0]["items"]
        check([item["status"] for item in effectful_items] ==
              ["planned", "unsupported", "unresolved"],
              "arbitrary exec lost the supported prefix or did not mark dependent work unresolved")
        check([item["id"]["number"] if item["id"] else None for item in effectful_items] ==
              [1, None, 2], "opaque setup diagnostics consumed a test invocation number")
        check("bsc.plan/effectful.exp:2:1:" in effectful_result.stderr and
              "exec" in effectful_result.stderr,
              "exec diagnostic omitted its source location or construct")
        check_summary(effectful_result, effectful_plan)
        check(not (work / "SHOULD_NOT_EXIST").exists(), "planner executed Tcl")

        malformed = group / "malformed.exp"
        malformed.write_text("compile_pass {unterminated\n", encoding="utf-8")
        malformed_result = plan(suite)
        check(malformed_result.returncode == 0 and "Tcl syntax" in malformed_result.stderr,
              "malformed Tcl prevented a structurally valid plan")
        malformed_plan = json.loads(malformed_result.stdout)
        scripts = {script["path"]: script for script in malformed_plan["scripts"]}
        syntax_items = scripts["bsc.plan/malformed.exp"]["items"]
        check(len(syntax_items) == 1 and syntax_items[0]["status"] in
              ("unsupported", "unresolved"), "malformed Tcl has no explicit issue item")
        check(syntax_items[0]["id"] is None, "whole-script syntax error has an invocation number")
        for script in whole_plan["scripts"] + mixed_plan["scripts"] + effectful_plan["scripts"]:
            check(scripts[script["path"]] == script,
                  "malformed Tcl changed another selected script")
        check_summary(malformed_result, malformed_plan)
        for extra in (("--internal-checks", "2"), ("--config", "duplicate"),
                      ("--compiler-option", "-unsupported")):
            invalid = plan(group / "basic.exp", *extra)
            check(invalid.returncode != 0 and not invalid.stdout,
                  f"invalid configuration was accepted: {extra}")
        outside = work / "outside.exp"
        outside.write_text("compile_pass Example.bs\n", encoding="utf-8")
        escaped = plan(outside)
        check(escaped.returncode != 0 and not escaped.stdout, "outside-suite script was accepted")
    print("Semantic plan/explain CLI goldens, configuration, selection, and issue checks passed")


if __name__ == "__main__":
    main()
