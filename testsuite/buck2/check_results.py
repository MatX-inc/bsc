#!/usr/bin/env python3
"""Validate every emitted BSC test result; Buck build success is not test success."""

import argparse
from collections import Counter
import json
from pathlib import Path
import sys


class Invalid(ValueError):
    pass


def require(condition, message):
    if not condition:
        raise Invalid(message)


def unique_object(pairs):
    result = {}
    for key, value in pairs:
        require(key not in result, f"duplicate JSON field: {key}")
        result[key] = value
    return result


def read_json(path):
    return json.loads(Path(path).read_text(encoding="utf-8"),
                      object_pairs_hook=unique_object)


def identity(value):
    require(isinstance(value, dict) and set(value) == {"test", "number"},
            "invalid test identity")
    test, number = value["test"], value["number"]
    require(isinstance(test, str) and test and not test.startswith("/")
            and all(part not in ("", ".", "..") for part in test.split("/"))
            and test.endswith(".exp"), "invalid test path")
    require(type(number) is int and number > 0, "invalid test number")
    return test, number


def selector(key):
    test, number = key
    return f"v3:{len(test)}:{test}:{number}"


def target_key(target):
    require(isinstance(target, str) and target.count("//") == 1,
            "invalid target label")
    cell, label = target.split("//", 1)
    require(cell in ("", "root") and label.startswith(":"),
            f"unexpected target label: {target}")
    return "//" + label


def configuration(value):
    require(isinstance(value, dict)
            and set(value) == {"name", "internal_checks", "compiler_options"},
            "invalid execution configuration")
    require(isinstance(value["name"], str) and value["name"],
            "invalid configuration name")
    require(type(value["internal_checks"]) is bool, "invalid internal-check policy")
    require(isinstance(value["compiler_options"], list)
            and all(isinstance(x, str) for x in value["compiler_options"]),
            "invalid compiler options")
    return value


def planned_tests(plan):
    require(plan.get("schema") == "bsc-testsuite-test-plan"
            and plan.get("version") == 3
            and plan.get("identity") == "file-test-number-v1", "unsupported plan")
    configuration(plan["configuration"])
    tests = {}
    gaps = Counter()
    for script in plan["scripts"]:
        for item in script["items"]:
            if item["status"] == "planned":
                key = identity(item["id"])
                require(key[0] == script["path"], "test identity differs from script")
                require(key not in tests, f"duplicate planned identity: {selector(key)}")
                kind = item["kind"]
                require(kind["kind"] == "compilation"
                        and kind["expectation"] in ("succeeds", "fails"),
                        "unsupported planned test kind")
                tests[key] = item
            else:
                require(item["status"] in ("unsupported", "unresolved"),
                        "unknown planning status")
                gaps[item["status"]] += 1
                gaps["numbered" if item["id"] is not None else "unnumbered"] += 1
    return tests, gaps


def expected_roles(test, config):
    return ["compilation"] + (["object-load"] if config["internal_checks"]
            and test["kind"]["expectation"] == "succeeds" else [])


def validate_execution(report, key, test, config):
    require(isinstance(report, dict) and set(report) == {
                "schema", "version", "id", "configuration", "installation",
                "working_directory", "staged_inputs", "checks"},
            "malformed execution report fields")
    require(report.get("schema") == "bsc-test-execution"
            and type(report.get("version")) is int and report["version"] == 1,
            "unsupported execution report")
    require(identity(report["id"]) == key, "execution identity differs from target")
    require(configuration(report["configuration"]) == config,
            "execution configuration differs from plan")
    require(all(isinstance(report[field], str) and report[field]
                for field in ("installation", "working_directory")),
            "invalid execution location metadata")
    require(isinstance(report["staged_inputs"], list)
            and all(isinstance(path, str) for path in report["staged_inputs"]),
            "invalid staged input metadata")
    checks = report["checks"]
    require(isinstance(checks, list), "checks must be a list")
    roles = [check["role"] for check in checks]
    require(roles == expected_roles(test, config),
            f"result roles differ: {roles!r}")
    counts = Counter()
    for check in checks:
        require(set(check) == {"role", "disposition", "process"},
                "malformed check fields")
        disposition = check["disposition"]
        require(disposition in ("PASS", "FAIL", "INFRASTRUCTURE_ERROR"),
                "unknown result disposition")
        process = check["process"]
        require(isinstance(process, dict) and set(process) == {
                    "program", "arguments", "transcript", "termination"},
                "malformed process fields")
        require(isinstance(process["program"], str) and process["program"],
                "missing process program")
        require(isinstance(process["arguments"], list)
                and all(isinstance(x, str) for x in process["arguments"]),
                "invalid process arguments")
        require(isinstance(process["transcript"], str) and process["transcript"],
                "missing process transcript")
        termination = process["termination"]
        mode = termination["kind"]
        if mode == "exited":
            code = termination["exit_code"]
            require(type(code) is int and code >= 0, "invalid process exit code")
            expect_success = (check["role"] == "object-load"
                              or test["kind"]["expectation"] == "succeeds")
            derived = ("INFRASTRUCTURE_ERROR" if code >= 126 else
                       "PASS" if (code == 0) == expect_success else "FAIL")
        else:
            require(mode in ("signal", "timeout", "launch-error"),
                    "unknown process termination")
            if mode == "signal":
                require(type(termination["signal"]) is int
                        and termination["signal"] > 0
                        and termination["exit_code"] == -termination["signal"],
                        "invalid process signal")
            elif mode == "timeout":
                require(type(termination["exit_code"]) is int,
                        "invalid timeout exit code")
            else:
                require(isinstance(termination["reason"], str)
                        and termination["reason"], "missing launch-error reason")
            derived = "INFRASTRUCTURE_ERROR"
        require(disposition == derived,
                f"{check['role']} disposition contradicts process termination")
        counts[disposition] += 1
    return counts


def audit(cell, build_report):
    cell = Path(cell).resolve()
    plan = read_json(cell / "plan.json")
    tests, gaps = planned_tests(plan)
    by_selector = {selector(key): key for key in tests}
    manifest = read_json(cell / "targets.json")
    require(manifest.get("schema") == "bsc-buck2-targets"
            and manifest.get("version") == 1, "unsupported target manifest")
    require(manifest["planned"] == len(tests)
            and manifest["unsupported"] == gaps["unsupported"]
            and manifest["unresolved"] == gaps["unresolved"],
            "target manifest population differs from plan")
    targets = {}
    selected = set()
    for item in manifest["targets"]:
        target = target_key(item["target"])
        require(target not in targets, f"duplicate emitted target: {target}")
        require(item["id"] in by_selector, "target names an unplanned identity")
        key = by_selector[item["id"]]
        require(key not in selected, f"duplicate emitted identity: {item['id']}")
        targets[target] = key
        selected.add(key)
    execution_gaps = set()
    for gap in manifest["execution_gaps"]:
        require(gap["id"] in by_selector, "execution gap names an unplanned identity")
        key = by_selector[gap["id"]]
        require(key not in selected and key not in execution_gaps,
                "duplicate or emitted execution gap")
        require(isinstance(gap["reason"], str) and gap["reason"],
                "execution gap has no reason")
        execution_gaps.add(key)
    require(selected | execution_gaps == set(tests),
            "manifest omits planned tests without an execution gap")
    require(targets, "target manifest has no emitted tests")

    build = read_json(build_report)
    require(isinstance(build, dict) and isinstance(build["results"], dict)
            and isinstance(build["failures"], dict), "malformed Buck2 build report")
    errors = []
    if build.get("success") is not True:
        errors.append("Buck2 build was not successful")
    if build.get("truncated") is not False:
        errors.append("Buck2 build report is truncated or lacks completion metadata")
    if build.get("failures"):
        errors.append("Buck2 build report contains failures")
    require(Path(build["project_root"]).resolve() == cell,
            "Buck2 build report belongs to a different cell")
    results = {}
    for label, value in build["results"].items():
        target = target_key(label)
        require(target not in results, f"duplicate build-report target: {target}")
        results[target] = value
    for target in sorted(set(results) - set(targets)):
        errors.append(f"unexpected build-report target: {target}")
    counts = Counter()
    verified = 0
    for target, key in targets.items():
        try:
            require(target in results, "missing target from build report")
            result = results[target]
            require(result["success"] == "SUCCESS" and not result.get("errors"),
                    "target build did not succeed")
            for configured in result.get("configured", {}).values():
                require(configured["success"] == "SUCCESS"
                        and not configured.get("errors"), "configured target build failed")
            outputs = result["outputs"]
            require(set(outputs) == {"DEFAULT"}
                    and isinstance(outputs["DEFAULT"], list)
                    and len(outputs["DEFAULT"]) == 1,
                    "expected one default result directory")
            output = Path(outputs["DEFAULT"][0])
            if not output.is_absolute():
                output = cell / output
            require(output.resolve().is_relative_to(cell), "result directory escapes cell")
            report = read_json(output / "result.json")
            counts.update(validate_execution(report, key, tests[key], plan["configuration"]))
            verified += 1
        except (Invalid, KeyError, TypeError, ValueError, OSError) as error:
            errors.append(f"{target} {selector(key)}: {error}")
    if counts["FAIL"] or counts["INFRASTRUCTURE_ERROR"]:
        errors.append("one or more executed checks did not pass")
    return {"emitted_tests": len(targets), "verified_tests": verified,
            "checks": dict(counts), "planning_gaps": dict(gaps),
            "execution_gaps": len(execution_gaps), "errors": errors}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("cell", type=Path)
    parser.add_argument("build_report", type=Path)
    args = parser.parse_args()
    try:
        report = audit(args.cell, args.build_report)
    except (Invalid, KeyError, TypeError, ValueError, OSError) as error:
        print(f"ERROR: {error}", file=sys.stderr)
        return 1
    checks = report["checks"]
    print(f"Tests: {report['verified_tests']}/{report['emitted_tests']}; "
          f"PASS {checks.get('PASS', 0)}; FAIL {checks.get('FAIL', 0)}; "
          f"INFRASTRUCTURE_ERROR {checks.get('INFRASTRUCTURE_ERROR', 0)}")
    gaps = report["planning_gaps"]
    print(f"Planning gaps: unsupported {gaps.get('unsupported', 0)}, "
          f"unresolved {gaps.get('unresolved', 0)}, "
          f"numbered {gaps.get('numbered', 0)}, unnumbered {gaps.get('unnumbered', 0)}; "
          f"execution gaps {report['execution_gaps']}")
    for error in report["errors"]:
        print(f"ERROR: {error}", file=sys.stderr)
    return 1 if report["errors"] else 0


if __name__ == "__main__":
    sys.exit(main())
