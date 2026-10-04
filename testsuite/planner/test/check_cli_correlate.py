#!/usr/bin/env python3
"""Match direct unix.exp log markers to CLI plans, with inert compiler calls."""

import argparse
import json
import os
from pathlib import Path
import subprocess
import tempfile

import check_provenance as harness


def check(value, message):
    if not value:
        raise AssertionError(message)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("planner", type=Path)
    args = parser.parse_args()
    executable = args.planner.resolve(strict=True)
    with tempfile.TemporaryDirectory(prefix="bsc-correlate-cli-") as directory:
        root = Path(directory)
        source_text = ("compile_pass Example.bs\n"
                       "compile_backend_pass Ignored.bs\n"
                       "foreach flags {{} {-v} {-v}} {\n"
                       "  compile_pass Example.bs $flags\n"
                       "}\n"
                       "compile_fail Fail.bs\n")
        driver = root / "driver.tcl"
        driver.write_text(harness.DRIVER, encoding="utf-8")

        def run(*arguments):
            return subprocess.run([str(executable), *map(str, arguments)],
                                  capture_output=True, text=True, timeout=30)

        def capture(output, internal, enabled=True):
            env = dict(os.environ, PROVENANCE_DEJAGNU=str(harness.DEJAGNU),
                       PROVENANCE_SUITE=str(harness.SUITE), PROVENANCE_ROOT=str(output),
                       PROVENANCE_ENABLED=str(int(enabled)), PROVENANCE_INTERNAL=str(internal),
                       BSC_TEST_TRACE=str(int(enabled)))
            observed = subprocess.run(["tclsh", str(driver)], env=env,
                                      capture_output=True, text=True, timeout=30)
            check(observed.returncode == 0 and not observed.stderr,
                  f"harness failed: {observed.stderr}{observed.stdout}")

        plans = []
        invocations = []
        for internal in (1, 0):
            output = root / f"capture-{internal}"
            cases = output / "cases"
            cases.mkdir(parents=True)
            source = cases / "example.exp"
            source.write_text(source_text, encoding="utf-8")
            capture(output, internal)
            log = output / "testrun.log"
            log_text = log.read_text(encoding="utf-8")
            invocations.append([line for line in log_text.splitlines()
                                if line.startswith("BSC-TEST: begin ")])
            check([line.split()[2] for line in invocations[-1]] == list(map(str, range(1, 6))),
                  "unsupported wrapper consumed a test number")
            planned = run("plan", "--config", "correlation", "--internal-checks", internal,
                          "--suite-root", output, source)
            check(planned.returncode == 0, planned.stderr)
            plans.append(json.loads(planned.stdout))
            plan_file = root / f"plan-{internal}.json"
            plan_file.write_text(planned.stdout, encoding="utf-8")
            result = run("correlate", plan_file, output)
            check(result.returncode == 0, result.stdout + result.stderr)
            check("5 matched tests, 0 skipped numbered tests, 0 problems" in result.stdout,
                  result.stdout)
            check(result.stdout.count("object-load=PASS") == (4 if internal else 0),
                  "internal results are not attached to their parent tests")
            single = run("correlate", plan_file, log)
            check(single.returncode == 0 and single.stdout == result.stdout,
                  "single-log and directory correlation differ")
            summary = (output / "testrun.sum").read_bytes()
            capture(output, internal, enabled=False)
            check((output / "testrun.sum").read_bytes() == summary,
                  "numbering changed legacy verdicts")
            check("BSC-TEST:" not in log.read_text(encoding="utf-8"),
                  "disabled numbering still wrote markers")
            log.write_text(log_text, encoding="utf-8")
        check(plans[0]["scripts"] == plans[1]["scripts"], "internal policy renamed tests")
        check(invocations[0] == invocations[1], "harness internal policy renamed tests")
        original = root / "plan-1.json"
        capture_dir = root / "capture-1"
        changed = json.loads(original.read_text(encoding="utf-8"))
        changed["scripts"][0]["items"][0]["kind"]["options"] = ["-v"]
        changed_file = root / "changed.json"
        changed_file.write_text(json.dumps(changed), encoding="utf-8")
        mismatch = run("correlate", changed_file, capture_dir)
        check(mismatch.returncode != 0 and "resolved arguments differ" in mismatch.stdout,
              "matching counters concealed a changed argument list")
        missing = root / "missing"
        missing.mkdir()
        absent = run("correlate", original, missing)
        check(absent.returncode != 0 and "missing" in absent.stdout,
              "missing captures were treated as matches")
        log_text = (capture_dir / "testrun.log").read_text(encoding="utf-8")
        check("BSC-TEST: finish 5" in log_text, "script finish was not logged")
        incomplete = root / "incomplete.log"
        incomplete.write_text(log_text.replace("BSC-TEST: finish 5", ""), encoding="utf-8")
        unfinished = run("correlate", original, incomplete)
        check(unfinished.returncode != 0, "an incomplete capture was accepted")
        bad_script = root / "error.log"
        bad_script.write_text(log_text.replace("BSC-TEST: finish 5",
                              "ERROR: injected script error\nBSC-TEST: finish 5"), encoding="utf-8")
        check(run("correlate", original, bad_script).returncode != 0,
              "a script error was hidden by matching invocation counters")
    print("Direct harness log/CLI correspondence, unchanged verdicts, and mismatch checks passed")


if __name__ == "__main__":
    main()
