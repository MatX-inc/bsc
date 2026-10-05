#!/usr/bin/env python3
"""Compare real diagnostic tests through Buck2 and DejaGNU, including mutations."""

import argparse
from collections import Counter
import json
from pathlib import Path
import re
import shutil
import subprocess
import sys

import check_results
from check_mutation import Experiment, digest, require, save_json


TESTS = (
    "bsc.typechecker/bound-type-vars/bound-type-vars.exp",
    "bsc.typechecker/error_recovery/error_recovery.exp",
)
SOURCE = Path(TESTS[0]).parent / "KindMismatchExplExpl.bs"
SYNTAX_ERROR = b"*** deliberate syntax error for the diagnostic regression ***\n"
SUCCESSFUL_SOURCE = b"package KindMismatchExplExpl () where\n"
VERDICTS = re.compile(r"^(PASS|FAIL|XFAIL|XPASS|KFAIL|KPASS|UNRESOLVED|UNTESTED|UNSUPPORTED):",
                      re.MULTILINE)


class DiagnosticExperiment(Experiment):
    """Reuse the mutation runner's command recording and Git file discovery."""

    def __init__(self, args):
        self.repo = args.repo.resolve()
        self.planner = args.planner.resolve()
        self.buck2 = args.buck2.resolve()
        self.output = args.output.absolute()
        self.installation = (args.installation or self.repo / "inst").resolve()
        require(not self.output.exists(), "output must be a new evidence directory")
        self.output.mkdir(parents=True)
        self.cell = self.output / "cell"
        self.legacy = self.output / "legacy" / "testsuite"
        self.original = (self.repo / "testsuite" / SOURCE).read_bytes()
        (self.output / "KindMismatchExplExpl.bs.original").write_bytes(self.original)
        self.summary = {"tests": list(TESTS), "source": str(SOURCE),
                        "original_sha256": digest(self.original), "phases": {}, "success": False}
        self.daemon = None

    def prepare(self):
        self.summary["planner_sha256"] = digest(self.planner.read_bytes())
        self.summary["buck2_sha256"] = digest(self.buck2.read_bytes())
        self.run("buck2-version", [self.buck2, "--version"])
        plans = []
        for number, test in enumerate(TESTS):
            run = self.run(f"plan-{number}", [self.planner, "plan", "--config",
                           "diagnostic-integration", "--suite-root", self.repo / "testsuite",
                           "--internal-checks", "1", self.repo / "testsuite" / test])
            plans.append(json.loads(run.stdout))
        plan = plans[0]
        require(all({key: value for key, value in other.items() if key != "scripts"}
                    == {key: value for key, value in plan.items() if key != "scripts"}
                    for other in plans), "per-script plan configurations differ")
        plan["scripts"] = [script for other in plans for script in other["scripts"]]
        require([script["path"] for script in plan["scripts"]] == list(TESTS),
                "unexpected planned scripts")
        tests, gaps = check_results.planned_tests(plan)
        require(len(tests) == 13 and all(item["kind"]["kind"] == "compilation-error"
                                      for item in tests.values()),
                "expected thirteen diagnostic tests")
        require(gaps == Counter(unsupported=5, numbered=1, unnumbered=4),
                f"unexpected planning gaps: {dict(gaps)}")
        self.identifiers = {check_results.selector(key) for key in tests}
        self.changed_identifier = check_results.selector((TESTS[0], 1))
        self.skipped = {check_results.selector(check_results.identity(item["id"]))
                        for script in plan["scripts"] for item in script["items"]
                        if item["status"] != "planned" and item["id"] is not None}
        self.plan_path = self.output / "plan.json"
        save_json(self.plan_path, plan)
        self.run("emit", [self.planner, "emit-buck2", self.plan_path, "--suite-root",
                 self.repo / "testsuite", "--installation", self.installation,
                 "--output", self.cell])
        manifest = check_results.read_json(self.cell / "targets.json")
        require(len(manifest["targets"]) == len(tests) and not manifest["execution_gaps"],
                "expected all thirteen diagnostic targets to be executable")
        require({item["id"] for item in manifest["targets"]} == self.identifiers,
                "emitted target identities differ from the plan")
        self.targets = [item["target"] for item in manifest["targets"]]
        self.summary["planning"] = {"planned": len(tests), "gaps": dict(gaps),
                                    "skipped_identifiers": sorted(self.skipped)}
        self.copy_legacy_inputs()

    def copy_legacy_inputs(self):
        for path in self.tracked("testsuite/config", "testsuite/lib",
                                 "testsuite/scripts/collapse.pl", "testsuite/site.exp"):
            relative = path.relative_to("testsuite")
            destination = self.legacy / relative
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(self.cell / "suite" / relative, destination, follow_symlinks=False)
        self.test_inputs = self.tracked(*("testsuite/" + str(Path(test).parent) for test in TESTS))
        self.original_inputs = {str(path): digest((self.repo / path).read_bytes())
                                for path in self.test_inputs}
        self.legacy_environment = {
            "PATH": str(self.cell / "installation/bin") + ":/usr/bin:/bin",
            "LANG": "C", "LC_ALL": "C", "MAKEFLAGS": "", "BSCTEST": "1",
            "BSC_OPTIONS": "", "BLUESPEC_LD_LIBRARY_PATH": "",
            "LD_LIBRARY_PATH": str(self.cell / "installation/lib/SAT"),
            "BSC_TEST_TRACE": "1", "DO_INTERNAL_CHECKS": "1",
            "VTEST": "1", "CTEST": "1", "SYSTEMCTEST": "1", "BSC_VERILOG_SIM": "iverilog",
            "TEST_CONFIG_DIR": str(self.legacy / "config"),
            "BSDIR": str(self.cell / "installation/lib"),
            "BLUESPECDIR": str(self.cell / "installation/lib"),
            "SYSTEMC_INC": "/usr/include", "SYSTEMC_LIB": "/usr/lib/x86_64-linux-gnu",
            "SYSTEMC_CXXFLAGS": "-std=c++17",
            "TEST_SYSTEMC_INC": "/usr/include", "TEST_SYSTEMC_LIB": "/usr/lib/x86_64-linux-gnu",
            "TEST_SYSTEMC_CXXFLAGS": "-std=c++17",
        }
        for variable, option in [("OSTYPE", "ostype"), ("MACHTYPE", "machtype")]:
            self.legacy_environment[variable] = subprocess.check_output(
                [str(self.repo / "platform.sh"), option], text=True).strip()
        for variable, tool in [("BSC", "bsc"), ("BLUETCL", "bluetcl"), ("DUMPBO", "dumpbo"),
                               ("DUMPBA", "dumpba"), ("BSC2BSV", "bsc2bsv"),
                               ("VCDCHECK", "vcdcheck"), ("SHOWRULES", "showrules")]:
            self.legacy_environment[variable] = str(self.cell / "installation/bin" / tool)

    def set_sources(self, data):
        (self.cell / "suite" / SOURCE).write_bytes(data)
        # Only these isolated legacy directories are reset, never the Buck2 cache.
        for test in TESTS:
            directory = self.legacy / Path(test).parent
            if directory.exists():
                shutil.rmtree(directory)
        for path in self.test_inputs:
            relative = path.relative_to("testsuite")
            destination = self.legacy / relative
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(self.cell / "suite" / relative, destination, follow_symlinks=False)

    def source_hashes(self, expected):
        require({str(path): digest((self.repo / path).read_bytes()) for path in self.test_inputs}
                == self.original_inputs, "checkout test inputs changed")
        for path in self.test_inputs:
            relative = path.relative_to("testsuite")
            require((self.cell / "suite" / relative).read_bytes()
                    == (self.legacy / relative).read_bytes(),
                    f"Buck2 and DejaGNU input bytes differ: {relative}")
        hashes = {"checkout": digest((self.repo / "testsuite" / SOURCE).read_bytes()),
                  "buck2": digest((self.cell / "suite" / SOURCE).read_bytes()),
                  "legacy": digest((self.legacy / SOURCE).read_bytes())}
        require(hashes["checkout"] == digest(self.original), "checkout source changed")
        require(hashes["buck2"] == hashes["legacy"] == digest(expected), "source mutation differs")
        return hashes

    def expected_checks(self, phase):
        expected = {identifier: [("diagnostic-count", "PASS")] for identifier in self.identifiers}
        if phase == "wrong-diagnostic":
            expected[self.changed_identifier] = [("diagnostic-count", "FAIL")]
        elif phase == "unexpected-success":
            expected[self.changed_identifier] = [("compilation", "FAIL"), ("object-load", "PASS")]
        return expected

    def buck_phase(self, phase, expected, reruns):
        directory = self.output / phase
        directory.mkdir(exist_ok=True)
        report_path = directory / "build.json"
        build = self.run(phase + "/build", [self.buck2, "build", "-j13", *self.targets,
                         "--build-report", report_path], cwd=self.cell)
        report = check_results.read_json(report_path)
        require(report["success"] is True and len(report["results"]) == len(self.targets),
                "unexpected Buck2 result population")
        failing = any(verdict != "PASS" for checks in expected.values() for _, verdict in checks)
        checked = self.run(phase + "/checker", [sys.executable,
                           self.repo / "testsuite/buck2/check_results.py", self.cell, report_path],
                           expected=1 if failing else 0)
        audit = check_results.audit(self.cell, report_path)
        expected_counts = Counter(verdict for checks in expected.values() for _, verdict in checks)
        require(audit["verified_tests"] == len(self.targets)
                and Counter(audit["checks"]) == expected_counts
                and audit["errors"] == (["one or more executed checks did not pass"] if failing else []),
                f"{phase}: unexpected structural result-checker errors: {audit['errors']}")
        observed = {}
        for target, result in report["results"].items():
            outputs = result["outputs"]["DEFAULT"]
            require(len(outputs) == 1, "expected one result directory per target")
            result_dir = Path(outputs[0])
            if not result_dir.is_absolute():
                result_dir = self.cell / result_dir
            execution = check_results.read_json(result_dir / "result.json")
            identifier = check_results.selector(check_results.identity(execution["id"]))
            require(identifier not in observed, "duplicate execution identity")
            observed[identifier] = [(check["role"], check["disposition"])
                                    for check in execution["checks"]]
            saved = directory / "results" / check_results.target_key(target).removeprefix("//:")
            saved.mkdir(parents=True)
            shutil.copy2(result_dir / "result.json", saved / "result.json")
            for check in execution["checks"]:
                transcript = Path(check["process"]["transcript"])
                require(not transcript.is_absolute() and len(transcript.parts) == 1,
                        "expected an output-relative transcript filename")
                shutil.copy2(result_dir / transcript, saved / transcript)
        require(observed == expected, f"{phase}: Buck2 results differ from expected checks")
        ran = self.run(phase + "/what-ran", [self.buck2, "log", "what-ran", "--trace-id",
                       report["trace_id"], "--format", "json", "--filter-category", "bsc_test"],
                       cwd=self.cell)
        actions = [json.loads(line) for line in ran.stdout.splitlines() if line]
        require(len(actions) in reruns,
                f"{phase}: expected action count in {sorted(reruns)}, saw {len(actions)}")
        status = self.run(phase + "/daemon", [self.buck2, "status"], cwd=self.cell)
        status_json = json.loads(status.stdout)
        pid = status_json.get("process_info", {}).get("pid", status_json.get("pid"))
        require(type(pid) is int, "could not identify Buck2 daemon PID")
        if self.daemon is None:
            self.daemon = pid
        require(pid == self.daemon, "Buck2 daemon changed during the experiment")
        return {"build_exit": build.returncode, "checker_exit": checked.returncode,
                "checks": observed, "action_runs": len(actions), "daemon_pid": pid,
                "trace_id": report["trace_id"]}

    def legacy_phase(self, phase, expected):
        dispositions = Counter()
        statuses = {}
        for test in TESTS:
            directory = self.legacy / Path(test).parent
            fails = sum(verdict != "PASS" for identifier, checks in expected.items()
                        if identifier.startswith(f"v3:{len(test)}:{test}:")
                        for _, verdict in checks)
            run = self.run(phase + "/dejagnu-" + Path(test).stem,
                           ["runtest", "--tool", "", "--objdir", ".", "--status", Path(test).name],
                           cwd=directory, environment=self.legacy_environment,
                           expected=1 if fails else 0)
            text = (directory / "testrun.sum").read_text()
            log = (directory / "testrun.log").read_text()
            require(not re.search(r"^ERROR:", text + "\n" + log, re.MULTILINE),
                    "DejaGNU infrastructure error")
            dispositions.update(VERDICTS.findall(text))
            statuses[test] = run.returncode
            shutil.copytree(directory, self.output / phase / "legacy-results" / Path(test).parent)
        failure_pairs = {(identifier, role) for identifier, checks in expected.items()
                         for role, verdict in checks if verdict != "PASS"}
        correlation = self.run(phase + "/correlate", [self.planner, "correlate", self.plan_path,
                               self.legacy], expected=1 if failure_pairs else 0)
        matches, skips, problems = {}, set(), []
        for line in correlation.stdout.splitlines():
            if line.startswith("MATCH "):
                identifier, checks = line[6:].split(" ", 1)
                require(identifier not in matches, "duplicate correlation identity")
                matches[identifier] = [tuple(check.split("=", 1)) for check in checks.split(", ")]
            elif line.startswith("SKIP "):
                identifier = line[5:].split(": ", 1)[0]
                require(identifier not in skips, "duplicate skipped identity")
                skips.add(identifier)
            elif line.startswith("PROBLEM "):
                problems.append(line[8:])
            else:
                require(re.fullmatch(r"Correspondence: \d+ matched tests, \d+ skipped numbered tests, \d+ problems\.",
                                     line), f"unexpected correlation output: {line}")
        require(matches == expected, f"{phase}: DejaGNU results differ from expected checks")
        require(skips == self.skipped, "correlation planning gaps differ")
        remaining = set(failure_pairs)
        for problem in problems:
            pair = next((pair for pair in remaining
                         if problem.startswith(f"{pair[0]}: {pair[1]} reported FAIL: ")), None)
            require(pair is not None, f"structural or unexpected correlation problem: {problem}")
            remaining.remove(pair)
        require(not remaining, "expected correlation failure was not reported")
        planned_counts = Counter(verdict for checks in matches.values() for _, verdict in checks)
        extra = {key: dispositions[key] - planned_counts[key]
                 for key in dispositions.keys() | planned_counts.keys()}
        require(all(count >= 0 and (key == "PASS" or count == 0) for key, count in extra.items()),
                "an unplanned legacy check failed or a planned check was absent")
        extra = {key: count for key, count in extra.items() if count}
        require(extra.get("PASS", 0) > 0, "expected additional legacy checks")
        if phase != "original":
            require(extra == self.summary["phases"]["original"]["dejagnu"]["extra_dispositions"],
                    "additional legacy check population changed")
        return {"exit_codes": statuses, "dispositions": dict(dispositions),
                "extra_dispositions": extra, "checks": matches, "correlate_exit": correlation.returncode,
                "expected_problems": problems}

    def execute(self):
        try:
            self.prepare()
            population = len(self.targets)
            for phase, data, reruns in [
                    ("original", self.original, {population}),
                    ("wrong-diagnostic", SYNTAX_ERROR + self.original, {population}),
                    ("unexpected-success", SUCCESSFUL_SOURCE, {population}),
                    ("restored", self.original, {0, population})]:
                print(f"Starting {phase}", flush=True)
                self.set_sources(data)
                before = self.source_hashes(data)
                expected = self.expected_checks(phase)
                buck = self.buck_phase(phase, expected, reruns)
                legacy = self.legacy_phase(phase, expected)
                after = self.source_hashes(data)
                self.summary["phases"][phase] = {"source_sha256_before": before,
                                               "source_sha256_after": after,
                                               "buck2": buck, "dejagnu": legacy}
                if phase == "original":
                    print("Checking unchanged warm build", flush=True)
                    self.summary["warm_reuse"] = self.buck_phase("warm", expected, {0})
                save_json(self.output / "summary.json", self.summary)
                print(f"{phase}: {dict(Counter(verdict for checks in expected.values() for _, verdict in checks))}; "
                      f"Buck2 ran {buck['action_runs']} actions; extra legacy checks passed", flush=True)
            self.summary["success"] = True
        except BaseException as error:
            self.summary["error"] = f"{type(error).__name__}: {error}"
        finally:
            try:
                for path in (self.cell / "suite" / SOURCE, self.legacy / SOURCE):
                    if path.exists():
                        path.write_bytes(self.original)
                if (self.cell / "suite" / SOURCE).exists() and (self.legacy / SOURCE).exists():
                    self.summary["restored_source_sha256"] = self.source_hashes(self.original)
            except Exception as error:
                self.summary.update(success=False, restoration_error=str(error))
            if (self.cell / ".buckconfig").exists():
                try:
                    cleanup = self.run("cleanup-daemon", [self.buck2, "kill"],
                                       cwd=self.cell, expected=None)
                    self.summary["cleanup_exit"] = cleanup.returncode
                    if cleanup.returncode != 0:
                        self.summary["success"] = False
                except Exception as error:
                    self.summary.update(success=False, cleanup_error=str(error))
            save_json(self.output / "summary.json", self.summary)
        print(f"{'PASS' if self.summary['success'] else 'FAIL'}: {self.output / 'summary.json'}")
        if "error" in self.summary:
            print(self.summary["error"], file=sys.stderr)
        return 0 if self.summary["success"] else 1


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--planner", type=Path, required=True)
    parser.add_argument("--buck2", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--repo", type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument("--installation", type=Path)
    args = parser.parse_args()
    try:
        return DiagnosticExperiment(args).execute()
    except Exception as error:
        print(f"ERROR: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
