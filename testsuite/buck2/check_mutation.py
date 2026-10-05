#!/usr/bin/env python3
"""Run an isolated b1213 PASS -> FAIL -> PASS DejaGNU/Buck2 negative control."""

import argparse
from collections import Counter
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import time

TEST = "bsc.bugs/bluespec_inc/b1213/b1213.exp"
SOURCE = Path(TEST).parent / "Example.bsv"
MUTATION = b"*** deliberate b1213 syntax error for the mutation control ***\n"


def require(condition, message):
    if not condition:
        raise RuntimeError(message)


def digest(data):
    return hashlib.sha256(data).hexdigest()


def save_json(path, data):
    path.write_text(json.dumps(data, indent=2, sort_keys=True) + "\n")


class Experiment:
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
        (self.output / "Example.bsv.original").write_bytes(self.original)
        self.summary = {"test": TEST, "source": str(SOURCE),
                        "original_sha256": digest(self.original),
                        "mutated_sha256": digest(MUTATION + self.original),
                        "phases": {}, "success": False}
        self.daemon = None

    def run(self, name, argv, cwd=None, environment=None, expected=0):
        path = self.output / name
        path.parent.mkdir(parents=True, exist_ok=True)
        overrides = environment or {}
        env = dict(os.environ)
        env.update(overrides)
        started = time.monotonic()
        run = subprocess.run([str(x) for x in argv], cwd=cwd or self.repo,
                             env=env, capture_output=True, text=True, errors="replace")
        path.with_suffix(".stdout").write_text(run.stdout)
        path.with_suffix(".stderr").write_text(run.stderr)
        save_json(path.with_suffix(".command.json"), {
            "argv": [str(x) for x in argv], "cwd": str(cwd or self.repo),
            "environment_overrides": overrides, "exit_code": run.returncode,
            "seconds": time.monotonic() - started})
        if expected is not None:
            require(run.returncode == expected,
                    f"{name}: exit {run.returncode}, expected {expected}; see saved output")
        return run

    def tracked(self, *paths):
        output = subprocess.check_output(
            ["git", "-C", str(self.repo), "ls-files", "-z", "--", *paths])
        return [Path(os.fsdecode(path)) for path in output.split(b"\0") if path]

    def copy_legacy_inputs(self):
        inputs = self.tracked("testsuite/config", "testsuite/lib",
                              "testsuite/scripts/collapse.pl", "testsuite/site.exp")
        for path in inputs:
            target = self.legacy / path.relative_to("testsuite")
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(self.repo / path, target)
        self.test_inputs = self.tracked("testsuite/" + str(Path(TEST).parent))
        self.legacy_environment = {
            "PATH": str(self.cell / "installation/bin") + ":/usr/bin:/bin",
            "LANG": "C", "LC_ALL": "C", "MAKEFLAGS": "", "BSCTEST": "1",
            "BSC_OPTIONS": "", "BLUESPEC_LD_LIBRARY_PATH": "",
            "LD_LIBRARY_PATH": str(self.cell / "installation/lib/SAT"),
            "BSC_TEST_TRACE": "1", "DO_INTERNAL_CHECKS": "1",
            "VTEST": "1", "CTEST": "1", "SYSTEMCTEST": "1",
            "BSC_VERILOG_SIM": "iverilog",
            "TEST_CONFIG_DIR": str(self.legacy / "config"),
            "BSDIR": str(self.cell / "installation/lib"),
            "BLUESPECDIR": str(self.cell / "installation/lib"),
            "SYSTEMC_INC": "/usr/include", "SYSTEMC_LIB": "/usr/lib/x86_64-linux-gnu",
            "SYSTEMC_CXXFLAGS": "-std=c++17",
            "TEST_SYSTEMC_INC": "/usr/include", "TEST_SYSTEMC_LIB": "/usr/lib/x86_64-linux-gnu",
            "TEST_SYSTEMC_CXXFLAGS": "-std=c++17",
            "OSTYPE": subprocess.check_output([str(self.repo / "platform.sh"), "ostype"], text=True).strip(),
            "MACHTYPE": subprocess.check_output([str(self.repo / "platform.sh"), "machtype"], text=True).strip(),
        }
        for variable, tool in [("BSC", "bsc"), ("BLUETCL", "bluetcl"),
                               ("DUMPBO", "dumpbo"), ("DUMPBA", "dumpba"),
                               ("BSC2BSV", "bsc2bsv"), ("VCDCHECK", "vcdcheck"),
                               ("SHOWRULES", "showrules")]:
            self.legacy_environment[variable] = str(self.cell / "installation/bin" / tool)

    def set_sources(self, data):
        # Every legacy phase starts without compiler products from earlier phases.
        directory = self.legacy / Path(TEST).parent
        if directory.exists():
            shutil.rmtree(directory)
        for path in self.test_inputs:
            target = self.legacy / path.relative_to("testsuite")
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(self.repo / path, target)
        (self.legacy / SOURCE).write_bytes(data)
        (self.cell / "suite" / SOURCE).write_bytes(data)

    def source_hashes(self, expected):
        result = {"checkout": digest((self.repo / "testsuite" / SOURCE).read_bytes()),
                  "buck2": digest((self.cell / "suite" / SOURCE).read_bytes()),
                  "legacy": digest((self.legacy / SOURCE).read_bytes())}
        require(result["checkout"] == digest(self.original), "checkout source changed")
        require(result["buck2"] == result["legacy"] == digest(expected),
                "Buck2 and DejaGNU source bytes differ")
        return result

    def buck_phase(self, phase, expected, reruns):
        path = self.output / phase
        path.mkdir(exist_ok=True)
        report_path = path / "build.json"
        build = self.run(phase + "/build", [self.buck2, "build", "-j1", self.target,
                         "--build-report", report_path], cwd=self.cell)
        report = json.loads(report_path.read_text())
        require(report["success"] is True and len(report["results"]) == 1,
                "unexpected Buck2 result population")
        result = next(iter(report["results"].values()))
        outputs = result["outputs"]["DEFAULT"]
        require(len(outputs) == 1, "expected one result directory")
        result_dir = Path(outputs[0])
        if not result_dir.is_absolute():
            result_dir = self.cell / result_dir
        execution = json.loads((result_dir / "result.json").read_text())
        checks = [(check["role"], check["disposition"]) for check in execution["checks"]]
        require(checks == [("compilation", expected), ("object-load", expected)],
                f"{phase}: unexpected Buck2 checks {checks}")
        checker = self.run(phase + "/checker", [sys.executable,
                           self.repo / "testsuite/buck2/check_results.py", self.cell, report_path],
                           expected=0 if expected == "PASS" else 1)
        ran = self.run(phase + "/what-ran", [self.buck2, "log", "what-ran",
                       "--trace-id", report["trace_id"], "--format", "json",
                       "--filter-category", "bsc_test"], cwd=self.cell)
        actions = [json.loads(line) for line in ran.stdout.splitlines() if line]
        if reruns is not None:
            require(len(actions) == reruns, f"{phase}: expected {reruns} action runs, saw {len(actions)}")
        status = self.run(phase + "/daemon", [self.buck2, "status"], cwd=self.cell)
        status_json = json.loads(status.stdout)
        pid = status_json.get("process_info", {}).get("pid", status_json.get("pid"))
        require(type(pid) is int, "could not identify Buck2 daemon PID")
        if self.daemon is None:
            self.daemon = pid
        require(pid == self.daemon, "Buck2 daemon changed during the experiment")
        shutil.copytree(result_dir, path / "buck-result")
        return {"build_exit": build.returncode, "checker_exit": checker.returncode,
                "checks": checks, "action_runs": len(actions), "daemon_pid": pid,
                "trace_id": report["trace_id"]}

    def legacy_phase(self, phase, expected):
        directory = self.legacy / Path(TEST).parent
        run = self.run(phase + "/dejagnu", ["runtest", "--tool", "", "--objdir", ".",
                       "--status", Path(TEST).name], cwd=directory,
                       environment=self.legacy_environment,
                       expected=0 if expected == "PASS" else 1)
        log = (directory / "testrun.log").read_text()
        summary = (directory / "testrun.sum").read_text()
        dispositions = re.findall(r"^(PASS|FAIL|XFAIL|XPASS|KFAIL|KPASS|UNRESOLVED|UNTESTED|UNSUPPORTED):",
                                  summary, re.MULTILINE)
        require(dispositions == [expected, expected],
                f"{phase}: unexpected DejaGNU checks {dispositions}")
        require(not re.search(r"^ERROR:", summary, re.MULTILINE), "DejaGNU infrastructure error")
        require(f"BSC-TEST: script 1 {TEST} 1" in log
                and "BSC-TEST: role 1 object-load" in log
                and "BSC-TEST: finish 1" in log, "missing original harness trace markers")
        shutil.copytree(directory, self.output / phase / "legacy-result")
        return {"exit_code": run.returncode, "dispositions": dict(Counter(dispositions))}

    def execute(self):
        try:
            self.summary["planner_sha256"] = digest(self.planner.read_bytes())
            self.summary["buck2_sha256"] = digest(self.buck2.read_bytes())
            self.run("buck2-version", [self.buck2, "--version"])
            plan = self.run("plan", [self.planner, "plan", "--config", "b1213-mutation-control",
                            "--suite-root", self.repo / "testsuite", "--internal-checks", "1",
                            self.repo / "testsuite" / TEST])
            plan_path = self.output / "plan.json"
            plan_path.write_text(plan.stdout)
            self.run("emit", [self.planner, "emit-buck2", plan_path,
                     "--suite-root", self.repo / "testsuite", "--installation", self.installation,
                     "--output", self.cell])
            manifest = json.loads((self.cell / "targets.json").read_text())
            require(len(manifest["targets"]) == 1 and not manifest["execution_gaps"],
                    "expected one executable b1213 target")
            self.target = manifest["targets"][0]["target"]
            self.copy_legacy_inputs()
            for phase, data, expected, reruns in [
                    ("original", self.original, "PASS", 1),
                    ("mutated", MUTATION + self.original, "FAIL", 1),
                    ("restored", self.original, "PASS", None)]:
                print(f"Starting {phase}: expect two {expected} results", flush=True)
                self.set_sources(data)
                before = self.source_hashes(data)
                buck = self.buck_phase(phase, expected, reruns)
                legacy = self.legacy_phase(phase, expected)
                after = self.source_hashes(data)
                self.summary["phases"][phase] = {
                    "source_sha256_before": before, "source_sha256_after": after,
                    "buck2": buck, "dejagnu": legacy}
                if phase == "original":
                    print("Checking unchanged warm build", flush=True)
                    self.summary["warm_reuse"] = self.buck_phase("warm", "PASS", 0)
                save_json(self.output / "summary.json", self.summary)
            self.summary["success"] = True
        except BaseException as error:
            self.summary["error"] = f"{type(error).__name__}: {error}"
        finally:
            for path in (self.cell / "suite" / SOURCE, self.legacy / SOURCE):
                if path.exists():
                    path.write_bytes(self.original)
            if (self.cell / "suite" / SOURCE).exists() and (self.legacy / SOURCE).exists():
                self.summary["restored_source_sha256"] = self.source_hashes(self.original)
            if (self.cell / ".buckconfig").exists():
                self.run("cleanup-daemon", [self.buck2, "kill"], cwd=self.cell, expected=None)
            save_json(self.output / "summary.json", self.summary)
        print(json.dumps(self.summary, indent=2, sort_keys=True))
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
        return Experiment(args).execute()
    except Exception as error:
        print(f"ERROR: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
