#!/usr/bin/env python3
"""CLI regressions ensuring build success cannot hide missing or failed tests."""

import copy
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


CHECKER = Path(__file__).resolve().parents[2] / "buck2/check_results.py"


class ResultCheckerTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix="bsc-result-check-")
        self.addCleanup(self.tmp.cleanup)
        self.cell = Path(self.tmp.name)
        self.config = {"name": "fixture", "internal_checks": True, "compiler_options": []}
        self.test_path = "bsc.fixture/example.exp"
        self.plan = {
            "schema": "bsc-testsuite-test-plan", "version": 3,
            "identity": "file-test-number-v1", "configuration": self.config,
            "scripts": [{"path": self.test_path, "items": [
                {"status": "planned", "id": {"test": self.test_path, "number": n},
                 "kind": {"kind": "compilation", "expectation": expectation}}
                for n, expectation in [(1, "succeeds"), (2, "fails")]
            ] + [{"status": "unsupported", "id": None}]}],
        }
        self.manifest = {
            "schema": "bsc-buck2-targets", "version": 1,
            "planned": 2, "unsupported": 1, "unresolved": 0,
            "targets": [{"target": f"//:test_{n}",
                         "id": f"v3:{len(self.test_path)}:{self.test_path}:{n}"}
                        for n in (1, 2)], "execution_gaps": [],
        }
        self.build = {"success": True, "truncated": False, "failures": {},
                      "project_root": str(self.cell), "results": {}}
        self.reports = {}
        for n in (1, 2):
            destination = self.cell / f"buck-out/test_{n}/result"
            destination.mkdir(parents=True)
            self.build["results"][f"root//:test_{n}"] = {
                "success": "SUCCESS", "errors": [],
                "outputs": {"DEFAULT": [str(destination.relative_to(self.cell))]},
            }
            self.reports[n] = {
                "schema": "bsc-test-execution", "version": 1,
                "id": {"test": self.test_path, "number": n},
                "configuration": copy.deepcopy(self.config),
                "installation": "installation", "working_directory": "work/bsc.fixture",
                "staged_inputs": ["bsc.fixture/Example.bs"],
                "checks": [self.check("compilation", 0 if n == 1 else 1)]
                          + ([self.check("object-load", 0)] if n == 1 else []),
            }

    @staticmethod
    def check(role, code):
        return {"role": role, "disposition": "PASS", "process": {
            "program": "bsc" if role == "compilation" else "dumpbo",
            "arguments": ["Example.bs"], "transcript": "Example.bs.bsc-out",
            "termination": {"kind": "exited", "exit_code": code},
        }}

    def run_checker(self, expected):
        for name, data in [("plan.json", self.plan), ("targets.json", self.manifest),
                           ("build-report.json", self.build)]:
            (self.cell / name).write_text(json.dumps(data))
        for n, report in self.reports.items():
            (self.cell / f"buck-out/test_{n}/result/result.json").write_text(json.dumps(report))
        run = subprocess.run([sys.executable, str(CHECKER), str(self.cell),
                              str(self.cell / "build-report.json")],
                             text=True, capture_output=True)
        self.assertEqual(run.returncode, expected, run.stdout + run.stderr)
        return run.stdout + run.stderr

    def test_complete_pass_and_expected_compile_failure(self):
        self.assertIn("PASS 3; FAIL 0", self.run_checker(0))

    def test_successful_build_with_ordinary_failure_is_not_green(self):
        check = self.reports[1]["checks"][0]
        check["process"]["termination"]["exit_code"] = 1
        check["disposition"] = "FAIL"
        self.assertIn("FAIL 1", self.run_checker(1))

    def test_missing_target_is_not_green(self):
        del self.build["results"]["root//:test_2"]
        self.assertIn("missing target", self.run_checker(1))

    def test_truncated_build_report_is_not_green(self):
        self.build["truncated"] = True
        self.assertIn("truncated", self.run_checker(1))

    def test_missing_result_file_is_not_green(self):
        del self.reports[2]
        self.assertIn("result.json", self.run_checker(1))

    def test_manifest_cannot_silently_omit_a_test(self):
        self.manifest["targets"].pop()
        self.assertIn("omits planned tests", self.run_checker(1))

    def test_duplicate_identity_is_rejected(self):
        self.reports[2]["id"]["number"] = 1
        self.assertIn("identity differs", self.run_checker(1))

    def test_missing_or_duplicate_role_is_rejected(self):
        self.reports[1]["checks"].pop()
        self.assertIn("roles differ", self.run_checker(1))
        self.reports[1]["checks"].append(copy.deepcopy(self.reports[1]["checks"][0]))
        self.assertIn("roles differ", self.run_checker(1))

    def test_configuration_mismatch_is_rejected(self):
        self.reports[1]["configuration"]["internal_checks"] = False
        self.assertIn("configuration differs", self.run_checker(1))

    def test_pass_cannot_hide_process_failure(self):
        self.reports[1]["checks"][0]["process"]["termination"]["exit_code"] = 1
        self.assertIn("contradicts", self.run_checker(1))

    def test_signal_is_infrastructure_failure_for_negative_test(self):
        check = self.reports[2]["checks"][0]
        check["process"]["termination"] = {"kind": "signal", "signal": 9, "exit_code": -9}
        check["disposition"] = "INFRASTRUCTURE_ERROR"
        self.assertIn("INFRASTRUCTURE_ERROR 1", self.run_checker(1))

    def test_exit_127_is_infrastructure_failure_for_negative_test(self):
        check = self.reports[2]["checks"][0]
        check["process"]["termination"]["exit_code"] = 127
        check["disposition"] = "INFRASTRUCTURE_ERROR"
        output = self.run_checker(1)
        self.assertIn("INFRASTRUCTURE_ERROR 1", output)
        self.assertNotIn("contradicts", output)

    def test_malformed_result_is_not_green(self):
        del self.reports[1]["checks"][0]["process"]
        self.assertIn("malformed check", self.run_checker(1))

    def test_unexpected_target_is_rejected(self):
        self.build["results"]["root//:unexpected"] = copy.deepcopy(
            self.build["results"]["root//:test_1"])
        self.assertIn("unexpected build-report target", self.run_checker(1))


if __name__ == "__main__":
    unittest.main()
