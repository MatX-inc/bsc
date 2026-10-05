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

    def compilation_error(self, expected_count=1, actual_count=1, transcript=None):
        self.plan["scripts"][0]["items"][1]["kind"] = {
            "kind": "compilation-error", "source": "Example.bs", "options": [],
            "compile_dependencies": True, "error_tag": "T0001", "error_count": expected_count,
        }
        check = self.check("diagnostic-count", 1)
        check["process"]["program"] = "bsc"
        check["diagnostic"] = {"error_tag": "T0001", "expected_count": expected_count,
                               "actual_count": actual_count}
        check["disposition"] = "PASS" if expected_count == actual_count else "FAIL"
        self.reports[2]["checks"] = [check]
        if transcript is None:
            transcript = "Error: expected diagnostic (T0001)\n" * actual_count
        (self.cell / "buck-out/test_2/result/Example.bs.bsc-out").write_bytes(
            transcript.encode("utf-8"))
        return check

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

    def test_diagnostic_count_matches_transcript_with_tcl_line_semantics(self):
        self.compilation_error(expected_count=3, actual_count=3, transcript=(
            "prefix Error: first (T0001)\r\n"
            "Error: second (T0001)\r"
            "Error: first (T0001) Error: third (T0001)\n"
            "Error:(T0001)\n"
            "Error: wrong tag (T0002)\n"
            "Error: trailing text (T0001) ignored\n"
            "Error: not the same line\n(T0001)\n"))
        self.assertIn("PASS 3; FAIL 0", self.run_checker(0))

    def test_zero_diagnostics_can_match_expected_zero(self):
        self.compilation_error(expected_count=0, actual_count=0,
                               transcript="Error: another tag (T0002)\n")
        self.assertIn("PASS 3; FAIL 0", self.run_checker(0))

    def test_exit_125_is_an_ordinary_diagnostic_failure(self):
        check = self.compilation_error()
        check["process"]["termination"]["exit_code"] = 125
        self.assertIn("PASS 3; FAIL 0", self.run_checker(0))

    def test_diagnostic_count_preserves_non_utf8_transcript_bytes(self):
        self.compilation_error()
        (self.cell / "buck-out/test_2/result/Example.bs.bsc-out").write_bytes(
            b"Error: echoed source \xff (T0001)\r\n")
        self.assertIn("PASS 3; FAIL 0", self.run_checker(0))

    def test_diagnostic_count_mismatch_is_an_ordinary_failure(self):
        self.compilation_error(expected_count=2)
        self.assertIn("FAIL 1", self.run_checker(1))

    def test_diagnostic_pass_cannot_hide_count_mismatch(self):
        check = self.compilation_error(expected_count=2)
        check["disposition"] = "PASS"
        self.assertIn("contradicts", self.run_checker(1))

    def test_diagnostic_count_cannot_disagree_with_saved_transcript(self):
        self.compilation_error(transcript="Error: different diagnostic (T0002)\n")
        self.assertIn("differs from saved transcript", self.run_checker(1))

    def test_diagnostic_metadata_must_match_plan(self):
        check = self.compilation_error()
        for field, value in [("error_tag", "T0002"), ("expected_count", 2),
                             ("expected_count", True)]:
            with self.subTest(field=field, value=value):
                original = check["diagnostic"][field]
                check["diagnostic"][field] = value
                self.assertIn("expectation differs", self.run_checker(1))
                check["diagnostic"][field] = original

    def test_invalid_actual_diagnostic_counts_are_rejected(self):
        check = self.compilation_error()
        for value in [-1, True, "1", 1.0]:
            with self.subTest(value=value):
                check["diagnostic"]["actual_count"] = value
                self.assertIn("invalid actual diagnostic count", self.run_checker(1))

    def test_diagnostic_and_check_fields_must_be_exact(self):
        check = self.compilation_error()
        original = copy.deepcopy(check)
        mutations = [
            lambda: check.pop("diagnostic"),
            lambda: check.update(extra=True),
            lambda: check["diagnostic"].pop("actual_count"),
            lambda: check["diagnostic"].update(extra=True),
        ]
        for mutate in mutations:
            with self.subTest(mutation=mutate):
                check.clear()
                check.update(copy.deepcopy(original))
                mutate()
                self.assertIn("malformed", self.run_checker(1))

    def test_diagnostic_transcript_must_exist_inside_result(self):
        check = self.compilation_error()
        for transcript, message in [("missing.out", "missing.out"),
                                    ("../escape.out", "escapes result directory"),
                                    (str(self.cell / "outside.out"), "output-relative")]:
            with self.subTest(transcript=transcript):
                check["process"]["transcript"] = transcript
                self.assertIn(message, self.run_checker(1))

    def test_diagnostic_failure_has_no_compilation_or_internal_role(self):
        check = self.compilation_error()
        original = copy.deepcopy(check)
        self.reports[2]["checks"].append(self.check("object-load", 0))
        self.assertIn("roles differ", self.run_checker(1))
        self.reports[2]["checks"] = [self.check("compilation", 1)]
        self.assertIn("roles differ", self.run_checker(1))
        self.reports[2]["checks"] = [original, copy.deepcopy(original)]
        self.assertIn("roles differ", self.run_checker(1))

    def test_diagnostic_test_unexpected_compilation_success_runs_internal_check(self):
        self.compilation_error()
        compile_check = self.check("compilation", 0)
        compile_check["disposition"] = "FAIL"
        self.reports[2]["checks"] = [compile_check, self.check("object-load", 0)]
        self.assertIn("PASS 3; FAIL 1", self.run_checker(1))
        compile_check["disposition"] = "PASS"
        self.assertIn("contradicts", self.run_checker(1))
        compile_check["disposition"] = "FAIL"
        self.reports[2]["checks"].pop()
        self.assertIn("roles differ", self.run_checker(1))

    def test_diagnostic_test_unexpected_success_without_internal_checks(self):
        self.compilation_error()
        self.config["internal_checks"] = False
        for report in self.reports.values():
            report["configuration"]["internal_checks"] = False
        self.reports[1]["checks"].pop()
        self.reports[2]["checks"] = [self.check("compilation", 0)]
        self.reports[2]["checks"][0]["disposition"] = "FAIL"
        self.assertIn("PASS 1; FAIL 1", self.run_checker(1))

    def test_diagnostic_test_success_cannot_claim_a_diagnostic_match(self):
        check = self.compilation_error()
        check["process"]["termination"]["exit_code"] = 0
        self.assertIn("roles differ", self.run_checker(1))

    def test_diagnostic_test_infrastructure_has_only_compilation_role(self):
        self.compilation_error()
        check = self.check("compilation", 126)
        check["disposition"] = "INFRASTRUCTURE_ERROR"
        self.reports[2]["checks"] = [check]
        for termination in [
                {"kind": "exited", "exit_code": 126},
                {"kind": "exited", "exit_code": 127},
                {"kind": "signal", "signal": 9, "exit_code": -9},
                {"kind": "timeout", "exit_code": -9},
                {"kind": "launch-error", "reason": "tool missing"}]:
            with self.subTest(termination=termination):
                check["process"]["termination"] = termination
                output = self.run_checker(1)
                self.assertIn("INFRASTRUCTURE_ERROR 1", output)
                self.assertNotIn("contradicts", output)
        self.reports[2]["checks"].append(self.check("object-load", 0))
        self.assertIn("roles differ", self.run_checker(1))

    def test_diagnostic_test_infrastructure_cannot_claim_a_diagnostic_match(self):
        check = self.compilation_error()
        check["process"]["termination"]["exit_code"] = 126
        self.assertIn("roles differ", self.run_checker(1))

    def test_invalid_diagnostic_plan_is_rejected(self):
        self.compilation_error()
        kind = self.plan["scripts"][0]["items"][1]["kind"]
        for field, value, message in [
                ("error_tag", "", "invalid compilation-error tag"),
                ("error_tag", "T.*", "invalid compilation-error tag"),
                ("error_tag", "1TAG", "invalid compilation-error tag"),
                ("error_count", -1, "invalid compilation-error count"),
                ("error_count", True, "invalid compilation-error count"),
                ("error_count", sys.maxsize + 1, "invalid compilation-error count"),
                ("compile_dependencies", "yes", "invalid compilation-error inputs")]:
            with self.subTest(field=field, value=value):
                original = kind[field]
                kind[field] = value
                self.assertIn(message, self.run_checker(1))
                kind[field] = original


if __name__ == "__main__":
    unittest.main()
