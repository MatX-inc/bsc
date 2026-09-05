"""Tests of evidence reporting and fail-closed gate behavior, not TRS semantics."""

import importlib.util
import json
from pathlib import Path
import subprocess
import unittest
from unittest.mock import patch


SPEC = importlib.util.spec_from_file_location("p0_runner", Path(__file__).resolve().parents[1] / "runner.py")
runner = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(runner)


class RunnerTests(unittest.TestCase):
    def test_model_success_is_not_p0_success(self):
        evidence = {"model_checks_passed": True, "p0_pass": False, "compiler_derived": False, "rtl_oracle_compared": False}
        completed = subprocess.CompletedProcess([], 0, json.dumps(evidence), "")
        with patch.object(runner, "command", return_value=completed):
            report, code = runner.run("model")
        self.assertEqual(code, 0)
        self.assertEqual(report["p0_status"], "blocked/inconclusive")
        self.assertEqual(report["checks"]["model"]["status"], "model_checks_passed")

    def test_nonzero_model_is_failure_even_with_valid_json(self):
        evidence = {"model_checks_passed": True, "p0_pass": False, "compiler_derived": False, "rtl_oracle_compared": False}
        completed = subprocess.CompletedProcess([], 1, json.dumps(evidence), "failed")
        with patch.object(runner, "command", return_value=completed):
            report, code = runner.run("model")
        self.assertEqual(code, 3)
        self.assertEqual(report["checks"]["model"]["status"], "model_check_failed")

    def test_invalid_json_is_failure(self):
        completed = subprocess.CompletedProcess([], 0, "PASS", "")
        with patch.object(runner, "command", return_value=completed):
            _, code = runner.run("model")
        self.assertEqual(code, 3)

    def test_missing_scope_markers_are_failure(self):
        completed = subprocess.CompletedProcess([], 0, '{"model_checks_passed": true}', "")
        with patch.object(runner, "command", return_value=completed):
            _, code = runner.run("model")
        self.assertEqual(code, 3)

    def test_overclaimed_model_evidence_is_failure(self):
        for overclaim in ("p0_pass", "compiler_derived", "rtl_oracle_compared"):
            with self.subTest(overclaim=overclaim):
                evidence = {"model_checks_passed": True, "p0_pass": False, "compiler_derived": False, "rtl_oracle_compared": False}
                evidence[overclaim] = True
                completed = subprocess.CompletedProcess([], 0, json.dumps(evidence), "")
                with patch.object(runner, "command", return_value=completed):
                    _, code = runner.run("model")
                self.assertEqual(code, 3)

    def test_missing_model_is_failure(self):
        with patch.object(runner, "command", side_effect=FileNotFoundError("missing")):
            _, code = runner.run("model")
        self.assertEqual(code, 3)

    def test_optimized_model_cannot_report_success(self):
        completed = subprocess.run(
            [runner.sys.executable, "-O", str(runner.SPIKE / "experiments/local_protocol.py")],
            cwd=runner.REPO, text=True, capture_output=True, timeout=20, check=False,
        )
        self.assertNotEqual(completed.returncode, 0)
        self.assertIn("require Python assertions", completed.stderr)
        self.assertEqual(completed.stdout, "")

    def test_timeout_is_failure(self):
        with patch.object(runner, "command", side_effect=subprocess.TimeoutExpired("model", 20)):
            _, code = runner.run("model")
        self.assertEqual(code, 3)

    def test_missing_tools_are_not_a_pass(self):
        with patch.object(runner.shutil, "which", return_value=None):
            report, code = runner.run("environment")
        self.assertEqual(code, 2)
        self.assertEqual(len(report["checks"]["environment"]["missing_required_tools"]), len(runner.REQUIRED_TOOLS))

    def test_all_never_claims_p0_pass_even_if_checks_succeed(self):
        with patch.object(runner, "source_check", return_value=({"status": "ok"}, 0)), \
             patch.object(runner, "model_check", return_value=({"status": "ok"}, 0)), \
             patch.object(runner, "environment_check", return_value=({"status": "ok"}, 0)):
            report, code = runner.run("all")
        self.assertEqual(code, 2)
        self.assertEqual(report["p0_status"], "blocked/inconclusive")
        self.assertTrue(report["remaining_gates"])

    def test_all_preserves_check_failure(self):
        with patch.object(runner, "source_check", return_value=({"status": "bad"}, 3)), \
             patch.object(runner, "model_check", return_value=({"status": "ok"}, 0)), \
             patch.object(runner, "environment_check", return_value=({"status": "ok"}, 0)):
            _, code = runner.run("all")
        self.assertEqual(code, 3)

    def test_changed_source_fails(self):
        def fake(args):
            blob = ("b" if args[1] == "hash-object" else "a") * 40
            return subprocess.CompletedProcess(args, 0, blob + "\n", "")
        with patch.object(runner, "command", side_effect=fake):
            report, code = runner.run("source")
        self.assertEqual(code, 3)
        self.assertFalse(report["checks"]["source"]["checks"][0]["matches_pin"])

    def test_missing_reference_fails(self):
        def fake(args):
            if args[1] == "rev-parse":
                return subprocess.CompletedProcess(args, 128, "", "missing pin")
            return subprocess.CompletedProcess(args, 0, "a" * 40 + "\n", "")
        with patch.object(runner, "command", side_effect=fake):
            _, code = runner.run("source")
        self.assertEqual(code, 3)

    def test_unlisted_production_source_change_fails(self):
        def fake(args):
            return subprocess.CompletedProcess(args, 1 if args[1] == "diff" else 0, "a" * 40 + "\n", "")
        with patch.object(runner, "command", side_effect=fake):
            report, code = runner.run("source")
        self.assertEqual(code, 3)
        self.assertFalse(report["checks"]["source"]["compiler_subtrees_match_pin"])

    def test_missing_bvi_pin_fails(self):
        def fake(args):
            return subprocess.CompletedProcess(args, 1 if args[1] == "cat-file" else 0, "a" * 40 + "\n", "")
        with patch.object(runner, "command", side_effect=fake):
            _, code = runner.run("source")
        self.assertEqual(code, 3)


if __name__ == "__main__":
    unittest.main()
