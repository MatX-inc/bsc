#!/usr/bin/env python3
"""Synthetic-only validation of timing reports and capture metadata/archival."""

import contextlib
from datetime import datetime, timedelta, timezone
import importlib.util
import io
import json
import os
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch


sys.dont_write_bytecode = True
SCRIPTS = Path(__file__).resolve().parents[1] / "scripts"
sys.path.insert(0, str(SCRIPTS))


def load(name):
    spec = importlib.util.spec_from_file_location(name, SCRIPTS / f"{name}.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


report = load("report-timings")
capture = load("capture-baselines")
boundaries = load("check-tcl-boundaries")


def write_json(path, value):
    path.write_text(json.dumps(value) + "\n")


class TimingTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="bsc-timing-test-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)

    def make_capture(self, name, mode, walls, first):
        path = self.root / name
        path.mkdir()
        data = {
            "base": "revision", "suite_root": str(self.root / "testsuite"),
            "command": ["make", "-j128", "-C", "testsuite", "fullparallel"],
            "runs": len(walls), "installation_sha256": "a" * 64,
            "testsuite_source_sha256": "b" * 64,
            "random_seed_environment": {"PERL_RAND_SEED": "42"},
            "perl_executable": {"path": "/usr/bin/perl", "resolved_path": "/usr/bin/perl",
                                "version": "v5.40.1", "sha256": "c" * 64},
            "inherited_tool_settings": {},
            "configuration": {key: "fixed" for key in report.CONFIG_KEYS},
        }
        data["configuration"]["DO_INTERNAL_CHECKS"] = str(mode)
        write_json(path / "configuration.json", data)
        for number, wall in enumerate(walls, 1):
            run = path / f"run-{number}"
            (run / "bsc.fixture").mkdir(parents=True)
            start = datetime(2026, 1, 1, tzinfo=timezone.utc) + timedelta(minutes=first + number)
            counts = dict.fromkeys(report.CATEGORIES, 0)
            counts.update(PASS=1, XFAIL=1)
            result = {
                "exit_code": 0, "counts": counts, "seconds": wall + 1,
                "suite_wall_seconds": wall, "started_at_utc": start.isoformat(),
                "finished_at_utc": (start + timedelta(seconds=wall)).isoformat(),
                "summary_files": 1, "timing_files": 1,
                "scheduled_discovery_matches_source": True,
                "installation_unchanged": True, "testsuite_source_unchanged": True,
                "testsuite_source_sha256_after": "b" * 64,
            }
            write_json(run / "result.json", result)
            for manifest in ("expected-tests.txt", "scheduled-tests.txt"):
                (run / manifest).write_text("bsc.fixture/test.exp\n")
            (run / "bsc.fixture/testrun.sum").write_text("PASS: example\nXFAIL: expected failure\n")
            timing = "Command exited with non-zero status 1\ncheck_bsc_compile, 1.00, 2.00, 3.00, /suite/a,b\n"
            if mode:
                timing += "check_dumpbo, 0.10, 0.20, 0.50, /suite/a,b\n"
            (run / "bsc.fixture/time.out").write_text(timing)
        return path

    def abba(self):
        return [self.make_capture("enabled-first", 1, [12], 0),
                self.make_capture("disabled", 0, [10, 10], 1),
                self.make_capture("enabled-last", 1, [14], 3)]

    def modify(self, path, change):
        value = json.loads(path.read_text())
        change(value)
        write_json(path, value)

    def test_clean_abba_aggregates_wall_and_categories_separately(self):
        paths = self.abba()
        value = report.build_report(paths, comparison=True)
        comparison = value["internal_check_comparison"]
        self.assertEqual(len(value["runs"]), 4)
        self.assertEqual(comparison["suite_wall_seconds"], {
            "enabled_mean": 13.0, "disabled_mean": 10.0,
            "enabled_minus_disabled": 3.0, "percent_change": 30.0})
        self.assertEqual(comparison["totals"]["elapsed_seconds_sum"]["enabled_mean"], 3.5)
        self.assertEqual(comparison["categories"]["check_dumpbo"]["count"]["disabled_mean"], 0)
        self.assertIsNone(comparison["categories"]["check_dumpbo"]["count"]["percent_change"])
        self.assertEqual(value["runs"][0]["command_status_notes"], 1)
        single = report.build_report([paths[0] / "run-1/result.json"])
        self.assertEqual(single["runs"][0]["totals"]["system_seconds"], 1.1)

    def test_metadata_mismatches_are_rejected(self):
        paths = self.abba()
        path = paths[2] / "configuration.json"
        original = path.read_text()
        for key, value in (("installation_sha256", "d" * 64),
                           ("testsuite_source_sha256", "e" * 64),
                           ("random_seed_environment", {"PERL_RAND_SEED": "43"}),
                           ("inherited_tool_settings", {"CXXFLAGS": "-O0"}),
                           ("extra_future_setting", "must compare")):
            with self.subTest(key=key):
                self.modify(path, lambda data: data.update({key: value}))
                with self.assertRaisesRegex(ValueError, "metadata differs"):
                    report.build_report(paths, comparison=True)
                path.write_text(original)

    def test_failed_incomplete_or_inconsistent_results_are_rejected(self):
        path = self.make_capture("single", 1, [12], 0)
        result = path / "run-1/result.json"
        original = result.read_text()
        changes = [lambda data: data.update(exit_code=1),
                   lambda data: data.update(installation_unchanged=False),
                   lambda data: data.update(testsuite_source_unchanged=False),
                   lambda data: data.update(testsuite_source_sha256_after="d" * 64),
                   lambda data: data.update(scheduled_discovery_matches_source=False),
                   lambda data: data["counts"].update(FAIL=1),
                   lambda data: data["counts"].update(PASS=2),
                   lambda data: data.update(summary_files=2),
                   lambda data: data.update(timing_files=2),
                   lambda data: data.update(exit_code=False),
                   lambda data: data.update(suite_wall_seconds=float("nan"))]
        for index, change in enumerate(changes):
            with self.subTest(index=index):
                self.modify(result, change)
                with self.assertRaises(ValueError):
                    report.build_report([path])
                result.write_text(original)
        result.unlink()
        with self.assertRaises(OSError):
            report.build_report([path])

    def test_manifest_and_archive_completeness(self):
        path = self.make_capture("single", 1, [12], 0)
        run = path / "run-1"
        for manifest in ("expected-tests.txt", "scheduled-tests.txt"):
            (run / manifest).write_text("bsc.fixture/test.exp\nbsc.missing/test.exp\n")
        with self.assertRaisesRegex(ValueError, "scheduled test directories"):
            report.build_report([path])
        self.modify(path / "configuration.json", lambda data: data.update(runs=2))
        with self.assertRaisesRegex(ValueError, "incomplete"):
            report.build_report([path])

    def test_capture_sidecar_files_are_not_run_directories(self):
        paths = self.abba()
        for path in paths:
            for run in path.glob("run-*"):
                (path / f"{run.name}.json").write_text("{}\n")
                (path / f"{run.name}-repeated-labels.json").write_text("{}\n")
        value = report.build_report(paths, comparison=True)
        self.assertEqual(len(value["runs"]), 4)
        self.assertEqual(value["internal_check_comparison"]["suite_wall_seconds"]["enabled_mean"], 13)

    def test_missing_and_extra_run_directories_remain_invalid(self):
        for kind in ("missing", "extra"):
            with self.subTest(kind=kind):
                path = self.make_capture(kind, 1, [12], 0)
                (path / "run-1.json").write_text("{}\n")
                if kind == "missing":
                    (path / "run-1").rename(path / "unrelated-directory")
                else:
                    (path / "run-2").mkdir()
                with self.assertRaisesRegex(ValueError, "incomplete or contains unexpected runs"):
                    report.build_report([path])

    def test_unsafe_json_and_invalid_timing_records(self):
        path = self.root / "record"
        for content in ('{"runs":1,"runs":2}', '{"value":Infinity}'):
            path.write_text(content)
            with self.assertRaises(ValueError):
                report.load_json(path)
        records = ["check_x, -1, 2, 3, /dir\n", "check_x, NaN, 2, 3, /dir\n",
                   "check_x, 1, 2, 3, relative\n", "check_x, 1, 2\n",
                   "unknown message\n", "Command terminated by signal 11\n"]
        for content in records:
            with self.subTest(content=content):
                path.write_text(content)
                with self.assertRaises(ValueError):
                    report.parse_timing(path)
        path.write_text("Command terminated by signal 11\ncheck_expected, 0, 0, 0.1, /dir\n")
        self.assertEqual(report.parse_timing(path)[1], 1)

    def test_comparison_rejects_duplicate_order_overlap_and_unfixed_seed(self):
        paths = self.abba()
        with self.assertRaisesRegex(ValueError, "duplicate"):
            report.build_report([paths[0], paths[0]])
        with self.assertRaisesRegex(ValueError, "1,0,0,1"):
            report.build_report([paths[1], paths[0], paths[2]], comparison=True)
        with self.assertRaisesRegex(ValueError, "out of order"):
            report.build_report([paths[2], paths[1], paths[0]], comparison=True)
        for path in paths:
            self.modify(path / "configuration.json",
                        lambda data: data["random_seed_environment"].update(PERL_RAND_SEED=None))
        with self.assertRaisesRegex(ValueError, "fixed numeric"):
            report.build_report(paths, comparison=True)

    def test_cli_refuses_to_overwrite_report(self):
        source = self.make_capture("single", 1, [12], 0)
        output = self.root / "report.json"
        args = ["report-timings", str(source), "--output", str(output)]
        with patch.object(sys, "argv", args):
            self.assertEqual(report.main(), 0)
        original = output.read_text()
        with patch.object(sys, "argv", args), contextlib.redirect_stderr(io.StringIO()):
            with self.assertRaises(SystemExit) as result:
                report.main()
        self.assertEqual(result.exception.code, 1)
        self.assertEqual(output.read_text(), original)

    def test_source_digest_tracks_inputs_but_ignores_mtime_and_planner(self):
        suite = self.root / "testsuite"
        suite.mkdir()
        source = suite / "input.exp"
        source.write_text("original\n")
        (suite / "link.exp").symlink_to("input.exp")
        (suite / "planner").mkdir()
        planner = suite / "planner/ignored"
        planner.write_text("one")
        tracked = b"testsuite/input.exp\0testsuite/link.exp\0testsuite/planner/ignored\0"
        with patch.object(capture, "ROOT", self.root), \
             patch.object(capture.subprocess, "check_output", return_value=tracked):
            baseline = capture.testsuite_source_digest()
            os.utime(source, (1, 1))
            planner.write_text("two")
            self.assertEqual(capture.testsuite_source_digest(), baseline)
            source.write_text("changed\n")
            self.assertNotEqual(capture.testsuite_source_digest(), baseline)
            source.write_text("original\n")
            source.chmod(source.stat().st_mode ^ 0o100)
            self.assertNotEqual(capture.testsuite_source_digest(), baseline)
            source.chmod(source.stat().st_mode ^ 0o100)
            (suite / "link.exp").unlink()
            (suite / "link.exp").symlink_to("different.exp")
            self.assertNotEqual(capture.testsuite_source_digest(), baseline)

    def test_capture_rejects_source_paths_and_symlink_escapes(self):
        suite = self.root / "testsuite"
        source = suite / "bsc.fixture"
        source.mkdir(parents=True)
        evidence = suite / ".stage1-validation"
        evidence.mkdir()
        (evidence / "source-link").symlink_to(source, target_is_directory=True)
        (self.root / "outside-link").symlink_to(source, target_is_directory=True)
        outputs = [source / "new", suite / "planner/new",
                   evidence / "source-link/new", self.root / "outside-link/new"]
        with patch.object(capture, "ROOT", self.root), patch.object(capture, "SUITE", suite), \
             patch.object(capture.CaptureSession, "start") as start:
            for output in outputs:
                with self.subTest(output=output), \
                     patch.object(sys, "argv", ["capture", "--output-dir", str(output)]), \
                     contextlib.redirect_stderr(io.StringIO()):
                    with self.assertRaises(SystemExit) as result:
                        capture.main()
                    self.assertEqual(result.exception.code, 2)
                    self.assertFalse(output.exists())
            start.assert_not_called()

    def test_boundary_output_paths_allow_only_dedicated_hidden_evidence_inside_suite(self):
        suite = self.root / "testsuite"
        source = suite / "bsc.fixture"
        source.mkdir(parents=True)
        evidence = suite / ".stage1-validation"
        evidence.mkdir()
        (evidence / "source-link").symlink_to(source, target_is_directory=True)
        for output, allowed in [(evidence / "boundaries", True),
                                (self.root / "outside-boundaries", True),
                                (source / "new", False),
                                (suite / "planner/new", False),
                                (evidence / "source-link/new", False)]:
            with self.subTest(output=output), patch.object(boundaries, "compare", return_value=0) as compare, \
                 patch.object(sys, "argv", ["boundaries", str(self.root), "--output-dir", str(output)]), \
                 contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
                if allowed:
                    self.assertEqual(boundaries.main(), 0)
                    compare.assert_called_once_with(self.root.resolve(), output.resolve(), "ghc")
                    self.assertTrue(output.is_dir())
                else:
                    with self.assertRaises(SystemExit) as result:
                        boundaries.main()
                    self.assertEqual(result.exception.code, 2)
                    compare.assert_not_called()
                    self.assertFalse(output.exists())

    def test_capture_archives_timings_and_rejects_source_drift(self):
        suite = self.root / "testsuite"
        fixture = suite / "bsc.fixture"
        fixture.mkdir(parents=True)
        (fixture / "test.exp").write_text("# never sourced\n")
        (fixture / "testrun.sum").write_text("PASS: fixture\n")
        timing = "check_bsc_compile, 0.1, 0.2, 0.3, /suite/bsc.fixture\n"
        (fixture / "time.out").write_text(timing)
        (suite / "all_tests.mk").write_text("ALL_TESTS := ./bsc.fixture/test.exp\n")
        (self.root / "inst/bin").mkdir(parents=True)
        (self.root / "inst/bin/bsc").write_text("not executed")

        class FakeProcess:
            returncode = 0

            def __init__(self, *args, **kwargs):
                pass

            def wait(self, timeout=None):
                return 0

        with patch.object(capture, "ROOT", self.root), patch.object(capture, "SUITE", suite), \
             patch.object(capture, "installation_digest", return_value="a" * 64), \
             patch.object(capture.subprocess, "check_output", return_value="fixed"), \
             patch.object(capture.CaptureSession, "start", side_effect=FakeProcess):
            for kind, changed in (("default", False), ("hidden", False), ("outside", True)):
                output = (suite / ".stage1-validation/baselines" if kind == "default" else
                          suite / ".stage1-validation/explicit" if kind == "hidden" else
                          self.root / "outside")
                args = ["capture", "--internal-checks", "0", "--runs", "1"]
                if kind != "default":
                    args.extend(["--output-dir", str(output)])
                digests = ["b" * 64, ("c" if changed else "b") * 64]
                with patch.object(sys, "argv", args), \
                     patch.object(capture, "testsuite_source_digest", side_effect=digests), \
                     contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
                    self.assertEqual(capture.main(), int(changed))
                result = json.loads((output / "run-1/result.json").read_text())
                metadata = json.loads((output / "configuration.json").read_text())
                self.assertEqual(metadata["testsuite_source_sha256"], "b" * 64)
                self.assertEqual(result["testsuite_source_unchanged"], not changed)
                self.assertEqual(result["timing_files"], 1)
                self.assertGreater(result["suite_wall_seconds"], 0)
                self.assertLessEqual(result["suite_wall_seconds"], result["seconds"])
                self.assertEqual((output / "run-1/bsc.fixture/time.out").read_text(), timing)


if __name__ == "__main__":
    unittest.main()
