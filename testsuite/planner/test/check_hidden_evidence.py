#!/usr/bin/env python3
"""Exercise legacy suite traversal against hidden archives in a temporary tree."""

import os
from pathlib import Path
import re
import shutil
import subprocess
import tarfile
import tempfile
import unittest


REPO = Path(__file__).resolve().parents[3]
SOURCE = REPO / "testsuite"


class HiddenEvidenceTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="bsc-hidden-evidence-")
        self.addCleanup(self.temporary.cleanup)
        self.repo = Path(self.temporary.name)
        self.suite = self.repo / "testsuite"
        self.suite.mkdir()
        for name in ("Makefile", "suitemake.mk", "norealclean.mk", "cleanonly.mk",
                     "clean.mk", "realclean.mk", "test_list.sh", "archive_logs.sh",
                     "findfailures.csh"):
            shutil.copy2(SOURCE / name, self.suite / name)
        shutil.copytree(SOURCE / "scripts", self.suite / "scripts")
        (self.suite / "config").mkdir()
        self.active = [self.suite / "bsc.active", self.suite / "bsc.active/nested"]
        self.hidden = [self.suite / ".stage1-validation/deep/deep/deep/bsc.archived",
                       self.suite / ".other/bsc.archived"]
        for index, directory in enumerate(self.active + self.hidden):
            directory.mkdir(parents=True, exist_ok=True)
            hidden = directory in self.hidden
            marker = "hidden_archive" if hidden else "active_" + str(index)
            (directory / "fixture.exp").write_text('error "fixture must never run"\n')
            (directory / "time.out").write_text(
                f"check_{marker}, 1, 2, 3, {directory}\n")
            verdict = "FAIL" if hidden or index == 0 else "PASS"
            count = "unexpected failures" if verdict == "FAIL" else "expected passes"
            (directory / "testrun.sum").write_text(
                f"Running {marker}.exp ...\n{verdict}: {marker}\n"
                f"=== bsc Summary ===\n# of {count} 1\n")
            (directory / "bsc.log").write_text(marker + "\n")
            (directory / "artifact.bo").write_text(marker + "\n")
            (directory / "backup~").write_text(marker + "\n")
            (directory / "core").write_text(marker + "\n")
            relative = os.path.relpath(self.suite, directory)
            confdir = "BROKEN_ARCHIVE_PATH" if hidden else f"$(realpath {relative})"
            (directory / "Makefile").write_text(
                f"CONFDIR = {confdir}\ninclude $(CONFDIR)/clean.mk\n")
        (self.suite / "timing.txt").write_text("10 bsc.active\n5 bsc.active/nested\n")
        # Avoid regenerating the unrelated group inventory during make checks.
        (self.suite / "groups.list").write_text("# fixture group inventory\n")
        self.environment = dict(os.environ, RUN_TESTCASES_IN_ORDER_OF_TIME="",
                                SKIP_SLOWEST_TESTCASES="")

    def run_command(self, command, *, cwd=None, environment=None, input=None):
        result = subprocess.run(command, cwd=cwd or self.suite,
                                env=environment or self.environment, input=input,
                                text=True, stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT, timeout=30)
        self.assertEqual(result.returncode, 0, result.stdout)
        return result.stdout

    def make(self, target):
        # Every make invocation, including realclean, is confined to the fixture.
        self.assertTrue(self.suite.is_relative_to(Path(self.temporary.name)))
        return self.run_command(["make", "--no-print-directory", target,
                                 "TEST_OSTYPE=Linux", "TEST_MACHTYPE=x86_64",
                                 f"TEST_BSDIR={self.suite}"])

    def test_discovery(self):
        expected = {"./bsc.active/fixture.exp", "./bsc.active/nested/fixture.exp"}
        for command in (["perl", "scripts/sort-by-time.pl"],
                        ["perl", "scripts/sort-by-time.pl", "bsc"],
                        ["bash", "scripts/tool-find.sh"],
                        ["bash", "scripts/tool-find.sh", "bsc"]):
            with self.subTest(command=command):
                self.assertEqual(set(self.run_command(command).splitlines()), expected)
        timed_environment = dict(self.environment, RUN_TESTCASES_IN_ORDER_OF_TIME="1")
        output = self.run_command(["perl", "scripts/sort-by-time.pl", "bsc"],
                                  environment=timed_environment)
        self.assertEqual(set(output.splitlines()), expected)
        self.make("run-tests-setup")
        self.assertEqual(set((self.suite / "all_tests.mk").read_text().split()[2:]),
                         expected)

    def test_statistics_and_failure_report(self):
        output = self.make("generate-stats")
        self.assertNotIn("hidden_archive", output)
        self.assertNotIn(".stage1-validation", output)
        self.assertNotIn(".other", output)
        for marker in ("active_0", "active_1"):
            self.assertIn(marker, output)
        self.assertRegex(output, r"# of expected passes\s+1\b")
        self.assertRegex(output, r"# of unexpected failures\s+1\b")
        failures = self.run_command(["csh", "findfailures.csh"])
        self.assertEqual(failures.splitlines(), ["./bsc.active/testrun.sum"])

    def test_nonparallel_timing_walk(self):
        # Load only the actual procedure: sourcing unix.exp starts the harness.
        source = (SOURCE / "config/unix.exp").read_text()
        match = re.search(r"^proc time_walk \{\} \{.*?^\}", source, re.M | re.S)
        self.assertIsNotNone(match)
        script = ("proc verbose {args} {puts $args}\n"
                  "proc time_file {} {return time.out}\n"
                  "set TEST_CONFIG_DIR [file join [pwd] config]\n"
                  "set LOCAL_TIME_WALK 0\n" + match.group() + "\ntime_walk\n")
        output = self.run_command(["tclsh"], input=script)
        self.assertNotIn("hidden_archive", output)
        self.assertIn("active_0", output)
        self.assertIn("active_1", output)

    def test_archive_from_repository_and_suite(self):
        for cwd in (self.repo, self.suite):
            with self.subTest(cwd=cwd):
                self.run_command(["sh", str(self.suite / "archive_logs.sh")], cwd=cwd)
                with tarfile.open(cwd / "logs.tar.gz") as archive:
                    names = set(archive.getnames())
                prefix = "./testsuite/" if cwd == self.repo else "./"
                self.assertEqual(names, {prefix + "bsc.active/bsc.log",
                                         prefix + "bsc.active/nested/bsc.log"})

    def test_ci_lint_ignores_archived_makefiles(self):
        for script in ("check_confdir.py", "check_symlinks.py"):
            with self.subTest(script=script):
                self.run_command(["python3", str(REPO / ".github/workflows" / script)])

    def test_realclean_preserves_hidden_evidence(self):
        before = {path: path.read_bytes() for directory in self.hidden
                  for path in directory.rglob("*") if path.is_file()}
        self.make("realclean")
        for path, content in before.items():
            self.assertTrue(path.is_file(), str(path))
            self.assertEqual(path.read_bytes(), content)
        for directory in self.active:
            for name in ("testrun.sum", "time.out", "artifact.bo", "backup~", "core"):
                self.assertFalse((directory / name).exists(), str(directory / name))
            self.assertTrue((directory / "fixture.exp").is_file())


if __name__ == "__main__":
    unittest.main(verbosity=2)
