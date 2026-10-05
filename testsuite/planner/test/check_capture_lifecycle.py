#!/usr/bin/env python3
"""Exercise only disposable Python processes and temporary checkout locks."""

import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import time
import unittest
from unittest.mock import patch


sys.dont_write_bytecode = True
SCRIPTS = Path(__file__).resolve().parents[1] / "scripts"
sys.path.insert(0, str(SCRIPTS))
from capture_lifecycle import CaptureSession, GROUP_ENV, GroupProcess, LOCK_ENV

BOOTSTRAP = f"""import sys
sys.dont_write_bytecode = True
sys.path.insert(0, {str(SCRIPTS)!r})
from capture_lifecycle import CaptureInterrupted, CaptureSession
"""


class LifecycleTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="bsc-lifecycle-test-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.environment = dict(os.environ, PYTHONDONTWRITEBYTECODE="1")
        self.environment.pop(LOCK_ENV, None)
        self.environment.pop(GROUP_ENV, None)

    def command(self, code, *args):
        return [sys.executable, "-c", BOOTSTRAP + code, *map(str, args)]

    def contender(self, **kwargs):
        return subprocess.run(self.command("with CaptureSession(sys.argv[1]): pass\n", self.root),
                              env=self.environment, capture_output=True, text=True, timeout=5, **kwargs)

    def wait_file(self, path, process):
        deadline = time.monotonic() + 5
        while time.monotonic() < deadline:
            if path.is_file():
                return path.read_text()
            if process.poll() is not None:
                self.fail(f"worker exited before readiness: {process.communicate()}")
            time.sleep(0.01)
        self.fail("disposable process did not become ready")

    def spawn_owner(self, code, *args):
        process = subprocess.Popen(self.command(code, *args), env=self.environment,
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                   text=True, start_new_session=True)
        self.addCleanup(self.stop_owner, process)
        return process

    def stop_owner(self, process):
        if process.poll() is None:
            process.terminate()
            try:
                process.communicate(timeout=10)
            except subprocess.TimeoutExpired:
                process.kill()
                process.communicate(timeout=5)
        else:
            process.communicate()

    def test_lock_exclusion_handoff_and_release(self):
        with CaptureSession(self.root) as session:
            self.assertTrue((self.root / "testsuite/.stage1-validation/capture.lock").is_file())
            self.assertFalse((self.root / ".stage1-validation").exists())
            self.assertIn("checkout lock", self.contender().stderr)
            nested = self.command("with CaptureSession(sys.argv[1]): pass\n", self.root)
            self.assertEqual(session.run(nested, env=self.environment), 0)
            # Closing the inherited descriptor must not unlock the parent's lease.
            self.assertIn("checkout lock", self.contender().stderr)
        self.assertEqual(self.contender().returncode, 0)

    def test_lock_rejects_hidden_directory_symlink_into_test_sources(self):
        suite = self.root / "testsuite"
        source = suite / "bsc.fixture"
        source.mkdir(parents=True)
        (suite / ".stage1-validation").symlink_to(source, target_is_directory=True)
        with self.assertRaisesRegex(RuntimeError, "ordinary test sources"):
            with CaptureSession(self.root):
                self.fail("source directory was accepted as evidence storage")
        self.assertFalse((source / "capture.lock").exists())

    def test_wrong_inherited_descriptor_is_rejected(self):
        with CaptureSession(self.root):
            pass
        other = self.root / "unrelated"
        with other.open("w") as stream:
            self.environment[LOCK_ENV] = str(stream.fileno())
            result = self.contender(pass_fds=(stream.fileno(),))
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("not this checkout's capture lock", result.stderr)

    def test_active_command_prevents_advancing_and_failures_propagate(self):
        with CaptureSession(self.root) as session:
            process = session.start(self.command("import time; time.sleep(0.15)\n"), env=self.environment)
            with self.assertRaisesRegex(RuntimeError, "previous process group"):
                session.start(self.command("pass\n"), env=self.environment)
            self.assertEqual(process.wait(), 0)
            with self.assertRaises(subprocess.CalledProcessError) as error:
                session.run(self.command("sys.exit(7)\n"), env=self.environment)
            self.assertEqual(error.exception.returncode, 7)

    def test_group_owner_does_not_finish_when_only_leader_exits(self):
        class Leader:
            args = ["disposable-leader"]
            returncode = 0

            def wait(self, timeout=None):
                return 0

        process = GroupProcess(Leader(), 123, owner=True)
        with patch("capture_lifecycle.group_exists", return_value=True):
            with self.assertRaises(subprocess.TimeoutExpired):
                process.wait(timeout=0)
        with patch("capture_lifecycle.group_exists", return_value=False):
            self.assertEqual(process.wait(timeout=0), 0)

    def test_persistent_zombie_like_group_has_bounded_failure(self):
        class Leader:
            args = ["already-exited-leader"]
            returncode = 0

            def poll(self):
                return 0

            def wait(self, timeout=None):
                return 0

        process = GroupProcess(Leader(), 123, owner=True)
        with patch("capture_lifecycle.group_exists", return_value=True), \
             patch("capture_lifecycle.GROUP_DRAIN_SECONDS", 0), \
             patch("capture_lifecycle.os.killpg"):
            with self.assertRaisesRegex(RuntimeError, "refusing to advance"):
                process.wait()
            with self.assertRaisesRegex(RuntimeError, "manual inspection"):
                process.terminate_group(grace=0)

    def test_sigterm_and_sigint_clean_nested_shared_group(self):
        leaf = "import time; time.sleep(60)"
        nested = BOOTSTRAP + """import json, os
from pathlib import Path
try:
    with CaptureSession(sys.argv[1], termination_grace=0.3) as session:
        child = session.start([sys.executable, '-c', sys.argv[3]])
        Path(sys.argv[2]).write_text(json.dumps([os.getpid(), child.child.pid, os.getpgrp(), child.group]))
        child.wait()
except CaptureInterrupted as error:
    sys.exit(128 + error.signum)
"""
        owner = """try:
    with CaptureSession(sys.argv[1], termination_grace=1) as session:
        session.run([sys.executable, '-c', sys.argv[3], sys.argv[1], sys.argv[2], sys.argv[4]])
except CaptureInterrupted as error:
    sys.exit(128 + error.signum)
"""
        for signum in (signal.SIGTERM, signal.SIGINT):
            with self.subTest(signal=signum):
                ready = self.root / f"ready-{signum}"
                process = self.spawn_owner(owner, self.root, ready, nested, leaf)
                nested_pid, leaf_pid, nested_group, leaf_group = json.loads(self.wait_file(ready, process))
                self.assertEqual(nested_pid, nested_group)
                self.assertEqual(nested_group, leaf_group)
                self.assertNotEqual(nested_group, os.getpgrp())
                self.assertIn("checkout lock", self.contender().stderr)
                process.send_signal(signum)
                stdout, stderr = process.communicate(timeout=10)
                self.assertEqual(process.returncode, 128 + signum, (stdout, stderr))
                for pid in (nested_pid, leaf_pid):
                    with self.assertRaises(ProcessLookupError):
                        os.kill(pid, 0)
                self.assertEqual(self.contender().returncode, 0)

    def test_sigterm_escalates_for_uncooperative_child(self):
        ready = self.root / "uncooperative"
        leaf = """import os, signal, sys, time
from pathlib import Path
signal.signal(signal.SIGTERM, signal.SIG_IGN)
Path(sys.argv[1]).write_text(str(os.getpid()))
time.sleep(60)
"""
        owner = """try:
    with CaptureSession(sys.argv[1], termination_grace=0.1) as session:
        session.run([sys.executable, '-c', sys.argv[3], sys.argv[2]])
except CaptureInterrupted as error:
    sys.exit(128 + error.signum)
"""
        process = self.spawn_owner(owner, self.root, ready, leaf)
        child_pid = int(self.wait_file(ready, process))
        process.terminate()
        stdout, stderr = process.communicate(timeout=10)
        self.assertEqual(process.returncode, 143, (stdout, stderr))
        with self.assertRaises(ProcessLookupError):
            os.kill(child_pid, 0)
        self.assertEqual(self.contender().returncode, 0)


if __name__ == "__main__":
    unittest.main()
