"""POSIX lock and process-group lifecycle for serial testsuite captures.

Never unlink the shared lock file. Legacy callers that do not take this lock,
or commands that escape the managed group with setsid(), are outside this guard.
The group owner checks disappearance after the leader exits. A bounded failure
prevents unreaped zombies from wedging capture or being mistaken for completion.
"""

import fcntl
import os
from pathlib import Path
import signal
import subprocess
import time


LOCK_ENV = "BSC_TEST_CAPTURE_LOCK_FD"
GROUP_ENV = "BSC_TEST_CAPTURE_SHARED_GROUP"
SIGNALS = (signal.SIGINT, signal.SIGTERM)
GROUP_DRAIN_SECONDS = 5


class CaptureInterrupted(BaseException):
    def __init__(self, signum):
        self.signum = signum
        super().__init__(f"capture interrupted by signal {signum}")


def group_exists(group):
    try:
        os.killpg(group, 0)
        return True
    except ProcessLookupError:
        return False


class CaptureSession:
    """Hold the checkout lease and clean up the current command on interruption."""

    def __init__(self, root, termination_grace=5):
        self.root = Path(root).resolve()
        self.termination_grace = termination_grace
        self.fd = None
        self.process = None
        self.starting = False
        self.pending_signal = None
        self.handlers = {}
        self.shared_group = False

    def __enter__(self):
        suite = (self.root / "testsuite").resolve()
        evidence = suite / ".stage1-validation"
        resolved = evidence.resolve()
        if (resolved == suite or suite in resolved.parents) and not (
                resolved == evidence or evidence in resolved.parents):
            raise RuntimeError("testsuite/.stage1-validation resolves into ordinary test sources")
        path = resolved / "capture.lock"
        path.parent.mkdir(parents=True, exist_ok=True)
        inherited = os.environ.get(LOCK_ENV)
        try:
            if inherited is not None:
                try:
                    self.fd = int(inherited)
                    actual, expected = os.fstat(self.fd), path.stat()
                except (OSError, ValueError) as error:
                    self.fd = None
                    raise RuntimeError("invalid inherited capture-lock descriptor") from error
                if (actual.st_dev, actual.st_ino) != (expected.st_dev, expected.st_ino):
                    self.fd = None
                    raise RuntimeError("inherited descriptor is not this checkout's capture lock")
                self.shared_group = os.environ.get(GROUP_ENV) == "1"
            else:
                self.fd = os.open(path, os.O_RDWR | os.O_CREAT | os.O_CLOEXEC | os.O_NOFOLLOW, 0o600)
            try:
                fcntl.flock(self.fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
            except BlockingIOError as error:
                raise RuntimeError(f"another capture owns the checkout lock: {path}") from error
            for signum in SIGNALS:
                self.handlers[signum] = signal.signal(signum, self._interrupted)
            return self
        except BaseException:
            self._restore()
            raise

    def _interrupted(self, signum, _frame):
        # Register a newly spawned child before allowing interruption to unwind.
        if self.starting:
            self.pending_signal = signum
        else:
            raise CaptureInterrupted(signum)

    def _restore(self):
        for signum, handler in self.handlers.items():
            signal.signal(signum, handler)
        self.handlers.clear()
        if self.fd is not None:
            # LOCK_UN would also unlock a parent's shared open file description.
            os.close(self.fd)
            self.fd = None

    def __exit__(self, *_error):
        for signum in self.handlers:
            signal.signal(signum, signal.SIG_IGN)
        try:
            if self.process is not None:
                self.process.terminate_group(self.termination_grace)
        finally:
            self._restore()

    def start(self, command, **kwargs):
        if self.process is not None:
            try:
                self.process.wait(timeout=0)
            except subprocess.TimeoutExpired as error:
                raise RuntimeError("cannot advance while the previous process group is running") from error
        environment = dict(kwargs.pop("env", os.environ))
        environment[LOCK_ENV] = str(self.fd)
        environment[GROUP_ENV] = "1"
        self.starting = True
        try:
            child = subprocess.Popen(command, env=environment, pass_fds=(self.fd,),
                                     start_new_session=not self.shared_group, **kwargs)
            group = os.getpgrp() if self.shared_group else child.pid
            self.process = GroupProcess(child, group, owner=not self.shared_group)
        finally:
            self.starting = False
        if self.pending_signal is not None:
            raise CaptureInterrupted(self.pending_signal)
        return self.process

    def run(self, command, **kwargs):
        process = self.start(command, **kwargs)
        status = process.wait()
        if status:
            raise subprocess.CalledProcessError(status, command)
        return status


class GroupProcess:
    def __init__(self, child, group, owner):
        self.child = child
        self.group = group
        self.owner = owner
        self.leader_finished_at = None

    @property
    def returncode(self):
        return self.child.returncode

    def wait(self, timeout=None):
        deadline = None if timeout is None else time.monotonic() + timeout
        self.child.wait(timeout=timeout)
        if self.leader_finished_at is None:
            self.leader_finished_at = time.monotonic()
        # An inherited capture itself belongs to this group; its outer runner
        # owns the final group-empty check. make's normal contract waits for jobs.
        while self.owner and group_exists(self.group):
            if time.monotonic() - self.leader_finished_at >= GROUP_DRAIN_SECONDS:
                raise RuntimeError(
                    f"process group {self.group} remains after its leader exited; "
                    "refusing to advance (this may include unreaped zombies)")
            if deadline is not None and time.monotonic() >= deadline:
                raise subprocess.TimeoutExpired(self.child.args, timeout)
            time.sleep(0.02 if deadline is None else max(0, min(0.02, deadline - time.monotonic())))
        return self.returncode

    def terminate_group(self, grace):
        self.child.poll()
        if self.child.returncode is not None and (not self.owner or not group_exists(self.group)):
            return
        try:
            os.killpg(self.group, signal.SIGTERM)
        except ProcessLookupError:
            pass
        try:
            self.wait(timeout=grace)
            return
        except (subprocess.TimeoutExpired, RuntimeError):
            if self.owner:
                try:
                    os.killpg(self.group, signal.SIGKILL)
                except ProcessLookupError:
                    pass
            else:
                # Do not SIGKILL our own inherited capture group. Its outer
                # owner will escalate after this capture exits unsuccessfully.
                self.child.kill()
        # SIGKILL cannot resolve zombies or uninterruptible kernel waits. Fail
        # rather than wait forever or claim that cleanup completed.
        cleanup_timeout = max(grace, 0.1)
        self.child.wait(timeout=cleanup_timeout)
        deadline = time.monotonic() + cleanup_timeout
        while self.owner and group_exists(self.group):
            if time.monotonic() >= deadline:
                raise RuntimeError(
                    f"process group {self.group} still exists after cleanup; "
                    "manual inspection is required before restarting")
            time.sleep(0.02)
