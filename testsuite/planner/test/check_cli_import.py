#!/usr/bin/env python3
"""Run the real planner CLI against archive-layout regression fixtures.

Usage: python3 testsuite/planner/test/check_cli_import.py /path/to/bsc-test-plan
The fixture and imported JSON live in a temporary directory. No suite runs.
"""

import argparse
import json
from pathlib import Path
import subprocess
import tempfile


def summary(test, label):
    return (f"Running {test} ...\nPASS: {label}\n\n"
            "\t\t===  Summary ===\n\n# of expected passes\t1\n")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("planner", type=Path)
    args = parser.parse_args()
    executable = args.planner.resolve(strict=True)
    with tempfile.TemporaryDirectory(prefix="bsc-planner-cli-") as temporary:
        work = Path(temporary)
        archive = work / "archive"
        one = archive / "bsc.one" / "testrun.sum"
        two = archive / "bsc.two" / "nested" / "testrun.sum"
        for path in (one, two):
            path.parent.mkdir(parents=True)
        one.write_text(summary("./one.exp", "one"), encoding="utf-8")
        two.write_text(summary("./two.exp", "two"), encoding="utf-8")
        # A root aggregate must not duplicate (or replace) leaf summaries.
        (archive / "testrun.sum").write_text(
            summary("./bsc.one/one.exp", "one"), encoding="utf-8")
        # Neither a symlinked group nor a symlinked summary is an archive input.
        (archive / "bsc.linked-group").symlink_to(one.parent, target_is_directory=True)
        symbolic = archive / "bsc.symbolic" / "testrun.sum"
        symbolic.parent.mkdir()
        symbolic.symlink_to(one)
        unrelated = archive / "unrelated" / "testrun.sum"
        unrelated.parent.mkdir()
        unrelated.write_text("not a summary\n", encoding="utf-8")
        expected = work / "expected.txt"
        expected.write_text("bsc.one/one.exp\nbsc.two/nested/two.exp\n", encoding="utf-8")
        output = work / "import.json"
        command = [str(executable), "import-sum", "--config", "cli-regression",
                   "--suite-root", str(work / "original-suite"), "--expected",
                   str(expected), str(archive), str(output)]
        result = subprocess.run(command, capture_output=True, text=True, timeout=30)
        if result.returncode:
            raise AssertionError(f"archive import failed: {result.stdout}{result.stderr}")
        manifest = json.loads(output.read_text(encoding="utf-8"))
        wanted = ["bsc.one/one.exp", "bsc.two/nested/two.exp"]
        if manifest["tests"] != wanted or len(manifest["verdicts"]) != 2:
            raise AssertionError(f"wrong imported population: {manifest}")
        result = subprocess.run([str(executable), "compare", "--expected", str(expected),
                                 str(output), str(output)], capture_output=True,
                                text=True, timeout=30)
        if result.returncode or "Population differences: 0" not in result.stdout:
            raise AssertionError(f"self-comparison failed: {result.stdout}{result.stderr}")
        # Ignored files must not hide a genuinely missing required leaf.
        two.unlink()
        result = subprocess.run(command, capture_output=True, text=True, timeout=30)
        if result.returncode == 0 or "test discovery mismatch" not in result.stderr:
            raise AssertionError(f"missing leaf was not rejected: {result.stdout}{result.stderr}")
    print("CLI archive import regression passed")


if __name__ == "__main__":
    main()
