#!/usr/bin/env python3
"""Exercise source snapshot boundaries with a real compiler installation.

This integration check uses the checkout's rules and installed compiler. It does
not build or modify that installation, run the legacy suite, or require Buck2.
"""

import argparse
import json
from pathlib import Path
import subprocess
import tempfile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("planner", type=Path)
    parser.add_argument("repository", type=Path)
    args = parser.parse_args()
    planner = args.planner.resolve(strict=True)
    repository = args.repository.resolve(strict=True)
    suite = repository / "testsuite"

    def run(*arguments):
        return subprocess.run([str(planner), *map(str, arguments)],
                              capture_output=True, text=True, timeout=120)

    with tempfile.TemporaryDirectory(prefix="bsc-emit-") as temporary:
        work = Path(temporary)
        script = suite / "bsc.bugs/bluespec_inc/b1213/b1213.exp"
        planned = run("plan", "--config", "emit-check", "--suite-root", suite, script)
        assert planned.returncode == 0, planned.stderr
        plan = work / "plan.json"
        value = json.loads(planned.stdout)
        value["scripts"].append({"path": "bsc.unplanned/generated.exp", "items": [{
            "status": "unsupported", "id": None,
            "origin": {"file": "bsc.unplanned/generated.exp", "line": 1, "column": 1, "offset": 0},
            "construct": "setup", "reason": "This unsupported script need not exist to execute its peers."}]})
        plan.write_text(json.dumps(value))
        cell = work / "cell"

        def emit(destination):
            return run("emit-buck2", plan, "--suite-root", suite,
                       "--installation", repository / "inst", "--output", destination)

        emitted = emit(cell)
        assert emitted.returncode == 0, emitted.stderr
        manifest = json.loads((cell / "targets.json").read_text())
        assert len(manifest["targets"]) == 1 and manifest["execution_gaps"] == []
        assert manifest["unsupported"] == 1, "unsupported peer was lost or blocked execution"
        assert (cell / "targets.txt").read_text().splitlines() == [manifest["targets"][0]["target"]]
        tracked = subprocess.check_output(
            ["git", "-C", str(suite), "ls-files", "-z", "--", "."]).decode().split("\0")
        assert (cell / "source-paths.txt").read_text().splitlines() == tracked[:-1]
        assert (cell / "suite" / script.relative_to(suite)).read_bytes() == script.read_bytes()
        assert not list((cell / "suite").rglob("testrun.log")), "generated logs leaked into snapshot"
        assert (cell / "suite/bsc.typechecker/config").is_symlink()
        assert (cell / "suite/bsc.typechecker/config").resolve().is_relative_to(cell / "suite")
        assert json.loads((cell / "host-identity.json").read_text())["files"]

        refused = emit(cell)
        assert refused.returncode != 0 and "already exists" in refused.stderr
        value = json.loads(plan.read_text())
        value["scripts"][0]["items"][0]["kind"]["source"] = "Changed.bs"
        plan.write_text(json.dumps(value))
        stale = emit(work / "stale")
        assert stale.returncode != 0 and "differs from current source" in stale.stderr
        assert not (work / "stale").exists()
        print("Buck2 snapshot inventory, isolation, and stale-plan checks passed.")


if __name__ == "__main__":
    main()
