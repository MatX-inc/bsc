#!/usr/bin/env python3
"""Check reports produced by existing DejaGnu tests; never run the compiler."""

import argparse
import hashlib
import json
from pathlib import Path
import sys


def snapshot(prefix):
    ignored = {prefix + suffix for suffix in
               (".snapshot.json", ".json", ".stdout", ".stderr")}
    ignored.update(("testrun.log", "testrun.sum"))
    result = {}
    # Child directories belong to independent fullparallel test workers.
    # These invocations write their outputs only in the current fixture.
    for path in sorted(Path(".").iterdir()):
        name = path.as_posix()
        if name in ignored:
            continue
        if path.is_symlink():
            result[name] = ["symlink", str(path.readlink())]
        elif path.is_file():
            stat = path.stat()
            result[name] = ["file", stat.st_mtime_ns,
                            hashlib.sha256(path.read_bytes()).hexdigest()]
        elif path.is_dir():
            result[name] = ["directory"]
    return result


def require(condition, message):
    if not condition:
        raise ValueError(message)


def check(args):
    before = json.loads(Path(args.prefix + ".snapshot.json").read_text())
    after = snapshot(args.prefix)
    changed = [name for name in sorted(before.keys() | after.keys())
               if before.get(name) != after.get(name)]
    require(not changed, "Query changed fixture files: " + ", ".join(changed))
    require(not Path(args.prefix + ".stdout").read_text().strip(),
            "File-report query unexpectedly wrote to stdout")
    report = json.loads(Path(args.prefix + ".json").read_text())
    require(report["schema"] == "bsc-dependencies" and report["version"] == 1,
            "Unexpected report schema")
    require(report["mode"] == args.mode, "Unexpected invocation mode")
    require(type(report["complete"]) is bool and
            report["complete"] == (not report["incomplete"]),
            "Inconsistent completeness flag")
    require(report["complete"] == (args.complete == "true"),
            "Unexpected completeness: " + repr(report["incomplete"]))
    requirements = report["requirements"]
    candidates = [candidate for item in requirements for candidate in item["candidates"]]
    require(candidates, "Empty input report")
    for candidate in candidates:
        path = Path(candidate["path"])
        exists = path.is_dir() if candidate["kind"] in (
            "directory-tree", "include-search-directory") else path.is_file()
        require(type(candidate["exists"]) is bool and candidate["exists"] == exists,
                "Incorrect availability for " + str(path))
    available = {Path(c["path"]).resolve() for c in candidates if c["exists"]}
    absent = {Path(c["path"]).resolve() for c in candidates if not c["exists"]}
    for name in args.input:
        require(Path(name).resolve() in available, "Missing input: " + name)
    for name in args.missing:
        require(Path(name).resolve() in absent, "Missing absence dependency: " + name)
    for role in args.role:
        require(any(item["role"] == role for item in requirements), "Missing role: " + role)
    outputs = {Path(name).resolve() for name in report["potential_outputs"]}
    for name in args.output:
        require(Path(name).resolve() in outputs, "Missing potential output: " + name)
    for entry in report["conditional_requirements"]:
        require(entry["requirement"] in requirements, "Conditional input absent from union")
        for condition in entry["when"]:
            require(type(condition["occurrence"]) is int and
                    isinstance(condition["choice"], str) and
                    0 <= condition["branch"] < condition["branches"],
                    "Invalid condition: " + repr(condition))
    for boundary in args.boundary:
        require(any(boundary in reason for reason in report["incomplete"]),
                "Missing unresolved boundary: " + boundary)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    commands.add_parser("snapshot").add_argument("prefix")
    checker = commands.add_parser("check")
    checker.add_argument("prefix")
    checker.add_argument("--mode", required=True)
    checker.add_argument("--complete", choices=("true", "false"), required=True)
    for option in ("input", "missing", "role", "output", "boundary"):
        checker.add_argument("--" + option, action="append", default=[])
    args = parser.parse_args()
    if args.command == "snapshot":
        Path(args.prefix + ".snapshot.json").write_text(json.dumps(snapshot(args.prefix)))
    else:
        check(args)


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, KeyError, TypeError) as error:
        sys.exit("Dependency check failed: " + str(error))
