#!/usr/bin/env python3
"""Check reports produced by existing DejaGnu tests; never run the compiler."""

import argparse
import hashlib
import json
import shutil
from pathlib import Path
import sys


def snapshot(prefix, trees=()):
    ignored = {prefix + suffix for suffix in
               (".snapshot.json", ".json", ".stdout", ".stderr")}
    ignored.update(("testrun.log", "testrun.sum"))
    # Child directories belong to independent fullparallel test workers.
    # These invocations write their outputs only in the current fixture.
    paths = set(Path(".").iterdir())
    # Selected fixture trees let alias tests watch the real target bytes too.
    # Do not recurse by default: neighbouring directories can have independent
    # fullparallel workers, and following arbitrary symlinks could leave this
    # fixture or encounter a cycle.
    for tree in trees:
        paths.add(Path(tree))
        paths.update(Path(tree).rglob("*"))
    return snapshot_paths(paths, ignored)


def snapshot_paths(paths, ignored=()):
    result = {}
    for path in sorted(paths):
        name = path.as_posix()
        if name in ignored:
            continue
        if path.is_symlink():
            result[name] = ["symlink", path.lstat().st_mtime_ns, str(path.readlink())]
        elif path.is_file():
            stat = path.stat()
            result[name] = ["file", stat.st_mtime_ns,
                            hashlib.sha256(path.read_bytes()).hexdigest()]
        elif path.is_dir():
            result[name] = ["directory"]
    return result


def tree_snapshot(root, exclusions):
    root = Path(root).resolve()
    require(root.is_dir(), "Snapshot tree is not a directory: " + str(root))
    excluded = set()
    for name in exclusions:
        path = Path(name)
        require(not path.is_absolute() and ".." not in path.parts,
                "Snapshot exclusion must stay inside its tree: " + name)
        excluded.add((root / path).as_posix())
    return snapshot_paths({root, *root.rglob("*")}, excluded)


def require(condition, message):
    if not condition:
        raise ValueError(message)


def check(args):
    before = json.loads(Path(args.prefix + ".snapshot.json").read_text())
    after = snapshot(args.prefix, args.tree)
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
    # Unlike --input/--missing, these assertions intentionally do not resolve
    # paths. A report must preserve the spelling of an alias and attach it to
    # the correct edge, including transitive imports read from object metadata.
    for owner, role, policy, kind, exists, path in args.candidate:
        require(exists in ("true", "false"), "Candidate existence must be true or false")
        expected = {"path": path, "kind": kind, "exists": exists == "true"}
        require(any(item["owner"] == owner and item["role"] == role
                    and item["policy"] == policy and expected in item["candidates"]
                    for item in requirements),
                "Missing exact candidate: " + repr((owner, role, policy, expected)))
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
    # Apply the exclusion independently to both views: a correct flat union
    # must not conceal stale installed candidates in a conditional branch.
    for name in args.exclude_tree:
        root = Path(name).resolve()
        for view, facts in (("flat", requirements),
                            ("conditional", [entry["requirement"] for entry in
                                             report["conditional_requirements"]])):
            for item in facts:
                for candidate in item["candidates"]:
                    path = Path(candidate["path"]).resolve()
                    require(not path.is_relative_to(root),
                            f"Distribution input retained in {view} requirements: {candidate['path']}")
    for boundary in args.forbid_boundary:
        require(not any(boundary in reason for reason in report["incomplete"]),
                "Unexpected unresolved boundary: " + boundary)
    for boundary in args.boundary:
        require(any(boundary in reason for reason in report["incomplete"]),
                "Missing unresolved boundary: " + boundary)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    snapshotter = commands.add_parser("snapshot")
    snapshotter.add_argument("prefix")
    snapshotter.add_argument("--tree", action="append", default=[])
    checker = commands.add_parser("check")
    checker.add_argument("prefix")
    checker.add_argument("--mode", required=True)
    checker.add_argument("--complete", choices=("true", "false"), required=True)
    checker.add_argument("--tree", action="append", default=[])
    checker.add_argument("--candidate", nargs=6, action="append", default=[],
                         metavar=("OWNER", "ROLE", "POLICY", "KIND", "EXISTS", "PATH"))
    for option in ("input", "missing", "role", "output", "boundary",
                   "exclude-tree", "forbid-boundary"):
        checker.add_argument("--" + option, action="append", default=[])
    for command in ("snapshot-tree", "check-tree"):
        tree = commands.add_parser(command)
        tree.add_argument("root")
        tree.add_argument("output")
        tree.add_argument("--exclude", action="append", default=[])
    copier = commands.add_parser("copy-distribution")
    copier.add_argument("source")
    copier.add_argument("destination")
    args = parser.parse_args()
    if args.command == "snapshot":
        Path(args.prefix + ".snapshot.json").write_text(json.dumps(snapshot(args.prefix, args.tree)))
    elif args.command in ("snapshot-tree", "check-tree"):
        contents = tree_snapshot(args.root, args.exclude)
        if args.command == "snapshot-tree":
            Path(args.output).write_text(json.dumps(contents))
        else:
            before = json.loads(Path(args.output).read_text())
            changed = [name for name in sorted(before.keys() | contents.keys())
                       if before.get(name) != contents.get(name)]
            require(not changed, "Fixture changed immutable installation files: " +
                    ", ".join(changed))
    elif args.command == "copy-distribution":
        # Dereference installed aliases into private bytes. A symlink back to
        # the live distribution would defeat both isolation and physical-path
        # assertions. copytree also refuses an existing destination.
        shutil.copytree(args.source, args.destination, symlinks=False)
    else:
        check(args)


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, KeyError, TypeError) as error:
        sys.exit("Dependency check failed: " + str(error))
