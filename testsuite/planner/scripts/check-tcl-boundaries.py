#!/usr/bin/env python3
"""Compare active .exp word boundaries with Tcl_ParseCommand, without evaluation.

This optional check needs GHC, a C compiler, pkg-config, and Tcl 8.6 development
headers and libraries. It checks top-level command starts and exact word source
bytes and offsets, not substituted values or semantic lowering. Tcl may fold a
constant expansion such as {*}{a b} into two words; our lexer retains one
Expanded word. That representation difference is reported as a mismatch, never
silently skipped.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path
import shlex
import subprocess
import sys
import tempfile


PLANNER = Path(__file__).resolve().parents[1]


def discover(repo):
    paths = []
    for root in sorted((repo / "testsuite").glob("bsc.*")):
        if not root.is_dir() or root.is_symlink():
            continue
        for parent, directories, files in os.walk(root, followlinks=False):
            directories[:] = sorted(
                name for name in directories
                if not (Path(parent) / name).is_symlink()
            )
            # Preserve lexical .exp names, including active long-test symlinks.
            paths.extend(Path(parent) / name for name in sorted(files)
                         if name.endswith(".exp"))
    return sorted(paths)


def checksum(repo, paths):
    digest = hashlib.sha256()
    for path in paths:
        digest.update(str(path.relative_to(repo)).encode("utf-8") + b"\0")
        digest.update(path.read_bytes() + b"\0")
    return digest.hexdigest()


def decode(output):
    """Reject incomplete or duplicate records, including missing file trailers."""
    files = {}
    current = None
    command = None
    for number, line in enumerate(output.splitlines(), 1):
        fields = line.split("\t")
        kind = fields[0]
        if kind == "F" and len(fields) == 2 and current is None:
            path = bytes.fromhex(fields[1]).decode("utf-8")
            if path in files:
                raise ValueError(f"duplicate probe file record: {path}")
            current = {"commands": [], "errors": []}
            files[path] = current
        elif kind == "C" and len(fields) == 2 and current is not None:
            command = {"offset": int(fields[1]), "words": []}
            current["commands"].append(command)
        elif (kind == "W" and len(fields) == 3 and current is not None
              and command is not None):
            bytes.fromhex(fields[2])  # Validate the complete raw source encoding.
            command["words"].append({"offset": int(fields[1]),
                                      "source": fields[2]})
        elif kind == "E" and len(fields) == 4 and current is not None:
            current["errors"].append({
                "type": fields[1], "offset": int(fields[2]),
                "message": bytes.fromhex(fields[3]).decode("utf-8"),
            })
        elif kind == "Z" and len(fields) == 1 and current is not None:
            current = None
            command = None
        else:
            raise ValueError(f"invalid probe record at output line {number}")
    if current is not None:
        raise ValueError("probe output ended before its file trailer")
    for path, result in files.items():
        for item in result["commands"]:
            if item["offset"] < 0 or not item["words"]:
                raise ValueError(f"invalid command record for {path}")
            if any(word["offset"] < 0 for word in item["words"]):
                raise ValueError(f"invalid word record for {path}")
    return files


def run_logged(command, work, label):
    result = subprocess.run(command, capture_output=True, text=True,
                            encoding="utf-8",
                            env={**os.environ, "GHC_ENVIRONMENT": "-"})
    (work / f"{label}.stdout").write_text(result.stdout, encoding="utf-8")
    (work / f"{label}.stderr").write_text(result.stderr, encoding="utf-8")
    if result.returncode:
        detail = result.stderr.strip() or result.stdout.strip()
        raise RuntimeError(f"{label} failed ({result.returncode}): {detail}")
    return result


def compare(repo, work, ghc):
    paths = discover(repo)
    if not paths:
        raise ValueError("no active testsuite/bsc.* .exp files found")
    before = checksum(repo, paths)
    lexer = PLANNER / "src/Tcl.hs"
    lexer_before = hashlib.sha256(lexer.read_bytes()).hexdigest()
    print(f"Comparing {len(paths)} active .exp files; no Tcl evaluation.",
          flush=True)

    flags = shlex.split(run_logged(
        ["pkg-config", "--cflags", "--libs", "tcl"], work, "pkg-config"
    ).stdout)
    run_logged(["cc", "-std=c11", "-O2", "-Wall", "-Wextra",
                str(PLANNER / "test/native-tcl-boundaries.c"),
                "-o", str(work / "native")] + flags, work, "build-native")
    ghc_output = work / "ghc"
    ghc_output.mkdir()
    run_logged([ghc, "-O1", "-Wall", "-v0", "-package-env", "-",
                "-hide-all-packages", "-package", "base", "-package", "array",
                "-i" + str(PLANNER / "src"), "-outputdir", str(ghc_output),
                "-o", str(work / "haskell"),
                str(PLANNER / "test/TclBoundaryProbe.hs")],
               work, "build-haskell")

    outputs = {}
    metadata = {"ghc": run_logged([ghc, "--numeric-version"], work,
                                   "ghc-version").stdout.strip()}
    expected_files = set(map(str, paths))
    for name in ("native", "haskell"):
        result = run_logged([str(work / name)] + list(map(str, paths)),
                            work, name)
        outputs[name] = decode(result.stdout)
        if set(outputs[name]) != expected_files:
            raise ValueError(f"{name} output does not cover the exact input files")
        metadata[name] = result.stderr.strip()

    totals = {"files": len(paths)}
    for name in outputs:
        results = outputs[name].values()
        totals[name + "_commands"] = sum(len(r["commands"]) for r in results)
        totals[name + "_words"] = sum(len(c["words"]) for r in results
                                       for c in r["commands"])
        totals[name + "_error_files"] = sum(bool(r["errors"]) for r in results)
    mismatches = []
    for path in paths:
        native = outputs["native"][str(path)]
        haskell = outputs["haskell"][str(path)]
        if native == haskell and not native["errors"]:
            continue
        detail = {
            "file": str(path.relative_to(repo)),
            "native_errors": native["errors"],
            "haskell_errors": haskell["errors"],
            "native_commands": len(native["commands"]),
            "haskell_commands": len(haskell["commands"]),
        }
        for index in range(max(len(native["commands"]), len(haskell["commands"]))):
            left = native["commands"][index:index + 1]
            right = haskell["commands"][index:index + 1]
            if left != right:
                detail.update(first_command_difference=index + 1,
                              native=left, haskell=right)
                break
        mismatches.append(detail)

    after_paths = discover(repo)
    stable = paths == after_paths and before == checksum(repo, after_paths)
    lexer_stable = lexer_before == hashlib.sha256(lexer.read_bytes()).hexdigest()
    summary = {
        "scope": "top-level command starts and raw word UTF-8 byte offsets and source bytes; no Tcl evaluation",
        "corpus_sha256": before, "corpus_unchanged": stable,
        "lexer_sha256": lexer_before, "lexer_unchanged": lexer_stable,
        "metadata": metadata, "totals": totals,
        "mismatch_files": len(mismatches), "mismatches": mismatches,
    }
    (work / "result.json").write_text(json.dumps(summary, indent=2) + "\n",
                                      encoding="utf-8")
    print(json.dumps({k: v for k, v in summary.items() if k != "mismatches"},
                     indent=2))
    for detail in mismatches:
        print(f"Mismatch: {detail['file']}; first differing command: "
              f"{detail.get('first_command_difference', 'parse error')}")
        for name in ("native", "haskell"):
            for error in detail[name + "_errors"]:
                print(f"  {name}: {error['message']}")
    if not stable or not lexer_stable:
        print("Source changed during comparison; this result cannot qualify.",
              file=sys.stderr)
    return 0 if stable and lexer_stable and not mismatches else 1


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("repo", type=Path, help="BSC repository root")
    parser.add_argument("--ghc", default="ghc", help="GHC executable (default: ghc)")
    parser.add_argument("--output-dir", type=Path,
                        help="new scratch directory in which to retain evidence")
    args = parser.parse_args()
    repo = args.repo.resolve()
    if not (repo / "testsuite").is_dir():
        parser.error(f"not a BSC repository root: {repo}")
    try:
        if args.output_dir is not None:
            work = args.output_dir.resolve()
            if work.exists():
                parser.error(f"refusing to overwrite existing evidence: {work}")
            suite = (repo / "testsuite").resolve()
            evidence = suite / ".stage1-validation"
            if (work == suite or suite in work.parents) and not (
                    work == evidence or evidence in work.parents):
                parser.error("scratch evidence must be under testsuite/.stage1-validation or outside testsuite")
            work.mkdir(parents=True)
            print(f"Evidence directory: {work}", flush=True)
            return compare(repo, work, args.ghc)
        with tempfile.TemporaryDirectory(prefix="bsc-tcl-boundaries-") as temporary:
            return compare(repo, Path(temporary), args.ghc)
    except (OSError, RuntimeError, ValueError) as error:
        print(f"check-tcl-boundaries: {error}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
