"""Group changed BSC files into stable, bounded review shards.

The model follows behavior and cross-artifact contracts within these coarse groups.
Path rules keep compiler, runtime, library, test, and build diffs from sharing one
truncation budget. stdlib only.

Usage:
    split_review_shards.py FILE [FILE ...]
    git diff --name-only BASE...HEAD | split_review_shards.py -
"""

import json
import sys

SHARD_ORDER = [
    "compiler",
    "libraries",
    "hdl",
    "runtime",
    "solvers",
    "tests",
    "build",
    "docs",
    "other",
]


def shard_for(path):
    """Return the coarse shard for one repository-relative path."""
    name = path.rsplit("/", 1)[-1]
    lower = path.lower()
    # Test build/configuration files belong with the regressions they govern.
    if path.startswith("testsuite/"):
        return "tests"
    if (
        path.startswith(".github/")
        or name in ("Makefile", "GNUmakefile", "CMakeLists.txt", "cabal.project")
        or name.startswith("Makefile.")
        or lower.endswith((".mk", ".cmake", ".cabal"))
    ):
        return "build"
    if path.startswith("doc/") or lower.endswith((".md", ".tex")) or name.startswith("README"):
        return "docs"
    if path.startswith("src/vendor/"):
        return "solvers"
    if path.startswith(("src/comp/", "src/Parsec/")):
        return "compiler"
    if path.startswith("src/Libraries/"):
        return "libraries"
    if path.startswith(("src/Verilog/", "src/Verilog.Quartus/", "src/Verilog.Vivado/")):
        return "hdl"
    if path.startswith(("src/bluesim/", "src/bluetcl/", "src/VPI/", "src/Verilator/")):
        return "runtime"
    # Utilities outside the major source areas can still be grouped by language.
    if lower.endswith((".hs", ".lhs")):
        return "compiler"
    if lower.endswith((".bsv", ".bs", ".bs.in", ".v", ".sv", ".svh", ".vhd", ".vhdl")):
        return "hdl"
    if lower.endswith((".c", ".cc", ".cpp", ".cxx", ".h", ".hh", ".hpp", ".hxx")):
        return "runtime"
    return "other"


def assign_shards(files):
    """Preserve path text and input order within each stable, nonempty shard."""
    groups = {name: [] for name in SHARD_ORDER}
    for path in files:
        if path:
            groups[shard_for(path)].append(path)
    return {name: paths for name, paths in groups.items() if paths}


def _read_args(argv):
    if len(argv) == 2 and argv[1] == "-":
        return [line.rstrip("\n") for line in sys.stdin if line.rstrip("\n")]
    return argv[1:]


def main(argv):
    files = _read_args(argv)
    if not files:
        sys.stderr.write("usage: split_review_shards.py FILE [FILE ...] | -\n")
        return 2
    json.dump(assign_shards(files), sys.stdout, indent=2)
    sys.stdout.write("\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
