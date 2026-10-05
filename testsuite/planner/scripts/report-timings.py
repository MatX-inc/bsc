#!/usr/bin/env python3
"""Summarize clean captured suite timings, optionally compare an internal 1/0/0/1 run.

Inputs are capture directories, run-N directories, or run-N/result.json files.
A capture directory expands to every requested run in order. For example:
  report-timings.py --compare-internal-checks enabled-first disabled-pair enabled-last

The report is JSON on stdout, or a new file selected by --output. It runs no tests.
"""

import argparse
from datetime import datetime, timedelta
import json
import math
from pathlib import Path, PurePosixPath
import re
import sys


CATEGORIES = ("PASS", "XFAIL", "FAIL", "XPASS", "KFAIL", "KPASS",
              "UNRESOLVED", "UNTESTED", "UNSUPPORTED", "ERROR", "WARNING")
FAILURES = ("FAIL", "XPASS", "KPASS", "UNRESOLVED", "ERROR")
METRICS = ("count", "user_seconds", "system_seconds", "elapsed_seconds_sum")
CONFIG_KEYS = ("TEST_SYSTEMC_INC", "TEST_SYSTEMC_LIB", "TEST_SYSTEMC_CXXFLAGS",
               "VTEST", "CTEST", "SYSTEMCTEST", "TEST_BSC_VERILOG_SIM",
               "DO_INTERNAL_CHECKS", "TEST_BSC_OPTIONS", "TEST_OSTYPE", "TEST_MACHTYPE")
NOTE = re.compile(r"Command (?:exited with non-zero status [1-9][0-9]*|"
                  r"terminated by signal [1-9][0-9]*)\Z")
NUMBER = re.compile(r"[0-9]+(?:\.[0-9]+)?\Z")


def require(condition, message):
    if not condition:
        raise ValueError(message)


def integer(value, minimum=0):
    return type(value) is int and value >= minimum


def finite_number(value, minimum=0):
    return (type(value) in (int, float) and math.isfinite(value)
            and value >= minimum)


def load_json(path):
    def pairs(items):
        result = {}
        for key, value in items:
            require(key not in result, f"{path}: duplicate JSON key {key!r}")
            result[key] = value
        return result

    def constant(value):
        raise ValueError(f"{path}: non-finite JSON number {value}")

    result = json.loads(path.read_text(), object_pairs_hook=pairs,
                        parse_constant=constant)
    require(isinstance(result, dict), f"{path}: expected a JSON object")
    return result


def metadata(capture):
    path = capture / "configuration.json"
    data = load_json(path)
    require(integer(data.get("runs"), 1), f"{path}: missing positive run count")
    require(data.get("command") == ["make", "-j128", "-C", "testsuite", "fullparallel"],
            f"{path}: not the full configured testsuite command")
    require(isinstance(data.get("base"), str) and data["base"],
            f"{path}: missing compiler revision")
    require(isinstance(data.get("suite_root"), str)
            and Path(data["suite_root"]).is_absolute(), f"{path}: missing suite root")
    for key in ("installation_sha256", "testsuite_source_sha256"):
        require(isinstance(data.get(key), str)
                and re.fullmatch(r"[0-9a-f]{64}", data[key]),
                f"{path}: missing or invalid {key}")
    config = data.get("configuration")
    require(isinstance(config, dict)
            and all(isinstance(config.get(key), str) for key in CONFIG_KEYS),
            f"{path}: incomplete configuration")
    require(config["DO_INTERNAL_CHECKS"] in ("0", "1"),
            f"{path}: invalid internal-check mode")
    require(isinstance(data.get("inherited_tool_settings"), dict),
            f"{path}: missing inherited tool settings")
    require(isinstance(data.get("random_seed_environment"), dict)
            and "PERL_RAND_SEED" in data["random_seed_environment"],
            f"{path}: missing seed metadata")
    perl = data.get("perl_executable")
    require(isinstance(perl, dict)
            and all(isinstance(perl.get(key), str) and perl[key]
                    for key in ("path", "resolved_path", "version", "sha256"))
            and re.fullmatch(r"[0-9a-f]{64}", perl["sha256"]),
            f"{path}: missing or invalid Perl identity")
    return data


def zero():
    return {key: 0 for key in METRICS}


def add(target, values):
    for key in METRICS:
        target[key] += values[key]


def rounded(values):
    return {key: round(value, 9) for key, value in values.items()}


def parse_timing(path):
    categories = {}
    notes = 0
    for number, line in enumerate(path.read_text().splitlines(), 1):
        if not line.strip():
            continue
        if NOTE.fullmatch(line):
            notes += 1
            continue
        fields = line.split(",", 4)
        prefix = f"{path}:{number}"
        require(len(fields) == 5, f"{prefix}: malformed timing record")
        category, system, user, elapsed, directory = (part.strip() for part in fields)
        require(re.fullmatch(r"check_[A-Za-z0-9_.+-]+", category),
                f"{prefix}: invalid timing category")
        require(all(NUMBER.fullmatch(value) and math.isfinite(float(value))
                    for value in (system, user, elapsed)),
                f"{prefix}: invalid nonnegative timing value")
        require(Path(directory).is_absolute(), f"{prefix}: missing absolute working directory")
        values = {"count": 1, "user_seconds": float(user),
                  "system_seconds": float(system), "elapsed_seconds_sum": float(elapsed)}
        add(categories.setdefault(category, zero()), values)
    require(notes <= sum(value["count"] for value in categories.values()),
            f"{path}: exit-status notes without complete timing records")
    return categories, notes


def read_manifest(path):
    paths = path.read_text().splitlines()
    require(bool(paths) and paths == sorted(set(paths)),
            f"{path}: empty, unsorted, or duplicate test manifest")
    for name in paths:
        parsed = PurePosixPath(name)
        require(not parsed.is_absolute() and ".." not in parsed.parts
                and parsed.parts[0].startswith("bsc.") and parsed.suffix == ".exp"
                and str(parsed) == name, f"{path}: invalid test path {name!r}")
    return paths


def utc_time(value, context):
    require(isinstance(value, str), f"{context}: missing UTC timestamp")
    result = datetime.fromisoformat(value)
    require(result.utcoffset() == timedelta(0), f"{context}: timestamp must be UTC")
    return result


def read_run(run, data):
    context = str(run / "result.json")
    result = load_json(run / "result.json")
    require(type(result.get("exit_code")) is int and result["exit_code"] == 0,
            f"{context}: run did not complete successfully")
    for key in ("scheduled_discovery_matches_source", "installation_unchanged",
                "testsuite_source_unchanged"):
        require(result.get(key) is True, f"{context}: {key} must be true")
    require(result.get("testsuite_source_sha256_after") == data["testsuite_source_sha256"],
            f"{context}: post-run source digest differs from capture identity")
    counts = result.get("counts")
    require(isinstance(counts, dict) and set(counts) == set(CATEGORIES)
            and all(integer(value) for value in counts.values()),
            f"{context}: incomplete or invalid verdict counts")
    require(counts["PASS"] > 0 and all(counts[key] == 0 for key in FAILURES),
            f"{context}: run is not clean")
    wall = result.get("suite_wall_seconds")
    require(finite_number(wall) and wall > 0,
            f"{context}: missing positive suite wall time (older captures are not comparable)")
    require(finite_number(result.get("seconds")) and result["seconds"] >= wall,
            f"{context}: invalid capture duration")
    start = utc_time(result.get("started_at_utc"), context)
    end = utc_time(result.get("finished_at_utc"), context)
    require(end > start, f"{context}: invalid run interval")
    summaries = sorted(run.glob("bsc.*/**/testrun.sum"))
    require(integer(result.get("summary_files"), 1)
            and len(summaries) == result["summary_files"],
            f"{context}: missing or extra archived summaries")
    actual = dict.fromkeys(CATEGORIES, 0)
    for path in summaries:
        for line in path.read_text(errors="replace").splitlines():
            category = line.partition(":")[0]
            if category in actual:
                actual[category] += 1
    require(actual == counts, f"{context}: archived verdicts disagree with recorded counts")
    expected = read_manifest(run / "expected-tests.txt")
    require(expected == read_manifest(run / "scheduled-tests.txt"),
            f"{context}: test discovery and schedule differ")
    required_summaries = {str(PurePosixPath(name).parent / "testrun.sum") for name in expected}
    require(required_summaries == {path.relative_to(run).as_posix() for path in summaries},
            f"{context}: summaries do not cover exactly the scheduled test directories")
    timings = sorted(run.glob("bsc.*/**/time.out"))
    require(integer(result.get("timing_files"), 1)
            and len(timings) == result["timing_files"],
            f"{context}: missing or extra archived timing files")
    categories = {}
    notes = 0
    for path in timings:
        values, extra_notes = parse_timing(path)
        notes += extra_notes
        for category, bucket in values.items():
            add(categories.setdefault(category, zero()), bucket)
    total = zero()
    for bucket in categories.values():
        add(total, bucket)
    require(total["count"] > 0, f"{context}: no timing records")
    return {"run": str(run), "internal_checks": int(data["configuration"]["DO_INTERNAL_CHECKS"]),
            "suite_wall_seconds": wall, "started_at_utc": start.isoformat(),
            "finished_at_utc": end.isoformat(), "verdict_counts": counts,
            "test_scripts": len(expected), "summary_files": len(summaries),
            "timing_files": len(timings), "command_status_notes": notes,
            "totals": rounded(total),
            "categories": {key: rounded(value) for key, value in sorted(categories.items())}}, expected


def expand_inputs(inputs):
    runs = []
    for path in inputs:
        path = path.resolve()
        if path.name == "result.json":
            path = path.parent
        if (path / "configuration.json").is_file():
            data = metadata(path)
            expected = {f"run-{number}" for number in range(1, data["runs"] + 1)}
            require({child.name for child in path.glob("run-*") if child.is_dir()} == expected,
                    f"{path}: capture is incomplete or contains unexpected runs")
            runs.extend((path / f"run-{number}", data) for number in range(1, data["runs"] + 1))
        else:
            data = metadata(path.parent)
            match = re.fullmatch(r"run-([1-9][0-9]*)", path.name)
            require(match is not None and int(match[1]) <= data["runs"],
                    f"{path}: not a run requested by its capture metadata")
            runs.append((path, data))
    require(len({path for path, _ in runs}) == len(runs), "duplicate run inputs")
    return runs


def comparable(data):
    # Evidence-directory locations are input arguments, never compared metadata.
    result = {key: value for key, value in data.items() if key != "runs"}
    result["configuration"] = {key: value for key, value in data["configuration"].items()
                               if key != "DO_INTERNAL_CHECKS"}
    return result


def difference(enabled, disabled):
    return {"enabled_mean": round(enabled, 9), "disabled_mean": round(disabled, 9),
            "enabled_minus_disabled": round(enabled - disabled, 9),
            "percent_change": round(100 * (enabled / disabled - 1), 6) if disabled else None}


def compare(runs, data):
    require([run["internal_checks"] for run in runs] == [1, 0, 0, 1],
            "internal-check comparison requires four runs in 1,0,0,1 (ABBA) order")
    seed = data["random_seed_environment"]["PERL_RAND_SEED"]
    require(isinstance(seed, str) and re.fullmatch(r"[0-9]+", seed),
            "internal-check comparison requires a fixed numeric PERL_RAND_SEED")
    version = re.fullmatch(r"v([0-9]+)\.([0-9]+)\.([0-9]+)", data["perl_executable"]["version"])
    require(version is not None and tuple(map(int, version.groups())) >= (5, 38, 0),
            "internal-check comparison requires a recorded Perl version supporting PERL_RAND_SEED")
    for before, after in zip(runs, runs[1:]):
        require(before["finished_at_utc"] <= after["started_at_utc"],
                "ABBA run intervals overlap or are out of order")
    categories = sorted({category for run in runs for category in run["categories"]})
    category_results = {}
    for category in categories:
        values = [run["categories"].get(category, zero()) for run in runs]
        category_results[category] = {
            key: difference((values[0][key] + values[3][key]) / 2,
                            (values[1][key] + values[2][key]) / 2) for key in METRICS}
    return {"design": "ABBA internal checks 1,0,0,1; two observations per mode",
            "suite_wall_seconds": difference(
                (runs[0]["suite_wall_seconds"] + runs[3]["suite_wall_seconds"]) / 2,
                (runs[1]["suite_wall_seconds"] + runs[2]["suite_wall_seconds"]) / 2),
            "totals": {key: difference(
                (runs[0]["totals"][key] + runs[3]["totals"][key]) / 2,
                (runs[1]["totals"][key] + runs[2]["totals"][key]) / 2) for key in METRICS},
            "categories": category_results}


def build_report(inputs, comparison=False):
    expanded = expand_inputs(inputs)
    require(bool(expanded), "no input runs")
    baseline = comparable(expanded[0][1])
    runs = []
    first_expected = None
    for path, data in expanded:
        require(comparable(data) == baseline,
                f"{path}: capture metadata differs beyond internal-check mode and requested run count")
        run, expected = read_run(path, data)
        if first_expected is None:
            first_expected = expected
        require(expected == first_expected, f"{path}: tested script population differs")
        runs.append(run)
    report = {
        "schema_version": 1,
        "measurement_notes": [
            "Suite wall seconds cover the full make command before archival. Timed waits detect completion promptly; completion during a tally includes that remaining monitor work.",
            "Elapsed seconds sum is the sum of timed command records; parallel and nested commands can overlap.",
            "User/system seconds cover timed commands and their descendants, not untimed harness work; nested timers may double-count.",
            "Command-status notes include expected failing compiler commands; clean DejaGNU verdicts are checked separately.",
            "The comparison is descriptive; two observations per mode do not establish statistical significance.",
        ],
        "shared_configuration": baseline,
        "runs": runs,
    }
    if comparison:
        report["internal_check_comparison"] = compare(runs, expanded[0][1])
    return report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("inputs", type=Path, nargs="+")
    parser.add_argument("--compare-internal-checks", action="store_true",
                        help="require a comparable, sequential 1,0,0,1 ABBA experiment")
    parser.add_argument("--output", type=Path, help="write JSON to a new file instead of stdout")
    args = parser.parse_args()
    try:
        report = build_report(args.inputs, args.compare_internal_checks)
        output = json.dumps(report, indent=2, allow_nan=False) + "\n"
        if args.output:
            with args.output.open("x") as stream:
                stream.write(output)
        else:
            print(output, end="")
    except (OSError, ValueError, OverflowError) as error:
        parser.exit(1, f"timing report: {error}\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())
