#!/usr/bin/env python3
"""Audit exact-label repetition in a version-1 legacy verdict manifest.

Usage: audit-repeated-labels.py MANIFEST [--log-root ARCHIVED_RUN] > audit.json

The audit never merges checks. A group is one (test path, exact label) pair;
"excess" is occurrences minus one, not a count of redundant checks. Label
templates are descriptive text families, not classifications of test intent.
Optional log analysis classifies only the observed command text of repeated
Bluesim execution checks. It cannot establish that artifacts were unchanged.
"""

import argparse
from collections import Counter, defaultdict
import hashlib
import json
from pathlib import Path
import re


DISPOSITIONS = {
    "PASS", "FAIL", "XFAIL", "XPASS", "KFAIL", "KPASS", "UNRESOLVED",
    "UNTESTED", "UNSUPPORTED",
}
RESULT = re.compile(r"^(" + "|".join(sorted(DISPOSITIONS)) + r"): (.*)$")
SIM_LABEL = re.compile(r"^Bluesim simulation `[^']+' executes$")
VCD_OPTION = re.compile(r"(?<!\S)-V\s+\S+\s*")


def unique_object(pairs):
    obj = {}
    for key, value in pairs:
        if key in obj:
            raise ValueError(f"duplicate JSON key: {key}")
        obj[key] = value
    return obj


def read_manifest(path):
    data = path.read_bytes()
    manifest = json.loads(data, object_pairs_hook=unique_object)
    if (manifest.get("schema"), manifest.get("version"), manifest.get("identity")) != (
        "bsc-testsuite-verdicts", 1, "dejagnu-label-v1"
    ):
        raise ValueError("expected a version-1 dejagnu-label-v1 verdict manifest")
    tests = manifest["tests"]
    if len(tests) != len(set(tests)):
        raise ValueError("duplicate discovered test")
    groups = defaultdict(list)
    for verdict in manifest["verdicts"]:
        identity = verdict["id"]
        test, label = identity["test"], identity["label"]
        if test not in tests or not isinstance(label, str):
            raise ValueError("verdict has invalid test or label")
        if verdict["disposition"] not in DISPOSITIONS:
            raise ValueError("unknown disposition")
        groups[test, label].append(verdict)
    for key, values in groups.items():
        occurrences = [v["id"]["occurrence"] for v in values]
        if any(type(n) is not int for n in occurrences) or sorted(occurrences) != list(
            range(1, len(values) + 1)
        ):
            raise ValueError(f"invalid occurrence sequence: {key}")
    return manifest, groups, hashlib.sha256(data).hexdigest()


def totals(rows):
    return {
        "scripts": len({row["test"] for row in rows}),
        "groups": len(rows),
        "occurrences": sum(row["occurrences"] for row in rows),
        "excess": sum(row["occurrences"] - 1 for row in rows),
    }


def log_events(path):
    """Associate only directly preceding sim_output execution with its verdict.

    Running paths must be unambiguous filenames in this per-directory log.
    Unknown/missing associations are errors, never silently classified.
    """
    events = defaultdict(list)
    current = None
    command = None
    for number, line in enumerate(path.read_text().splitlines(), 1):
        if line.startswith("Running ") and line.endswith(" ..."):
            current = Path(line[8:-4]).name
            command = None
        if line.startswith("Executing "):
            command = None
            prefix = "Executing /r (sim_output): "
            if line.startswith(prefix):
                command = {"line": number, "text": line[len(prefix):]}
        result = RESULT.fullmatch(line)
        if result and SIM_LABEL.fullmatch(result[2]):
            if current is None or command is None:
                raise ValueError(f"unassociated simulation verdict at {path}:{number}")
            events[current, result[2]].append({
                "line": number, "disposition": result[1], "command": command,
            })
            command = None
    return events


def add_simulation_evidence(rows, log_root):
    cache = {}
    classified = defaultdict(list)
    for row in rows:
        if not SIM_LABEL.fullmatch(row["label"]):
            continue
        test = Path(row["test"])
        relative_log = test.parent / "testrun.log"
        path = log_root / relative_log
        if path not in cache:
            cache[path] = log_events(path)
        events = cache[path].get((test.name, row["label"]), [])
        if len(events) != row["occurrences"] or dict(Counter(
            event["disposition"] for event in events
        )) != row["dispositions"]:
            raise ValueError(f"log/manifest mismatch for {row['test']}: {row['label']}")
        commands = [event["command"]["text"] for event in events]
        # Compare text after removing the observed -V pathname argument. This
        # is not a Tcl/shell parser or a claim about hidden filesystem state.
        bases = {" ".join(VCD_OPTION.sub("", command).split()) for command in commands}
        vcd_count = sum(bool(VCD_OPTION.search(command)) for command in commands)
        if len(bases) == 1 and len(events) == 2 and vcd_count == 1:
            category = "one_plain_one_vcd_same_other_command_text"
        elif len(bases) == 1 and vcd_count:
            category = "vcd_and_repetition_same_other_command_text"
        elif vcd_count:
            category = "vcd_and_other_command_text_variants"
        else:
            category = "no_vcd_option_in_repeated_group"
        row["simulation_command_evidence"] = {
            "category": category, "log": str(relative_log), "events": events,
        }
        classified[category].append(row)
    return {category: totals(values) for category, values in sorted(classified.items())}


def audit(path, log_root=None):
    manifest, groups, digest = read_manifest(path)
    rows = [{
        "test": test, "label": label, "occurrences": len(values),
        "dispositions": dict(sorted(Counter(v["disposition"] for v in values).items())),
    } for (test, label), values in sorted(groups.items()) if len(values) > 1]
    by_script = defaultdict(list)
    by_template = defaultdict(list)
    for row in rows:
        by_script[row["test"]].append(row)
        by_template[re.sub(r"`[^']*'", "<?>", row["label"])].append(row)
    by_label = defaultdict(list)
    for (test, label), values in groups.items():
        by_label[label].append((test, len(values)))
    shared = {label: values for label, values in by_label.items() if len(values) > 1}
    shared_only = {label: values for label, values in shared.items()
                   if all(count == 1 for _, count in values)}

    def shared_totals(labels):
        return {
            "distinct_labels": len(labels),
            "scripts": len({test for values in labels.values() for test, _ in values}),
            "script_label_pairs": sum(len(values) for values in labels.values()),
            "occurrences": sum(count for values in labels.values() for _, count in values),
        }

    report = {
        "schema": "bsc-repeated-label-audit", "version": 1,
        "manifest_sha256": digest, "configuration": manifest["configuration"],
        "definitions": {
            "group": "one script path and exact label, with more than one occurrence",
            "excess": "occurrences minus groups; does not imply redundant checks",
            "cross_script": "shared wording across distinct scripts; not an identity collision",
            "templates": "descriptive label families, not inferred intent",
            "simulation_evidence": "command text only; does not prove unchanged artifacts",
        },
        "population": {"scripts": len(manifest["tests"]),
                       "checks": len(manifest["verdicts"]),
                       "script_label_pairs": len(groups),
                       "distinct_labels": len(by_label)},
        "within_script_repetition": totals(rows),
        "multiplicity_distribution": {str(n): count for n, count in sorted(
            Counter(row["occurrences"] for row in rows).items())},
        "dispositions_in_repeated_groups": dict(sorted(Counter(
            v["disposition"] for values in groups.values() if len(values) > 1
            for v in values).items())),
        "mixed_disposition_groups": sum(len(row["dispositions"]) > 1 for row in rows),
        "cross_script_shared_labels": shared_totals(shared),
        "cross_script_only_labels": shared_totals(shared_only),
        "label_templates": [{"template": template, **totals(values)}
                            for template, values in sorted(by_template.items(),
                                key=lambda item: (-len(item[1]), item[0]))],
        "scripts": [{"test": test, **totals(values)}
                    for test, values in sorted(by_script.items(),
                        key=lambda item: (-len(item[1]), item[0]))],
        "groups": rows,
    }
    if log_root is not None:
        report["simulation_command_categories"] = add_simulation_evidence(rows, log_root)
    return report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("manifest", type=Path)
    parser.add_argument("--log-root", type=Path)
    args = parser.parse_args()
    try:
        report = audit(args.manifest, args.log_root)
    except (ValueError, KeyError, TypeError, OSError) as error:
        parser.exit(1, f"audit failed: {error}\n")
    print(json.dumps(report, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
