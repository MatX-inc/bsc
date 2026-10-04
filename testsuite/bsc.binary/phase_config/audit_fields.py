"""Require an explicit decision for every legacy Flags field.

This is a coverage audit, not proof that a field belongs in the right phase.
VerifyPhaseConfig.hs tests representative dependency boundaries and adapters;
the compiler regressions test their behavior. New fields must reach a phase
projection or have a documented reason to stay at the invocation boundary.
"""

from pathlib import Path
import re
import sys


OUTSIDE_PHASES = {
    "codegenNames": "CLI work selection; selected module names are explicit inputs.",
    "elabOnly": "Driver/dependency output selection; decides whether generation runs.",
    "entry": "CLI work selection; the selected top is an explicit phase input.",
    "ifcPathRaw": "Decoder input; phases receive the resolved ifcPath.",
    "neatNames": "Legacy option with no active implementation reader.",
    "printFlags": "CLI preamble reporting before any phase starts.",
    "printFlagsHidden": "CLI preamble reporting before any phase starts.",
    "printFlagsRaw": "CLI preamble reporting before any phase starts.",
    "reverseInternOrder": "Process-start determinism probe read from raw argv.",
    "rstGate": "Disabled legacy option; AVerilog explicitly fixes it to False.",
    "updCheck": "Driver selects recursive dependency work before phase dispatch.",
    "vPathRaw": "Decoder input; phases receive the resolved vPath.",
}


def main():
    repo = Path(__file__).resolve().parents[3]
    flags_source = (repo / "src/comp/Flags.hs").read_text()
    phase_source = (repo / "src/comp/PhaseConfig.hs").read_text()
    record = re.search(
        r"data\s+Flags\s*=\s*Flags\s*\{(.*?)^\s*\}",
        flags_source,
        re.MULTILINE | re.DOTALL,
    )
    if record is None:
        sys.exit("FAIL: could not find the legacy Flags record for coverage audit")
    declared = set(re.findall(r"^\s*(\w+)\s*::", record.group(1), re.MULTILINE))
    # Match selector applications, not field names mentioned in documentation.
    projected = set(re.findall(r"\bF\.(\w+)\s+flags\b", phase_source)) & declared
    outside = set(OUTSIDE_PHASES)
    missing = declared - projected - outside
    stale = outside - declared
    redundant = outside & projected
    if missing or stale or redundant:
        for label, fields in [
            ("unclassified Flags fields", missing),
            ("removed fields with stale exemptions", stale),
            ("projected fields with stale exemptions", redundant),
        ]:
            if fields:
                print(f"FAIL: {label}: {', '.join(sorted(fields))}", file=sys.stderr)
        sys.exit(1)
    print(
        "PASS: phase configuration field inventory "
        f"({len(projected)} projected, {len(outside)} documented outside phases)"
    )


if __name__ == "__main__":
    main()
