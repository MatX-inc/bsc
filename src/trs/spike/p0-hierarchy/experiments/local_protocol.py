#!/usr/bin/env python3
"""P0a hand-authored models. NOT BSC extraction, RTL parity, or a P0 pass.

Print one JSON object to stdout. No external dependencies or file writes.
The two supplied public contracts intentionally leave general admission,
effects, ME, clocks and progress out of this experiment.
"""

import hashlib
import json
from dataclasses import dataclass


@dataclass
class Counts:
    boundary_calls: int = 0
    local_steps: int = 0
    guards: int = 0


PUBLIC_CONTRACT = b'{"version":"toy-1","methods":["start","done"]}'
WRAPPER_ARTIFACT = b"toy wrapper: forward start/done to one direct child"


class RelayLeaf:
    """A hand-supplied leaf with R private pulse relay operations."""

    def __init__(self, private_length, counts):
        assert private_length >= 1
        self._program = tuple(range(private_length))
        self._counts = counts
        self._input = False
        self._output = False
        self._completed = False

    def start(self):
        self._counts.boundary_calls += 1
        self._input = True

    def _advance(self, frozen_guard=None):
        if self._completed:
            return
        active = self._input if frozen_guard is None else frozen_guard
        for _ in self._program:
            self._counts.local_steps += 1
        self._output = active
        self._completed = True

    def done(self):
        self._counts.boundary_calls += 1
        self._advance()
        return self._output

    def atomic_tick(self):
        """Deliberately inadequate run-once candidate for comparison."""
        self._advance()

    def freeze_all_guards_then_tick(self):
        """Deliberately inadequate blanket pre-edge predicate snapshot."""
        self._advance(frozen_guard=False)


class Forwarder:
    """Calls only its direct child's public methods; owns no child plan."""

    artifact = WRAPPER_ARTIFACT
    contract = PUBLIC_CONTRACT

    def __init__(self, child, counts):
        self._child = child
        self._counts = counts

    def start(self):
        self._counts.boundary_calls += 1
        self._child.start()

    def done(self):
        self._counts.boundary_calls += 1
        return self._child.done()


class AtomicRelayLeaf(RelayLeaf):
    """Only atomic_tick runs work; observations never advance the owner."""

    def done(self):
        self._counts.boundary_calls += 1
        return self._output


def parent_interaction(child):
    child.start()
    return child.done()


class ConditionalLeaf:
    """Public early/late surface, not a general interpreter.

    A private update samples a -> b between early (captures b) and late
    (writes a). No private operation is returned to the caller.
    """

    def __init__(self):
        self._a, self._b, self._captured = 7, 3, 0
        self._early_closed = False
        self._completed = False

    def begin(self):
        self._early_closed = False
        self._completed = False

    def early(self):
        assert not self._early_closed and not self._completed
        self._captured = self._b

    def close_early(self):
        self._early_closed = True

    def _advance(self):
        if not self._early_closed:
            return False
        if not self._completed:
            self._b = self._a
            self._completed = True
        return True

    def late(self):
        if not self._advance():
            return "Pending(early-use-closure)"
        self._a = 0
        return "Ready"

    def finish_edge(self):
        assert self._advance()

    def observe_state(self):
        # Public observation for this fixture, never used as scheduling data.
        return [self._a, self._b, self._captured]


class ConditionalParent:
    """Owns its choice; caller sees neither a choice vector nor a plan."""

    def __init__(self, counts):
        self._child = ConditionalLeaf()
        self._counts = counts
        self._x = 9
        self._copied = 0

    def tick(self, c):
        self._counts.guards += 1
        choice = bool(c)  # specified pre-edge Boolean input in THIS model
        self._child.begin()
        if choice:
            self._child.close_early()
        # first: always executes before second, regardless of c
        self._copied = self._x
        if choice:
            assert self._child.late() == "Ready"
        # second: still executes when first fires
        self._x = 0
        if not choice:
            self._child.early()
            self._child.close_early()
        self._child.finish_edge()
        return self._child.observe_state()


def check_counterexamples():
    first = AtomicRelayLeaf(1, Counts())
    first.atomic_tick()
    child_before_parent = parent_interaction(first)

    # Parent's observation occurs before the run-once child tick.
    last = AtomicRelayLeaf(1, Counts())
    child_after_parent = parent_interaction(last)
    last.atomic_tick()

    frozen = RelayLeaf(1, Counts())
    frozen.start()
    frozen.freeze_all_guards_then_tick()
    blanket_snapshot = frozen.done()
    rendezvous = parent_interaction(RelayLeaf(1, Counts()))
    assert [child_before_parent, child_after_parent, blanket_snapshot,
            rendezvous] == [False, False, False, True]

    waiting = ConditionalLeaf()
    waiting.begin()
    union_wait = waiting.late()
    assert union_wait == "Pending(early-use-closure)"
    mode_true = ConditionalParent(Counts()).tick(True)
    mode_false = ConditionalParent(Counts()).tick(False)
    assert mode_true == [0, 7, 0]
    assert mode_false == [7, 7, 3]
    return {
        "atomic_child_before": child_before_parent,
        "atomic_child_after": child_after_parent,
        "blanket_guard_snapshot": blanket_snapshot,
        "public_observation_frontier": rendezvous,
        "unclosed_inactive_use": union_wait,
        "conditional_true_state": mode_true,
        "conditional_false_state": mode_false,
        "disproved_candidates": ["child-run-once", "blanket-guard-snapshot"],
        "claim_limit": "Supplied toy contracts only; no actual BSC/RTL run",
    }


def check_ladders():
    depth_rows = []
    for depth in [1, 2, 4, 8, 16, 32]:
        counts = Counts()
        child = RelayLeaf(8, counts)
        for _ in range(depth):
            child = Forwarder(child, counts)
        assert parent_interaction(child)
        assert counts.boundary_calls == 2 * (depth + 1)
        assert counts.local_steps == 8
        depth_rows.append({"D_wrappers": depth, **vars(counts)})

    hidden_rows = []
    for length in [8, 32, 128, 512]:
        counts = Counts()
        parent = Forwarder(RelayLeaf(length, counts), counts)
        assert parent_interaction(parent)
        assert counts.local_steps == length
        assert counts.boundary_calls == 4
        hidden_rows.append({
            "R": length, **vars(counts),
            "parent_artifact_sha256": hashlib.sha256(parent.artifact).hexdigest(),
            "contract_sha256": hashlib.sha256(parent.contract).hexdigest(),
        })

    repeated_rows = []
    for n in [1, 2, 4, 8, 16, 32, 64, 128]:
        counts = Counts()
        instances = [Forwarder(RelayLeaf(8, counts), counts) for _ in range(n)]
        for instance in instances:
            assert parent_interaction(instance)
            assert instance.artifact is WRAPPER_ARTIFACT
        assert counts.boundary_calls == 4 * n
        assert counts.local_steps == 8 * n
        repeated_rows.append({"N": n, "shared_wrapper_objects": 1, **vars(counts)})

    choice_rows = []
    for k in [1, 2, 4, 8, 16, 32, 64]:
        counts = Counts()
        instances = [ConditionalParent(counts) for _ in range(k)]
        for edge in range(4):
            for index, instance in enumerate(instances):
                instance.tick((index + edge) % 2)
        assert counts.guards == 4 * k
        choice_rows.append({"K": k, "edges": 4, "guard_evaluations": counts.guards})
    return {"depth": depth_rows, "hidden": hidden_rows,
            "repeated": repeated_rows, "independent_choices": choice_rows}


def check_public_route_guard():
    def resolve_public_method(name):
        if name not in {"start", "done"}:
            raise ValueError("not a declared direct-child public method")
        return name

    assert resolve_public_method("done") == "done"
    try:
        resolve_public_method("child.grandchild.RL_private")
    except ValueError:
        return {"deliberately_forbidden_route_rejected": True,
                "claim_limit": "Toy API check, not reachable-code/dependency audit"}
    raise AssertionError("forbidden route was accepted")


def main():
    if not __debug__:
        raise RuntimeError("P0a model checks require Python assertions; disable optimization")
    report = {
        "schema_version": 1,
        "evidence_kind": "P0a hand-authored executable model",
        "p0_pass": False,
        "compiler_derived": False,
        "rtl_oracle_compared": False,
        "model_checks_passed": True,
        "counterexamples": check_counterexamples(),
        "toy_ladders": check_ladders(),
        "route_guard": check_public_route_guard(),
    }
    print(json.dumps(report, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
