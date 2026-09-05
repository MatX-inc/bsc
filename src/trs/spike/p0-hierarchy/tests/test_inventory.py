"""Validate the evidence index, not the correctness of its semantic claims."""

import json
from pathlib import Path
import subprocess
import unittest


SPIKE = Path(__file__).resolve().parents[1]
REPO = SPIKE.parents[3]
INVENTORY = json.loads((SPIKE / "inventory.json").read_text())


class InventoryTests(unittest.TestCase):
    def test_ids_are_unique_and_witness_references_resolve(self):
        rows = INVENTORY["inventory"]
        witnesses = INVENTORY["witnesses"]
        row_ids = {row["id"] for row in rows}
        witness_ids = {witness["id"] for witness in witnesses}
        self.assertEqual(len(rows), len(row_ids))
        self.assertEqual(len(witnesses), len(witness_ids))
        self.assertTrue(rows)
        for row in rows:
            with self.subTest(entry=row["id"]):
                self.assertTrue(row["witness_ids"])
                self.assertLessEqual(set(row["witness_ids"]), witness_ids)

    def test_required_fields_and_statuses_are_explicit(self):
        self.assertEqual(INVENTORY["schema_version"], 1)
        for row in INVENTORY["inventory"]:
            with self.subTest(entry=row["id"]):
                self.assertIn(row["status"], INVENTORY["status_vocabulary"])
                for key in ("category", "source", "required_interaction", "owner",
                            "oracle", "comparison", "evidence"):
                    self.assertTrue(row[key], key)
                self.assertIn("reduction", row)
                if row["category"] == "refusal":
                    self.assertTrue(row["classification"])

    def test_pinned_source_paths_exist_and_new_sources_are_marked(self):
        pinned = set()
        for row in INVENTORY["inventory"]:
            for source in row["source"]:
                with self.subTest(entry=row["id"], source=source):
                    self.assertTrue(source["symbol"])
                    if source["pin"] == "new_p0a_file":
                        self.assertTrue((REPO / source["path"]).is_file())
                    else:
                        self.assertIn(source["pin"], INVENTORY["pins"])
                        pinned.add((INVENTORY["pins"][source["pin"]], source["path"]))
        for pin, path in sorted(pinned):
            with self.subTest(pin=pin, path=path):
                result = subprocess.run(
                    ["git", "cat-file", "-e", f"{pin}:{path}"], cwd=REPO,
                    text=True, capture_output=True, timeout=10, check=False,
                )
                self.assertEqual(result.returncode, 0, result.stderr)

    def test_witness_paths_exist_or_are_explicitly_not_authored(self):
        for witness in INVENTORY["witnesses"]:
            with self.subTest(witness=witness["id"]):
                self.assertGreater(witness["run_bound_seconds"], 0)
                self.assertTrue(witness["oracle"])
                self.assertTrue(witness["comparison"])
                if witness["status"] == "not_authored":
                    self.assertIsNone(witness["path"])
                    self.assertIsNone(witness["pin"])
                elif witness["pin"] == "new_p0a_file":
                    self.assertTrue((REPO / witness["path"]).is_file())
                else:
                    pin = INVENTORY["pins"][witness["pin"]]
                    result = subprocess.run(
                        ["git", "cat-file", "-e", f"{pin}:{witness['path']}"],
                        cwd=REPO, text=True, capture_output=True,
                        timeout=10, check=False,
                    )
                    self.assertEqual(result.returncode, 0, result.stderr)

    def test_this_checkpoint_does_not_claim_closed_semantic_coverage(self):
        self.assertEqual(INVENTORY["verdict"], "blocked/inconclusive")
        coverage = INVENTORY["coverage"]
        for key in ("semantic_matrix_closed", "compiler_derived_extraction_demonstrated",
                    "p0b_interpreter_implemented", "composition_argument_reviewed"):
            self.assertIs(coverage[key], False)
        self.assertEqual(coverage["real_rtl_oracle_runs"], 0)
        self.assertEqual(coverage["real_verilator_runs"], 0)
        self.assertIs(coverage["no_p0_acceptance_from_model_success"], True)
        self.assertTrue(coverage["open_census_gates"])

    def test_only_the_hand_model_is_marked_run(self):
        ran = [w["id"] for w in INVENTORY["witnesses"] if w["status"] == "implemented_and_run"]
        self.assertEqual(ran, ["W_TOY"])
        for row in INVENTORY["inventory"]:
            if row["status"] == "disproved":
                self.assertIn("W_TOY", row["witness_ids"])
                self.assertEqual(row["category"], "candidate_counterexample")


if __name__ == "__main__":
    unittest.main()
