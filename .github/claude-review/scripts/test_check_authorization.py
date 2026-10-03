"""Regression tests for trigger identity bypasses and fail-closed approval metadata."""

import copy
import io
import json
import os
import sys
import tempfile
import unittest
from contextlib import redirect_stderr, redirect_stdout

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import check_authorization as auth


def policy():
    return {"environment": "claude-pr-review", "authors": ["author"], "requesters": ["requester"], "approvers": ["approver"]}


def environment():
    return {"name": "claude-pr-review", "protection_rules": [
        {"type": "required_reviewers", "reviewers": [{"type": "User", "reviewer": {"type": "User", "login": "approver"}}]}
    ]}


class TestIdentityAuthorization(unittest.TestCase):
    def test_author_and_both_initial_and_rerun_requesters_are_required(self):
        self.assertTrue(auth.identities_authorized(policy(), "author", "requester", "requester"))
        for author, requester, initial in (("outsider", "requester", "requester"),
                                           ("author", "outsider", "requester"),
                                           ("author", "requester", "outsider"),
                                           ("", "requester", "requester"),
                                           ("author", "", "requester")):
            with self.subTest(author=author, requester=requester, initial=initial):
                self.assertFalse(auth.identities_authorized(policy(), author, requester, initial))

    def test_login_matching_is_case_insensitive(self):
        self.assertTrue(auth.identities_authorized(policy(), "AUTHOR", "REQUESTER", "Requester"))

    def test_every_empty_list_disables_all_trigger_paths(self):
        for key in ("authors", "requesters", "approvers"):
            p = policy()
            p[key] = []
            self.assertFalse(auth.identities_authorized(p, "author", "requester", "requester"))

    def test_missing_or_malformed_policy_is_rejected(self):
        malformed = [[], {}, {**policy(), "unknown": []}, {**policy(), "environment": "unprotected"},
                     {**policy(), "authors": "author"}, {**policy(), "authors": ["author", "AUTHOR"]},
                     {**policy(), "requesters": ["requester\noutsider"]}, {**policy(), "approvers": [None]}]
        for p in malformed:
            with self.subTest(policy=p), self.assertRaises(ValueError):
                auth.validate_policy(p)


class TestEnvironmentApproval(unittest.TestCase):
    def test_documented_user_reviewer_metadata_is_ready(self):
        self.assertTrue(auth.environment_ready(policy(), environment()))

    def test_absent_environment_or_reviewers_never_auto_creates_an_approval_path(self):
        for env in ({}, {"name": "claude-pr-review"}, {"name": "claude-pr-review", "protection_rules": []},
                    {"name": "claude-pr-review", "protection_rules": [{"type": "wait_timer"}]},
                    {"name": "claude-pr-review", "protection_rules": [{"type": "required_reviewers", "reviewers": []}]}):
            with self.subTest(environment=env):
                self.assertFalse(auth.environment_ready(policy(), env))

    def test_unknown_user_or_team_cannot_supply_an_alternate_approval(self):
        for entry in ({"type": "User", "reviewer": {"type": "User", "login": "outsider"}},
                      {"type": "Team", "reviewer": {"type": "Team", "slug": "approver"}},
                      {"type": "User", "reviewer": {"type": "Bot", "login": "approver"}}):
            env = environment()
            env["protection_rules"][0]["reviewers"].append(entry)
            self.assertFalse(auth.environment_ready(policy(), env))

    def test_exact_user_set_and_correct_environment_are_required(self):
        p = policy()
        p["approvers"].append("another")
        self.assertFalse(auth.environment_ready(p, environment()))
        env = environment()
        env["name"] = "unprotected"
        self.assertFalse(auth.environment_ready(policy(), env))
        env = environment()
        env["protection_rules"].append(copy.deepcopy(env["protection_rules"][0]))
        self.assertFalse(auth.environment_ready(policy(), env))

    def test_administrator_bypass_is_not_falsely_claimed_api_verifiable(self):
        # GitHub's documented response has no administrator-bypass property.
        self.assertTrue(auth.environment_ready(policy(), environment()))


class TestCommandLineGate(unittest.TestCase):
    def test_missing_policy_or_environment_file_emits_false(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = os.path.join(tmp, "policy.json")
            with open(path, "w", encoding="utf-8") as fh:
                json.dump(policy(), fh)
            common = ["--author", "author", "--requester", "requester", "--initial-requester", "requester"]
            for args in (["--policy", os.path.join(tmp, "missing")], ["--policy", path],
                         ["--policy", path, "--environment-file", os.path.join(tmp, "missing")]):
                out = io.StringIO()
                with redirect_stdout(out), redirect_stderr(io.StringIO()):
                    self.assertEqual(auth.main(args + common), 0)
                self.assertEqual(out.getvalue(), "authorized=false\n")

    def test_full_gate_authorizes_only_after_environment_check(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = os.path.join(tmp, "policy.json")
            envpath = os.path.join(tmp, "environment.json")
            for filename, value in ((path, policy()), (envpath, environment())):
                with open(filename, "w", encoding="utf-8") as fh:
                    json.dump(value, fh)
            out = io.StringIO()
            with redirect_stdout(out):
                auth.main(["--policy", path, "--author", "author", "--requester", "requester",
                           "--initial-requester", "requester", "--environment-file", envpath])
            self.assertEqual(out.getvalue(), "authorized=true\n")


if __name__ == "__main__":
    unittest.main()
