"""Regression tests for org eligibility, personal approval, and credential failures."""

import copy
import io
import json
import os
import subprocess
import sys
import tempfile
import types
import unittest
from contextlib import redirect_stderr, redirect_stdout
from unittest.mock import patch

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import check_authorization as auth


def policy():
    return {"environment": "claude-pr-review", "organization": "MatX-inc", "approvers": ["nanavati"]}


def environment():
    return {"name": "claude-pr-review", "protection_rules": [
        {"type": "required_reviewers", "reviewers": [{"type": "User", "reviewer": {"type": "User", "login": "nanavati"}}]}
    ]}


def membership(login, **updates):
    result = {"state": "active", "role": "member", "visibility": "private",
              "organization": {"login": "MatX-inc"}, "user": {"login": login, "type": "User"}}
    result.update(updates)
    return result


def api_response(command, **kwargs):
    login = command[4].rsplit("/", 1)[-1]
    return types.SimpleNamespace(stdout=json.dumps(membership(login)))


class TestApprovalPreflight(unittest.TestCase):
    def test_any_valid_human_identity_is_syntactically_ready(self):
        # Preflight deliberately does not claim these people are org members.
        self.assertTrue(auth.identities_ready(policy(), "org-member", "requester", "rerunner"))

    def test_bots_and_malformed_logins_are_rejected_before_api(self):
        invalid = ("", "github-actions[bot]", "user/name", "user\nname", " user", "user ",
                   "-user", "user-", "user--name", "under_score", "a" * 40, None)
        for value in invalid:
            for position in range(3):
                logins = ["author", "requester", "original"]
                logins[position] = value
                with self.subTest(login=value, position=position):
                    self.assertFalse(auth.identities_ready(policy(), *logins))

    def test_empty_approvers_and_malformed_policies_fail_closed(self):
        self.assertFalse(auth.identities_ready({**policy(), "approvers": []}, "author", "requester", "original"))
        invalid = [[], {}, {**policy(), "authors": ["nanavati"]}, {**policy(), "environment": "unprotected"},
                   {**policy(), "organization": "other-org"}, {**policy(), "approvers": "nanavati"},
                   {**policy(), "approvers": ["nanavati", "NANAVATI"]}, {**policy(), "approvers": ["bot[bot]"]}]
        for value in invalid:
            with self.subTest(policy=value), self.assertRaises(ValueError):
                auth.validate_policy(value)

    def test_exact_personal_user_reviewer_metadata_is_ready(self):
        self.assertTrue(auth.environment_ready(policy(), environment()))

    def test_absent_environment_or_reviewers_cannot_supply_approval(self):
        for env in ({}, {"name": "claude-pr-review"}, {"name": "claude-pr-review", "protection_rules": []},
                    {"name": "claude-pr-review", "protection_rules": [{"type": "wait_timer"}]},
                    {"name": "claude-pr-review", "protection_rules": [{"type": "required_reviewers", "reviewers": []}]}):
            with self.subTest(environment=env):
                self.assertFalse(auth.environment_ready(policy(), env))

    def test_other_user_or_team_cannot_supply_alternate_approval(self):
        for entry in ({"type": "User", "reviewer": {"type": "User", "login": "outsider"}},
                      {"type": "Team", "reviewer": {"type": "Team", "slug": "nanavati"}},
                      {"type": "User", "reviewer": {"type": "Bot", "login": "nanavati"}}):
            env = environment()
            env["protection_rules"][0]["reviewers"].append(entry)
            self.assertFalse(auth.environment_ready(policy(), env))

    def test_correct_environment_and_single_reviewer_rule_are_required(self):
        env = environment()
        env["name"] = "unprotected"
        self.assertFalse(auth.environment_ready(policy(), env))
        env = environment()
        env["protection_rules"].append(copy.deepcopy(env["protection_rules"][0]))
        self.assertFalse(auth.environment_ready(policy(), env))

    def test_administrator_bypass_is_not_falsely_claimed_api_verifiable(self):
        self.assertTrue(auth.environment_ready(policy(), environment()))


class TestActiveOrganizationMembership(unittest.TestCase):
    def test_active_private_member_and_owner_are_eligible_case_insensitively(self):
        for role in ("member", "admin"):
            response = membership("MixedCase", role=role, organization={"login": "MATX-INC"})
            self.assertTrue(auth.active_membership(response, "MatX-inc", "mixedcase"))

    def test_outside_pending_removed_bot_and_wrong_identity_responses_are_denied(self):
        invalid = [None, {}, membership("author", state="pending"), membership("author", state="inactive"),
                   membership("author", role="billing_manager"), membership("different"),
                   membership("author", organization={"login": "another-org"}),
                   membership("author", user={"login": "author", "type": "Bot"}),
                   membership("author", user={"login": "author"}), membership("author", organization={})]
        for response in invalid:
            with self.subTest(response=response):
                self.assertFalse(auth.active_membership(response, "MatX-inc", "author"))

    @patch.dict(os.environ, {auth.MEMBERS_TOKEN: "synthetic-members-token"})
    @patch.object(auth.subprocess, "run", side_effect=api_response)
    def test_all_three_distinct_identities_are_checked(self, run):
        self.assertTrue(auth.memberships_authorized(policy(), "author", "rerunner", "original"))
        self.assertEqual([call.args[0][4] for call in run.call_args_list],
                         ["orgs/MatX-inc/memberships/author", "orgs/MatX-inc/memberships/original", "orgs/MatX-inc/memberships/rerunner"])
        for call in run.call_args_list:
            self.assertEqual(call.kwargs["env"]["GH_TOKEN"], "synthetic-members-token")
            self.assertNotIn("GITHUB_TOKEN", call.kwargs["env"])
            self.assertNotIn("synthetic-members-token", " ".join(call.args[0]))
            self.assertEqual(call.args[0][2:4], ["--hostname", "github.com"])
            self.assertTrue(call.kwargs["check"])

    @patch.dict(os.environ, {auth.MEMBERS_TOKEN: "synthetic-members-token"})
    def test_each_initial_current_and_author_membership_is_required(self):
        for outsider in ("author", "original", "rerunner"):
            def response(command, **kwargs):
                login = command[4].rsplit("/", 1)[-1]
                return types.SimpleNamespace(stdout=json.dumps(membership(login, state="pending" if login == outsider else "active")))
            with self.subTest(outsider=outsider), patch.object(auth.subprocess, "run", side_effect=response):
                self.assertFalse(auth.memberships_authorized(policy(), "author", "rerunner", "original"))

    @patch.dict(os.environ, {auth.MEMBERS_TOKEN: "synthetic-members-token"})
    @patch.object(auth.subprocess, "run", side_effect=api_response)
    def test_duplicate_logins_are_cached_only_within_each_run(self, run):
        self.assertTrue(auth.memberships_authorized(policy(), "Author", "AUTHOR", "author"))
        self.assertEqual(run.call_count, 1)
        self.assertTrue(auth.memberships_authorized(policy(), "Author", "AUTHOR", "author"))
        self.assertEqual(run.call_count, 2)

    @patch.dict(os.environ, {"GH_TOKEN": "repository-token", auth.MEMBERS_TOKEN: ""})
    @patch.object(auth.subprocess, "run")
    def test_missing_dedicated_secret_never_falls_back_to_repository_token(self, run):
        self.assertFalse(auth.memberships_authorized(policy(), "author", "requester", "original"))
        run.assert_not_called()

    @patch.dict(os.environ, {auth.MEMBERS_TOKEN: "synthetic-members-token"})
    def test_removed_members_permission_errors_expiry_and_network_errors_fail_closed(self):
        failures = [subprocess.CalledProcessError(code, ["gh"], stderr="synthetic-members-token") for code in (1, 403, 404)]
        failures += [subprocess.TimeoutExpired(["gh"], 30), OSError("gh missing")]
        for failure in failures:
            with self.subTest(error=type(failure).__name__), patch.object(auth.subprocess, "run", side_effect=failure):
                self.assertFalse(auth.memberships_authorized(policy(), "author", "requester", "original"))

    @patch.dict(os.environ, {auth.MEMBERS_TOKEN: "synthetic-members-token"})
    def test_malformed_api_json_fails_closed(self):
        for output in ("not JSON", "null", "[]", '{"state":"active"}'):
            with self.subTest(output=output), patch.object(auth.subprocess, "run", return_value=types.SimpleNamespace(stdout=output)):
                self.assertFalse(auth.memberships_authorized(policy(), "author", "requester", "original"))

    @patch.dict(os.environ, {auth.MEMBERS_TOKEN: "synthetic-members-token"})
    @patch.object(auth.subprocess, "run")
    def test_invalid_actor_or_bot_never_reaches_membership_api(self, run):
        self.assertFalse(auth.memberships_authorized(policy(), "github-actions[bot]", "requester", "original"))
        run.assert_not_called()


class TestCommandLineModes(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.policy_path = os.path.join(self.tmp.name, "policy.json")
        self.environment_path = os.path.join(self.tmp.name, "environment.json")
        for path, value in ((self.policy_path, policy()), (self.environment_path, environment())):
            with open(path, "w", encoding="utf-8") as fh:
                json.dump(value, fh)
        self.common = ["--policy", self.policy_path, "--author", "author", "--requester", "requester", "--initial-requester", "original"]

    @patch.object(auth.subprocess, "run")
    def test_preflight_only_reports_readiness_and_never_queries_members(self, run):
        output = io.StringIO()
        with redirect_stdout(output):
            auth.main(self.common + ["--preflight", "--environment-file", self.environment_path])
        self.assertEqual(output.getvalue(), "ready=true\n")
        run.assert_not_called()

    def test_missing_preflight_policy_or_environment_file_reports_not_ready(self):
        for arguments in (self.common + ["--preflight"], self.common + ["--preflight", "--environment-file", "/missing/environment"]):
            output = io.StringIO()
            with redirect_stdout(output), redirect_stderr(io.StringIO()):
                auth.main(arguments)
            self.assertEqual(output.getvalue(), "ready=false\n")

    @patch.dict(os.environ, {auth.MEMBERS_TOKEN: "synthetic-members-token"})
    @patch.object(auth.subprocess, "run", side_effect=api_response)
    def test_membership_mode_reports_actual_authorization(self, run):
        output = io.StringIO()
        with redirect_stdout(output):
            auth.main(self.common + ["--memberships"])
        self.assertEqual(output.getvalue(), "authorized=true\n")

    @patch.dict(os.environ, {auth.MEMBERS_TOKEN: "synthetic-members-token"})
    @patch.object(auth.subprocess, "run", side_effect=subprocess.CalledProcessError(1, ["gh"], stderr="synthetic-members-token"))
    def test_api_failure_and_credential_are_not_echoed(self, run):
        output, errors = io.StringIO(), io.StringIO()
        with redirect_stdout(output), redirect_stderr(errors):
            auth.main(self.common + ["--memberships"])
        self.assertEqual(output.getvalue(), "authorized=false\n")
        self.assertNotIn("synthetic-members-token", output.getvalue() + errors.getvalue())


if __name__ == "__main__":
    unittest.main()
