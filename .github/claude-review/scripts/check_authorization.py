"""Preflight personal approval and verify active MatX organization membership.

Preflight emits ready=true/false using trusted policy, non-bot identity syntax,
and documented environment metadata. It does not establish org membership.
After actual environment approval, membership mode emits authorized=true/false
using the dedicated environment secret; GitHub's repository token is not a
fallback. Administrator bypass must be disabled manually in GitHub's UI.
"""

import argparse
import json
import os
import re
import subprocess
import sys

ENVIRONMENT = "claude-pr-review"
ORGANIZATION = "MatX-inc"
MEMBERS_TOKEN = "MATX_ORG_MEMBERS_READ_TOKEN"
POLICY_KEYS = {"environment", "organization", "approvers"}
LOGIN = re.compile(r"[A-Za-z0-9](?:[A-Za-z0-9-]{0,37}[A-Za-z0-9])?\Z")


def valid_login(login):
    return isinstance(login, str) and bool(LOGIN.fullmatch(login)) and "--" not in login


def validate_policy(policy):
    if not isinstance(policy, dict) or set(policy) != POLICY_KEYS:
        raise ValueError("policy must contain only environment, organization, and approvers")
    if policy["environment"] != ENVIRONMENT or policy["organization"] != ORGANIZATION:
        raise ValueError("policy must use claude-pr-review and MatX-inc")
    values = policy["approvers"]
    if not isinstance(values, list) or any(not valid_login(value) for value in values):
        raise ValueError("approvers must be a list of human GitHub logins")
    if len({value.casefold() for value in values}) != len(values):
        raise ValueError("approvers must not contain duplicate logins")
    return policy


def identities_ready(policy, author, requester, initial_requester):
    validate_policy(policy)
    return bool(policy["approvers"]) and all(valid_login(login) for login in (author, requester, initial_requester))


def environment_ready(policy, environment):
    validate_policy(policy)
    if not policy["approvers"] or not isinstance(environment, dict) or environment.get("name") != ENVIRONMENT:
        return False
    rules = environment.get("protection_rules")
    if not isinstance(rules, list):
        return False
    reviewer_rules = [rule for rule in rules if isinstance(rule, dict) and rule.get("type") == "required_reviewers"]
    if len(reviewer_rules) != 1:
        return False
    reviewers = reviewer_rules[0].get("reviewers")
    if not isinstance(reviewers, list) or not reviewers:
        return False
    logins = []
    for entry in reviewers:
        if not isinstance(entry, dict) or entry.get("type") != "User":
            return False
        reviewer = entry.get("reviewer")
        if not isinstance(reviewer, dict) or reviewer.get("type") != "User":
            return False
        login = reviewer.get("login")
        if not valid_login(login):
            return False
        logins.append(login.casefold())
    # GitHub requires any one listed reviewer; unexpected users are an alternate
    # approval path, so require the exact configured personal User reviewer set.
    return len(set(logins)) == len(logins) and set(logins) == {value.casefold() for value in policy["approvers"]}


def active_membership(response, organization, login):
    if not isinstance(response, dict) or response.get("state") != "active" or response.get("role") not in ("member", "admin"):
        return False
    org = response.get("organization")
    user = response.get("user")
    return bool(isinstance(org, dict) and isinstance(org.get("login"), str)
                and org["login"].casefold() == organization.casefold()
                and isinstance(user, dict) and user.get("type") == "User"
                and isinstance(user.get("login"), str)
                and user["login"].casefold() == login.casefold())


def memberships_authorized(policy, author, requester, initial_requester):
    if not identities_ready(policy, author, requester, initial_requester):
        return False
    token = os.environ.get(MEMBERS_TOKEN)
    if not token:
        return False
    api_environment = os.environ.copy()
    api_environment["GH_TOKEN"] = token
    api_environment.pop("GITHUB_TOKEN", None)
    cache = {}
    for login in (author, initial_requester, requester):
        key = login.casefold()
        if key not in cache:
            try:
                response = subprocess.run(
                    ["gh", "api", "--hostname", "github.com",
                     "orgs/" + policy["organization"] + "/memberships/" + login,
                     "-H", "Accept: application/vnd.github+json",
                     "-H", "X-GitHub-Api-Version: 2022-11-28"],
                    env=api_environment, check=True, capture_output=True, text=True, timeout=30,
                )
                cache[key] = active_membership(json.loads(response.stdout), policy["organization"], login)
            except (OSError, ValueError, subprocess.SubprocessError):
                # Never echo API output, exception text, or the token-bearing environment.
                cache[key] = False
        if not cache[key]:
            return False
    return True


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--policy", required=True)
    parser.add_argument("--author", required=True)
    parser.add_argument("--requester", required=True)
    parser.add_argument("--initial-requester", required=True)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--preflight", action="store_true")
    mode.add_argument("--memberships", action="store_true")
    parser.add_argument("--environment-file")
    args = parser.parse_args(argv)
    allowed = False
    try:
        with open(args.policy, encoding="utf-8") as fh:
            policy = json.load(fh)
        if args.preflight:
            if identities_ready(policy, args.author, args.requester, args.initial_requester) and args.environment_file:
                with open(args.environment_file, encoding="utf-8") as fh:
                    allowed = environment_ready(policy, json.load(fh))
        else:
            allowed = memberships_authorized(policy, args.author, args.requester, args.initial_requester)
    except (OSError, ValueError):
        # Configuration errors can include untrusted text; report no values.
        pass
    output = "ready" if args.preflight else "authorized"
    print(output + "=" + str(allowed).lower())
    if not allowed:
        message = "Approval preflight is not ready." if args.preflight else "Active MatX-inc membership could not be verified for all three identities."
        print(message, file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
