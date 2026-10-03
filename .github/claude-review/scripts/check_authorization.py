"""Validate the trusted BSC reviewer identity policy and GitHub environment metadata.

This read-only gate emits authorized=true/false for GitHub Actions. Empty policy
lists, malformed configuration, unknown identities, teams, or missing required
reviewers deny authorization. Administrator bypass is not exposed by the GitHub
REST environment schema and must be disabled in GitHub's environment settings.
"""

import argparse
import json
import re
import sys

ENVIRONMENT = "claude-pr-review"
POLICY_KEYS = {"environment", "authors", "requesters", "approvers"}
LOGIN = re.compile(r"[A-Za-z0-9][A-Za-z0-9-]*(?:\[bot\])?\Z")


def validate_policy(policy):
    if not isinstance(policy, dict) or set(policy) != POLICY_KEYS:
        raise ValueError("policy must contain only environment, authors, requesters, and approvers")
    if policy["environment"] != ENVIRONMENT:
        raise ValueError("policy environment must be " + ENVIRONMENT)
    for key in ("authors", "requesters", "approvers"):
        values = policy[key]
        if not isinstance(values, list) or any(not isinstance(value, str) or not LOGIN.fullmatch(value) for value in values):
            raise ValueError(key + " must be a list of GitHub logins")
        if len({value.casefold() for value in values}) != len(values):
            raise ValueError(key + " must not contain duplicate logins")
    return policy


def identities_authorized(policy, author, requester, initial_requester):
    validate_policy(policy)
    if not all(policy[key] for key in ("authors", "requesters", "approvers")):
        return False
    authors = {value.casefold() for value in policy["authors"]}
    requesters = {value.casefold() for value in policy["requesters"]}
    return bool(author and requester and initial_requester
                and author.casefold() in authors
                and requester.casefold() in requesters
                and initial_requester.casefold() in requesters)


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
        if not isinstance(login, str) or not LOGIN.fullmatch(login):
            return False
        logins.append(login.casefold())
    # GitHub requires any one listed reviewer, so unexpected reviewers would
    # provide an alternate approval path. Require the exact approved User set.
    return len(set(logins)) == len(logins) and set(logins) == {value.casefold() for value in policy["approvers"]}


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--policy", required=True)
    parser.add_argument("--author", required=True)
    parser.add_argument("--requester", required=True)
    parser.add_argument("--initial-requester", required=True)
    parser.add_argument("--identities-only", action="store_true")
    parser.add_argument("--environment-file")
    args = parser.parse_args(argv)
    authorized = False
    try:
        with open(args.policy, encoding="utf-8") as fh:
            policy = json.load(fh)
        if identities_authorized(policy, args.author, args.requester, args.initial_requester):
            if args.identities_only:
                authorized = True
            elif args.environment_file:
                with open(args.environment_file, encoding="utf-8") as fh:
                    authorized = environment_ready(policy, json.load(fh))
            else:
                raise ValueError("environment metadata is required")
    except (OSError, ValueError) as error:
        print("Authorization denied: " + str(error), file=sys.stderr)
    print("authorized=" + str(authorized).lower())
    if not authorized:
        print("Reviewer disabled: identity policy or required User reviewers are not ready.", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
