#!/usr/bin/env bash

# Check out the companion repository REPO (bdw, bsc-contrib, Toooba) beside
# this checkout, on the branch that goes with the bsc under test:
#
#   1. a branch of the same name as the bsc branch, in the fork of REPO owned
#      by whoever owns the bsc branch (for a pull request, the head's owner);
#   2. otherwise the default branch of the fork of REPO owned by the repository
#      this workflow runs in, when such a fork exists;
#   3. otherwise the default branch of OWNER/REPO, the upstream home.
#
# When the repository running the workflow is OWNER's own, or owns no fork of
# REPO, step 2 finds nothing and step 3 applies, which is the old behaviour.
# For B-Lang-org's CI that is every companion today: bdw and bsc-contrib are
# its own, and it has no Toooba. A fork running these workflows tests against
# its own goldens by default, and needs a same-named companion branch only
# where a bsc branch's output differs from them.
#
# The probes' stderr is left in the log on purpose: a repository that does
# not exist and one that could not be reached both fall through to the next
# step, and the log is what tells them apart.

set -e

OWNER=$1
REPO=$2

if [ -z "$OWNER" ] || [ -z "$REPO" ] ; then
    echo "Usage: $0 <owner> <repo>"
    exit 1;
fi

# Whether github.com/$1/REPO has any branch, which is to say exists.
has_repo () {
    local res
    res=$(GIT_TERMINAL_PROMPT=0 git ls-remote --heads "https://github.com/$1/${REPO}" || echo)
    [ -n "$res" ]
}

# Whether github.com/$1/REPO has a branch named $2.
has_branch () {
    local res
    res=$(GIT_TERMINAL_PROMPT=0 git ls-remote --heads "https://github.com/$1/${REPO}" "refs/heads/$2" || echo)
    [ -n "$res" ]
}

clone_default () {
    echo "Checking out the default branch of $1/${REPO}"
    git clone --recursive "https://github.com/$1/${REPO}" "../${REPO}"
}

clone_branch () {
    echo "Checking out branch $2 of $1/${REPO}"
    git clone --recursive --branch "$2" "https://github.com/$1/${REPO}" "../${REPO}"
}

# Steps 2 and 3.
default_checkout () {
    if [ -n "${GITHUB_REPOSITORY_OWNER}" ] && [ "${GITHUB_REPOSITORY_OWNER}" != "${OWNER}" ] ; then
        if has_repo "${GITHUB_REPOSITORY_OWNER}" ; then
            clone_default "${GITHUB_REPOSITORY_OWNER}"
            return
        fi
        echo "No ${GITHUB_REPOSITORY_OWNER}/${REPO} to fall back to"
    fi
    clone_default "${OWNER}"
}

# Step 1, then the rest. A branch literally named main is not looked up: a
# fork's main is a copy of upstream of some age, not a companion to a change.
user_branch_checkout () {
    if [ -n "$2" ] && [ "$2" != "main" ] ; then
        if has_branch "$1" "$2" ; then
            clone_branch "$1" "$2"
            return
        fi
        echo "No branch $2 in $1/${REPO}"
    fi
    default_checkout
}

if [ "${GITHUB_EVENT_NAME}" = "pull_request" ]; then
    if [ -z "${HEAD_OWNER}" ] ; then
        echo "HEAD_OWNER not defined in environment"
        # Single quotes: this is a GitHub expression for the workflow to set,
        # not a shell expansion.
        echo 'set it from ${{ github.event.pull_request.head.repo.owner.login }}'
        exit 1
    fi
    user_branch_checkout "${HEAD_OWNER}" "${GITHUB_HEAD_REF}"
elif [ "${GITHUB_EVENT_NAME}" = "push" ] || [ "${GITHUB_EVENT_NAME}" = "workflow_dispatch" ]; then
    # workflow_dispatch also sets GITHUB_REF_NAME (the ref dispatched on),
    # so release runs dispatched on a branch pick up name-matched
    # downstream branches exactly like a push of that branch would
    user_branch_checkout "${GITHUB_REPOSITORY_OWNER}" "${GITHUB_REF_NAME}"
else
    echo "GITHUB_EVENT_NAME: ${GITHUB_EVENT_NAME}"
    default_checkout
fi
