# Claude PR reviewer for BSC

A static first-pass reviewer ported from `MatX-inc/matx` to `MatX-inc/bsc`.
The review workflow is `.github/workflows/claude-review.yml`;
`.github/workflows/claude-review-tests.yml` validates its Python pipeline. Both
require the same author/requester policy and personal environment approval. This
directory owns the review policy, schemas, pipeline, and access policy.

## Review pipeline

The job builds a review packet from the PR description, commits, diff, and
already-present CI results. The model follows definitions and call sites using
only `Read`, `Glob`, `Grep`, and `LS`, then returns JSON. A separate model pass
tries to refute candidate findings. Python validates and renders the result;
only the workflow posts a PR review with line-anchored comments or a clean
summary. A run with no usable model output posts no review.

This is static review: it does not build bsc, run GHC or a solver, execute tests,
or simulate generated HDL. BSC's existing build/test CI remains the source of
execution results. A green workflow conclusion does not prove feedback was
delivered, and the reviewer never supplies a human approval.

## Maintained conventions

The packet always includes `DEVELOP.md` and `INSTALL.md`. Testsuite changes also
include `testsuite/README.md`; Haskell Language Server and tree-sitter changes
include their maintained utility READMEs. Change those documents when conventions
change. Update `select_convention_docs` in `scripts/make_review_packet.py` and its
tests when documents move; missing selected docs deliberately abort the packet.

Shards group compiler, library, HDL, runtime, solver, tests, build, and docs files
so unrelated source areas do not share one bounded diff budget. The model still
follows cross-shard behavior and contracts. `REVIEW.md` owns evidence, rebuttal,
and static-review rules; the schemas own the model output contracts.

## Triggers and deployment

Eligible PRs can be reviewed on open, reopen, or ready-for-review; automatic
review skips drafts. A PR conversation comment containing `@claude-review`
requests a review or re-review. Pushes only catch up an undelivered first review,
with a two-notice retry cap; after delivery, request a fresh pass explicitly.
Manual review dispatch accepts a PR number and must use the default branch.
A mention never bypasses author/requester restrictions.

`access-policy.json` separately lists approved PR authors, initial/current
requesters, and personal approvers. The initial policy allows only `nanavati` in
all three roles. Both workflows read the helper and policy from the default
branch on every path, so a PR or alternate dispatch branch cannot enroll itself.
Reruns require both the original requester and the person initiating the rerun
to be approved. Empty lists disable the workflows. Change these lists only in a
normal reviewed default-branch PR.

Before any model, comment-writing, or Python test job starts, a read-only gate
checks those identities and fetches the existing `claude-pr-review` environment
configuration. It requires exactly the policy's named **User** reviewers; missing
metadata, absent reviewers, teams, unexpected users, or API failure deny the
run. The protected job then waits for GitHub's actual environment approval. A
mention acknowledgment is posted only after that approval; requesting a run is
not approval to execute it.

An organization owner/repository admin must separately:

1. Give the official [Claude GitHub app](https://github.com/apps/claude) access to
   `MatX-inc/bsc`.
2. Open **bsc → Settings → Environments → New environment**, create the exact
   name `claude-pr-review`, enable **Required reviewers**, and select only the
   policy's approvers (initially `nanavati`). Do not include teams or extra users:
   GitHub requires any one listed reviewer, so those would offer an alternate
   approval path. Disable administrator bypass in the environment settings.
   Leave **Prevent self-review** unchecked if `nanavati` must personally approve
   runs they initiated; with only that requester/approver, blocking self-review
   leaves nobody eligible to approve.
3. In that environment's deployment branch rules, choose selected branches/tags
   and allow `main` plus the literal `refs/pull/*/merge` pattern. Main-only rules
   block `pull_request` jobs; issue comments and default-branch dispatch use main.
   See [GitHub's deployment branch rules](https://docs.github.com/actions/reference/workflows-and-actions/deployments-and-environments#deployment-branches-and-tags).
4. Add `CLAUDE_API_KEY_MONOREPO_CLAUDE_BOT` **only as an environment secret** inside
   `claude-pr-review`, using an approved Anthropic API key. Remove any repository
   binding with this name and exclude bsc from organization bindings with this
   name. A repository/organization secret could be used by a modified workflow
   without the approval environment; it would defeat approval-gated API access.
   Never store its value in source or chat.
5. Permit the pinned checkout and `anthropics/claude-code-action` revisions in
   the organization's Actions policy. Ensure `GH_UBUNTU_RUNNER`, if inherited,
   refers to an available runner; the fallback is GitHub-hosted `ubuntu-24.04`.
6. Protect the default branch so the executable reviewer and access policy can
   change only through the required human review process.

Required-reviewer protection for a private/internal repository needs an eligible
GitHub Enterprise plan; verify the feature is supported before deploying. The
gate deliberately refuses an unprotected or automatically created environment.
GitHub's documented REST environment response does not expose administrator
bypass; disabling bypass is a required manual setting, not an API-verified claim.
These restrictions apply to the two new Claude workflows. Existing compiler,
release, and mirror workflows are unchanged.

The review action needs `id-token: write` for GitHub app authentication. The
workflow separately uses GitHub's generated token for metadata and bounded PR
posting. The model steps use an Anthropic API key; the separate conversational
MatX `@claude` assistant's OAuth secret is not used here. No personal access token
or GHC installation is required for the reviewer.

The workflow pins Claude CLI `2.1.284`, model `claude-opus-4-8`, and effort `xhigh`
to match the inspected MatX reviewer. Verify that the configured Anthropic
account has access to that model.

## Trust boundaries

Only same-repository PR branches are supported; fork PRs are skipped on every
path, including mentions and dispatch. Bsc itself being a fork of upstream does
not matter: a branch created inside `MatX-inc/bsc` is eligible. Review upstream
changes after integrating them onto a same-repository branch.

The executable reviewer scripts, schemas, and rubric are overlaid from a
trusted branch. PR diff, source, description, and maintained docs are review
input, never permission to execute code. PR-supplied Claude settings and MCP
configuration are removed, git credentials are scrubbed before model reads,
and the CLI ignores project settings/MCP configuration.

The pinned action can otherwise put its app token in `.git/config` after the
workflow's credential scrub. `use_commit_signing: true` suppresses that write
in agent mode; its injected file-operations MCP tools are explicitly denied.
`--tools` limits built-in tools to the four read tools, in addition to the
read-only autoapproval list. The model has no Bash, Skill, file-edit, or posting
tools. Signing is not used to create commits.

This reviewer retains an internal, trusted-input assumption. Enabling external
fork/untrusted PR review needs a separately hardened design; do not remove the
fork gate or switch to `pull_request_target` to pass secrets to external code.
Protect the default branch carrying the executable reviewer and author policy.

## Local validation

From the repository root:

```bash
PYTHONDONTWRITEBYTECODE=1 python3 -m unittest discover \
  -s .github/claude-review/scripts -p 'test_*.py'
python3 .github/claude-review/scripts/make_review_packet.py \
  --base origin/main --head HEAD --out /tmp/bsc-review-packet.md
```

The Python pipeline is standard-library-only. Tests cover packet docs, BSC
routing, structured-output extraction, verifier result merging, inline anchors,
rendering, and delivery state. It has no Bazel dependency.

Merge setup to the default branch before testing production comment triggers:
GitHub loads that workflow from the default branch and the reviewer overlays
trusted machinery before using it. On a small eligible same-repository PR,
request a review and complete the configured approval. Check the gate, review
packet, model output, and delivery in **Actions → Claude PR Review**. A clean
review skips verification; candidate findings exercise the independent pass.
Then test the automatic trigger for an approved author's non-draft PR.

The GitHub app, environment secret, model access, approval protections, and
actual personal approval are external requirements; local unit tests cannot
establish those settings. On the initial setup PR, trusted default-branch
authorization machinery is not present yet, so both new workflows safely skip.
The documented local checks validate that PR before merge.
