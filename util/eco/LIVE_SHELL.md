# Live helper roles

`LiveShell` restores OLD names, declaration groups, and helper order around
CURRENT combinational value cones. It can recover correspondence when a changed
expression no longer matches by value but still occupies the same typed consumer
role. Candidate generation uses only retained native ASTs. There is no RTL
parser, direct-edit input, private benchmark rule, or compiler change.

The standard `--refinements standard` preset enables this external provider. It
keeps the `F22` and `F22Inactive` historical unused-helper policies disabled.
Standalone scripts may select `ECOASTLiveShell.liveShellProvider` with the
`LiveShell` feature. The usual `--structure ordinary|preserve` option still
controls conditional restoration separately.

The provider reuses the existing bounded pure-shell role proposals, then removes
disconnected OLD-only continuous helper cones before scoring or proof. It keeps
CURRENT's existing unused scaffold only when its name, declaration type/range,
and RHS remain unchanged. A conflicting scaffold causes the proposal to be
rejected. Deletion preserves surviving declaration groups and member order;
declaration partition remains part of the substantive diff metric.

Liveness covers the complete native AST, including simulation assertions, task
arguments, procedural uses, and non-expression references such as `dumpvars`.
It is independent of the translated-only cost objective. Only unique scalar
wire definitions may be removed. The provider rejects unsupported declarations,
fixed-port mismatches, directive comments, and the pure-shell provider's opaque
instance/function/DPI restrictions. OLD and CURRENT are each limited to 512
continuous scalar definitions, counted through native group/comment wrappers.
The fixed OLD bound makes larger modules ineligible throughout every search
round; enabling this provider cannot change their proposal lists.

Each proposal still requires the ordinary independent proof against CURRENT and
fresh saved-plan replay. The ordinary expression-order proposal is eligible for
strict four-state checking. The alternative that restores associative Boolean
operand order remains labelled binary-only and is available only when that proof
domain is explicitly selected. No temporal reachability, initialization,
physical ECO cost, timing, or global edit minimum is established by this rule.

`testsuite/bsc.eco/postba/eco_live_shell.exp` uses the existing DejaGNU/bluehs test
infrastructure. It checks transitive deletion, unchanged ordinary scaffold,
complete-AST liveness, guarded rejection, strict search and saved replay, and an
incorrect rewrite rejected by the independent solver. Private production cases
and their directed edits remain outside this repository.
