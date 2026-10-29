# Existing alias orientation

The standard refinements preset includes `AliasOrientation`. This external
proposal handles a correlation lost when matching moves a value to an OLD
shared-helper identity and a later role-restoration rule recreates its original
helper as an alias. If CURRENT has `a = b; b = expression;` and OLD already had
`a = expression;`, the provider can propose `a = expression; b = a;`.

The expression always comes from CURRENT. OLD supplies an exact expression
match used to select the orientation. Both wires already exist, and all
consumers, declarations, grouping, state, control, and instances remain fixed.
This can restore an OLD helper definition without adding logic or inlining
anything. It is independent of optional historical conditional or unused-state
policies.

The proposer requires equal known widths, unique scalar wire declarations and
explicit writers, pure expressions, fixed ports, and no dependency cycle.
Ports cannot be reoriented. Writer checks include translated-off and procedural
scopes; child directions remain independent whole-module proof obligations.
Unsupported or directive-bearing inputs produce no proposal. It enumerates
individual eligible orientations and a joint proposal for disjoint pairs.
Conflicting alternatives remain separate candidates for the normal objective
and proof gate. There is no module-size restriction.

Every accepted proposal requires the unchanged independent CURRENT proof and
saved semantic/objective replay. This rule is eligible for strict four-state
checking and requires no binary assumptions or temporal contract. It does not
establish a globally minimal RTL edit or a physical ECO improvement.

Standalone scripts can select
`ECOASTAliasOrientation.aliasOrientationProvider` with the `AliasOrientation`
feature. The implementation is a bluehs SDK module, with no native compiler or
`.ba` format change. The DejaGNU `eco_alias_orientation.exp` test covers strict
search/replay, incorrect transfers, width/port/writer/cycle/effect guards,
conflicting and disjoint proposals, and modules larger than 512 helpers.
