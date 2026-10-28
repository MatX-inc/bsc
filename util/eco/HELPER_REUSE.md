# Reusing live historical helpers

`ECOASTHelperReuse.helperReuseProvider` proposes `F26LiveHelpers` candidates
through the normal external AST search. The implementation ports the qualified
live projection of the earlier helper refinement; it does not import the
unused-state checker, certificate, policy or special replay runner.

The complete candidate can change several helper drivers and consumers together.
It removes obsolete OLD state and OLD-only disconnected helpers while preserving
unchanged helper scaffolding in ordinary CURRENT. The usual search independently
proves CURRENT equivalence before accepting a cost improvement. Its saved plan
uses ordinary semantic replay and, when selected, the translated objective
witnesses. There is no separate raw-only selection path.

See [REFINEMENTS.md](REFINEMENTS.md) for the recommended combined command,
constraints, proof scope, policy switches and existing tests. OLD source and AST
structure are proposal evidence; a direct edit is never an input.
