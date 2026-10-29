# Optional final-AST name correspondence

`ECOASTCorrespondence.correspondenceProvider` supplies bounded external naming proposals. It changes only names in CURRENT's native AST; it retains CURRENT declarations, widths, assignments, processes, state, ports and instance identity. The standard search must prove every candidate against CURRENT and reprove each saved step. OLD supplies naming evidence, never replacement values.

- `F24WidthRole`: recover a vacant OLD continuous-wire name after width growth when uniquely corresponding ordered leaves of the same existing assignment owner identify the role. Conflicting votes are reported and skipped.
- `F24ExactLeaf`: restore names for unique exact typed expressions over shared boundary values, slices and repetitions. Occupied eligible wire names move simultaneously into vacated names; ambiguous buckets and protected collisions are rejected. This does not expand arbitrary expressions or infer equivalence.

- `F24Layout`: optionally combine a naming proposal with up to four existing F04 historical-layout proposals. `F24Layout` is an explicit permission to compose the existing F04 generators. The public `correspondence.hs` runner enables it only when the input configuration permits F04. The joint candidate receives the same complete proof and uses the caller's selected objective and monotonic acceptance policy. The standalone `correspondence.hs` runner uses whole-file raw cost; `captures.hs --refinements standard` also supports the [translated objectives](README.md#synthesis-translated-search-objective).

Directive-bearing comments reject the entire proposal, and references from functions, DPI items and translate-off items reserve their names. Declaration groups and ordering change only in the explicitly enabled joint layout family.

Combine the provider with the standard provider, add the desired feature names to `soFeatures`, and include this module and the calling script in `cpSources`. Run the usual independent proof and saved-plan replay. This provider is optional and does not change the frozen default flow. Raw and normalized costs can move in opposite directions; report both against the same original OLD.

`correspondenceAudit` records proposals, ambiguity/collision reasons and base naming-family budget limitations (joint layout enumeration has its separate fixed bound). It is naming evidence, not a correctness certificate or an assertion of globally optimal correspondence. The directed `eco_correspondence.exp` group exercises strict proof, changed widths/truncation, exact permutation, displacement, ambiguity, duplicate declarations, invalid selects, macros and procedural writers.

`ECOASTValueNames.valueNamesProvider` is a separate optional family. `ValueNames`
recovers vacant OLD names for unique exact typed pure CURRENT wire expressions,
including their value reads in assertion/effect statements. It never copies an
OLD expression. Unlike the conservative F24 provider, a value use inside a
translate-off statement alone is not a naming barrier. Definitions must remain
in translated regions. The rewrite touches only declaration binders, writer
lvalues and variable-value reads; callable identifiers, strings, operator
annotations, VId provenance, events and flags are unchanged. Port/instance and
hierarchical references, functions/DPI, declaration ranges, arrays, initialized
wires, duplicate/procedural writers and ambiguous correspondence are rejected.
Every candidate still requires the unchanged complete CURRENT proof and replay.

`ValueNamesLayout` jointly applies at most four existing F04 layouts. It requires
`ValueNames` as well; the optional runner enables this composition only when F04
is enabled. `valueNamesAudit` records the pairs or a concrete eligibility reason.
No dead state or logic is introduced or retained. Report synthesized-region edit
counts when requested: assertion-only text is excluded, while translated helper
definitions remain counted. This naming rule does not prune their dead cones.
