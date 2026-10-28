# Compiler compatibility

The external utilities use the exact compiled `bsc` library exposed by bluehs.
`package-sdk.hs` binds that library, its runtime, compiler binary and policy
sources. A successful package identity check does not imply that captures from
another compiler version can be decoded or adapted. Native loading and
adaptation require the same full compiler version; a different producer version
requires fresh captures and separate qualification.

This series targets the upstream-oriented native ECO facilities reviewed in
MatX-inc/bsc #241–243 and the Verilog-info layer, after the shared bluehs
prerequisite. The optional #244 observation producer is not a dependency:
without it, invariant export reports unavailable-in-capture with zero facts.
The reviewed base
has positional or named instance parameters, untyped string parameters and
program-level DPI declarations. The external visitors preserve both instance
parameter forms and their names, values and directive-bearing comments; proof
checks retain fixed module parameters and opaque DPI identity.

The separately qualified preview prototype has a different Verilog AST:
instance parameters are named-only, string parameters carry a type flag, and
DPI declarations also appear at module scope. Its external string-parameter
macro renderer is not needed or representable as a typed-parameter transform
with this upstream AST. The external `ECOAST` facade delegates checked loading,
rendering, emission and verification to `ECO.VerilogInfo`. Its historical
`ASTRenderingPolicy` type remains only for external report compatibility; native
rendering has no policy switch. Filter identity, exact ordinary rendering and
candidate output checks remain enforced.
Public tests exercise the native string-parameter and program-level DPI forms.
They do not establish coverage of the preview-only type flag or module DPI.

Native modules use the `ECO.*` namespace. The in-memory rendering API is
`VerilogInfo`, with `vi*` selectors and `readVerilogInfo`, `emitVerilog` and
`verifyVerilogInfo`; existing `.eco-ast` artifacts and serialized envelope names
remain unchanged.

The upstream prerequisite retains the existing optional VProgram slot under
`bsc-ba-20260712-1`. The preview prototype extends its capture layout under
`bsc-ba-20261027-eco-1`, with different flags and AST codecs. No cross-version
capture compatibility is claimed. To qualify a new producer, regenerate its
captures, compare ordinary output with capture output, run native replay and
AST-render controls, then independently prove selected candidates and replay
their saved plans. Private corpus qualification from another producer keeps
that producer identity until this gate is rerun.

No new compiler pass, runtime, source parser or RTL parser is introduced by
this external stack. The package exposes policy extension points for matching,
rewrite proposals, costs and selection. Temporal contracts remain outside the
flow; binary and conservative settled four-state checks retain distinct claims.
