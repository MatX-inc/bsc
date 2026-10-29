# Preserving live packed-output aliases

`ECOASTLiveAliases.liveAliasProvider` is an optional external candidate provider.
Pass its source paths for audit, combine it with the other providers, and enable
`LiveAliases` in the search policy. It uses only OLD and CURRENT native ASTs.

A pipeline insertion can leave an OLD packed output view feeding a new stage
while outputs read the final stage directly. The provider preserves that OLD
view by rebinding its existing data/valid aliases to the CURRENT output values,
restoring the OLD output field selections, and reconnecting the new stage's
inputs to the aliases' previous CURRENT values. These changes must be proposed
jointly: moving the aliases alone changes the pipeline's transition function.

The rule accepts complete fields of private, exactly sized, uniquely driven
concatenation wires. It requires fixed module ports, unchanged historical
packing, compatible existing alias widths, and new consumer owners for each
rebound alias. It does not create or remove declarations, helpers, state,
processes, initializers, instance identities, or control logic. Every currently
live continuous owner must remain live. Protected native regions and directive
comments prevent the proposal. Combined and individual proposals permit the
normal search cost policy to select a useful subset.

The candidate is not a proof. The normal independent AST checker validates its
complete observable combinational and next-state behavior against CURRENT.
The checker separately recognizes exact typed concatenation-field projections;
this identity copies every 0/1/X/Z bit and does not assume reset, reachability,
or a don't-care value. Partial and cross-field slices, resizing, and unsupported
constructs receive no additional proof freedom. Saved-plan replay needs no
candidate provider and checks the concrete candidate again.

`eco_live_aliases.check` runs a generic synthetic pipeline example, candidate
rejection controls, a deliberately incorrect reconnection, cost checking, and
saved-plan replay through the existing DejaGNU infrastructure. `eco_proof.check`
checks concat projections, including emitted-Verilog Icarus runs over all 256
four-state input vectors and an incorrect-slice negative control. MatX sources
and corpus results remain outside this repository.
