# Compatibility

## Version binding

Every native artifact records `bscVersionStr True`, the first line of
`bsc -v`, and every reader requires exact equality: a capture, replay,
adaptation or snapshot from another build is refused (`producer/native
schema mismatch; regenerate the capture` for a capture, `snapshot
schema/native library mismatch` for a snapshot, `current bluehs runtime does
not match this SDK` for the SDK). Exact replay also
requires the producer executable's SHA-256; adaptation requires only the
same full version and records the original producer by identity. The SDK
manifest additionally records the GHC version and the bluehs environment
file; `package-sdk.hs --check` requires both to match the running launcher.
Nothing in the stack migrates a capture between versions.

## Format tags

| Artifact | Tag | Since |
|---|---|---|
| `.ba` codec (the retained `VProgram` uses the existing optional slot) | `bsc-ba-20260712-1` | upstream |
| `MOD.replay` manifest | `rmSchema = "bsc-eco-native-replay-2"` | #242 |
| `adapter-receipt` | `arSchema = "bsc-eco-native-adapter-2"` | #242 |
| `MOD.eco-ast` envelope / `MOD.eco-ast.bin` payload | `aeSchema = 2` / magic `bsc-eco-verilog-ast-2` | #253 |
| `MOD.eco-preopt`, `MOD.eco-graph` | `egSchema = 1` | #243 |
| `keep.plan` | `ebpSchema = 1` | #243 |
| `*.eco-plan` rename plans | `erpSchema = 1` | #246 |
| `search.plan` | `planSchema = 3` | #247 |
| `objective.plan` | `costSchema = 1` | #247 |
| `build.manifest` | `sdkSchema = 4` | #246 |
| proof receipts (`*.proof.json`, `ordinary-to-kept.json`, `proof-N.json`) | `"schema": 1` | #246 |

Haskell records are written with `show` and read with `readMaybe`; JSON is
written by `ECOJSON`. A record of another schema is refused, never
converted.

## Producer identity

A capture is bound to the executable that produced it; adaptation produces
a new capture bound to the adapting compiler and a receipt linking the two.
Results obtained with another producer keep that producer's identity: the
private corpus was produced by the preview compiler (`bsc-ba-20261027-eco-1`
codec), its captures cannot be read by this stack, and no claim about this
build follows from them until they are recaptured and rerun here.

## Deliberately unsupported

Cross-version captures; the preview AST codec; Verilog filters with
arguments or shell syntax; ECO options in `BSC_OPTIONS`; `-eco-capture`
without `-elab` or without the Verilog backend; replay from a different
working directory; output paths that alias or lie inside a capture;
programs that use an identifier before declaring it (so RTL compiled with
`-no-verilog-declare-first` is refused at emission); macros other than the
printer's reset value and edge in a proof inventory; X/Z literals, calls,
strings or macros inside a binary miter; changed ports, parameters, DPI
declarations, instance sets or child output connections; a changed child
input connection without a native width descriptor (multi-copy method
formals and inouts are undescribed); temporal, reset, initialisation or
reachability assumptions; any cost or receipt as an acceptance authority.
