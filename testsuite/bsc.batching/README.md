# bsc.batching: directed determinism probes

`Ord` on an `Id` compares `SpeedyString` arrival order, so a `Data.Map`,
`Data.Set`, or `sort` over `Id`s iterates in the order the process first
saw each string. Where such an order reaches an output, the output depends
on what else the process compiled first (`bsc -u`, a persistent worker) or
on an unrelated edit that changes when a name is first met.

Every fix for such a site has a directory here with a directed probe: a
small design (in Bluespec Classic `.bs` unless BSV syntax is under test)
on which the unfixed compiler produces different output in two orders and
the fixed compiler produces the same output.

The probes use the helpers in `config/unix.exp`:

| proc | check |
|---|---|
| `batching_bo_pair probe root scramble` | `.bo` dump of `probe` matches compiled alone vs. compiled through `root` behind `scramble` |
| `batching_verilog_pair src mod top topmod scramblers ?options?` | Verilog of `mod` matches compiled alone vs. compiled through `top` behind `scramblers` |
| `reverse_intern_same_verilog src mod ?options?` | Verilog of `mod` matches with and without `-trace-intern-reverse` |
| `reverse_intern_same_bo src ?options?` | `.bo` dump of `src` matches with and without `-trace-intern-reverse` |
| `check_verilog_regen src mod ?options?` | fresh-process `bsc -verilog -c` regeneration from `.ba` (normal and `-trace-intern-reverse`) matches the direct `.v` |

`-trace-intern-reverse` reverses `Ord` on `SString` (and therefore `FString`
and `Id`), `-trace-intern-random` permutes `Ord` using `$BSC_INTERN_SEED`,
and `-trace-intern-load` restores a table snapshot from `$BSC_INTERN_LOAD`,
all while keeping the dense table indices `0 .. internCount - 1` unchanged.
