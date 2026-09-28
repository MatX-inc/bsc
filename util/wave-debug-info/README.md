# Waveform debug information

`Main.hs` writes the debug information for a design's waveform
dumps as JSON: which dumped signal is which source-level entity, and how
the Bluespec types of those signals lay out in bits. Surfer's Bluespec
translator (`util/surfer-translator`) reads it beside the dump.

It is a bluehs script (see `util/bluehs/README.md`): it runs against the
compiled bsc library, so it reads the elaboration files that the same
build of bsc wrote, and needs no compiler change.

    util/bluehs/bluehs -iutil/wave-debug-info util/wave-debug-info/Main.hs \
        -sim -p <bdir>:+ <top module> <output.json>

The bsc flags name the backend whose elaboration to read (`-sim` or
`-verilog`, since a design can elaborate differently per backend) and
the search path of its `.ba` and `.bo` files; `BSC_OPTIONS` in the
environment is read first, as bluetcl does. `BLUESPECDIR` must name the
install whose `Libraries` the design was compiled against.

Modules:

- `WaveDebugInfo.hs`: the document, its `signals` from the design's
  instance trees and its `types` from the packages' type definitions.
- `WaveLayout.hs`: a type's bit layout, recovered from its `Bits`
  instance by posing queries against its own `pack`/`unpack` and having
  the evaluator reduce them over a symbolic argument. Each query is a
  `noinline` function, which bsc wraps as a module whose one method is
  the function.
- `InstScopes.hs`: the source-level scope view of an instance tree.
- `PrimSignals.hs`: the signals a primitive instance puts in a dump, as
  Bluesim and Verilog each spell them.
