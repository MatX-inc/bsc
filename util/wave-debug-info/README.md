# Waveform debug information

`Main.hs` writes the debug information for one synthesized module's
waveform dumps as JSON: which dumped signal is which source-level entity,
how the Bluespec types of those signals lay out in bits, and which
synthesized modules it instantiates where. The fragments of a design's
modules describe the design's dumps together, each under its instance's
scope; Surfer's Bluespec translator (`util/surfer-translator`) reads them
beside the dump.

It is a bluehs script (see `util/bluehs/README.md`): it runs against the
compiled bsc library, so it reads the elaboration files that the same
build of bsc wrote, and needs no compiler change.

    util/bluehs/bluehs -iutil/wave-debug-info util/wave-debug-info/Main.hs \
        -verilog -p <bdir>:+ <module> <module.json>

The bsc flags name the backend whose elaboration to read (`-sim` or
`-verilog`, since a design can elaborate differently per backend) and
the search path of its `.ba` and `.bo` files; `BSC_OPTIONS` in the
environment is read first, as bluetcl does. `BLUESPECDIR` must name the
install whose `Libraries` the design was compiled against. For the
Verilog elaboration the script also runs bsc's own back end on the
module, so that the generated Verilog's every net is described under
the name the netlist gives it.

`testsuite/bsc.bluehs/wave-debug-info` runs the script on a fixture with
every kind of type and compares the fragments of both backends against
expected files (`make bsc.bluehs/wave-debug-info/wave-debug-info.check`
in `testsuite`); it is skipped when the installation has no bluehs runtime.

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
- `Netlist.hs`: the generated Verilog's nets, named by running bsc's own
  back end on the elaboration, each with the rules and methods it belongs
  to.
