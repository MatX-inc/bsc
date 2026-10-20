# Surfer translator for Bluesim waveform dumps

A [Surfer](https://surfer-project.org) plugin that shows the signals of a
Bluesim dump by their Bluespec types: a struct as named fields, a tagged
union as its active constructor and that constructor's fields, an enum
as its constructor name, a vector as its elements.

It reads the debug information that `util/wave-debug-info` writes for
the design -- the bit layout of every type the signals name,
hand-written `Bits` instances included, and which dumped signal is which
source entity -- from the bundle beside the dump, `<dump>.debug/`: an
`index.json` naming the top module and one fragment per synthesized
module, each describing that module's own signals relative to its
scope and the modules it instantiates.  The plugin finds the top
module's scope in the dump by itself (`main.top` under bsc's Bluesim
harness; whatever a testbench names it), so a dump of the design under
any wrapper reads the same, and follows instances down from there by
their names in the synthesized hierarchy or their paths in the source
(a Bluesim dump made with `-wave-source-hierarchy`).  A signal's type
comes from the dump itself when the dump records it (FST, with
`-dump-formats fst`), else from the debug information's entry for the
signal's path (VCD).

The plugin also shapes the variable list: the design's own signals
(registers, ports, rule fires) sort ahead of everything else, and the
compiler's intermediate values (`…__h<n>`, `…__d<n>`, and the nets a
Verilog fragment describes) are hidden until the filter menu's "Show
hidden" is ticked.  Variables keep their dumped
names: a Bluespec signal's declaration site, often inside an inlined
library module, does not identify it the way the flattened name does,
so the plugin does not offer Surfer's source-line "true names".  Hiding
needs a Surfer built from the MatX fork's `william/bluespec-support`
branch; an older Surfer ignores the field and shows everything.  A
`Bool` reads `True` or
`False` and is drawn as a level, which also needs the fork's Surfer:
an older Surfer draws a level low only for the text `0`, so it draws
`False` high.

Build with the `wasm32-unknown-unknown` target installed
(`rustup target add wasm32-unknown-unknown`):

    cargo build --release --target wasm32-unknown-unknown

and install `target/wasm32-unknown-unknown/release/bsc_wave_translator.wasm`
into a `.surfer/translators/` directory that Surfer searches (the current
directory's, or the user data directory's).  `cargo test` checks the
decoding and the path resolution against the exporter's testsuite
goldens (`testsuite/bsc.bluehs/wave-debug-info`, the Bluesim fragments
of `WaveTypes.bs`), which the testsuite produces with the repository's
own bsc.
