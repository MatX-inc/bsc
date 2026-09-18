# Surfer translator for Bluesim waveform dumps

A [Surfer](https://surfer-project.org) plugin that shows the signals of a
Bluesim dump by their Bluespec types: a struct as named fields, a tagged
union as its active constructor and that constructor's fields, an enum
as its constructor name, a vector as its elements.

It reads the debug information that bluetcl's `module wavedebuginfo`
writes for the design -- the bit layout of every type the signals name,
hand-written `Bits` instances included, and which dumped signal is which
source entity -- from `<dump>.debug.json` beside the dump.  A signal's type comes from the dump itself when the dump
records it (FST, with `-dump-formats fst`), else from the debug
information's entry for the signal's path (VCD).

The plugin also shapes the variable list: the design's own signals
(registers, ports, rule fires) sort ahead of everything else, the
compiler's intermediate values (`…__h<n>`, `…__d<n>`) are hidden until
the filter menu's "Show hidden" is ticked, and a signal whose dumped name
is not its source name -- a flattened `pair_lo`, a `WILL_FIRE_RL_step` --
is shown by the source line declaring it (read relative to Surfer's
working directory, else by the source name alone), with the file in the
variable's tooltip.  Hiding and the file need a Surfer built from the
MatX fork's `william/bluespec-support` branch; an older Surfer ignores
those two fields and shows everything.

Build with the `wasm32-unknown-unknown` target installed
(`rustup target add wasm32-unknown-unknown`):

    cargo build --release --target wasm32-unknown-unknown

and install `target/wasm32-unknown-unknown/release/bsc_wave_translator.wasm`
into a `.surfer/translators/` directory that Surfer searches (the current
directory's, or the user data directory's).  `cargo test` checks the
decoding against the compiler testsuite's exported debug information.
