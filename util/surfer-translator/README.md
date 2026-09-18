# Surfer translator for Bluesim waveform dumps

A [Surfer](https://surfer-project.org) plugin that shows the signals of a
Bluesim dump by their Bluespec types: a struct as named fields, a tagged
union as its active constructor and that constructor's fields, an enum
as its constructor name, a vector as its elements.

It reads the debug information that bluetcl's `module wavedebuginfo`
writes for the design -- the bit layout of every type the signals name,
and which dumped signal is which source entity -- from `<dump>.debug.json`
beside the dump.  A signal's type comes from the dump itself when the dump
records it (FST, with `-dump-formats fst`), else from the debug
information's entry for the signal's path (VCD).

Build with the `wasm32-unknown-unknown` target installed
(`rustup target add wasm32-unknown-unknown`):

    cargo build --release --target wasm32-unknown-unknown

and install `target/wasm32-unknown-unknown/release/bsc_wave_translator.wasm`
into a `.surfer/translators/` directory that Surfer searches (the current
directory's, or the user data directory's).  `cargo test` checks the
decoding against the compiler testsuite's exported debug information.
