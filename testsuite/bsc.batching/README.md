# bsc.batching: directed determinism probes

bsc's `Ord Id` compares string-intern ids, so a `Data.Map`, `Data.Set` or
`sort` over `Id`s iterates in the order the process first saw each string.
Where such an order reaches an output, the output depends on what else the
process compiled first (a `bsc -u` batch, a persistent worker) or on an
unrelated edit that changes when a name is first met.

Every fix for such a site has a directory here with a directed probe:
a small design on which the unfixed compiler produces different output in
two orders and the fixed compiler the same.  The `.exp` file's header
comment names the issue, the fix and its pull request, so the directory is
self-describing and nothing here is shared between issues.

The probes are built from the shapes in `config/unix.exp`:

| proc | what it checks |
|---|---|
| `batching_bo_pair probe root scramble` | the `.bo` dump of `probe` is the same compiled alone and compiled through `root`, whose imports name `scramble` first so the names are interned in the opposite order |
| `batching_verilog_pair src mod top topmod scramblers ?options?` | the Verilog of `mod` is the same compiled alone and compiled through `top` behind the scramblers |
| `reverse_intern_same_verilog src mod ?options?` | the Verilog of `mod` is the same with and without `-reverse-intern-order` |
| `reverse_intern_same_bo src ?options?` | the `.bo` dump of `src` is the same with and without `-reverse-intern-order` |

A scrambler is a package that merely mentions the names the probe uses, in
the order that flips the site under test.  `-reverse-intern-order` is the
whole-compiler version of a scrambler: it hands intern ids out from the top
down, reversing every `Ord Id` order at once, so a probe built on it
catches any site the design exercises.

Once the individual cases are fixed and probed here, the whole testsuite
is run normally and with `TEST_BSC_OPTIONS=-reverse-intern-order` and every
generated file is compared byte for byte; that comparison is the end check,
and these directories are what make each of its findings a fixed, named
case rather than a difference nobody owns.
