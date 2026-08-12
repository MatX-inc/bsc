# bsc lexer benchmark: hand-written Lex.hs vs Alex-generated

Benchmarking experiment for Amy (MatX). Replaces bsc's hand-written Bluespec
Classic lexer (`src/comp/Lex.hs`, consumes `String`) with an Alex-generated
lexer producing the *identical* token stream, in five input-representation
variants, and measures all six.

## PR context

- **#1084 "Switch to using Cabal"** (matx-amy, draft, `MatX-inc:amy/cabalize`):
  replaces the Makefile build with a `bsc.cabal` + `SetupHooks.hs`, moves the
  `main` files to `src/comp/app/`, statically links the SAT solvers. It does
  **not** touch `Lex.hs` or `src/comp/Parser/*`. Relevance to Alex: a cabal
  build runs Alex automatically on `.x` modules via `build-tool-depends:
  alex:alex`, so integrating a generated lexer becomes a one-line packaging
  change instead of Makefile surgery. Since the branch's lexer sources are
  identical to `main`'s, this work was done against `main` @ 941eecf
  (2026-07-14); the `.x` file drops into the #1084 branch unchanged.
- **#1085 "Transcode ISO-8859-1 files to UTF-8"** (matx-amy, open): transcodes
  repo files to UTF-8, switches the LaTeX docs to UTF-8, adds a CI encoding
  check. It does **not** touch the lexer or parser — only docs, testsuite
  fixtures and CI. (It *is* why 6 corpus files below are skipped: they are
  intentionally Latin-1 and bsc's reader rejects them before lexing.)

## What was built

Everything under `harness/` in this directory; nothing in bsc was modified
except one line: `Lex.hs`'s export list gains `LexError(..)` so the wrapper
can construct the same error tokens (no code change).

- `harness/alexparts/{header.template,rules.part,footer.part}` → one Alex spec
  instantiated for five stream types (assembled by `genx.sh`, built with
  `alex -g`, GHC 9.4.7 `-O2`):
  - `alex-string` — `String` input (same representation as `Lex.hs`)
  - `alex-sbs` — strict `ByteString` (raw UTF-8 bytes, no upfront decode)
  - `alex-lbs` — lazy `ByteString` (the "flat representation" ask)
  - `alex-lt` — lazy `Text`
  - `alex-st` — strict `Text` (`Data.Text.uncons`; added on follow-up)
- Design: the DFA sees one byte per source **character** — ASCII bytes as
  themselves, non-ASCII characters collapsed by `alexGetByte` into 5
  classification pseudo-bytes (0xF1–0xF5) computed with the *same* predicates
  (`isSym`/`isAlpha`/`isIdChar`, in Lex.hs's testing order). Same trick GHC's
  own Alex lexer uses. ByteString variants UTF-8-decode inline in
  `alexGetByte`. File/line/column are threaded through the driver loop (not
  boxed per input step); `# <line> "<file>"` directives (column-0 only) are
  handled by the driver before each `alexScan` call. Lex.hs's private helpers
  (`lexLitChar'`, `readN`, `skipComm`, tab stops, SV-keyword sets) are ported
  verbatim so quirks match bug-for-bug — including the hand lexer's column
  drift on simple string/char escapes (`\n` advances the column by 3, not 4),
  `.`/`,` never *starting* an operator token, `--@`/`---}`-style comment
  disambiguation, `_`-separator rules in literals (`1_2.5` is *not* a float),
  the `package`-at-column-(c−1) hack, the `(line+1, col −1)` EOF position, and
  the infinite `L_error : repeat L_eof` error tail.
- `harness/Main.hs` — standalone harness (does **not** build bsc; compiles
  `Lex.hs` + its ~20-module dependency closure directly with `-i` flags plus
  three shims: `Error` (5 `ErrMsg` constructors), `BuildVersion`,
  `Data.Generics` → `Data.Data`). Modes: `bench` (forces the entire token
  stream via a strict fold: count + order-dependent hash covering positions,
  interned FStrings, full Integer/Rational values, every literal char),
  `dump`, `compare`.

## Equivalence

All **1047/1047** UTF-8-valid `.bs` files in the repo (stdlib `src/Libraries`
+ `testsuite` + examples) produce **token-for-token identical streams**
(positions included, deep `Eq` plus `show` comparison) for all five Alex
variants vs `Lex.hs`, including the 6 files that end in lexical-error tokens.
Plus 10 synthetic edge-case files (Unicode symbol/ident classes, tabs/CR/FF
column rules, line directives, nested/unterminated comments, missing-NL,
sized/underscored/exponent literals, `$`-idents, escape-position drift).
**Zero deviations.** 6 non-UTF-8 (Latin-1) testsuite files are excluded —
bsc itself refuses them before lexing (`ENotUTF8`; cf. PR #1085). Lexers run
with `lf_allow_sv_kws=True` (the SV-keyword check path is an
`internalError` crash by design; flag logic is replicated identically).

## Benchmarks

Machine: Intel Xeon @ 2.10GHz (4 vCPU, 15 GB), Linux; GHC 9.4.7 `-O2`,
Alex 3.3.0.0 (`-g`), default RTS unless noted. Every run forces the full
token stream; hashes are asserted identical across engines (they are:
9,047,095 tokens, hash 2170315049108700862).

### (a) Throughput — 66.5 MB corpus (all 1041 error-free UTF-8 `.bs` files × 42)

Lex-only wall time (input pre-built/pre-forced outside the timed region;
String/Text decode NOT counted; median of 5 runs, spread was ≤ 5%):

| engine | median | MB/s | vs hand | alloc in lex region |
|---|---|---|---|---|
| hand (Lex.hs, String) | 3.575 s | 18.6 | 1.00x | 4.13 GB |
| alex-string | **2.628 s** | **25.3** | **1.36x** | 6.43 GB |
| alex-sbs (strict BS) | 2.934 s | 22.7 | 1.22x | 11.24 GB |
| alex-lbs (lazy BS) | 3.542 s | 18.8 | 1.01x | 15.62 GB |
| alex-lt (lazy Text) | 3.502 s | 19.0 | 1.02x | 17.32 GB |
| alex-st (strict Text) | 2.923 s | 22.8 | 1.22x | 10.89 GB |

Whole-process view (same runs, `+RTS -s`; this *includes* building the input
representation, which for String means `decodeUtf8` + `T.unpack` + forcing a
1.07 GB `[Char]`):

| engine | process wall | MUT | GC | max residency |
|---|---|---|---|---|
| hand | 14.15 s | 4.07 s | 10.09 s | 1.07 GB |
| alex-string | 9.80 s | 3.19 s | 6.62 s | 1.07 GB |
| alex-sbs | **3.13 s** | 3.11 s | **0.03 s** | **67 MB** |
| alex-lbs | 3.75 s | 3.71 s | 0.04 s | 67 MB |
| alex-lt | 3.64 s | 3.60 s | 0.04 s | 133 MB |
| alex-st | 3.02 s | 2.99 s | 0.03 s | 133 MB |

The String pipelines drown in GC because the retained 1 GB `[Char]` is
re-traversed every major GC. The ByteString/Text variants keep residency at
the input size and GC ≈ 0. (This effect is proportional to file size, so it
is dramatic on a 66 MB file and small on real KB-sized files — see (b).)
Sensitivity check with `+RTS -A64m`: ordering unchanged (hand 17.9,
alex-string 24.3, alex-sbs 21.7, alex-lbs 18.6, alex-lt 18.3 MB/s).

### (b) Typical-file overhead — 25th-percentile file, 30,000 iterations

Corpus size distribution (1041 files): min 20 B, p25 221 B, p50 556 B,
p75 1.2 KB, p90 2.6 KB, max 169 KB, mean 1.5 KB. Benchmarked file:
`testsuite/bsc.typechecker/literals/LiteralInTuple.bs` (221 B, 37 tokens).

| engine | median / file | p90 | alloc / file |
|---|---|---|---|
| hand | 8.58 µs | 10.45 µs | 25.6 KB |
| alex-string | **5.54 µs** | 8.97 µs | 31.1 KB |
| alex-sbs | 6.48 µs | 9.20 µs | 48.0 KB |
| alex-lbs | 8.15 µs | 11.63 µs | 63.1 KB |
| alex-lt | 8.35 µs | 10.30 µs | 69.0 KB |
| alex-st | 6.51 µs | 8.12 µs | 46.6 KB |

Note these loops hold the input representation fixed, which *understates*
`alex-sbs`: per real file, the String engines additionally pay
decodeUtf8 + unpack (bsc's actual read path, `FileIOUtil.hs`), which
`alex-sbs` skips entirely.

### (c) strict Text follow-up (Amy's question: does it beat String?)

Added `alex-st`: strict `Data.Text` input via `Data.Text.uncons`, same
classification/pseudo-byte scheme and token actions, same timing convention
(the `Text` is pre-built and forced outside the timed region, exactly as the
`[Char]` is for `alex-string`). Equivalence re-verified: 1047/1047 corpus
files + the 10 synthetic edge files, token-identical (`equiv_st_corpus.txt`).

**No — lex-only it does not beat String**: 22.8 MB/s vs alex-string's 25.3
(0.90x; it is 1.22x vs hand, statistically tied with strict ByteString's
22.7). The timing convention explains it: alex-string's `[Char]` is fully
built before the clock starts, so its per-char step is one cons-cell
dereference with **zero** allocation in the timed region beyond token
building (6.43 GB). Strict `Text` (text-2.0, UTF-8 internally on GHC 9.4)
must do the UTF-8 `iter` decode per character *and* `uncons` allocates a
fresh 4-word `Text` slice record per step — 10.89 GB allocated, ~4.5 GB more
than String, i.e. it re-pays at lex time the decode work String was granted
for free, plus slice boxing that strict ByteString's `unsafeDrop` also pays
(hence the near-identical 22.7/22.8). Lexeme extraction is identical across
variants (`takeStr` builds a `String` via `unconsChar`), so it doesn't
differentiate.

Where strict Text *does* win is end-to-end: 3.02 s whole-process on the
66 MB file (vs 9.80 s for alex-string, 3.13 s for alex-sbs), because
ByteString→Text is a validate+copy under text-2.0 rather than unpacking to a
1 GB `[Char]`, GC ≈ 0, residency 133 MB. Per-file (p25) it is 6.51 µs —
faster than hand (8.58) and lazy variants, a hair behind alex-sbs (6.48) and
alex-string (5.54).

- Laziness defeated by a strict fold over the whole stream hashing positions
  + payloads (forces FString interning, Integer/Rational values, every
  string char); `Main` compiled with `-fno-full-laziness`, engines
  `NOINLINE`, per-iteration distinct filename FString so iterations can't be
  CSE'd. Hash printed and checked identical across all engines/runs.
- Both lexers produce the same `FString`-interned tokens, so SpeedyString's
  global-IntMap interning cost (a big chunk of the per-token cost) is paid
  identically on both sides.
- Timed at ns resolution (`getMonotonicTimeNSec`); allocation via
  `getAllocationCounter` around the timed region only.
- lazy BS/Text inputs were single-chunk (`fromStrict`); per-char
  `uncons`-with-chunk-boundary-check is what makes them slow, not chunk size.

## Take

- **Correctness: an Alex lexer can replace Lex.hs exactly** — token-for-token
  including positions and error behavior over the entire repo corpus. The
  hairy bits (column drift on escapes, `--`/symbol disambiguation, `_` rules
  in literals, line directives at column 0, the `package` hack) are all
  expressible with ~15 extra rules plus a small hand-driven driver loop.
- **Speed: Alex wins, but modestly on like-for-like input.** Same input type
  (String): **1.36x** faster lex-only and it's the fastest variant overall on
  both big-file and per-file tests. Strict ByteString is 1.22x lex-only but
  is the clear end-to-end winner (**4.5x** process time on the 66 MB stress
  file, GC ≈ 0, residency = input size) because it eliminates the `[Char]`
  pipeline entirely; on typical KB files that end-to-end edge shrinks to
  ~25–40%. The requested lazy-ByteString and lazy-Text variants roughly tie
  with the hand lexer (1.01–1.02x lex-only) — per-character `uncons` through
  the lazy-chunk indirection eats the DFA's advantage; a chunk-aware
  `alexGetByte` would close most of that gap.
- **Ceiling note:** with identical token payloads, a large fraction of the
  per-token cost is shared by construction (FString hash-consing through a
  global unsafePerformIO IntMap + Integer/Rational building + Position/Token
  allocation) — all engines cluster within ~1.4x, consistent with the shared
  part dominating — which caps how much any lexer swap alone can help. If the
  goal is a big win, pairing the Alex+
  ByteString lexer with an FString/Text overhaul would compound.
- Practical path: land #1084 (cabal), then add the `.x` with
  `build-tool-depends: alex:alex` and the strict-ByteString wrapper wired to
  bsc's existing `ByteString` read path (it already reads files as strict
  `ByteString` and only converts to `String` for the lexer).

## Repro

```
cd harness && ./genx.sh && ghc -O2 -ishims -i../bsc/src/comp -i../bsc/src/comp/Libs -igen -o harness Main.hs -rtsopts
./harness compare FILE.bs          # token-stream equivalence, all engines
./harness bench ENGINE FILE ITERS  # ENGINE = hand|alex-string|alex-sbs|alex-lbs|alex-lt|alex-st
./harness dump ENGINE FILE         # printed token stream
```

Corpus lists: `corpus_utf8.txt` (1047 equivalence files), `corpus_clean.txt`
(1041 concat-safe files), `big.bs` (66.5 MB), raw outputs under `bench/`,
equivalence log `equiv_out2.txt`. bsc source: `main` @ 941eecfe (fetched via
Go-module-proxy zip; GitHub direct was blocked in this environment). The only
bsc modification is the `LexError(..)` export in `src/comp/Lex.hs`.

## (d) ASCII-identifier fast path (alex-stf) — asm inspection + hack + measurement

Question (Amy): does the generated machine code have a specialized all-ASCII
identifier path, or does every identifier char walk the DFA?

### Asm/Core verdict: NO fast path — every char walks the full DFA

Compiled `gen/LexAlexST.hs` (alex 3.3.0 `-g`, GHC 9.4.7 `-O2`) with
`-ddump-simpl`/`-ddump-asm` (dumps in `harness/dumps/gen/`). The whole lexer
funnels through one inner loop, `$s$walex_scan_tkn` (Core line ~8549), and
per input **character** — identifier or not — it does:

1. **a boxed accept-array read**: `indexArray# arr# state` (the `AlexAcc`
   array; alex `-g` unboxes the transition tables but NOT the accept array),
   plus a pointer-tag test/eval check on the result;
2. because every identifier state is accepting, **two heap allocations**:
   a fresh `AlexLastAcc` record *and* a fresh 4-word `Data.Text.Internal.Text`
   slice, per identifier character (asm: `movq $LexAlexST.AlexLastAcc_con_info
   ,-16(%r12)` / `movq $Data.Text.Internal.Text_con_info,-24(%r12)` inside the
   loop, with heap-check `cmpq 856(%r13),%r12` each time);
3. the text-2.0 UTF-8 iter: `indexWord8Array#` + `clz8#` (asm `bsr`) to
   compute the char and its byte length — even for ASCII;
4. the classify step (ASCII passes the raw byte through: `ltChar# c '\128'`,
   non-ASCII calls `Lex.$wisSym` / ccall `u_iswalpha`);
5. **three unboxed table reads**: `indexInt32OffAddr# alex_base state`,
   `indexInt16OffAddr# alex_check offset`, then `alex_table` (or `alex_deflt`)
   — asm:

   ```
   movslq alex_base_r7DS_bytes(%rcx),%rcx
   movswq alex_check_r7DU_bytes(%rdx),%rdx
   movswq alex_table_r7DT_bytes(%rcx),%rax
   ```

There is no byte-scan loop anywhere in the object code; keywords are ordinary
DFA states. So an ASCII identifier of length n costs n×(1 boxed array read +
3 table reads + UTF-8 iter + 2 allocations) before the token is even built.

### The hack: `alex-stf` (`gen/LexAlexSTF.x` = same rules, custom driver)

New variant, strict Text input. At each token start the driver first calls
`fastScanId` (`harness/LexAlexFastPath.hs`): if the head byte is ASCII
`[A-Za-z_]`, scan forward over the Text's underlying `ByteArray#` while bytes
are ASCII `[A-Za-z0-9_']` (exactly Lex.hs's `isIdChar` ∩ ASCII; one
`indexWord8Array#` + two compares + an immediate-bitmask shift/test per char
— the compiled loop is ~11 instructions, zero allocation, masks are inline
constants `0x03FF008000000000`/`0x07FFFFFE87FFFFFE`):

```
_blk_cbyH:  cmpq %rcx,%rdi              ; i >= end?
            jge  _blk_cbzY
            movb (%r9,%rdi,1),%r9b      ; byte
            cmpb $64,%r9b               ; lo/hi half
            jae  _blk_cbzs
            movq $287949450930814976,%r10   ; idCharMaskLo
            movzbl %r9b,%ecx ; andl $63,%ecx
            shrq %cl,%r10 ; testb $1,%r10b  ; bit test
            je   _blk_cbzR
            incq %rdi ; jmp _blk_cbyH       ; next char
```

- The token is emitted directly: exact-match keyword lookup on a zero-copy
  Text slice (`Map Text LexItem`, 37 keywords + `_`→`L_uscore`, pre-filtered
  to first-byte `[a-z_]` and length ≤ 10 so most identifiers skip it; keywords
  are exact-match DFA rules, longest-match otherwise prefers the longer
  identifier, so exact lookup reproduces them, incl. the `package`
  column-(c−1) hack), else `L_conid`/`L_varid` with the same
  `mkFString`/SV-keyword logic as `idTok`.
- **Bail-outs** (nothing consumed, generic DFA proceeds verbatim): head not
  an ASCII idstart, or the run is terminated by a byte ≥ 0x80 — that byte
  could start a non-ASCII idchar (e.g. `fooλ` continues the identifier), so
  the DFA must decide. Bytes < 0x80 failing the mask are never idchars, so
  the token is complete. Columns: n bytes = n chars = n columns (all ASCII).
- Position tracking, `# line` directives (col 0, checked before the fast
  path), and all non-identifier tokens are untouched.

### Equivalence

`compare` mode (now 6 variants): **1047/1047** corpus files token-identical,
plus edge files `tests/t1–t10` and three new fast-path-adversarial files
`tests/t11–t13` (identifier continued by non-ASCII idchar `fooλbar`/`abcλ`,
non-ASCII symbol after identifier `x→y`, `λstart`/`füü`, keyword-prefix
identifiers `doo`/`iff`/`lett`, `_`/`_x`/`x_`, primes `f'`/`f'h3`, `Conλ`,
`package` position quirk, keyword at EOF with no newline). **Zero
deviations** (`equiv_stf_corpus.txt`).

### Numbers (same machine/methodology as (a)–(c); re-run 2026-08-12, all
three engines re-measured back-to-back interleaved, median of 5)

Lex-only, 66.5 MB corpus (`bench/big3_*`):

| engine | median | MB/s | vs hand | vs alex-st | alloc in lex region |
|---|---|---|---|---|---|
| hand (Lex.hs, String) | 3.318 s | 20.1 | 1.00x | — | 4.13 GB |
| alex-st (strict Text) | 3.095 s | 21.5 | 1.07x | 1.00x | 10.89 GB |
| **alex-stf (fast path)** | **2.833 s** | **23.5** | **1.17x** | **1.09x** | **8.43 GB** |

(hash asserted identical: 9,047,095 tokens, 2170315049108700862; alloc
−2.46 GB = −22.6% vs alex-st)

hyperfine end-to-end, whole process incl. read+decode (warmup 2, 10 runs,
`bench/hyperfine_e2e_stf.*`):

| engine | mean ± σ | vs alex-stf |
|---|---|---|
| hand | 11.174 ± 1.371 s | 4.04 ± 0.50 |
| alex-st | 3.104 ± 0.041 s | 1.12 ± 0.02 |
| **alex-stf** | **2.765 ± 0.046 s** | 1.00 |

p25 file (221 B, 30,000 iterations, iter 1 dropped, `bench/p25c_*`):

| engine | median / file | p90 |
|---|---|---|
| hand | 7.49 µs | 8.17 µs |
| alex-st | 5.94 µs | 7.84 µs |
| **alex-stf** | **5.12 µs** | **6.46 µs** |

### Read

56.3% of corpus bytes sit in ASCII identifier-shaped runs (7.7M runs, avg
4.9 chars), and the fast path removes ~7 ns/char (~15 cycles @2.1 GHz) on
those — consistent with the ~0.26 s saved. The remaining cost is the shared
per-token tail (mkFString interning through the global IntMap, Position/
Token/list construction, forcing) plus the DFA on the other 44% of bytes —
the ceiling noted in the Take still binds: identifiers get ~2x cheaper to
*scan*, but scanning was only ~1/4 of lex time. A String-building-free
FString (Text-keyed interning) is the next multiplier; whitespace runs are
the next DFA-bypass candidate.

Repro: `./genx.sh` now also emits `gen/LexAlexSTF.x`; engine name `alex-stf`.
Fast path source: `harness/LexAlexFastPath.hs` + `harness/alexparts/
footerSTF.part`. Dumps: `harness/dumps/gen/LexAlexST{,F}.dump-{asm,simpl}`.
