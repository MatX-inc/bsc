All questions are answered. Here is the report.

# bsc.options under verilator (implied `-use-dpi`): the 7 remaining failures

## Context

- The harness change (currently **uncommitted** in `/home/ravi/bluespec/matx/bsc/testsuite/config/unix.exp`) adds `vsim_implied_bsc_flags` (unix.exp:498–508), which returns `-use-dpi` when `$verilog_compiler == "verilator"`. It is prepended at both compile (`bsc_compile_verilog`, unix.exp:1345) and link (`bsc_link_verilog`, `/home/ravi/bluespec/matx/bsc/testsuite/config/verilog.tcl:57`) — deliberately *before* per-test options so a test can override it.
- Driving test file: `/home/ravi/bluespec/matx/bsc/testsuite/bsc.options/options.exp`. The design is `GCD.bsv` with `import "BDPI" function ActionValue#(Bit#(32)) my_time ()` (monomorphic, fixed width) and implementation `my_time.c.keep`.
- The 7 failures map to options.exp lines 154, 171, 183, 184, 191, 192 (the VPI-wrapper section, lines 128–192) and 363 (the `-q` quiet-link section, lines 334–364). `files_exist` (unix.exp:2316) emits exactly one PASS/FAIL per call, so the four file checks + two links + one compare = 7. The prior baseline run (`/home/ravi/bluespec/matx/bsc/rc4-verilator-baseline.log`, options.exp block at lines 8147–8387, "7 unexpected failures") predates the compile-side implied flag, so its FAIL set differs slightly; the current set matches the section-by-section analysis below. (bsc.options was cleaned by the in-flight run; its `testrun.sum` does not exist at the moment.)

## Compiler-side facts (src/comp)

- **Compile stage, VPI flavor:** `genVPIWrappers` writes `vpi_wrapper_<fn>.c/.h` via `mkVPICName/mkVPIHName (vdir flags) prefix` (`/home/ravi/bluespec/matx/bsc/src/comp/VPIWrappers.hs:34-35`) — i.e., into `-vdir`, else into the *source file's directory*. That placement rule is exactly what options.exp:154/171 test.
- **Compile stage, DPI flavor:** `bsc.hs:453-460` skips `genVPIWrappers` entirely when `useDPI`. Monomorphic imports get **no wrapper file at all** — the `import "DPI-C"` declaration in the generated `.v` names the user's C function directly (`/home/ravi/bluespec/matx/bsc/src/comp/DPIWrappers.hs:18-20`). Only *polymorphic* imports get a generated `dpi_wrapper_<fn>.c` (no `.h`), written during `genModuleVerilog` (`bsc.hs:1230-1238`) with the **same** vdir-else-source-dir rule (`DPIWrappers.hs:59`, `mkDPICName`, `FileNameUtil.hs:89-90`).
- **Link stage, VPI flavor:** bsc finds `vpi_wrapper_<fn>.c` via the Verilog search path, compiles it to `.o` next to it, and generates+compiles `vpi_startup_array.c` into vdir/cwd (`bsc.hs` `vGenFFuncs`, ~2185+). All of this happens inside bsc *before* the simulator build script runs.
- **Link stage, DPI flavor:** no registration array exists (DPI binds by symbol name); nothing is precompiled. For polymorphic imports only, `dpi_wrapper_<fn>.c` is located via the same search-path mechanism (`readFilePath … (vPath flags)`, error `EMissingVPIWrapperFile`, `bsc.hs:2208-2230`) and handed as *source* to verilator (it needs `svdpi.h`). So there is **no `.o` artifact analogue whatsoever**.
- `imported_BDPI_functions.h` (`bsc.hs:1409`, `SimBlocksToC.hs:673`) is **Bluesim-only** — not relevant to the Verilog DPI flavor.
- **`-no-use-dpi` exists and works per-test:** `use-dpi` is a `Toggle` flag (`FlagsDecode.hs:1741-1743`); the `no-` prefix machinery (`FlagsDecode.hs:775-787`) accepts it, and last-flag-wins means a per-test option overrides the implied flag.

## Per-failure analysis

### 1. options.exp:154 — `files_exist {srcfiles/vpi_wrapper_my_time.h srcfiles/vpi_wrapper_my_time.c}`
- **(a) VPI:** compile with no `-vdir` places wrapper `.c/.h` in the *source file's* directory (`srcfiles/`), not the cwd.
- **(b) DPI equivalent:** for a monomorphic import, *no* artifact — the DPI declaration is inline in `srcfiles/mkGCD.v`. The placement rule does have a DPI analogue (`srcfiles/dpi_wrapper_<fn>.c`), but only for a polymorphic import.
- **(c) Resolution:** sim-keyed expectations. Under DPI flavor either assert the true equivalent (no vpi files + `import "DPI-C"` string occurs in `srcfiles/mkGCD.v`), or — to keep testing the placement rule itself — add a polymorphic-import companion test asserting `srcfiles/dpi_wrapper_<fn>.c`. `-no-use-dpi` is also viable *for this compile-only check* (no simulator involvement at compile), but see #3/#5 for why that can't carry the whole section.

### 2. options.exp:171 — `files_exist {vfiles/vpi_wrapper_my_time.h vfiles/vpi_wrapper_my_time.c}`
- **(a) VPI:** with `-vdir vfiles`, wrappers go to the vdir.
- **(b) DPI:** same as #1; DPI analogue is `vfiles/dpi_wrapper_<fn>.c` for polymorphic imports (`mkDPICName (vdir flags) …`), nothing for `my_time`.
- **(c) Resolution:** same as #1 (sim-keyed artifact list, ideally with a polymorphic-import variant to preserve the vdir-routing intent).

### 3 & 5. options.exp:183 and :191 — `link_verilog_pass {srcfiles/my_time.ba} sysGCD …` ("should link to executable sysGCD")
- **(a) VPI:** the link finds `vpi_wrapper_my_time.c` through `-vsearch`, compiles it and the registration array, and links — **without** `my_time.c`. This only links because iverilog packages VPI code as a shared object where the undefined `my_time` symbol is tolerated (it would fail at *runtime*).
- **(b) DPI:** verilator statically links an executable; the `import "DPI-C"` reference to `my_time` must resolve, so without `my_time.c` the final g++ link fails with an undefined reference. This is correct behavior, **not a bsc bug**. The DPI analogue of "link finds wrapper via search path" exists only for polymorphic imports (`findDPIWrapperFile` over `vPath`).
- **(c) Resolution:** sim-keyed: under DPI flavor add `my_time.c` to the link objects (still exercises the `.ba`-driven foreign-function link flow and should then pass), or mark the pair UNSUPPORTED under verilator. `-no-use-dpi` is **not** viable here: verilator has no VPI support and rejects the `$my_time` system-task style Verilog, so the link fails regardless — that is precisely why the implied flag exists.

### 4 & 6. options.exp:184 and :192 — `files_exist {vfiles/vpi_wrapper_my_time.o …vpi_startup_array.o}`
- **(a) VPI:** link compiles the found wrapper to `.o` *next to the wrapper source* (in the vsearch dir) and puts `vpi_startup_array.o` in `-vdir` (else cwd).
- **(b) DPI:** no equivalent artifacts exist by design — no registration array at all, and wrapper sources (polymorphic only) are handed to verilator uncompiled. Nothing bsc could be expected to produce.
- **(c) Resolution:** sim-keyed skip of these assertions under DPI flavor (or invert: assert the vpi files are *absent*). If a polymorphic-import variant is added, the analogous search-path behavior is verified by link success vs. the `EMissingVPIWrapperFile` error, not by `.o` files.

### 7. options.exp:363 — `compare_file sysGCD.bsc-vcomp-out.filtered empty.expected` (the `-q` test)
- **(a) VPI/iverilog:** `bsc -q … -e sysGCD -q my_time.c my_time.ba` must produce *no output* — iverilog is silent on success and `-q`/`-quiet` (alias, `FlagsDecode.hs:1774-1775`) suppresses all of bsc's own messages (`bsc.hs:2120` etc.). The sed filter at options.exp:351-362 only strips iverilog-specific warnings.
- **(b) DPI/verilator:** the link itself succeeds (`my_time.c` is passed), but the log can never be empty: `vSimLink` forwards only `-verbose` to the build script — quiet is never propagated (`bsc.hs:2085-2100`) — and `bsc_build_vsim_verilator` unconditionally lets verilator print its "V e r i l a t i o n Report" stats banner and runs `make -C $OBJDIR -j` with full command echo (script lines 267-285; see a live example in `/home/ravi/bluespec/matx/bsc/testsuite/bsc.verilog/sysSimple.bsc-vcomp-out`: banner + make Entering/Leaving + every g++ line).
- **(c) Resolution:** this one is a **genuine bsc gap**, not just a test-expectation issue: `-q` does not reach the verilator build path. Proper fix: plumb quiet through `vSimLink` → `bsc_build_vsim_verilator` (pass `--quiet-stats`/`--quiet` to verilator 5.048 and `make -s`, mirroring the existing `-verbose` plumbing). Short-term harness fix for the rc: sim-keyed filtering — extend the sed at options.exp:354 under verilator to drop `^- V e r i l a t i o n`, `^- Verilator:`, make Entering/Leaving, and compiler-command echo lines (precedent: the Bluesim `-parallel-sim-link` branch already filters make chatter at options.exp:369-376).

## Summary table

| # | options.exp | Check | DPI analogue exists? | Recommendation |
|---|---|---|---|---|
| 1 | :154 | wrapper .h/.c beside source | only for polymorphic imports (`dpi_wrapper_<fn>.c`, no .h) | sim-keyed expected artifacts (or polymorphic-import variant) |
| 2 | :171 | wrapper .h/.c in vdir | same | same as #1 |
| 3 | :183 | link w/o C impl (vdir) | no — DPI must resolve symbol | sim-keyed: add `my_time.c` to link, else UNSUPPORTED |
| 4 | :184 | wrapper .o + startup_array.o | none by design | sim-keyed skip / assert absence |
| 5 | :191 | link w/o C impl (no vdir) | no | same as #3 |
| 6 | :192 | wrapper .o + startup_array.o in cwd | none by design | same as #4 |
| 7 | :363 | `-q` ⇒ empty link log | link log inherently noisy | **bsc bug/gap**: quiet not plumbed to `bsc_build_vsim_verilator`; short-term verilator-keyed sed filter |

`-no-use-dpi` exists and per-test override works (implied flag is prepended), but it only rescues the four artifact checks (#1/2/4/6 — bsc creates those files before the simulator script runs) while leaving #3/#5 failing, since verilator cannot consume VPI-flavored Verilog; it is therefore not a standalone resolution for this section.