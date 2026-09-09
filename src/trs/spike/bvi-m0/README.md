# M0 spike: BVI-via-Verilator de-risk battery

Prototype of the trs BVI link pipeline (design: KB draft "KB:
BVI-via-Verilator design (trs)", v4) built BEFORE any compiler code, to
find integration reality before it finds us.  `sh run.sh [verilator]`
runs all nine fixtures; everything passes on distro Verilator 5.020
(XML metadata) and source-built 5.050 (JSON metadata).

## Pipeline pieces (prototypes of R3/R4)

- `metaparse.py` — versioned metadata adapter: `--xml-only` (< 5.046) /
  `--json-only` (>= 5.046), one normalized result.
- `shimgen.py` — contract + metadata -> shim.cpp implementing the
  engine-neutral C ABI (per-instance VerilatedContext, threadContextp on
  every entry, VL_PRINTF redirect, vl_fatal containment, final() once).
- `driver.py` — verilate/build/dlopen + the v4 shadow-vector protocol in
  ctypes (observation frontiers, three-phase batched edge commit,
  TRS_BVI_CHECK=observe witnesses) + the fixture scenarios.

## Fixtures

counter, shadow (self-SBR replacement; the ratified atomic-read
condition), argrdy (arg-dependent RDY), liar (undeclared path -> sound
DYNAMIC_LIE witness with attribution), xing (coincident two-clock NBA
batching; the sequential-protocol shoot-through is asserted as the
divergence), violator (arg-transition-clocked state; witnessed), xprobe
(=== 1'bx readiness under two-state; accepted CONTROL divergence,
asserted), params (typed -G: signed / string / real / 96-bit wide,
semantic equality; display capture through the redirect), fatal
($fatal/$stop containment: host survives, error surfaces, message
retrievable).

`bsv/` holds iverilog-oracle twins: the same .v imported as real BVIs,
compiled by bsc, run under iverilog -- TwinCounter prints exactly the
harness's pre-edge series (0,3,6,9); TwinShadow prints v=21 / last=20,
matching the harness's shadow semantics cycle for cycle.

## M0 discoveries (design deltas already applied)

1. A --no-timing dump CANNOT power the delay refusal: verilator discards
   delays before dumping and -Werror-*DLY stays silent in dump-only
   mode.  The inspection dump runs with --timing; the build stays
   --no-timing.
2. 5.020's XML has NO DPI marker at all.  The portable detector is
   verilator's own emission: V<top>__Dpi.h exists iff DPI is present;
   the driver backstops with it after --cc.
3. -G on an undeclared parameter is a hard verilator error on both
   versions natively -- no bespoke absent-parameter check needed.
4. Assertion failure on 5.020 routes through $stop; the contained fatal
   message carries the source location, the user text goes to the
   output stream.

## Q2 (Verilator version floor) -- ANSWERED EMPIRICALLY

Floor = 5.020 (distro), via the XML adapter + the __Dpi.h backstop.
Post-5.046 releases work via the JSON adapter (proven on 5.050).  The
adapter layer is mandatory, not defensive: 5.050 rejects --xml-only
outright.  No --timing needed anywhere in v1 (the inspection dump's
--timing is analysis-only).

## Delegated

The static path checker (Yosys structural reachability + SAT miter) is
Ravi's external BVI flow per standing decision; the liar fixture is the
shared test vector for it.  yosys is deliberately NOT part of this
pipeline.
