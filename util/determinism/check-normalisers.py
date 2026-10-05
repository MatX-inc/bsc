#!/usr/bin/env python3
"""check-normalisers.py -- exercise compare.py's normalisers on captured samples.

Each case is a pair of file contents under a file name, taken (shortened) from
gate archives of 2026-10-05, and the verdict compare.py must reach for them:
'same' when the two differ only by the noise the normaliser is for, 'differ'
when a real difference sits next to that noise or the file is of a kind the
normaliser must leave alone.  Run it from anywhere; it exits 1 on a failure.

    util/determinism/check-normalisers.py
"""

import os
import sys

sys.dont_write_bytecode = True   # no __pycache__ next to compare.py
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import compare  # noqa: E402

RTS_A = b"""\
    Alloc    Copied     Live     GC     GC      TOT      TOT  Page Flts
    bytes     bytes     bytes   user   elap     user     elap
checking package dependencies
  4338176    147992    230016  0.002  0.003    0.005    0.011    0    0  (Gen:  0)
compiling VsortWorkaround.bsv
262531776  12465400  12607544  0.055  0.055    0.236    0.246    0    0  (Gen:  1)
code generation for module_beSort_2 starts
Verilog file created: module_beSort_2.v
Elaborated module file created: module_beSort_2.ba
All packages are up to date.
 91003120      6584     88608  0.002  0.002    3.826    3.910    0    0  (Gen:  1)
     1136                      0.000  0.000

   4,649,933,432 bytes allocated in the heap
     267,409,376 bytes copied during GC
      53,333,928 bytes maximum residency (5 sample(s))
         208,984 bytes maximum slop
             271 MiB total memory in use (0 MiB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0        20 colls,     0 par    0.450s   0.455s     0.0227s    0.0755s
  Gen  1         5 colls,     0 par    0.276s   0.280s     0.0559s    0.0828s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    3.101s  (  3.176s elapsed)
  GC      time    0.725s  (  0.734s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    3.826s  (  3.910s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    1,499,732,740 bytes per MUT second

  Productivity  81.0% of total user, 81.2% of total elapsed

"""
RTS_B = b"""\
    Alloc    Copied     Live     GC     GC      TOT      TOT  Page Flts
    bytes     bytes     bytes   user   elap     user     elap
checking package dependencies
  4338176    147992    230016  0.002  0.006    0.005    0.010    0    0  (Gen:  0)
compiling VsortWorkaround.bsv
262531776  12465400  12607544  0.053  0.101    0.250    0.533    0    0  (Gen:  1)
code generation for module_beSort_2 starts
Verilog file created: module_beSort_2.v
Elaborated module file created: module_beSort_2.ba
All packages are up to date.
 91003128      6584     88608  0.002  0.002    3.900    4.577    0    0  (Gen:  1)
     1136                      0.000  0.001

   4,649,933,440 bytes allocated in the heap
     267,409,376 bytes copied during GC
      53,333,928 bytes maximum residency (5 sample(s))
         208,984 bytes maximum slop
             271 MiB total memory in use (0 MiB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0        20 colls,     0 par    0.470s   0.901s     0.0235s    0.0801s
  Gen  1         5 colls,     0 par    0.280s   0.283s     0.0560s    0.0830s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    3.150s  (  3.390s elapsed)
  GC      time    0.750s  (  1.184s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    3.900s  (  4.577s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    1,476,170,933 bytes per MUT second

  Productivity  80.8% of total user, 74.1% of total elapsed

"""

VCOMP_A = b"""\
checking package dependencies
compiling SimpleSwitch.bsv
code generation for mkDutWrapped starts
Verilog file created: mkDutWrapped.v
Elaborated module file created: mkDutWrapped.ba
All packages are up to date.
"""
VCOMP_B = VCOMP_A.replace(b"Verilog file created: mkDutWrapped.v", b"Verilog file reused: mkDutWrapped.v")

CCOMP_A = b"""\
Bluesim object created: mkGCD.{h,o}
Bluesim object created: mkTbGCD.{h,o}
Bluesim object created: model_mkTbGCD.{h,o}
exec: make   -f "compile_mkTbGCD.mk" -j 2 all
make[2]: Entering directory '/bazel-cache/ravi/bsc-det/x/trees/normal/testsuite/bsc.bluesim/parallel'
c++ -O3 -c -o mkTbGCD.o mkTbGCD.cxx
make[2]: Leaving directory '/bazel-cache/ravi/bsc-det/x/trees/normal/testsuite/bsc.bluesim/parallel'
 elapsed time: CPU 0.00s, real 0.40s

"""
CCOMP_B = b"""\
Bluesim object created: mkGCD.{h,o}
Bluesim object reused: mkTbGCD.{h,o}
Bluesim object created: model_mkTbGCD.{h,o}
exec: make   -f "compile_mkTbGCD.mk" -j 2 all
make[1]: Entering directory '/bazel-cache/ravi/bsc-det/x/trees/reversed/testsuite/bsc.bluesim/parallel'
c++ -O3 -c -o mkTbGCD.o mkTbGCD.cxx
make[1]: Leaving directory '/bazel-cache/ravi/bsc-det/x/trees/reversed/testsuite/bsc.bluesim/parallel'
 elapsed time: CPU 0.01s, real 0.42s

"""
# the same transcript with a different object: a real difference
CCOMP_OTHER = CCOMP_A.replace(b"Bluesim object created: mkGCD.{h,o}", b"Bluesim object created: mkGCD2.{h,o}")
# the objects in the other order: the bluesim-package-map-order leak, a real difference
CCOMP_MOVED = CCOMP_A.replace(
    b"Bluesim object created: mkGCD.{h,o}\nBluesim object created: mkTbGCD.{h,o}\n",
    b"Bluesim object created: mkTbGCD.{h,o}\nBluesim object created: mkGCD.{h,o}\n")

# a reused .v is reported before the other modules' code generation, a created
# one after it (bsc.bsv_examples/MacTestBench/mkTbEnv.bsc-vcomp-out, 2026-10-05)
TBENV_A = b"""\
checking package dependencies
compiling TbEnv.bsv
code generation for module_calculateCrcNext starts
code generation for mkTbEnv starts
Verilog file created: mkTbEnv.v
Elaborated module file created: mkTbEnv.ba
Verilog file created: module_calculateCrcNext.v
Elaborated module file created: module_calculateCrcNext.ba
All packages are up to date.
"""
TBENV_B = b"""\
checking package dependencies
compiling TbEnv.bsv
code generation for module_calculateCrcNext starts
Verilog file reused: module_calculateCrcNext.v
code generation for mkTbEnv starts
Verilog file created: mkTbEnv.v
Elaborated module file created: mkTbEnv.ba
Elaborated module file created: module_calculateCrcNext.ba
All packages are up to date.
"""

FSTVCD_A = b"""\
$date
\tMon Oct  5 04:09:22 2026

$end
$version
\tfst2vcd
$end
$scope module main $end
"""
FSTVCD_B = FSTVCD_A.replace(b"04:09:22", b"04:09:25")

ELAB_A = b"""\
starting imports
read /x/inst/lib/Libraries/Prelude.bo
imports done
 elapsed time: CPU 0.09s, real 0.09s

total
 elapsed time: CPU 0.36s, real 0.36s

"""
ELAB_B = ELAB_A.replace(b"CPU 0.09s, real 0.09s", b"CPU 0.08s, real 0.12s").replace(b"real 0.36s", b"real 0.41s")

MODEL_A = b"""\
/*
 * Generated by Bluespec Compiler, version A0-34-gee766708 (build ee766708)
 *
 * On Mon Oct  5 01:24:11 UTC 2026
 *
 */
#include "bluesim_primitives.h"
void MODEL_sysWireTypeTest::get_version(char const **name, char const **build)
{
  *name = "A0-34-gee766708";
  *build = "ee766708";
}

/* Get the model creation time */
time_t MODEL_sysWireTypeTest::get_creation_time()
{
  /* Mon Oct  5 01:24:11 UTC 2026 */
  return 1791163451llu;
}
"""
MODEL_B = MODEL_A.replace(b"Mon Oct  5 01:24:11 UTC 2026", b"Mon Oct  5 01:07:53 UTC 2026").replace(b"1791163451llu", b"1791162473llu")
# a model whose creation time was zeroed (-no-show-timestamps) against one that was not
MODEL_ZERO = MODEL_A.replace(b"  /* Mon Oct  5 01:24:11 UTC 2026 */\n  return 1791163451llu;\n", b"  /* Thu Jan  1 00:00:00 UTC 1970 */\n  return 0llu;\n")

DIFF_A = b"""\
--- FixedPointLibrary.bo.dumpbi-out.expected\t2026-10-05 01:19:09.770390670 +0000
+++ FixedPointLibrary.bo.dumpbi-out\t2026-10-05 01:27:19.041978087 +0000
@@ -1,3 +1,3 @@
 signature FixedPointLibrary where {
-import Prelude;
+++ a body line that begins with plus signs stays as it is
"""
DIFF_B = b"""\
--- FixedPointLibrary.bo.dumpbi-out.expected\t2026-10-05 00:56:57.151514438 +0000
+++ FixedPointLibrary.bo.dumpbi-out\t2026-10-05 01:07:22.342980395 +0000
@@ -1,3 +1,3 @@
 signature FixedPointLibrary where {
-import Prelude;
+++ a body line that begins with plus signs stays as it is
"""
DIFF_BODY = DIFF_B.replace(b"-import", b"-export")

CASES = [
    # (name, file name, A, B, expected verdict)
    ("Verilog file created/reused", "bsc.bsv_examples/MacTestBench/mkSimpleSwitch.bsc-vcomp-out", VCOMP_A, VCOMP_B, "same"),
    ("Bluesim object reused, make level and directory, elapsed", "bsc.bluesim/parallel/mkTbGCD-2.bsc-ccomp-out", CCOMP_A, CCOMP_B, "same"),
    ("a different object name is a real difference", "bsc.bluesim/parallel/mkTbGCD-2.bsc-ccomp-out", CCOMP_A, CCOMP_OTHER, "differ"),
    ("Bluesim objects in another order is a real difference (bluesim-package-map-order)", "bsc.bluesim/vcd/sysVCDTest1.bsc-ccomp-out", CCOMP_A, CCOMP_MOVED, "differ"),
    ("Elaborated module files in another order is a real difference", "bsc.bsv_examples/MacTestBench/mkTbEnv.bsc-vcomp-out", TBENV_A,
     TBENV_A.replace(b"Elaborated module file created: mkTbEnv.ba\nVerilog file created: module_calculateCrcNext.v\nElaborated module file created: module_calculateCrcNext.ba\n",
                     b"Elaborated module file created: module_calculateCrcNext.ba\nVerilog file created: module_calculateCrcNext.v\nElaborated module file created: mkTbEnv.ba\n"), "differ"),
    ("Verilog file reused reported earlier than created", "bsc.bsv_examples/MacTestBench/mkTbEnv.bsc-vcomp-out", TBENV_A, TBENV_B, "same"),
    ("a missing Verilog file line is a real difference", "bsc.bsv_examples/MacTestBench/mkTbEnv.bsc-vcomp-out", TBENV_A,
     TBENV_A.replace(b"Verilog file created: module_calculateCrcNext.v\n", b""), "differ"),
    ("fst2vcd output: the $date section", "bsc.bluesim/fst/sysFstDump.fst-vcd", FSTVCD_A, FSTVCD_B, "same"),
    ("elapsed time lines of a -v transcript", "bsc.verilog/elab_only/elabv.bsc-out", ELAB_A, ELAB_B, "same"),
    ("+RTS -S rows and -s summary", "bsc.bugs/bluespec_inc/b1490/VsortWorkaround.bsv.bsc-vcomp-out", RTS_A, RTS_B, "same"),
    ("RTS table next to a moved module is a real difference", "bsc.bugs/bluespec_inc/b1490/VsortWorkaround.bsv.bsc-vcomp-out",
     RTS_A, RTS_B.replace(b"module_beSort_2", b"module_beSort_4"), "differ"),
    ("the same text as a dump output is not a transcript", "bsc.bsv_examples/MacTestBench/mkSimpleSwitch.bo.dumpbo-out", VCOMP_A, VCOMP_B, "differ"),
    ("the same text as a .diff-out is not a transcript", "bsc.x/y.bsc-vcomp-out.diff-out", VCOMP_A, VCOMP_B, "differ"),
    ("model .cxx: ' * On' header, creation-time comment and epoch literal", "bsc.bluetcl/vcd_correlation/sim/model_sysWireTypeTest.cxx", MODEL_A, MODEL_B, "same"),
    ("model .cxx: the pair goes whatever the time, a zeroed one included", "bsc.bluetcl/vcd_correlation/sim/model_sysWireTypeTest.cxx", MODEL_A, MODEL_ZERO, "same"),
    ("non-model .cxx keeps the return line", "bsc.bluetcl/vcd_correlation/sim/sysWireTypeTest.cxx", MODEL_A, MODEL_B, "differ"),
    ("harness diff header timestamps", "bsc.bugs/bluespec_inc/b1197/FixedPointLibrary.bo.dumpbi-out.diff-out", DIFF_A, DIFF_B, "same"),
    ("harness diff body change is a real difference", "bsc.bugs/bluespec_inc/b1197/FixedPointLibrary.bo.dumpbi-out.diff-out", DIFF_A, DIFF_BODY, "differ"),
    ("binaries are byte-exact", "bsc.x/Foo.bo", VCOMP_A, VCOMP_B, "differ"),
]


def main():
    failures = 0
    for name, rel, a, b, expected in CASES:
        if compare.normalisers_for(rel):
            got = "same" if compare.normalise(rel, a) == compare.normalise(rel, b) else "differ"
        else:
            got = "same" if a == b else "differ"
        ok = got == expected
        failures += not ok
        print(f"{'ok  ' if ok else 'FAIL'}  {name}  [{rel.rsplit('/', 1)[-1]}: expected {expected}, got {got}]")
    # the normalised transcript must keep every line that is not noise
    kept = compare.normalise("x.bsc-ccomp-out", CCOMP_A)
    for must in (b"exec: make", b"c++ -O3", b"Bluesim object created: mkGCD.{h,o}\n", b"make: Entering directory\n",
                 b"Bluesim object created: mkTbGCD.{h,o}\n"):
        if must not in kept:
            failures += 1
            print(f"FAIL  normalised transcript lost {must!r}")
    print(f"{len(CASES)} cases, {failures} failures")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
