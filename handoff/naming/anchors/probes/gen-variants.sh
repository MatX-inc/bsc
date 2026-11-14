#!/bin/bash
# usage: gen-variants.sh
# Writes the stability variants of src/Anchors.bsv:
#   E1  unrelated register + rule inserted before all other state (EDIT1)
#   E2  unrelated value method added at the end of the interface (EDIT2*)
#   E3  unrelated register + rule inserted after the other rules (EDIT3)
#   E4  one more writer of rc (a related edit), after the other rules
set -e
P=/home/ravi/claude-tmp/claude-1548697660/-home-ravi/6640a2f0-3ba9-46af-8f63-2f02110094cd/scratchpad/isimp/work/anchors/anchors-inv2/probes
S=$P/src/Anchors.bsv
sed -e 's/^package Anchors;/package AnchorsE1;/' \
    -e 's|^   //EDIT1$|   Reg#(Bit#(8)) ux <- mkReg(0);\n   rule r_unrel1;\n      ux <= ux + 3;\n   endrule|' \
    $S > $P/src/AnchorsE1.bsv
sed -e 's/^package Anchors;/package AnchorsE2;/' \
    -e 's|^   //EDIT2I$|   method Bit#(8) extra;|' \
    -e 's|^   //EDIT2M$|   method Bit#(8) extra = rb ^ 8'"'"'h5A;|' \
    $S > $P/src/AnchorsE2.bsv
sed -e 's/^package Anchors;/package AnchorsE3;/' \
    -e 's|^   //EDIT3$|   Reg#(Bit#(8)) uy <- mkReg(0);\n   rule r_unrel3;\n      uy <= uy + 5;\n   endrule|' \
    $S > $P/src/AnchorsE3.bsv
sed -e 's/^package Anchors;/package AnchorsE4;/' \
    -e 's|^   //EDIT3$|   rule r_rc4 (rd == 8'"'"'h77);\n      rc <= rd + 2;\n   endrule|' \
    $S > $P/src/AnchorsE4.bsv
