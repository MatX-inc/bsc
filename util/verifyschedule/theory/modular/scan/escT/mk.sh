#!/bin/bash
# mk.sh: the task-site escape probe (cost review r8b-1 M1) through the real emitter, and its negative:
# EscT8 (one escaped pair, 8 task statements in each step), EscT_neg_8 (the bound `lo` raised past the second
# step's first statement: must fail at escT2S_0's decision). Probes for design.md §17 are in ../../../r8c/escE.
cd "$(dirname "$0")"
E2E_SITES=0 python3 -I genescE.py ../../e2e/tools EscT8.lean EscT8 tasks 8
sed -e 's/Nat.blt (tnum p.1.1 p.2) 8)/Nat.blt (tnum p.1.1 p.2) 9)/g' -e 's/Nat.ble 8 (tnum/Nat.ble 9 (tnum/g' \
    -e 's/escB_0 8 hz1/escB_0 9 hz1/' -e 's/^namespace EscT8$/namespace EscT_neg_8/' -e 's/^end EscT8$/end EscT_neg_8/' \
    -e 's/EscT8\.obl/EscT_neg_8.obl/; s/EscT8\.top/EscT_neg_8.top/' EscT8.lean > EscT_neg_8.lean
