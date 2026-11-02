"""LcProbe<N>.bs: N rules write one register in every cycle (one last-caller port of N callers)."""
import sys
n = int(sys.argv[1])
rules = '\n'.join('      "r%d": when True ==> x := %d' % (i, i) for i in range(n))
print('''package LcProbe%d(mkLcProbe%d) where

{-# verilog mkLcProbe%d #-}
mkLcProbe%d :: Module Empty
mkLcProbe%d =
  module
    x :: Reg (Bit 16) <- mkReg 0
    rules
%s
''' % (n, n, n, n, n, rules))
