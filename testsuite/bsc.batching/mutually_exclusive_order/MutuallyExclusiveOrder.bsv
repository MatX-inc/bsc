// Two rules, declared in one order and named by the mutually_exclusive
// attribute in the other.  The compiler cannot tell whether the two
// registers are ever set together, so it generates the runtime check; the
// rule the attribute names first should host it, be the first operand of
// its condition and be named first in its message.  Each rule has a
// register of its own so that nothing else in the module depends on the
// order of the two rules.

(* synthesize *)
module sysMutuallyExclusiveOrder(Empty);

   Reg#(Bool)     goDeclaredFirst <- mkReg(False);
   Reg#(Bool)     goNamedFirst    <- mkReg(False);
   Reg#(UInt#(8)) countA          <- mkReg(0);
   Reg#(UInt#(8)) countB          <- mkReg(0);

   rule declaredFirst (goDeclaredFirst);
      countA <= countA + 1;
   endrule

   (* mutually_exclusive = "namedFirst, declaredFirst" *)
   rule namedFirst (goNamedFirst);
      countB <= countB + 2;
   endrule

endmodule
