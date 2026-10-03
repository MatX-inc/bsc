package MutuallyExclusiveOrderScramble;

// The probe's two rules, declared in the order declaredFirst, namedFirst
// and with no attribute: compiled before the probe in a batch, this module
// creates the rule names RL_declaredFirst and RL_namedFirst in that order.
(* synthesize *)
module mkMutuallyExclusiveOrderScramble(Empty);
   Reg#(UInt#(8)) count <- mkReg(0);

   rule declaredFirst (count < 10);
      count <= count + 1;
   endrule

   rule namedFirst (count > 20);
      count <= count - 1;
   endrule
endmodule

endpackage
