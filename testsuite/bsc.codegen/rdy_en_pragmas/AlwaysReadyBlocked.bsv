package AlwaysReadyBlocked;

// G0006: val must read x before r writes it but w after r sets it, so they
// conflict; r is more urgent, so RDY_val = CAN_FIRE_val && !WILL_FIRE_r.

interface AlwaysReadyBlocked;
   method Bit#(8) val;
   method Bit#(8) cur;
endinterface

(* synthesize, always_ready *)
module mkAlwaysReadyBlocked(AlwaysReadyBlocked);
   Reg#(Bit#(8)) x <- mkReg(0);
   RWire#(Bit#(8)) w <- mkRWire;

   (* descending_urgency = "r, val" *)
   rule r (x[0] == 1);
      w.wset(x);
      x <= x + 1;
   endrule

   // advances x when r does not, so r fires every other cycle
   rule s (x[0] == 0);
      x <= x + 1;
   endrule

   method Bit#(8) val = isValid(w.wget) ? x : 0;
   method Bit#(8) cur = x;
endmodule

// While r fires, every serialization of r and val gives 0 or x+1, but with
// -unsafe-always-ready the parent reads val = x.
(* synthesize *)
module sysAlwaysReadyBlocked();
   AlwaysReadyBlocked dut <- mkAlwaysReadyBlocked;
   Reg#(UInt#(4)) cycle <- mkReg(0);

   rule check;
      Bit#(8) x = dut.cur;
      Bit#(8) v = dut.val;
      Bool rFires = x[0] == 1;
      Bit#(8) inOrder = 0;                  // val, r: val sees w empty
      Bit#(8) rFirst  = rFires ? x + 1 : 0; // r, val: val sees r's write
      $display("cycle %0d: x=%0d r fires=%b  atomic val (order val,r)=%0d (order r,val)=%0d  hardware val=%0d",
               cycle, x, rFires, inOrder, rFirst, v);
      if (v != inOrder && v != rFirst)
         $display("UNSOUND: val=%0d while r fires, but every serialization of r and val gives %0d or %0d",
                  v, inOrder, rFirst);
      cycle <= cycle + 1;
      if (cycle == 3) $finish(0);
   endrule
endmodule

endpackage
