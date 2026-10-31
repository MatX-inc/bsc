// A user wrapper around a class method costs no unfolding step: the
// wrapper is harmless and the method it selects (+ on Integer, on
// Bit#(8)) is simple.  A chain of 300 wrappers of each kind fits in
// a budget of 100 steps.

function Integer inc(Integer x) = x + 1;
function Bit#(8) incb(Bit#(8) x) = x + 1;

(* synthesize *)
module sysHarmlessChain();
   Reg#(Bit#(8)) r <- mkReg(0);

   rule r1;
      Integer v =
      inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(
      inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(
      inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(
      inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(
      inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(
      inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(
      inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(
      inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(
      inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(
      inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(
      inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(
      inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(
      inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(
      inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(
      inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(
      0
      ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      $display(v);
      r <=
      incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(
      incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(
      incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(
      incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(
      incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(
      incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(
      incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(
      incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(
      incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(
      incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(
      incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(
      incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(
      incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(
      incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(
      incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(
      incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(
      incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(
      incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(
      incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(
      incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(incb(
      r
      ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
   endrule
endmodule
