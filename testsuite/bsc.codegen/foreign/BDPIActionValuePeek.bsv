import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeek ();
   Get#(Bit#(32)) g = toGet(my_count(1));

   // peekGet reads the value of g.get without running it, but the value
   // is the result of a call of my_count, which exists only when it runs;
   // the $display that uses it is in the rule, which is reported
   rule r;
      let p = peekGet(g);
      let x <- g.get;
      $display("peek %0d get %0d", p, x);
      $finish(0);
   endrule
endmodule
