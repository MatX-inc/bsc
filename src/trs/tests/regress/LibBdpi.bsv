// A BDPI import the COMPILER ships, not the design: Randomizable
// declares rand32/srand and bsc writes their .ba into
// $BLUESPECDIR/Libraries.  The fragment names them and carries no
// signature, so the link has to find rand32.bir where the install put
// it -- beside the .ba -- rather than beside the design.
import Randomizable::*;

(* synthesize *)
module sysLibBdpi();
   Randomize#(Bit#(16)) r <- mkGenericRandomizer;
   Reg#(Bit#(4)) n <- mkReg(0);
   Reg#(Bool) started <- mkReg(False);

   rule start (!started);
      r.cntrl.init;
      started <= True;
   endrule

   rule step (started);
      let v <- r.next;
      $display("%0d", v);
      n <= n + 1;
      if (n == 5) $finish(0);
   endrule
endmodule
