import Vector::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValueStructCond ();
   Vector#(2, Reg#(Bool)) rs <- replicateM(mkReg(False));
   Reg#(Bit#(1)) idx <- mkReg(0);
   Reg#(Bool) c <- mkReg(False);
   Reg#(Bool) d <- mkReg(False);
   Reg#(Bit#(32)) r <- mkReg(0);
   Reg#(Bit#(32)) r2 <- mkReg(0);

   function Bool pick(Bit#(1) i) = map(readReg, rs)[i];

   // the call and the use are under one condition, written twice: a
   // dynamic selection from an array that each use builds again ...
   rule a;
      Bit#(32) x = ?;
      if (pick(idx)) x <- my_count(1);
      if (pick(idx)) r <= x;
   endrule

   // ... or the same reads of two registers
   rule b;
      Bit#(32) x = ?;
      if (c && d) x <- my_count(2);
      if (c && d) r2 <= x;
   endrule
endmodule
