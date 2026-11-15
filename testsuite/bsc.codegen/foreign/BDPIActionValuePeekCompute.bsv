import GetPut::*;
import Vector::*;
import BDPIActionValueImport::*;

function Bool isZero (Bit#(32) x) = (x == 0);

(* synthesize *)
module mkBDPIActionValuePeekCompute ();
   Reg#(Bit#(32)) r <- mkReg(42);
   Reg#(Bit#(32)) s <- mkReg(0);

   // peekGet reads the value of g.get without running it.  The steps
   // compute from the results of my_count and my_flag (a primitive, a
   // conversion, a function given to fmap, an if, && and ||, an if whose
   // value is an Integer, a Vector), but the value returned does not
   // depend on them, so they are not used
   Get#(Bit#(32)) g = toGet(actionvalue
                               let n <- my_count(1);
                               let z <- fmap(isZero, my_count(2));
                               let b <- my_flag(1);
                               UInt#(32) u = unpack(n + 1);
                               Integer k = (n == 0) ? 1 : 2;
                               Vector#(4, Bit#(8)) v = map(\+ (1), unpack(n));
                               if (b && !z || n > 5)
                                  s <= pack(u) + fromInteger(k) + zeroExtend(v[1]);
                               return r;
                            endactionvalue);

   rule go;
      let p = peekGet(g);
      let x <- g.get;
      $display("peek %0d get %0d", p, x);
      $finish(0);
   endrule
endmodule
