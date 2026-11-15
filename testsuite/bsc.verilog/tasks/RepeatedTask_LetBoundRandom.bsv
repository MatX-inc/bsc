import Vector::*;

(* synthesize *)
module sysRepeatedTask_LetBoundRandom() ;

   Reg#(Bool) b <- mkReg(True);
   Reg#(Vector#(3, Int#(32))) rgs <- mkReg(replicate(0));

   // each execution of the let-bound ActionValue is its own call of $random
   rule r1 ( b ) ;
      ActionValue#(Int#(32)) next = $random;
      Vector#(3, Int#(32)) v <- replicateM(next);
      rgs <= v;
      b <= False;
   endrule

   rule r2 ( !b ) ;
      $display("distinct = %b", rgs[0] != rgs[1] && rgs[1] != rgs[2] &&
                                rgs[0] != rgs[2]);
      $finish(0) ;
   endrule

endmodule
