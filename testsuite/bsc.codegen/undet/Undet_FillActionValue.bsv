// Under -split-undetermined-vals, the undetermined bits of one arm are not
// taken from the result of an ActionValue method in the other arm, unless
// -opt-undetermined-vals is given for Verilog (the same rule as for a whole
// undetermined arm).

interface FillAVSub;
   method ActionValue#(Bit#(5)) get();
endinterface

(* synthesize *)
module mkUndet_FillAVSub(FillAVSub);
   Reg#(Bit#(5)) r <- mkReg(3);
   method ActionValue#(Bit#(5)) get();
      r <= r + 7;
      return r;
   endmethod
endmodule

(* synthesize *)
module sysUndet_FillActionValue(Empty);
   FillAVSub s <- mkUndet_FillAVSub;
   Reg#(Bit#(8)) cnt <- mkReg(0);
   Reg#(Bit#(8)) x <- mkReg(0);
   rule step;
      Bit#(8) nx;
      if (cnt[0] == 1) begin
         let v <- s.get;
         nx = {3'b110, v};
      end
      else
         nx = {3'b001, ?};
      x <= nx;
      cnt <= cnt + 1;
   endrule
endmodule
