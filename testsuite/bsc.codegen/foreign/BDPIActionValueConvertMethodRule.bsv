import BDPIActionValueImport::*;

interface ConvertGet;
   method ActionValue#(Bit#(32)) get();
   method Bit#(32) last();
endinterface

// A rule and a method of one module that both run the same converted
// action and can run in one cycle.  They share the one call that
// toActionValue_ makes, and nothing reports it.
//
// This is a reproducer for that open case.  foreign.exp does not run it,
// because a test that compares the two simulations fails.
(* synthesize *)
module mkBDPIActionValueConvertMethodRuleSub (ConvertGet);
   Reg#(Bit#(32)) r <- mkReg(0);

   let av_ = toActionValue_(my_count(1));

   rule run;
      let x <- fromActionValue_(av_);
      r <= x;
   endrule

   method ActionValue#(Bit#(32)) get();
      let y <- fromActionValue_(av_);
      return y;
   endmethod

   method Bit#(32) last() = r;
endmodule

(* synthesize *)
module mkBDPIActionValueConvertMethodRule ();
   ConvertGet s <- mkBDPIActionValueConvertMethodRuleSub;
   Reg#(Bit#(8)) cyc <- mkReg(0);

   rule tick;
      cyc <= cyc + 1;
      if (cyc == 3) $finish(0);
   endrule

   rule take;
      let v <- s.get;
      $display("cycle %0d: rule %0d method %0d", cyc, s.last, v);
   endrule
endmodule
