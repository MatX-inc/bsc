interface RdyAfterWrite;
   method Action setR(Bit#(8) x);
   method Action useR;
endinterface

// A ready signal that reads, out of order, a register an action method
// writes.  setR writes r and reads s; useR's implicit condition, its RDY,
// reads r, and useR writes s.  bsc exports setR C useR and executes setR
// first, so useR reads r after setR's write, which r's annotations forbid.
// useR's read is arbitrary only when useR fires after setR wrote r: in a
// cycle in which the parent enables setR (and so not useR), RDY_useR is
// still an output and must be the hardware's.
//
// Checked (bsc 2026.07-13): setR C useR; order setR, useR.
//
// Compile: bsc -u -verilog -elab -show-schedule RdyAfterWrite.bsv
(* synthesize *)
module mkRdyAfterWrite(RdyAfterWrite);
   Reg#(Bit#(8)) r   <- mkReg(0);
   Reg#(Bit#(8)) s   <- mkReg(0);
   Reg#(Bit#(8)) out <- mkReg(0);

   method Action setR(Bit#(8) x);
      r <= x;
      out <= s;
   endmethod

   method Action useR() if (r == 0);
      s <= s + 1;
   endmethod
endmodule
