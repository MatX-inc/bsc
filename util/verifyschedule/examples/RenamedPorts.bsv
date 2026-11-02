interface RenamedPorts;
   (* enable = "go_a", ready = "ok_a" *)
   method Action a(Bit#(8) x);
   (* enable = "go_b", always_ready *)
   method Action b;
   (* always_ready *)
   method Bit#(8) v;
endinterface

(* synthesize *)
module mkRenamedPorts(RenamedPorts);
   Reg#(Bit#(8)) r <- mkReg(0);

   method Action a(Bit#(8) x) if (r != 9);
      r <= r + x;
   endmethod

   method Action b;
      r <= r + 1;
   endmethod

   method Bit#(8) v = r;
endmodule
