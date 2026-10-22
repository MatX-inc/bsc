import SVA::*;

(* synthesize *)
module mkPassThroughActions(Empty);
   Reg#(Bool) req <- mkReg(False);
   Reg#(Bool) ack <- mkReg(False);
   rule tick;
      req <= !req;
      ack <= req;
   endrule
   always assert property (req |-> ##[1:2] ack) else $display("FAIL");
endmodule
