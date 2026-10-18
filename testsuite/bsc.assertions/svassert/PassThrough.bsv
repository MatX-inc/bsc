// The BSV frontend's own SVA syntax, passed through to the Verilog
import SVA::*;

(* synthesize *)
module mkPassThrough(Empty);
   Reg#(Bool) req <- mkReg(False);
   Reg#(Bool) ack <- mkReg(False);
   rule tick;
      req <= !req;
      ack <= req;
   endrule
   always assert property (req |-> ##1 ack) else $display("FAIL");
endmodule
