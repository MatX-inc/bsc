// AvMethInline.bsv — ActionValue method call on a USER-MODULE child,
// inside a conditional arm, result consumed downstream.
//
// The compiled lowering inlines the method body (EN protocol, body
// stmts, result expr in the child frame) and phi-binds the result out
// of the taken arm.  The AvAction result def is a SYNTHETIC temp that
// appears in no def table, so its binding width must come from the
// evaluated result — an intermediate version used def_width's 1-bit
// fallback and truncated every taken value to i1 (grid v3's checksum
// caught it; this keeps it caught).
import FIFOF :: *;

interface Pop;
   method Bool ok;
   method ActionValue#(Bit#(32)) take;
   method Action feed(Bit#(32) v);
endinterface

(* synthesize, always_ready *)
module mkPopper(Pop);
   FIFOF#(Bit#(32)) q <- mkUGFIFOF;
   method Bool ok = q.notEmpty;
   method ActionValue#(Bit#(32)) take;
      q.deq;
      return q.first + 1;
   endmethod
   method Action feed(Bit#(32) v);
      if (q.notFull) q.enq(v);
   endmethod
endmodule

(* synthesize *)
module sysAvMethInline(Empty);
   Pop p <- mkPopper;
   Reg#(Bit#(8))  c   <- mkReg(0);
   Reg#(Bit#(32)) acc <- mkReg(0);

   rule drive;
      c <= c + 1;
      p.feed({24'h0, c} * 32'h01000193);
   endrule

   rule drain;
      if (p.ok) begin
         let v <- p.take;
         acc <= (acc ^ v) + 1;
      end
   endrule

   rule fin (c == 100);
      $display("acc %h", acc);
      $finish(0);
   endrule
endmodule
