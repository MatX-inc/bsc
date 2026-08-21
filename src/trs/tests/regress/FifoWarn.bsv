// Guarded-FIFO warn arms (bs_prim_mod_fifo.h): enq-to-full and
// deq-from-empty print the reference warning and drop the op.  An
// unguarded interface (mkUGFIFOF) lets the design reach both arms.
// Under TRS_RUNCORE=1 these are compiled prim call sites serviced by
// the boot's natively restored Fifo (rung 3b) — byte parity of the
// warning text, the drop semantics, and the surviving contents is
// the witness.
import FIFOF::*;

(* synthesize *)
module sysFifoWarn();
    FIFOF#(Bit#(8)) f <- mkUGFIFOF;
    Reg#(Bit#(8)) c <- mkReg(0);

    rule step;
        c <= c + 1;
        if (c < 4) begin
            // depth 2: c=2,3 enqueue into a full fifo (warn + drop)
            f.enq(c);
            $display("enq %0d notFull=%b", c, f.notFull);
        end
        else if (c < 6) begin
            // the two survivors must be 0 and 1 (drops really dropped)
            $display("deq %0d first=%0d", c, f.first);
            f.deq;
        end
        else if (c == 6) begin
            // empty now: deq warns and is ignored
            f.deq;
            $display("deq on empty notEmpty=%b", f.notEmpty);
        end
        else
            $finish(0);
    endrule
endmodule
