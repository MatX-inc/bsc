import FIFO::*;

(* synthesize *)
module sysFifoVcd();
  FIFO#(Bit#(8)) f <- mkFIFO;
  Reg#(Bit#(6)) t <- mkReg(0);

  rule start (t == 0);
    $dumpvars;
  endrule

  rule enq (t[0] == 0);
    f.enq(8'hC0 | zeroExtend(t[3:0]));
  endrule

  rule deq (t[0] == 1);
    $display("%0d got %h", t, f.first);
    f.deq;
  endrule

  rule tick;
    t <= t + 1;
    if (t == 40) $finish(0);
  endrule
endmodule
