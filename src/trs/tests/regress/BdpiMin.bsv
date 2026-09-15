import "BDPI" function Bit#(32) c_mix(Bit#(32) a, Bit#(32) b);
import "BDPI" function Bit#(128) c_wide(Bit#(128) x);
(* synthesize *)
module sysBdpiMin();
  Reg#(Bit#(16))  cyc <- mkReg(0);
  Reg#(Bit#(32))  acc <- mkReg(1);
  Reg#(Bit#(128)) wac <- mkReg(128'h1);
  rule step;
    acc <= c_mix(acc, zeroExtend(cyc));
    wac <= c_wide(wac) ^ {96'd0, acc};
    cyc <= cyc + 1;
    if (cyc == 50) begin
      $display("acc=%h wac=%h", acc, wac);
      $finish(0);
    end
  endrule
endmodule
