import BRAMCore::*;

(* synthesize *)
module sysCollideEq();
  BRAM_DUAL_PORT_BE#(Bit#(4), Bit#(32), 4) b <- mkBRAMCore2BE(16, False);
  Reg#(Bit#(6)) t <- mkReg(0);

  rule drive;
    let i = t[3:0];
    case (t[5:4])
      // phase 0: overlapping lane 1, EQUAL chunk values (the
      // reference's collision warning fires on equality)
      0: begin
        b.a.put(4'b0011, i, {16'h0000, 8'hEE, 8'h11});
        b.b.put(4'b0110, i, {8'h00, 8'hBC, 8'hEE, 8'h00});
      end
      // phase 1: overlapping lane 1, different values (no warning)
      1: begin
        b.a.put(4'b0010, i, {16'h0000, 8'hAA, 8'h00});
        b.b.put(4'b0010, i, {16'h0000, 8'hBD, 8'h00});
      end
      // phase 2-3: read everything back
      default: begin
        b.a.put(4'b0000, i, 0);
        b.b.put(4'b0000, i, 0);
      end
    endcase
    t <= t + 1;
  endrule

  rule show;
    $display("%0d A=%h B=%h", t, b.a.read, b.b.read);
    if (t == 63) $finish(0);
  endrule
endmodule
