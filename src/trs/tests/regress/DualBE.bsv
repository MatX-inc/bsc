import BRAMCore::*;

(* synthesize *)
module sysDualBE();
  // 16-entry, 32-bit, 4 byte lanes, dual-port with byte enables
  BRAM_DUAL_PORT_BE#(Bit#(4), Bit#(32), 4) b <- mkBRAMCore2BE(16, False);
  Reg#(Bit#(6)) t <- mkReg(0);

  rule drive;
    let i = t[3:0];
    case (t[5:4])
      // phase 0: A writes low half, B writes high half, SAME address
      0: begin
        b.a.put(4'b0011, i, {16'hAAAA, 8'hA0 | zeroExtend(i[2:0]), 8'h11});
        b.b.put(4'b1100, i, {8'hB0 | zeroExtend(i[2:0]), 8'hBB, 16'h2222});
      end
      // phase 1: overlapping lanes (both write byte 1), A vs B order
      1: begin
        b.a.put(4'b0010, i, 32'h00AA_0000 >> 8);
        b.b.put(4'b0110, i, {8'h00, 8'hBC, 8'hBD, 8'h00});
      end
      // phase 2: A reads while B writes same address (bypass semantics)
      2: begin
        b.a.put(4'b0000, i, 0);
        b.b.put(4'b1111, i, 32'hC0DE_0000 | zeroExtend(t));
      end
      // phase 3: read everything back on both ports
      3: begin
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
