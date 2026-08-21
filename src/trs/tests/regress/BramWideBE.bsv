import BRAMCore::*;

(* synthesize *)
module sysBramWideBE();
  // 1024-bit data, 8-bit chunks -> 128 write enables: lanes past 64
  // exercise the wide-enable path.  memSize 12 < 2^4 so out-of-bounds
  // addresses exist: the bounds warning's Write/Read discriminator is
  // the line the reference got wrong for wide enables (is_zero fix).
  BRAM_PORT_BE#(Bit#(4), Bit#(1024), 128) ram <- mkBRAMCore1BE(12, False);
  Reg#(Bit#(4)) st <- mkReg(0);

  Bit#(1024) aaaa = (~0 / 15) * 10;  // 0xAAAA...A across all 1024 bits

  rule step;
    st <= st + 1;
    case (st)
      0: ram.put('1, 0, aaaa);              // all lanes: 0xAA everywhere
      1: ram.put((1 << 127) | 1, 0, 0);     // only lanes 127 and 0
      2: ram.put(0, 0, ?);                  // read
      3: ram.put('1, 13, aaaa);             // out of bounds, wens!=0: Write warning
      5: ram.put(0, 14, ?);                 // out of bounds, wens==0: Read warning
      6: ram.put(0, 0, ?);                  // re-read addr 0 (read 14 was undet)
      8: begin
           $display("data = %h", ram.read);
           $finish(0);
         end
    endcase
  endrule
endmodule
