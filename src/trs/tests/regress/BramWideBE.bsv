import BRAMCore::*;

(* synthesize *)
module sysBramWideBE();
  // 1024-bit data, 8-bit chunks -> 128 write enables: lanes past 64
  // exercise the wide-enable path
  BRAM_PORT_BE#(Bit#(4), Bit#(1024), 128) ram <- mkBRAMCore1BE(16, False);
  Reg#(Bit#(4)) st <- mkReg(0);

  Bit#(1024) aaaa = (~0 / 15) * 10;  // 0xAAAA...A across all 1024 bits

  rule step;
    st <= st + 1;
    case (st)
      0: ram.put('1, 0, aaaa);              // all lanes: 0xAA everywhere
      1: ram.put((1 << 127) | 1, 0, 0);     // only lanes 127 and 0
      2: ram.put(0, 0, ?);                  // read
      4: begin
           $display("data = %h", ram.read);
           $finish(0);
         end
    endcase
  endrule
endmodule
