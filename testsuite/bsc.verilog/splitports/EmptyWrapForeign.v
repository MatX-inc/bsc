module EmptyWrapForeign(
    input CLK,
    input RST_N,
    input [7:0] VALUE
);
   always @(posedge CLK)
      if (RST_N && VALUE != 8'ha5)
         $display("FAIL: imported empty interface argument");
endmodule
