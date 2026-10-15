module BVIReadyForeign(output [7:0] VALUE, output READY);
   assign VALUE = 8'd42;
   assign READY = 1'b0;
endmodule
