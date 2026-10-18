module formal_top(input CLK, input RST_N, input [1:0] req, input req_en);
  `DUT dut(.CLK(CLK), .RST_N(RST_N), .request_1(req), .EN_request(req_en), .grant());
  // hold reset for the first cycle, then release it
  reg init = 1;
  always @(posedge CLK) init <= 0;
  always @(*) if (init) assume (!RST_N); else assume (RST_N);
endmodule
