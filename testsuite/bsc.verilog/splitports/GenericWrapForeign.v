// The BVI ABI is one packed port per Payload, even though Payload has a
// custom SplitPorts instance which changes both order and port names.
module GenericWrapForeign #(parameter WIDTH = 16)(
    input CLK,
    input RST_N,
    input [WIDTH-1:0] PUT,
    input EN_PUT,
    input [WIDTH-1:0] EXCHANGE,
    input EN_EXCHANGE,
    input [WIDTH-1:0] TRANSLATE,
    output [WIDTH-1:0] READ,
    output [WIDTH-1:0] SNAPSHOT,
    output [WIDTH-1:0] EXCHANGE_RESULT,
    output [WIDTH-1:0] TRANSLATE_RESULT,
    output reg READY
);
   reg [WIDTH-1:0] stored;

   assign READ = stored;
   assign SNAPSHOT = stored;
   assign EXCHANGE_RESULT = stored;
   assign TRANSLATE_RESULT = TRANSLATE ^ 16'hf00f;

   always @(posedge CLK) begin
      if (!RST_N) begin
         stored <= 16'h1234;
         READY <= 1'b0;
      end else begin
         READY <= !READY;
         if (EN_PUT)
            stored <= PUT;
         if (EN_EXCHANGE)
            stored <= EXCHANGE;
      end
   end
endmodule
