import "BDPI" mkSameName = function Bit#(32) foreign_add(Bit#(32) value);

(* synthesize *)
module mkSameName(Empty);
    rule report;
        $display("%0d", foreign_add(35));
        $finish(0);
    endrule
endmodule
