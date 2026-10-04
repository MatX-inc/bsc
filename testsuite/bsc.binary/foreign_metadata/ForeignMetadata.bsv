import "BDPI" function Bit#(32) metadata_add(Bit#(32) value);

(* synthesize *)
module mkForeignMetadata(Empty);
    rule report;
        $display("%0d", metadata_add(35));
        $finish(0);
    endrule
endmodule
