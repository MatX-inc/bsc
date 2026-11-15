// an imported function whose name is not that of the C function
import "BDPI" c_time = function ActionValue_#(Bit#(32)) bsv_time (Bit#(8) x);

(* synthesize *)
module mkBDPIActionValue_Link ();
   rule r;
      Bit#(32) a <- fromActionValue_(bsv_time(0));
      $display("%d", a);
   endrule
endmodule
