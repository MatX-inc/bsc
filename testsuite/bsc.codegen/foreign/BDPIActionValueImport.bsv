package BDPIActionValueImport;

// Imported functions for the peek tests: a call of a function imported
// from another package is reported at the call
import "BDPI" function ActionValue#(Bit#(32)) my_count (Bit#(8) step);
import "BDPI" function ActionValue#(Bool) my_flag (Bit#(8) x);

export my_count;
export my_flag;

endpackage
