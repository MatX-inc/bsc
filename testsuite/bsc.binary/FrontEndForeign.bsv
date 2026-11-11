package FrontEndForeign;

// One foreign function imported twice: the full compile reports it after
// typecheck (see frontend.exp)

import "BDPI" add32 = function Bit#(32) and32 (Bit#(32) v1, Bit#(32) v2);

import "BDPI" add32 = function Bit#(30) and31 (Bit#(30) v1, Bit#(30) v2);

endpackage
