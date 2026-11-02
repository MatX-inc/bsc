package Dup;

// two foreign imports of the same link name: T0091 under a full compile
// and under -check-only alike

import "BDPI" add32 = function Bit#(32) and32 (Bit#(32) v1, Bit#(32) v2);

import "BDPI" add32 = function Bit#(30) and31 (Bit#(30) v1, Bit#(30) v2);

endpackage
