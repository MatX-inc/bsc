package Both;

// a dead import and a duplicate link name: both modes report T0091 alone,
// because the duplicate check precedes the unused-import warning on both

import A :: *;

import "BDPI" add32 = function Bit#(32) and32 (Bit#(32) v1, Bit#(32) v2);

import "BDPI" add32 = function Bit#(30) and31 (Bit#(30) v1, Bit#(30) v2);

endpackage
