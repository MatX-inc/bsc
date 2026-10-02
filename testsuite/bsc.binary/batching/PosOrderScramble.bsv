package PosOrderScramble;

// Parsed before PosOrder in the batch: builds the same ground types from
// this file's positions first.
import Vector::*;

typedef Vector#(4, Bit#(8)) ScrambleRow;
typedef Maybe#(Bit#(8)) ScrambleMaybe;
typedef Tuple2#(Bool, Bit#(8)) ScramblePair;

endpackage
