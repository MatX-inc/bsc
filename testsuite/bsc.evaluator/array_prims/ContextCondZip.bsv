import Vector::*;

// Regression test: primArrayZipWith inside a conditioned context.
// See ContextCondMap.bsv for the full explanation.
//
// zipWith on two vectors inside a guarded rule must not promote the
// contextual condition to a container condition on the output array.

(* synthesize *)
module sysContextCondZip(Empty);

  Vector#(4, Reg#(UInt#(8))) as <- replicateM(mkReg(0));
  Vector#(4, Reg#(UInt#(8))) bs <- replicateM(mkReg(0));
  Reg#(Bool) ready <- mkReg(False);
  Reg#(UInt#(8)) out <- mkRegU;

  rule use_zip (ready);
    Vector#(4, UInt#(8)) va = readVReg(as);
    Vector#(4, UInt#(8)) vb = readVReg(bs);
    Vector#(4, UInt#(8)) zipped = zipWith(\+ , va, vb);
    out <= fold(\+ , zipped);
  endrule

endmodule
