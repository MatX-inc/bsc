import Vector::*;

// Regression test: array primitives inside a conditioned context.
//
// When primArrayMap (or primArrayZipWith) is evaluated inside a guarded
// method or rule, the evaluator picks up the guard predicate as part of
// evaluating the input array.  The primitive must push this contextual
// condition into element-level conditions on the result, NOT promote it
// to a container condition on the output array.
//
// If the condition is incorrectly made a container condition, then
// arrayLength on the result (or on an element, if the result is an
// array of arrays) will return a conditioned Integer, which cannot be
// used in compile-time comparisons — causing a G0013 error like:
//   PrimIntegerLT N (PrimWhenPred Integer _ M)
//
// This test exercises the pattern from PAClib's mkUnfunnel/concat:
//   - A guarded rule reads registers into a vector of vectors
//   - concat calls Array.map (now primArrayMap) then arrayLength
//   - arrayLength must see a clean Integer, not a conditioned one

function UInt#(8) addOne(UInt#(8) x);
  return x + 1;
endfunction

(* synthesize *)
module sysContextCondMap(Empty);

  Vector#(4, Reg#(UInt#(8))) vals <- replicateM(mkReg(0));
  Reg#(Bool) ready <- mkReg(False);
  Reg#(UInt#(8)) out <- mkRegU;

  // map inside a guarded rule: primArrayMap must not promote the
  // guard condition to a container condition on the result array
  rule use_map (ready);
    Vector#(4, UInt#(8)) v = readVReg(vals);
    Vector#(4, UInt#(8)) mapped = map(addOne, v);
    out <= fold(\+ , mapped);
  endrule

  // concat inside a guarded rule: the inner arrays' lengths must
  // be clean Integers even though the context is conditioned
  rule use_concat (ready);
    Vector#(4, UInt#(8)) v = readVReg(vals);
    Vector#(2, Vector#(2, UInt#(8))) vv;
    vv[0] = take(v);
    vv[1] = takeTail(v);
    Vector#(4, UInt#(8)) flat = concat(vv);
    out <= fold(\+ , flat);
  endrule

endmodule
