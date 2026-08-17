// Exercise the SVAProp combinator library from the BSV frontend.
// Each mkSvaAssert* instance should emit a labeled concurrent
// `assert property` in the generated Verilog.

import SVAProp::*;

(* synthesize *)
module sysSvaPropTest(Empty);

  Reg#(Bool) req  <- mkReg(False);
  Reg#(Bool) ack  <- mkReg(False);
  Reg#(Bool) busy <- mkReg(False);
  Reg#(Bool) done <- mkReg(False);

  // basic overlapped implication with a leading delay: req |-> ##1 ack
  Empty a0 <- mkSvaAssert("basicImpl",
      pImplies(sExpr(req), pSeq(sNext(1, sExpr(ack)))));

  // non-overlapped implication, delay range and unbounded delay range
  Empty a1 <- mkSvaAssert("delayRanges",
      pImpliesNext(sExpr(req),
        pSeq(sOr(sDelayRange(2, 5, sExpr(ack), sExpr(busy)),
                 sDelayUnbound(1, sExpr(busy), sExpr(done))))));

  // all repetition forms
  Empty a2 <- mkSvaAssert("repetitions",
      pSeq(sConcat(sRepeat(2, sExpr(req)),
           sConcat(sRepeatRange(2, 3, sExpr(busy)),
           sConcat(sRepeatUnbound(1, sExpr(ack)),
           sConcat(sRepeatNonconsec(2, done),
           sConcat(sRepeatNonconsecRange(1, 3, ack),
           sConcat(sRepeatGoto(2, busy),
                   sRepeatGotoRange(1, 4, req)))))))));

  // sequence algebra: and, intersect, fusion, first_match
  Empty a3 <- mkSvaAssert("seqAlgebra",
      pSeq(sFirstMatch(sIntersect(sAnd(sExpr(req), sExpr(ack)),
                                  sFuse(sExpr(busy), sExpr(done))))));

  // sampled-value functions and property algebra
  Empty a4 <- mkSvaAssert("sampledOps",
      pAnd(pNot(pSeq(sRose(req))),
           pOr(pSeq(sFell(ack)), pSeq(sStable(busy)))));

  // disable iff guard plus pass/fail actions
  Empty a5 <- mkSvaAssertDisableAction("withDisable", busy,
      pImplies(sExpr(req), pSeq(sExpr(ack))),
      $display("PASS withDisable"), $display("FAIL withDisable"));

  rule drive;
    req  <= !req;
    ack  <= req;
    busy <= !busy;
    done <= busy;
  endrule

endmodule
