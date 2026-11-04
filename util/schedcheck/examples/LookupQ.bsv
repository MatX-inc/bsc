import FIFO::*;
import RegFile::*;

interface LookupQ;
   method Action request(Bit#(2) key);
   method ActionValue#(Bit#(8)) response();
endinterface

// Showcase: a credit-controlled lookup unit.
//  * request (needs a credit) hands its key to lookup through an RWire,
//    so the table read and the enqueue happen in the same cycle
//  * response dequeues a result and returns its credit through a PulseWire
//  * account is the single writer of credits: -1 on request, +1 on response
//  * fill rewrites table entries; RegFile sub SB upd orders lookup before it
// Children: tbl (RegFile), respQ (FIFO2), credits/fillIdx (RegN),
// reqW (RWire), retW (RWire0).  Expected execution order: request and
// response before lookup and account, lookup before fill; no conflicts.
// With 2 credits and a depth-2 FIFO, lookup is never blocked when request
// fires (credits + FIFO occupancy == 2), but bsc does not know that.
//
// Checked (bsc 2026.07-6): order request, response, lookup, account, fill;
// request C request, response C response, request CF response; no blocking.
//
// Compile: bsc -u -verilog -elab -show-schedule LookupQ.bsv
(* synthesize *)
module mkLookupQ(LookupQ);
   RegFile#(Bit#(2), Bit#(8)) tbl <- mkRegFileFull;
   FIFO#(Bit#(8))  respQ   <- mkFIFO;
   Reg#(UInt#(2))  credits <- mkReg(2);
   Reg#(Bit#(2))   fillIdx <- mkReg(0);
   RWire#(Bit#(2)) reqW    <- mkRWire;
   PulseWire       retW    <- mkPulseWire;

   rule lookup (reqW.wget matches tagged Valid .k);
      respQ.enq(tbl.sub(k));
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule account;
      UInt#(2) c = credits;
      if (isValid(reqW.wget)) c = c - 1;
      if (retW) c = c + 1;
      credits <= c;
   endrule

   rule fill (fillIdx != 3);
      tbl.upd(fillIdx, zeroExtend(fillIdx) * 8'h11);
      fillIdx <= fillIdx + 1;
   endrule

   method Action request(Bit#(2) key) if (credits != 0);
      reqW.wset(key);
   endmethod

   method ActionValue#(Bit#(8)) response();
      respQ.deq;
      retW.send;
      return respQ.first;
   endmethod
endmodule
