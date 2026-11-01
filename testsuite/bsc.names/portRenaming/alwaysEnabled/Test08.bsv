import FIFO :: *;
(* always_enabled *)
interface IFC#(type mType);
 method Action start(mType a, mType b);
 method mType result(mType c);
 method ActionValue#(mType) check(mType d);
endinterface


typedef Bit#(5) Type;

(* synthesize *)
module mkDesign_08 (IFC#(Type));

  FIFO#(Type) fifo1 <- mkFIFO;
  FIFO#(Type) fifo2 <- mkFIFO;


  method Action start(a,b);
    fifo1.enq(a);
    fifo2.enq(b);
  endmethod

  method Type result(c);
    return fifo1.first+c;
  endmethod

  // dequeue (start enqueues), so the always_enabled methods do not conflict
  method ActionValue#(Type) check(d);
    fifo1.deq;
    fifo2.deq;
    return fifo2.first+d;
  endmethod
endmodule
