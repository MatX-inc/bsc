package StaleT;

typedef Bit#(16) S;
typedef struct { S a; Bit#(4) b; } T deriving (Bits, Eq);

endpackage
