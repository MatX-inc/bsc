typedef enum { A, B, C } Letter deriving (Bits, Eq);

typedef struct {
   Bool a;
   Bit#(4) b;
} S deriving (Bits, Eq);

// missing enum tag C
function Bit#(4) missingC(Letter l);
   return (case (l) matches
              A: 0;
              B: 1;
           endcase);
endfunction

// literals of Bit#(2) with values 2 and 3 unmatched
function Bit#(4) missingLits(Bit#(2) x);
   return (case (x) matches
              0: 5;
              1: 6;
           endcase);
endfunction

// struct with field a = False unmatched
function Bit#(4) missingField(S s);
   return (case (s) matches
              S { a: True, b: .bb }: bb;
           endcase);
endfunction

// a guarded arm does not count towards completeness
function Bit#(4) guarded(Letter l);
   return (case (l) matches
              A: 0;
              B: 1;
              C &&& (l == C): 2;
           endcase);
endfunction
