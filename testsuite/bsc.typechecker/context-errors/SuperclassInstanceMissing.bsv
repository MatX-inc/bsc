import SuperclassInstanceMissing_Classes::*;

typedef enum { A, B } T deriving (Bits, Eq);

// There is no instance of the superclass Super for T
instance Sub#(T);
   function Bool subMeth(T x) = True;
endinstance
