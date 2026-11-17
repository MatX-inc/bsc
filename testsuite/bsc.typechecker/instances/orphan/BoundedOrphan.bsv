import classes::*;

instance Bounded#(a) provisos(Bits#(a,sa));
  function minBound = unpack(Prelude::minBound);
  function maxBound = unpack(Prelude::maxBound);
endinstance
