// The EXT scheduling annotation is not supported

interface Ifc;
   method Action c;
endinterface

import "BVI" VMod =
module mkMod (Ifc);
   method c() enable(EN_C);
   schedule EXT (c);
endmodule
