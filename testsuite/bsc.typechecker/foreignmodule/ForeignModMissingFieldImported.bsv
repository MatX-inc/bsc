import ForeignModMissingFieldImported_Ifc::*;

import "BVI" VMod =
  module mkMod (IFC);
     default_clock clk();
     default_reset rst();
     method m1() enable(EN_m1);
     schedule m1 C m1;
  endmodule
