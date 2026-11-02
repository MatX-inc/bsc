
import "BVI" VMod =
module mkMod (Reg#(Bool));

   method _write (D_IN) enable (EN);
   method Q_OUT _read;

   // one statement which says both _read SBR _write and _write SBR _read
   schedule (_read, _write) SBR (_read, _write);
endmodule

