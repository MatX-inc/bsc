// BDPI imports have no BH syntax. Keep this shim separate from the BH hardware
// fixtures so the replay tests still exercise a real native .ba dependency.
package ReplayForeign;
import "BDPI" function Bit#(8) replayForeign(Bit#(8) value);
endpackage
