-- Policy-neutral public module contract, including ordered ports and parameters.
module ECO.Interface (fixedPorts) where
import Verilog

fixedPorts :: VProgram -> String
fixedPorts (VProgram modules _ _) = show [(vm_name m,vm_ports m) | m <- modules] ++ "\n"
