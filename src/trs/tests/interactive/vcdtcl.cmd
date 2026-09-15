# VCD-under-Tcl witness: bk_set_VCD_file / bk_enable_VCD_dumping /
# bk_disable_VCD_dumping via `sim vcd`.  The harness compares stdout
# AND the VCD bytes (modulo the $date line) against the reference.

# no active dump yet
puts [sim vcd]

# dump to a named file, step
sim vcd waves.vcd
sim step 10
puts [sim vcd]

# disable (deferred Xs section), step
sim vcd off
sim step 5

# re-enable onto the same file ($dumpon-style restart), step
sim vcd on
sim step 5
puts [sim vcd]

# end of script
