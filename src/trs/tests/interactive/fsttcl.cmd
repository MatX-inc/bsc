# FST-under-Tcl witness: `sim fst` drives bk_set_waveform_format +
# the shared VCD/FST engine.  The harness compares stdout AND the
# FST files semantically (fst2vcd + fstcmp.py — FST bytes embed
# timestamps).  Mirrors vcdtcl.cmd's shape.

# no active dump yet
puts [sim fst]

# dump to a named file, step
sim fst waves.fst
sim step 10
puts [sim fst]

# disable (blackout region in FST), step
sim fst off
sim step 5

# re-enable onto the same file, step
sim fst on
sim step 5
puts [sim fst]

# end of script
