# $finish edge-completion witness: run to $finish, then peek the
# registers written by rules scheduled AFTER the $finish rule on the
# finish edge.  The reference completes the in-flight edge schedule:
# mark = 1000042, cyc = 1000001.  A mid-edge abort leaves mark = 0.

set h_cyc  [sim lookup cyc]
set h_mark [sim lookup mark]

# run to $finish
sim run

puts "time [sim time]"
puts "cyc  [sim get $h_cyc]"
puts "mark [sim get $h_mark]"

# end of script
