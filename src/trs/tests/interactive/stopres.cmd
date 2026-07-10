# run hits the $stop at cyc==3; the session must be resumable
sim run
puts "after-stop time [sim time]"
set h [sim lookup cyc]
puts "cyc [sim get $h]"
# resume: run to the $finish
sim run
puts "after-finish time [sim time]"
puts "cyc [sim get $h]"
# stepping past $finish must error
if {[catch {sim step} e]} { puts "step-err: $e" }
