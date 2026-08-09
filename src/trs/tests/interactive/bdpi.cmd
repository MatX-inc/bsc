# BDPI-under-Tcl: step, peek the BDPI-fed registers, run to $finish
set h_acc [sim lookup acc]
set h_wac [sim lookup wac]
sim step 5
puts "acc [sim get $h_acc]"
puts "wac [sim get $h_wac]"
sim run
puts "time [sim time]"
puts "acc [sim get $h_acc]"
puts "wac [sim get $h_wac]"
