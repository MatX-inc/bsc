# Counter/CReg oracle witness: stepping stops make the state compare
# read both prims on both engines (state_children surface — the bk
# tree stays reference-empty for them).
sim step 5
puts [sim time]
sim step 10
puts [sim time]
sim run
