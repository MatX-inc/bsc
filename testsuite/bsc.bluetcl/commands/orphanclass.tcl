namespace import ::Bluetcl::*

# "type full" shows "orphan", after any coherence word, for a class
# declared orphan, and nothing for a class that is not

puts [bpackage load OrphanClassTest]
puts ""

puts [type full {Orphan#(a)}]
puts ""
puts [type full {CoherentOrphan#(a)}]
puts ""
puts [type full {IncoherentOrphan#(a)}]
puts ""
puts [type full {NotOrphan#(a)}]
puts ""
