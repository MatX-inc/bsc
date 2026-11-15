namespace import ::Bluetcl::*

# One process, one package name, two definitions.  The first load sees the
# struct with one field; the file is then replaced by the build with two
# fields, the package is cleared and loaded again, and the second query
# must describe the new definition.  Any table in the compiler that keeps
# a type's payload under its name alone answers the second query with the
# first definition.
puts [bpackage load StaleT]
puts [type full StaleT::S]
puts [type full StaleT::T]

file copy -force v2/StaleT.bo StaleT.bo

puts [bpackage clear]
puts [bpackage load StaleT]
puts [type full StaleT::S]
puts [type full StaleT::T]
