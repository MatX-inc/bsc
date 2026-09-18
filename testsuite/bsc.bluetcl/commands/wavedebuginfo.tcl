namespace import ::Bluetcl::*

# The debug information for a design's waveform dumps: each source
# entity's path in the dump and in the source, its kind and type, and
# the bit layout of every type those signals name.  Written as JSON.

puts [flags set {-sim}]
puts [module load mkWaveTypesCore]
puts [module wavedebuginfo mkWaveTypesCore wavedebuginfo.json]

set fh [open wavedebuginfo.json r]
puts -nonewline [read $fh]
close $fh
