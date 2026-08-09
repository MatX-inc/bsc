# step past the fifo fill point: guard warnings must appear exactly
# once per cycle (the quiet secondary suppresses its copies)
sim step 6
puts [sim time]
sim run
