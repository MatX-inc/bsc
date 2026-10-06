#!/usr/bin/env tclsh
# Reuse the real fixture without loading DejaGNU or running unrelated tests.
# The compiler must already have been built through the repository root.
if {$argc != 2} {
    puts stderr "usage: run_distribution.tcl BSC DISTRIBUTION"
    exit 2
}
set bsc [file normalize [lindex $argv 0]]
set bsdir [file normalize [lindex $argv 1]]
if {![file executable $bsc] || ![file isdirectory $bsdir]} {
    puts stderr "compiler executable or distribution directory is unavailable"
    exit 2
}
set successes 0
set failures 0
set unavailable 0
proc bsc_initialize {} {}
proc pass {label} { incr ::successes; puts "PASS: $label" }
proc fail {label} { incr ::failures; puts "FAIL: $label" }
proc unsupported {label} { incr ::unavailable; puts "UNSUPPORTED: $label" }
proc verbose {args} { puts [join $args " "] }
set fixture [file join [file dirname [file normalize [info script]]] distribution distribution.exp]
if {[catch {source $fixture} message options]} {
    puts stderr $message
    if {[dict exists $options -errorinfo]} {
        puts stderr [dict get $options -errorinfo]
    }
    incr failures
}
puts "Distribution regression: $successes passed, $failures failed, $unavailable unavailable"
if {$failures != 0 || $unavailable != 0 || $successes == 0} { exit 1 }
