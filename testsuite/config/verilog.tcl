# -------------------------

# create a simulation executable from verilog
proc link_verilog_pass { objects toplevel {options ""}} {
    global vtest

    if {$vtest == 1} {
      incr_stat "link_verilog_pass"

      if [bsc_link_verilog $objects $toplevel $options] then {
          pass "`$objects' link to executable `$toplevel'"
      } else {
          fail "`$objects' should link to executable `$toplevel'"
      }
    }
}

# create a simulation executable from verilog (expected to fail)
proc link_verilog_pass_bug { objects toplevel {bug ""} {options ""}} {
    global target_triplet
    global vtest

    if {$vtest == 1} {
      setup_xfail $target_triplet $bug
      link_verilog_pass $objects $toplevel $options
    }
}

# create a simulation executable from verilog
proc link_verilog_fail { objects toplevel {options ""}} {
    global vtest

    if {$vtest == 1} {
      incr_stat "link_verilog_fail"

      if [bsc_link_verilog $objects $toplevel $options] then {
          fail "`$objects' should not link to executable `$toplevel'"
      } else {
          pass "`$objects' do not link to executable `$toplevel'"
      }
    }
}

# returns true iff linking succeeded
# linking here is the generation of an simulation executable.
proc bsc_link_verilog { objects toplevel { options "" } } {
    global bsc
    global srcdir
    global subdir
    global verilog_compiler
    global vcomp_flags

    set here [absolute $srcdir]
    cd [file join $here $subdir]
    set output [make_bsc_vcomp_output_name $toplevel]
    set vexename [make_vexe_name $toplevel]
    set link_options "-verilog -vsim $verilog_compiler -e $toplevel -o $vexename [dpi_flag] $vcomp_flags"
    set cmd "$bsc $link_options $options $objects >& $output"
    verbose "Executing: $cmd" 4
    set status [exec_with_log "def_link_verilog" $cmd 2]
    cd $here
    return [expr $status == 0]
}

# -------------------------

# Versions of the above that don't include "main.v"

proc link_verilog_no_main_pass { objects toplevel { options "" } } {
    global vtest
    global verilog_compiler
    global target_triplet

    if {$vtest == 1} {
      incr_stat "link_verilog_no_main_pass"

      # Linking without BSC's main.v means the top module drives its own clock
      # internally, which requires Verilator --timing (locked by default; see
      # bsc_build_vsim_verilator).  So under Verilator this link is expected to
      # fail unless the BSC_VERILATOR_ENABLE_TIMING back door is set.
      if { [verilator_no_timing] } {
          setup_xfail $target_triplet "verilator-no-main"
      }

      if [link_verilog_no_main $objects $toplevel $options] then {
          pass "`$objects' link to executable `$toplevel'"
      } else {
          fail "`$objects' should link to executable `$toplevel'"
      }
    }
}

# True when the Verilog simulator is Verilator and the --timing back door is
# NOT enabled -- i.e. designs that require --timing (clock/reset generators,
# or no-main self-driving tops) are expected to fail.
proc verilator_no_timing {} {
    global verilog_compiler
    return [expr {$verilog_compiler eq "verilator" \
                  && ![info exists ::env(BSC_VERILATOR_ENABLE_TIMING)]}]
}

# True when we are under Verilator (no back door) AND at least one of the given
# generated Verilog files carries the BSC_NEEDS_TIMING marker -- i.e. the design
# will be locked by bsc_build_vsim_verilator.  Tests that drive the Verilog flow
# by hand (raw link_verilog_pass / sim_verilog / compare_file) use this to XFAIL
# the link and skip the run/compare steps, the way test_c_veri_worker_int does
# automatically.  The files are looked up in the test's source directory.
proc verilator_timing_locked { vfiles } {
    global srcdir
    global subdir
    if { ! [verilator_no_timing] } { return 0 }
    foreach f $vfiles {
        if { [file_contains [file join [absolute $srcdir] $subdir $f] \
                            "BSC_NEEDS_TIMING"] } {
            return 1
        }
    }
    return 0
}

# XXX Replace with 'bsc_link_verilog' when BSC supports a flag like '-no-include-main'
proc link_verilog_no_main { objects toplevel { options "" } } {
    global srcdir
    global subdir
    global bsdir
    global verilog_compiler
    global vcomp_flags

    set here [absolute $srcdir]
    cd [file join $here $subdir]

    set output [make_bsc_vcomp_output_name $toplevel]
    set vexename [make_vexe_name $toplevel]

    set bsvlib_dir [file join $bsdir Libraries]
    set vlib_dir   [file join $bsdir Verilog]

    set exec_name [file join $bsdir "exec" "bsc_build_vsim_$verilog_compiler"]
    set exec_options "link $vexename $toplevel -y . -y $bsvlib_dir -y $vlib_dir"
    # XXX vcomp_flags? options?
    set cmd "$exec_name $exec_options $objects >& $output"

    verbose "Executing: $cmd" 4
    set status [exec_with_log "link_verilog_no_main" "$cmd" 2]

    cd $here
    return [expr $status == 0]
}

# -------------------------

proc compare_verilog { filename {expected ""} } {
    global vtest
    if {$vtest == 1} {
        incr_stat "compare_verilog"
        compare_file_filter_ids $filename $expected "-e \"/Bluespec Compiler/d\""
    }

}

proc compare_verilog_bug { filename { bug "" } { expected "" }} {
    global target_triplet
    global vtest

    if {$vtest == 1} {
      setup_xfail $target_triplet $bug
      compare_verilog $filename $expected
    }
}

# -------------------------

# Run a Verilog simulation (without VCD dumping)
proc sim_verilog { sim {options ""} } {
    global vtest
    global lasterr

    if { $vtest == 1 } {
        set status [sim_verilog_int $sim $options 0]
        incr_stat "sim_verilog"
        if { $status == 0 } then {
            pass "Verilog simulation `$sim' executes"
        } else {
            fail "Verilog simulation `$sim' should execute: $lasterr"
        }
    }
}

# Run a Verilog simulation (with VCD dumping)
proc sim_verilog_vcd { sim {options ""} } {
    global vtest
    global lasterr

    if { $vtest == 1 } {
        set status [sim_verilog_int $sim $options 1]
        incr_stat "sim_verilog_vcd"
        if { $status == 0 } then {
            pass "Verilog simulation `$sim' executes with VCD dump"
        } else {
            fail "Verilog simulation `$sim' should execute with VCD dump: $lasterr"
        }
    }
}

proc sim_verilog_status { sim expstatus {options ""} } {
    global vtest
    global lasterr

    if { $vtest == 1 } {
        set status [sim_verilog_int $sim $options 0]
        incr_stat "sim_verilog_status"

        set status [get_exec_status $status]

        if { [lsearch -exact $expstatus $status] != -1 } then {
            pass "Verilog simulation `$sim' exits with expected status"
        } else {
            fail "Verilog simulation `$sim' exits with status $status (expected $expstatus)"
        }
    }
}

## This is where the compiled simulator is executed.
proc sim_verilog_int { sim {options ""} {vcd 0} } {
    global srcdir
    global subdir
    global subdir
    global vrun_novcd_flags
    global vrun_vcd_flags
    global verilog_compiler
    global lasterr

    set output "$sim.out"

    set here [absolute $srcdir]
    cd [file join $here $subdir]

    set sim_execN [file join $here $subdir $sim]
    set sim_exec [make_vexe_name $sim_execN]
    if { $vcd == 0 } {
        set sim_name sim_verilog
        set sim_flags $vrun_novcd_flags
    } elseif { $vcd == 1 } {
        set sim_name sim_verilog_vcd
        set sim_flags $vrun_vcd_flags
    } else {
        perror "sim_verilog_int called with invalid vcd argument: $vcd"
    }
    set status [exec_with_log_and_return $sim_name "$sim_exec $sim_flags $options >& $output" lasterr 2]

    # Pop back up, because
    # the commands after this point will also cd to the subdir
    cd $here

    # Move the raw output aside
    # so that we can put the filtered output in its place
    set rawfile "$output.raw"
    copy $output $rawfile

    if { [string match $verilog_compiler "ncverilog"] } then {
        clean_ncverilog_output $rawfile $output
    } elseif { [string match $verilog_compiler "vcsi"] } then {
	clean_vcsi_output $rawfile $output
    } elseif { [string match $verilog_compiler "modelsim"] } then {
	clean_modelsim_output $rawfile $output
    } elseif { [string match $verilog_compiler "questa"] } then {
	clean_modelsim_output $rawfile $output
    } elseif { [string match $verilog_compiler "iverilog"] } then {
	clean_iverilog_output $rawfile $output
    } elseif { [string match $verilog_compiler "cvc"] } then {
	clean_cvc_output $rawfile $output
    } elseif { [string match $verilog_compiler "verilator"] } then {
	clean_verilator_output $rawfile $output
    }

    # Remove the raw output
    erase $rawfile

    # Return the simulation status
    return $status
}

## Cleans the simulation output
proc clean_ncverilog_output { infile outfile } {

    set ncsim_script {-e {/^ncsim/d}}
    set finish_script {-e {/\$finish/d}}

    sed $infile $outfile {} "$ncsim_script $finish_script"
}

## Cleans the simulation output
proc clean_vcsi_output { infile outfile } {

    set hdr_script {-e {1,4d}}
    set vcs_script {-e {/V C S   S i m/,$d}}

    sed $infile $outfile $hdr_script $vcs_script
}

## Cleans the simulation output
proc clean_modelsim_output { infile outfile } {

    set hdr_script {-e {1,/^\# run -all/d}}
    set end_script {-e {/^\# End time\: /,$d}}
    set prefix_script {-e {s/^\# //g}}

    sed $infile $outfile {} "$hdr_script $end_script $prefix_script"
}

## Cleans the simulation output
proc clean_iverilog_output { infile outfile } {

    # Older iverilog versions don't display $readmem warnings.
    # As long as they are in use, we filter the warnings out of all
    # iverilog output.
    # Iverilog 0.9.x emits VCD info messages that we filter out
    # The format of readmem warnings has changed, too

    set readmem_script {-e {/^\$readmem/d}}
    set readmem_script2 {-e {/^WARNING:.*\$readmem/d}}
    set finish_script {-e {/\$finish/d}}
    set vcd_script {-e {/^VCD info/d}}

    sed $infile $outfile {} "$readmem_script $readmem_script2 $finish_script $vcd_script"
}

## Cleans the simulation output
proc clean_cvc_output { infile outfile } {

    ## Delete first three lines (Copyright banner)
    ## Delete lines with $finish
    ## Delete lines with "inform(s)"

    set hdr_script {-e {1,3d}}
    set finish_script {-e {/\$finish/d}}
    set inform_script {-e {/inform\(s\)\./d}}

    sed $infile $outfile $hdr_script "$finish_script $inform_script"
}

## Cleans the simulation output
proc clean_verilator_output { infile outfile } {

    # Don't include the finish message, but also don't include
    # any lines after that, since Verilator will execute actions
    # after the finish (such as $display in the same block)
    set finish_script {-e {/\$finish/,$d}}

    # Verilator prints runtime warnings on stdout that iverilog does not, and
    # that no Verilator flag disables.  Match only the invariant substring: the
    # exact wording and IEEE citation vary by Verilator version (e.g.
    # "proceeded"/"preceded", "ending-address"/"final address", IEEE 2017/2023),
    # so these patterns work across 4.x and 5.x.
    set dumpvar_script {-e {/\$dumpvar ignored/d}}
    set readmem_script {-e {/\$readmem file ended/d}}

    sed $infile $outfile {} "$dumpvar_script $readmem_script $finish_script"
}

# -------------------------

proc get_verilog_compiler_version {} {
    global verilog_compiler
    global verilog_compiler_version

    if { [string match $verilog_compiler "iverilog"] } then {
	if { [catch "exec $verilog_compiler -V 2>/dev/null | head -1" line1] } {
	    # If there's an error, use an empty string
	    set verilog_compiler_version {}
	} elseif { ! [regexp -- {^Icarus Verilog version ([^ ]+).*$} $line1 tmp verilog_compiler_version] } {
	    # If we can't parse the first line, use an empty string
	    set verilog_compiler_version {}
	}

    } else {
	# Until we need to distinguish other Verilog tool versions,
	# use an empty string
	set verilog_compiler_version {}
    }

}

# -------------------------

proc isPositiveReset {} {
    set opts $::env(BSC_OPTIONS)
    regexp "BSV_POSITIVE_RESET" $opts
}

# -------------------------

# True when foreign functions are compiled to DPI-C rather than VPI (the
# use_dpi flag, derived from the simulator in unix.exp).  DPI and VPI generate
# different wrappers (DPI at link time, with no VPI-style startup array), so
# VPI-specific tests key off this.
proc useDPI {} {
    global use_dpi
    return $use_dpi
}

# The bsc option for compiling/linking Verilog with foreign functions as DPI-C
# ("-use-dpi"), or "" -- see the use_dpi flag.  Applied only at the Verilog
# compile/link points (not globally via BSC_OPTIONS), so it never leaks into
# flag/error-dump output and is never given to iverilog or the C backend.
proc dpi_flag {} {
    global use_dpi
    return [expr {$use_dpi ? "-use-dpi" : ""}]
}

# -------------------------
