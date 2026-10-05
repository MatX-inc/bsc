# Run alongside an existing design's normal compile/link sequence.
# Python only snapshots files and checks JSON; compiler invocation stays here.
set dependency_report_checker [file join [file dirname [file normalize [info script]]] check_dependencies.py]
proc check_dependencies {test_directory label arguments expectations {trees {}}} {
    global bsc dependency_report_checker
    if {[auto_execok python3] eq ""} {
        unsupported "dependencies: $label (python3 unavailable)"
        return
    }
    bsc_initialize
    set here [pwd]
    set checker $dependency_report_checker
    cd $test_directory
    set prefix ".dependencies-$label"
    set tree_options [list]
    foreach tree $trees {
        lappend tree_options --tree $tree
    }
    set status [catch {
        exec python3 $checker snapshot $prefix {*}$tree_options
        verbose -log "Dependency query: $bsc $arguments" 2
        exec $bsc -dependencies "$prefix.json" {*}$arguments \
            > "$prefix.stdout" 2> "$prefix.stderr"
        exec python3 $checker check $prefix {*}$expectations {*}$tree_options
    } output]
    if {$status != 0} {
        verbose -log $output 2
        foreach suffix {stdout stderr} {
            if {[file exists "$prefix.$suffix"]} {
                set channel [open "$prefix.$suffix" r]
                verbose -log [read $channel] 2
                close $channel
            }
        }
    }
    cd $here
    if {$status == 0} {
        pass "dependencies: $label"
    } else {
        fail "dependencies: $label"
    }
}
