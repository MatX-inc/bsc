set source Example.bs
foreach flags {{} {-v} {-v}} {
    compile_pass $source $flags
}
