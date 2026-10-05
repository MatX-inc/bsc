# Inert fixture: the Python marker test sources this through real runtest.
foreach flag {-first -second} {
    compile_pass Same.bs $flag
}
compile_pass {odd {name} with a tab	.bs} {-Dname={value with spaces}}
proc unsupported_wrapper {name} { compile_pass $name }
unsupported_wrapper Wrapped.bs
compile_backend_pass Backend.bs
source [file join [file dirname [info script]] sourced.tcl]
set xfail_flag 1
set xfail_prms 123
compile_pass Fail.bs
set xfail_flag 1
set xfail_prms 456
compile_pass UnexpectedPass.bs
compile_fail Fail.bs
compile_fail UnexpectedCompile.bs
set errcnt 1
compile_pass Threshold.bs
