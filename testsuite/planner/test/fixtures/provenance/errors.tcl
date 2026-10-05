compile_fail_error Fail.bs T0001
compile_pass Good.bs
compile_fail_error FailTwo.bs T0001 2
compile_fail FailPlain.bs
proc unsupported_error_wrapper {} { compile_fail_error Fail.bs T0001 }
unsupported_error_wrapper
compile_fail_error Fail.bs T9999
compile_fail_error FailTwo.bs T0001 1
compile_fail_error Good.bs T0001
compile_fail_error FailZero.bs T0001 0
