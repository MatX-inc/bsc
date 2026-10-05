compile_fail_error Bad.bs T0001
compile_fail_error Zero.bs T0002 0
compile_fail_error NoDeps.bs S0001 2 {-v -dinternal} 1
compile_fail_error Regex.bs {T[0-9]+}
compile_pass After.bs
