# A discovered script with no assertions still belongs in the plan.
foreach source {} {
    compile_pass $source
}
