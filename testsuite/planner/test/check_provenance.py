#!/usr/bin/env python3
"""Check direct BSC-TEST log markers using real DejaGnu verdict/runtest code.

Compiler/object-reader calls are stubs. No compiler build or suite run occurs.
Fixtures use .tcl so normal DejaGnu discovery never executes them.
"""

import base64
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


PLANNER = Path(__file__).resolve().parents[1]
SUITE = PLANNER.parent
FIXTURES = Path(__file__).resolve().parent / "fixtures" / "provenance"
DEJAGNU = Path(os.environ.get("DEJAGNU_LIBRARY", "/usr/share/dejagnu"))

DRIVER = r'''
# Load real verdict handling and selected real harness procedures without
# running unrelated compiler/tool discovery from their top-level setup.
source [file join $env(PROVENANCE_DEJAGNU) framework.exp]
proc load_procedures {path names} {
    set input [open $path r]
    set command ""
    while {[gets $input line] >= 0} {
        append command $line "\n"
        if {![info complete $command]} { continue }
        set command [string trim $command]
        if {[string match "proc *" $command] &&
            [lindex $command 1] in $names} {
            uplevel #0 $command
        }
        set command ""
    }
    close $input
}
load_procedures [file join $env(PROVENANCE_DEJAGNU) runtest.exp] {runtest}
load_procedures [file join $env(PROVENANCE_SUITE) config unix.exp] {
    compile_pass compile_fail compile_backend_pass dumpbo_pass dumpba_pass
    compile_fail_error make_bsc_output_name find_n_error find_n_emsg
    check_intermediate_files do_internal_checks bsc_test_trace_enabled
    bsc_test_trace_log bsc_test_trace_path bsc_test_init bsc_test_finish
    bsc_test_begin bsc_test_role bsc_test_end bsc_init bsc_finish _init _finish
}
set log [open [file join $env(PROVENANCE_ROOT) testrun.log] w]
proc verbose {args} { puts $::log [lindex $args end-1] }
proc send_log {args} { puts -nonewline $::log [lindex $args end] }
proc send_user {args} { puts -nonewline $::log [lindex $args end] }
proc send_error {args} { puts -nonewline $::log [lindex $args end] }
proc timestamp {} { clock seconds }
proc incr_stat {args} {}
proc bsc_compile {source args} {
    set fails [string match "Fail*" $source]
    set transcript [open [file join $::srcdir $::subdir [make_bsc_output_name $source]] w]
    if {$fails} {
        set count [expr {$source eq "FailTwo.bs" ? 2 : $source eq "FailZero.bs" ? 0 : 1}]
        for {set n 0} {$n < $count} {incr n} {
            puts $transcript "Error: \"$source\", line 1, column 1: (T0001)"
            puts $transcript "  example diagnostic"
        }
    }
    close $transcript
    return [expr {!$fails}]
}
proc dumpbo {source} { return 1 }
proc dumpba {source} { return 1 }
proc absolute {path} { file normalize $path }
proc get_test_config_dir {} { file join $::env(PROVENANCE_ROOT) config }
namespace eval ::dejagnu::error { variable list {} }
foreach {name value} {
    exit_status 0 xml 0 prms_id 0 bug_id 0 xfail_flag 0 xfail_prms 0
    kfail_flag 0 kfail_prms 0 errcnt 0 warncnt 0 warning_threshold 0
    perror_threshold 1 multipass_name {} all_flag 0 subdir {} tool bsc
} { set ::$name $value }
if {[info exists env(PROVENANCE_TOOL)]} { set tool $env(PROVENANCE_TOOL) }
if {![info exists env(BSC_TEST_TRACE)] && [info exists env(PROVENANCE_ENABLED)]} {
    set env(BSC_TEST_TRACE) $env(PROVENANCE_ENABLED)
}
set DO_INTERNAL_CHECKS $env(PROVENANCE_INTERNAL)
set srcdir $env(PROVENANCE_ROOT)
set sum_file [open [file join $env(PROVENANCE_ROOT) testrun.sum] w]
init_testcounts
foreach type {PASS FAIL XPASS XFAIL KPASS KFAIL WARNING ERROR UNRESOLVED UNSUPPORTED UNTESTED} {
    set test_counts($type,count) 0
}
proc existing_pass_hook {message} {
    upvar 1 type final_type
    puts $::log "EXISTING HOOK $final_type $message"
}
set local_record_procs(pass) existing_pass_hook
cd $srcdir
set subdir cases
foreach fixture [lsort [glob -nocomplain [file join $env(PROVENANCE_ROOT) cases *.exp]]] {
    runtest $fixture
}
close $sum_file
close $log
'''


def decode_list(value):
    env = dict(os.environ, PROVENANCE_VALUE=value)
    result = subprocess.run(["tclsh"], input=r'''
foreach item $env(PROVENANCE_VALUE) {
    puts [binary encode base64 -maxlen 0 [encoding convertto utf-8 $item]]
}
''', text=True, capture_output=True, env=env)
    if result.returncode or result.stderr:
        raise AssertionError(f"Tcl list parsing failed: {result.stderr}")
    return [base64.b64decode(line).decode("utf-8") for line in result.stdout.splitlines()]


@unittest.skipUnless(shutil.which("tclsh") and (DEJAGNU / "framework.exp").is_file(),
                     "requires Tcl and installed DejaGnu (or DEJAGNU_LIBRARY)")
class ProvenanceTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="bsc-provenance-test-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.cases = self.root / "cases"
        self.cases.mkdir()
        self.driver = self.root / "driver.tcl"
        self.driver.write_text(DRIVER)

    def run_capture(self, *, internal=1, enabled=True, tool="bsc"):
        env = dict(os.environ, PROVENANCE_DEJAGNU=str(DEJAGNU),
                   PROVENANCE_SUITE=str(SUITE), PROVENANCE_ROOT=str(self.root),
                   PROVENANCE_ENABLED=str(int(enabled)),
                   PROVENANCE_INTERNAL=str(internal), PROVENANCE_TOOL=tool,
                   BSC_TEST_TRACE=str(int(enabled)))
        result = subprocess.run(["tclsh", str(self.driver)], env=env,
                                text=True, capture_output=True)
        self.assertEqual(result.returncode, 0, result.stderr + result.stdout)
        self.assertEqual(result.stderr, "")
        return (self.root / "testrun.log").read_text()

    def write_case(self, name, contents):
        (self.cases / f"{name}.exp").write_text(contents)

    def markers(self, log):
        return [decode_list(line.removeprefix("BSC-TEST: "))
                for line in log.splitlines() if line.startswith("BSC-TEST: ")]

    def test_direct_calls_locations_roles_and_final_verdicts(self):
        self.write_case("basic", (FIXTURES / "basic.tcl").read_text())
        shutil.copyfile(FIXTURES / "sourced.tcl", self.cases / "sourced.tcl")
        log = self.run_capture()
        markers = self.markers(log)
        begins = [m for m in markers if m[0] == "begin"]
        self.assertEqual(markers[0], ["script", "1", "cases/basic.exp", "1"])
        self.assertEqual(markers[-1], ["finish", "9"])
        self.assertEqual([m[1] for m in begins], list(map(str, range(1, 10))))
        self.assertEqual([m[4] for m in begins[:2]], ["3", "3"])
        self.assertEqual([decode_list(m[5]) for m in begins[:2]],
                         [["Same.bs", "-first", "0"], ["Same.bs", "-second", "0"]])
        self.assertEqual(decode_list(begins[2][5]),
                         ["odd {name} with a tab\t.bs", "-Dname={value with spaces}", "0"])
        self.assertEqual(begins[3][2:5], ["compile_fail", "cases/sourced.tcl", "2"])
        self.assertFalse(any("Wrapped.bs" in m[5] or "Backend.bs" in m[5] for m in begins))
        self.assertEqual(markers[1:4], [begins[0], ["role", "1", "object-load"], ["end", "1"]])
        summary = (self.root / "testrun.sum").read_text()
        self.assertIn("XFAIL: `Fail.bs' should compile (PRMS 123)", summary)
        self.assertIn("XPASS: `UnexpectedPass.bs' compiles (PRMS 456)", summary)
        self.assertIn("UNRESOLVED: `Threshold.bs' compiles", summary)
        self.assertIn("PASS: Intermediate file `Same.bo' can be loaded", summary)
        verdicts = [line for line in log.splitlines() if line.startswith(
            ("PASS:", "FAIL:", "XFAIL:", "XPASS:", "UNRESOLVED:"))]
        self.assertEqual(verdicts, summary.splitlines()[1:])
        self.assertIn("EXISTING HOOK PASS", log)

    def test_counters_reset_with_bsc_and_empty_tool(self):
        self.write_case("first", "compile_pass One.bs\ncompile_fail Fail.bs\n")
        self.write_case("second", "compile_pass Two.bs\n")
        for tool in ("bsc", ""):
            with self.subTest(tool=tool):
                markers = self.markers(self.run_capture(internal=0, tool=tool))
                self.assertEqual([m for m in markers if m[0] == "script"],
                                 [["script", "1", "cases/first.exp", "0"],
                                  ["script", "1", "cases/second.exp", "0"]])
                self.assertEqual([m for m in markers if m[0] == "finish"],
                                 [["finish", "2"], ["finish", "1"]])
                self.assertEqual([m[1] for m in markers if m[0] == "begin"], ["1", "2", "1"])
                self.assertNotIn("Intermediate file", (self.root / "testrun.sum").read_text())

    def test_compile_error_branch_roles_and_interleaved_numbering(self):
        self.write_case("errors", (FIXTURES / "errors.tcl").read_text())
        for internal in (1, 0):
            with self.subTest(internal=internal):
                log = self.run_capture(internal=internal)
                markers = self.markers(log)
                begins = [m for m in markers if m[0] == "begin"]
                self.assertEqual([m[1] for m in begins], list(map(str, range(1, 9))))
                self.assertEqual([m[2] for m in begins], ["compile_fail_error", "compile_pass",
                    "compile_fail_error", "compile_fail", "compile_fail_error",
                    "compile_fail_error", "compile_fail_error", "compile_fail_error"])
                self.assertEqual(decode_list(begins[0][5]), ["Fail.bs", "T0001", "1", "", "0"])
                diagnostic_ids = [m[1] for m in markers if m[0] == "role" and m[2] == "diagnostic-count"]
                self.assertEqual(diagnostic_ids, ["1", "3", "5", "6", "8"])
                unexpected = log.split("BSC-TEST: begin 7 ", 1)[1].split("BSC-TEST: end 7", 1)[0]
                self.assertIn("FAIL: `Good.bs' shouldn't compile", unexpected)
                self.assertEqual("BSC-TEST: role 7 object-load" in unexpected, bool(internal))
                self.assertEqual("PASS: Intermediate file" in unexpected, bool(internal))
                self.assertIn("FAIL: expected `1' copies of Error T9999 in `cases/Fail.bs.bsc-out', found `0'", log)
                self.assertIn("FAIL: expected `1' copies of Error T0001 in `cases/FailTwo.bs.bsc-out', found `2'", log)
                self.assertIn("PASS: found `0' copies of Error T0001", log)
                summary = (self.root / "testrun.sum").read_bytes()
                self.run_capture(internal=internal, enabled=False)
                self.assertEqual((self.root / "testrun.sum").read_bytes(), summary)

    def test_off_on_summaries_and_return_values_are_identical(self):
        self.write_case("returns", r'''
set a [compile_pass One.bs]
set b [compile_fail Fail.bs]
set output [open [file join $::env(PROVENANCE_ROOT) returns.txt] w]
puts $output [list $a $b]
close $output
''')
        self.assertNotIn("BSC-TEST:", self.run_capture(enabled=False))
        summary = (self.root / "testrun.sum").read_bytes()
        returns = (self.root / "returns.txt").read_bytes()
        self.assertIn("BSC-TEST:", self.run_capture(enabled=True))
        self.assertEqual((self.root / "testrun.sum").read_bytes(), summary)
        self.assertEqual((self.root / "returns.txt").read_bytes(), returns)

    def test_multiline_metadata_is_an_explicit_gap(self):
        self.write_case("multiline", 'compile_pass "two\\nlines.bs"\n'
                        'compile_pass "two{\\nlines.bs"\n'
                        'compile_pass "two\\rlines.bs"\ncompile_fail Fail.bs\n')
        log = self.run_capture()
        self.assertEqual(log.count("BSC-TEST: unsupported multiline metadata\n"), 3)
        begins = [m for m in self.markers(log) if m[0] == "begin"]
        self.assertEqual(len(begins), 1)
        self.assertEqual(begins[0][1:3], ["1", "compile_fail"])
        self.assertIn("PASS: `two\nlines.bs' compiles", log)

    def test_script_error_leaves_incomplete_call_and_next_file_resets(self):
        self.write_case("bad", r'''
rename bsc_compile original_bsc_compile
proc bsc_compile {args} { error "injected compiler exception" }
compile_pass Broken.bs
''')
        self.write_case("next", r'''
rename bsc_compile {}
rename original_bsc_compile bsc_compile
compile_pass Next.bs
''')
        log = self.run_capture()
        sections = log.split("BSC-TEST: script ")
        self.assertIn("BSC-TEST: begin 1 compile_pass", sections[1])
        self.assertNotIn("BSC-TEST: end 1", sections[1])
        self.assertIn("ERROR: tcl error sourcing", sections[1])
        self.assertIn("UNRESOLVED:", sections[1])
        self.assertIn("BSC-TEST: begin 1 compile_pass", sections[2])
        self.assertIn("BSC-TEST: end 1", sections[2])
        self.assertIn("BSC-TEST: finish 1", sections[2])


if __name__ == "__main__":
    unittest.main()
