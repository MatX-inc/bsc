#!/usr/bin/env python3
"""Exercise rule inputs and local build-action reuse with the pinned Buck2.

The runner is a stub: this does not build BSC or execute compiler tests.
"""

import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


HERE = Path(__file__).resolve().parent
REPO = HERE.parents[1]
VALIDATION = HERE.parent / ".stage1-validation"
BUCK2 = Path(os.environ.get("BUCK2", VALIDATION / "buck2-tool" / "buck2")).resolve()

RUNNER = '''#!/usr/bin/python3
import json
from pathlib import Path
import sys
import uuid

assert sys.argv[1] == "execute"
plan = json.loads(Path(sys.argv[2]).read_text())
if plan.get("infrastructure_error"):
    raise SystemExit(9)
output = Path(sys.argv[sys.argv.index("--output") + 1])
assert not output.exists(), "output must start absent"
output.mkdir(parents=True)
(output / "result.json").write_text(json.dumps({
    "verdict": "FAIL", "execution": str(uuid.uuid4()), "identifier": sys.argv[3],
}))
'''


@unittest.skipUnless(BUCK2.is_file(), "run testsuite/buck2/bootstrap.py first")
class RuleTests(unittest.TestCase):
    def setUp(self):
        VALIDATION.mkdir(exist_ok=True)
        self.temporary = tempfile.TemporaryDirectory(prefix="buck2-rules-", dir=VALIDATION)
        self.cell = Path(self.temporary.name)
        self.addCleanup(self.temporary.cleanup)
        self.addCleanup(lambda: subprocess.run([str(BUCK2), "kill"], cwd=self.cell,
                                              capture_output=True, check=False))
        shutil.copytree(REPO / "rules", self.cell / "rules")
        shutil.copyfile(HERE / "buckconfig", self.cell / ".buckconfig")
        for path in ("suite", "installation", "tools"):
            (self.cell / path).mkdir()
        (self.cell / "suite" / "example.bs").write_text("package Example where\n")
        (self.cell / "installation" / "identity").write_text("compiler snapshot\n")
        (self.cell / "host-identity.json").write_text("{}\n")
        (self.cell / "plan.json").write_text("{}\n")
        (self.cell / "tools" / "runner").write_text(RUNNER)
        (self.cell / "tools" / "runner").chmod(0o755)
        (self.cell / "BUCK").write_text('''
load("//rules/bluespec:defs.bzl", "bluespec_toolchain")
load("//rules/bsctest:defs.bzl", "bsc_test")
bluespec_toolchain(name = "bsc", installation = "installation", host_identity = "host-identity.json")
bsc_test(name = "sample", toolchain = ":bsc", runner = "tools/runner", plan = "plan.json", identifier = "v3:example", suite = "suite")
''')
        self.build_number = 0

    def buck(self, *args, successful=True):
        result = subprocess.run([str(BUCK2), *args], cwd=self.cell,
                                capture_output=True, text=True)
        if successful:
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        else:
            self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        return result

    def build(self):
        self.build_number += 1
        report_path = self.cell / f"build-{self.build_number}.json"
        self.buck("build", "//:sample", "--build-report", str(report_path))
        report = json.loads(report_path.read_text())
        output, = report["results"]["root//:sample"]["outputs"]["DEFAULT"]
        result = json.loads((self.cell / output / "result.json").read_text())
        ran = self.buck("log", "what-ran", "--trace-id", report["trace_id"],
                        "--format", "json", "--filter-category", "bsc_test")
        commands = [json.loads(line) for line in ran.stdout.splitlines() if line]
        return result, commands

    def test_failed_verdict_reuse_and_all_declared_inputs(self):
        result, commands = self.build()
        self.assertEqual(result["verdict"], "FAIL")
        self.assertEqual(len(commands), 1)
        repeated, commands = self.build()
        self.assertEqual(repeated, result)
        self.assertEqual(commands, [])
        for path in ("suite/example.bs", "installation/identity", "plan.json",
                     "host-identity.json", "tools/runner"):
            with self.subTest(changed=path):
                file = self.cell / path
                # Whitespace changes preserve the stub's behavior while
                # requiring a new action key for every declared input.
                with file.open("a") as output:
                    output.write("\n")
                updated, commands = self.build()
                self.assertNotEqual(updated["execution"], result["execution"])
                self.assertEqual(len(commands), 1)
                result = updated
        (self.cell / "plan.json").write_text('{"infrastructure_error": true}\n')
        self.buck("build", "//:sample", successful=False)


if __name__ == "__main__":
    unittest.main()
