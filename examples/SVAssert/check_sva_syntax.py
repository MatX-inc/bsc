# Parses bsc output with slang (pip install pyslang) and reports any SystemVerilog
# errors, as a syntax check on the emitted SVA:  python3 check_sva_syntax.py mkArb.v
import sys
from pyslang.syntax import SyntaxTree
from pyslang.ast import Compilation
import pyslang

ok = True
for path in sys.argv[1:]:
    tree = SyntaxTree.fromFile(path)
    comp = Compilation()
    comp.addSyntaxTree(tree)
    diags = list(comp.getAllDiagnostics())
    errors = [d for d in diags if d.isError()]
    n = sum(1 for line in open(path) if " property (" in line)
    print(f"{path}: {n} property statements, {len(errors)} errors, {len(diags)-len(errors)} warnings")
    if errors:
        de = pyslang.DiagnosticEngine(comp.sourceManager) if hasattr(pyslang, 'DiagnosticEngine') else None
        for d in errors[:20]:
            print("  ", de.formatMessage(d) if de else d, "line", comp.sourceManager.getLineNumber(d.location))
        ok = False
sys.exit(0 if ok else 1)
