"""Most sites of one step on one port, per corpus design, from the emitter's model (E2E_GATE=1, no site
conditions): the step index check (`hchkT`, `contains` per site) is quadratic in this number.
usage: E2E_GATE=1 python3 -I portsites.py E2E_TOOLS CORPUS_DIR BA_DATA"""
import os, sys
from collections import Counter
sys.path.insert(0, sys.argv[1])
import design, model  # noqa: E402
root, data = sys.argv[2], sys.argv[3]
best = []
for d in sorted(os.listdir(root)):
    p = os.path.join(root, d, 'dump.txt')
    if not os.path.exists(p):
        continue
    try:
        M = model.Model(design.Design(p, os.path.join(data, d + '.ba.txt')), None, None)
    except Exception as ex:
        print(d, 'error', type(ex).__name__, str(ex)[:80]); continue
    m = max([max(Counter(st['port'] for st in r['sites']).values(), default=0) for r in M.rules] + [0])
    best.append((m, d))
    print(d, m)
print('max', max(best))
