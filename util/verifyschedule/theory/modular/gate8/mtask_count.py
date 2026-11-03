"""Task calls per corpus design: rules' statements and calls inside methods (G0020).
usage: python3 -I mtask_count.py EMITTER_TOOLS DUMPROOT BADATA"""
import os, sys
sys.path.insert(0, sys.argv[1])
import design
root, data = sys.argv[2], sys.argv[3]
tot_m = tot_r = 0
for d in sorted(os.listdir(root)):
    dump = os.path.join(root, d, 'dump.txt')
    ba = os.path.join(data, d + '.ba.txt')
    if not os.path.exists(dump):
        continue
    D = design.Design(dump, ba if os.path.exists(ba) else '')
    if D.A is None:
        print(d, 'no APackage'); continue
    mt = [(s['name'], sum(1 for a in D.A.rules.get(s['name'], (None, []))[1] if a and a[0] == 'task'))
          for s in D.steps if s['kind'] == 'method']
    rt = sum(sum(1 for a in D.A.rules.get(s['name'], (None, []))[1] if a and a[0] == 'task') for s in D.steps if s['kind'] == 'rule')
    mt = [x for x in mt if x[1]]
    tot_m += sum(n for _, n in mt); tot_r += rt
    # also any APackage rule body not matched to a step (a method body under another name)
    names = set(s['name'] for s in D.steps)
    other = [(n, sum(1 for a in acts if a and a[0] == 'task')) for n, (p, acts) in D.A.rules.items() if n not in names]
    other = [x for x in other if x[1]]
    if mt or other:
        print(d, 'method tasks', mt, 'unmatched bodies with tasks', other)
print('rule task statements', tot_r, 'method task calls', tot_m)
