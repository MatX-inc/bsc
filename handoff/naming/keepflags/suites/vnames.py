# Declared-name extraction and classification for bsc-generated Verilog.
# usage as a module: from vnames import modules; modules(path) -> {modname: {name: kind}}
import re, sys
KINDS = ['port', 'state', 'fire', 'methcond', 'inst$port', 'reg$port', 'wire$port', 'mux', 'temp', 'other']
TEMP = re.compile(r'(__h\d+|___d\d+|_d\d+|__q\d+)$')
DECL = re.compile(r'^\s*(input|output|inout|wire|reg)\b(.*)$', re.S)
def strip_comments(t):
    t = re.sub(r'/\*.*?\*/', ' ', t, flags=re.S)
    return re.sub(r'//[^\n]*', '', t)
def split_modules(text):
    # returns [(name, body_with_comments)]
    out = []
    for m in re.finditer(r'^module\s+(\S+?)\s*\(.*?^endmodule', text, flags=re.S | re.M):
        out.append((m.group(1), m.group(0)))
    return out
def classify_module(body):
    insts = set(re.findall(r'^\s*// submodule (\S+)', body, flags=re.M))
    regs = set(re.findall(r'^\s*// register (\S+)', body, flags=re.M))
    code = strip_comments(body)
    # drop the module header port list, keep statements after it
    hdr_end = code.find(');')
    stmts = code[hdr_end + 2:].split(';')
    names = {}
    for st in stmts:
        m = DECL.match(st)
        if not m: continue
        rest = re.sub(r'\[[^\]]*\]', ' ', m.group(2))
        rest = re.sub(r'\b(signed|reg|wire)\b', ' ', rest)
        for part in rest.split(','):
            part = part.split('=')[0].strip()
            if not part: continue
            n = part.split()[-1] if part.split() else ''
            if n: names[n] = m.group(1)
    res = {}
    for n, d in names.items():
        if d in ('input', 'output', 'inout'): k = 'port'
        elif n in regs: k = 'state'
        elif n.startswith('CAN_FIRE_') or n.startswith('WILL_FIRE_'): k = 'fire'
        elif n.startswith('COND_'): k = 'methcond'
        elif n.startswith('MUX_'): k = 'mux'
        elif '$' in n:
            p = n.split('$')[0]
            k = 'inst$port' if p in insts else ('reg$port' if p in regs else 'wire$port')
        elif TEMP.search(n): k = 'temp'
        else: k = 'other'
        res[n] = k
    return res
def modules(path):
    text = open(path, errors='replace').read()
    return {name: classify_module(body) for name, body in split_modules(text)}
if __name__ == '__main__':
    for mod, ns in modules(sys.argv[1]).items():
        for n, k in sorted(ns.items(), key=lambda x: (x[1], x[0])):
            print(mod, k, n)
