# usage: python3 -I mkedits.py <srcdir> <outdir> : writes <outdir>/<probe>/<edit>/<file> for each
# probe and edit; the marker lines are deleted in the base and replaced by the edit text, so the
# lines after an insertion shift as they would in a real edit
import os, sys
src, out = sys.argv[1], sys.argv[2]
E = {
 'Probe.bsv': {
   'base': {},
   'addrule': {'RULE': "   rule clear_acc (mode && phase == 7);\n      acc <= 0;\n   endrule\n"},
   'addrule_top': {'TOPRULE': "   rule clear_acc (mode && phase == 7);\n      acc <= 0;\n   endrule\n\n"},
   'addreg': {'DECL': "   Reg#(Bit#(8)) seen <- mkReg(0);\n", 'ALT': "      seen <= seen + 1;\n"},
   'addmeth': {'IFC': "   method Action flush;\n", 'METH': "   method Action flush;\n      acc <= 0;\n   endmethod\n"},
   'addvmeth': {'IFC': "   method Bool busy;\n", 'METH': "   method Bool busy = mode || outq.notFull;\n"},
   'addsub': {'CTR_DECL': "   Reg#(Bit#(8)) peak <- mkReg(0);\n", 'CTR_RULE': "\n   rule track (cnt > peak);\n      peak <= cnt;\n   endrule\n"},
 },
 'Probe2.bs': {
   'base': {},
   'addrule': {'RULE': "      \"reset_held\": when busy, cnt == 3 ==> held := 0\n"},
   'addrule_top': {'TOPRULE': "      \"reset_held\": when busy, cnt == 3 ==> held := 0\n"},
   'addreg': {'DECL': "    seen :: Reg (Bit 8) <- mkReg 0\n", 'MOVE': "          seen := seen + 1\n"},
   'addmeth': {'IFC': "    flush :: Action\n", 'METH': "      flush = held := 0\n"},
   'addvmeth': {'IFC': "    isBusy :: Bool\n", 'METH': "      isBusy = busy\n"},
   'addsub': {'ACC_DECL': "    hits :: Reg (Bit 8) <- mkReg 0\n", 'ACC_RULE': "      \"count_hits\": when isValid inc.wget ==> hits := hits + 1\n"},
 },
}
for f, edits in E.items():
    text = open(os.path.join(src, f)).read().splitlines(keepends=True)
    for ed, ins in edits.items():
        o = []
        for line in text:
            st = line.strip()
            if st.startswith('//@') or st.startswith('--@'):
                o.append(ins.get(st[3:], ''))
            else:
                o.append(line)
        d = os.path.join(out, f.split('.')[0], ed); os.makedirs(d, exist_ok=True)
        open(os.path.join(d, f), 'w').write(''.join(o))
