#!/usr/bin/env python3
"""Pipeline matrix: the LLVM-upgrade ritual for AOT_PIPELINE.

The bespoke pass list (trs-codegen lower.rs AOT_PIPELINE) is chosen
by measurement, not by -O level.  New passes only appear when LLVM
itself is upgraded, so on every LLVM version bump (and whenever the
emitted IR's shape changes materially) re-run this matrix:

    python3 tools/pipeline-matrix.py <workdir>

where <workdir> holds per-design dirs with prebuilt .bir files (a
bench.py workdir).  It links each design under default<O3>,
default<O1>, and the bespoke candidates, measuring link wall,
runtime (min/median of 7), and byte-exactness.  Decision rule: if
the NEW LLVM's default<O3> beats AOT_PIPELINE on any axis, the new
version gained a pass worth stealing — find it (llvm opt
-print-pipeline-passes 'default<O3>' diffed against the previous
version) and re-derive the list.  A rejected pipeline string (pass
renamed/removed) is caught loudly at run time by run_ir_passes.
"""
import sys
import subprocess, time, os, statistics
R=os.environ.get("TRS", "trs")
IC="instcombine<no-verify-fixpoint>"
PIPES={
 "O3(default)": None,
 "O1": "default<O1>",
 "P-min": f"cgscc(inline),function(early-cse<memssa>,{IC},simplifycfg)",
 "P-full": f"cgscc(inline),function(early-cse<memssa>,{IC},simplifycfg,jump-threading,gvn,dse,{IC},simplifycfg)",
 "P-nogvn": f"cgscc(inline),function(early-cse<memssa>,{IC},simplifycfg,jump-threading,dse,{IC},simplifycfg)",
}
ROOT=sys.argv[1] if len(sys.argv)>1 else "/tmp/trs-bench-4124"
TRS=os.environ.get("TRS", R)
DESIGNS=[
 ("BRAM0Test",ROOT+"/BRAM0Test/trs","sysBRAM0Test.bir",[]),
 ("DFT64v1",ROOT+"/DFT64v1/trs","sysTb_v1.bir",[]),
 ("FloatTest",ROOT+"/FloatTest/trs","sysFloatTest.bir",[]),
 ("TrafficBRAM",ROOT+"/TrafficBRAM/trs","sysTrafficBRAM.bir",[]),
 ("Dividers",ROOT+"/Dividers/trs","sysTest_mkNonPipelinedDivider.bir",[]),
]
for name,cwd,bir,args in DESIGNS:
    ref=None
    for pname,p in PIPES.items():
        env=dict(os.environ, TRS_REQUIRE_AOT="1")
        if p: env["TRS_JIT_PIPELINE"]=p
        t0=time.perf_counter()
        r=subprocess.run([TRS,"link",bir,"-o","px"],cwd=cwd,env=env,capture_output=True)
        lt=time.perf_counter()-t0
        if r.returncode!=0:
            print(f"{name:12s} {pname:10s} LINK-FAIL"); continue
        out=subprocess.run([TRS,"run","px.so"]+args,cwd=cwd,env=env,capture_output=True).stdout
        if ref is None: ref=out
        ok = "EXACT" if out==ref else "DIFF!"
        ts=[]
        for _ in range(7):
            t1=time.perf_counter()
            subprocess.run([TRS,"run","px.so"]+args,cwd=cwd,env=env,stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL)
            ts.append((time.perf_counter()-t1)*1000)
        print(f"{name:12s} {pname:10s} link {lt:7.1f}s run min {min(ts):7.1f} med {statistics.median(ts):7.1f} ms {ok}", flush=True)
