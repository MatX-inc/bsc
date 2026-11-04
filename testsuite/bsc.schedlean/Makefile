# for "make clean" to work everywhere

CONFDIR = $(realpath ..)

# the Lean files schedlean writes (its .check.log files are *.log)
DONTKEEPFILES = MethTask.lean SysMeJoin.lean ConflictFree.lean MethArg.lean RdyRead.lean Mutants.lean *_mut*.lean \
	GatedFamily.lean TwoResets.lean GatedChild.lean TwoDomains.lean \
	SysHier1.lean MkCounter.lean MkPair.lean SysHier2.lean MkMid.lean MkLeaf.lean SysHier3.lean MkGuarded.lean SysHierArg.lean \
	sysGrow512.gate.log sysGrow2048.gate.log sysGrow2048.show.log sysWide64.gate.log sysWide256.gate.log \
	sysWide64.alone.log sysWide256.alone.log alone SysAE.lean MkAccAE.lean SysChain*.lean MkA*.lean MkB*.lean \
	sysChain32.emit.log sysChain128.emit.log schedlean-cache *.olean.d \
	GatedMix.lean RegFileMeth.lean ConflictFree3.lean FanIn.lean SysHierSf.lean MkSfKid.lean sysWide32.lean.log \
	sysWide64.lean.log

include $(CONFDIR)/clean.mk
