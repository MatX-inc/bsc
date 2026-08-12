#!/usr/bin/env python3
# List the unit ids of the direct Haskell dependencies of the bsc package's
# library and executables (not its custom-setup), from cabal's build plan.
# These seed make-ghc-pkg-info.sh, which follows the transitive dependencies
# via ghc-pkg; unit ids keep the lookups unambiguous even when a package db
# carries several instances of the same package.

import json
import sys

plan_path = sys.argv[1] if len(sys.argv) > 1 else "dist-newstyle/cache/plan.json"
with open(plan_path) as f:
    plan = json.load(f)

deps = set()
for item in plan["install-plan"]:
    if item.get("pkg-name") != "bsc":
        continue
    if "components" in item:
        for name, comp in item["components"].items():
            if name != "setup":
                deps.update(comp.get("depends", []))
    elif item.get("component-name") != "setup":
        deps.update(item.get("depends", []))

print(" ".join(sorted(deps)))
