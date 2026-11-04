#!/bin/sh
# A stand-in for lean (tests that need no Lean): the log recorded for the file's stem (<stem>.fakelog
# here), for a mutant an error in its first definition, and for Slow a run that does not end.
for a; do f=$a; done
d=$(dirname "$0")
stem=$(basename "$f" .lean)
case $stem in
  *_mut*) n=$(grep -n '^def ' "$f" | head -1 | cut -d: -f1)
          echo "$f:$n:0: error: (stand-in) the edit broke a definition"; exit 1 ;;
  Slow) exec sleep 60 ;;
esac
if [ -f "$d/$stem.fakelog" ]; then cat "$d/$stem.fakelog"; exit 0; fi
echo "$f:1:0: error: (stand-in) no log recorded for $stem"; exit 1
