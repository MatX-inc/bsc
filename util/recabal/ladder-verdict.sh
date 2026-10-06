#!/usr/bin/env bash
# Compare the hash lists of a determinism ladder (hash-build.sh outputs, one
# per build) against the first one named: pass means every list is identical.
# Differences are reported by category (program, object, interface, archive)
# so a failure says what moved. Usage: util/recabal/ladder-verdict.sh REF.sha256 OTHER.sha256...
set -u
ref="${1:?reference hash list}"; shift
[ -s "$ref" ] || { echo "verdict: reference $ref is missing or empty"; exit 2; }
fail=0
for other in "$@"; do
  name="$(basename "$other" .sha256)"
  if [ ! -s "$other" ]; then echo "MISSING: $name (no hash list)"; fail=1; continue; fi
  if diff -q "$ref" "$other" >/dev/null; then echo "IDENTICAL: $(basename "$ref" .sha256) vs $name ($(wc -l < "$other") files)"; continue; fi
  fail=1
  diffs=$(diff "$ref" "$other" | grep '^[<>]' | awk '{print $NF}' | sort -u)
  n=$(printf '%s\n' "$diffs" | sed '/^$/d' | wc -l)
  echo "DIFFER: $(basename "$ref" .sha256) vs $name: $n file(s)"
  for cat in 'program' 'object' 'interface' 'archive' 'other'; do
    case "$cat" in
      program) sel=$(printf '%s\n' "$diffs" | grep -v '\.\(o\|hi\|dyn_o\|dyn_hi\|p_o\|p_hi\|a\)$' || true);;
      object) sel=$(printf '%s\n' "$diffs" | grep '\.\(o\|dyn_o\|p_o\)$' || true);;
      interface) sel=$(printf '%s\n' "$diffs" | grep '\.\(hi\|dyn_hi\|p_hi\)$' || true);;
      archive) sel=$(printf '%s\n' "$diffs" | grep '\.a$' || true);;
      *) sel="";;
    esac
    c=$(printf '%s\n' "$sel" | sed '/^$/d' | wc -l)
    [ "$c" -gt 0 ] && { echo "   $cat: $c"; printf '%s\n' "$sel" | sed '/^$/d' | head -8 | sed 's/^/      /'; }
  done
done
echo "result: $([ $fail -eq 0 ] && echo PASS || echo FAIL)"
exit $fail
