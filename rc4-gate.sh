#!/usr/bin/env bash
# rc3-gate.sh <pr-number> [<pr-number>...]
# Content gate: every line ADDED by the PR's net diff (merge-base..head) must exist
# verbatim somewhere in the corresponding file of the assembled tree (this worktree).
# Prints missing lines; exit 0 iff none missing (after exceptions).
# Exceptions: rc4-gate-exceptions.txt lines of the form "<pr>|<file>|<exact line>"
set -u
cd "$(dirname "$0")"
EXC=rc4-gate-exceptions.txt
fail_total=0
for pr in "$@"; do
  base=$(git merge-base upstream/main "refs/rcpr/$pr") || { echo "PR $pr: no merge-base"; fail_total=1; continue; }
  missing=0
  # list files with additions in the PR diff
  while IFS= read -r file; do
    # collect added lines for this file (skip blank/whitespace-only)
    tmp_added=$(mktemp)
    git diff -U0 "$base" "refs/rcpr/$pr" -- "$file" \
      | sed -n 's/^+\([^+].*\|$\)/\1/p' \
      | grep -v '^[[:space:]]*$' > "$tmp_added" || true
    if [ ! -s "$tmp_added" ]; then rm -f "$tmp_added"; continue; fi
    if [ ! -f "$file" ]; then
      # whole file must have been (re)moved by a later PR: every line is a miss unless excepted
      while IFS= read -r line; do
        if [ -f "$EXC" ] && grep -qxF "$pr|$file|$line" "$EXC"; then continue; fi
        echo "MISS pr=$pr file=$file (file absent): $line"
        missing=$((missing+1))
      done < "$tmp_added"
      rm -f "$tmp_added"; continue
    fi
    # check each added line exists verbatim in the assembled file
    while IFS= read -r line; do
      if grep -qxF -- "$line" "$file"; then continue; fi
      if [ -f "$EXC" ] && grep -qxF -- "$pr|$file|$line" "$EXC"; then continue; fi
      echo "MISS pr=$pr file=$file: $line"
      missing=$((missing+1))
    done < "$tmp_added"
    rm -f "$tmp_added"
  done < <(git diff --name-only --diff-filter=ACMR "$base" "refs/rcpr/$pr")
  if [ "$missing" -eq 0 ]; then
    echo "GATE OK pr=$pr"
  else
    echo "GATE FAIL pr=$pr missing=$missing"
    fail_total=1
  fi
done
exit $fail_total
