#!/usr/bin/env bash
# Differential test of two Bluespec Classic parsers.
#
#   difftest.sh [-o ORACLE] [-p PARSER] [--defns] FILE...
#   find DIR -name '*.bs' -print0 | difftest.sh -0 [options]
#
# Runs bsc's own parser (bsc-classic-parse, "the oracle") and another parser
# (bh-parse by default) over every FILE and prints each file on which the two
# disagree about accept/reject, with both tools' messages.  Exit status is 0
# if they agree everywhere, 1 if not, 2 on a usage problem.
#
# The oracle defaults to $BSC_CLASSIC_PARSE, else the binary under
# dist-newstyle next to this script; the other parser to $BH_PARSE, else
# `bh-parse` on $PATH.  Both are run once with all the files, and their
# per-file lines ("NAME: ok ..." / "NAME:LINE:COL: message") are joined on
# NAME, so a name containing ':' will confuse it.
set -u

usage() { sed -n '2,/^$/{s/^# \{0,1\}//;p}' "$0"; }

oracle=${BSC_CLASSIC_PARSE:-}
parser=${BH_PARSE:-bh-parse}
mode=()
files=()
nul=0
while (($#)); do
  case $1 in
    -o) oracle=$2; shift 2 ;;
    -p) parser=$2; shift 2 ;;
    --defns) mode=(--defns); shift ;;
    -0) nul=1; shift ;;
    -h|--help) usage; exit 0 ;;
    --) shift; files+=("$@"); break ;;
    -*) echo "difftest.sh: unknown option $1" >&2; exit 2 ;;
    *) files+=("$1"); shift ;;
  esac
done
if ((nul)); then
  while IFS= read -r -d '' f; do files+=("$f"); done
fi
if [[ -z $oracle ]]; then
  oracle=$(find "$(dirname "$0")/dist-newstyle" -type f -name bsc-classic-parse -perm -u+x 2>/dev/null | head -n1)
fi
if [[ -z $oracle || ! -x $oracle ]]; then
  echo "difftest.sh: oracle not found; run 'cabal build' in $(dirname "$0") or pass -o" >&2
  exit 2
fi
if ! command -v "$parser" >/dev/null; then
  echo "difftest.sh: parser '$parser' not found; pass -p or set \$BH_PARSE" >&2
  exit 2
fi
if ((${#files[@]} == 0)); then
  echo "difftest.sh: no files given" >&2
  exit 2
fi

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
printf '%s\n' "${files[@]}" > "$tmp/files"
"$oracle" "${mode[@]}" "${files[@]}" > "$tmp/oracle" 2>&1
"$parser" "${mode[@]}" "${files[@]}" > "$tmp/parser" 2>&1

# Classify each tool's per-file line as ok / crash / reject, then compare.
awk -v oracle_out="$tmp/oracle" -v parser_out="$tmp/parser" '
function load(file, verdict, line,    l, name, v) {
  while ((getline l < file) > 0) {
    name = l; sub(/:.*/, "", name)
    if (!(name in known)) continue
    if (l ~ /^[^:]*: ok( |$)/) v = "ok"
    else if (l ~ /^[^:]*: CRASH/) v = "crash"
    else v = "reject"
    verdict[name] = v; line[name] = l
  }
  close(file)
}
NR == FNR { known[$0] = 1; order[NR] = $0; n = NR; next }
END {
  load(oracle_out, ov, ol); load(parser_out, pv, pl)
  agree = 0; d1 = 0; d2 = 0; d3 = 0
  for (i = 1; i <= n; i++) {
    f = order[i]
    o = (f in ov) ? ov[f] : "missing"; p = (f in pv) ? pv[f] : "missing"
    if (o == p) { agree++; continue }
    if (o == "ok" && p == "reject") d1++
    else if (o == "reject" && p == "ok") d2++
    else d3++
    printf "DISAGREE %s\n  oracle: %s\n  parser: %s\n", f, (f in ol) ? ol[f] : "(no output)", (f in pl) ? pl[f] : "(no output)"
  }
  printf "%d files, %d agree, %d disagree (oracle accepts/parser rejects %d, oracle rejects/parser accepts %d, crash or missing %d)\n", n, agree, n - agree, d1, d2, d3
  exit (agree == n) ? 0 : 1
}' "$tmp/files"
