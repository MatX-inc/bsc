#!/bin/bash
# Assemble the three variant .x files from shared parts and run alex.
set -e
cd "$(dirname "$0")"
mkdir -p gen

mkvariant() {
  local mod=$1 stream=$2 comment=$3
  sed -e "s/@@MODNAME@@/$mod/" -e "s|@@STREAMTYPE@@|$stream|" -e "s/@@VARIANTCOMMENT@@/$comment/" \
      alexparts/header.template > gen/$mod.x
  cat alexparts/rules.part >> gen/$mod.x
  sed -e "s|@@STREAMTYPE@@|$stream|" alexparts/footer.part >> gen/$mod.x
  alex -g gen/$mod.x -o gen/$mod.hs
}

mkvariant LexAlexString "String"        "String"
mkvariant LexAlexSBS    "SB.ByteString" "strict ByteString"
mkvariant LexAlexLBS    "LB.ByteString" "lazy ByteString"
mkvariant LexAlexLT     "TL.Text"       "lazy Text"
mkvariant LexAlexST     "T.Text"        "strict Text"

# strict Text + ASCII-identifier fast path: same header/rules, custom footer
# (modified driver `go` + fastIdTok), plus an extra import of LexAlexFastPath.
sed -e "s/@@MODNAME@@/LexAlexSTF/" -e "s|@@STREAMTYPE@@|T.Text|" \
    -e "s/@@VARIANTCOMMENT@@/strict Text with ASCII-identifier fast path/" \
    -e "s/^import LexAlexShared$/import LexAlexShared\nimport LexAlexFastPath/" \
    alexparts/header.template > gen/LexAlexSTF.x
cat alexparts/rules.part >> gen/LexAlexSTF.x
sed -e "s|@@STREAMTYPE@@|T.Text|" alexparts/footerSTF.part >> gen/LexAlexSTF.x
alex -g gen/LexAlexSTF.x -o gen/LexAlexSTF.hs

echo "alex generation OK"
