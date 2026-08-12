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
echo "alex generation OK"
