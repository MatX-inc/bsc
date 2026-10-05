#!/bin/csh
# script to find failures after parallel build
foreach i (`find . -path './.*' -prune -o -name "*.sum" -print`)
  grep -l -e "^FAIL" $i
end
