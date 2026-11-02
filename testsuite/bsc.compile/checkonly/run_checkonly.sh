#!/usr/bin/env sh
# Byte-level checks of the files the signature-only modes write.
# -signature-only writes <Pkg>.bi and nothing else; -check-only writes
# <Pkg>.bi, the same bytes, and <Pkg>.bcheck, the typechecked package, and
# nothing else.  An edit inside definition bodies leaves the .bi unchanged
# and changes the .bcheck; an interface edit changes both.  The .bi bytes
# do not depend on the directory; the .bcheck keeps real positions, so its
# bytes name the directory unless -remap-path-prefix maps it, and are the
# same from two directories mapped to one prefix.  A .bi on the path changes nothing
# about a full compile.  The signature a .bi holds is the one its .bo
# holds, and the package a .bcheck holds (the typechecked package after
# dictionary lifting, with its lifted dictionaries) prints as the
# compiler's -dliftdicts dump of the same compile, and a class default
# method's do block keeps its let statement, which the round-trip covers.
set -e

BSC=${BSC:-bsc}
DUMPBO=${DUMPBO:-dumpbo}

rm -rf work
mkdir -p work/one work/two work/three work/sub/deeper work/remap/a work/remap/b \
         work/full work/kinded work/defaults
cp A1.bs work/one/A.bs
cp A2.bs work/two/A.bs
cp A3.bs work/three/A.bs
cp A1.bs work/sub/deeper/A.bs
cp A1.bs work/remap/a/A.bs
cp A1.bs work/remap/b/A.bs
cp A1.bs work/full/A.bs
cp B.bs work/full/B.bs
cp Kinded.bsv work/kinded/Kinded.bsv
cp Defaults.bs work/defaults/Defaults.bs

status=0
check() {   # check LABEL COMMAND...: print LABEL on success, FAILED LABEL otherwise
    label=$1; shift
    if "$@"; then echo "$label"; else echo "FAILED $label"; status=1; fi
}

# -signature-only writes the .bi and nothing else; -check-only the .bi and
# the .bcheck and nothing else, and its .bi is the same bytes.  The check
# compile also dumps the typechecked package for the comparison below.
( cd work/one && $BSC -signature-only A.bs ) > work/one.log 2>&1
check "WROTE A.bi ONLY (-signature-only)" \
    sh -c 'cd work/one && test -f A.bi && ! test -f A.bcheck && ! test -f A.bo'
mv work/one/A.bi work/one/A.bi.sig
( cd work/one && $BSC -check-only -dliftdicts=A.lift A.bs ) >> work/one.log 2>&1
check "WROTE A.bi A.bcheck ONLY (-check-only)" \
    sh -c 'cd work/one && test -f A.bi && test -f A.bcheck && ! test -f A.bo'
check "IDENTICAL A.bi (-signature-only, -check-only)" cmp -s work/one/A.bi.sig work/one/A.bi

# each file starts with its own tag (GenBin.biHeader, bcheckHeader)
check "TAG A.bi bsc-bi-20261102-1" \
    sh -c 'test "$(head -c 17 work/one/A.bi)" = "bsc-bi-20261102-1"'
check "TAG A.bcheck bsc-bcheck-20261102-2" \
    sh -c 'test "$(head -c 21 work/one/A.bcheck)" = "bsc-bcheck-20261102-2"'

# the package a .bcheck holds is the typechecked package after dictionary
# lifting: dumpbo prints it with the printer the -dliftdicts dump uses,
# then its lifted dictionaries.  The dump wraps its text in a header line
# ("=== liftdicts (A):") and a footer (a blank line, then "-----"), which
# are dropped here; dumpbo prints the dependency names on its first line
# and the lifted dictionaries after the package, dropped too.
sed -e '1d' -e '/^-----$/,$d' work/one/A.lift | sed -e '$d' > work/one/A.lift.body
$DUMPBO work/one/A.bcheck | sed -e '1d' -e '/^Lifted dictionaries: $/,$d' > work/one/A.bcheck.txt
check "SAME-PACKAGE dumpbo A.bcheck, -dliftdicts" cmp -s work/one/A.lift.body work/one/A.bcheck.txt
# the lifted dictionaries are in the file: dumpbo -stats counts them
check "LIFTED-DICTIONARIES-IN A.bcheck (dumpbo -stats)" \
    sh -c "$DUMPBO -stats work/one/A.bcheck | grep -q '^lifted dictionaries: [1-9]'"
check "DEPENDS A.bcheck Prelude PreludeBSV" \
    sh -c "$DUMPBO work/one/A.bcheck | head -n 1 | grep -q '^Depends: Prelude PreludeBSV\$'"

# a type parameter declared with its kind ("numeric type n") is an IdPKind
# in the typechecked package, a form no .bo ever holds; the .bcheck writes
# it and the package prints as the dump does
( cd work/kinded && $BSC -check-only -dliftdicts=Kinded.lift Kinded.bsv ) > work/kinded.log 2>&1
check "WROTE Kinded.bcheck (kind-annotated type parameters)" test -f work/kinded/Kinded.bcheck
sed -e '1d' -e '/^-----$/,$d' work/kinded/Kinded.lift | sed -e '$d' > work/kinded/Kinded.lift.body
$DUMPBO work/kinded/Kinded.bcheck | sed -e '1d' -e '/^Lifted dictionaries: $/,$d' > work/kinded/Kinded.bcheck.txt
check "SAME-PACKAGE dumpbo Kinded.bcheck, -dliftdicts" \
    cmp -s work/kinded/Kinded.lift.body work/kinded/Kinded.bcheck.txt

# a class default method keeps the let statement of its do block through
# typechecking (a CSletrec, which the reader once returned as a letseq);
# the dump shows the statement and the package prints as the dump does
( cd work/defaults && $BSC -check-only -dliftdicts=Defaults.lift Defaults.bs ) > work/defaults.log 2>&1
check "LET-STATEMENT-IN Defaults -dliftdicts (class default method)" \
    grep -q 'do { let {' work/defaults/Defaults.lift
sed -e '1d' -e '/^-----$/,$d' work/defaults/Defaults.lift | sed -e '$d' > work/defaults/Defaults.lift.body
$DUMPBO work/defaults/Defaults.bcheck | sed -e '1d' -e '/^Lifted dictionaries: $/,$d' > work/defaults/Defaults.bcheck.txt
check "SAME-PACKAGE dumpbo Defaults.bcheck, -dliftdicts" \
    cmp -s work/defaults/Defaults.lift.body work/defaults/Defaults.bcheck.txt

# a second compile writes the same bytes
cp work/one/A.bcheck work/one/A.bcheck.first
( cd work/one && $BSC -check-only A.bs ) >> work/one.log 2>&1
check "IDENTICAL A.bcheck (recompiled)" cmp -s work/one/A.bcheck.first work/one/A.bcheck

# the .bcheck keeps real positions, so the same source compiled in two
# directories writes the same bytes only when -remap-path-prefix maps both
# directories to one prefix; without it the bytes name the directory
( cd work/remap/a && $BSC -check-only -remap-path-prefix "$PWD=SRC" A.bs ) > work/remap-a.log 2>&1
( cd work/remap/b && $BSC -check-only -remap-path-prefix "$PWD=SRC" A.bs ) > work/remap-b.log 2>&1
check "IDENTICAL A.bcheck (two directories, -remap-path-prefix)" \
    cmp -s work/remap/a/A.bcheck work/remap/b/A.bcheck
check "NO-DIRECTORY-IN A.bcheck (-remap-path-prefix)" \
    sh -c '! grep -qa "work/remap" work/remap/a/A.bcheck'
( cd work/sub/deeper && $BSC -check-only A.bs ) > work/sub.log 2>&1
check "DIFFER A.bcheck (two directories, no -remap-path-prefix)" \
    sh -c '! cmp -s work/one/A.bcheck work/sub/deeper/A.bcheck'
check "DIRECTORY-IN A.bcheck" sh -c 'grep -qa "work/one" work/one/A.bcheck'

# a body-only edit leaves the .bi unchanged and changes the .bcheck; an
# interface edit changes both (the .bcheck compared under one mapping)
( cd work/two && $BSC -check-only -remap-path-prefix "$PWD=SRC" A.bs ) > work/two.log 2>&1
( cd work/three && $BSC -check-only -remap-path-prefix "$PWD=SRC" A.bs ) > work/three.log 2>&1
check "IDENTICAL A.bi (body-only edit)" cmp -s work/one/A.bi work/two/A.bi
check "DIFFER A.bcheck (body-only edit)" sh -c '! cmp -s work/remap/a/A.bcheck work/two/A.bcheck'
check "DIFFER A.bi (interface edit)" sh -c '! cmp -s work/one/A.bi work/three/A.bi'
check "DIFFER A.bcheck (interface edit)" sh -c '! cmp -s work/remap/a/A.bcheck work/three/A.bcheck'

# the .bi of the same source compiled in another directory, by a relative
# and by an absolute path, is the same bytes and names no directory
check "IDENTICAL A.bi (other directory)" cmp -s work/one/A.bi work/sub/deeper/A.bi
( cd work/sub/deeper && rm -f A.bi A.bcheck && $BSC -signature-only "$PWD/A.bs" ) > work/sub-abs.log 2>&1
check "IDENTICAL A.bi (absolute path)" cmp -s work/one/A.bi work/sub/deeper/A.bi
check "NO-DIRECTORY-IN A.bi" sh -c '! grep -qa "work/one" work/one/A.bi'

# a full compile is unaffected by a .bi on the path: the .bo of A and of B
# are the same bytes with and without A.bi present
(
    cd work/full
    $BSC A.bs && mv A.bo A.bo.plain
    $BSC -signature-only A.bs
    $BSC A.bs
    $BSC B.bs && mv B.bo B.bo.with-bi
    rm -f A.bi
    $BSC B.bs
) > work/full.log 2>&1
check "IDENTICAL A.bo (A.bi present)" cmp -s work/full/A.bo work/full/A.bo.plain
check "IDENTICAL B.bo (A.bi present or absent)" cmp -s work/full/B.bo work/full/B.bo.with-bi

# the signature a .bi holds is the signature its .bo holds
$DUMPBO -bi work/full/A.bo > work/full/A.bo.sig 2>&1
$DUMPBO -bi work/one/A.bi > work/one/A.bi.sig.txt 2>&1
check "SAME-SIGNATURE dumpbo -bi A.bo A.bi" cmp -s work/full/A.bo.sig work/one/A.bi.sig.txt
# and the .bi holds the names of the import closure
check "DEPENDS A.bi Prelude PreludeBSV" \
    sh -c "$DUMPBO work/one/A.bi | grep -q '^Depends: Prelude PreludeBSV\$'"

exit $status
