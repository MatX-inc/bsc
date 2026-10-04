#!/usr/bin/env sh
# Build the same sources from two different directories with
# -remap-path-prefix and check that every output is byte-identical and
# free of the build directories' absolute paths.
set -e

BSC=${BSC:-bsc}

rm -rf dirA dirB dirC
mkdir -p dirA dirB
for d in dirA dirB; do
    cp Foo.bsv Bar.bsv "$d"/
done

for d in dirA dirB; do
    (
        cd "$d"
        # one compile via -u (exercises the import/dependency path),
        # one elaboration and schedule to .bmod/.bsched
        $BSC -remap-path-prefix "$PWD=." -u Bar.bsv
        $BSC -remap-path-prefix "$PWD=." -sim -g mkFoo Foo.bsv
    ) > "$d.log" 2>&1
done

status=0
for f in Foo.bo Bar.bo mkFoo.bmod mkFoo.bsched; do
    if cmp -s dirA/"$f" dirB/"$f"; then
        echo "IDENTICAL $f"
    else
        echo "DIFFER $f"
        status=1
    fi
done

# no residual build-directory path may survive in any output
for f in dirA/Foo.bo dirA/Bar.bo dirA/mkFoo.bmod dirA/mkFoo.bsched; do
    if grep -qF "$(pwd)/dirA" "$f"; then
        echo "RESIDUAL-PATH $f"
        status=1
    else
        echo "CLEAN $f"
    fi
done

# repeatability on one machine: a re-run in place is byte-identical
( cd dirA && $BSC -remap-path-prefix "$PWD=." -sim -g mkFoo Foo.bsv ) \
    > dirA-rerun.log 2>&1
for f in mkFoo.bmod mkFoo.bsched; do
    if cmp -s dirA/"$f" dirB/"$f"; then
        echo "REPEATABLE $f"
    else
        echo "NOT-REPEATABLE $f"
        status=1
    fi
done

# absolute-path invocation: the stored source name must be remapped
# cleanly, with no invented trailing separator (a marker-free path must
# not go through the /// pwd-marker decoder)
mkdir -p dirC
cp Foo.bsv Bar.bsv dirC/
(
    cd dirC
    $BSC -remap-path-prefix "$PWD=." -u Bar.bsv
    $BSC -remap-path-prefix "$PWD=." -sim -g mkFoo "$PWD/Foo.bsv"
) > dirC.log 2>&1
for f in mkFoo.bmod mkFoo.bsched; do
    if grep -qa "\./Foo.bsv/" dirC/"$f"; then
        echo "TRAILING-SLASH dirC/$f"
        status=1
    else
        echo "NO-TRAILING-SLASH dirC/$f"
    fi
    if grep -qa "$(pwd)/dirC" dirC/"$f"; then
        echo "RESIDUAL-PATH dirC/$f"
        status=1
    else
        echo "CLEAN dirC/$f"
    fi
    # the absolute invocation must produce the same bytes as the relative one
    if cmp -s dirC/"$f" dirA/"$f"; then
        echo "IDENTICAL-ABS $f"
    else
        echo "DIFFER-ABS $f"
        status=1
    fi
done

exit $status
