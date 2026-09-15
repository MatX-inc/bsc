# Sourceable helper: export a .bir for every .ba of the design EXCEPT
# the top's.
#
#   frags_sub <top>
#
# The caller exports the top itself, because that export carries the
# flags the design needs (--bdpi and friends) and this one must not
# guess at them.
#
# bsc writes a .ba per module it elaborated and per `import "BDPI"' it
# saw, and trs-bir writes one .bir per .ba whichever kind it is, so a
# design is a set of files rather than one.  The link follows the
# instantiations and the imports out of the top, finding each by name
# beside it, so all this has to do is make sure they are all there.
#
# Other designs' tops, which a source file declaring several sys*
# modules leaves lying in the same directory, are skipped: by name, and
# then by trying the export and keeping what succeeds.
#
# Expects $TRSBIR to name the exporter.
frags_sub() {
    fr_top=$1
    for fr_f in *.ba; do
        [ -e "$fr_f" ] || continue
        fr_m=${fr_f%.ba}
        [ "$fr_m" = "$fr_top" ] && continue
        case $fr_m in sys*) continue;; esac
        $TRSBIR "$fr_f" >/dev/null 2>&1 || continue
    done
}
