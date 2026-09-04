# Sourceable helper: export a .bir for every synthesized module of the
# design EXCEPT the top.
#
#   frags_sub <top>
#
# The caller exports the top itself, because that export carries the
# flags the design needs (--bdpi and friends) and this one must not
# guess at them.
#
# bsc writes a .ba per module it elaborated and trs-bir writes one .bir
# per .ba, so a design is a set of files rather than one.  The link
# follows the instantiations out of the top, finding each by module name
# beside it, so all this has to do is make sure they are all there.
#
# A .ba that is not a module -- the one an `import "BDPI"` produces --
# is skipped by trying the export and keeping what succeeds.  So are
# other designs' tops, which a source file declaring several sys*
# modules leaves lying in the same directory.
#
# Expects $TRSBIR to name the exporter.
frags_sub() {
    fr_top=$1
    for fr_f in *.ba; do
        [ -e "$fr_f" ] || continue
        fr_m=${fr_f%.ba}
        [ "$fr_m" = "$fr_top" ] && continue
        case $fr_m in sys*) continue;; esac
        $TRSBIR "$fr_m" >/dev/null 2>&1 || continue
    done
}
