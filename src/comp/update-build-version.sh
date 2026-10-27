#! /usr/bin/env bash
set -euo pipefail

export TMOUT=1

# NOGIT comes from the environment (src/comp/Makefile, SetupHooks.hs).
NOGIT=${NOGIT:-0}

# -------------------------
# GIT

# Always inspect this source tree, even from an external build directory or
# a caller (such as a Git hook) with another repository in its environment.
root=$(cd "$(dirname "$0")/../.." && pwd)
repoGit () (
    unset $(git rev-parse --local-env-vars)
    git --git-dir="$root/.git" --work-tree="$root" "$@"
)

# The install stamp, HEAD and the version name, written when they change;
# make install and the hooks' version rule share it (SetupHooks.hs).
writeStamp () {
    if [ -e "$root/.git" ]; then
        head=$(repoGit rev-parse HEAD)
    else
        head=no-repository
    fi
    mkdir -p "$(dirname "$1")"
    printf '%s\n%s\n' "$head" "$GITDESCR" > "$1.tmp"
    if cmp -s "$1.tmp" "$1"; then
        rm -f "$1.tmp"
    else
        mv "$1.tmp" "$1"
    fi
}

# Arguments: 1=commit hash, 2=describe string
genGITBuildVersion () {
    echo "module BuildVersion(buildVersion, buildVersionNum, buildVersionName) where" > BuildVersion.hs.new;

    echo >> BuildVersion.hs.new;
    echo "import Warmup ()" >> BuildVersion.hs.new;

    echo >> BuildVersion.hs.new;
    echo buildVersion :: String >> BuildVersion.hs.new;
    echo buildVersion = \"$1\" >> BuildVersion.hs.new;

    echo >> BuildVersion.hs.new;
    echo buildVersionNum :: Integer >> BuildVersion.hs.new;
    echo buildVersionNum = 0x$1 >> BuildVersion.hs.new;

    echo >> BuildVersion.hs.new;
    echo buildVersionName :: String >> BuildVersion.hs.new;
    echo buildVersionName = \"$2\" >> BuildVersion.hs.new;

    if test -f BuildVersion.hs; then
	if !(diff BuildVersion.hs BuildVersion.hs.new); then
            mv BuildVersion.hs.new BuildVersion.hs;
	else
            echo "BuildVersion.hs up-to-date";
            rm BuildVersion.hs.new;
	fi;
    else
	mv BuildVersion.hs.new BuildVersion.hs;
    fi;
}

# Sets GITCOMMIT and GITDESCR: null values with NOGIT=1, the values git
# substituted on export for an archive (.gitattributes, export-subst), and
# the repository's otherwise.
getGITVersion () {
    if [ "$NOGIT" = 1 ] ; then
	GITCOMMIT="0000000"
	GITDESCR="no-git"
    else

	# If the source was exported (via 'git archive')
	if [[ '$Format:%%$' == "%" ]]; then
	    GITCOMMIT='$Format:%h$'
	    if [[ '$Format:%D$' =~ tag:\ ([^ ]+) ]]; then
		GITDESCR="${BASH_REMATCH[1]}"
	    else
		GITDESCR="archive-g${GITCOMMIT}"
	    fi
	elif [ ! -e "$root/.git" ]; then

	    # A tree without a repository, such as an unpacked sdist
	    GITCOMMIT="0000000"
	    GITDESCR="no-git"
	else

	    # Get the current commit hash
	    GITCOMMIT=`repoGit show -s --format=%h HEAD`
	    if ! GITDESCR=`repoGit describe --tags`; then
		GITDESCR="untagged-g${GITCOMMIT}"
	    fi

	fi
    fi
}

# -------------------------

# --module DIR writes DIR/BuildVersion.hs; --stamp FILE writes the install
# stamp; --name prints the version name. The hooks give the first two together.
getGITVersion
[ $# -gt 0 ] || { echo "usage: $0 [--module DIR] [--stamp FILE] [--name]" >&2; exit 2; }
while [ $# -gt 0 ]; do
    case "$1" in
        --module) (mkdir -p "$2" && cd "$2" && genGITBuildVersion "$GITCOMMIT" "$GITDESCR"); shift 2 ;;
        --stamp) writeStamp "$2"; shift 2 ;;
        --name) echo "$GITDESCR"; shift ;;
        *) echo "usage: $0 [--module DIR] [--stamp FILE] [--name]" >&2; exit 2 ;;
    esac
done

# -------------------------
