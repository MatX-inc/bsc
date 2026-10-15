#! /usr/bin/env bash
set -euo pipefail

export TMOUT=1

# Input, from the environment: src/comp/Makefile exports NOGIT, and the
# cabal hooks pass it through (SetupHooks.hs, writeBuildVersionHs).
NOGIT=${NOGIT:-0}

# -------------------------
# GIT

# Arguments: 1=commit hash, 2=describe string
genGITBuildVersion () {
    echo "module BuildVersion(buildVersion, buildVersionNum, buildVersionName) where" > BuildVersion.hs.new;

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
	else

	    # Get the current commit hash
	    GITCOMMIT=`git show -s --format=%h HEAD`
	    if ! GITDESCR=`git describe --tags`; then
		GITDESCR="untagged-g${GITCOMMIT}"
	    fi

	fi
    fi
}

# -------------------------

# --name prints the version name alone and writes nothing: what the Bluesim
# kernel's version header carries (src/bluesim/Makefile).
if [ "${1:-}" = "--name" ] ; then
    getGITVersion
    echo "$GITDESCR"
    exit 0
fi

getGITVersion
genGITBuildVersion ${GITCOMMIT} ${GITDESCR}

# -------------------------
