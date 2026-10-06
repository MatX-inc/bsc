#! /usr/bin/env bash
set -euo pipefail

PACKAGES=$@

# ===========================================================================

# Implement associative arrays, in case the shell is not bash 4+

## ainit STEM
## Declare an empty associative array named STEM.
ainit () {
  eval "__aa__${1}=' '"
}

## akeys STEM
## List the keys in the associatve array named STEM.
akeys () {
  eval "echo \"\$__aa__${1}\""
}

## aget STEM KEY VAR
## Set VAR to the value of KEY in the associative array named STEM.
## If KEY is not present, unset VAR.
aget () {
  eval "unset $3
        case \$__aa__${1} in
          *\" $2 \"*) $3=\$__aa__${1}__$2;;
        esac"
}

## aset STEM KEY VALUE
## Set KEY to VALUE in the associative array named STEM.
aset () {
  eval "__aa__${1}__${2}=\$3
        case \$__aa__${1} in
          *\" $2 \"*) :;;
          *) __aa__${1}=\"\${__aa__${1}}$2 \";;
        esac"
}

## aunset STEM KEY
## Remove KEY from the associative array named STEM.
aunset () {
  eval "unset __aa__${1}__${2}
        case \$__aa__${1} in
          *\" $2 \"*) __aa__${1}=\"\${__aa__${1}%% $2 *} \${__aa__${1}#* $2 }\";;
        esac"
}

# ===========================================================================

ainit arr_id
ainit arr_ver
ainit arr_lic
ainit arr_copyr
ainit arr_notice
ainit arr_deps

# Horizontal delimiter between packages in the output
#
DELIM='-------------------------'

# Regex for removing the hash from names like
#   syb-0.7.2.4-FBa2dfZrzzu7owkvhCx23j
# Package names can include capitals and digits (OneTuple, text-iso8601).
# Keep name-version in capture group 1 for the callers below.
#
STRIP_HASH_REGEX='^([-[:alnum:]]+-[[:digit:]]+([.][[:digit:]]+)*)-[[:alnum:]]{4,}$'

# Function to add a package to the database and follow its dependencies
#
add_pkg() {
    local PKG_ID
    local PKG_NAME
    local PKG_VER
    local PKG_LIC
    local PKG_COPYR
    local PKG_NOTICE
    local PKG_HTML_DIR
    local PKG_LICENSE_FILE
    local PKG_DEPS

    #echo "Looking up $1"

    PKG_ID=`ghc-pkg field $1 id --simple-output`

    if [[ ${PKG_ID} =~ ${STRIP_HASH_REGEX} ]] ; then
	#echo "stripping ${PKG_ID} => ${BASH_REMATCH[1]}"
	PKG_NAME=${BASH_REMATCH[1]}
    else
	PKG_NAME=${PKG_ID}
    fi

    PKG_NAME=`echo "${PKG_NAME}" | tr .- _`

    aget arr_id "${PKG_NAME}" i_id
    if [ -z ${i_id+x} ] ; then
	PKG_VER=`ghc-pkg field $1 version --simple-output`
	PKG_LIC=`ghc-pkg field $1 license --simple-output`
	PKG_COPYR=`ghc-pkg field $1 copyright --simple-output`
	PKG_DEPS=`ghc-pkg field $1 depends --simple-output`

	case "${PKG_LIC}" in
	    BSD-3-Clause|BSD-2-Clause|MIT|ISC) ;;
	    *)
		echo "Unexpected license for ${PKG_ID}: ${PKG_LIC}"
		exit 1
		;;
	esac

	# Cabal installs LICENSE alongside the Haddock directory even when
	# documentation is not built. Preserve its copyright notice when
	# available: package metadata can omit years or contact details.
	PKG_NOTICE=""
	case "${PKG_LIC}" in
	    MIT|ISC)
		PKG_HTML_DIR=`ghc-pkg field "$1" haddock-html --simple-output`
		case "${PKG_HTML_DIR}" in
		    */html)
			PKG_LICENSE_FILE="${PKG_HTML_DIR%/html}/LICENSE"
			if [ -r "${PKG_LICENSE_FILE}" ]; then
			    PKG_NOTICE=`sed -n '/^[Cc]opyright/,/^$/p' "${PKG_LICENSE_FILE}"`
			fi
			;;
		esac
		;;
	esac

	aset arr_id ${PKG_NAME} "${PKG_ID}"
	aset arr_ver ${PKG_NAME} "${PKG_VER}"
	aset arr_lic ${PKG_NAME} "${PKG_LIC}"
	aset arr_copyr ${PKG_NAME} "${PKG_COPYR}"
	aset arr_notice ${PKG_NAME} "${PKG_NOTICE}"
	aset arr_deps ${PKG_NAME} "${PKG_DEPS}"

	for dep in ${PKG_DEPS}
	do
	    #echo "Following dep: $dep"
	    if [[ ${dep} =~ ${STRIP_HASH_REGEX} ]] ; then
		#echo "stripping ${dep} => ${BASH_REMATCH[1]}"
		dep=${BASH_REMATCH[1]}
	    fi
	    add_pkg "${dep}"
	done
    fi
}

# Add the packages from the command line (and their dependencies)
for i in ${PACKAGES}
do
    add_pkg "$i"
done

# Generate the output, starting with a delimiter
echo $DELIM

# For each package in the database
keys=$(akeys arr_id)
sorted_keys=`echo ${keys} | tr ' ' '\012' | sort | tr '\012' ' '`
for i in ${sorted_keys}
do
    aget arr_id $i i_id
    aget arr_ver $i i_ver
    aget arr_lic $i i_lic
    aget arr_copyr $i i_copyr
    aget arr_notice $i i_notice

    # Because the package name was mangled to make the assoc array key
    # re-construct it from the ID

    if [[ ${i_id} =~ ${STRIP_HASH_REGEX} ]] ; then
	pkg=${BASH_REMATCH[1]}
    else
	pkg=${i_id}
    fi

    # And then strip the version number

    STRIP_VER_REGEX='^([-[:alnum:]]+)-[[:digit:]]+([.][[:digit:]]+)*$'
    if [[ $pkg =~ ${STRIP_VER_REGEX} ]] ; then
	pkg=${BASH_REMATCH[1]}
    fi

    echo
    echo "package: $pkg"
    #echo "id: ${i_id}"
    echo "version: ${i_ver}"
    echo "license: ${i_lic}"
    if [[ -n "${i_copyr}" ]]; then
	echo "copyright: ${i_copyr}"
    fi
    if [[ -n "${i_notice}" ]]; then
	printf '\n%s\n' "${i_notice}"
    fi

    # Keep the existing BSD metadata output unchanged. Include the complete
    # permission/disclaimer text for the newly supported licenses.
    case "${i_lic}" in
	MIT)
	    cat <<'EOF'

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
EOF
	    ;;
	ISC)
	    cat <<'EOF'

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
EOF
	    ;;
    esac
    echo
    echo $DELIM
done

# Done
exit 0
