#!/bin/bash
#
# Simple script to download latest versions of all the most important
# Common Lisp projects.
#
# Part of clbuild by Luke Gorrie and contributors:
#   Luke Gorrie <luke@member.fsf.org>
#   Anthony Chaumas-Pellet <achaumas@wispery.info>
#   Christophe Rhodes <csr21@cantab.net>
#   David Lichteblau <david@lichteblau.com>
#   Eric Marsden <eric.marsden@free.fr>

if ! test -f ../clbuild; then
    echo 1>&2
    echo error: update.sh called outside of clbuild
    echo '(use "clbuild update PROJECT_NAME" to download individual projects)'
    exit 1
fi

blank_line="                                                                  "
tail_last() {
    if tty 0>&1 >/dev/null; then
	while read line; do
	    echo -e '\r\c'
	    echo -n "$blank_line"
	    echo -e '\r\c'
	    echo -n $line | cut -b 1-65 | tr -d '\n'
	done
	echo -e '\r\c'
	echo -n "$blank_line"
	echo -e '\r\c'
    else
	while read line; do
	    echo $line
	done
    fi
}

dribble_get() {
    label="$1"
    name="$2"

    if test -d `echo ${name}*/ | awk '{print $1;}'`; then
	echo -n "UPDATE "
    else
	echo -n "NEW "
    fi
    echo "$label $name"
}

dry_run_ok() {
   if test -n "$dry_run"; then
       echo "OK: $1"
   fi
}

dry_run_missing() {
   if test -n "$dry_run"; then
       echo "MISSING: $1"
   fi
}

get_darcs() {
    name="$1"
    url="$2"

    if [ -d $name ]; then
	actual="`cat $name/_darcs/prefs/defaultrepo`"
	if test "x$actual" = "x$url"; then
	    dry_run_ok $1
	else
	    echo "MISMATCH: $1 was installed from $actual, current is $url"
	fi
    else
	dry_run_missing $1
    fi
    if test -n "$dry_run"; then
	exit 0
    fi

    # don't use tail_last, since darcs already has this kind of progress bar
    if [ -d $name ]; then
	dribble_get "darcs pull" $name
	(
	    cd $name
	    if ! test -d _darcs; then
		echo ERROR: not a darcs repository
		exit 1
	    fi
	    darcs pull --all
	    )
    else
	dribble_get "darcs get" $name
	darcs get $url $name
    fi
}

get_git() {
    name="$1"
    url="$2"

    if [ -d $name ]; then
	actual="`cd $name && git config --get remote.origin.url`"
	if test "x$actual" = "x$url"; then
	    dry_run_ok $1
	else
	    echo "MISMATCH: $1 was installed from $actual, current is $url"
	fi
    else
	dry_run_missing $1
    fi
    if test -n "$dry_run"; then
	exit 0
    fi

    if [ -d $name ]; then
	dribble_get "git pull" $name
	(
	    cd $name
	    if ! test -d .git; then
		echo ERROR: not a darcs repository
		exit 1
	    fi
	    git pull
	    )
    else
	dribble_get "git clone" $name
	git clone $url $name
    fi
}

get_svn() {
    name="$1"
    url="$2"

    if [ -d $name ]; then
	actual="`cd $name && svn info | grep ^URL: | awk '{print $2;}'`"
	if test "x$actual" = "x$url"; then
	    dry_run_ok $1
	else
	    echo "MISMATCH: $1 was installed from $actual, current is $url"
	fi
    else
	dry_run_missing $1
    fi
    if test -n "$dry_run"; then
	exit 0
    fi

    dribble_get "svn co" $name

    svn co $url $name | tail_last
}

get_cvs() {
    module="$1"
    repository="$2"

    if [ -d $module ]; then
	actual="`cat $module/CVS/Root`"
	if test "x$actual" = "x$repository"; then
	    dry_run_ok $1
	else
	    echo "MISMATCH: $1 was installed from $actual, current is $repository"
	fi
    else
	dry_run_missing $1
    fi
    if test -n "$dry_run"; then
	exit 0
    fi

    dribble_get "cvs co" $module

    cvs -d $repository co $module | tail_last
}

get_tarball() {
    name="$1"
    url="$2"
    flags="${3:-z}"

    echo "Warning: Using deprecated method get_tarball."

    if test -n "$dry_run"; then
	if ls -d ${name}* >/dev/null; then
	    directories="`ls -d ${name}*`"
	    echo "TARBALL: $directories installed from a tarball, cannot check"
	else
	    dry_run_missing $1
	fi
	exit 0
    fi

    tmp=$TMPDIR/${name}.tar.gz

    dribble_get wget $name

    # We used to delete old directories completely here.  Unfortunately,
    # that can easily lead to loss of changes, a problem all the VC checkout
    # method do not have to the extent.  Save all old data instead.  Users
    # can delete the resulting backup directories easily enough themselves.
    if ls ${name}* >/dev/null; then
	echo got here
	backup=`mktemp -d backup_${name}_XXXXXXXXXX`
	mv ${name}* "$backup/"
    fi
    wget \
	--no-check-certificate \
	--progress=dot \
	-O "$tmp" \
	$url \
	2>&1 | tail_last
    tar v${flags}xf "$tmp" | tail_last
    rm "$tmp"
}

get_svn_clnet() {
    name="$1"
    path="$2"

    get_svn $name svn://common-lisp.net/project/$name/svn/$2
}

get_cvs_clnet() {
    module="$1"
    project="${2:-$1}"

    get_cvs $module ${CLNET_USER}@common-lisp.net:/project/$project/cvsroot
}

get_ediware() {
    get_darcs $1 http://common-lisp.net/~loliveira/ediware/$1
}

get_tarball_bz2() {
    get_tarball "$1" "$2" j
}

if test x$1 = x--dry-run; then
    shift
    dry_run=1
else
    unset dry_run
fi

if test $# -ne 1; then
    exec 1>&2
    echo error: invalid number of arguments
    echo usage: ./update.sh [--dry-run] PROJECT_NAME
    exit 1
fi

case $1 in
    flexichain|mcclim|zip|cxml|closure|gsharp|climacs|slime|beirc|eclipse)
	get_cvs_clnet $1
	;;

    alexandria)
	get_darcs alexandria \
	    http://common-lisp.net/project/alexandria/darcs/alexandria
	;;

    trivial-features)
	get_darcs trivial-features \
	    http://common-lisp.net/~loliveira/soc07/trivial-features
	;;

    cl+ssl)
	get_cvs_clnet cl+ssl cl-plus-ssl
	;;

    closure-common)
	get_cvs_clnet closure-common cxml
	;;

    trivial-gray-streams)
	get_cvs_clnet trivial-gray-streams cl-plus-ssl
	;;

    usocket)
	get_svn_clnet usocket usocket/trunk
	;;

    cl-irc)
	get_svn_clnet cl-irc trunk
	;;

    cl-ppcre|flexi-streams|cl-fad|hunchentoot|chunga|url-rewrite|cl-who|drakma)
	get_ediware $1
	;;

    clx)
	get_darcs clx http://common-lisp.net/~crhodes/clx
	;;

    babel)
	get_darcs babel http://common-lisp.net/~loliveira/soc07/babel
	;;

    cffi)
	#get_darcs cffi http://common-lisp.net/project/cffi/darcs/cffi/
	get_darcs cffi http://common-lisp.net/~loliveira/soc07/cffi+grovel+babel+stuff
	;;
	
    bordeaux-threads)
	get_darcs bordeaux-threads \
	    http://common-lisp.net/project/bordeaux-threads/darcs/bordeaux-threads
	;;

    climplayer)
	get_darcs climplayer \
	    http://common-lisp.net/project/climplayer/darcs/climplayer
	;;

    closer-mop)
	get_darcs closer-mop \
	    http://common-lisp.net/project/closer/repos/closer-mop
	;;

    lw-compat)
	get_darcs lw-compat \
	    http://common-lisp.net/project/closer/repos/lw-compat
	;;

    closure-html)
	get_git closure-html git://repo.or.cz/closure-html.git
	;;

    cxml-rng|cxml-stp)
	get_git $1 http://www.lichteblau.com/git/$1.git
	;;

    midi)
	get_darcs midi http://rvw.doc.gold.ac.uk/sullivan/darcs/midi
	;;

    puri|md5|cl-base64)
	get_git $1 http://git.b9.com/$1.git
	;;

    spatial-trees)
	get_darcs spatial-trees \
		http://rvw.doc.gold.ac.uk/sullivan/darcs/spatial-trees
	;;

    cl-webdav|skippy|salza|trivial-sockets|split-sequence|rfc2388)
	get_darcs $1 http://common-lisp.net/project/clbuild/mirror/$1
	;;

    graphic-forms)
	get_svn_clnet graphic-forms trunk
	;;

    --help|help|-help)
	echo usage: ./update.sh PROJECT_NAME
	exit 0
	;;

    *)
	echo Error: cannot download unknown project $1
	exit 1
esac
