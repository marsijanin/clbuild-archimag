#!/bin/sh
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

get_darcs() {
    name="$1"
    url="$2"

    # don't use tail_last, since darcs already has this kind of progress bar
    if [ -d $name ]; then
	dribble_get "darcs pull" $name
	(cd $name; darcs pull --all)
    else
	dribble_get "darcs get" $name
	darcs get $url $name
    fi
}

get_git() {
    name="$1"
    url="$2"

    if [ -d $name ]; then
	dribble_get "git pull" $name
	(cd $name; git pull)
    else
	dribble_get "git clone" $name
	git clone $url $name
    fi
}

get_svn() {
    name="$1"
    url="$2"

    dribble_get "svn co" $name

    svn co $url $name | tail_last
}

get_cvs() {
    module="$1"
    repository="$2"

    dribble_get "cvs co" $module

    cvs -d $repository co $module | tail_last
}

get_tarball() {
    name="$1"
    url="$2"
    flags="${3:-z}"

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

if test $# -ne 1; then
    exec 1>&2
    echo error: invalid number of arguments
    echo usage: ./update.sh PROJECT_NAME
    exit 1
fi

case $1 in
    flexichain|mcclim|zip|cxml|closure|gsharp|climacs|slime|beirc|eclipse)
	get_cvs_clnet $1
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

    cl-webdav)
	# not in the darcs mirror yet
	get_tarball $1 http://weitz.de/files/$1.tar.gz
	;;

    clx)
	get_darcs clx http://common-lisp.net/~crhodes/clx
	;;

    cffi)
	#get_darcs cffi http://common-lisp.net/project/cffi/darcs/cffi/
	get_darcs cffi http://common-lisp.net/~loliveira/darcs/cffi-newtypes/
	;;
	
    bordeaux-threads)
	get_darcs bordeaux-threads \
	    http://common-lisp.net/project/bordeaux-threads/darcs/bordeaux-threads/
	;;

    climplayer)
	get_darcs climplayer \
	    http://common-lisp.net/project/climplayer/darcs/climplayer/
	;;

    closer-mop)
	get_darcs closer-mop \
	    http://common-lisp.net/project/closer/repos/closer-mop/
	;;

    lw-compat)
	get_darcs lw-compat \
	    http://common-lisp.net/project/closer/repos/lw-compat/
	;;

    closure-html)
	get_git closure-html git://repo.or.cz/closure-html.git
	;;

    cxml-rng|cxml-stp)
	get_git $1 http://www.lichteblau.com/git/$1.git
	;;

    skippy)
	get_tarball skippy http://www.xach.com/lisp/skippy.tgz
	;;

    salza)
	get_tarball salza http://www.xach.com/lisp/salza/salza-0.7.2.tar.gz
	;;

    midi)
	get_tarball midi http://doc.gold.ac.uk/isms/lisp/midi/midi.tar.gz
	;;

    puri|md5|cl-base64)
	get_git $1 git://git.b9.com/$1.git
	;;

    spatial-trees)
	get_tarball spatial-trees \
	    http://ftp.linux.org.uk/pub/lisp/cclan/spatial-trees.tar.gz
	;;

    trivial-sockets)
	get_tarball trivial-sockets \
	    http://ftp.linux.org.uk/pub/lisp/cclan/trivial-sockets.tar.gz
	;;
	
    split-sequence)
	get_tarball split-sequence \
	    http://ftp.linux.org.uk/pub/lisp/cclan/split-sequence.tar.gz
	    ;;

    rfc2388)
	get_tarball rfc2388 \
	    http://common-lisp.net/project/rfc2388/rfc2388_latest.tar.gz
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
