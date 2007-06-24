get_cvs_clnet flexichain
get_cvs_clnet mcclim
get_cvs_clnet zip
get_cvs_clnet cxml
get_cvs_clnet closure
get_cvs_clnet gsharp
get_cvs_clnet climacs
get_cvs_clnet slime
get_cvs_clnet beirc

get_svn_clnet usocket usocket/trunk
get_svn_clnet cl-irc trunk

get_ediware cl-ppcre
get_ediware flexi-streams
get_ediware cl-fad

get_darcs clx http://common-lisp.net/~crhodes/clx
#get_darcs cffi http://common-lisp.net/project/cffi/darcs/cffi/
get_darcs cffi http://common-lisp.net/~loliveira/darcs/cffi-newtypes/
get_darcs bordeaux-threads \
    http://common-lisp.net/project/bordeaux-threads/darcs/bordeaux-threads/
get_darcs climplayer \
    http://common-lisp.net/project/climplayer/darcs/climplayer/
# closer-* (for graphic-forms)
get_darcs closer-mop http://common-lisp.net/project/closer/repos/closer-mop/
get_darcs lw-compat http://common-lisp.net/project/closer/repos/lw-compat/

get_tarball skippy http://www.xach.com/lisp/skippy.tgz
get_tarball salza http://www.xach.com/lisp/salza/salza-0.7.2.tar.gz
get_tarball puri http://files.b9.com/puri/puri-latest.tar.gz
get_tarball midi http://doc.gold.ac.uk/isms/lisp/midi/midi.tar.gz
get_tarball spatial-trees \
    http://ftp.linux.org.uk/pub/lisp/cclan/spatial-trees.tar.gz
get_tarball trivial-gray-streams \
    http://common-lisp.net/project/cl-plus-ssl/download/trivial-gray-streams.tar.gz
get_tarball trivial-sockets \
    http://ftp.linux.org.uk/pub/lisp/cclan/trivial-sockets.tar.gz
get_tarball split-sequence \
    http://ftp.linux.org.uk/pub/lisp/cclan/split-sequence.tar.gz

# old standalone tab-layout (for climplayer?)
get_tarball_bz2 tab-layout http://bl0rg.net/~mgr/flux/tab-layout_2005-09-19_02-52+0200.tar.bz2

# graphic-forms
get_svn_clnet graphic-forms trunk
# some .asd files are hidden:
ln -f -s $(pwd)/graphic-forms/src/external-libraries/*/*/*.asd ${system_dir}
