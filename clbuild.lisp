;;; Short script for the Lisp side of clbuild:
;;; Argument parsing and application startup.
;;;
;;; Part of clbuild by Luke Gorrie and contributors:
;;;   Luke Gorrie <luke@member.fsf.org>
;;;   Anthony Chaumas-Pellet <achaumas@wispery.info>
;;;   Christophe Rhodes <csr21@cantab.net>
;;;   David Lichteblau <david@lichteblau.com>
;;;   Eric Marsden <eric.marsden@free.fr>

(defpackage :clbuild
  (:use :cl))

(in-package :clbuild)

(defparameter *raw-args*
  #+sbcl (cdr sb-ext:*posix-argv*)
  #-sbcl (error "not implemented"))
(defparameter *cmd* (car *raw-args*))
(defparameter *args*
  (loop
     with args = (cdr *raw-args*)
     while args
     for arg = (pop args)
     if (eql (mismatch arg "--") 2)
     collect (intern (string-upcase (subseq arg 2)) :keyword) into keys
     and collect (pop args) into keys
     else collect arg into normals
     finally (return (append normals keys))))

(push (intern (string-upcase *cmd*) :clbuild) *features*)

(defun quit (rc)
  #+sbcl (sb-ext:quit :unix-status rc)
  #-sbcl (error "not implemented"))

(defun namify (arg)
  (if (listp arg)
      (car arg)
      arg))

(defun &p (x)
  (find x '(&key &optional)))

(defmacro with-application ((&rest args) &body body)
  (let* ((keypos (position '&key args))
	 (non-keys (mapcar #'namify (if keypos (subseq args 0 keypos) args)))
	 (keys (when keypos (mapcar #'namify (subseq args (1+ keypos))))))
    `(let ((non-keys ',non-keys)
	   (keys ',keys))
       #+sbcl (declare (optimize sb-ext:inhibit-warnings))
       (flet ((usage ()
		(format t "Usage: clbuild ~A~:[~; [KEYS]~]~{ ~(~A~)~}~%"
			*cmd* keys non-keys)
		(when keys
		  (format t "Optional keys:~{ --~(~A~) VALUE~}~%"
			  keys))))
	 (when (find (car *args*) '("help" "--help" "-help" "-h" "-H")
		     :test #'equal)
	   (usage)
	   (quit 0))
	 (handler-case
	     (destructuring-bind ,args *args*
	       (declare (ignore ,@(mapcar #'namify (remove-if #'&p args)))))
	   (error ()
	     (format t "Error: Invalid command line arguments.~%~%")
	     (usage)
	     (quit 1)))) 
       (destructuring-bind ,args
	   *args*
	 ,@body)
       (quit 0))))

;; helper function instead of REQUIRE, for the benefit of non-SBCL lisps
(defun make (system)
  (asdf:operate 'asdf:load-op system))

(defun make-clim (system)
  #-(or :clim-graphic-forms :gtkairo)
  (unless (find-package :xlib)
    (asdf:operate 'asdf:load-op :clx))
  (make :mcclim)
  (make system))

(declaim (optimize sb-ext:inhibit-warnings))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hunchentoot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::hunchentoot
#+clbuild::hunchentoot

(make :hunchentoot-test)

(with-application (&key (port "4242"))
  (setf port (parse-integer port))
  (setf tbnl:*catch-errors-p* nil)
  (hunchentoot:start-server :port port)
  (format t "~%Webserver is running at ~
               http://localhost:~D/hunchentoot/test~%~
             Type RET to quit~%"
	  port)
  (read-line))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WebDAV
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::webdav
#+clbuild::webdav

(make :cl-webdav)

(with-application (directory &key (port "4242"))
  (setf port (parse-integer port))
  (setf tbnl:*catch-errors-p* nil)
  (setf dav:*file-resource-base-path-namestring* directory)
  (push (dav:create-dav-dispatcher 'dav:file-resource) tbnl:*dispatch-table*)
  (hunchentoot:start-server :port port)
  (format t "~%WebDAV server for ~A is running at http://localhost:~D/~%"
	  directory
	  port)
  (format t "Type RET to quit~%")
  (read-line))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eclipse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::eclipse
#+clbuild::eclipse

(unless (find-package :eclipse)
  (make :eclipse))

;;; (eclipse-system:compile-themes \"source/eclipse/themes/microGUI/\"
;;; 			       \"source/eclipse/themes/Step/\"
;;; 			       \"source/eclipse/themes/brushed-metal/\"
;;; 			       \"source/eclipse/themes/CoolClean/\")

(with-application (&optional display)
  (defparameter cl-user::*eclipse-initfile* ".eclipse.lisp")
  (defparameter cl-user::*eclipse-eclipsedir* nil)
  (load (compile-file "source/eclipse/lib/clx-ext/event.lisp"))
  (apply #'eclipse:eclipse
	 (when display
	   (list :display (parse-integer display)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLIM Listener
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::listener
#+clbuild::listener

(make-clim :clim-listener)

(with-application ()
  (clim-listener:run-listener))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gsharp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::gsharp
#+clbuild::gsharp

(make-clim :gsharp)

(with-application ()
  (gsharp::gsharp))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; demo demo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::demodemo
#+clbuild::demodemo

(make-clim :clim-examples)

(with-application ()
  (clim-demo::demodemo))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; climacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::climacs
#+clbuild::climacs

(make-clim :climacs)

(with-application ()
  (climacs:climacs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; closure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::closure
#+clbuild::closure

(make-clim :climacs)

(with-application (&optional url)
  (setf gui:*home-page* url)
  (clim-user::run-closure :new-process nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; beirc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::beirc
#+clbuild::beirc

(make-clim :beirc)

(with-application ()
  (beirc:beirc :new-process nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; climplayer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::climplayer
#+clbuild::climplayer

(make-clim :climplayer)

(with-application ()
  (climplayer:climplayer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parse-xml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::parse-xml
#+clbuild::parse-xml

(make :cxml)

(with-application (filename)
  (handler-case
      (cxml:parse (pathname filename) nil)
    (error (c)
      (format t "~A~%" c)
      (quit 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; validate-xml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::validate-xml
#+clbuild::validate-xml

(make :cxml)

(with-application (filename)
  (handler-case
      (cxml:parse (pathname filename) nil :validate t)
    (error (c)
      (format t "~A~%" c)
      (quit 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; validate-relax-ng
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::validate-relax-ng
#+clbuild::validate-relax-ng

(make :cxml-rng)

(with-application (xml-filename rng-filename &key (compact "no"))
  (handler-case
      (cxml:parse (pathname xml-filename)
		  (cxml-rng:make-validator
		   (cond
		     ((equal compact "yes")
		       (cxml-rng:parse-compact (pathname rng-filename)))
		     ((equal compact "no")
		       (cxml-rng:parse-schema (pathname rng-filename)))
		     (t
		      (error "invalid compact value, must be yes or no")))))
    (error (c)
      (format t "~A~%" c)
      (quit 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; html-to-xhtml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::html-to-xhtml
#+clbuild::html-to-xhtml
#+clbuild::html-to-xhtml

(make :closure-html)
(make :cxml)

(with-application (html-filename output-filename)
  (with-open-file (out output-filename
		       :element-type '(unsigned-byte 8)
		       :if-exists :error
		       :direction :output)
    (chtml:parse (pathname html-filename)
		 (cxml:make-octet-stream-sink out))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xhtml-to-html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clbuild::xhtml-to-html
#+clbuild::xhtml-to-html
#+clbuild::xhtml-to-html

(make :closure-html)
(make :cxml)

(with-application (xml-filename output-filename)
  (with-open-file (out output-filename
		       :element-type '(unsigned-byte 8)
		       :if-exists :error
		       :direction :output)
    (cxml:parse (pathname xml-filename)
		(chtml:make-octet-stream-sink out))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shouldn't get here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(error "clbuild.lisp fell through")
