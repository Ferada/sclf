;;;  directory.lisp --- filesystem directory access

;;;  Copyright (C) 2006, 2007, 2008, 2009, 2010 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: sclf

#+cmu (ext:file-comment "$Module: directory.lisp $")

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 2.1
;;; of the License, or (at your option) any later version.
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;;; 02111-1307 USA


(cl:in-package :sclf)

(defun pathname-as-directory (pathname)
  "Converts PATHNAME to directory form and return it."
  (setf pathname (pathname pathname))
  (if (pathname-name pathname)
      (make-pathname :directory (append (or (pathname-directory pathname)
					    '(:relative))
					(list (file-namestring pathname)))
		     :name nil
		     :type nil
		     :defaults pathname)
      pathname))

(defun d+ (path &rest rest)
  "Concatenate directory pathname parts and return a pathname."
  (make-pathname :defaults path
		 :directory (append (pathname-directory path) rest)))

(defun delete-directory (pathname)
  "Remove directory PATHNAME.  Return PATHNAME."
  #+cmu (multiple-value-bind (done errno)
	     (unix:unix-rmdir (namestring pathname))
	   (unless done
	     (error "Unable to delete directory ~A (errno=~A)"
		    pathname errno)))
  #+sbcl (sb-posix:rmdir pathname)
  #+lispworks (lw:delete-directory pathname)
  #-(or cmu sbcl)
  (error "DELETE-DIRECTORY not implemented for you lisp system.")
  pathname)

(defun list-directory (pathname &key truenamep)
  "List content of directory PATHNAME.  If TRUENAMEP is true don't try
to follow symbolic links."
  #-(or sbcl cmu) (declare (ignore truenamep))
  (let (#+cmu (lisp::*ignore-wildcards* t))
    (directory (make-pathname :defaults (pathname-as-directory pathname)
			      :name :wild
			      :type :wild
			      :version :wild)
	       #+cmu :truenamep #+cmu truenamep
	       #+sbcl :resolve-symlinks #+sbcl truenamep)))

(defun traverse-directory-tree (root-pathname proc &key truenamep test depth-first)
  "Call PROC on all pathnames under ROOT-PATHNAME, both files and
directories.  Unless TRUENAMEP is true, this function doesn't try
to lookup the truename of files, as finding the truename may be a
superfluous and noxious activity expecially when you expect
broken symbolic links in your filesystem."
  (check-type root-pathname pathname)
  (check-type proc (or function symbol))
  (check-type test (or function symbol null))
  (labels ((ls (dir)
	     (declare (type pathname dir))
	     (list-directory dir :truenamep truenamep))
	   (traverse? (file)
	     (declare (type pathname file))
	     (and (not (pathname-name file))
		  (or truenamep
		      (not (symbolic-link-p file)))
		  (or (not test)
		      (funcall test file))))
	   (traverse-pre-order (dir)
	     (declare (type pathname dir))
	     (loop
		for file in (ls dir)
		do (funcall proc file)
		when (traverse? file)
		do (traverse-pre-order file)))
	   (traverse-post-order (dir)
	     (declare (type pathname dir))
	     (loop
		for file in (ls dir)
		when (traverse? file)
		do (traverse-post-order file)
		do (funcall proc file))))
    (if depth-first
	(traverse-post-order root-pathname)
	(traverse-pre-order root-pathname))
    (values)))

(defmacro do-directory-tree ((file root-pathname &key truenamep test depth-first) &body body)
  "Call TRAVERSE-DIRECTORY-TREE with BODY es procedure."
  `(traverse-directory-tree ,root-pathname
			    #'(lambda (,file)
				,@body)
			    :truenamep ,truenamep
			    :test ,test
			    :depth-first ,depth-first))

(defun empty-directory-p (pathname)
  (and (directory-p pathname)
       (endp (list-directory pathname))))

(defun remove-empty-directories (root)
  (do-directory-tree (pathname root :depth-first t)
    (when (empty-directory-p pathname)
      (delete-directory pathname))))

(defun map-directory-tree (pathname function)
  "Apply FUNCTION to every file in a directory tree starting from
PATHNAME.  Return the list of results."
  (be return-list '()
    (do-directory-tree (directory-entry pathname)
      (push (funcall function directory-entry) return-list))
    (nreverse return-list)))

(defun find-files (root-pathname matcher-function &key truenamep)
  "In the directory tree rooted at ROOT-PATHNAME, find files that
when the pathname is applied to MATCHER-FUNCTION will return
true.  Return the list of files found.  Unless TRUENAMEP is true
this function doesn't try to lookup the truename of
files. Finding the truename may be a superfluous and noxious
activity expecially when you expect broken symbolic links in your
filesystem.  (This may not apply to your particular lisp
system.)"
  (be files '()
    (do-directory-tree (file root-pathname :truenamep truenamep)
      (when (funcall matcher-function file)
	(push file files)))
    (nreverse files)))

(defun delete-directory-tree (pathname)
  "Recursively delete PATHNAME and all the directory structure below
it.

WARNING: depending on the way the DIRECTORY function is implemented on
your Lisp system this function may follow Unix symbolic links and thus
delete files outside the PATHNAME hierarchy.  Check this before using
this function in your programs."
  (if (pathname-name pathname)
      (delete-file pathname)
      (progn
	(dolist (file (list-directory pathname))
	  (delete-directory-tree file))
	(delete-directory pathname))))

(defun make-directory (pathname &optional (mode #o777))
  "Create a new directory in the filesystem.  Permissions MODE
will be assigned to it.  Return PATHNAME."
  #+cmu (multiple-value-bind (done errno)
	    (unix:unix-mkdir (native-namestring pathname) mode)
	  (unless done
	    (error "Unable to create directory ~A (errno=~A)." pathname errno)))
  #+sbcl (sb-posix:mkdir pathname mode)
  #-(or cmu sbcl)
  (error "MAKE-DIRECTORY is not implemented for this Lisp system.")
  pathname)

;; At least on SBCL/CMUCL + Unix + NFS this function is faster than
;; ENSURE-DIRECTORIES-EXIST, because it doesn't check all the pathname
;; components starting from the root; it proceeds from the leaf and
;; crawls the directory tree upward only if necessary."
(defun ensure-directory (pathname &key verbose (mode #o777))
  "Just like ENSURE-DIRECTORIES-EXIST but, in some situations,
it's faster."
  (labels ((ensure (path)
	     (unless (probe-file path)
	       (be* tail (last (pathname-directory path) 2)
		    last (cdr tail)
		 (setf (cdr tail) nil)
		 (unwind-protect
		      (ensure path)
		   (setf (cdr tail) last))
		 (make-directory path mode)
		 (when verbose
		   (format t "Created ~S~%" path))))))
    (ensure (make-pathname :defaults pathname
			   :name nil :type nil
			   :version nil))))

(defun make-temp-directory (&optional (default-pathname *tmp-file-defaults*) (mode #o777))
  "Create a new directory and return its pathname.
If DEFAULT-PATHNAME is specified and not NIL it's used as
defaults to produce the pathname of the directory.  Return the
pathname of the temporary directory."
  (loop
     for name = (pathname-as-directory (temp-file-name default-pathname))
     when (ignore-errors (make-directory name mode))
     return name))

(defmacro with-temp-directory ((path &rest make-temp-directory-args) &body body)
  "Execute BODY with PATH bound to the pathname of a new unique
temporary directory.  On exit of BODY the directory tree starting from
PATH will be automatically removed from the filesystem.  Return what
BODY returns.  BODY is _not_ executed within the PATH directory; the
working directory is never changed."
  `(be ,path (make-temp-directory ,@make-temp-directory-args)
     (unwind-protect
	  (progn ,@body)
       (delete-directory-tree ,path))))

(defun current-directory ()
  "Return the pathname of the current directory."
  (truename (make-pathname :directory '(:relative))))

(defun ensure-home-translations ()
  "Ensure that the logical pathname translations for the host \"home\"
are defined."
  ;; CMUCL already defines a HOME translation of its own and gets
  ;; angry if we try to redefine it
  #-cmu
  (be home (user-homedir-pathname)
    ;; we should discard and replace whatever has been defined in any
    ;; rc file during compilation
    (setf (logical-pathname-translations "home")
	  (list
	   (list "**;*.*.*"
		 (make-pathname :defaults home
				:directory (append (pathname-directory home)
						   '(:wild-inferiors))
				:name :wild
				:type :wild))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-native-namestring (string &optional host (defaults *default-pathname-defaults*)
				&key (start 0) end junk-allowed)
  #+sbcl (sb-ext:parse-native-namestring string host defaults
					 :start start
					 :end end
					 :junk-allowed junk-allowed)
  #-sbcl (let (#+cmu(lisp::*ignore-wildcards* t))
	   (parse-namestring string host defaults
			     :start start
			     :end end
			     :junk-allowed junk-allowed)))

(defun native-namestring (pathname)
  #+sbcl (sb-ext:native-namestring pathname)
  #-sbcl (let (#+cmu (lisp::*ignore-wildcards* t))
	   (namestring pathname)))

(defun native-file-namestring (pathname)
  #+sbcl (sb-ext:native-namestring
	  (make-pathname :name (pathname-name pathname)
			 :type (pathname-type pathname)))
  #+cmu (be lisp::*ignore-wildcards* t
	  (file-namestring pathname)))

(defun native-pathname (thing)
  #+sbcl (sb-ext:native-pathname thing)
  #+cmu (be lisp::*ignore-wildcards* t
	  (pathname thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bits-set-p (x bits)
  (= (logand x bits)
     bits))

(defun directory-p (pathname)
  "Return true if PATHNAME names a directory on the filesystem."
  #-clisp (awhen (unix-stat (native-namestring pathname))
	    (bits-set-p (stat-mode it)
			#+sbcl sb-posix:s-ifdir
			#+cmu unix:s-ifdir))
  #+clisp (ext:probe-directory (pathname-as-directory pathname)))

(defun regular-file-p (pathname)
  "Return true if PATHNAME names a regular file on the filesystem."
  #-(or sbcl cmu) (error "don't know how to check whether a file might be a regular file")
  (awhen (unix-stat (native-namestring pathname))
    (bits-set-p (stat-mode it)
		#+sbcl sb-posix:s-ifreg
		#+cmu unix:s-ifreg)))

(defun file-readable-p (pathname)
  #+sbcl (sb-unix:unix-access (native-namestring pathname) sb-unix:r_ok)
  #+cmu (unix:unix-access (native-namestring pathname) unix:r_ok)
  #-(or sbcl cmu) (error "don't know how to check whether a file might be readable"))

(defun file-writable-p (pathname)
  #+sbcl (sb-unix:unix-access (native-namestring pathname) sb-unix:w_ok)
  #+cmu (unix:unix-access (native-namestring pathname) unix:w_ok)
  #-(or sbcl cmu) (error "don't know how to check whether a file might be writable"))

(defun file-executable-p (pathname)
  #+sbcl (sb-unix:unix-access (native-namestring pathname) sb-unix:x_ok)
  #+cmu (unix:unix-access (native-namestring pathname) unix:x_ok)
  #-(or sbcl cmu) (error "don't know how to check whether a file might be executable"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (unix-file-stat (:conc-name stat-))
  device
  inode
  links
  atime
  mtime
  ctime
  size
  blksize
  blocks
  uid
  gid
  mode)

(defun unix-stat (pathname)
  ;; this could be different depending on the unix systems
  (multiple-value-bind (ok? device inode mode links uid gid rdev
			    size atime mtime ctime
			    blksize blocks)
      (#+cmu unix:unix-lstat
       #+sbcl sb-unix:unix-lstat
       (if (stringp pathname)
	   pathname
	   (native-namestring pathname)))
    (declare (ignore rdev))
    (when ok?
      (make-unix-file-stat :device device
			   :inode inode
			   :links links
			   :atime atime
			   :mtime mtime
			   :ctime ctime
			   :size size
			   :blksize blksize
			   :blocks blocks
			   :uid uid
			   :gid gid
			   :mode mode))))

(defun stat-modification-time (stat)
  "Return the modification time of the STAT structure as Lisp
Universal Time, which is not the same as the Unix time."
  #-(or cmu sbcl) (error "Don't know how to adjust Unix time to Lisp Universal Time.")
  (+ #+cmu lisp::unix-to-universal-time
     #+sbcl sb-impl::unix-to-universal-time
     (stat-mtime stat)))

(defun stat-creation-time (stat)
  "Return the creation time of the STAT structure as Lisp
Universal Time, which is not the same as the Unix time."
  #-(or cmu sbcl) (error "Don't know how to adjust Unix time to Lisp Universal Time.")
  (+ #+cmu lisp::unix-to-universal-time
     #+sbcl sb-impl::unix-to-universal-time
     (stat-ctime stat)))

(defun file-modification-time (file)
  "Return the modification time of FILE as Lisp Universal Time, which
is not the same as the Unix time."
  (awhen (unix-stat file)
    (stat-modification-time it)))

(defun file-creation-time (file)
  "Return the creation time of FILE as Lisp Universal Time, which
is not the same as the Unix time."
  (awhen (unix-stat file)
    (stat-creation-time it)))

(defun read-symbolic-link (symlink)
  "Return the pathname the SYMLINK points to.  That is, it's
contents."
  #+sbcl (sb-posix:readlink (native-namestring symlink))
  #+cmu (unix:unix-readlink (native-namestring symlink)))

;; FILE-LENGTH is a bit idiosyncratic in this respect.  Besides, Unix
;; allows to get to know the file size without being able to open a
;; file; just ask politely.
(defun file-size (pathname)
  (stat-size (unix-stat pathname)))

(defun symbolic-link-p (pathname)
  #-(or sbcl cmu) (error "don't know hot to test for symbolic links.")
  (aand (unix-stat pathname)
	(bits-set-p (stat-mode it)
		    #+sbcl sb-posix:s-iflnk
		    #+cmu unix:s-iflnk)))

(defun broken-link-p (pathname)
 (when (symbolic-link-p pathname)
   #+cmu (not (ignore-errors (truename pathname)))
   ;; On a broken symlink SBCL returns the link path without resolving
   ;; the link itself.  De gustibus non est disputandum.
   #+sbcl (equalp pathname (probe-file pathname))))

(defun move-file (old new)
  "Just like RENAME-FILE, but doesn't carry on to NEW file the type of
OLD file, if NEW doesn't specify one.  It does what most people would
expect from a rename function, which RENAME-FILE doesn't do.
So (MOVE-FILE \"foo.bar\" \"foo\") does rename foo.bar to foo, losing
the \"bar\" type; RENAME-FILE wouldn't allow you that."
  #+sbcl (sb-posix:rename (native-namestring old) (native-namestring new))
  #+cmu (unix:unix-rename (native-namestring old) (native-namestring new)))
