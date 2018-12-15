;;;  sclf.lisp --- miscellanea

;;;  Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: SCLF

#+cmu (ext:file-comment "$Module: sclf.lisp $")

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

;;;  Commentary:

;;; This is a collection of Common Lisp functions of the most disparate
;;; uses and purposes.  These functions are too small or too unrelated
;;; to each other to deserve an own module.
;;;
;;; If you want to indent properly the following macros you should add
;;; the following lines to your .emacs file:
;;;
;;; (defun cl-indent-be (path state indent-point sexp-column normal-indent)
;;;   (let ((sexp-start (cadr state))
;;; 	(i 0))
;;;     (save-excursion
;;;       (goto-char sexp-start)
;;;       (forward-char)
;;;       (+ sexp-column
;;; 	 (block indentation
;;; 	   (condition-case nil
;;; 	       (while (< (point) indent-point)
;;; 		 (setq i (1+ i))
;;; 		 (when (and (= 0 (logand i 1))
;;; 			    (looking-at "[\t\n ]*\\s("))
;;; 		   (return-from indentation 2))
;;; 		 (forward-sexp))
;;; 	     (error nil))
;;; 	   (if (= 1 (logand i 1))
;;; 	       6 4))))))
;;;
;;; (put 'be 'common-lisp-indent-function 'cl-indent-be)
;;; (put 'be* 'common-lisp-indent-function 'cl-indent-be)
;;; (put 'awhen 'lisp-indent-function 1)
;;; (put 'gcase 'lisp-indent-function 1)
;;; (put 'acase 'lisp-indent-function 1)
;;; (put 'acond 'lisp-indent-function 1)
;;; (put 'until 'lisp-indent-function 1)



(cl:in-package :sclf)

(defmacro be (&rest bindings-and-body)
  "Less-parenthetic let."
  (let ((bindings
	 (loop
	    while (and (symbolp (car bindings-and-body))
		       (cdr bindings-and-body))
	    collect (list (pop bindings-and-body)
			  (pop bindings-and-body)))))
    `(let ,bindings
       ,@bindings-and-body)))

(defmacro be* (&rest bindings-and-body)
  "Less-parenthetic let*."
  (let ((bindings
	 (loop
	    while (and (symbolp (car bindings-and-body))
		       (cdr bindings-and-body))
	    collect (list (pop bindings-and-body)
			  (pop bindings-and-body)))))
    `(let* ,bindings
       ,@bindings-and-body)))

(defmacro defconst (name value &rest etc)
  "For some reason SBCL, between usefulness and adherence to the ANSI
standard, has chosen the latter, thus rendering the DEFCONSTANT pretty
useless.  This macro works around that problem."
  #+sbcl (list* 'defvar name value etc)
  #-sbcl (list* 'defconstant name value etc))

(defmacro with-gensyms ((&rest symbols) &body body)
  "Gensym all SYMBOLS and make them available in BODY.
See also LET-GENSYMS."
  `(let ,(mapcar #'(lambda (s)
		     (list s '(gensym))) symbols)
     ,@body))

(defun s+ (&rest strings)
  "Return a string which is made of the concatenation of STRINGS."
  (apply #'concatenate 'string strings))

(defun string-starts-with (prefix string &optional (compare #'string=))
  (be prefix-length (length prefix)
    (and (>= (length string) prefix-length)
	 (funcall compare prefix string :end2 prefix-length))))

(defun string-ends-with (postfix string &optional (compare #'string=))
  "Return true if STRING's last characters are the same as POSTFIX."
  (be postfix-length (length postfix)
      string-length (length string)
    (and (>= string-length postfix-length)
	 (funcall compare postfix string :start2 (- string-length postfix-length)))))

(defun string-substitute (from to sequence &key (start 0) end (test #'eql))
  "Replace in SEQUENCE occurrences of FROM with TO.  FROM and TO don't
need to be the same length."
  (be from-length (length from)
    (with-output-to-string (out)
      (write-string sequence out :start 0 :end start)
      (loop
	 for position = (search from sequence :start2 start :end2 end :test test)
	 while position
	 do
	   (write-string sequence out :start start :end position)
	   (write-string to out)
	   (setf start (+ position from-length))
	 finally (write-string (subseq sequence start) out)))))

(defun string-escape (string character &key (escape-character #\\) (escape-escape t))
  "Prepend all occurences of CHARACTER in STRING with a
ESCAPE-CHARACTER."
  (with-output-to-string (stream)
    (loop
       for c across string
       when (or (char= c character)
		(and escape-escape
		     (char= c escape-character)))
       do (write-char escape-character stream)
       do (write-char c stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro aif (test then &optional else)
  `(be it ,test
     (if it
	 ,then
	 ,else)))

(defmacro awhen (test &body then)
  `(be it ,test
     (when it
       ,@then)))

(defmacro acond (&body forms)
  (when forms
    `(aif ,(caar forms)
	  (progn ,@(cdar forms))
	  (acond ,@(cdr forms)))))

(defmacro aand (&rest args)
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro acase (condition &body forms)
  `(be it ,condition
     (case it ,@forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst +whitespace+ '(#\return #\newline #\tab #\space #\page))

(defun string-trim-whitespace (string)
  (string-trim +whitespace+ string))

(defun string-right-trim-whitespace (string)
  (string-right-trim +whitespace+ string))

(defun string-left-trim-whitespace (string)
  (string-left-trim +whitespace+ string))

(defun whitespace-p (char)
  (member char +whitespace+))

(defun seq-whitespace-p (sequence)
  (every #'whitespace-p sequence))

(defun not-empty (sequence)
  "Return SEQUENCE if it's not empty, otherwise NIL.
NIL is indeed empty."
  (when (or (listp sequence)
	    (not (zerop (length sequence))))
      sequence))

(defun position-any (bag sequence &rest position-args)
  "Find any element of bag in sequence and return its position.
Accept any argument accepted by the POSITION function."
  (apply #'position-if #'(lambda (element)
			   (find element bag)) sequence position-args))

(defun find-any (bag sequence &rest find-args)
  "Find any element of bag in sequence.  Accept any argument
accepted by the FIND function."
  (apply #'find-if #'(lambda (element)
			   (find element bag)) sequence find-args))

(defun split-at (bag sequence &key (start 0) key)
  "Split SEQUENCE at occurence of any element from BAG.
Contiguous occurences of elements from BAG are considered atomic;
so no empty sequence is returned."
  (be len (length sequence)
    (labels ((split-from (start)
	       (unless (>= start len)
		 (be sep (position-any bag sequence :start start :key key)
		   (cond ((not sep)
			  (list (subseq sequence start)))
			 ((> sep start)
			  (cons (subseq sequence start sep)
				(split-from (1+ sep))))
			 (t
			  (split-from (1+ start))))))))
      (split-from start))))

(defun split-string-at-char (string separator &key escape skip-empty)
  "Split STRING at SEPARATORs and return a list of the substrings.  If
SKIP-EMPTY is true then filter out the empty substrings.  If ESCAPE is
not nil then split at SEPARATOR only if it's not preceded by ESCAPE."
  (declare (type string string) (type character separator))
  (labels ((next-separator (beg)
             (be pos (position separator string :start beg)
	       (if (and escape
			pos
			(plusp pos)
			(char= escape (char string (1- pos))))
		   (next-separator (1+ pos))
		   pos)))
           (parse (beg)
             (cond ((< beg (length string))
                    (let* ((end (next-separator beg))
                           (substring (subseq string beg end)))
                      (cond ((and skip-empty (string= "" substring))
                             (parse (1+ end)))
                            ((not end)
                             (list substring))
                            (t
			     (cons substring (parse (1+ end)))))))
                   (skip-empty
		    '())
                   (t
		    (list "")))))
    (parse 0)))

(defun copy-stream (in out)
  (loop
     for c = (read-char in nil)
     while c
     do (write-char c out)))

(defun pathname-as-file (pathname)
  "Converts PATHNAME to file form and return it."
  (unless (pathnamep pathname)
    (setf pathname (pathname pathname)))
  (cond ((pathname-name pathname)
	 pathname)
	((stringp (car (last (pathname-directory pathname))))
	 (be name (parse-native-namestring (car (last (pathname-directory pathname))))
	   (make-pathname :directory (butlast (pathname-directory pathname))
			  :name (pathname-name name)
			  :type (pathname-type name)
			  :defaults pathname)))
	;; it can't be done?
	(t pathname)))

(defun copy-file (file copy-file &key (if-exists :error))
  (with-open-file (in file)
    (with-open-file (out copy-file :direction :output :if-exists if-exists)
      (copy-stream in out))))

(defun symlink-file (src dst &key (if-exists :error))
  (when (and (eq :supersede if-exists)
	     (probe-file dst))
    (delete-file dst))
  #+sbcl (sb-posix:symlink src dst)
  #+cmu(unix:unix-symlink (native-namestring src) (native-namestring dst))
  #-(or sbcl cmu) (error "don't know how to symlink files"))

(defun read-whole-stream (stream)
  "Read stream until the end and return it as a string."
  (with-output-to-string (string)
    (loop
       for line = (read-line stream nil)
       while line
       do (write-line line string))))

(defun read-lines (stream &optional n)
  "Read N lines from stream and return them as a list of strings.  If
N is NIL, read the whole stream til the end.  If the stream ends
before N lines a read, this function will return those without
signalling an error."
  (loop
     for line = (read-line stream nil)
     for i from 0
     while (and line
		(or (not n)
		    (< i n)))
     collect line))

(defun read-file (pathname &key (element-type 'character) (if-does-not-exist :error) default)
  "Read the whole content of file and return it as a sequence which
can be a string, a vector of bytes, or whatever you specify as
ELEMENT-TYPE."
  (with-open-file (in pathname
		      :element-type element-type
		      :if-does-not-exist (unless (eq :value if-does-not-exist)
					   :error))
    (if in
	(be seq (make-array (file-length in) :element-type element-type)
	  (read-sequence seq in)
	  seq)
	default)))

(defun write-file (pathname contents &key (if-exists :error))
  "Read the whole content of file and return it as a sequence which
can be a string, a vector of bytes, or whatever you specify as
ELEMENT-TYPE."
  (with-open-file (out pathname
		       :element-type (if (stringp contents)
					 'character
					 (array-element-type contents))
		       :if-exists if-exists)
    (write-sequence contents out)))

(defun read-from-file (pathname &key (on-error :error) default)
  "Similar to READ-FROM-STRING but for files.  Read the first Lisp
object in file and return it.  If file does not exist or does not
contain a readable Lisp object, ON-ERROR tells what to do.  If
ON-ERROR is :ERROR, an error is signalled.  If ON-ERROR is :VALUE,
DEFAULT is returned."
  (ecase on-error
    (:error
     (with-open-file (in pathname)
       (read in)))
    (:value
     (handler-case (with-open-file (in pathname)
		     (read in))
       (t ()
	 default)))))

(defun write-to-file (object pathname &key (if-exists :error) pretty)
  "Similar to WRITE-TO-STRING but for files.  Write OBJECT to a file
with pathname PATHNAME."
  (with-open-file (out pathname :direction :output :if-exists if-exists)
    (write object :stream out :escape t :readably t :pretty pretty)))

(defun string-concat (list &optional (separator ""))
  "Concatenate the strings in LIST interposing SEPARATOR (default
nothing) between them."
  (reduce #'(lambda (&rest args)
	      (if args
		  (s+ (car args) separator (cadr args))
		  ""))
	  list))

;; to indent it properly: (put 'gcase 'lisp-indent-function 1)
(defmacro gcase ((value &optional (test 'equalp)) &rest cases)
  "Generic CASE macro.  Match VALUE to CASES as if by the normal CASE
but use TEST as the comparison function, which defaults to EQUALP."
  (with-gensyms (val)
    `(be ,val ,value
       ,(cons 'cond
	      (mapcar #'(lambda (case-desc)
			  (destructuring-bind (vals &rest forms) case-desc
			    `(,(cond ((consp vals)
				      (cons 'or (mapcar #'(lambda (v)
							    (list test val v))
							vals)))
				     ((or (eq vals 'otherwise)
					  (eq vals t))
				      t)
				     (t (list test val vals)))
			       ,@forms)))
		      cases)))))

(defun string-truncate (string max-length)
  "If STRING is longer than MAX-LENGTH, return a shorter version.
Otherwise return the same string unchanged."
  (if (> (length string) max-length)
      (subseq string 0 max-length)
      string))

;; to indent properly: (put 'until 'lisp-indent-function 1)
(defmacro until (test &body body)
  (with-gensyms (result)
    `(loop
	for ,result = ,test
	until ,result
	do (progn ,@body)
	finally (return ,result))))

(defun keywordify (string)
  (intern (string-upcase string) :keyword))

(defun locate-system-program (name)
  "Given the NAME of a system program try to find it through the
search of the environment variable PATH.  Return the full
pathname."
  (loop
     for dir in (split-string-at-char (getenv "PATH") #\:)
     for pathname = (merge-pathnames name (pathname-as-directory dir))
     when (probe-file pathname)
     return pathname))

(defvar *tmp-file-defaults* #P"/tmp/")

(defun temp-file-name (&optional (default *tmp-file-defaults*))
  "Create a random pathname based on DEFAULT.  No effort is made
to make sure that the returned pathname doesn't identify an
already existing file.  If missing DEFAULT defaults to
*TMP-FILE-DEFAULTS*."
  (make-pathname :defaults default
		 :name (format nil "~36R" (random #.(expt 36 10)))))

(defun open-temp-file (&optional default-pathname &rest open-args)
  "Open a new temporary file and return a stream to it.  This function
makes sure the pathname of the temporary file is unique.  OPEN-ARGS
are arguments passed verbatim to OPEN.  If OPEN-ARGS specify
the :DIRECTION it should be either :OUTPUT (default) or :IO;
any other value causes an error.  If DEFAULT-PATHNAME is specified and
not NIL it's used as defaults to produce the pathname of the temporary
file, otherwise *TMP-FILE-DEFAULTS* is used."
  (unless default-pathname
    (setf default-pathname *tmp-file-defaults*))
  ;; if :DIRECTION is specified check that it's compatible with the
  ;; purpose of this function, otherwise make it default to :OUTPUT
  (aif (getf open-args :direction)
       (unless (member it '(:output :io))
	 (error "Can't create temporary file with open direction ~A." it))
       (setf open-args (append '(:direction :output)
			       open-args)))
  (do* ((name #1=(temp-file-name default-pathname) #1#)
	(stream #2=(apply #'open  name
			  :if-exists nil
			  :if-does-not-exist :create
			  open-args) #2#))
       (stream stream)))

(defmacro with-temp-file ((stream &rest open-temp-args) &body body)
  "Execute BODY within a dynamic extent where STREAM is bound to
a STREAM open on a unique temporary file name.  OPEN-TEMP-ARGS are
passed verbatim to OPEN-TEMP-FILE."
  `(be ,stream (open-temp-file ,@open-temp-args)
     (unwind-protect
	  (progn ,@body)
       (close ,stream)
       ;; body may decide to rename the file so we must ignore the errors
       (ignore-errors
	 (delete-file (pathname ,stream))))))

(defmacro with-hidden-temp-file ((stream &rest open-args) &body body)
  "Just like WITH-TEMP-FILE but unlink (delete) the temporary file
before the execution of BODY.  As such BODY won't be able to
manipulate the file but through STREAM, and no other program is able
to see it.  Once STREAM is closed the temporary file blocks are
automatically relinquished by the operating system.  This works at
least on Unix filesystems.  I don't know about MS-OSs where the system
may likely decide to crash, take all your data with it and, in the
meanwhile, report you to the NSA as terrorist."
  `(be ,stream (open-temp-file ,@open-args)
     (unwind-protect
	  (progn (delete-file (pathname ,stream))
		 ,@body)
       (close ,stream))))

(defun insert-in-order (item seq &key (test #'<) key)
  "Destructively insert ITEM in LIST in order by TEST.  Return
the new list.  This is a simple wrapper around MERGE."
  (merge (if seq
	     (type-of seq)
	     'list)
	 (list item) seq test :key key))

(defmacro f++ (x &optional (delta 1))
  "Same as INCF but hopefully optimised for fixnums."
  `(setf ,x (+ (the fixnum ,x) (the fixnum ,delta))))

(defun soundex (word &optional (key-length 4))
  "Knuth's Soundex algorithm.  Returns a string representing the
sound of a certain word (English).  Different words will thus
yield the same output string.  To compare two string by the
sound, simply do:

   (string= (soundex str1) (soundex str2))

Examples:

   (soundex \"Knuth\") => \"K530\"
   (soundex \"Kant\") => \"K530\"
   (soundex \"Lloyd\") => \"L300\"
   (soundex \"Ladd\") => \"L300\""
  (declare (type string word))
  (flet ((translate-char (char)
	   (awhen (position char "BFPVCGJKQSXZDTLMNR")
	     (elt "111122222222334556" it))))
    (let ((key (make-string key-length :initial-element #\0))
	  (word-length (length word)))
      (setf (elt key 0) (elt word 0))
      (loop
	 with previous-sound = (translate-char (char-upcase (elt word 0)))
	 with j = 1
	 for i from 1 by 1 below word-length
	 for c = (char-upcase (elt word i))
	 while (< j key-length)
	 do (be sound (translate-char c)
	      (cond ((not (eq sound previous-sound))
		     (unless (member c '(#\H #\W))
		       (setf previous-sound sound))
		     (when sound
		       (setf (elt key j) sound)
		       (incf j))))))
      key)))

(defun string-soundex= (string1 string2)
  (let ((l1 (split-at +whitespace+ string1))
	(l2 (split-at +whitespace+ string2)))
    (and (= (length l1) (length l2))
	 (every #'string= (mapcar #'soundex l1) (mapcar #'soundex l2)))))

#+(OR)
(defun soundex-test ()
  (let* ((words1 '("Euler" "Gauss" "Hilbert" "Knuth" "Lloyd" "Lukasiewicz" "Wachs"))
	 (words2 '("Ellery" "Ghosh" "Heilbronn" "Kant" "Ladd" "Lissajous" "Waugh"))
	 (results '("E460" "G200" "H416" "K530" "L300" "L222" "W200")))
    (mapc #'(lambda (w1 w2 r)
	      (let ((r1 (soundex w1))
		    (r2 (soundex w2)))
		(format t "~A = ~A, ~A = ~A => ~A~%" w1 r1 w2 r2
			(if (and (string= r1 r2)
				 (string= r r1))
			    "OK"
			    (format nil "ERROR (expected ~A)" r)))))
	  words1 words2 results)
    (values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defstruct cache-slot ()
;;   ((previous :type (or cache-slot null)
;; 	     :initarg :previous
;; 	     :initform nil
;; 	     :accessor cslot-previous)
;;    (key :initarg :key
;; 	:accessor cslot-key)
;;    (value :initarg :value
;; 	  :accessor cslot-value)
;;    (next :type (or cache-slot null)
;; 	 :initarg :next
;; 	 :initform nil
;; 	 :accessor cslot-next)))

;; (defmethod print-object ((object cache-slot) stream)
;;   (print-unreadable-object (object stream :type t)
;;     (if (slot-boundp object 'key)
;; 	(format stream "key=~S, value=~S" (cslot-key object) (cslot-value object))
;; 	(format stream "NULL"))))


(defstruct (double-linked-element (:conc-name dle-))
  (previous nil :type (or double-linked-element null))
  value
  (next nil :type (or double-linked-element null)))

(defmethod print-object ((object double-linked-element) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "value=~S" (dle-value object))))

(defun cons-dle (value previous next)
  (declare (type (or double-linked-element null) previous next))
  (be new-element (make-double-linked-element :previous previous :next next :value value)
    (when previous
      (setf (dle-next previous) new-element))
    (when next
      (setf (dle-previous next) new-element))
    new-element))

(defun dle-remove (dle-object)
  "Remove the DLE-OBJECT from its current position in the list of
elements agjusting the pointer of dle-objects before and after this
one (if any)."
  (declare (type double-linked-element dle-object))
  (awhen (dle-next dle-object)
    (setf (dle-previous it) (dle-previous dle-object)))
  (awhen (dle-previous dle-object)
    (setf (dle-next it) (dle-next dle-object))))

(defun dle-map (function dle-object)
  (when dle-object
    (make-double-linked-element :value (funcall function (dle-value dle-object))
				:previous (dle-previous dle-object)
				:next (dle-map function (dle-next dle-object)))))

(defmacro do-dle ((var dle &optional (result nil)) &body body)
  "Iterate over a list of DOUBLE-LINKED-ELEMENTs and map body to
each element's value.  Bind VAR to the value on each iteration."
  (be cursor (gensym)
    `(do ((,cursor ,dle (dle-next ,cursor)))
	 ((not ,cursor) ,result)
       (be ,var (dle-value ,cursor)
	 ,@body))))

(defmacro do-dle* ((var dle &optional (result nil)) &body body)
  "Same as DO-DLE but VAR is a symbol macro, so that BODY can
modify the element's value."
  (be cursor (gensym)
    `(symbol-macrolet ((,var (dle-value ,cursor)))
       (do ((,cursor ,dle (dle-next ,cursor)))
	   ((not ,cursor) ,result)
	 ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass double-linked-list ()
  ((elements :type double-linked-element
	     :documentation "The actual list of elements held by this object.")
   (last-element :type double-linked-element))
  (:documentation
   "A double linked list where elements can be added or removed
from either end."))

(defmethod initialize-instance ((object double-linked-list) &rest rest)
  (declare (ignorable rest))
  (call-next-method)
  (with-slots (last-element elements) object
    (setf last-element (make-double-linked-element)
	  elements last-element)))

(defmethod print-object ((object double-linked-list) stream)
  (print-unreadable-object (object stream :type t)
    (be elements '()
      (do-dle (e (slot-value object 'elements))
	(push e elements))
      (format stream "elements=~S" (nreverse elements)))))

(defgeneric pop-first (double-linked-list)
  (:documentation
   "Pop the first element of a double-linked-list."))
(defgeneric pop-last (double-linked-list)
  (:documentation
   "Pop the last element of a double-linked-list."))
(defgeneric push-first (item double-linked-list)
  (:documentation
   "Push an item in front of a double-linked-list."))
(defgeneric push-last (item double-linked-list)
  (:documentation
   "Append an item to a double-linked-list."))
(defgeneric list-map (function double-linked-list)
  (:documentation
   "Map a function to a double-linked-list."))
(defgeneric dll-find-cursor (object dll &key test key))
(defgeneric dll-find (object dll &key test key))
(defgeneric dll-remove (cursor dll))

(defmethod pop-last ((list double-linked-list))
  "Drop the last element in the dl list."
  (with-slots (last-element) list
    (awhen (dle-previous last-element)
      (dle-remove it)
      (dle-value it))))

(defmethod pop-first ((list double-linked-list))
  "Drop the first element in the dl list."
  (with-slots (elements) list
    (when (dle-next elements)
      (prog1 (dle-value elements)
	(setf (dle-previous (dle-next elements)) nil
	      elements (dle-next elements))))))

(defmethod push-first (value (list double-linked-list))
  (with-slots (elements) list
    (setf elements (cons-dle value nil elements)))
  list)

(defmethod push-last (value (list double-linked-list))
  (with-slots (last-element) list
    (cons-dle value (dle-previous last-element) last-element))
  list)

(defmethod list-map (function (list double-linked-list))
  (labels ((map-dll (dle)
	     (when (dle-next dle)
	       (make-double-linked-element
		:value (funcall function (dle-value dle))
		:previous (dle-previous dle)
		:next (map-dll (dle-next dle))))))
    (map-dll (slot-value list 'elements))))

(defmethod dll-find-cursor (object (list double-linked-list) &key (test #'eql) (key #'identity))
  (do ((cursor (slot-value list 'elements) (dle-next cursor)))
      ((not (dle-next cursor)))
    (be value (dle-value cursor)
      (when (funcall test (funcall key value) object)
	(return cursor)))))

(defmethod dll-find (object (list double-linked-list) &key (test #'eql) (key #'identity))
  (awhen (dll-find-cursor object list :test test :key key)
    (dle-value it)))

(defmethod dll-remove ((cursor double-linked-element) (list double-linked-list))
  (with-slots (elements) list
    (if (dle-previous cursor)
	(dle-remove cursor)
	(setf (dle-previous (dle-next elements)) nil
	      elements (dle-next elements))))
  list)

(defmacro do-dll ((var list &optional (result nil)) &body body)
  "Iterate over a dll and map body to each element's
value.  Bind VAR to the value on each iteration."
  (be cursor (gensym)
    `(do ((,cursor (slot-value ,list 'elements) (dle-next ,cursor)))
	 ((not (dle-next ,cursor)) ,result)
       (be ,var (dle-value ,cursor)
	 ,@body))))

(defmacro do-dll* ((var list &optional (result nil)) &body body)
  "Same as DO-DLL but VAR is a symbol macro, so that BODY can
modify the element's value."
  (be cursor (gensym)
    `(symbol-macrolet ((,var (dle-value ,cursor)))
       (do ((,cursor (slot-value ,list 'elements) (dle-next ,cursor)))
	   ((not (dle-next ,cursor)) ,result)
	 ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass limited-list (double-linked-list)
  ((max-size :initform nil
	     :initarg :size
	     :reader max-size
	     :type (or integer null)
	     :documentation "Size limit to which the list is allowed to grow to.  NIL = no limit.")
   (size :initform 0
	 :reader size
	 :type integer
	 :documentation "Current number of elements in the list."))
  (:documentation
   "A double linked list where the maximum number of elements can
be limited."))

(defun dll-member-p (dle list)
  (with-slots (elements size) list
    (do ((e elements (dle-next e)))
	((not e))
      (when (eq e dle)
	(return t)))))

(defmethod dll-remove ((cursor double-linked-element) (list limited-list))
  (with-slots (size) list
    (unless (zerop size)
      (decf size)
      (call-next-method)))
  list)

(defmethod pop-first ((list limited-list))
  (with-slots (size) list
    (unless (zerop size)
      (decf size)
      (call-next-method))))

(defmethod pop-last ((list limited-list))
  (with-slots (size) list
    (unless (zerop size)
      (decf size)
      (call-next-method))))

(defmethod push-first (value (list limited-list))
  "Add in front of the list and drop the last element if list is
full."
  (declare (ignore value))
  (prog1 (call-next-method)
    (with-slots (max-size size last-element) list
      (if (or (not max-size)
	      (< size max-size))
	  (incf size)
	  (dle-remove (dle-previous last-element))))))

(defmethod push-last (value (list limited-list))
  "Add at the end of the list and drop the first element if list
is full."
  (declare (ignore value))
  (prog1 (call-next-method)
    (with-slots (max-size size elements) list
      (if (or (not max-size)
	      (< size max-size))
	(incf size)
	(setf (dle-previous (dle-next elements)) nil
	      elements (dle-next elements))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sorted-list (limited-list)
  ((test :type function
	 :initarg :test))
  (:documentation
   "A double linked list where elements are inserted in a
sorted order."))

(defgeneric insert (item sorted-list)
  (:documentation
   "Insert an item in a sorted-list."))

(defmethod insert (item (sl sorted-list))
  "Insert ITEM in SL, which is a sorted double linked list,
before the item for which TEST is true or at the end of the list.
Returns two values, the modified list and the cursor to the new
element."
  (with-slots (max-size size elements test last-element) sl
    (do ((cursor elements (dle-next cursor)))
	((or (not (dle-next cursor))
	     (funcall test item (dle-value cursor)))
	 (if (dle-previous cursor)
	     (cons-dle item (dle-previous cursor) cursor)
	     (setf elements (cons-dle item nil cursor)))
	 (if (or (not max-size)
		  (< size max-size))
	     (incf size)
	     (dle-remove (dle-previous last-element)))
	 (values sl (dle-previous cursor))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass heap ()
  ((less-than :type function
	      :initarg :test
	      :documentation "The heap invariant.")
   (data :type array
	 :documentation "The heap tree representation.")))

(defmethod initialize-instance ((heap heap) &rest args)
  (declare (ignore args))
  (call-next-method)
  (with-slots (data) heap
    (setf data (make-array 0 :fill-pointer 0 :adjustable t))))

(defgeneric heap-add (heap item))

(defun bubble-up (heap pos)
  (with-slots (data less-than) heap
    (loop
       for current = pos then parent
       for parent = (truncate (1- current) 2)
       until (or (zerop current)
		 (funcall less-than (aref data parent) (aref data current)))
       do (rotatef (aref data current) (aref data parent)))))

(defmethod heap-add ((heap heap) item)
  (with-slots (data) heap
    (vector-push-extend item data)
    (bubble-up heap (1- (fill-pointer data)))))

(defgeneric heap-size (heap))

(defmethod heap-size ((heap heap))
  (fill-pointer (slot-value heap 'data)))

(defgeneric heap-empty-p (heap))

(defmethod heap-empty-p ((heap heap))
  (zerop (heap-size heap)))


(defgeneric heap-pop (heap))

(defun percolate-down (heap pos)
  (with-slots (data less-than) heap
    (loop
       with end = (fill-pointer data)
       for current = pos then child
       for left-child = (+ 1 (* 2 current))
       for right-child = (+ 2 (* 2 current))
       for child = (cond ((>= left-child end)
			  (return))
			 ((>= right-child end)
			  left-child)
			 ((funcall less-than (aref data left-child) (aref data right-child))
			  left-child)
			 (t
			  right-child))
       while (funcall less-than (aref data child) (aref data current))
       do (rotatef (aref data current) (aref data child)))))

(defmethod heap-pop ((heap heap))
  (assert (not (heap-empty-p heap)))
  (with-slots (data) heap
    (be root (aref data 0)
      (setf (aref data 0) (vector-pop data))
      (percolate-down heap 0)
      root)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (lru-cache-slot (:include double-linked-element)
			   (:conc-name lruc-slot-))
  key)

(defmethod print-object ((object lru-cache-slot) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "key=~S value=~S" (lruc-slot-key object) (lruc-slot-value object))))

(defvar *default-cache-size* 100
  "Default size of a LRU cache if it's not specified at instantiation
time.")

(defclass lru-cache ()
  ((max-size :initform *default-cache-size*
	     :initarg :size
	     :reader max-size
	     :type (or integer null)
	     :documentation
	     "Maximum number of elements that the cache can fit.")
   (elements-list :type lru-cache-slot
		  :documentation "The list of elements held by the cache.")
   (elements-hash :type hash-table
		  :documentation "The hash table of the elements held bye the cache.")
   (last-element :type lru-cache-slot)
   (size :initform 0
	 :reader size
	 :type integer
	 :documentation "Current number of elements in the cache.")
   (finalizer :initform nil
	      :initarg :finalizer
	      :documentation
	      "Procedure to call when elements are dropped from cache."))
  (:documentation
   "An objects cache that keeps the elements used more often and
drops those that are used less often.  The usage is similar to an
hash table.  Elements are added to the list up to MAX-SIZE, then
any new element will drop the less used one in the cache.  Every
time an element is set or retrieved it goes in front of a list.
Those which get at the end of the list are dropped when more room
is required."))

(defmethod initialize-instance ((object lru-cache) &key test &allow-other-keys)
  (call-next-method)
  (with-slots (last-element elements-list elements-hash) object
    (setf last-element (make-lru-cache-slot)
	  elements-list last-element
	  elements-hash (if test
			    (make-hash-table :test test)
			    (make-hash-table)))))

(defgeneric getcache (key cache)
  (:documentation
   "Get an item with KEY from a CACHE."))

(defgeneric (setf getcache) (value key cache)
  (:documentation
   "Set or add an item with KEY in a CACHE."))

(defgeneric remcache (key cache)
  (:documentation
   "Remove an item with KEY from a CACHE."))

(defun move-in-front-of-cache-list (slot cache)
  "Relocate slot to the front of the elements list in cache.
This will stretch its lifespan in the cache."
  (declare (type lru-cache-slot slot)
	   (type lru-cache cache))
  (with-slots (elements-list) cache
    ;; unless it's already the first
    (unless (eq slot elements-list)
      ;; remove the slot from its original place...
      (dle-remove slot)
      ;; ... and add it in front of the list
      (setf (lruc-slot-next slot) elements-list
	    (lruc-slot-previous slot) nil
	    (lruc-slot-previous elements-list) slot
	    elements-list slot))))

(defun drop-last-cache-element (cache)
  "Drop the last element in the list of the cache object."
  (declare (type lru-cache cache))
  (with-slots (last-element elements-hash finalizer) cache
    (let ((second-last (lruc-slot-previous last-element)))
      (assert second-last)
      (when finalizer
	(funcall finalizer (lruc-slot-value second-last)))
      (dle-remove second-last)
      (remhash (lruc-slot-key second-last) elements-hash))))

(defun add-to-cache (slot cache)
  (declare (type lru-cache-slot slot)
	   (type lru-cache cache))
  (move-in-front-of-cache-list slot cache)
  (with-slots (max-size size elements-hash) cache
    (setf (gethash (lruc-slot-key slot) elements-hash) slot)
    (if (and max-size
	     (< size max-size))
	(incf size)
	(drop-last-cache-element cache))))

(defmethod getcache (key (cache lru-cache))
  (multiple-value-bind (slot found?) (gethash key (slot-value cache 'elements-hash))
    (when found?
      (move-in-front-of-cache-list slot cache)
      (values (lruc-slot-value slot) t))))

(defmethod (setf getcache) (value key (cache lru-cache))
  (with-slots (elements-hash elements-list) cache
    (multiple-value-bind (slot found?) (gethash key elements-hash)
      (if found?
	  (progn
	    (move-in-front-of-cache-list slot cache)
	    (setf (lruc-slot-value slot) value))
	  (add-to-cache (make-lru-cache-slot :key key :value value) cache))
      value)))

(defmethod remcache (key (cache lru-cache))
  (with-slots (elements-hash size elements-list finalizer) cache
    (multiple-value-bind (slot found?) (gethash key elements-hash)
      (when found?
	(remhash key elements-hash)
	(when finalizer
	  (funcall finalizer (lruc-slot-value slot)))
	(when (eq slot elements-list)
	  (setf elements-list (dle-next slot)))
	(dle-remove slot)
	(decf size)
	t))))

(defmacro cached (cache key value)
  "If KEY is found in CACHE return the associated object.  Otherwise
store VALUE for later re-use."
  (with-gensyms (object my-cache my-key my-value found?)
    `(let* ((,my-cache ,cache)
	    (,my-key ,key))
       (multiple-value-bind (,object ,found?) (getcache ,my-key ,my-cache)
	 (if ,found?
	     ,object
	     (let ((,my-value ,value))
	       (setf (getcache ,my-key ,my-cache) ,my-value)
	       ,my-value))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(declaim (inline list->string))
(defun list->string (list)
  "Coerce a list of characters into a string."
  (coerce list 'string))

(defun setuid (id)
  "Set the Unix real user id."
  (when (stringp id)
    (setf id (find-uid id)))
  #+sbcl (sb-posix:setuid id)
  #+cmu (unix:unix-setuid id)
  #+clisp (posix::%setuid id)		; not verified -wcp26/8/09.
  #-(or cmu sbcl clisp)
  (error "setuid unsupported under this Lisp implementation"))

(defun seteuid (id)
  "Set the Unix effective user id."
  (when (stringp id)
    (setf id (find-uid id)))
  #+sbcl (sb-posix:seteuid id)
  #+cmu (unix:unix-setreuid -1 id)
  #+clisp (posix::%seteuid id)		; not verified -wcp26/8/09.
  #-(or cmu sbcl clisp)
  (error "seteuid unsupported under this Lisp implementation"))

(defun find-uid (name)
  "Find the user id of NAME.  Return an integer."
  #+sbcl (awhen (sb-posix:getpwnam name)
	   (sb-posix:passwd-uid it))
  #+cmu (awhen (unix:unix-getpwnam name)
	  (unix:user-info-uid it))
  #-(or cmu sbcl)
  (error "Unable to find a UID on this Lisp system."))

#+clisp (ffi:def-call-out %getuid
	    (:name "getuid")
	  (:arguments)
	  (:return-type ffi:int)
	  (:library "libc.so"))

(defun getuid ()
  "Return the Unix user id.  This is an integer."
  #+sbcl (sb-unix:unix-getuid)
  #+cmu (unix:unix-getuid)
  #+clisp (%getuid)
  #-(or cmu sbcl clisp)
  (error "getuid unsupported under this Lisp implementation"))

(defun super-user-p (&optional id)
  "Return true if the user ID is zero.  ID defaults to the current
user id."
  (zerop (or id (getuid))))

(defmacro with-euid (uid &body forms)
  "Switch temporarely to Unix user id UID, while performing FORMS."
  (with-gensyms (ruid)
    `(be ,ruid (getuid)
       (seteuid ,uid)
       (unwind-protect (progn ,@forms)
	 (seteuid ,ruid)))))

(defun get-logname (&optional uid)
  "Return the login id of the user.  This is a string and it is not
the Unix uid, which is a number."
  (unless uid
    (setf uid (getuid)))
  (when (stringp uid)
    (setf uid (find-uid uid)))
  (when uid
    #+sbcl (sb-unix:uid-username uid)
    #+cmu (unix:user-info-name (unix:unix-getpwuid uid))
    #+clisp (posix:user-info-login-id (posix:user-info uid))
    #-(or cmu sbcl clisp)
    (error "get-logname unsupported under this Lisp implementation")))

(defun get-user-name (&optional uid)
  "Return the user name, taken from the GCOS field of the /etc/passwd
file."
  (unless uid
    (setf uid (getuid)))
  (when (stringp uid)
    (setf uid (find-uid uid)))
  (when uid
    (car (split-string-at-char #+cmu (unix:user-info-gecos (unix:unix-getpwuid uid))
			       #+sbcl (sb-posix:passwd-gecos (sb-posix:getpwuid uid))
			       #-(or cmu sbcl) (error "can't getpwuid() on this Lisp system.")
			       #\,))))

(defun get-user-home (&optional uid)
  (unless uid
    (setf uid (getuid)))
  (when (stringp uid)
    (setf uid (find-uid uid)))
  (when uid
    #+cmu (unix:user-info-dir (unix:unix-getpwuid uid))
    #+sbcl (sb-posix:passwd-dir (sb-posix:getpwuid uid))))

;; Rather stupid, but the mnemonic is worth it
(declaim (inline alist->plist))
(defun alist->plist (alist)
  "Convert an association list into a property list.  The alist
elements are assumed to be lists of just two elements: the key
and the value.  If the element list is longer this function
doesn't work."
  (mapcan #'identity alist))

(defun plist->alist (plist &optional pairs-p)
  "Convert a property list into an association list.  The alist
elements wiil be lists of just two elements: the key and the
value.  If PAIRS-P is true the alist elements will be pairs."
  (loop
     for (key val) on plist by #'cddr
     collect (if pairs-p
		 (cons key val)
		 (list key val))))

(defun string->byte-vector (string &key start end)
  "Convert a string of characters into a vector of (unsigned-byte
8) elements."
  (map '(vector (unsigned-byte 8)) #'char-code
       (if (or start end)
	   (subseq string (or start 0) end)
	   string)))

(defun byte-vector->string (vector &key start end)
  "Convert a vector of (unsigned-byte 8) elements into a string
of characters."
  (map 'string #'code-char
       (if (or start end)
	   (subseq vector (or start 0) end)
	   vector)))

(defun outdated-p (file dependencies)
  "Check if FILE has been modified before any of its
DEPENDENCIES."
  (be epoch (and (probe-file file)
		 (file-write-date file))
    ;; if file is missing altogether, we consider it outdated
    (or (not epoch)
	(loop
	   for dep in dependencies
	   thereis (aand (probe-file dep)
			 (file-write-date dep)
			 (> it epoch))))))

(defmacro let-places (places-and-values &body body)
  "Execute BODY binding temporarily some places to new values and
restoring the original values of these places on exit of BODY.  The
syntax of this macro is identical to LET.  The difference is that
instead of new variable names this macro binds values to existing
places (variables)."
  (be tmp-variables (loop for x in places-and-values collect (gensym))
    `(let ,(mapcar #'(lambda (tmp-var place-and-value)
		       (list tmp-var (car place-and-value)))
		   tmp-variables places-and-values)
       (unwind-protect
	    (progn
	      ;; as some assignments could signal an error, we assign
	      ;; within the unwind-protect block so that we can always
	      ;; guarantee a consistent state on exit
	      ,@(mapcar #'(lambda (place-and-value)
			    `(setf ,(car place-and-value) ,(cadr place-and-value)))
			places-and-values)
	      ,@body)
	 ,@(mapcar #'(lambda (tmp-var place-and-value)
		       `(setf ,(car place-and-value) ,tmp-var))
		   tmp-variables
		   places-and-values)))))

(defmacro let-slots (accessor/new-value-pairs object &body body)
  "Execute BODY with some OBJECT's slots temporary sets to new
values as described in ACCESSOR/NEW-VALUE-PAIRS.  The latter
should be an alist of accessor names and the value to be assigned
to that slot.  On exit from BODY, those slots are restored to
their original value.  See LET-PLACES."
  (with-gensyms (obj)
    `(be ,obj ,object
       (let-places ,(mapcar #'(lambda (av)
				`((,(car av) ,obj) ,(cadr av)))
			    accessor/new-value-pairs)
	 ,@body))))

(defvar *decimal-point* #\.)
(defvar *thousands-comma* #\,)

(defun format-amount (number &key (decimals 2) (rounder #'round)
		      (comma *thousands-comma*) (comma-stance 3)
		      (decimal-point *decimal-point*))
  "Return a string formatted as fixed decimal point number of DECIMALS
adding commas every COMMA-STANCE places before the decimal point."
  (declare (type number number)
	   (type fixnum decimals comma-stance)
	   (type function rounder)
	   (type character comma decimal-point)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((int (funcall rounder (* number (expt 10 decimals))))
	 (negative (< int 0)))
    (declare (integer int))
    (when negative
      (setf int (- int)))
    (let* ((digits (max (1+ decimals)
			(1+ (if (zerop int)
				0
				(truncate (log int 10))))))
	   (string-length (+ digits
			     ;; the minus sign
			     (if negative 1 0)
			     ;; the decimal point
			     (if (zerop decimals) 0 1)
			     ;; the thousands commas
			     (1- (ceiling (- digits decimals) comma-stance))))
	   (string (make-string string-length))
	   (pos (1- string-length)))
      (declare (type fixnum pos digits))
      (labels ((add-char (char)
		 (setf (schar string pos) char)
		 (decf pos))
	       (add-digit ()
		 (add-char (digit-char (mod int 10)))
		 (setf int (truncate int 10))))
	(unless (zerop decimals)
	  (loop
	     for i fixnum from 0 below decimals
	     do (add-digit))
	  (add-char decimal-point))
	(loop
	   for i fixnum from 1
	   do (add-digit)
	   while (>= pos (if negative 1 0))
	   when (zerop (mod i comma-stance))
	   do (add-char comma))
	(when negative
	  (add-char #\-)))
      string)))

(defun parse-amount (string &key (start 0) end)
  "Parse STRING as if it was formatted with FORMAT-AMOUNT and return
the parsed number.  Return NIL if STRING is malformed.  Leading or
trailing spaces must be removed from the string in advance."
  (loop
     with amount = 0
     with decimals = nil
     with negative = (when (and (not (zerop (length string)))
				(char= #\- (char string 0)))
		       (incf start)
		       t)
     for i from start below (or end (length string))
     for c = (char string i)
     do (cond ((char= c *decimal-point*)
	       (if decimals
		   (return nil)
		   (setf decimals 0)))
	      ((char= c *thousands-comma*))
	      (t
	       (be d (digit-char-p c)
		 (cond ((not d)
			(return nil))
		       (decimals
			(incf decimals)
			(incf amount (/ d (expt 10 decimals))))
		       (t
			(setf amount (+ d (* amount 10))))))))
     finally (return (if negative
			 (- amount)
			 amount))))

(defmacro with-package (name &body body)
  `(let ((*package* (find-package ,name)))
     ,@body))

(defun bytes-simple-string (n &optional imply-bytes)
  "Return a string describing N using a unit of measure multiple
of a byte that is most apporpriate for the magnitude of N.  A
kilobyte is 1024 not 1000 bytes, everything follows."
  (let* ((kilo 1024)
	 (mega (* kilo kilo))
	 (giga (* kilo mega))
	 (tera (* mega mega))
	 (peta (* kilo tera)))
    (apply #'format nil "~,1F~A"
	   (cond ((> n (* 2 peta))
		  (list (/ n peta) (if imply-bytes "P" "PB")))
		 ((> n (* 2 tera))
		  (list (/ n tera) (if imply-bytes "T" "TB")))
		 ((> n (* 2 giga))
		  (list (/ n giga) (if imply-bytes "G" "GB")))
		 ((> n (* 2 mega))
		  (list (/ n mega) (if imply-bytes "M" "MB")))
		 ((> n (* 2 kilo))
		  (list (/ n kilo) (if imply-bytes "K" "KB")))
		 (t (list n (if imply-bytes "" " bytes")))))))

;; WARNING: This function may or may not work on your Lisp system.  It
;; all depends on how the OPEN function has been implemented regarding
;; the :IF-EXISTS option.  This function requires that OPEN be
;; implemented in a way so that the checking of the existence of file
;; and its open attempt be atomic.  If the Lisp OPEN first checks that
;; the file exists and then tries to open it, this function won't be
;; reliable.  CMUCL seems to use the O_EXCL open() flag in the right
;; way.  So at least on CMUCL this function will work.  Same goes for
;; SBCL.
(defun make-lock-files (pathnames &key (sleep-time 7) retries (suspend 13) expiration)
  "Create semaphore files.  If it can't create all the specified
files in the specified order, it waits SLEEP-TIME seconds and
retries the last file that didn't succeed.  You can specify the
number of RETRIES to do until failure is returned.  If the number
of retries is NIL this function will retry forever.

If it tries RETRIES times without success, this function signal
an error and removes all the lock files it created until then.

All files created by lock file will be read-only.

If you specify a EXPIRATION then an existing lock file will be
removed by force after EXPIRATION seconds have passed since the
lock file was last modified/created (most likely by some other
program that unexpectedly died without cleaning up its lock
files).  After a lock file has been removed by force, a
suspension of SUSPEND seconds is taken into account, in order to
prevent the inadvertent immediate removal of any newly created
lock file by another program."
  (be locked '()
    (flet ((lock (file)
	     (when (and expiration
			(> (get-universal-time)
			   (+ (file-write-date file) expiration)))
	       (delete-file file)
	       (when suspend
		 (sleep suspend)))
	     (do ((i 0 (1+ i))
		  (done nil))
		 (done)
	       (unless (or (not retries)
			   (< i retries))
		 (error "Can't create lock file ~S: tried ~A time~:P." file retries))
	       (with-open-file (out file :direction :output :if-exists nil)
		 (cond (out
			(format out "Lock file created on ~A~%" (time-string (get-universal-time)))
			(setf done t))
		       (sleep-time
			(sleep sleep-time)))))))
      (unwind-protect
	   (progn
	     (dolist (file pathnames)
	       (lock file)
	       (push file locked))
	     (setf locked '()))
	(mapc #'delete-file locked)))))

(defmacro with-lock-files ((lock-files &rest lock-args) &body body)
  "Execute BODY after creating LOCK-FILES.  Remove the lock files
on exit.  LOCK-ARGS are passed to MAKE-LOCK-FILES."
  (with-gensyms (files)
    `(be ,files (list ,@lock-files)
       (make-lock-files ,files ,@lock-args)
       (unwind-protect (progn ,@body)
	 (mapc #'delete-file ,files)))))

(defun getpid ()
  #+cmu (unix:unix-getpid)
  #+sbcl (sb-unix:unix-getpid)
  #+clisp (ext:process-id)
  #-(or cmu sbcl clisp)
   (error "getpid unsupported under this Lisp implementation"))

(defmacro on-error (form &body error-forms)
  "Execute FORM and in case of error execute ERROR-FORMS too.
This does _not_ stop the error from propagating."
  (be done-p (gensym)
    `(be ,done-p nil
       (unwind-protect
	    (prog1
		,form
	      (setf ,done-p t))
	 (unless ,done-p
	   ,@error-forms)))))

(defun floor-to (x aim)
  "Round X down to the nearest multiple of AIM."
  (* (floor x aim) aim))

(defun round-to (x aim)
  "Round X to the nearest multiple of AIM."
  (* (round x aim) aim))

(defun ceiling-to (x aim)
  "Round X up to the nearest multiple of AIM."
  (* (ceiling x aim) aim))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct queue
  first
  last)

(defgeneric queue-append (queue objects))
(defgeneric queue-pop (queue))
(defgeneric queue-empty-p (queue))

(defmethod queue-append ((queue queue) (objects list))
  (cond ((null (queue-first queue))
	 (setf (queue-first queue) objects
	       (queue-last queue) (last objects)))
	(t
	 (setf (cdr (queue-last queue)) objects
	       (queue-last queue) (last objects))))
  queue)

(defmethod queue-append ((queue queue) object)
  (queue-append queue (list object)))

(defmethod queue-pop ((queue queue))
  (prog1 (car (queue-first queue))
    (setf (queue-first queue) (cdr (queue-first queue)))))

(defmethod queue-empty-p ((queue queue))
  (null (queue-first queue)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun package-locked-p (package)
  #+sbcl (sb-ext:package-locked-p package)
  #+cmu (ext:package-definition-lock package)
  #+clisp (ext:package-lock package)
  #-(or sbcl cmu clisp) (error "Don't know how to check whether a package might be locked."))

(defun forget-documentation (packages)
  "Remove documentation from all known symbols in PACKAGES.  If
PACKAGES is NIL remove documentations from all packages.  This may not
make sense if your Lisp image has been built so that existing objects
don't get garbage collected.  It may work for your own code, though.
Locked packages are left alone.  If you need to do those too, unlock
them first."
  (flet ((forget (symbol)
	   (dolist (type '(compiler-macro function method-combination setf structure type variable))
	     (when (ignore-errors (documentation symbol type))
	       (setf (documentation symbol type) nil)))))
    (setf packages (mapcar #'(lambda (pkg)
			       (if (packagep pkg)
				   (package-name pkg)
				   (package-name (find-package pkg))))
			   packages))
    (setf packages
	  ;; don't try to modify locked packages
	  (remove-if #'package-locked-p
		     (mapcar #'find-package
			     (or packages
				 (list-all-packages)))))
    (dolist (package packages)
      (with-package-iterator (next package :internal :external)
	(loop
	   (multiple-value-bind (more? symbol) (next)
	     (unless more?
	       (return))
	     (forget symbol)))))
    #+(OR) (do-all-symbols (symbol)
	     (when (member (symbol-package symbol) packages)
	       (forget symbol))))
  (values))

(defun load-compiled (pathname &optional compiled-pathname)
  "Make sure to compile PATHNAME before loading it.  Don't compile if
the compiled version is more recent than its source."
  ;; be tolerant if we didn't get a type
  (unless (probe-file pathname)
    (setf pathname (merge-pathnames pathname (make-pathname :type "lisp"))))
  (if (probe-file pathname)
      (progn
	(setf compiled-pathname (or compiled-pathname
				    (compile-file-pathname pathname)))
	(when (or (not (probe-file compiled-pathname))
		  (< (file-write-date compiled-pathname)
		     (file-write-date pathname)))
	  (compile-file pathname))
	(load compiled-pathname))
      (error "Can't load ~A as it doesn't exist." pathname)))

;; Just a silly mnemonic for those used to lesser languages
(defmacro swap (x y)
  "Swap values of places X and Y."
  `(rotatef ,x ,y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro show (&rest things)
  "Debugging macro to show the name and content of variables.  You can
also specify forms, not just variables."
  (let ((*print-pretty* nil))
    `(let ((*print-circle* t))
       (format t ,(format nil "~~&~{~A=~~:W~~%~}" things)
	       ,@things)
       (finish-output)
       (values))))

(defmacro memoize-function (name &key test)
  "Make function NAME memoized.  TEST is passed to MAKE-HASH-TABLE."
  `(setf (get ',name 'results-hash-table)
	 (make-hash-table ,@(when test (list :test test)))))

(defmacro defun-memoized (name args &body forms)
  "Define function NAME and make it memoizable.  Then the MEMOIZED
macro can be used to call this function and memoize its results.  The
function NAME must accept only one argument and return just one
argument; more complicated cases are not considered.  The hash table
test function is the default 'EQL."
  `(eval-when (:load-toplevel :compile-toplevel)
     (defun ,name ,args ,@forms)
     (memoize-function ,name)))

(defmacro memoized (function arg)
  "If necessary call FUNCTION passing ARG so that its return value is
memoized.  The next time this form is executed with the same argument
value, the memoized result is returned instead of executing FUNCTION."
  (with-gensyms (table key result not-found)
    `(be* ,key ,arg
	  ,table (get ',function 'results-hash-table)
	  ,not-found (list nil)
	  ,result (gethash ,key ,table ,not-found)
       (if (eq ,not-found ,result)
	   (setf (gethash ,key ,table)
		 (,function ,key))
	   ,result))))


(defmacro save-file-excursion ((stream &optional position) &body forms)
  "Execute FORMS returning, on exit, STREAM to the position it was
before FORMS.  Optionally POSITION can be set to the starting offset."
  (unless position
    (setf position (gensym)))
  `(be ,position (file-position ,stream)
     (unwind-protect (progn ,@forms)
       (file-position ,stream ,position))))

(defun circular-list (&rest elements)
  "Return a circular list of ELEMENTS."
  (setf (cdr (last elements)) elements))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun getenv (var)
  "Return the string associate to VAR in the system environment."
  #+cmu (cdr (assoc (if (symbolp var)
			var
			(intern var :keyword))
		    ext:*environment-list*))
  #+sbcl (sb-ext:posix-getenv (string var))
  #+lispworks (hcl:getenv var)
  #+clisp (ext:getenv (string var))
  #-(or cmu sbcl lispworks clisp)
  (error "GETENV not implemented for your Lisp system."))

#+clisp (ffi:def-call-out %setenv
	    (:name "setenv")
	  (:arguments (name ffi:c-string) (value ffi:c-string) (overwrite ffi:int))
	  (:return-type ffi:int)
	  (:library "libc.so"))

#+clisp (ffi:def-call-out %unsetenv
	    (:name "unsetenv")
	  (:arguments (name ffi:c-string))
	  (:return-type ffi:int)
	  (:library "libc.so"))

(defun setenv (name value &optional (overwrite t))
  (typecase value
    (string)
    (pathname
     (setf value (native-namestring value)))
    (t
     (setf value (format nil "~A" value))))
  #+sbcl (unless (zerop (sb-posix:setenv name value (if overwrite 1 0)))
	   (error "unable to setenv ~A: errno=~A." name
		  (sb-alien:get-errno)))
  #+cmu (be key (keywordify name)
	  (aif (assoc key
		      ext:*environment-list*)
	       (when overwrite
		 (setf (cdr it) value))
	       (setf ext:*environment-list*
		     (cons (cons key value)
			   ext:*environment-list*))))
  #-(or cmu sbcl) (unless (zerop (%setenv name value (if overwrite 1 0)))
		    (error "unable to setenv ~A." name)))

(defun unsetenv (name)
  #+sbcl (unless (zerop (sb-posix:unsetenv name))
	   (error "unable to unsetenv ~A: errno=~A." name
		  (sb-alien:get-errno)))
  #+cmu (be key (keywordify name)
	  (setf ext:*environment-list*
		(delete-if #'(lambda (e)
			       (eq (car e) key))
			   ext:*environment-list*)))
  #-(or cmu sbcl) (unless (zerop (%unsetenv name))
		    (error "unable to unsetenv ~A." name)))

(defun (setf getenv) (value name)
  (if value
      (setenv name value t)
      (unsetenv name)))

;; in CMUCL it's much easier (see below)
#-cmu
(defmacro with-system-environment ((&rest var-and-values) &body body)
  (be gensym-alist (mapcar #'(lambda (vv)
			       (list (gensym) (string (car vv)) (cadr vv)))
			   var-and-values)
      `(let ,(mapcar #'(lambda (vv)
			 (destructuring-bind (varsym var value) vv
			   (declare (ignore value))
			   `(,varsym (getenv ,var))))
		     gensym-alist)
	 (unwind-protect
	      (progn
		,@(mapcar #'(lambda (vv)
			      (destructuring-bind (varsym var value) vv
				(declare (ignore varsym))
				`(setenv ,var ,value)))
			  gensym-alist)
		,@body)
	   ,@(mapcar #'(lambda (vv)
			 (destructuring-bind (varsym var value) vv
			   (declare (ignore value))
			   `(if ,varsym
				(setenv ,var ,varsym)
				(unsetenv ,var))))
		     gensym-alist)))))

#+cmu
(defmacro with-system-environment ((&rest var-and-values) &body body)
  `(let ((ext:*environment-list*
	  (append (list ,@(mapcar #'(lambda (vv)
				      (destructuring-bind (variable value) vv
					`(cons ,(keywordify variable)
					       ,value)))
				  var-and-values))
		  ext:*environment-list*)))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun last-member (item list &key key (test #'eq))
  "Return the last sublist in LIST that is prefixed by ITEM."
  (loop
     with l = list and result = nil
     for l2 = (member item l :key key :test test)
     while l2
     do (setf result l2
	      l (cdr l2))
     finally (return result)))


(defun glob->regex (string)
  "Convert a shell glob expression into a regular expression string."
  (with-output-to-string (out)
    ;; globs are always anchored to beginning and end
    (write-char #\^ out)
    (loop
       for i from 0 below (length string)
       do (be c (char string i)
	    (cond ((char= c #\\)
		   (setf c (char string (incf i))))
		  ((find c  ".+()|^$")
		   (write-char #\\ out))
		  ((char= c #\*)
		   (write-char #\. out))
		  ((char= c #\?)
		   (setf c #\.)))
	    (write-char c out)))
    (write-char #\$ out)))
