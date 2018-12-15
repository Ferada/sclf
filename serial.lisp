 ;;; serial.lisp --- serialisation of CLOS objects

 ;;; Copyright (C) 2009 by Walter C. Pelissero

 ;;; Author: Walter C. Pelissero <walter@pelissero.de>
 ;;; Project: sclf

#+cmu (ext:file-comment "$Module: serial.lisp $")

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

(in-package :sclf)

(defclass printable-object-mixin () ())

(defmacro reconstruct-object (class &rest args)
  `(apply #'make-instance ',class ',args))

(defun print-readable-instance (object &optional stream)
  (unless stream
    (setf stream *standard-output*))
  (be class (class-of object)
    (pprint-logical-block (stream (copy-list (class-slots class)) :prefix "#.(" :suffix ")")
      (flet ((spc ()
	       (write-char #\space stream)))
	(write 'reconstruct-object :stream stream)
	(spc)
	(write (class-name class) :stream stream :escape t :readably t :pretty t)
	(pprint-exit-if-list-exhausted)
	(spc)
	(loop
	   (be* slot (pprint-pop)
		slot-name (slot-definition-name slot)
		initarg (car (slot-definition-initargs slot))
	     (when (and initarg
			(slot-boundp object slot-name))
	       (write initarg :stream stream)
	       (spc)
	       (when *print-pretty*
		 (pprint-newline :miser stream))
	       (write (slot-value object slot-name)
		      :stream stream)
	       (pprint-exit-if-list-exhausted)
	       (if *print-pretty*
		   (pprint-newline :linear stream)
		   (spc)))))))))

(defmethod print-object ((object printable-object-mixin) stream)
  (if *print-readably*
      (print-readable-instance object stream)
      (call-next-method)))
