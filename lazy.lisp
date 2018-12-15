;;;  lazy.lisp --- lazy primitives

;;;  Copyright (C) 2008, 2009, 2010 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: sclf

#+cmu (ext:file-comment "$Module: lazy.lisp $")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lazy primitives
;;;

(in-package :sclf)

(defstruct promise
  procedure
  value)

(defmacro lazy (form)
  `(make-promise :procedure #'(lambda () ,form)))

(defun forced-p (promise)
  (null (promise-procedure promise)))

(defun force (promise)
  (if (forced-p promise)
      (promise-value promise)
      (prog1 (setf (promise-value promise)
		   (funcall (promise-procedure promise)))
	(setf (promise-procedure promise) nil))))

(defmacro deflazy (name value &optional documentation)
  `(defparameter ,name (lazy ,value)
     ,@(when documentation
	     (list documentation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass lazy-metaclass (standard-class)
  ()
  (:documentation "Metaclass for object having lazy slots.  Lazy slots
should be specified with the :LAZY keyword which must be a function of
one argument.  If required this function will be called once to get
the value to memoize in the slot.  Lazy slots can also be set/read as
any other."))

(defmethod validate-superclass ((class lazy-metaclass) (super standard-class))
  "Lazy classes may inherit from ordinary classes."
  (declare (ignore class super))
  t)

(defmethod validate-superclass ((class standard-class) (super lazy-metaclass))
  "Ordinary classes may inherit from lazy classes."
  (declare (ignore class super))
  t)

(defclass lazy-slot-mixin ()
  ((lazy-function :initarg :lazy
		   :reader lazy-slot-function
		   :initform nil))
  (:documentation
   "Slot for LAZY-METACLASS classes.  Lazy slots must be declared with
the argument :LAZY which must be a function accepting the object
instance as argument."))

(defclass lazy-direct-slot-definition (lazy-slot-mixin standard-direct-slot-definition)
  ())

(defclass lazy-effective-slot-definition (lazy-slot-mixin standard-effective-slot-definition)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod direct-slot-definition-class ((class lazy-metaclass) &rest initargs)
  (if (getf initargs :lazy nil)
      (find-class 'lazy-direct-slot-definition)
      (call-next-method)))

(defmethod effective-slot-definition-class ((class lazy-metaclass) &rest initargs)
  (if (getf initargs :lazy nil)
      (find-class 'lazy-effective-slot-definition)
      (call-next-method)))

(defmethod compute-effective-slot-definition-initargs ((class lazy-metaclass) direct-slots)
  (let ((ds (car direct-slots)))
    (if (typep ds 'lazy-direct-slot-definition)
      (let ((form (lazy-slot-function ds))
	    (args (call-next-method)))
	(when (or (getf args :initarg)
		  (getf args :initform))
	  (error "Lazy slot ~S cannot have :INITARG nor :INITFORM arguments." ds))
	(list* :lazy
	       (cond ((and (listp form)
			   (eq 'lambda (car form)))
		      (compile nil form))
		     ((symbolp form)
		      form)
		     (t (compile nil `(lambda (self)
					(declare (ignorable self))
					,form))))
	       args))
      (call-next-method))))

(defmethod slot-value-using-class ((class lazy-metaclass) instance (slot lazy-slot-mixin))
  (declare (ignore class))
  ;; If the slot is unbound, call the lazy function passing the
  ;; instance and memoize the value in the slot.
  (unless (slot-boundp-using-class class instance slot)
    (setf (slot-value-using-class class instance slot)
	  (funcall (lazy-slot-function slot) instance)))
  (call-next-method))

(defun reset-lazy-slots (object)
  "Unbind all the lazy slots in OBJECT so that they will be
re-evaluated next time their value is requested again."
  (be* class (class-of object)
    (dolist (slot (class-slots class))
      (when (typep slot 'lazy-effective-slot-definition)
	(slot-makunbound object (slot-definition-name slot))))))