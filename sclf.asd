;;;  sclf.asd --- system definition

;;;  Copyright (C) 2005, 2006, 2008, 2009 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: SCLF

#+cmu (ext:file-comment "$Module: sclf.asd, Time-stamp: <2013-06-17 15:32:29 wcp> $")

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

(in-package :cl-user)

(defpackage :sclf-system
  (:use :common-lisp :asdf #+asdfa :asdfa))

(in-package :sclf-system)

(defsystem sclf
    :name "SCLF"
    :author "Walter C. Pelissero <walter@pelissero.de>"
    :maintainer "Walter C. Pelissero <walter@pelissero.de>"
    ;; :version "0.0"
    :description "Stray Common Lisp Functions"
    :long-description
    "A collection of Common Lisp functions for the most disparate
uses, too small to fit anywhere else."
    :licence "LGPL"
    :depends-on (#+sbcl :sb-posix)
    :components
    ((:doc-file "README")
     (:file "package")
     (:file "sclf" :depends-on ("package"))
     (:file "sysproc" :depends-on ("package" "sclf"))
     (:file "lazy" :depends-on ("package" "sclf"))
     (:file "directory" :depends-on ("package" "sclf"))
     (:file "time" :depends-on ("package" "sclf"))
     (:file "serial" :depends-on ("package" "sclf"))
     (:module "mp"
	      :depends-on ("package" "sclf")
	      :components
	      ((:doc-file "README")
	       (:file #.(first
			 (list #+cmu "cmu"
			       #+sbcl "sbcl"
			       "unknown")))))))
