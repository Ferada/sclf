;;;
;;; Code freely lifted from various places with compatible license
;;; terms.  Most of this code is copyright Daniel Barlow
;;; <dan@metacircles.com> or Gilbert Baumann
;;; <unk6@rz.uni-karlsruhe.de>.  The bugs are copyright Walter
;;; C. Pelissero <walter@pelissero.de>.
;;;

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

(in-package :sclf)

(defstruct (process
	     (:constructor %make-process)
	     (:predicate processp))
  name
  state
  whostate
  function
  thread)

(defvar *current-process*
  (%make-process
   :name "initial process" :function nil
   :thread
   #+#.(cl:if (cl:find-symbol "THREAD-NAME" "SB-THREAD") '(and) '(or))
   sb-thread:*current-thread*
   #-#.(cl:if (cl:find-symbol "THREAD-NAME" "SB-THREAD") '(and) '(or))
   (sb-thread:current-thread-id)))

(defvar *all-processes* (list *current-process*))

(defvar *all-processes-lock*
  (sb-thread:make-mutex :name "Lock around *ALL-PROCESSES*"))

;; we implement disable-process by making the disablee attempt to lock
;; *permanent-queue*, which is already locked because we locked it
;; here.  enable-process just interrupts the lock attempt.

(defmacro get-mutex (mutex &optional (wait t))
  `(
    #+#.(cl:if (cl:find-symbol "GRAB-MUTEX" "SB-THREAD") '(and) '(or))
	sb-thread:grab-mutex
	#-#.(cl:if (cl:find-symbol "GRAB-MUTEX" "SB-THREAD") '(and) '(or))
	sb-thread:get-mutex
	,mutex :waitp ,wait))

(defvar *permanent-queue*
  (sb-thread:make-mutex :name "Lock for disabled threads"))
(unless (sb-thread:mutex-owner *permanent-queue*)
  (get-mutex *permanent-queue* nil))

(defun make-process (function &key name)
  (let ((p (%make-process :name name
			  :function function)))
    (sb-thread:with-mutex (*all-processes-lock*)
      (pushnew p *all-processes*))
    (restart-process p)))

(defun process-kill-thread (process)
  (let ((thread (process-thread process)))
    (when (and thread
	       (sb-thread:thread-alive-p thread))
      (assert (not (eq thread sb-thread:*current-thread*)))
      (sb-thread:terminate-thread thread)
      ;; Wait until all the clean-up forms are done.
      (sb-thread:join-thread thread :default nil))
    (setf (process-thread process) nil)))

(defun process-join (process)
  (sb-thread:join-thread (process-thread process)))

(defun restart-process (p)
  (labels ((boing ()
	     (let ((*current-process* p)
		   (function (process-function p)))
	       (when function
		 (funcall function)))))
    (process-kill-thread p)
    (when (setf (process-thread p)
		(sb-thread:make-thread #'boing :name (process-name p)))
      p)))

(defun destroy-process (process)
  (sb-thread:with-mutex (*all-processes-lock*)
    (setf *all-processes* (delete process *all-processes*)))
  (process-kill-thread process))

(defun current-process ()
  *current-process*)

(defun all-processes ()
  ;; we're calling DELETE on *ALL-PROCESSES*.  If we look up the value
  ;; while that delete is executing, we could end up with nonsense.
  ;; Better use a lock (or call REMOVE instead in DESTROY-PROCESS).
  (sb-thread:with-mutex (*all-processes-lock*)
    *all-processes*))

(defun process-yield ()
  (sb-thread:thread-yield))

(defun process-wait (reason predicate)
  (let ((old-state (process-whostate *current-process*)))
    (unwind-protect
	 (progn
	   (setf old-state (process-whostate *current-process*)
		 (process-whostate *current-process*) reason)
	   (until (funcall predicate)
	     (process-yield)))
      (setf (process-whostate *current-process*) old-state))))

(defun process-wait-with-timeout (reason timeout predicate)
  (let ((old-state (process-whostate *current-process*))
	(end-time (+ (get-universal-time) timeout)))
    (unwind-protect
	 (progn
	   (setf old-state (process-whostate *current-process*)
		 (process-whostate *current-process*) reason)
	   (loop 
	      for result = (funcall predicate)
	      until (or result
			(> (get-universal-time) end-time))
	      do (process-yield)
	      finally (return result)))
      (setf (process-whostate *current-process*) old-state))))

(defun process-interrupt (process function)
  (sb-thread:interrupt-thread (process-thread process) function))

(defun disable-process (process)
  (sb-thread:interrupt-thread
   (process-thread process)
   (lambda ()
     (catch 'interrupted-wait (get-mutex *permanent-queue*)))))

(defun enable-process (process)
  (sb-thread:interrupt-thread
   (process-thread process) (lambda () (throw 'interrupted-wait nil))))

(defmacro without-scheduling (&body body)
  (declare (ignore body))
  (error "WITHOUT-SCHEDULING is not supported on this platform."))

(defparameter *atomic-lock*
  (sb-thread:make-mutex :name "atomic incf/decf"))

(defmacro atomic-incf (place)
  `(sb-thread:with-mutex (*atomic-lock*)
    (incf ,place)))

(defmacro atomic-decf (place) 
  `(sb-thread:with-mutex (*atomic-lock*)
    (decf ,place)))

;;; 32.3 Locks

(defun make-lock (&optional name)
  (sb-thread:make-mutex :name name))

(defmacro with-lock-held ((place &key state (wait t) timeout) &body body)
  (declare (ignore timeout))
  (let ((old-state (gensym "OLD-STATE")))
    `(sb-thread:with-mutex (,place :wait-p ,wait)
       (let (,old-state)
	 (unwind-protect
	      (progn
		(when ,state
		  (setf ,old-state (process-state *current-process*))
		  (setf (process-state *current-process*) ,state))
		,@body)
	   (setf (process-state *current-process*) ,old-state))))))


(defun make-recursive-lock (&optional name)
  (sb-thread:make-mutex :name name))

(defmacro with-recursive-lock-held ((place &optional state (wait t) timeout) &body body)
  (declare (ignore wait timeout))
  (let ((old-state (gensym "OLD-STATE")))
  `(sb-thread:with-recursive-lock (,place)
    (let (,old-state)
      (unwind-protect
	   (progn
	     (when ,state
	       (setf ,old-state (process-state *current-process*))
	       (setf (process-state *current-process*) ,state))
	     ,@body)
	(setf (process-state *current-process*) ,old-state))))))

(defun make-condition-variable () (sb-thread:make-waitqueue))

(defun condition-wait (cv lock &optional timeout)
  (if timeout
      (handler-case 
	  (sb-ext:with-timeout timeout
	    (sb-thread:condition-wait cv lock)
	    t)
	(sb-ext:timeout (c)
	  (declare (ignore c))
	  nil))
      (progn (sb-thread:condition-wait cv lock) t)))

(defun condition-notify (cv)
  (sb-thread:condition-notify cv))


(defvar *process-plists* (make-hash-table)
  "Hash table mapping processes to a property list.  This is used by
PROCESS-PLIST.")

(defun process-property-list (process)
  (gethash process *process-plists*))

(defun (setf process-property-list) (value process)
  (setf (gethash process *process-plists*) value))

(defun process-execute (process function)
  (setf (process-function process) function)
  (restart-process process))

(defun process-alive-p (process)
  (sb-thread:thread-alive-p (process-thread process)))
