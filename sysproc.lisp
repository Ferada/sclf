;;;  sysproc.lisp --- system processes

;;;  Copyright (C) 2008, 2009, 2010 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: sclf

#+cmu (ext:file-comment "$Module: sysproc.lisp $")

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

(defvar *bourne-shell* "/bin/sh")

(defvar *run-verbose* nil
  "If true system commands are displayed before execution and standard
error is not discarded.")

;;
;; SIGINFO is missing in both CMUCL and SBCL
;;

#+cmu
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant unix::siginfo 29)
  (defvar siginfo (unix::make-unix-signal :siginfo unix::siginfo "Information"))
  (export '(unix::siginfo) "UNIX")
  (pushnew siginfo unix::*unix-signals*))

#+sbcl (in-package :sb-posix)
#+sbcl
(eval-when (:execute :compile-toplevel :load-toplevel)
  (unless (find-symbol "SIGINFO" :sb-posix)
    (sb-ext:with-unlocked-packages (:sb-posix)
      (defvar siginfo 29)
      (export '(SIGINFO)))))
#+sbcl (in-package :sclf)

(defun signal-number (signal-name)
  (ecase signal-name
    ((:abrt :abort)
     #+cmu unix:sigabrt
     #+sbcl sb-posix:sigabrt)
    ((:alrm :alarm)
     #+cmu unix:sigalrm
     #+sbcl sb-posix:sigalrm)
    ((:bus :bus-error)
     #+cmu unix:sigbus
     #+sbcl sb-posix:sigbus)
    ((:chld :child)
     #+cmu unix:sigchld
     #+sbcl sb-posix:sigchld)
    ((:cont :continue)
     #+cmu unix:sigcont
     #+sbcl sb-posix:sigcont)
    #+freebsd((:emt :emulate-instruction)
	      #+cmu unix:sigemt
	      #+sbcl sb-posix:sigemt)
    ((:fpe :floating-point-exception)
     #+cmu unix:sigfpe
     #+sbcl sb-posix:sigfpe)
    ((:hup :hangup)
     #+cmu unix:sighup
     #+sbcl sb-posix:sighup)
    ((:ill :illegal :illegal-instruction)
     #+cmu unix:sigill
     #+sbcl sb-posix:sigill)
    ((:int :interrupt)
     #+cmu unix:sigint
     #+sbcl sb-posix:sigint)
    ((:io :input-output)
     #+cmu unix:sigio
     #+sbcl sb-posix:sigio)
    (:kill
     #+cmu unix:sigkill
     #+sbcl sb-posix:sigkill)
    ((:pipe :broke-pipe)
     #+cmu unix:sigpipe
     #+sbcl sb-posix:sigpipe)
    ((:prof :profiler)
     #+cmu unix:sigprof
     #+sbcl sb-posix:sigprof)
    (:quit
     #+cmu unix:sigquit
     #+sbcl sb-posix:sigquit)
    ((:segv :segmentation-violation)
     #+cmu unix:sigsegv
     #+sbcl sb-posix:sigsegv)
    (:stop
     #+cmu unix:sigstop
     #+sbcl sb-posix:sigstop)
    ((:sys :system-call)
     #+cmu unix:sigsys
     #+sbcl sb-posix:sigsys)
    ((:term :terminate)
     #+cmu unix:sigterm
     #+sbcl sb-posix:sigterm)
    ((:trap)
     #+cmu unix:sigtrap
     #+sbcl sb-posix:sigtrap)
    ((:tstp :terminal-stop)
     #+cmu unix:sigtstp
     #+sbcl sb-posix:sigtstp)
    ((:ttin :tty-input)
     #+cmu unix:sigttin
     #+sbcl sb-posix:sigttin)
    ((:ttou :tty-output)
     #+cmu unix:sigttou
     #+sbcl sb-posix:sigttou)
    ((:urg :urgent)
     #+cmu unix:sigurg
     #+sbcl sb-posix:sigurg)
    ((:usr1 :user1)
     #+cmu unix:sigusr1
     #+sbcl sb-posix:sigusr1)
    ((:usr2 :user2)
     #+cmu unix:sigusr2
     #+sbcl sb-posix:sigusr2)
    ((:vtalrm :virtual-timer-alarm)
     #+cmu unix:sigvtalrm
     #+sbcl sb-posix:sigvtalrm)
    ((:winch :window-change :window-size-change)
     #+cmu unix:sigwinch
     #+sbcl sb-posix:sigwinch)
    ((:xcpu :exceeded-cpu)
     #+cmu unix:sigxcpu
     #+sbcl sb-posix:sigxcpu)
    ((:xfsz :exceeded-file-size)
     #+cmu unix:sigxfsz
     #+sbcl sb-posix:sigxfsz)
    ;; oddly this is not defined by neither CMUCL nor SBCL
    (:info 29)))

(defun sysproc-kill (process signal)
  (when (keywordp signal)
    (setf signal (signal-number signal)))
  #+cmu (ext:process-kill process signal)
  #+sbcl (sb-ext:process-kill process signal)
  #-(or sbcl cmu) (error "Don't know how to kill a process"))

(defun sysproc-exit-code (process)
  #+cmu (ext:process-exit-code process)
  #+sbcl (sb-ext:process-exit-code process)
  #-(or sbcl cmu) (error "Don't know how to get a process exit code"))

(defun sysproc-wait (process)
  #+cmu (ext:process-wait process)
  #+sbcl (sb-ext:process-wait process)
  #-(or sbcl cmu) (error "Don't know how to wait a process"))

(defun sysproc-input (process)
  #+cmu (ext:process-input process)
  #+sbcl (sb-ext:process-input process)
  #-(or sbcl cmu) (error "Don't know how to get the process input"))

(defun sysproc-output (process)
  #+cmu (ext:process-output process)
  #+sbcl (sb-ext:process-output process)
  #-(or sbcl cmu) (error "Don't know how to get the process output"))

(defun sysproc-alive-p (process)
  #+cmu (ext:process-alive-p process)
  #+sbcl (sb-ext:process-alive-p process)
  #-(or sbcl cmu) (error "Don't know how to test wether a process might be running"))

(defun sysproc-pid (process)
  #+cmu (ext:process-pid process)
  #+sbcl (sb-ext:process-pid process)
  #-(or sbcl cmu) (error "Don't know how to get the id of a process"))

(defun sysproc-p (thing)
  #+sbcl (sb-ext:process-p thing)
  #+cmu (ext:process-p thing)
  #-(or sbcl cmu) (error "Don't know how to figure out whether something is a system process"))

(defun run-program (program arguments &key (wait t) pty input output error)
  "Run PROGRAM with ARGUMENTS (a list) and return a process object."
  ;; convert arguments to strings
  (setf arguments
	(mapcar #'(lambda (item)
		    (typecase item
		      (string item)
		      (pathname (native-namestring item))
		      (t (format nil "~A" item))))
		arguments))
  (when *run-verbose*
    (unless error
      (setf error t))
    (format t "~&; run-pipe ~A~{ ~S~}~%" program arguments))
  #+cmu (ext:run-program program arguments
			 :wait wait
			 :pty pty
			 :input input
			 :output output
			 :error (or error *run-verbose*))
  #+sbcl (sb-ext:run-program program arguments
			     :search t
			     :wait wait
			     :pty pty
			     :input input
			     :output output
			     :error (or error *run-verbose*))
  #-(or sbcl cmu)
  (error "Unsupported Lisp system."))

(defun run-pipe (direction program arguments &key error)
  "Run PROGRAM with a list of ARGUMENTS and according to DIRECTION
return the input and output streams and process object of that
process."
  (be process (run-program program arguments
			   :wait nil
			   :pty nil
			   :input (when (member direction '(:output :input-output :io))
				    :stream)
			   :output (when (member direction '(:input :input-output :io))
				     :stream)
			   :error error)
    (values (sysproc-output process)
	    (sysproc-input process)
	    process))
  #-(or sbcl cmu)
  (error "Unsupported Lisp system."))

(defun exit-code (process)
  (sysproc-wait process)
  (sysproc-exit-code process))

(defun run-shell-command (fmt &rest args)
  "Run a Bourne Shell command.  Return the exit status of the command."
  (run-program *bourne-shell* (list "-c" (apply #'format nil fmt args))))

(defun run-async-shell-command (fmt &rest args)
  "Run a Bourne Shell command asynchronously. Return a process
object if provided by your Lisp implementation."
  (run-program *bourne-shell* (list "-c" (apply #'format nil fmt args))
	       :wait nil))

(defmacro with-open-pipe ((in out program arguments &key (process (gensym)) error pty) &body forms)
  "Run BODY with IN and OUT bound respectively to an input and an
output stream connected to a system process created by running PROGRAM
with ARGUMENTS.  If IN or OUT are NIL, then don't create that stream."
  (with-gensyms (prg args)
    `(be* ,prg ,program
	  ,args ,arguments
	  ,process (run-program ,prg ,args
				:output ,(case in
					       ((t nil) in)
					       (t :stream))
				:input ,(case out
					      ((t nil) out)
					      (t :stream))
				:wait nil
				:pty ,pty
				,@(when error `(:error ,error)))
       (if ,process
	   (let (,@(case in
			 ((t nil))
			 (t `((,in (sysproc-output ,process)))))
		 ,@(case out
			 ((t nil))
			 (t `((,out (sysproc-input ,process))))))
	     (unwind-protect
		  (progn
		    ,@forms)
	       ,@(case in
		       ((t nil))
		       (t `((close ,in))))
	       ,@(case out
		       ((t nil))
		       (t `((close ,out))))
	       (when (sysproc-alive-p ,process)
		 (sysproc-kill ,process :term))))
	   (error "unable to run ~A~{ ~A~}." ,prg ,args)))))


(defun sysproc-set-signal-callback (signal handler)
  "Arrange HANDLER function to be called when receiving the system
signal SIGNAL."
  (when (keywordp signal)
    (setf signal (signal-number signal)))
  #+cmu (system:enable-interrupt signal handler)
  #+sbcl (sb-sys:enable-interrupt signal handler)
  #-(or cmu sbcl) (error "Don't know how to set a system signal callback."))
