;;;  package.lisp --- packages description

;;;  Copyright (C) 2006, 2007, 2008, 2009, 2010 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: sclf

#+cmu (ext:file-comment "$Module: package.lisp $")

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

(defpackage :sclf
  (:use :common-lisp
	;; we need the MOP for lazy.lisp and serial.lisp
	#+cmu :pcl
	#+sbcl :sb-mop)
  ;; Don't know why but compute-effective-slot-definition-initargs is
  ;; internal in both CMUCL and SBCL
  (:import-from #+cmu"PCL" #+sbcl"SB-PCL"
		#-(or cmu sbcl) "CLOS"
		"COMPUTE-EFFECTIVE-SLOT-DEFINITION-INITARGS")
  #+cmu (:import-from :mp
		      #:make-process
		      #:current-process
		      #:all-processes
		      #:processp
		      #:process-name
		      #:process-state
		      #:process-whostate
		      #:process-wait
		      #:process-wait-with-timeout
		      #:process-yield
		      #:process-interrupt
		      #:disable-process
		      #:enable-process
		      #:without-scheduling
		      #:atomic-incf
		      #:atomic-decf
		      #:process-property-list)
  (:export #:be #:be*
	   #:defconst
	   #:with-gensyms
	   #:d+
	   #:s+
	   #:f++
	   #:list->string
	   #:string-starts-with #:string-ends-with
	   #:aif #:awhen #:acond #:aand #:acase #:it
	   #:+whitespace+
	   #:string-trim-whitespace
	   #:string-right-trim-whitespace
	   #:string-left-trim-whitespace
	   #:whitespace-p #:seq-whitespace-p
	   #:not-empty
	   #:position-any
	   #:+month-names+
	   #:find-any
	   #:split-at
	   #:split-string-at-char
	   #:week-day->string
	   #:month->string
	   #:month-string->number
	   #:add-months #:add-days
	   #:read-whole-stream
	   #:read-file #:write-file #:read-lines
	   #:read-from-file #:write-to-file
	   #:string-concat
	   #:gcase
	   #:string-truncate
	   #:promise #:force #:forced-p #:lazy #:deflazy #:lazy-metaclass #:self #:reset-lazy-slots
	   #:copy-stream #:copy-file
	   #:symlink-file
	   #:keywordify
	   #:until
	   #:year #:month #:day #:hour #:minute #:week-day #:week #:day-of-the-year
	   #:beginning-of-week #:end-of-week
	   #:next-week-day #:next-monday #:full-weeks-in-span
	   #:beginning-of-first-week #:end-of-last-week
	   #:beginning-of-month #:end-of-month
	   #:locate-system-program
	   #:*tmp-file-defaults*
	   #:temp-file-name
	   #:open-temp-file
	   #:with-temp-file
	   #:file-size
	   #:getenv
	   #:with-system-environment
	   #:time-string #:iso-time-string #:parse-iso-time-string
	   #:soundex
	   #:string-soundex=
	   #:lru-cache
	   #:getcache #:cached
	   #:print-time-span
	   #:double-linked-list #:limited-list #:sorted-list
	   #:insert #:size
	   #:heap #:heap-add #:heap-pop #:heap-empty-p
	   #:double-linked-element #:make-double-linked-element #:double-linked-element-p
	   #:dle-previous #:dle-next #:dle-value
	   #:cons-dle #:dle-remove #:dle-map #:do-dle :do-dle*
	   #:sl-map #:do-dll #:do-dll*
	   #:dll-find #:dll-find-cursor
	   #:push-first #:push-last #:dll-remove
	   #:pop-first #:pop-last
	   #:leap-year-p #:last-day-of-month
	   #:getuid #:setuid #:with-euid
	   #:get-logname #:get-user-name #:get-user-home #:find-uid
	   #:super-user-p
	   #:pathname-as-directory #:pathname-as-file
	   #:alist->plist #:plist->alist
	   #:byte-vector->string
	   #:string->byte-vector
	   #:outdated-p
	   #:with-hidden-temp-file
	   #:let-places #:let-slots
	   #:*decimal-point*
	   #:*thousands-comma*
	   #:format-amount #:parse-amount
	   #:with-package
	   #:make-directory #:ensure-directory
	   #:make-temp-directory
	   #:with-temp-directory
	   #:delete-directory
	   #:delete-directory-tree
	   #:do-directory-tree
	   #:traverse-directory-tree
	   #:empty-directory-p
	   #:remove-empty-directories
	   #:map-directory-tree
	   #:find-files
	   #:directory-p
	   #:regular-file-p
	   #:file-readable-p
	   #:file-writable-p
	   #:file-executable-p
	   #:current-directory
	   #:ensure-home-translations
	   #:list-directory
	   #:string-escape
	   #:string-substitute
	   #:bytes-simple-string
	   #:make-lock-files
	   #:with-lock-files
	   #:getpid
	   #:on-error
	   #:floor-to
	   #:round-to
	   #:ceiling-to
	   #:insert-in-order
	   #:forget-documentation
	   #:load-compiled
	   #:swap
	   #:queue #:make-queue #:queue-append #:queue-pop #:queue-empty-p
	   #:unix-stat #:unix-file-stat
	   #:stat-device
	   #:stat-inode
	   #:stat-links
	   #:stat-atime
	   #:stat-mtime
	   #:stat-ctime
	   #:stat-birthtime
	   #:stat-size
	   #:stat-blksize
	   #:stat-blocks
	   #:stat-uid
	   #:stat-gid
	   #:stat-mode
	   #:save-file-excursion
	   #:stat-modification-time
	   #:file-modification-time
	   #:file-creation-time
	   #:show
	   #:memoize-function
	   #:memoized
	   #:defun-memoized
	   #:parse-native-namestring
	   #:native-file-namestring
	   #:native-namestring
	   #:native-pathname
	   #:read-symbolic-link
	   #:symbolic-link-p
	   #:broken-link-p
	   #:circular-list
	   #:last-member
	   #:glob->regex
	   #:universal->unix-time #:unix->universal-time
	   #:get-unix-time
	   #:move-file

	   ;; sysproc.lisp
	   #:*run-verbose*
	   #:run-pipe
	   #:run-program
	   #:run-shell-command
	   #:run-async-shell-command
	   #:exit-code
	   #:with-open-pipe
	   #:*bourne-shell*
	   #:sysproc-kill
	   #:sysproc-input
	   #:sysproc-output
	   #:sysproc-alive-p
	   #:sysproc-pid
	   #:sysproc-p
	   #:sysproc-wait
	   #:sysproc-exit-code
	   #:sysproc-set-signal-callback

	   ;; MP
	   #:make-process
	   #:destroy-process
	   #:current-process
	   #:all-processes
	   #:processp
	   #:process-name
	   #:process-state
	   #:process-whostate
	   #:process-wait
	   #:process-wait-with-timeout
	   #:process-yield
	   #:process-interrupt
	   #:disable-process
	   #:enable-process
	   #:restart-process
	   #:without-scheduling
	   #:atomic-incf
	   #:atomic-decf
	   #:process-property-list
	   #:process-alive-p
	   #:process-join
	   ;;
	   #:make-lock
	   #:with-lock-held
	   #:make-recursive-lock
	   #:with-recursive-lock-held
	   ;;
	   #:make-condition-variable
	   #:condition-wait
	   #:condition-notify
	   #:process-property-list
	   #:process-execute
	   ;; mop.lisp
	   #:printable-object-mixin
	   ))
