(in-package #:regenerate-websites)

(unless (ignore-errors (logical-pathname-translations "user-home"))
  (setf (logical-pathname-translations "user-home")
	`(("**;*.*" ,(namestring 
		      (make-pathname 
		       :directory
		       (append (pathname-directory 
				(user-homedir-pathname))
			       (list :wild-inferiors))))))))

(defvar *common-links* (make-container 'simple-associative-container))
(defvar *website-source* nil)
(defvar *website-output* nil)
(defvar *force-rebuild?* nil)
(defvar *current-system* nil)

(defparameter *metabang-common-lisp-systems* nil
  "List of system information")