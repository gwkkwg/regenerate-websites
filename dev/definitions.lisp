(in-package #:regenerate-websites)

(defvar *common-links* (make-container 'simple-associative-container))
(defvar *website-source* nil)
(defvar *website-output* nil)
(defvar *force-rebuild?* nil)
(defvar *current-system* nil)

(defparameter *metabang-common-lisp-systems* nil
  "List of system information")