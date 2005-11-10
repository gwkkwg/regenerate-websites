;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

#| simple-header

Author: Gary King

DISCUSSION

|#

(in-package :common-lisp-user)
(defpackage :asdf-regenerate-websites (:use #:asdf #:cl))
(in-package :asdf-regenerate-websites)

;;; ---------------------------------------------------------------------------

(defsystem regenerate-websites
  :author "Gary Warren King <gwking@metabang.com>"
  :version "0.5"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "Various license"
  :components ((:file "package")
               (:file "class-defs"
                      :depends-on ("package"))
               (:file "macros"
                      :depends-on ("package"))
               (:file "utilities"
                      :depends-on ("package"))
               (:file "regenerate-websites"
                      :depends-on ("package" "utilities")))
  :depends-on (metatilities-base lml2))                    

