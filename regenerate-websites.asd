;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

#| simple-header

Author: Gary King

DISCUSSION

|#

(in-package :common-lisp-user)
(defpackage :asdf-regenerate-websites (:use #:asdf #:cl))
(in-package :asdf-regenerate-websites)

(defsystem regenerate-websites
  :author "Gary Warren King <gwking@metabang.com>"
  :version "0.6"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "Various license"
  :components ((:module
		"dev"
		:components 
		((:static-file "notes.text")
                                     
		 (:file "package")
		 (:file "definitions"
			:depends-on ("package"))
		 (:file "class-defs"
			:depends-on ("package"))
		 (:file "macros"
			:depends-on ("package"))
		 (:file "utilities"
			:depends-on ("definitions"))
		 (:file "regenerate-websites"
			:depends-on ("utilities"))
		 (:file "changelogs"
			:depends-on ("utilities"))
		 (:file "bits-and-pieces"
			:depends-on ("utilities"))
		 (:file "commands"
			:depends-on ("utilities"))
		 (:file "document-websites"
			:depends-on ("regenerate-websites")))))
  :depends-on (:metatilities-base 
	       :tinaa
	       :xmls
	       :cl-markdown))

