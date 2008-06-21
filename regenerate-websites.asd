;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

#| simple-header

Author: Gary King

DISCUSSION

|#

(in-package :common-lisp-user)
(defpackage :asdf-regenerate-websites (:use #:asdf #:cl))
(in-package :asdf-regenerate-websites)

;; try hard
(unless (find-system 'asdf-system-connections nil)
  (warn "The regenerate-websites system would enjoy having ~
asdf-system-connections around. See 
http://www.cliki.net/asdf-system-connections for details and download
instructions."))
(when (find-system 'asdf-system-connections nil)
  (operate 'load-op 'asdf-system-connections))

(defsystem regenerate-websites
  :author "Gary Warren King <gwking@metabang.com>"
  :version "0.6"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "Various license"
  :components ((:module
		"setup"
		:pathname "dev/"
		:components
		((:file "package")))
	       (:module
		"dev"
		:depends-on ("setup")
		:components 
		((:static-file "notes.text")
		 (:file "definitions")
		 (:file "class-defs")
		 (:file "macros")
		 (:file "utilities"
			:depends-on ("definitions"))
		 (:file "regenerate-websites"
			:depends-on ("utilities"))
		 (:file "changelogs"
			:depends-on ("utilities"))
		 (:file "bits-and-pieces"
			:depends-on ("utilities"))
		 (:file "commands"
			:depends-on ("utilities")))))
  :depends-on ((:version :metatilities-base "0.6.2")
	       :xmls
	       :docudown
	       :lml2))

#+asdf-system-connections
(asdf:defsystem-connection rw-and-tinaa
  :requires (:regenerate-websites :tinaa)
  :components ((:module 
		"dev"
		:components ((:file "document-websites")))))


