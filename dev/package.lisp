(in-package #:common-lisp)

(defpackage #:regenerate-websites
  (:use #:common-lisp #:metatilities #:lml2 #:cl-containers)
  (:nicknames #:rw)
  (:shadowing-import-from #:metatilities
                          #:copy-file)
  (:export #:link 
           #:set-link-info
           #:*metabang-common-lisp-systems*
           #:regenerate-website
           #:regenerate-websites
	   #:create-tinaa-documentation-for-system
	   #:create-tinaa-documentation-for-systems
           #:software?
           #:key
           #:short-description
	   #:asdf-packaging? 
	   #:metabang-software?))
