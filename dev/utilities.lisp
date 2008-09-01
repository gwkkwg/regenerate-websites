;;;-*- Mode: Lisp; Package: regenerate-websites -*-

(in-package #:regenerate-websites)

(defun markdown-extensions ()
  '(cl-markdown::docs cl-markdown::docs-index
    cl-markdown::today cl-markdown::now
					;	 cl-markdown::footnote cl-markdown::footnotes 
    cl-markdown::glossary
    cl-markdown::metabang-projects-list))

(defun search-locations ()
  `((:search-locations 
     . ,(list
	 (asdf:system-relative-pathname 
	  'regenerate-websites "../shared/"))
     )))

(defun ensure-string (x)
  (typecase x
    (string x)
    (t (format nil "~A" x))))

(defclass* metabang-system ()
  ((name nil ir)
   (key nil ir)
   (sub-folder nil ir)
   (folder nil ir)
   (root nil ir)
   (cliki nil ir)
   (short-description nil ir)

   ;;; descriptive, not-normative
   (asdf-packaging? nil ir)
   (metabang-software? t ir)
   (build-website? t ir)
   
   (build-documentation? nil ia)
   (documentation-package nil ir)
   (home-directory nil ir)
   (darcs-repo nil ir)
   (e8el? nil ir)))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object metabang-system) &key)
  (unless (folder object)
    (setf (slot-value object 'folder) (key object)))
  (unless (home-directory object)
    (setf (slot-value object 'home-directory) 
          (string-downcase (ensure-string (key object)))))
  (unless (documentation-package object)
    (setf (slot-value object 'documentation-package)
          (key object))))

;;; ---------------------------------------------------------------------------

(defmethod print-object ((object metabang-system) stream)
  (print-unreadable-object (object stream :type t)
    (prin1 (key object) stream)))

;;; ---------------------------------------------------------------------------

(defun find-system (system-name)
  (find system-name *metabang-common-lisp-systems* 
        :key 'key))

;;; ---------------------------------------------------------------------------

(defun system-home (system-name)
  (let ((system (find-system system-name)))
    (assert system
	    nil
	    "Error, cannot find system ~S" system-name)
    ;;?? ugh
    (format nil "user-home:darcs;~(~A~);" (home-directory system))))

;;; ---------------------------------------------------------------------------

;;?? also in tinaa
(defun lml-insert-file (file)
  (if (probe-file file)
      (with-open-file (in file :direction :input)
        (do ((line (read-line in nil 'eof) (read-line in nil 'eof)))
	    ((eq line 'eof))
	  (html (lml-princ line))))
    (format *trace-output* "Warning: unable to insert LML file ~S" file)))

;;; ---------------------------------------------------------------------------

(defun website-source-directory (system-name)
  (let ((system (find-system system-name))) 
    (translate-logical-pathname
     (format nil "user-home:darcs;~(~A~);website;source;"
             (home-directory system)))))

;;; ---------------------------------------------------------------------------

(defun website-output-directory (system-name)
  (let ((system (find-system system-name))) 
    (translate-logical-pathname
     (format nil "user-home:darcs;~(~A~);website;output;" 
             (home-directory system)))))

;;; ---------------------------------------------------------------------------

(defun changelog-source (system-name)
  (let ((source (website-source-directory system-name)))
    (make-pathname
     :type "xml"
     :name "changelog"
   :directory (butlast (pathname-directory source)) 
   :defaults source)))

;;; ---------------------------------------------------------------------------

(defun copy-source-to-output (source)
  (let ((target (output-path-for-source source)))
    #+Ignore
    (when (and (probe-file target)
               (file-newer-than-file-p target source))
      (cerror (format nil "Overwrite existing file with ~S" source)
              'file-error :pathname target))
    (copy-file source target :if-exists :supersede)))

;;; ---------------------------------------------------------------------------

(defun output-path-for-source (source-file)
  (translate-pathname
   source-file
   (make-pathname 
    :directory `(,@(pathname-directory *website-source*) :wild-inferiors)
    :name :wild
    :type :wild)
   (make-pathname 
    :directory `(,@(pathname-directory *website-output*) :wild-inferiors)
    :name :wild
    :type (output-type-for-source source-file))))

;;; ---------------------------------------------------------------------------

(defun output-type-for-source (source-file)
  (let ((type (pathname-type source-file)))
    (cond ((string-equal type "lml")
           "html")
	  ((string-equal type "md")
           "html")
          (t
           type))))

(setf *metabang-common-lisp-systems*
      (sort
       (mapcar (lambda (data)
		 (apply #'make-instance 'metabang-system data))
	       '((:key :asdf-binary-locations :name "ASDF-Binary-Locations"
		  :short-description "Put Lisp binaries in their places"
		  :build-documentation? nil
		  :darcs-repo "http://common-lisp.net/project/asdf-binary-locations")
                   
		 (:key :asdf-install-tester
		  :name "ASDF-Install-Tester"
		  :root "http://common-lisp.net/project/ait/"
		  :short-description "Test ASDF Installable systems automagically"
		  :build-documentation? t
		  :home-directory "ait"
		  :folder ""
		  :darcs-repo "http://common-lisp.net/project/tinaa/darcs/ait")
                   
		 (:key :asdf-status :name "ASDF-Status"
		  :sub-folder "cl-containers"
		  :short-description "Display ASDF-Install-testers results nicely"
		  :darcs-repo "http://common-lisp.net/project/cl-containers/asdf-status/darcs/asdf-status")
                   
		 (:key :asdf-system-connections :name "ASDF-System-Connections"
		  :sub-folder "cl-containers"
		  :short-description "Link ASDF systems together declaratively"
		  :build-documentation? t
		  :darcs-repo "http://common-lisp.net/project/cl-containers/asdf-system-connections/darcs/asdf-system-connections")
                   
		 (:key :bundler :name "Bundler"
		  :sub-folder "bundler"
		  :short-description "Bundle many ASDF systems into one"
		  :build-documentation? nil
		  :metabang-software? t
		  :darcs-repo "http://common-lisp.net/project/cl-containers/bundler/darcs/bundler")
                   
		 (:key :cl-containers :name "CL-Containers"
		  :short-description "Common-Lisp's answer to STL and Smalltalk"
		  :build-documentation? t
		  :darcs-repo "http://common-lisp.net/project/cl-containers/darcs/cl-containers")
                   
		 (:key :cl-graph :name "CL-Graph"
		  :short-description "Utilities and algorithms for Graph manipulation"
		  :build-documentation? t
		  :darcs-repo "http://common-lisp.net/project/cl-graph/darcs/cl-graph")
                   
		 (:key :cl-markdown :name "CL-Markdown"
		  :short-description "Common Lisp version of the Markdown text processing langauge"
		  :build-documentation? t
		  :darcs-repo "http://common-lisp.net/project/cl-markdown/darcs/cl-markdown")
                   
		 (:key :cl-mathstats :name "CL-MathStats"
		  :short-description "Miscellaneous math and statistics utilities"
		  :build-documentation? t
		  :darcs-repo "http://common-lisp.net/project/cl-mathstats/darcs/cl-mathstats")
                   
		 (:key :cl-variates :name "CL-Variates"
		  :short-description "Portable Random Number Generators and tools."
		  :build-documentation? t
		  :darcs-repo "http://common-lisp.net/project/cl-variates/darcs/cl-variates")
                   
		 (:key :clnuplot :name "CLNUPlot"
		  :sub-folder "cl-containers"
		  :short-description "Common Lisp interface for GNUPlot"
		  :build-documentation? t
		  :metabang-software? nil
		  :darcs-repo "http://common-lisp.net/project/cl-containers/clnuplot/darcs/clnuplot")
                   
		 (:key :defsystem-compatibility :name "defsystem-compatibility"
		  :sub-folder "cl-containers"
		  :short-description "Help different system definers to live together."
		  :build-documentation? t
		  :darcs-repo "http://common-lisp.net/project/cl-containers/defsystem-compatibility/darcs/defsystem-compatibility")
                   
		 (:key :e8el :name "Enterprise Lisp"
		  :root "http://www.enterpriselisp.com/"
		  :home-directory "enterpriselisp"
		  :metabang-software? nil
		  :build-documentation? nil
		  )
                   
		 (:key :lift :name "LIFT"
		  :short-description "the LIsp Framework for Testing"
		  :build-documentation? t
		  :documentation-package lift
		  :darcs-repo "http://common-lisp.net/project/lift/darcs/lift")
                   
		 (:key :log5 :name "log5"
		  :short-description "Common Lisp Logging - It's one more"
		  :build-documentation? t
		  :darcs-repo "http://common-lisp.net/project/log5/darcs/log5")

		 (:key :metacopy :name "metacopy"
		  :short-description "Shallow and deep copy toolkit"
		  :build-documentation? t
		  :darcs-repo "http://common-lisp.net/project/metacopy/darcs/metacopy")
                   
		 (:key :metatilities :name "Metatilities" 
		  :short-description "Various useful utilities"
		  :build-documentation? t
		  :darcs-repo "http://common-lisp.net/project/metatilities/darcs/metatilities")
                   
		 (:key :moptilities :name "Moptilities"
		  :short-description "Implementation independent MOP utilities"
		  :build-documentation? t
		  :darcs-repo "http://common-lisp.net/project/moptilities")
                   
		 (:key :metabang-bind :name "metabang-bind" 
		  :cliki "bind"
		  :short-description "Handle destructuring, multiple-values and let simultaneously"
		  :build-documentation? t
		  :home-directory "metabang-bind"
		  :darcs-repo "http://common-lisp.net/project/metabang-bind")
                   
		 (:key :simple-advice :name "simple-advice"
		  :short-description "Portable Common Lisp advice facility"
		  :build-documentation? nil
		  :darcs-repo "http://common-lisp.net/project/simple-advice/")
		 (:key :simple-http :name "Simple HTTP" 
		  :short-description "Not as trivial as trivial HTTP, but still simple..."
		  :build-documentation? t
		  :darcs-repo "http://common-lisp.net/project/simple-http"
		  :e8el? t)
		 (:key :trivial-http :name "Trivial HTTP" 
		  :short-description "You say simple, I say trivial"
		  :build-documentation? t
		  :darcs-repo "http://common-lisp.net/project/trivial-http"
		  :e8el? t)

		 (:key :geohash :name "Geohash in Common Lisp" 
		  :short-description "Implements the geohash.org algorithm"
		  :build-documentation? t
		  :darcs-repo "http://common-lisp.net/project/geohash"
		  :e8el? t)
		 (:key :system-check :name "System-check" 
		  :short-description "Keeping your systems up to date"
		  :root "http://metabang.gotdns.com/software/" 
		  :build-documentation? t
		  :darcs-repo "http://metabang.gotdns.com/software/system-check/darcs/system-check"
		  :e8el? t)
                   
		 (:key :tinaa :name "TINAA"
		  :short-description "Common-Lisp documentation tool"
		  :build-documentation? t
		  :darcs-repo "http://common-lisp.net/project/tinaa/darcs/tinaa")
                   
		 (:key :trivial-shell :name "trivial-shell"
		  :short-description "One shell to run them all"
		  :build-documentation? t
		  :darcs-repo "http://common-lisp.net/project/trivial-shell/darcs/trivial-shell")
                   
		 (:key :metasite :name "metabang.com" 
		  :root "http://www.metabang.com/"
		  :folder ""
		  :metabang-software? nil)
                   
		 (:key :metabang-local :name "metabang.com" 
		  :root "http://metabang.gotdns.com/"
		  :folder ""
		  :home-directory "metasite;metabang-local"
		  :metabang-software? nil)             
                   
		 (:key :cl-html-parse :name "Common Lisp HTML Parser" 
		  :short-description "A portable version of Franz's Opensource HTML Parser"
		  :metabang-software? nil
		  :asdf-packaging? t
		  :build-website? nil
		  :build-documentation? t
		  :darcs-repo "http://metabang.gotdns.com/software/system-check/darcs/system-check"
		  :e8el? t)
                   
		 (:key :portable-threads :name "Portable Threads"
		  :root "http://gbbopen.org/hyperdoc/ref-portable-thread-entities.html"
		  :short-description "A Portable Thread Library originally developed for GBBOpen."
		  :folder ""
		  :metabang-software? nil
		  :asdf-packaging? t
		  :build-website? nil)
                   
		 (:key :asdf-install :name "ASDF-Install"
		  :short-description "A tool for downloading and installing lisp libraries and packages."
		  :metabang-software? nil
		  :asdf-packaging? t
		  :build-website? t)

		 (:key :dynamic-classes :name "dynamic-classes"
		  :short-description "Classes the way you want them"
		  :build-documentation? t
		  :darcs-repo "http://common-lisp.net/project/dyanmic-classes/")
		   
		 (:key :metatilities-base :name "metatilities-base"
		  :short-description "something to stand on"
		  :build-documentation? t
		  :darcs-repo "http://common-lisp.net/project/metatilities-base/")

		 (:key :docudown :name "docudown"
		  :short-description "CL-Markdown does documentation, details at 11"
		  :build-documentation? t
		  :darcs-repo "http://common-lisp.net/project/docudown/")
		 (:key :trivial-backtrace :name "trivial-backtrace"
		  :short-description "a backrace for the rest of us."
		  :build-documentation? t
		  :darcs-repo "http://common-lisp.net/project/trivial-backtrace/")))
       #'string-lessp
       :key #'key))

;;; ---------------------------------------------------------------------------
;;; bits and pieces
;;; ---------------------------------------------------------------------------

(defun button-img-src (image-name)
  (format nil "http://common-lisp.net/project/cl-containers/shared/buttons/~A"
          image-name))


#+(or)
'(
  (:key :closer-mop :name "Closer to MOP" 
   :root "http://common-lisp.net/project/closer/closer-mop.html"
   :short-description "A compatibility layer to paper over Common-Lisp implmentation difference in MOP support."
   :folder ""
   :metabang-software? nil
   :asdf-packaging? t
   :build-website? nil
   :darcs-repo "http://common-lisp.net/project/closer/repos/closer-mop")
                   
  (:key :mop-features :name "MOP Feature Tests" 
   :root "http://common-lisp.net/project/closer/features.html"
   :folder ""
   :short-description "Meta-object Protocol evaluator to test a Common-Lisp implementation's MOP support."
   :metabang-software? nil
   :asdf-packaging? t
   :build-website? nil
   :darcs-repo "http://common-lisp.net/project/closer/repos/mop-features")
                   
  (:key :contextl :name "ContextL" 
   :root "http://common-lisp.net/project/closer/contextl.html"
   :folder ""
   :short-description "a CLOS extension for Context-oriented Programming."
   :metabang-software? nil
   :asdf-packaging? t
   :build-website? nil
   :darcs-repo "http://common-lisp.net/project/closer/repos/contextl")
                   
  (:key :lw-compat :name "LW-Compat" 
   :root "http://common-lisp.net/project/closer/downloads/"
   :short-description "Utility functions from the LispWorks library used in the Closer project"
   :folder ""
   :metabang-software? nil
   :asdf-packaging? t
   :build-website? nil
   :darcs-repo "http://common-lisp.net/project/closer/repos/lw-compat"))