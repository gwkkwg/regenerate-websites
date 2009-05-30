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
   (homepage nil ir)
   (e8el? nil ir)
   (load-system? t ir)
   (vcs t ir)))

(defmethod initialize-instance :after ((object metabang-system) &key)
  (unless (folder object)
    (setf (slot-value object 'folder) (key object)))
  (unless (home-directory object)
    (setf (slot-value object 'home-directory) 
	  (format nil "user-home:darcs;~(~A~);"
		  (string-downcase (ensure-string (key object))))))
  (unless (documentation-package object)
    (setf (slot-value object 'documentation-package)
          (key object))))

(defmethod print-object ((object metabang-system) stream)
  (print-unreadable-object (object stream :type t)
    (prin1 (key object) stream)))

(defun find-system (system-name)
  (find system-name *metabang-common-lisp-systems* 
        :key 'key))

(defun system-property (system-name property)
  (slot-value (find-system system-name) property))

(defun system-home (system-name)
  ;;?? ugh
  (system-property system-name 'home-directory))

;;?? also in tinaa
(defun lml-insert-file (file)
  (if (probe-file file)
      (with-open-file (in file :direction :input)
        (do ((line (read-line in nil 'eof) (read-line in nil 'eof)))
	    ((eq line 'eof))
	  (html (lml-print line))))
    (format *trace-output* "Warning: unable to insert LML file ~S" file)))

(defun website-source-directory (system-name)
  (let ((system (find-system system-name))) 
    (translate-logical-pathname
     (merge-pathnames
      (make-pathname :directory '(:relative "website" "source"))
      (home-directory system)))))

(defun website-output-directory (system-name)
  (let ((system (find-system system-name))) 
    (translate-logical-pathname
     (merge-pathnames
      (make-pathname :directory '(:relative "website" "output"))
      (home-directory system)))))

(defun changelog-source (system-name)
  (let ((source (website-source-directory system-name)))
    (make-pathname
     :type "xml"
     :name "changelog"
   :directory (butlast (pathname-directory source)) 
   :defaults source)))

(defun copy-source-to-output (source)
  (let ((target (output-path-for-source source)))
    #+Ignore
    (when (and (probe-file target)
               (file-newer-than-file-p target source))
      (cerror (format nil "Overwrite existing file with ~S" source)
              'file-error :pathname target))
    (copy-file source target :if-exists :supersede)))


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

(defun output-type-for-source (source-file)
  (let ((type (pathname-type source-file)))
    (cond ((string-equal type "lml")
           "html")
	  ((string-equal type "md")
           "html")
          (t
           type))))

;;; bits and pieces

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
   :homepage "http://common-lisp.net/project/closer/repos/closer-mop")
                   
  (:key :mop-features :name "MOP Feature Tests" 
   :root "http://common-lisp.net/project/closer/features.html"
   :folder ""
   :short-description "Meta-object Protocol evaluator to test a Common-Lisp implementation's MOP support."
   :metabang-software? nil
   :asdf-packaging? t
   :build-website? nil
   :homepage "http://common-lisp.net/project/closer/repos/mop-features")
                   
  (:key :contextl :name "ContextL" 
   :root "http://common-lisp.net/project/closer/contextl.html"
   :folder ""
   :short-description "a CLOS extension for Context-oriented Programming."
   :metabang-software? nil
   :asdf-packaging? t
   :build-website? nil
   :homepage "http://common-lisp.net/project/closer/repos/contextl")
                   
  (:key :lw-compat :name "LW-Compat" 
   :root "http://common-lisp.net/project/closer/downloads/"
   :short-description "Utility functions from the LispWorks library used in the Closer project"
   :folder ""
   :metabang-software? nil
   :asdf-packaging? t
   :build-website? nil
   :homepage "http://common-lisp.net/project/closer/repos/lw-compat"))