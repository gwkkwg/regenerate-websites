;;;-*- Mode: Lisp; Package: regenerate-websites -*-

(in-package #:regenerate-websites)

(defun regenerate-websites (&key (force? nil))
  (loop for system in *metabang-common-lisp-systems* 
        when (build-website? system) do
       (print system) 
       (regenerate-website (key system) :force? force?)))

(defun regenerate-website (system-name &key (force? nil) (load-system? t))
  (when load-system?
    (asdf:oos 'asdf:load-op system-name))
  (let ((lml2::*output-dir* (website-output-directory system-name))
        (*package* *package*)
        (*website-source* (website-source-directory system-name))
        (*website-output* (website-output-directory system-name))
        (*force-rebuild?* force?)
        (*current-system* system-name))
    (format t "~%Source: ~A~%Output: ~A"
	    *website-source* *website-output*)
    (ensure-directories-exist *website-output*)
    (mapc 
     (lambda (file)
       (unless (ignore-file-p file)
	 (print file)
	 (regenerate-file 
	  (form-keyword (pathname-type file)) file)))
     (directory (merge-pathnames
		 *website-source*
		 (make-pathname :name :wild :type :wild
				:directory '(:relative :wild-inferiors)))))
    #-DIGITOOL
    (create-changelog system-name)
    #-DIGITOOL
    (create-changelog-page system-name)))

(defun ignore-file-p (file)
  (let ((filetype (pathname-type file))
	(filename (pathname-name file)))
    (or (not filetype)
	(char= #\~ (aref filetype (1- (length filetype))))
	(string= ".#" (subseq filename 0 2)))))

(defgeneric regenerate-file (kind file)
  (:documentation ""))

(defmethod regenerate-file (kind file)
  (warn "I have no idea what to do with ~A of kind ~A" file kind))

(defmethod regenerate-file ((kind (eql :lml)) file)
  #+(or)
  (format t "~%~A ~A~%    ~A"
	  (file-newer-than-file-p 
	   file (output-path-for-source file)) 
	  file
	  (output-path-for-source file))
  (when (or *force-rebuild?*
            (file-newer-than-file-p 
             file (output-path-for-source file))) 
    (let ((lml2::*output-dir* (output-path-for-source file)))
      (lml2::lml-load file))))

(defmethod regenerate-file ((kind (eql :md)) file)
  #+(or)
  (format t "~%~s  ~s"
	  (file-newer-than-file-p 
	   file (output-path-for-source file)) 
	  (output-path-for-source file))
  (when (or *force-rebuild?*
            (file-newer-than-file-p 
             file (output-path-for-source file))) 
         ;;?? hack (?) to handle markdown extensions...
    (let (#+(or) (*package* (find-package :cl-markdown)))
      (markdown:markdown 
       file :stream
       (output-path-for-source file) :format :html
       :additional-extensions
       '(cl-markdown::docs cl-markdown::docs-index
	 cl-markdown::today cl-markdown::now
;	 cl-markdown::footnote cl-markdown::footnotes 
	 cl-markdown::glossary
	 cl-markdown::metabang-projects-list)

       :properties `((:search-locations 
		      . ,(list
			  (asdf:system-relative-pathname 
			   'regenerate-websites "../shared/"))
		      ))))
    (copy-file file (make-pathname 
		     :type "text"
		     :defaults (output-path-for-source file))
	       :if-exists :supersede)))

(defmethod regenerate-file ((kind (eql :css)) file)
  (copy-source-to-output file))

(defmethod regenerate-file ((kind (eql :vcf)) file)
  (copy-source-to-output file))

(defmethod regenerate-file ((kind (eql :xml)) file)
  (copy-source-to-output file))

(defmethod regenerate-file ((kind (eql :swf)) file)
  ;; Flash animation
  (copy-source-to-output file))

(defmethod regenerate-file ((kind (eql :ico)) file)
  (copy-source-to-output file))

(defmethod regenerate-file ((kind (eql :png)) file)
  (copy-source-to-output file))

(defmethod regenerate-file ((kind (eql :gif)) file)
  (copy-source-to-output file))

(defmethod regenerate-file ((kind (eql :zip)) file)
  (copy-source-to-output file))

(defmethod regenerate-file ((kind (eql :jpg)) file)
  (copy-source-to-output file))

(defmethod regenerate-file ((kind (eql :pdf)) file)
  (copy-source-to-output file))

(defmethod regenerate-file ((kind (eql :asc)) file)
  ;; PGP files
  (copy-source-to-output file))

(defmethod regenerate-file ((kind (eql :html)) file)
  ;; Take HTML directly
  (copy-source-to-output file))

#+Ignore
(defmethod regenerate-file ((kind (eql :xml)) file)
  (process-xml-file (pathname-name file) file))

#|
(in-package #:few)

#+Ignore
;; suck site up and output as LML
(mapc 
 (lambda (file)
   (let ((html (net.html.parser:parse-html file)))
     (setf html (remove-if (lambda (x)
                             (and (consp x)
                                  (member (first x) '(:!doctype))))
                           html))
     (with-new-file (s (make-pathname :type "lml" :defaults file)
                       :print-right-margin 70)
       (format s "~S" html))))
 (directory "Billy-Pilgrim:Users:gwking:darcs:metabang.tinaa:website:*.html"))

(net.html.parser:parse-html 
 #P"Billy-Pilgrim:Users:gwking:darcs:metabang.tinaa:website:index.html")

(probe-file
 "Billy-Pilgrim:Users:gwking:darcs:cl-containers:website:index.shtml")

(eval `(html
        ((:html :xmlns "http://www.w3.org/1999/xhtml")
         ,@(rest ccl:!))))

#+No
(with-new-file (*html-stream* (spy (make-pathname :type "lml" :defaults file)))
  (dtd-prologue :xhtml11)
  (eval `(html
          ((:html :xmlns "http://www.w3.org/1999/xhtml")
           ,@html))))
|#

(cl-markdown::defextension (metabang-projects-list
			    :arguments ((other :keyword)))
  (ecase cl-markdown::phase
    (:parse
     ;; no worries
     )
    (:render 
     (loop for system in rw:*metabang-common-lisp-systems* 
	when (if other
		 (not (rw:metabang-software? system))
		 (rw:metabang-software? system)) do
	(format cl-markdown::*output-stream*
		"~&<div class='system-name'>~a</div>~%"
		(with-output-to-string (lml2:*html-stream*)
		  (rw:link (rw:key system))))
	(format cl-markdown::*output-stream*
		"~&<div class='system-description'>~a</div>~%"
		(with-output-to-string (lml2:*html-stream*)
		  (html (:princ (rw:short-description system)))))))))


#+(or)
(cl-markdown:markdown
 "Hi

{metabang-projects-list :other t}

Bye"
 :additional-extensions
 '(cl-markdown::docs cl-markdown::docs-index
   cl-markdown::today cl-markdown::now
					;	 cl-markdown::footnote cl-markdown::footnotes 
   cl-markdown::glossary
   cl-markdown::metabang-projects-list))
