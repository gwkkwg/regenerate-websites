;;; -*- Mode: Lisp -*-

(in-package #:regenerate-websites)

(defun create-changelog (system-name)
  (let ((repo (system-home system-name))
        (output (changelog-source system-name)))
    (kl:run-shell-command 
     "darcs changes --xml --repo=file://~A > ~A" 
     (translate-logical-pathname repo)
     (translate-logical-pathname output))
    output))

#+Ignore
(defun create-changelog (system-name)
  (let ((repo (system-home system-name))
        (output (changelog-source system-name)))
    (format t
     "darcs changes --xml --repo=file://~A > ~A"
     (translate-logical-pathname repo)
     (translate-logical-pathname output))
    output))

;;; ---------------------------------------------------------------------------

(defun create-changelog-page (system-name)
  (let* ((system (find-system system-name))
         (lml2::*output-dir* (website-output-directory system-name))
         (*package* *package*)
	 (entries (read-changelog (changelog-source system-name))))
    
    (when entries
      (html-file-page ("changelog")
	(html
	 (:head (:title (lml-format "Changelog for ~A" (name system)))
		(generate-shared-headers))
       
	 (:body
	  ((:div :class "header")
	   ((:span :class "logo")
	    ((:a :href "http://www.metabang.com/" :title "metabang.com")
	     ((:img :src "http://common-lisp.net/project/cl-containers/images/metabang-2.png"
		    :title "metabang.com" :width 100))))
	   (:h2 (lml-format "Changelog for ~A" (name system)))
	   (:h4 "Generated on "
		(lml-princ (metatilities:format-date "%A, %e %B %Y" (get-universal-time)))))
        
	  ((:div :class "changelog")
	   ((:table)
	    (loop for entry in entries
	       do
	       (html 
		(:tr
		 ((:td :class "changelog-date") 
		  (lml2::princ-safe-http (local_date entry)))
		 ((:td :class "changelog-author") 
		  (lml2::princ-safe-http (author entry))))
		((:tr :class "changelog-row")
		 ((:td :class "changelog-description" :colspan 2) 
		  (lml2::princ-safe-http (description entry))))))))
        
	  ((:div :class "footer")
	   (generate-button-row))))))))

;;; ---------------------------------------------------------------------------

(defclass* changelog-entry ()
  ((hash nil ir)
   (inverted nil ir)
   (local_date nil ir)
   (date nil ir)
   (author nil ir)
   (description nil ir)))

;;; ---------------------------------------------------------------------------

(defun read-changelog (file)
  (when (plusp (kl:file-size file))
    (let ((close? nil))
      (setf file (etypecase file
		   (stream file)
		   (string file)
		   (pathname (setf close? t)
			     (open file :direction :input))))
      (unwind-protect
	   (let* ((xmls (xmls:parse file))
		  (entries 
		   (collect-elements 
		    ;; the first two elements of posts aren't tags
		    (cddr xmls)
		    :transform
		    (lambda (entry)
		      (let ((properties (second entry))
			    (name-value (third entry)))
			(apply #'make-instance
			       'changelog-entry
			       :description (third name-value)
			       (loop for (name value) in properties nconc
				    ;;?? probable case issues
				    (list (form-keyword name)
					  value))))))))
	     entries)
	(when close? 
	  (close file))))))

