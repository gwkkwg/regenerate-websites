;;; -*- Mode: Lisp -*-

(in-package #:regenerate-websites)


(defun generate-shared-headers ()
  (html
   ((:link :rel "stylesheet" :type "text/css" :href "http://common-lisp.net/project/cl-containers/shared/style.css"))
   ((:meta :http-equiv "Content-Type" :content "text/html; charset=ISO-8859-1"))))

(defun generate-button-row (&optional text)
  (html
   ((:a :class "nav" :href "http://validator.w3.org/check/referer" 
        :title "xhtml1.1")
    ((:img :src (button-img-src "xhtml.gif")
           :width "80" :height "15" :title "valid xhtml button"
           :alt "valid xhtml")))
   #+Ignore
   ((:a :class "nav" 
        :href "http://jigsaw.w3.org/css-validator/validator?uri=http%3A%2F%2Fcommon-lisp.net~2Fproject~2Fcl-containers%2Fstyle.css" 
        :title "css")
    ((:img :src (button-img-src "cssvalid.gif")
           :width "80" :height "15" :title "valid css button"
           :alt "valid css")))
   ((:a :class "nav" :href "http://www.catb.org/hacker-emblem/" :title "hacker")
    ((:img :src (button-img-src "hacker.png") 
           :width "80" :height "15" :title "hacker emblem"
           :alt "hacker button")))
   ((:a :class "nav" :HREF "http://lml2.b9.com/" :title "lml2 powered")
    ((:img :src (button-img-src "lml2-powered.png") 
           :width "80" :height "15" :title "lml2 powered"
           :alt "lml2 button")))
   ((:a :class "nav" :href "http://www.lisp.org/" :title "Association of Lisp Users")
    ((:img :src (button-img-src "lambda-lisp.png") 
           :width "80" :height "15" :title "ALU emblem"
           :alt "ALU button")))
   ((:a :class "nav" :href "http://common-lisp.net/" :title "Common-Lisp.net")
    ((:img :src (button-img-src "lisp-lizard.png") 
           :width "80" :height "15" :title "Common-Lisp.net"
           :alt "Common-Lisp.net button")))
   (when text
     (html ((:span :class "footer-text") (lml-princ text))))))

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

