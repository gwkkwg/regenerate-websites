;;; -*- Mode: Lisp -*-

(in-package regenerate-websites)

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
     "darcs changes --xml --repo=file://~A > ~A" repo output)
    output))

;;; ---------------------------------------------------------------------------

(defun create-changelog-page (system-name)
  (let* ((system (find-system system-name))
         (lml2::*output-dir* (website-output-directory system-name))
         (*package* *package*))
    
    (html-file-page ("changelog")
      (html
       (:HEAD (:TITLE (lml-format "Changelog for ~A" (name system)))
	      (generate-shared-headers))
       
       (:BODY
        ((:DIV :CLASS "header")
         ((:SPAN :CLASS "logo")
          ((:A :HREF "http://www.metabang.com/" :title "metabang.com")
           ((:IMG :SRC "http://common-lisp.net/project/cl-containers/images/metabang-2.png"
                  :TITLE "metabang.com" :width 100))))
         (:H2 (lml-format "Changelog for ~A" (name system)))
         (:H4 "Generated on "
              (lml-princ (metatilities:format-date "%A, %e %B %Y" (get-universal-time)))))
        
        ((:DIV :CLASS "changelog")
         ((:TABLE)
          (loop for entry in (read-changelog (changelog-source system-name))
                do
                (html 
                 (:TR
                  ((:TD :CLASS "changelog-date") 
                   (lml2::princ-safe-http (local_date entry)))
                  ((:TD :CLASS "changelog-author") 
		   (lml2::princ-safe-http (author entry))))
                 ((:TR :CLASS "changelog-row")
                  ((:TD :CLASS "changelog-description" :COLSPAN 2) 
		   (lml2::princ-safe-http (description entry))))))))
        
        ((:DIV :CLASS "footer")
         (generate-button-row)))))))

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
  (let* ((xmls (xmls:parse file))
         (entries (collect-elements 
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
                                    (list (form-keyword (string-upcase name)) value))))))))
    entries))

