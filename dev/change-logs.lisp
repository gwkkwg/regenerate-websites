;;; -*- Mode: Lisp -*-

(in-package #:regenerate-websites)

(let ((lml2::*output-dir* 
       (translate-logical-pathname
        (format nil "user-home:darcs;~(~A~);website;output;" 'cl-containers))))
  (html-file-page ("test")
    (html
     (:HEAD (:TITLE "CL-MathStats")
            ((:LINK :REL "stylesheet" :TYPE "text/css" :HREF "style.css"))
            ((:META :HTTP-EQUIV "Content-Type" :CONTENT "text/html; charset=ISO-8859-1")))
     (:BODY
      (:PRE
       (lml-insert-file "cl-mathstats:website;logs;openmcl.log"))))))

(let ((lml2::*output-dir* 
       (translate-logical-pathname
        (format nil "user-home:darcs;~(~A~);website;output;" 'cl-mathstats)))
      (project "CL-MathStats"))
  (html-file-page ("change-log")
    (html
     (:HEAD (:TITLE (lml-format "~A Change Log" project))
            ((:LINK :REL "stylesheet" :TYPE "text/css" :HREF "style.css"))
            ((:META :HTTP-EQUIV "Content-Type" :CONTENT "text/html; charset=ISO-8859-1")))
     (:BODY
      ((:TABLE :BORDER "1")
       (loop for entry in (read-change-log #P"cl-mathstats:website;logs;change-log.xml") 
             do
             (html 
              (:TR
               ((:TD :COLSPAN 2) (lml-format "~A" (local_date entry)))
               ((:TD :ALIGN "RIGHT") (lml-format "~A" (author entry))))
              (:TR
               (:TD)
               ((:TD :COLSPAN 2) (lml-format "~A" (description entry)))))))))))

(xmls:parse #P"cl-mathstats:website;logs;change-log.xml")

"Billy-Pilgrim:Users:gwking:darcs:cl-containers:website:output:test.html"

(defclass* change-log-entry ()
  ((hash nil ir)
   (inverted nil ir)
   (local_date nil ir)
   (date nil ir)
   (author nil ir)
   (description nil ir)))

;;; ---------------------------------------------------------------------------

(defun read-change-log (&optional 
                        (change-log #P"cl-mathstats:website;logs;change-log.xml"))
  (let* ((xmls (xmls:parse change-log))
         (entries (collect-elements 
                   ;; the first two elements of posts aren't tags
                   (cddr xmls)
                   :transform
                   (lambda (entry)
                     (let ((properties (second entry))
                           (name-value (third entry)))
                       (apply #'make-instance
                              'change-log-entry
                              :description (third name-value)
                              (loop for (name value) in properties nconc
                                    (list (form-keyword (string-upcase name)) value))))))))
    entries))

