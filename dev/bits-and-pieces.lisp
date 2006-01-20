(in-package rw)

(defgeneric output-footer (site current-page)
  (:documentation ""))

(defgeneric output-headers (site title)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defmethod output-footer ((site (eql :metabang)) current-page) 
  (flet ((make-link (name link text title)
           (if (eq current-page name)
             (html
              ((:SPAN :class "link") (lml-princ text)))
             (html
              ((:SPAN :class "link")
               ((:A :HREF link :title (lml-princ title))
                (lml-princ text)))))))
    (html
     ((:DIV :CLASS "footer")
      
      ((:DIV :CLASS "links")
       (make-link :about-page "about.html" "About" "About metabang.com")
       (make-link :open-source-software-page
                  "open-source-software.html" "Software" "Open source software")
       
       #+Ignore
       ((:SPAN :class "link")
        ((:A :HREF "http://www.metabang.com/quotes.html" :TITLE "Quotes we like")
         "Quotes"))
       
       (make-link :gpg-page
                  "public-key-gwking.html" "PGP Key" "PGP Public Key")
       (make-link :about-gwking-page
                  "contact.html" "Contact" "Contact information"))
       
      ((:DIV :CLASS "copyright")
       "Page updated: "
       ((:SPAN :CLASS "date") (lml-princ (format-date "%B %e, %Y" (get-universal-time))))
       ", Copyright  2004 - " (lml-princ (format-date "%Y" (get-universal-time))) " -- " 
       ((:A :HREF "mailto:gwking@metabang.com") "Gary Warren King"))))))

;;; ---------------------------------------------------------------------------

(defmethod output-headers ((site (eql :metabang)) title)
  (html
   (:HEAD 
    (:TITLE (lml-princ title))
    ((:LINK :REL "shortcut icon" :HREF "favicon.ico"))
    ((:LINK :REL "icon" :HREF "favicon.ico"))
    ((:META :HTTP-EQUIV "content-type" :CONTENT "text/html; charset=iso-8859-1"))
    ((:LINK :REL "stylesheet" :HREF "style.css" :TYPE "text/css")))))
  