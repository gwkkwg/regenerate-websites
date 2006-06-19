(in-package #:rw)

(defgeneric output-footer (site current-page)
  (:documentation ""))

(defgeneric output-headers (site title)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defmethod output-footer ((site (eql :metabang)) current-page) 
  (flet ((make-link (name link text title &optional (class "link"))
           (if (eq current-page name)
             (html
              ((:SPAN :class class) text))
             (html
              ((:SPAN :class class)
               ((:A :HREF link :title title)
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
       (make-link :widgets-page
                  "widgets.html" "Widgets" "Our Dashboard Widgets")
       (make-link :about-gwking-page
                  "contact.html" "Contact" "Contact information" "last-link"))
       
      ((:DIV :CLASS "copyright")
       "Page updated: "
       ((:SPAN :CLASS "date") (lml-princ (format-date "%B %e, %Y" (get-universal-time))))
       ", Copyright  2004 - " (lml-princ (format-date "%Y" (get-universal-time))) " -- " 
       ((:A :HREF "mailto:gwking@metabang.com") "Gary Warren King"))))))

;;; ---------------------------------------------------------------------------

(defmethod output-headers ((site (eql :metabang)) title)
  (html
   (:head 
    (:title (lml-princ title))
    ((:link :rel "shortcut icon" :href "favicon.ico"))
    ((:link :rel "icon" :href "favicon.ico"))
    ((:meta :http-equiv "content-type" :content "text/html; charset=iso-8859-1"))
    ((:link :rel "stylesheet" :href "style.css" :type "text/css")))))
  