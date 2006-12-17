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
              ((:span :class class) text))
             (html
              ((:span :class class)
               ((:a :href link :title title)
                (lml-princ text)))))))
    (html
     ((:div :class "footer")
      
      ((:div :class "links")
       (make-link :about-page "about.html" "About" "About metabang.com")
       (make-link :open-source-software-page
                  "open-source-software.html" "Software" "Open source software")
       
       #+Ignore
       ((:span :class "link")
        ((:a :href "http://www.metabang.com/quotes.html" :title "Quotes we like")
         "Quotes"))
       
       (make-link :gpg-page
                  "public-key-gwking.html" "PGP Key" "PGP Public Key")
       (make-link :widgets-page
                  "widgets.html" "Widgets" "Our Dashboard Widgets")
       (make-link :about-gwking-page
                  "contact.html" "Contact" "Contact information" "last-link"))
       
      ((:div :class "copyright")
       "Page updated: "
       ((:span :class "date") (lml-princ (format-date "%B %e, %Y" (get-universal-time))))
       ", Copyright  2004 - " (lml-princ (format-date "%Y" (get-universal-time))) " -- " 
       ((:a :href "mailto:gwking@metabang.com") "Gary Warren King"))))))

;;; ---------------------------------------------------------------------------

(defmethod output-headers ((site (eql :metabang)) title)
  (html
   (:head 
    (:title (lml-princ title))
    ((:link :rel "shortcut icon" :href "favicon.ico"))
    ((:link :rel "icon" :href "favicon.ico"))
    ((:meta :http-equiv "content-type" :content "text/html; charset=iso-8859-1"))
    ((:link :rel "stylesheet" :href "style.css" :type "text/css")))))
  