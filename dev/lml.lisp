(in-package #:regenerate-websites)

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

(defstruct (link-info (:conc-name link-))
  href title)

(defun set-link-info (name &rest args &key (title name) &allow-other-keys)
  (setf (item-at-1 *common-links* name) 
        (apply #'make-link-info :title title args)))

(defun link (name &key title &allow-other-keys)
  (let ((link-info (item-at-1 *common-links* name)))
    (assert link-info nil
            "Unable to find link information for ~S" name)
    (html ((:a :href (link-href link-info)) 
	   (:princ (or title (link-title link-info))))))) 

(defun generate-two-line-header (title sub-title &key (logo? t))
  (html
   ((:div :class "header")
    (when logo?
      (html
       ((:span :class "logo")
        ((:a :href "http://www.metabang.com/" :title "metabang.com")
         ((:img :src "http://common-lisp.net/project/cl-containers/shared/metabang-2.png"
                :title "metabang.com" :width 100
                :alt "Metabang Logo"))))))
    (:h2 (lml-princ title))
    (when sub-title
      (html (:h4 (lml-princ sub-title)))))))

(defun generate-system-sidebar (&key (news? t))
  (html
   ((:div :class "system-links")
    (:ul
     (:li ((:a :href "#mailing-lists") "Mailing Lists"))
     (:li ((:a :href "#downloads") "Getting it"))
     (let* ((documentation-file 
             (merge-pathnames
              (make-pathname :name "index"
                             :type "html"
                             :directory '(:relative "dev" "documentation"))
              (dsc:system-source-directory *current-system*)))
            (documentation-url "documentation/"))
       (when (probe-file documentation-file)
         (html
          (:li ((:a :href documentation-url :title "documentation link") 
                "Documentation")))))
     (when news? (html (:li ((:a :href "#news") "News"))))
     (:li ((:a :href "changelog.html") "Changelog"))))))

(set-link-info
 :mail-gwking
 :href (format nil "mailto:gwking@metabang.com")
 :title "Gary King")

(set-link-info
 :homepage-gwking
 :href (format nil "http://www.metabang.com/about-gwking.html")
 :title "Gary King")

(set-link-info
 :gpg-gwking
 :href (format nil "http://www.metabang.com/public-key-gwking.html")
 :title "Gary King's Public Key")

(set-link-info
 :cln-cl-utilities
 :href "http://common-lisp.net/project/cl-utilities/"
 :title "cl-utilities")

(set-link-info
 :cln-root
 :href "http://common-lisp.net/"
 :title "Common-Lisp.net")

(set-link-info
 :metabang
 :href "http://www.metabang.com/"
 :title "metabang.com")

(set-link-info
 :darcs
 :href "http://www.darcs.net/"
 :title "Darcs")

(set-link-info
 :asdf-install
 :href "http://www.cliki.net/asdf-install"
 :title "ASDF-Install")

(set-link-info
 :mk-defsystem
 :href "http://www.cliki.net/mk-defsystem"
 :title "MK-Defsystem")

(set-link-info
 :asdf
 :href "http://www.cliki.net/asdf"
 :title "ASDF")

(set-link-info
 :cliki
 :href "http://www.cliki.net/"
 :title "CLiki")

(set-link-info
 :mit-license
 :href "http://www.opensource.org/licenses/mit-license.php"
 :title "MIT License")

(set-link-info
 :mop
 :href "http://www.lisp.org/mop/index.html"
 :title "MOP")

(set-link-info
 :gnuplot
 :href "http://www.gnuplot.info/"
 :title "GNUPlot")

(set-link-info
 :unclog
 :href "http://www.metabang.com/unclog/"
 :title "unCLog")

(set-link-info
 :polliblog
 :href "http://www.metabang.com/polliblog/"
 :title "Polliblog")

(set-link-info
 :widgets
 :href "http://www.metabang.com/widgets/index.html"
 :title "Widgets")





#+Test
(link :metabang-site)
#+Test
(link :cl-containers)
#+Test
(link :metatilities)


(loop for system in *metabang-common-lisp-systems* do 
     (let* ((key (key system))
	    (folder (folder system))
	    (title (name system))
	    (sub-folder (sub-folder system))
	    (root (or (root system) 
		      "http://common-lisp.net/project/"))
	    (cliki (or (cliki system) key)))
       ;;?? very hacky
       (when sub-folder
	 (setf folder (format nil "~(~A/~A~)" sub-folder folder)))
       ;;?? slightly hacky
       (setf folder 
	     (if (or (symbolp folder)
		     (and (stringp folder) (plusp (size folder))))
		 (format nil "~A~(~A~)/" root folder)
		 (format nil "~A" root)))
       (set-link-info
	key
	:href folder
	:title title)
       (set-link-info
	(form-keyword key "-tinaa")
	:href (format nil "~Adocumentation/" folder)
	:title title)
       (set-link-info
	(form-keyword key "-package")
	:href (format nil "~A~(~A~)_latest.tar.gz" folder key)
	:title title)
       (set-link-info
	(form-keyword key "-cliki")
	:href (format nil "http://www.cliki.net/~(~A~)" cliki)
	:title title)))