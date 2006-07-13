;;;-*- Mode: Lisp; Package: REGENERATE-WEBSITES -*-

#| simple-header

Copyright 2004 - 2005 metabang.com (www.metabang.com), 
55 Harkness Road, Pelham, MA 01002
Gary Warren King

MIT Open Source License:

http://www.opensource.org/licenses/mit-license.php

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Author: Gary King

DISCUSSION

|#
(in-package #:regenerate-websites)

(export '(asdf-packaging? 
          metabang-software?))

;;; ---------------------------------------------------------------------------

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
   (darcs-repo nil ir)
   (e8el? nil ir)))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object metabang-system) &key)
  (unless (folder object)
    (setf (slot-value object 'folder) (key object)))
  (unless (home-directory object)
    (setf (slot-value object 'home-directory) 
          (string-downcase (kl:ensure-string (key object)))))
  (unless (documentation-package object)
    (setf (slot-value object 'documentation-package)
          (key object))))

;;; ---------------------------------------------------------------------------

(defmethod print-object ((object metabang-system) stream)
  (print-unreadable-object (object stream :type t)
    (prin1 (key object) stream)))

;;; ---------------------------------------------------------------------------


(defun find-system (system-name)
  (find system-name *metabang-common-lisp-systems* 
        :key 'key))

;;; ---------------------------------------------------------------------------

(defun system-home (system-name)
  (let ((system (find-system system-name)))
    (assert system
	    nil
	    "Error, cannot find system ~S" system-name)
    ;;?? ugh
    (format nil "user-home:darcs;~(~A~);" (home-directory system))))

;;; ---------------------------------------------------------------------------

;;?? also in tinaa
(defun lml-insert-file (file)
  (if (probe-file file)
      (with-open-file (in file :direction :input)
        (do ((line (read-line in nil 'eof) (read-line in nil 'eof)))
	    ((eq line 'eof))
	  (html (lml-princ line))))
    (format *trace-output* "Warning: unable to insert LML file ~S" file)))

;;; ---------------------------------------------------------------------------

(defun website-source-directory (system-name)
  (let ((system (find-system system-name))) 
    (translate-logical-pathname
     (format nil "user-home:darcs;~(~A~);website;source;"
             (home-directory system)))))

;;; ---------------------------------------------------------------------------

(defun website-output-directory (system-name)
  (let ((system (find-system system-name))) 
    (translate-logical-pathname
     (format nil "user-home:darcs;~(~A~);website;output;" 
             (home-directory system)))))

;;; ---------------------------------------------------------------------------

(defun changelog-source (system-name)
  (let ((source (website-source-directory system-name)))
    (make-pathname
     :type "xml"
     :name "changelog"
   :directory (butlast (pathname-directory source)) 
   :defaults source)))

;;; ---------------------------------------------------------------------------

(defun copy-source-to-output (source)
  (let ((target (output-path-for-source source)))
    #+Ignore
    (when (and (probe-file target)
               (file-newer-than-file-p target source))
      (cerror (format nil "Overwrite existing file with ~S" source)
              'file-error :pathname target))
    (copy-file source target :if-exists :supersede)))

;;; ---------------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------------

(defun output-type-for-source (source-file)
  (let ((type (pathname-type source-file)))
    (cond ((string-equal type "lml")
           "html")
          (t
           type))))

;;; ---------------------------------------------------------------------------

(defstruct (link-info (:conc-name link-))
  href title)

;;; ---------------------------------------------------------------------------

(defun set-link-info (name &rest args &key (title name) &allow-other-keys)
  (setf (item-at-1 *common-links* name) 
        (apply #'make-link-info :title title args)))

;;; ---------------------------------------------------------------------------

(defun link (name &key title &allow-other-keys)
  (let ((link-info (item-at-1 *common-links* name)))
    (assert link-info nil
            "Unable to find link information for ~S" name)
    (html ((:a :href (link-href link-info)) (:princ (or title (link-title link-info))))))) 

;;; ---------------------------------------------------------------------------

(progn
  (setf *metabang-common-lisp-systems*
        (sort
         (mapcar (lambda (data)
                   (apply #'make-instance 'metabang-system data))
                 '((:key :ASDF-Binary-Locations :name "ASDF-Binary-Locations"
                         :sub-folder "cl-containers"
                         :short-description "Put Lisp binaries in their places"
	                 :build-documentation? nil
                         :darcs-repo "http://common-lisp.net/project/cl-containers/asdf-binary-locations/darcs/asdf-binary-locations")
                   
                   (:key :asdf-install-tester
                         :name "ASDF-Install-Tester"
                         :root "http://common-lisp.net/project/ait/"
                         :short-description "Test ASDF Installable systems automagically"
                         :build-documentation? t
                         :home-directory "ait"
                         :folder ""
                         :darcs-repo "http://common-lisp.net/project/tinaa/darcs/ait")
                   
                   (:key :asdf-status :name "ASDF-Status"
                         :sub-folder "cl-containers"
                         :short-description "Display ASDF-Install-testers results nicely"
                         :darcs-repo "http://common-lisp.net/project/cl-containers/asdf-status/darcs/asdf-status")
                   
                   (:key :asdf-system-connections :name "ASDF-System-Connections"
                         :sub-folder "cl-containers"
                         :short-description "Link ASDF systems together declaratively"
	                 :build-documentation? t
                         :darcs-repo "http://common-lisp.net/project/cl-containers/asdf-system-connections/darcs/asdf-system-connections")
                   
                   (:key :cl-containers :name "CL-Containers"
                         :short-description "Common-Lisp's answer to STL and Smalltalk"
                         :build-documentation? t
                         :darcs-repo "http://common-lisp.net/project/cl-containers/darcs/cl-containers")
                   
                   (:key :cl-graph :name "CL-Graph"
                         :short-description "Utilities and algorithms for Graph manipulation"
                         :build-documentation? t
                         :darcs-repo "http://common-lisp.net/project/cl-graph/darcs/cl-graph")
                   
                   (:key :cl-markdown :name "CL-Markdown"
                         :short-description "Common Lisp version of the Markdown text processing langauge"
                         :build-documentation? t
                         :darcs-repo "http://common-lisp.net/project/cl-markdown/darcs/cl-markdown")
                   
                   (:key :cl-mathstats :name "CL-MathStats"
                         :short-description "Miscellaneous math and statistics utilities"
                         :build-documentation? t
                         :darcs-repo "http://common-lisp.net/project/cl-mathstats/darcs/cl-mathstats")
                   
                   (:key :cl-variates :name "CL-Variates"
                         :short-description "Portable Random Number Generators and tools."
                         :build-documentation? t
                         :darcs-repo "http://common-lisp.net/project/cl-variates/darcs/cl-variates")
                   
                   (:key :clnuplot :name "CLNUPlot"
                         :sub-folder "cl-containers"
                         :short-description "Common Lisp interface for GNUPlot"
                         :build-documentation? t
                         :metabang-software? nil
                         :darcs-repo "http://common-lisp.net/project/cl-containers/clnuplot/darcs/clnuplot")
                   
                   (:key :defsystem-compatibility :name "defsystem-compatibility"
                         :sub-folder "cl-containers"
                         :short-description "Help different system definers to live together."
                         :build-documentation? t
                         :darcs-repo "http://common-lisp.net/project/cl-containers/defsystem-compatibility/darcs/defsystem-compatibility")
                   
                   (:key :e8el :name "Enterprise Lisp"
                         :root "http://www.enterpriselisp.com/"
                         :home-directory "enterpriselisp"
                         :metabang-software? nil
                         :build-documentation? nil
                         )
                   
                   (:key :lift :name "LIFT"
                         :short-description "the LIsp Framework for Testing"
                         :build-documentation? t
                         :documentation-package lift
                         :darcs-repo "http://common-lisp.net/project/lift/darcs/lift")
                   
                   (:key :metacopy :name "metacopy"
                         :short-description "Shallow and deep copy toolkit"
                         :build-documentation? t
                         :darcs-repo "http://common-lisp.net/project/metacopy/darcs/metacopy")
                   
                   (:key :metatilities :name "Metatilities" 
                         :sub-folder "cl-containers"
                         :short-description "Various useful utilities"
                         :build-documentation? t
                         :darcs-repo "http://common-lisp.net/project/cl-containers/metatilities/darcs/metatilities")
                   
                   (:key :moptilities :name "Moptilities" :sub-folder "cl-containers"
                         :short-description "Implementation independent MOP utilities"
                         :build-documentation? t
                         :darcs-repo "http://common-lisp.net/project/cl-containers/moptilities/darcs/moptilities")
                   
                   (:key :metabang-bind :name "metabang-bind" 
                         :sub-folder "cl-containers"
                         :cliki "bind"
                         :short-description "Handle destructuring, multiple-values and let simultaneously"
                         :build-documentation? t
                         :home-directory "metabang-bind"
                         :darcs-repo "http://common-lisp.net/project/cl-containers/metabang-bind/darcs/metabang-bind")
                   
                   (:key :simple-http :name "Simple HTTP" 
                         :root "http://metabang.gotdns.com/software/"
                         :short-description "Not as trivial as trivial HTTP, but still simple..."
                         :build-documentation? t
                         :darcs-repo "http://metabang.gotdns.com/software/simple-http/darcs/simple-http"
                         :e8el? t)
                   
                   (:key :system-check :name "System-check" 
                         :short-description "Keeping your systems up to date"
                         :root "http://metabang.gotdns.com/software/" 
                         :build-documentation? t
                         :darcs-repo "http://metabang.gotdns.com/software/system-check/darcs/system-check"
                         :e8el? t)
                   
                   (:key :tinaa :name "TINAA"
                         :short-description "Common-Lisp documentation tool"
                         :build-documentation? t
                         :darcs-repo "http://common-lisp.net/project/tinaa/darcs/tinaa")
                   
                   (:key :trivial-shell :name "trivial-shell"
                         :short-description "One shell to run them all"
                         :build-documentation? t
                         :darcs-repo "http://common-lisp.net/project/trivial-shell/darcs/trivial-shell")
                   
                   (:key :metasite :name "metabang.com" 
                         :root "http://www.metabang.com/"
                         :folder ""
                         :metabang-software? nil)
                   
                   (:key :metabang-local :name "metabang.com" 
                         :root "http://metabang.gotdns.com/"
                         :folder ""
                         :home-directory "metasite;metabang-local"
                         :metabang-software? nil)             
                   
                   (:key :cl-html-parse :name "Common Lisp HTML Parser" 
                         :short-description "A portable version of Franz's Opensource HTML Parser"
                         :metabang-software? nil
                         :asdf-packaging? t
                         :build-website? nil
                         :build-documentation? t
                         :darcs-repo "http://metabang.gotdns.com/software/system-check/darcs/system-check"
                         :e8el? t)
                   
                   (:key :closer-mop :name "Closer to MOP" 
                         :root "http://common-lisp.net/project/closer/closer-mop.html"
                         :short-description "A compatibility layer to paper over Common-Lisp implmentation difference in MOP support."
                         :folder ""
                         :metabang-software? nil
                         :asdf-packaging? t
                         :build-website? nil
                         :darcs-repo "http://common-lisp.net/project/closer/repos/closer-mop")
                   
                   (:key :mop-features :name "MOP Feature Tests" 
                         :root "http://common-lisp.net/project/closer/features.html"
                         :folder ""
                         :short-description "Meta-object Protocol evaluator to test a Common-Lisp implementation's MOP support."
                         :metabang-software? nil
                         :asdf-packaging? t
                         :build-website? nil
                         :darcs-repo "http://common-lisp.net/project/closer/repos/mop-features")
                   
                   (:key :contextl :name "ContextL" 
                         :root "http://common-lisp.net/project/closer/contextl.html"
                         :folder ""
                         :short-description "a CLOS extension for Context-oriented Programming."
                         :metabang-software? nil
                         :asdf-packaging? t
                         :build-website? nil
                         :darcs-repo "http://common-lisp.net/project/closer/repos/contextl")
                   
                   (:key :lw-compat :name "LW-Compat" 
                         :root "http://common-lisp.net/project/closer/downloads/"
                         :short-description "Utility functions from the LispWorks library used in the Closer project"
                         :folder ""
                         :metabang-software? nil
                         :asdf-packaging? t
                         :build-website? nil
                         :darcs-repo "http://common-lisp.net/project/closer/repos/lw-compat")
                   
                   (:key :portable-threads :name "Portable Threads"
                         :root "http://gbbopen.org/hyperdoc/ref-portable-thread-entities.html"
                         :short-description "A Portable Thread Library originally developed for GBBOpen."
                         :folder ""
                         :metabang-software? nil
                         :asdf-packaging? t
                         :build-website? nil)
                   
                   (:key :asdf-install :name "ASDF-Install"
                         :short-description "A tool for downloading and installing lisp libraries and packages."
                         :metabang-software? nil
                         :asdf-packaging? t
                         :build-website? t)))
         #'string-lessp
         :key #'key))
  
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
           (form-keyword key "-TINAA")
           :href (format nil "~Adocumentation/" folder)
           :title title)
          (set-link-info
           (form-keyword key "-PACKAGE")
           :href (format nil "~A~(~A~)_latest.tar.gz" folder key)
           :title title)
          (set-link-info
           (form-keyword key "-CLIKI")
           :href (format nil "http://www.cliki.net/~(~A~)" cliki)
           :title title))))

;;; ---------------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------------
;;; bits and pieces
;;; ---------------------------------------------------------------------------

(defun button-img-src (image-name)
  (format nil "http://common-lisp.net/project/cl-containers/shared/buttons/~A"
          image-name))

;;; ---------------------------------------------------------------------------

(defun generate-button-row (&optional text)
  (html
   ((:A :CLASS "nav" :HREF "http://validator.w3.org/check/referer" 
        :title "xhtml1.1")
    ((:IMG :SRC (button-img-src "xhtml.gif")
           :WIDTH "80" :HEIGHT "15" :TITLE "valid xhtml button"
           :alt "valid xhtml")))
   
   #+Ignore
   ((:A :CLASS "nav" 
        :HREF "http://jigsaw.w3.org/css-validator/validator?uri=http%3A%2F%2Fcommon-lisp.net~2Fproject~2Fcl-containers%2Fstyle.css" 
        :title "css")
    ((:IMG :SRC (button-img-src "cssvalid.gif")
           :WIDTH "80" :HEIGHT "15" :TITLE "valid css button"
           :alt "valid css")))
   
   ((:A :CLASS "nav" :HREF "http://www.catb.org/hacker-emblem/" :title "hacker")
    ((:IMG :SRC (button-img-src "hacker.png") 
           :WIDTH "80" :HEIGHT "15" :TITLE "hacker emblem"
           :alt "hacker button")))
   ((:A :CLASS "nav" :HREF "http://lml2.b9.com/" :title "lml2 powered")
    ((:IMG :SRC (button-img-src "lml2-powered.png") 
           :WIDTH "80" :HEIGHT "15" :TITLE "lml2 powered"
           :alt "lml2 button")))
   ((:A :CLASS "nav" :HREF "http://www.lisp.org/" :title "Association of Lisp Users")
    ((:IMG :SRC (button-img-src "lambda-lisp.png") 
           :WIDTH "80" :HEIGHT "15" :TITLE "ALU emblem"
           :alt "ALU button")))
   ((:A :CLASS "nav" :HREF "http://common-lisp.net/" :title "Common-Lisp.net")
    ((:IMG :SRC (button-img-src "lisp-lizard.png") 
           :WIDTH "80" :HEIGHT "15" :TITLE "Common-Lisp.net"
           :alt "Common-Lisp.net button")))
   (when text
     (html ((:span :class "footer-text") (lml-princ text))))))

;;; ---------------------------------------------------------------------------

(defun generate-two-line-header (title sub-title &key (logo? t))
  (html
   ((:DIV :CLASS "header")
    (when logo?
      (html
       ((:SPAN :CLASS "logo")
        ((:A :HREF "http://www.metabang.com/" :title "metabang.com")
         ((:IMG :SRC "http://common-lisp.net/project/cl-containers/shared/metabang-2.png"
                :TITLE "metabang.com" :width 100
                :ALT "Metabang Logo"))))))
    (:H2 (lml-princ title))
    (when sub-title
      (html (:H4 (lml-princ sub-title)))))))

;;; ---------------------------------------------------------------------------

(defun generate-system-sidebar ()
  (html
   ((:DIV :CLASS "system-links")
    (:UL
     (:LI ((:A :HREF "#mailing-lists") "Mailing Lists"))
     (:LI ((:A :HREF "#downloads") "Getting it"))
     (let* ((documentation-file 
             (merge-pathnames
              (make-pathname :name "index"
                             :type "html"
                             :directory '(:relative "dev" "documentation"))
              (dsc:system-source-directory *current-system*)))
            (documentation-url "documentation/"))
       (when (probe-file documentation-file)
         (html
          (:LI ((:A :HREF documentation-url :TITLE "documentation link") 
                "Documentation")))))
     (:LI ((:A :HREF "#news") "News"))
     (:LI ((:A :HREF "changelog.html") "Changelog"))))))

;;; ---------------------------------------------------------------------------

(defun generate-shared-headers ()
  (html
   ((:LINK :REL "stylesheet" :TYPE "text/css" :HREF "http://common-lisp.net/project/cl-containers/shared/style.css"))
   ((:META :HTTP-EQUIV "Content-Type" :CONTENT "text/html; charset=ISO-8859-1"))))

;;; ---------------------------------------------------------------------------

(defun smallest-system-set (system)
  (let ((all-dependencies (dsc:collect-system-dependencies system)))
    (loop for unneeded =
          (find-if 
           (lambda (sub-system)
             (some (lambda (other-sub-system)
                     (and (not (eq sub-system other-sub-system))
                          (member sub-system 
                                  (dsc:collect-system-dependencies other-sub-system))))
                   all-dependencies))
           all-dependencies)
          while unneeded do
          (print unneeded)
          (print all-dependencies)
          (setf all-dependencies (remove unneeded all-dependencies)))
    all-dependencies))

#+Ignore
(defun make-systems-graph (system)
  (let ((g (make-container 'cl-graph:graph-container 
                           :default-edge-type :directed)))
    (labels ((add-edges (system)
               (dsc:map-system-dependencies 
                system
                (lambda (sub-system)
                  (cl-graph:add-edge-between-vertexes 
                   g system sub-system)
                  (add-edges sub-system)))))
      (add-edges system))
    g))

#+Example
(smallest-system-set 'tinaa)
                          
(defun generate-darcs-commands (project)
  (let ((result nil))
    (html
     (:pre
      (mapc 
       (lambda (system)
         (let ((system (intern (string-upcase (symbol-name system)) :keyword)))
           (if (find-system system)
             (html (lml-format "~%darcs get \"~A\"" (darcs-repo (find-system system))))
             (push system result))))
         (append (list project) (smallest-system-set project))))
     (when result
       (flet ((make-project-link (project)
                (html ((:a :href (format nil "http://www.cliki.net/~(~A~)" project))
                       (lml-princ project)))))
         (html
          (:p (lml-princ project) " also requires the following other ASDF-Installable projects: "
              (cond ((= (length result) 1)
                     (make-project-link (first result)))
                    ((= (length result) 2)
                     (make-project-link (first result)) 
                     (html " and ")
                     (make-project-link (second result)))
                    (t 
                     (loop for i from 0 to (- (length result) 2) do
                           (make-project-link (nth i result))
                           (html ", "))
                     (html " and ")
                     (make-project-link (first (last result)))))
              (html "."))))))
    (values result)))

#+Example
(generate-darcs-commands 'metatilities)
   
