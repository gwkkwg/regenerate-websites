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
(in-package regenerate-websites)

(defvar *common-links* (make-container 'simple-associative-container))
(defvar *website-wild-source* nil)
(defvar *website-output* nil)
(defvar *force-rebuild?* nil)

(defclass* metabang-system ()
  ((name nil ir)
   (key nil ir)
   (sub-folder nil ir)
   (folder nil ir)
   (root nil ir)
   (cliki nil ir)
   (short-description nil ir)
   (software? t ir)))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object metabang-system) &key)
  (unless (folder object)
    (setf (slot-value object 'folder) (key object))))

;;; ---------------------------------------------------------------------------

(defmethod print-object ((object metabang-system) stream)
  (print-unreadable-object (object stream :type t)
    (prin1 (key object) stream)))

;;; ---------------------------------------------------------------------------

(defparameter *metabang-common-lisp-systems*
  (sort
   (mapcar (lambda (data)
             (apply #'make-instance 'metabang-system data))
           '((:key :asdf-system-connections :name "ASDF-System-Connections"
                   :sub-folder "cl-containers"
                   :short-description "Link ASDF systems together declaratively")
             (:key :cl-graph :name "CL-Graph"
                   :short-description "Utilities and algorithms for Graph manipulation")
             (:key :cl-containers :name "CL-Containers"
                   :short-description "Common-Lisp's answer to STL and Smalltalk")
             (:key :cl-mathstats :name "CL-MathStats"
                   :short-description "Miscellaneous math and statistics utilities")
             (:key :cl-variates :name "CL-Variates"
                   :short-description "Portable Random Number Generators and tools.")
             (:key :lift :name "LIFT"
                   :short-description "the LIsp Framework for Testing")
             (:key :metatilities :name "Metatilities" :sub-folder "cl-containers"
                   :short-description "Various useful utilities")
             (:key :moptilities :name "Moptilities" :sub-folder "cl-containers"
                   :short-description "Implementation independent MOP utilities")
             (:key :metabang.bind :name "metabang.bind" 
                   :sub-folder "cl-containers"
                   :cliki "bind"
                   :short-description "Handle destructuring, multiple-values and let simultaneously")
             (:key :tinaa :name "TINAA"
                   :short-description "Common-Lisp documentation tool")
             
             (:key :metabang-site :name "metabang.com" 
                   :root "http://www.metabang.com/"
                   :folder ""
                   :software? nil)))
   #'string-lessp
   :key #'key))

;;; ---------------------------------------------------------------------------

(defun lml-insert-file (file)
  (if (probe-file file)
      (with-open-file (in file :direction :input)
        (do ((line (read-line in nil 'eof) (read-line in nil 'eof)))
	    ((eq line 'eof))
	  (html (lml-princ line))))
    (format *trace-output* "Warning: unable to insert LML file ~S" file)))

;;; ---------------------------------------------------------------------------

(defun website-source-directory (system-name)
  (translate-logical-pathname
   (format nil "user-home:darcs;~(~A~);website;source;**;*.*" system-name)))

;;; ---------------------------------------------------------------------------

(defun website-output-directory (system-name)
  (translate-logical-pathname
   (format nil "user-home:darcs;~(~A~);website;output;" system-name)))

;;; ---------------------------------------------------------------------------

(defun copy-source-to-output (file)
  (copy-file 
   file
   (output-path-for-source file)
   :if-exists :overwrite))

;;; ---------------------------------------------------------------------------

(defun output-path-for-source (source-file)
  (translate-pathname
   source-file
   *website-wild-source*
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
    (html ((:a :href (link-href link-info)) (:princ (or title (link-title link-info))))))) 

;;; ---------------------------------------------------------------------------

#+notyet
(defun home-folder (system)
  (let* ((key (key system))
         (folder (folder system))
         (sub-folder (sub-folder system))
         (root (or (root system) 
                   "http://common-lisp.net/project/")))
    ;;?? very hacky
    (when sub-folder
      (setf folder (format nil "~(~A/~A~)" sub-folder folder)))
    
    (set-link-info
     key
     :href (if (or (symbolp folder)
                   (and (stringp folder) (plusp (size folder))))
             (format nil "~A~(~A~)/" root folder)
             (format nil "~A" root))
     :title title)
    (set-link-info
     (form-keyword key "-TINAA")
     :href (format nil "~A~(~A~)/documentation/" root folder)
     :title title)))

;;; ---------------------------------------------------------------------------

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
        (set-link-info
         key
         :href (if (or (symbolp folder)
                       (and (stringp folder) (plusp (size folder))))
                 (format nil "~A~(~A~)/" root folder)
                 (format nil "~A" root))
         :title title)
        (set-link-info
         (form-keyword key "-TINAA")
         :href (format nil "~A~(~A~)/documentation/" root folder)
         :title title)
        (set-link-info
         (form-keyword key "-PACKAGE")
         :href (format nil "~A~(~A/~A~)_latest.tar.gz" root folder key)
         :title title)
        (set-link-info
         (form-keyword key "-CLIKI")
         :href (format nil "http://www.cliki.net/~(~A~)" cliki)
         :title title)))

;;; ---------------------------------------------------------------------------

(set-link-info
 :mail-gwking
 :href (format nil "mailto:gwking@metabang.com")
 :title "Gary King")

(set-link-info
 :gpg-gwking
 :href (format nil "http://www.metabang.com/gwking-public-key.html")
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
 :asdf
 :href "http://www.cliki.net/asdf"
 :title "ASDF")



#+Test
(link :metabang-site)
#+Test
(link :cl-containers)
#+Test
(link :metatilities)

