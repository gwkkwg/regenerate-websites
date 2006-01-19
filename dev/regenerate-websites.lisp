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

;;; ---------------------------------------------------------------------------

(defun regenerate-websites (&key (force? nil))
  (loop for system in *metabang-common-lisp-systems* 
        when (build-website? system) do
        (regenerate-website (key system) :force? force?)))

;;; ---------------------------------------------------------------------------

(defun regenerate-website (system-name &key (force? nil))
  (let ((lml2::*output-dir* (website-output-directory system-name))
        (*package* *package*)
        (*website-source* (website-source-directory system-name))
        (*website-output* (website-output-directory system-name))
        (*force-rebuild?* force?)
        (*current-system* system-name))
    (cl-fad:walk-directory 
     *website-source*
     (lambda (file)
       (regenerate-file 
        (form-keyword (string-upcase (pathname-type file))) file)))
    ;;?? Need run-command...
    #-DIGITOOL
    (create-changelog system-name)
    #-DIGITOOL
    (create-changelog-page system-name)))

;;; ---------------------------------------------------------------------------

(defgeneric regenerate-file (kind file)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defmethod regenerate-file (kind file)
  (warn "I have no idea what to do with ~A" file))

;;; ---------------------------------------------------------------------------

(defmethod regenerate-file ((kind (eql :lml)) file)
  (when (or *force-rebuild?*
            (file-newer-than-file-p 
             file (output-path-for-source file))) 
    (lml2::lml-load file)))

;;; ---------------------------------------------------------------------------

(defmethod regenerate-file ((kind (eql :css)) file)
  (copy-source-to-output file))

;;; ---------------------------------------------------------------------------

(defmethod regenerate-file ((kind (eql :swf)) file)
  ;; Flash animation
  (copy-source-to-output file))

;;; ---------------------------------------------------------------------------

(defmethod regenerate-file ((kind (eql :ico)) file)
  (copy-source-to-output file))

;;; ---------------------------------------------------------------------------

(defmethod regenerate-file ((kind (eql :png)) file)
  (copy-source-to-output file))

;;; ---------------------------------------------------------------------------

(defmethod regenerate-file ((kind (eql :gif)) file)
  (copy-source-to-output file))

;;; ---------------------------------------------------------------------------

(defmethod regenerate-file ((kind (eql :jpg)) file)
  (copy-source-to-output file))

;;; ---------------------------------------------------------------------------

(defmethod regenerate-file ((kind (eql :pdf)) file)
  (copy-source-to-output file))

;;; ---------------------------------------------------------------------------

(defmethod regenerate-file ((kind (eql :asc)) file)
  ;; PGP files
  (copy-source-to-output file))

;;; ---------------------------------------------------------------------------

(defmethod regenerate-file ((kind (eql :html)) file)
  ;; Take HTML directly
  (copy-source-to-output file))

;;; ---------------------------------------------------------------------------

#+Ignore
(defmethod regenerate-file ((kind (eql :xml)) file)
  (process-xml-file (pathname-name file) file))

#|
(in-package few)

#+Ignore
;; suck site up and output as LML
(mapc 
 (lambda (file)
   (let ((html (net.html.parser:parse-html file)))
     (setf html (remove-if (lambda (x)
                             (and (consp x)
                                  (member (first x) '(:!doctype))))
                           html))
     (with-new-file (s (make-pathname :type "lml" :defaults file)
                       :print-right-margin 70)
       (format s "~S" html))))
 (directory "Billy-Pilgrim:Users:gwking:darcs:metabang.tinaa:website:*.html"))

(net.html.parser:parse-html 
 #P"Billy-Pilgrim:Users:gwking:darcs:metabang.tinaa:website:index.html")

(probe-file
 "Billy-Pilgrim:Users:gwking:darcs:cl-containers:website:index.shtml")

(eval `(html
        ((:html :xmlns "http://www.w3.org/1999/xhtml")
         ,@(rest ccl:!))))

#+No
(with-new-file (*html-stream* (spy (make-pathname :type "lml" :defaults file)))
  (dtd-prologue :xhtml11)
  (eval `(html
          ((:html :xmlns "http://www.w3.org/1999/xhtml")
           ,@html))))
|#