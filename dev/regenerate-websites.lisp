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

(defparameter *meta-systems*
  `(cl-containers))

;;; ---------------------------------------------------------------------------

(defun regenerate-system (system-name)
  (let ((lml2::*output-dir* (website-output-directory system-name))
        (*package* *package*)
        (*website-wild-source* (website-source-directory system-name))
        (*website-output* (website-output-directory system-name)))
    (loop for file in (directory *website-wild-source*) do
          (regenerate-file (form-keyword (string-upcase (pathname-type file)))
                           file))))

;;; ---------------------------------------------------------------------------

(defgeneric regenerate-file (kind file)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defmethod regenerate-file (kind file)
  (warn "I have no idea what to do with ~A" file))

;;; ---------------------------------------------------------------------------

(defmethod regenerate-file ((kind (eql :lml)) file)
  (lml2::lml-load file))

;;; ---------------------------------------------------------------------------

(defmethod regenerate-file ((kind (eql :css)) file)
  (copy-source-to-output file))

;;; ---------------------------------------------------------------------------

(defmethod regenerate-file ((kind (eql :png)) file)
  (copy-source-to-output file))


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