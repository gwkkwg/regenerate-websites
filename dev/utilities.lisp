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

(defvar *website-wild-source* nil)
(defvar *website-output* nil)

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
   (translate-pathname
    file
    *website-wild-source*
    (make-pathname 
     :directory `(,@(pathname-directory *website-output*) :wild-inferiors)
     :name :wild
     :type :wild))))