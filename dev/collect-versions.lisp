(in-package #:rw)

(defun collect-versions ()
  (loop for system in *metabang-common-lisp-systems*
        when (metabang-software? system) collect
       (list (key system)
	     (system-version-from-cliki (key system))
	     (and 
	      (asdf:find-system (key system) nil)
	      (asdf:system-source-file (key system))))))

#+(or)
(print (collect-versions))

(defun system-version-from-cliki (system)
  (system-version-from-url
   (format nil "http://cliki.net/~a?download" system)))

#+(or)
(system-version-from-cliki 'cl-who)
   
(defun system-version-from-url (url)
  (when (probe-file "/tmp/x.tar.gz")
    (delete-file "/tmp/x.tar.gz"))
  (ignore-errors
    (simple-http:http-download url "/tmp/x.tar.gz"))
  (let ((pathname "/tmp/x.tar"))
    (when (probe-file pathname) 
      (delete-file pathname))
    (gzip-contents-to "/tmp/x.tar.gz" pathname)
    (let ((asd-files (collect-archive-entries
		      pathname
		      :filter
		      (lambda (pathname)
			(string-equal 
			 (pathname-type (namestring pathname))
			 "ASD")))))
      (extract-archive pathname
		       :filter (lambda (pathname)
				 (string-equal 
				  (pathname-type (namestring pathname))
				  "ASD")))
      (remove nil 
	      (loop for asd-file in asd-files collect
		   (let ((file (merge-pathnames 
				asd-file
				(asdf::pathname-sans-name+type pathname))))
		     (list file (system-version-from-file file))))
	      :key #'first))))

#+(or)
(system-version-from-url 
 "http://common-lisp.net/project/metatilities-base/metatilities-base.tar.gz")

(defun system-version-from-file (system-definition-file)
  (first 
   (collect-lines 
    system-definition-file
    :filter 
    (lambda (line)
      (string-starts-with (strip-whitespace line) ":version" 
			  :test #'char-equal))
    :transform 
    (lambda (line)
      (read-from-string 
       (subseq (strip-whitespace line) (1+ (length ":version"))))))))

(defun system-version-from-definition (system)
  (let ((system-definition-file 
	 (asdf:system-relative-pathname system (format nil "~a.asd" system))))
    (if system-definition-file 
	(system-version-from-file system-definition-file)
	#+(or)
	;; cannot-happen
	(error "no system definition found for ~a" system))))

#+(or)
(system-version-from-definition 'lift)

(defun gzip-contents (pathname)
  (with-open-file (stream pathname :direction :input
                                   :element-type '(unsigned-byte 8))
    (chipz:decompress nil :gzip stream)))

(defun gzip-contents-to (pathname output)
  (with-output (out output)
    (with-open-file (stream pathname :direction :input
			    :element-type '(unsigned-byte 8))
      (chipz:decompress out :gzip stream))))

#+(or)
(gzip-contents-to "/tmp/x" "/tmp/x.tar")


(defun extract-archive 
    (pathname &key (relative-to 
		    (asdf::pathname-sans-name+type pathname))
     (filter (constantly t)))
  (let ((*default-pathname-defaults* relative-to))
    (archive::extract-files-from-pathname pathname filter)))

#+(or)
(extract-archive "/tmp/y.tar")

(defun collect-archive-entries (pathname &key filter)
  (archive:with-open-archive (archive pathname :direction :input)
    (let ((result nil))
      (archive:do-archive-entries (entry archive)
	(when (or (null filter) (funcall filter (archive:name entry)))
	  (push (archive:name entry) result)))
      (nreverse result))))

#+(or)
(list-archive-entries "/tmp/x.tar")

