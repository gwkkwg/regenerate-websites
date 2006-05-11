(in-package #:rw)

(defun upload-website (system)
  (let* ((def (find-system system))
         (dest (if (sub-folder def)
                 (format nil "gking@common-lisp.net:/project/~(~A~)/public_html/documentation" 
                         (name def))
                 (format nil "gking@common-lisp.net:/project/~(~A~)/public_html/~(~A~)/documentation" 
                         (sub-folder def) (name def))))
         (src (translate-logical-pathname
               (make-pathname 
                :host "user-home"
                :name (string-downcase (name def))
                :directory '(:absolute "darcs" "website" "output")))))
    (format nil "~@{~A ~}" "rcopy" "--exclude" "*.tem" src dest)
    #+Ignore
    (shell-command "rcopy" "--exclude" "*.tem" src dest)))

;;; ---------------------------------------------------------------------------

(defun upload-documentation (system)
  (let* ((def (find-system system))
         (dest (if (sub-folder def)
                 (format nil "gking@common-lisp.net:/project/~(~A~)/public_html" 
                         (name def))
                 (format nil "gking@common-lisp.net:/project/~(~A~)/public_html/~(~A~)" 
                         (sub-folder def) (name def))))
         (src (translate-logical-pathname
               (make-pathname 
                :host "user-home"
                :name (string-downcase (name def))
                :directory '(:absolute "darcs" "website" "output")))))
    (format nil "~@{~A ~}" "rcopy" "--exclude" "*.tem" src dest)
    #+Ignore
    (shell-command "rcopy" "--exclude" "*.tem" src dest)))

;;; ---------------------------------------------------------------------------

(defun make-asdf-system (system)
  )

;;; ---------------------------------------------------------------------------

(defun upload-source (system)
  )

;;; ---------------------------------------------------------------------------

(defun whatsnew (system)
  )


