(in-package rw)

(defun create-tinaa-documentation-for-system (system internal-too? &key 
                                                     (home system)
                                                     (package system))
  (asdf:oos 'asdf:load-op system)
  #+Ignore (ecs system)
  (let ((hack (format nil "user-home:darcs;~(~A~);dev" home)))
    (document-system 'package package
                     (format nil "~A;documentation;" hack)
                     :symbol-kinds '(:external))
    (when internal-too?
      (document-system 'package package 
                       (format nil "~A;documentation;all-symbols;" hack)
                       :symbol-kinds '(:external :internal)))))

;;; ---------------------------------------------------------------------------

(defun create-tinaa-documentation-for-systems ()
  (loop for datum in '(
                       (asdf-install-tester t)
                       (cl-containers t)
                       (cl-graph t)
                       (cl-mathstats t)
                       (cl-variates t)
                       (clnuplot t)
                       (defsystem-compatibility t)
                       (lift t "lift" lift)
                       (moptilities t)
                       (metatilities t)
                       (tinaa t))
        
        do 
        (bind:bind (((system internal-too? &optional (home system) (package system)) datum)) 
          (create-tinaa-documentation-for-system
           system internal-too? :package package :home home))))


