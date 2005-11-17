(in-package rw)

(defun create-tinaa-documentation-for-system (system internal-too? &key 
                                                     (home system)
                                                     (package system))
  #+Ignore (asdf:oos 'asdf:load-op system)
  (ecs system)
  (document-system 'package package
                   (format nil "~(~A~):documentation;" home)
                   :symbol-kinds '(:external))
  (when internal-too?
    (document-system 'package package 
                     (format nil "~(~A~):documentation;all-symbols;" home)
                     :symbol-kinds '(:external :internal))))

#+Test
(loop for datum in '((cl-containers t)
                     (cl-graph t)
                     (cl-mathstats t)
                     (cl-variates t)
                     (basic-lift t :home "lift" :package 'lift)
                     (moptilities t)
                     (metatilities t)
                     (tinaa t))
      
                     do 
      (bind:bind (((system internal-too? &optional (home system) (package system)) datum)) 
        (create-tinaa-documentation-for-system
         system internal-too? :package package :home home)))


