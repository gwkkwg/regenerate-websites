(in-package rw)

(defun create-tinaa-documentation-for-system (system internal-too? &key 
                                                     (home system)
                                                     (package system))
  (asdf:oos 'asdf:load-op system)
  #+Ignore 
  (ecs system)
  (document-system 'package package
                   (make-pathname 
                    :host "user-home"
                    :directory `(:absolute "darcs" 
                                           ,(string-downcase (kl:ensure-string home))
                                           "dev" "documentation"))
                   :symbol-kinds '(:external))
  (when internal-too?
    (document-system 'package package 
                     (make-pathname 
                      :host "user-home"
                      :directory `(:absolute "darcs" 
                                             ,(string-downcase (kl:ensure-string home))
                                             "dev" "documentation" "all-symbols"))
                     :symbol-kinds '(:external :internal))))

;;; ---------------------------------------------------------------------------

(defun create-tinaa-documentation-for-systems ()
  (loop for system in *metabang-common-lisp-systems*
        when (build-documentation? system) do

        (bind:bind ((system-name (key system))
                    (internal-too? t)
                    (documation-package (documentation-package system))
                    (home (home-directory system)))
          (create-tinaa-documentation-for-system
           system-name internal-too? 
           :package documation-package
           :home home))))

#+old
'(
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

