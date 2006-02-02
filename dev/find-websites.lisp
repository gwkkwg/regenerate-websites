(time 
 (mapcar (compose #'namestring #'translate-logical-pathname)
         (remove-if (lambda (pn)
                      (search "_darcs" (namestring (translate-logical-pathname pn))
                              :test #'string-equal))
                    (directory "user-home:darcs;**;index.lml"))))

(time
 (let ((result nil))
   (fad:walk-directory
    "user-home:darcs;"
    (lambda (pn)
      (push (namestring (translate-logical-pathname pn)) result))
    :test 
    (lambda (pn)
      (and (string-equal "index" (pathname-name pn))
           (string-equal "lml" (pathname-type pn))
           (not (search "_darcs" (namestring (translate-logical-pathname pn))
                        :test #'string-equal)))))
   result))


"Billy-Pilgrim:Users:gwking:darcs:asdf-install:website:source:index.lml" 

"Billy-Pilgrim:Users:gwking:darcs:tinaa:website:source:index.lml" 
"Billy-Pilgrim:Users:gwking:darcs:moptilities:website:source:index.lml" 
"Billy-Pilgrim:Users:gwking:darcs:metatilities:website:source:index.lml" 
"Billy-Pilgrim:Users:gwking:darcs:metabang.bind:website:source:index.lml" 
"Billy-Pilgrim:Users:gwking:darcs:lift:website:source:index.lml" 
"Billy-Pilgrim:Users:gwking:darcs:defsystem-compatibility:website:source:index.lml" 
"Billy-Pilgrim:Users:gwking:darcs:clnuplot:website:source:index.lml" 
"Billy-Pilgrim:Users:gwking:darcs:cl-variates:website:source:index.lml" 
"Billy-Pilgrim:Users:gwking:darcs:cl-mathstats:website:source:index.lml" 
"Billy-Pilgrim:Users:gwking:darcs:cl-graph:website:source:index.lml" 
"Billy-Pilgrim:Users:gwking:darcs:cl-containers:website:source:index.lml" 
"Billy-Pilgrim:Users:gwking:darcs:asdf-system-connections:website:source:index.lml" 
"Billy-Pilgrim:Users:gwking:darcs:asdf-status:website:source:index.lml" 
"Billy-Pilgrim:Users:gwking:darcs:asdf-binary-locations:website:source:index.lml" 
          
"Billy-Pilgrim:Users:gwking:darcs:metabang-site:website:source:index.lml" 

openmcl --eval "(asdf:oos 'asdf:load-op 'asdf-install-tester)"
(in-package ait)
(setf *asdf-install-directory* "user-home:darcs;asdf-install;asdf-install;")
(asdf-test :systems *metabang-systems* :lisps '(:clisp :sbcl :openmcl :allegro))

(asdf-test :lisps '(:clisp :sbcl :openmcl))

(asdf-test :systems *metabang-systems* :lisps '(:clisp :sbcl :openmcl))
(asdf-test :systems '(moptilities) :lisps '(:clisp :openmcl :sbcl))
(asdf-test :systems '(moptilities) :lisps '(:allegro))