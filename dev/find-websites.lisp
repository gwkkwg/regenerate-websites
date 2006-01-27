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



"Billy-Pilgrim:Users:gwking:darcs:tinaa:website:source:index.lml" 
"Billy-Pilgrim:Users:gwking:darcs:moptilities:website:source:index.lml" 
"Billy-Pilgrim:Users:gwking:darcs:metatilities:website:source:index.lml" 
"Billy-Pilgrim:Users:gwking:darcs:metabang.bind:website:source:index.lml" 
"Billy-Pilgrim:Users:gwking:darcs:metabang-site:website:source:index.lml" 
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
          